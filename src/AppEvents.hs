module AppEvents where

import Brick
import Brick.BChan (BChan, writeBChan)
import Brick.Types (zoom)
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import Control.Concurrent (forkIO, threadDelay)
import System.Directory (doesFileExist)

import AppState
import AppCore

-- | GLOBAL ROUTER
handleEvent :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()

-- Intercept the custom timer event
handleEvent _ (AppEvent (CompileTimerFired version)) = do
    st <- get
    -- Only compile if the user hasn't typed anything since this timer started
    when (version == _editVersion st) $ do
        let code = unlines $ E.getEditContents (_editor st)
        newErr <- liftIO $ compileAndSave False code
        let newStatus = case newErr of
                Nothing -> Normal
                Just (e, lineNum) -> ErrorMsg e lineNum
        put (st { _status = newStatus })

handleEvent _ (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt -- Global Killswitch
handleEvent chan ev = do
    st <- get
    case _mode st of
        Splash        -> handleSplash chan ev
        Editing       -> handleEditing chan ev
        SaveDialog    -> handleSaveDialog chan ev
        OpenDialog    -> handleOpenDialog chan ev
        UnsavedPrompt -> handleUnsavedPrompt chan ev

-- | 1. SPLASH SCREEN
handleSplash :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()
handleSplash _ (VtyEvent (V.EvKey V.KEsc []))   = halt
handleSplash _ (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    put (st { _mode = Editing, _editor = E.editor CodeEditor Nothing "", _currentFile = Nothing, _status = Normal, _isDirty = False })
handleSplash _ (VtyEvent (V.EvKey (V.KChar 'o') [])) = do
    st <- get
    put (st { _mode = OpenDialog, _status = Normal, _openInput = E.editor OpenEditor (Just 1) "" })
handleSplash _ (VtyEvent (V.EvKey (V.KChar c) [])) 
    | c `elem` ['1'..'5'] = do
        st <- get
        let idx = read [c] - 1
            recents = _recentFiles st
        when (idx < length recents) $ do
            let path = recents !! idx
            exists <- liftIO $ doesFileExist path
            when exists $ do
                content <- liftIO $ readFile path
                -- True: Hard save on open
                _ <- liftIO $ compileAndSave True content
                newRecents <- liftIO $ saveRecent path recents
                put (st { _mode = Editing
                        , _currentFile = Just path
                        , _editor = E.editor CodeEditor Nothing content
                        , _recentFiles = newRecents 
                        , _isDirty = False
                        })
handleSplash _ _ = return ()

-- | 2. EDITING SCREEN
handleEditing :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()
handleEditing _ (VtyEvent (V.EvKey V.KEsc [])) = do
    st <- get
    if _isDirty st 
        then put (st { _mode = UnsavedPrompt })
        else put (st { _mode = Splash })

-- CYCLE MODES (Ctrl+E)
handleEditing _ (VtyEvent (V.EvKey (V.KChar 'e') [V.MCtrl])) = do
    st <- get
    let nextMode = case _viewerMode st of
            OrbitMode  -> StaticMode
            StaticMode -> FlyMode
            FlyMode    -> OrbitMode
    
    liftIO $ sendCommand ("CMD:" ++ show nextMode)
    put (st { _viewerMode = nextMode })

handleEditing _ (VtyEvent (V.EvKey (V.KChar 'r') [V.MCtrl])) = do
    liftIO $ sendCommand "CMD:RESET_CAMERA"
    -- No state changes needed, just fire and forget
    return ()

handleEditing _ (VtyEvent (V.EvKey (V.KChar 'o') [V.MCtrl])) = do
    st <- get
    put (st { _mode = OpenDialog, _status = Normal, _openInput = E.editor OpenEditor (Just 1) "" })

handleEditing _ (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) = do
    st <- get
    case _currentFile st of
        Just path -> do
            let code = unlines $ E.getEditContents (_editor st)
            liftIO $ writeFile path code
            newErr <- liftIO $ compileAndSave True code 
            newRecents <- liftIO $ saveRecent path (_recentFiles st) 
            let newStatus = case newErr of
                    Nothing -> Saved
                    Just (e, lineNum) -> ErrorMsg e lineNum
            put (st { _status = newStatus, _recentFiles = newRecents, _isDirty = False })
        Nothing -> 
            put (st { _mode = SaveDialog })

-- INTERCEPT MOVEMENT KEYS (For FlyMode)
handleEditing chan brickEv@(VtyEvent ev) | isMovementKey ev = do
    st <- get
    if _viewerMode st == FlyMode
        then do
            let cmd = case ev of
                        V.EvKey V.KUp []         -> "CMD:PITCH_UP"
                        V.EvKey (V.KChar 'w') [] -> "CMD:PITCH_UP"
                        V.EvKey V.KDown []       -> "CMD:PITCH_DOWN"
                        V.EvKey (V.KChar 's') [] -> "CMD:PITCH_DOWN"
                        V.EvKey V.KLeft []       -> "CMD:YAW_LEFT"
                        V.EvKey (V.KChar 'a') [] -> "CMD:YAW_LEFT"
                        V.EvKey V.KRight []      -> "CMD:YAW_RIGHT"
                        V.EvKey (V.KChar 'd') [] -> "CMD:YAW_RIGHT"
                        V.EvKey (V.KChar 'z') [] -> "CMD:ZOOM_IN"
                        V.EvKey (V.KChar 'x') [] -> "CMD:ZOOM_OUT"
                        V.EvKey (V.KChar 'W') [] -> "CMD:PAN_FORWARD"
                        V.EvKey (V.KChar 'S') [] -> "CMD:PAN_BACKWARD"
                        V.EvKey (V.KChar 'A') [] -> "CMD:PAN_LEFT"
                        V.EvKey (V.KChar 'D') [] -> "CMD:PAN_RIGHT"
                        V.EvKey (V.KChar 'Z') [] -> "CMD:PAN_UP"
                        V.EvKey (V.KChar 'X') [] -> "CMD:PAN_DOWN"
                        _                        -> ""
            liftIO $ sendCommand cmd
            return () -- Consumes the event so it doesn't type into the editor
        else do
            handleEditorInput chan brickEv -- Passes it through to type normally
  where
    isMovementKey (V.EvKey V.KUp []) = True
    isMovementKey (V.EvKey V.KDown []) = True
    isMovementKey (V.EvKey V.KLeft []) = True
    isMovementKey (V.EvKey V.KRight []) = True
    isMovementKey (V.EvKey (V.KChar c) []) = c `elem` ['w', 'a', 's', 'd', 'z', 'x', 'W', 'A', 'S', 'D', 'Z', 'X']
    isMovementKey _ = False

-- Catch-all for standard typing
handleEditing chan ev = handleEditorInput chan ev

-- | HELPER: Runs standard editor inputs and triggers compilation
handleEditorInput :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()
handleEditorInput chan ev = do
    st <- get 
    let oldText = E.getEditContents (_editor st)
    
    zoom editorLens $ E.handleEditorEvent ev
    
    st' <- get 
    let newText = E.getEditContents (_editor st')
    
    if oldText /= newText
        then do
            -- Increment the version, set dirty flag, and fork the timer thread
            let nextVersion = _editVersion st' + 1
            put (st' { _isDirty = True, _editVersion = nextVersion })
            
            liftIO $ forkIO $ do
                threadDelay 500000 -- Sleep for 500ms
                writeBChan chan (CompileTimerFired nextVersion)
            return ()
        else 
            put st'


-- | 3. UNSAVED CHANGES PROMPT
handleUnsavedPrompt :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()
handleUnsavedPrompt _ (VtyEvent (V.EvKey V.KEsc [])) = do
    st <- get
    put (st { _mode = Editing })

handleUnsavedPrompt _ (VtyEvent (V.EvKey (V.KChar 'n') [])) = do
    st <- get
    put (st { _mode = Splash, _isDirty = False })
handleUnsavedPrompt chan (VtyEvent (V.EvKey (V.KChar 'N') [])) = handleUnsavedPrompt chan (VtyEvent (V.EvKey (V.KChar 'n') []))

handleUnsavedPrompt _ (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    case _currentFile st of
        Just path -> do
            let code = unlines $ E.getEditContents (_editor st)
            liftIO $ writeFile path code
            _ <- liftIO $ compileAndSave True code
            newRecents <- liftIO $ saveRecent path (_recentFiles st)
            put (st { _mode = Splash, _isDirty = False, _recentFiles = newRecents })
        Nothing ->
            put (st { _mode = SaveDialog })
handleUnsavedPrompt _ _ = return ()


-- | 4. SAVE DIALOG
handleSaveDialog :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()
handleSaveDialog _ (VtyEvent (V.EvKey V.KEsc [])) = do
    st <- get
    put (st { _mode = Editing })

handleSaveDialog _ (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    let filenameLines = E.getEditContents (_saveInput st)
        filename = if null filenameLines || null (head filenameLines) 
                   then "untitled.irsm" 
                   else head filenameLines
        code = unlines $ E.getEditContents (_editor st)
    
    liftIO $ writeFile filename code
    newErr <- liftIO $ compileAndSave True code
    newRecents <- liftIO $ saveRecent filename (_recentFiles st) 
    
    let newStatus = case newErr of
            Nothing -> Saved
            Just (e, lineNum) -> ErrorMsg e lineNum
    
    put (st { _mode = Editing
            , _currentFile = Just filename
            , _saveInput = E.editor SaveEditor (Just 1) ""
            , _status = newStatus
            , _recentFiles = newRecents
            , _isDirty = False
            })

handleSaveDialog _ ev = do
    zoom saveInputLens $ E.handleEditorEvent ev


-- | 5. OPEN DIALOG
handleOpenDialog :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()
handleOpenDialog _ (VtyEvent (V.EvKey V.KEsc [])) = do
    st <- get
    let nextMode = if _currentFile st == Nothing && E.getEditContents (_editor st) == [""] 
                   then Splash else Editing
    put (st { _mode = nextMode, _status = Normal })

handleOpenDialog _ (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    let pathLines = E.getEditContents (_openInput st)
        path = if null pathLines || null (head pathLines) then "" else head pathLines
    
    exists <- liftIO $ doesFileExist path
    if exists
        then do
            content <- liftIO $ readFile path
            _ <- liftIO $ compileAndSave True content
            newRecents <- liftIO $ saveRecent path (_recentFiles st)
            put (st { _mode = Editing
                    , _currentFile = Just path
                    , _editor = E.editor CodeEditor Nothing content
                    , _status = Normal
                    , _recentFiles = newRecents
                    , _isDirty = False
                    })
        else do
            put (st { _status = ErrorMsg "File not found!" 0 })

handleOpenDialog _ ev = do
    zoom openInputLens $ E.handleEditorEvent ev