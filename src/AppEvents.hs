module AppEvents where

import Brick
import Brick.Types (zoom)
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import System.Directory (doesFileExist)

import AppState
import AppCore

-- | GLOBAL ROUTER
handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt -- Global Killswitch
handleEvent ev = do
    st <- get
    case _mode st of
        Splash        -> handleSplash ev
        Editing       -> handleEditing ev
        SaveDialog    -> handleSaveDialog ev
        OpenDialog    -> handleOpenDialog ev
        UnsavedPrompt -> handleUnsavedPrompt ev

-- | 1. SPLASH SCREEN
handleSplash :: BrickEvent Name e -> EventM Name AppState ()
handleSplash (VtyEvent (V.EvKey V.KEsc []))   = halt
handleSplash (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    put (st { _mode = Editing, _editor = E.editor CodeEditor Nothing "", _currentFile = Nothing, _status = Normal, _isDirty = False })
handleSplash (VtyEvent (V.EvKey (V.KChar 'o') [])) = do
    st <- get
    put (st { _mode = OpenDialog, _status = Normal, _openInput = E.editor OpenEditor (Just 1) "" })
handleSplash (VtyEvent (V.EvKey (V.KChar c) [])) 
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
handleSplash _ = return ()

-- | 2. EDITING SCREEN
handleEditing :: BrickEvent Name e -> EventM Name AppState ()
handleEditing (VtyEvent (V.EvKey V.KEsc [])) = do
    st <- get
    if _isDirty st 
        then put (st { _mode = UnsavedPrompt })
        else put (st { _mode = Splash })

-- CYCLE MODES (Ctrl+E)
handleEditing (VtyEvent (V.EvKey (V.KChar 'e') [V.MCtrl])) = do
    st <- get
    let nextMode = case _viewerMode st of
            OrbitMode  -> StaticMode
            StaticMode -> FlyMode
            FlyMode    -> OrbitMode
    
    liftIO $ sendToViewer ("CMD:" ++ show nextMode)
    put (st { _viewerMode = nextMode })

handleEditing (VtyEvent (V.EvKey (V.KChar 'o') [V.MCtrl])) = do
    st <- get
    put (st { _mode = OpenDialog, _status = Normal, _openInput = E.editor OpenEditor (Just 1) "" })

handleEditing (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) = do
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
handleEditing brickEv@(VtyEvent ev) | isMovementKey ev = do
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
                        _                        -> ""
            liftIO $ sendToViewer cmd
            return () -- Consumes the event so it doesn't type into the editor
        else do
            handleEditorInput brickEv -- Passes it through to type normally
  where
    isMovementKey (V.EvKey V.KUp []) = True
    isMovementKey (V.EvKey V.KDown []) = True
    isMovementKey (V.EvKey V.KLeft []) = True
    isMovementKey (V.EvKey V.KRight []) = True
    isMovementKey (V.EvKey (V.KChar c) []) = c `elem` ['w', 'a', 's', 'd']
    isMovementKey _ = False

-- Catch-all for standard typing
handleEditing ev = handleEditorInput ev

-- | HELPER: Runs standard editor inputs and triggers compilation
handleEditorInput :: BrickEvent Name e -> EventM Name AppState ()
handleEditorInput ev = do
    st <- get 
    let oldText = E.getEditContents (_editor st)
    
    zoom editorLens $ E.handleEditorEvent ev
    
    st' <- get 
    let newText = E.getEditContents (_editor st')
    
    if oldText /= newText
        then do
            newErr <- liftIO $ compileAndSave False (unlines newText)
            let newStatus = case newErr of
                    Nothing -> Normal
                    Just (e, lineNum) -> ErrorMsg e lineNum
            put (st' { _status = newStatus, _isDirty = True })
        else 
            put st'


-- | 3. UNSAVED CHANGES PROMPT
handleUnsavedPrompt :: BrickEvent Name e -> EventM Name AppState ()
handleUnsavedPrompt (VtyEvent (V.EvKey V.KEsc [])) = do
    st <- get
    put (st { _mode = Editing })

handleUnsavedPrompt (VtyEvent (V.EvKey (V.KChar 'n') [])) = do
    st <- get
    put (st { _mode = Splash, _isDirty = False })
handleUnsavedPrompt (VtyEvent (V.EvKey (V.KChar 'N') [])) = handleUnsavedPrompt (VtyEvent (V.EvKey (V.KChar 'n') []))

handleUnsavedPrompt (VtyEvent (V.EvKey V.KEnter [])) = do
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
handleUnsavedPrompt _ = return ()


-- | 4. SAVE DIALOG
handleSaveDialog :: BrickEvent Name e -> EventM Name AppState ()
handleSaveDialog (VtyEvent (V.EvKey V.KEsc [])) = do
    st <- get
    put (st { _mode = Editing })

handleSaveDialog (VtyEvent (V.EvKey V.KEnter [])) = do
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

handleSaveDialog ev = do
    zoom saveInputLens $ E.handleEditorEvent ev


-- | 5. OPEN DIALOG
handleOpenDialog :: BrickEvent Name e -> EventM Name AppState ()
handleOpenDialog (VtyEvent (V.EvKey V.KEsc [])) = do
    st <- get
    let nextMode = if _currentFile st == Nothing && E.getEditContents (_editor st) == [""] 
                   then Splash else Editing
    put (st { _mode = nextMode, _status = Normal })

handleOpenDialog (VtyEvent (V.EvKey V.KEnter [])) = do
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

handleOpenDialog ev = do
    zoom openInputLens $ E.handleEditorEvent ev