module AppEvents where

import Brick
import Brick.Types (zoom)
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
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
        if idx < length recents
            then do
                let path = recents !! idx
                exists <- liftIO $ doesFileExist path
                if exists
                    then do
                        content <- liftIO $ readFile path
                        _ <- liftIO $ compileAndSave content
                        newRecents <- liftIO $ saveRecent path recents
                        put (st { _mode = Editing
                                , _currentFile = Just path
                                , _editor = E.editor CodeEditor Nothing content
                                , _recentFiles = newRecents 
                                , _isDirty = False
                                })
                    else return () 
            else return ()
handleSplash _ = return ()

-- | 2. EDITING SCREEN
handleEditing :: BrickEvent Name e -> EventM Name AppState ()
handleEditing (VtyEvent (V.EvKey V.KEsc [])) = do
    -- Intercept Escape key to check for unsaved changes
    st <- get
    if _isDirty st 
        then put (st { _mode = UnsavedPrompt }) -- Prompt if unsaved
        else put (st { _mode = Splash })        -- Go straight to splash if clean

handleEditing (VtyEvent (V.EvKey (V.KChar 'o') [V.MCtrl])) = do
    st <- get
    put (st { _mode = OpenDialog, _status = Normal, _openInput = E.editor OpenEditor (Just 1) "" })

handleEditing (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) = do
    st <- get
    case _currentFile st of
        Just path -> do
            let code = unlines $ E.getEditContents (_editor st)
            liftIO $ writeFile path code
            newErr <- liftIO $ compileAndSave code 
            newRecents <- liftIO $ saveRecent path (_recentFiles st) 
            let newStatus = case newErr of
                    Nothing -> Saved
                    Just (e, lineNum) -> ErrorMsg e lineNum
            put (st { _status = newStatus, _recentFiles = newRecents, _isDirty = False })
        Nothing -> 
            put (st { _mode = SaveDialog })

handleEditing ev = do
    -- Compare text before and after the event to see if they actually typed
    st <- get 
    let oldText = E.getEditContents (_editor st)
    
    zoom editorLens $ E.handleEditorEvent ev
    
    st' <- get 
    let newText = E.getEditContents (_editor st')
    
    if oldText /= newText
        then do
            -- Text changed! Compile and mark as dirty.
            newErr <- liftIO $ compileAndSave (unlines newText)
            let newStatus = case newErr of
                    Nothing -> Normal
                    Just (e, lineNum) -> ErrorMsg e lineNum
            put (st' { _status = newStatus, _isDirty = True })
        else 
            -- Text didn't change (e.g. they just pressed arrow keys)
            put st'


-- | 3. UNSAVED CHANGES PROMPT
handleUnsavedPrompt :: BrickEvent Name e -> EventM Name AppState ()
handleUnsavedPrompt (VtyEvent (V.EvKey V.KEsc [])) = do
    -- Cancel exit, go back to code
    st <- get
    put (st { _mode = Editing })

handleUnsavedPrompt (VtyEvent (V.EvKey (V.KChar 'n') [])) = do
    -- Discard and Return to Splash
    st <- get
    put (st { _mode = Splash, _isDirty = False })
handleUnsavedPrompt (VtyEvent (V.EvKey (V.KChar 'N') [])) = handleUnsavedPrompt (VtyEvent (V.EvKey (V.KChar 'n') []))

handleUnsavedPrompt (VtyEvent (V.EvKey V.KEnter [])) = do
    -- Save and Return to Splash
    st <- get
    case _currentFile st of
        Just path -> do
            let code = unlines $ E.getEditContents (_editor st)
            liftIO $ writeFile path code
            _ <- liftIO $ compileAndSave code
            newRecents <- liftIO $ saveRecent path (_recentFiles st)
            put (st { _mode = Splash, _isDirty = False, _recentFiles = newRecents })
        Nothing ->
            -- If it's a new file, we have to ask for a name first
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
    newErr <- liftIO $ compileAndSave code
    newRecents <- liftIO $ saveRecent filename (_recentFiles st) 
    
    let newStatus = case newErr of
            Nothing -> Saved
            Just (e, lineNum) -> ErrorMsg e lineNum
    
    put (st { _mode = Editing
            , _currentFile = Just filename
            , _saveInput = E.editor SaveEditor (Just 1) ""
            , _status = newStatus
            , _recentFiles = newRecents
            , _isDirty = False -- Clean!
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
            _ <- liftIO $ compileAndSave content
            newRecents <- liftIO $ saveRecent path (_recentFiles st)
            put (st { _mode = Editing
                    , _currentFile = Just path
                    , _editor = E.editor CodeEditor Nothing content
                    , _status = Normal
                    , _recentFiles = newRecents
                    , _isDirty = False -- Clean!
                    })
        else do
            put (st { _status = ErrorMsg "File not found!" 0 })

handleOpenDialog ev = do
    zoom openInputLens $ E.handleEditorEvent ev