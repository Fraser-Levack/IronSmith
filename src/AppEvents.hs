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

handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent ev = do
    st <- get
    case _mode st of
        Splash     -> handleSplash ev
        Editing    -> handleEditing ev
        SaveDialog -> handleSaveDialog ev
        OpenDialog -> handleOpenDialog ev

handleSplash :: BrickEvent Name e -> EventM Name AppState ()
handleSplash (VtyEvent (V.EvKey V.KEsc []))   = halt
handleSplash (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    put (st { _mode = Editing, _editor = E.editor CodeEditor Nothing "", _currentFile = Nothing, _status = Normal })
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
                                })
                    else return () 
            else return ()
handleSplash _ = return ()

handleEditing :: BrickEvent Name e -> EventM Name AppState ()
handleEditing (VtyEvent (V.EvKey V.KEsc [])) = halt
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
                    Just e  -> ErrorMsg e
            put (st { _status = newStatus, _recentFiles = newRecents })
        Nothing -> 
            put (st { _mode = SaveDialog })

handleEditing ev = do
    zoom editorLens $ E.handleEditorEvent ev
    st <- get 
    let codeString = unlines $ E.getEditContents (_editor st)
    newErr <- liftIO $ compileAndSave codeString
    let newStatus = case newErr of
            Nothing -> Normal
            Just e  -> ErrorMsg e
    put (st { _status = newStatus })

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
            Just e  -> ErrorMsg e
    
    put (st { _mode = Editing
            , _currentFile = Just filename
            , _saveInput = E.editor SaveEditor (Just 1) ""
            , _status = newStatus
            , _recentFiles = newRecents
            })

handleSaveDialog ev = do
    zoom saveInputLens $ E.handleEditorEvent ev

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
                    })
        else do
            put (st { _status = ErrorMsg "File not found!" })

handleOpenDialog ev = do
    zoom openInputLens $ E.handleEditorEvent ev