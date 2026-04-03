-- app/main.hs
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick
import Brick.Types (zoom)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center (center)
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import Lens.Micro (Lens')
import Text.Megaparsec (parse, errorBundlePretty)
import System.Directory (doesFileExist)
import System.FilePath (takeFileName) -- NEW: For stripping out the folder paths
import Data.List (nub)

import AST
import Parser
import Evaluator

-- | 1. APP MODES, NAMES & STATE
data AppMode = Splash | Editing | SaveDialog | OpenDialog
    deriving (Eq)

data Name = CodeEditor | SaveEditor | OpenEditor
    deriving (Eq, Ord, Show)

data AppStatus = Normal | Saved | ErrorMsg String

data AppState = AppState
    { _mode        :: AppMode
    , _editor      :: E.Editor String Name
    , _saveInput   :: E.Editor String Name 
    , _openInput   :: E.Editor String Name 
    , _currentFile :: Maybe FilePath     
    , _recentFiles :: [FilePath]           
    , _status      :: AppStatus
    }

editorLens :: Lens' AppState (E.Editor String Name)
editorLens f st = (\e -> st { _editor = e }) <$> f (_editor st)

saveInputLens :: Lens' AppState (E.Editor String Name)
saveInputLens f st = (\e -> st { _saveInput = e }) <$> f (_saveInput st)

openInputLens :: Lens' AppState (E.Editor String Name)
openInputLens f st = (\e -> st { _openInput = e }) <$> f (_openInput st)

-- | 2. PERSISTENCE HELPERS
loadRecents :: IO [FilePath]
loadRecents = do
    exists <- doesFileExist ".ironsmith_recents"
    if exists
        then lines <$> readFile ".ironsmith_recents"
        else return []

saveRecent :: FilePath -> [FilePath] -> IO [FilePath]
saveRecent path oldRecents = do
    let newRecents = take 5 $ nub (path : oldRecents)
    writeFile ".ironsmith_recents" (unlines newRecents)
    return newRecents


-- | 3. UI DRAWING FUNCTIONS
drawUI :: AppState -> [Widget Name]
drawUI st = case _mode st of
    Splash     -> [drawSplash st]
    Editing    -> [drawEditor st]
    SaveDialog -> [drawSaveDialog st] 
    OpenDialog -> [drawOpenDialog st]

drawSplash :: AppState -> Widget Name
drawSplash st = center $ vBox $
    [ withAttr (attrName "title") $ str " _____                 _____           _ _   _ "
    , withAttr (attrName "title") $ str "|_   _|               /  ___|         (_) | | |"
    , withAttr (attrName "title") $ str "  | | _ __ ___  _ __  \\ `--. _ __ ___  _| |_| |__ "
    , withAttr (attrName "title") $ str "  | || '__/ _ \\| '_ \\  `--. \\ '_ ` _ \\| | __| '_ \\"
    , withAttr (attrName "title") $ str " _| || | | (_) | | | |/\\__/ / | | | | | | |_| | | |"
    , withAttr (attrName "title") $ str " \\___/_|  \\___/|_| |_|\\____/|_| |_| |_|_|\\__|_| |_|"
    , str " "
    , center $ str "The Procedural 3D Forge"
    , str " "
    , center $ withAttr (attrName "title") $ str "--- Recent Files ---"
    ] ++ drawRecents (_recentFiles st) ++
    [ str " "
    , center $ withAttr (attrName "success") $ str "[ Press ENTER for New File | Press 'O' to Open File ]"
    , center $ str "[ Press ESC to Quit ]"
    ]
  where
    drawRecents [] = [ center $ str "(No recent files)" ]
    -- FIX: Apply takeFileName to cleanly display just the filename
    drawRecents fs = [ center $ str ("[" ++ show i ++ "] " ++ takeFileName f) | (i, f) <- zip [(1::Int)..] fs ]

drawEditor :: AppState -> Widget Name
drawEditor st = ui
  where
    -- FIX 1: Use (vBox . map str) instead of (str . unlines) to properly render multi-line text!
    codeWidget = E.renderEditor (vBox . map str) True (_editor st)
    
    fileLabel = case _currentFile st of
        Nothing -> " *UNSAVED* "
        Just f  -> " " ++ takeFileName f ++ " "

    statusWidget = case _status st of
        Normal      -> withAttr (attrName "success") $ str "Status: OK"
        Saved       -> withAttr (attrName "saved")   $ str "Status: FILE SAVED SUCCESSFULLY"
        -- FIX 2: Break the Megaparsec error into multiple lines, and limit it to 5 lines tall max
        ErrorMsg e  -> withAttr (attrName "error")   $ vLimit 5 $ vBox (map str (lines e))

    ui = withBorderStyle unicode
         $ borderWithLabel (str (" IronSmith:" ++ fileLabel))
         $ vBox
             -- FIX 3: Remove vLimitPercent! Let Brick calculate the remaining space naturally.
             [ padAll 1 codeWidget
             , hBorder
             , padAll 1 statusWidget
             ]

drawSaveDialog :: AppState -> Widget Name
drawSaveDialog st = center $ borderWithLabel (str " Save As (.irsm) ") $ padAll 2 $ vBox
    [ str "Enter file name (e.g., my_model.irsm):"
    , str " "
    , vLimit 1 $ E.renderEditor (str . unlines) True (_saveInput st)
    , str " "
    , withAttr (attrName "success") $ str "[ Press ENTER to confirm ]"
    , withAttr (attrName "error")   $ str "[ Press ESC to cancel ]"
    ]

drawOpenDialog :: AppState -> Widget Name
drawOpenDialog st = center $ borderWithLabel (str " Open File ") $ padAll 2 $ vBox
    [ str "Enter path to .irsm file:"
    , str " "
    , vLimit 1 $ E.renderEditor (str . unlines) True (_openInput st)
    , str " "
    , case _status st of
        ErrorMsg e -> withAttr (attrName "error") $ str e
        _          -> str " "
    , withAttr (attrName "success") $ str "[ Press ENTER to open ]"
    , withAttr (attrName "error")   $ str "[ Press ESC to cancel ]"
    ]

-- | 4. EVENT ROUTER
handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent ev = do
    st <- get
    case _mode st of
        Splash     -> handleSplash ev
        Editing    -> handleEditing ev
        SaveDialog -> handleSaveDialog ev
        OpenDialog -> handleOpenDialog ev

-- | 4a. Splash Screen Events
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

-- | 4b. Editing Events
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

-- | 4c. Save Dialog Events
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

-- | 4d. Open Dialog Events
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

-- | 5. COMPILER BRIDGE
compileAndSave :: String -> IO (Maybe String)
compileAndSave code =
    case parse pScript "editor" code of
        Left bundle -> return $ Just (errorBundlePretty bundle)
        Right astScript -> do
            let glslData = compileToGLSL astScript
            writeFile "output.glsl" glslData
            return Nothing

-- | 6. BOOTSTRAP
app :: App AppState e Name
app = App
    { appDraw         = drawUI
    , appChooseCursor = showFirstCursor 
    , appHandleEvent  = handleEvent
    , appStartEvent   = return ()
    , appAttrMap      = const $ attrMap V.defAttr
        [ (attrName "error",   fg V.red)
        , (attrName "success", fg V.green)
        , (attrName "saved",   fg V.yellow `V.withStyle` V.bold)
        , (attrName "title",   fg V.cyan)
        ]
    }

main :: IO ()
main = do
    recents <- loadRecents
    
    let initialState = AppState
            { _mode        = Splash
            , _editor      = E.editor CodeEditor Nothing "" 
            , _saveInput   = E.editor SaveEditor (Just 1) "" 
            , _openInput   = E.editor OpenEditor (Just 1) ""
            , _currentFile = Nothing
            , _recentFiles = recents
            , _status      = Normal
            }
            
    _ <- defaultMain app initialState
    putStrLn "Forge safely closed."