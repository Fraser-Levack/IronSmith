module Main where

import Brick
import Brick.BChan (BChan, newBChan)
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import System.Directory (doesFileExist, makeAbsolute)
import System.Environment (getArgs)
import System.FilePath (takeExtension, takeFileName)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (put)

import AppState
import Config (loadConfig, readFileStrict, cfgDefaultObjectColor, cfgFilter,
               cfgLaunchViewerOnStart, cfgRestoreLastFile)
import AppCore
import AppUI
import AppEvents
import Cli (runCli)

app :: BChan CustomEvent -> EventM Name AppState () -> App AppState CustomEvent Name
app chan startEvent = App
    { appDraw         = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent  = handleEvent chan 
    , appStartEvent   = enableTerminalModes >> startEvent
    , appAttrMap      = const $ attrMap V.defAttr
        [ (attrName "error",    fg V.red)
        , (attrName "success", fg V.green)
        , (attrName "saved",   fg V.yellow `V.withStyle` V.bold)
        , (attrName "title",   fg (V.rgbColor 255 144 47))
        , (attrName "shape",      fg V.cyan)
        , (attrName "csg",        fg V.magenta)
        , (attrName "transform", fg V.blue)
        , (attrName "number",    fg V.yellow)
        , (attrName "errorBg",   bg V.red)
        , (attrName "colorHex", fg V.magenta `V.withStyle` V.bold)
        , (attrName "comment",  fg V.brightBlack)
        , (attrName "section",  fg (V.rgbColor 255 144 47) `V.withStyle` V.bold)
        , (attrName "key",      fg V.cyan `V.withStyle` V.bold)
        , (attrName "selected", V.defAttr `V.withStyle` V.reverseVideo)
        , (attrName "selection", V.white `on` V.rgbColor 38 79 120)
        , (scrollbarAttr,        fg V.brightBlack)
        , (scrollbarHandleAttr,  fg (V.rgbColor 255 144 47))
        ]
    }

-- | Turn on mouse reporting (click to place the cursor, drag to select,
--   wheel to scroll, clickable scroll bars) and bracketed paste, so pasted
--   text arrives as one event instead of a stream of keystrokes. Native
--   terminal selection is still available with Shift+drag.
enableTerminalModes :: EventM Name AppState ()
enableTerminalModes = do
    vty <- getVtyHandle
    let out = V.outputIface vty
    mapM_ (\m -> when (V.supportsMode out m) $ liftIO (V.setMode out m True))
          [V.Mouse, V.BracketedPaste]

-- | With no arguments, open the TUI editor; with a single .irsm path, open
--   the editor on that file; otherwise run the headless CLI
--   (check/render/export).
main :: IO ()
main = do
    args <- getArgs
    case args of
        []                                       -> runTui Nothing
        [file] | takeExtension file == ".irsm"   -> runTui (Just file)
        _                                        -> runCli args

runTui :: Maybe FilePath -> IO ()
runTui fileArg = do
    recents <- loadRecents
    configPath <- getConfigPath
    cfg <- loadConfig configPath    -- loads or creates ironsmith.toml

    -- Reuse a viewer that is already open (e.g. from `ironsmith render`),
    -- otherwise start one. A missing or broken viewer is reported in the
    -- editor instead of crashing it; Ctrl+L retries.
    viewer <- if cfgLaunchViewerOnStart cfg
        then ensureViewer
        else return (Right Nothing)
    sendConfig cfg

    demoPath <- getDemoPath
    demoExists <- doesFileExist demoPath
    demoCode <- if demoExists
        then either (const "torus(4, 1)") id <$> tryIO (readFileStrict demoPath)
        else return "torus(4, 1)"
    _ <- compileAndSave (cfgDefaultObjectColor cfg) True demoCode

    chan <- newBChan 10
    
    let initialState = AppState
            { _mode         = Splash
            , _viewerMode   = initialViewerMode cfg
            , _editor       = E.editor CodeEditor Nothing "" 
            , _saveInput    = E.editor SaveEditor (Just 1) "" 
            , _openInput    = E.editor OpenEditor (Just 1) ""
            , _configInput  = E.editor ConfigEditor Nothing ""
            , _paletteInput = E.editor PaletteEditor (Just 1) ""
            , _paletteSelected = 0
            , _currentFile  = Nothing
            , _recentFiles  = recents
            , _status       = either (`ErrorMsg` 0) (const Normal) viewer
            , _isDirty      = False
            , _viewerHandle = either (const Nothing) id viewer
            , _editVersion  = 0
            , _config       = cfg
            , _exportProgress = Nothing
            , _currentFilter = cfgFilter cfg
            , _animPlaying  = False
            , _undoStack    = []
            , _redoStack    = []
            , _pendingUndoSnapshot = True
            , _selAnchor    = Nothing
            , _mouseDragging = False
            , _clipboard    = ""
            , _afterPrompt  = Nothing
            }

    -- What to show first: the file named on the command line (a new,
    -- not-yet-saved buffer if it doesn't exist), else the most recent
    -- file when restore_last_file is on, else the start screen.
    restoreFile <- case recents of
        (r:_) | cfgRestoreLastFile cfg -> do
            exists <- doesFileExist r
            return (if exists then Just r else Nothing)
        _ -> return Nothing
    let startup = case (fileArg, restoreFile) of
            (Just f, _) -> Just f
            (_, r)      -> r
    startEvent <- case startup of
        Nothing -> return (return ())
        Just f -> do
            exists <- doesFileExist f
            absPath <- makeAbsolute f
            return $ if exists
                then openFile initialState f
                else put initialState { _mode = Editing, _currentFile = Just absPath
                                      , _status = Info ("New file: " ++ takeFileName f ++ " (Ctrl+S to create it)") }

    let buildVty = mkVty V.defaultConfig
    initialVty <- buildVty
    finalState <- customMain initialVty buildVty (Just chan) (app chan startEvent) initialState
    
    stopViewer (_viewerHandle finalState)
    putStrLn "Forge cold. Viewer closed."
