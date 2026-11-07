module Main where

import Brick
import Brick.BChan (BChan, newBChan)
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import AppState
import Config (loadConfig, defaultConfig, cfgDefaultObjectColor, cfgFilter)
import AppCore
import AppUI
import AppEvents
import Cli (runCli)

app :: BChan CustomEvent -> App AppState CustomEvent Name
app chan = App
    { appDraw         = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent  = handleEvent chan 
    , appStartEvent   = enableTerminalModes
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

-- | With arguments, run the headless CLI (check/render/export); with none,
--   open the TUI editor.
main :: IO ()
main = do
    args <- getArgs
    if null args then runTui else runCli args

runTui :: IO ()
runTui = do
    recents <- loadRecents
    configPath <- getConfigPath
    cfg <- loadConfig configPath    -- loads or creates ironsmith.toml

    h <- launchViewer

    -- Wait for the viewer's socket before sending anything: messages sent
    -- earlier are silently dropped (the startup demo used to be lost).
    _ <- waitForViewer 10
    sendConfig cfg

    demoPath <- getDemoPath
    demoExists <- doesFileExist demoPath
    demoCode <- if demoExists then readFile demoPath else return "torus(4, 1)"
    _ <- compileAndSave (cfgDefaultObjectColor cfg) True demoCode

    chan <- newBChan 10
    
    let initialState = AppState
            { _mode         = Splash
            , _viewerMode   = OrbitMode
            , _editor       = E.editor CodeEditor Nothing "" 
            , _saveInput    = E.editor SaveEditor (Just 1) "" 
            , _openInput    = E.editor OpenEditor (Just 1) ""
            , _configInput  = E.editor ConfigEditor Nothing ""  -- NEW
            , _paletteInput = E.editor PaletteEditor (Just 1) ""
            , _paletteSelected = 0
            , _currentFile  = Nothing
            , _recentFiles  = recents
            , _status       = Normal
            , _isDirty      = False
            , _viewerHandle = Just h 
            , _editVersion  = 0
            , _config       = cfg                               -- NEW
            , _exportProgress = Nothing
            , _currentFilter = cfgFilter cfg
            , _animPlaying  = False
            , _undoStack    = []
            , _redoStack    = []
            , _pendingUndoSnapshot = True
            , _selAnchor    = Nothing
            , _mouseDragging = False
            , _clipboard    = ""
            }
            
    -- NEW: Boot up customMain instead of defaultMain
    let buildVty = mkVty V.defaultConfig           -- FIX 3: Removed V. prefix
    initialVty <- buildVty
    finalState <- customMain initialVty buildVty (Just chan) (app chan) initialState
    
    stopViewer (_viewerHandle finalState)
    putStrLn "Forge cold. Viewer closed."