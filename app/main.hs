module Main where

import Brick
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import System.Directory (doesFileExist)

import AppState
import AppCore
import AppUI
import AppEvents

app :: App AppState e Name
app = App
    { appDraw         = drawUI
    , appChooseCursor = showFirstCursor 
    , appHandleEvent  = handleEvent
    , appStartEvent   = return ()
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
        ]
    }

main :: IO ()
main = do
    recents <- loadRecents
    
    -- --- FIX: USE GLOBAL DEMO PATH ---
    demoPath <- getDemoPath
    demoExists <- doesFileExist demoPath
    
    if demoExists
        then do
            -- Load and compile the castle demo from the global config folder
            code <- readFile demoPath
            _ <- compileAndSave True code
            return ()
        else do
            -- Safety fallback if demo.irsm is missing from the global folder
            _ <- compileAndSave True "torus(4, 1, 32)"
            return ()
    
    -- 2. Launch the viewer
    h <- launchViewer
    
    let initialState = AppState
            { _mode        = Splash
            , _viewerMode   = OrbitMode
            , _editor      = E.editor CodeEditor Nothing "" 
            , _saveInput   = E.editor SaveEditor (Just 1) "" 
            , _openInput   = E.editor OpenEditor (Just 1) ""
            , _currentFile = Nothing
            , _recentFiles = recents
            , _status      = Normal
            , _isDirty     = False
            , _viewerHandle = Just h 
            }
            
    finalState <- defaultMain app initialState
    
    -- 3. Cleanup
    stopViewer (_viewerHandle finalState)
    putStrLn "Forge cold. Viewer closed."