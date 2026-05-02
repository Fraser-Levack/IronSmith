module Main where

import Brick
import Brick.BChan (BChan, newBChan)
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import System.Directory (doesFileExist)

import AppState
import AppCore
import AppUI
import AppEvents

app :: BChan CustomEvent -> App AppState CustomEvent Name
app chan = App
    { appDraw         = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent  = handleEvent chan 
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
        , (attrName "colorHex", fg V.magenta `V.withStyle` V.bold)
        ]
    }

main :: IO ()
main = do
    recents <- loadRecents
    demoPath <- getDemoPath
    demoExists <- doesFileExist demoPath
    
    if demoExists
        then do
            code <- readFile demoPath
            _ <- compileAndSave True code
            return ()
        else do
            _ <- compileAndSave True "torus(4, 1, 32)"
            return ()
    
    h <- launchViewer
    
    -- NEW: Create the bounded channel
    chan <- newBChan 10
    
    let initialState = AppState
            { _mode         = Splash
            , _viewerMode   = OrbitMode
            , _editor       = E.editor CodeEditor Nothing "" 
            , _saveInput    = E.editor SaveEditor (Just 1) "" 
            , _openInput    = E.editor OpenEditor (Just 1) ""
            , _currentFile  = Nothing
            , _recentFiles  = recents
            , _status       = Normal
            , _isDirty      = False
            , _viewerHandle = Just h 
            , _editVersion  = 0 -- NEW: Start at 0
            }
            
    -- NEW: Boot up customMain instead of defaultMain
    let buildVty = mkVty V.defaultConfig           -- FIX 3: Removed V. prefix
    initialVty <- buildVty
    finalState <- customMain initialVty buildVty (Just chan) (app chan) initialState
    
    stopViewer (_viewerHandle finalState)
    putStrLn "Forge cold. Viewer closed."