-- app/main.hs
module Main where

import Brick
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V

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
        [ (attrName "error",   fg V.red)
        , (attrName "success", fg V.green)
        , (attrName "saved",   fg V.yellow `V.withStyle` V.bold)
        , (attrName "title",   fg (V.rgbColor 255 144 47))
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
            , _isDirty     = False
            }
            
    _ <- defaultMain app initialState
    putStrLn "Forge safely closed."