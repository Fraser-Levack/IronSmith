-- app/main.hs
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick
import Brick.Types (zoom)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import Lens.Micro (Lens')
import Text.Megaparsec (parse, errorBundlePretty)

import AST
import Parser
import Evaluator

-- | 1. THE STATE
data AppState = AppState
    { _editor    :: E.Editor String ()
    , _lastError :: Maybe String
    }

-- | 1b. THE LENS (Allows Brick to 'zoom' into the editor's state)
editorLens :: Lens' AppState (E.Editor String ())
editorLens f st = (\e -> st { _editor = e }) <$> f (_editor st)

-- | 2. THE UI DRAWING FUNCTION
drawUI :: AppState -> [Widget ()]
drawUI st = [ui]
  where
    editorWidget = E.renderEditor (str . unlines) True (_editor st)
    
    statusWidget = case _lastError st of
        Nothing  -> withAttr (attrName "success") $ str "Status: OK - output.glsl forged successfully!"
        Just err -> withAttr (attrName "error")   $ str err

    ui = withBorderStyle unicode
         $ borderWithLabel (str " IronSmith Forge ")
         $ vBox
             [ vLimitPercent 75 $ padAll 1 editorWidget
             , hBorder
             , padAll 1 statusWidget
             ]

-- | 3. THE EVENT HANDLER
handleEvent :: BrickEvent () e -> EventM () AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt -- Press ESC to exit
handleEvent ev = do
    -- 1. Zoom into the editor and pass it the entire BrickEvent
    zoom editorLens $ E.handleEditorEvent ev
    
    -- 2. Grab the freshly updated state
    st <- get 
    
    -- 3. Extract text from the editor
    let codeLines  = E.getEditContents (_editor st)
        codeString = unlines codeLines

    -- 4. Run our compiler bridge
    newErrorState <- liftIO $ compileAndSave codeString
    
    -- 5. Save the error state back
    put (st { _lastError = newErrorState })


-- | 4. THE COMPILER BRIDGE
compileAndSave :: String -> IO (Maybe String)
compileAndSave code =
    case parse pScript "editor" code of
        Left bundle -> return $ Just (errorBundlePretty bundle)
        Right astScript -> do
            let glslData = compileToGLSL astScript
            writeFile "output.glsl" glslData
            return Nothing

-- | 5. THE APP DEFINITION
app :: App AppState e ()
app = App
    { appDraw         = drawUI
    , appChooseCursor = showFirstCursor 
    , appHandleEvent  = handleEvent
    , appStartEvent   = return ()
    , appAttrMap      = const $ attrMap V.defAttr
        [ (attrName "error",   fg V.red)
        , (attrName "success", fg V.green)
        ]
    }

main :: IO ()
main = do
    initialCode <- readFile "test.irsm"
    
    let initialState = AppState
            -- FIX: Pass 'Nothing' for unlimited lines, and 'initialCode' as a single string
            { _editor    = E.editor () Nothing initialCode 
            , _lastError = Nothing
            }
            
    _ <- defaultMain app initialState
    putStrLn "Forge safely closed."