{-# LANGUAGE OverloadedStrings #-}
module AppUI where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center (center, hCenter)
import qualified Brick.Widgets.Edit as E
import System.FilePath (takeFileName)
import Data.Char (isAlpha, isAlphaNum, isDigit) -- Required for the Lexer

import AppState

drawUI :: AppState -> [Widget Name]
drawUI st = case _mode st of
    Splash        -> [drawSplash st]
    Editing       -> [drawEditor st]
    SaveDialog    -> [drawSaveDialog st] 
    OpenDialog    -> [drawOpenDialog st]
    UnsavedPrompt -> [drawUnsavedPrompt st]

drawSplash :: AppState -> Widget Name
drawSplash st = 
    center $ 
    withBorderStyle unicode $
    borderWithLabel (str " Your 3D Forge ") $
    vBox 
        [ padAll 1 titleWidget
        , hBorder
        , padAll 1 recentsWidget
        , hBorder
        , padAll 1 commandsWidget
        ]
  where
    recentsWidget = vBox $
        [ withAttr (attrName "title") $ str "--- Recent Files ---"
        , str " "
        ] ++ drawRecents (_recentFiles st)
        
    drawRecents [] = [ str "(No recent files)" ]
    drawRecents fs = [ str ("[" ++ show i ++ "] " ++ takeFileName f) | (i, f) <- zip [(1::Int)..] fs ]

    commandsWidget = vBox
        [ withAttr (attrName "success") $ str "[ Press ENTER for New File | Press 'O' to Open File ]"
        , str " "
        , str "[ Press ESC to Quit ]"
        ]

-- | SPLASH LOGO WIDGET
titleWidget :: Widget Name
titleWidget = hBox
    [ withAttr (attrName "title") $ vBox
        [ str "▄▄ ▄▄ ▄▄        ▄▄ ▄▄ ▄▄ "
        , str "████████        ████████ "
        , str "███▀▀███▄▄████▄▄███▀▀███ "
        , str "███  ████▀▀  ▀▀████  ███ "
        , str "█████████      █████████ "
        , str "█████████      █████████ " 
        ]
    , str "       "
    , withAttr (attrName "title") $ vBox
        [ str "██╗██████╗  ██████╗ ███╗   ██╗███████╗███╗   ███╗██╗████████╗██╗  ██╗"
        , str "██║██╔══██╗██╔═══██╗████╗  ██║██╔════╝████╗ ████║██║╚══██╔══╝██║  ██║"
        , str "██║██████╔╝██║   ██║██╔██╗ ██║███████╗██╔████╔██║██║   ██║   ███████║"
        , str "██║██╔══██╗██║   ██║██║╚██╗██║╚════██║██║╚██╔╝██║██║   ██║   ██╔══██║"
        , str "██║██║  ██║╚██████╔╝██║ ╚████║███████║██║ ╚═╝ ██║██║   ██║   ██║  ██║"
        , str "╚═╝╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝╚═╝     ╚═╝╚═╝   ╚═╝   ╚═╝  ╚═╝"
        ]
    ]

drawEditor :: AppState -> Widget Name
drawEditor st = ui
  where
    -- This handles Syntax Highlighting, Error Backgrounds, and Empty Line Collapsing
    drawLine lineNum s = 
        let isErr = case _status st of
                        ErrorMsg _ errLine -> lineNum == errLine
                        _                  -> False
            
            -- Prevent line collapse by rendering a space if the line is completely empty
            baseWidget = if null s then str " " else hBox (tokenize s)
            
        in if isErr then withAttr (attrName "errorBg") baseWidget else baseWidget

    -- Zip the lines with their line number [1..] so we know which one is rendering
    codeWidget = E.renderEditor (vBox . zipWith drawLine [(1::Int)..]) True (_editor st)
    
    dirtyMarker = if _isDirty st then "*" else ""
    fileLabel = case _currentFile st of
        Nothing -> " *UNSAVED*" ++ dirtyMarker ++ " "
        Just f  -> " " ++ takeFileName f ++ dirtyMarker ++ " "

    statusWidget = case _status st of
        Normal            -> withAttr (attrName "success") $ str "Status: OK"
        Saved             -> withAttr (attrName "saved")   $ str "Status: FILE SAVED SUCCESSFULLY"
        ErrorMsg e _      -> withAttr (attrName "error")   $ vLimit 5 $ vBox (map str (lines e))

    ui = withBorderStyle unicode
         $ borderWithLabel (str (" IronSmith:" ++ fileLabel))
         $ vBox
             [ padAll 1 codeWidget
             , hBorder
             , padAll 1 statusWidget
             ]

drawUnsavedPrompt :: AppState -> Widget Name
drawUnsavedPrompt st = center $ borderWithLabel (str " Unsaved Changes ") $ padAll 2 $ vBox
    [ str "You have unsaved changes in your forge!"
    , str "Would you like to save before returning to the menu?"
    , str " "
    , withAttr (attrName "success") $ str "[ Press ENTER to Save & Exit ]"
    , withAttr (attrName "error")   $ str "[ Press 'N' to Discard Changes ]"
    , str "[ Press ESC to Cancel ]"
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
        ErrorMsg e _ -> withAttr (attrName "error") $ str e
        _            -> str " "
    , withAttr (attrName "success") $ str "[ Press ENTER to open ]"
    , withAttr (attrName "error")   $ str "[ Press ESC to cancel ]"
    ]

-- | SYNTAX HIGHLIGHTING LEXER
tokenize :: String -> [Widget Name]
tokenize [] = []
tokenize s@(c:cs)
    | isAlpha c = let (w, rest) = span (\x -> isAlphaNum x || x == '_') s
                  in colorKeyword w : tokenize rest
    | isDigit c || (c == '-' && not (null cs) && isDigit (head cs)) = 
                  let (w, rest) = span (\x -> isDigit x || x == '.' || x == '-') s
                  in withAttr (attrName "number") (str w) : tokenize rest
    | otherwise = str [c] : tokenize cs

colorKeyword :: String -> Widget Name
colorKeyword w
    | w `elem` ["cube", "sphere", "cylinder", "cone", "torus"] = withAttr (attrName "shape") (str w)
    | w `elem` ["union", "difference", "intersection", "group"] = withAttr (attrName "csg") (str w)
    | w `elem` ["move", "rotateX", "rotateY", "rotateZ"] = withAttr (attrName "transform") (str w)
    | otherwise = str w