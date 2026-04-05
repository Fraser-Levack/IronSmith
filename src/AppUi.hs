{-# LANGUAGE OverloadedStrings #-}
module AppUI where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center (center, hCenter)
import qualified Brick.Widgets.Edit as E
import System.FilePath (takeFileName)

import AppState

drawUI :: AppState -> [Widget Name]
drawUI st = case _mode st of
    Splash        -> [drawSplash st]
    Editing       -> [drawEditor st]
    SaveDialog    -> [drawSaveDialog st] 
    OpenDialog    -> [drawOpenDialog st]
    UnsavedPrompt -> [drawUnsavedPrompt st] -- NEW

-- (Keep drawSplash and titleWidget exactly the same as your code)
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
    -- FIX: If a line is completely empty, render a space so it keeps a height of 1
    drawLine "" = str " "
    drawLine s  = str s
    
    -- Apply our custom drawLine function instead of just 'str'
    codeWidget = E.renderEditor (vBox . map drawLine) True (_editor st)
    
    fileLabel = case _currentFile st of
        Nothing -> " *UNSAVED* "
        Just f  -> " " ++ takeFileName f ++ " "

    statusWidget = case _status st of
        Normal      -> withAttr (attrName "success") $ str "Status: OK"
        Saved       -> withAttr (attrName "saved")   $ str "Status: FILE SAVED SUCCESSFULLY"
        ErrorMsg e  -> withAttr (attrName "error")   $ vLimit 5 $ vBox (map str (lines e))

    ui = withBorderStyle unicode
         $ borderWithLabel (str (" IronSmith:" ++ fileLabel))
         $ vBox
             [ padAll 1 codeWidget
             , hBorder
             , padAll 1 statusWidget
             ]

-- NEW UI COMPONENT
drawUnsavedPrompt :: AppState -> Widget Name
drawUnsavedPrompt st = center $ borderWithLabel (str " Unsaved Changes ") $ padAll 2 $ vBox
    [ str "You have unsaved changes in your forge!"
    , str "Would you like to save before returning to the menu?"
    , str " "
    , withAttr (attrName "success") $ str "[ Press ENTER to Save & Exit ]"
    , withAttr (attrName "error")   $ str "[ Press 'N' to Discard Changes ]"
    , str "[ Press ESC to Cancel ]"
    ]

-- (Keep drawSaveDialog and drawOpenDialog the same)
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