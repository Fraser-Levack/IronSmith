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
    Splash     -> [drawSplash st]
    Editing    -> [drawEditor st]
    SaveDialog -> [drawSaveDialog st] 
    OpenDialog -> [drawOpenDialog st]

drawSplash :: AppState -> Widget Name
drawSplash st = center $ vBox $
    [ hCenter titleWidget -- NEW: We abstracted the logo out to its own widget
    , str " "
    , hCenter $ withAttr (attrName "title") $ str "--- Recent Files ---"
    ] ++ drawRecents (_recentFiles st) ++
    [ str " "
    , hCenter $ withAttr (attrName "success") $ str "[ Press ENTER for New File | Press 'O' to Open File ]"
    , str " "
    , hCenter $ str "[ Press ESC to Quit ]"
    ]
  where
    drawRecents [] = [ center $ str "(No recent files)" ]
    drawRecents fs = [ center $ str ("[" ++ show i ++ "] " ++ takeFileName f) | (i, f) <- zip [(1::Int)..] fs ]

-- | THE NEW SPLASH LOGO WIDGET
-- We use hBox to put the Castle vBox and the Title vBox side-by-side.
titleWidget :: Widget Name
titleWidget = hBox 
    [ center $ withAttr (attrName "title") $ vBox
        [ str "▄▄ ▄▄ ▄▄        ▄▄ ▄▄ ▄▄   "
        , str "████████        ████████   "
        , str "███▀▀███▄▄████▄▄███▀▀███   "
        , str "███  ████▀▀  ▀▀████  ███   "
        , str "█████████      █████████   "
        , str "█████████      █████████   " 
        ]
    , center $ withAttr (attrName "title") $ vBox
        [ str " _____                 _____           _ _   _ "
        , str "|_   _|               /  ___|         (_) | | |"
        , str "  | | _ __ ___  _ __  \\ `--. _ __ ___  _| |_| |__ "
        , str "  | || '__/ _ \\| '_ \\  `--. \\ '_ ` _ \\| | __| '_ \\"
        , str " _| || | | (_) | | | |/\\__/ / | | | | | | |_| | | |"
        , str " \\___/_|  \\___/|_| |_|\\____/|_| |_| |_|_|\\__|_| |_|"
        ]
    ]

drawEditor :: AppState -> Widget Name
drawEditor st = ui
  where
    codeWidget = E.renderEditor (vBox . map str) True (_editor st)
    
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