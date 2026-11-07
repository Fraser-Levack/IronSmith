{-# LANGUAGE OverloadedStrings #-}
module AppUI where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center (center, hCenter)
import qualified Brick.Widgets.Edit as E
import System.FilePath (takeFileName)
import Data.Char (isAlpha, isAlphaNum, isDigit, isHexDigit, isSpace) -- Required for the Lexer

import AppState
import Selection (lineSelection, applySelection)

drawUI st = case _mode st of
    Splash        -> [drawSplash st]
    Editing       -> [drawEditor st]
    SaveDialog    -> [drawSaveDialog st]
    OpenDialog    -> [drawOpenDialog st]
    UnsavedPrompt -> [drawUnsavedPrompt st]
    ConfigEditing -> [drawConfigEditor st]
    CommandPalette -> [drawCommandPalette st, drawEditor st]

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
        , withAttr (attrName "title") $ str "[ Press 'G' to open Settings ]"
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
    -- Syntax highlighting, error-line background, and the selection
    -- overlay. Empty lines render as a space so Brick doesn't collapse
    -- them (which would misalign the cursor with the text).
    selection = fmap (\a -> (a, E.getCursorPosition (_editor st))) (_selAnchor st)

    drawLine lineNum s =
        let isErr = case _status st of
                        ErrorMsg _ errLine -> lineNum == errLine
                        _                  -> False
            sel     = lineSelection selection (lineNum - 1) s
            spans   = applySelection (attrName "selection") sel (tokenSpans s)
            baseWidget = if null spans then str " " else hBox (map spanWidget spans)
        in if isErr then withAttr (attrName "errorBg") baseWidget else baseWidget

    -- Zip the lines with their line number [1..] so we know which one is
    -- rendering. The editor draws inside its own viewport; the scroll bars
    -- attach to it, and the vertical one is clickable (see AppEvents).
    codeWidget = withClickableVScrollBars ScrollBar
               $ withVScrollBars OnRight
               $ withVScrollBarHandles
               $ withHScrollBars OnBottom
               $ E.renderEditor (vBox . zipWith drawLine [(1::Int)..]) True (_editor st)
    
    dirtyMarker = if _isDirty st then "*" else ""
    fileLabel = case _currentFile st of
        Nothing -> " *UNSAVED*" ++ dirtyMarker ++ " "
        Just f  -> " " ++ takeFileName f ++ dirtyMarker ++ " "

    statusWidget = case _exportProgress st of
        Just p  -> withAttr (attrName "title") $ str ("Exporting to .OBJ... " ++ progressBarText p)
        Nothing -> case _status st of
            Normal            -> withAttr (attrName "success") $ str "Status: OK"
            Saved             -> withAttr (attrName "saved")   $ str "Status: FILE SAVED SUCCESSFULLY"
            Exported path     -> withAttr (attrName "saved")   $ str ("Status: EXPORTED TO " ++ takeFileName path)
            FilterSet name    -> withAttr (attrName "saved")   $ str ("Status: FILTER -> " ++ name)
            Info msg          -> withAttr (attrName "saved")   $ str ("Status: " ++ msg)
            ErrorMsg e _      -> withAttr (attrName "error")   $ vLimit 8 $ vBox (map str (lines e))

    modeIndicator
        | _animPlaying st = withAttr (attrName "saved") $ str " -- ANIMATION PLAYING -- "
        | otherwise = case _viewerMode st of
            OrbitMode  -> withAttr (attrName "title") $ str " -- ORBIT EDIT -- "
            StaticMode -> withAttr (attrName "success") $ str " -- STATIC EDIT -- "
            FlyMode    -> withAttr (attrName "errorBg") $ str " -- FLY MODE (WASD) -- "

    ui = withBorderStyle unicode
         $ borderWithLabel (str (" IronSmith:" ++ fileLabel))
         $ vBox
             [ padLeft (Pad 1) $ padTop (Pad 1) codeWidget
             , hBorder
             , padAll 1 $ hBox [modeIndicator, str " | ", statusWidget]
             ]

-- | Render a hand-rolled text progress bar, e.g. "[#####-----] 42%", for a
--   progress value in [0,1].
progressBarText :: Float -> String
progressBarText p =
    let clamped = max 0 (min 1 p)
        width   = 20
        filled  = round (clamped * fromIntegral width)
        bar     = replicate filled '#' ++ replicate (width - filled) '-'
        pct     = round (clamped * 100) :: Int
    in "[" ++ bar ++ "] " ++ show pct ++ "%"

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

drawConfigEditor :: AppState -> Widget Name
drawConfigEditor st =
    withBorderStyle unicode $
    borderWithLabel (str " IronSmith Settings (ironsmith.toml) ") $
    vBox
        [ padAll 1 $ E.renderEditor (vBox . map drawConfigLine) True (_configInput st)
        , hBorder
        , padAll 1 $ hBox
            [ withAttr (attrName "success") $ str "Ctrl+S: Save & Apply"
            , str "  |  "
            , withAttr (attrName "error")   $ str "ESC: Discard"
            ]
        ]

drawCommandPalette :: AppState -> Widget Name
drawCommandPalette st =
    center $
    withBorderStyle unicode $
    borderWithLabel (str " Command Palette ") $
    hLimit 60 $
    vBox
        [ padAll 1 $ vLimit 1 $ E.renderEditor (str . unlines) True (_paletteInput st)
        , hBorder
        , padAll 1 $ vBox (zipWith drawItem [0..] cmds)
        , hBorder
        , padAll 1 $ str "Up/Down: select | Enter: run | Esc: cancel"
        ]
  where
    cmds = filteredCommands st
    cfg  = _config st
    drawItem :: Int -> CommandId -> Widget Name
    drawItem i cmdId =
        let row = padRight Max (str (commandLabel cmdId))
                  <+> str (formatKeyCombo (keybindingFor cfg cmdId))
        in if i == _paletteSelected st
               then withAttr (attrName "selected") row
               else row

-- | Render a single line of the settings file with syntax highlighting.
--   Empty lines are rendered as a single space, otherwise Brick collapses
--   them to zero height and the cursor row no longer lines up with the
--   underlying editor contents (the same fix drawEditor applies).
drawConfigLine :: String -> Widget Name
drawConfigLine s
    | null s    = str " "
    | otherwise = hBox (tokenizeConfig s)

-- | SETTINGS SYNTAX HIGHLIGHTING (TOML-ish)
--   Highlights comments, [section] headers, keys, and values.
tokenizeConfig :: String -> [Widget Name]
tokenizeConfig line = case dropWhile isSpace line of
    ('#':_) -> [withAttr (attrName "comment") (str line)]
    ('[':_) -> [withAttr (attrName "section") (str line)]
    _       -> case break (== '=') line of
        (key, '=':val) -> withAttr (attrName "key") (str key)
                        : str "="
                        : tokenizeConfigValue val
        _              -> [str line]

-- | Highlight the value side of a "key = value" line, including any
--   trailing "# ..." comment.
tokenizeConfigValue :: String -> [Widget Name]
tokenizeConfigValue [] = []
tokenizeConfigValue s@(c:cs)
    | isSpace c = let (ws, rest) = span isSpace s in str ws : tokenizeConfigValue rest
    | c == '#'  =
        let (hex, rest) = splitAt 6 cs
        in if length hex == 6 && all isHexDigit hex
           then withAttr (attrName "colorHex") (str ('#':hex)) : tokenizeConfigValue rest
           else [withAttr (attrName "comment") (str s)]
    | otherwise =
        let (word, rest) = break (\x -> isSpace x || x == '#') s
        in colorConfigValue word : tokenizeConfigValue rest

colorConfigValue :: String -> Widget Name
colorConfigValue w
    | w == "true"  = withAttr (attrName "success") (str w)
    | w == "false" = withAttr (attrName "error") (str w)
    | not (null w) && all (\ch -> isDigit ch || ch == '.' || ch == '-') w
                   = withAttr (attrName "number") (str w)
    | otherwise    = str w

-- | SYNTAX HIGHLIGHTING LEXER
--   Splits a line into highlighted spans that cover every character in
--   order, so a column index into the line maps directly onto them (the
--   selection overlay relies on this).
tokenSpans :: String -> [(AttrName, String)]
tokenSpans [] = []
tokenSpans s@(c:cs)
    | isAlpha c = let (w, rest) = span (\x -> isAlphaNum x || x == '_') s
                  in (keywordAttr w, w) : tokenSpans rest
    | isDigit c || (c == '-' && not (null cs) && isDigit (head cs)) =
                  let (w, rest) = span (\x -> isDigit x || x == '.' || x == '-') s
                  in (attrName "number", w) : tokenSpans rest
    | c == '/' && take 1 cs == "/" = [(attrName "comment", s)]
    | c == '#'  = let (w, rest) = span isHexDigit cs
                  in (attrName "colorHex", '#' : w) : tokenSpans rest
    | otherwise = (plainAttr, [c]) : tokenSpans cs

-- | Attribute for unhighlighted text: rendered without 'withAttr' so it
--   inherits the surrounding one (e.g. the error-line background).
plainAttr :: AttrName
plainAttr = attrName "plain"

spanWidget :: (AttrName, String) -> Widget Name
spanWidget (a, t)
    | a == plainAttr = str t
    | otherwise      = withAttr a (str t)

keywordAttr :: String -> AttrName
keywordAttr w
    | w `elem` ["cube", "sphere", "cylinder", "cone", "torus"] = attrName "shape"
    | w `elem` ["union", "difference", "intersection", "group"] = attrName "csg"
    | w `elem` ["move", "rotateX", "rotateY", "rotateZ", "paint", "material", "light", "camera"] = attrName "transform"
    | otherwise = plainAttr