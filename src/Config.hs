{-# LANGUAGE OverloadedStrings #-}
module Config where

import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Data.List (isPrefixOf, stripPrefix)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, mapMaybe)

-- | The full application config
data IronConfig = IronConfig
    { cfgBgColor            :: (Float, Float, Float) -- viewer background RGB
    , cfgDefaultCameraDist  :: Float
    , cfgAutoOrbit          :: Bool
    , cfgShadowEnabled      :: Bool
    , cfgMarchSteps         :: Int
    , cfgAutoCompileDelayMs :: Int
    , cfgDefaultMaterial    :: String
    , cfgLaunchViewerOnStart :: Bool
    , cfgRestoreLastFile    :: Bool
    , cfgDefaultObjectColor :: (Float, Float, Float) -- default colour for unpainted shapes
    , cfgEditorLineWrap     :: Bool
    , cfgKeybindings        :: [(String, String)] -- command id -> key combo, e.g. "save" -> "ctrl+s"
    } deriving (Show)

-- | Rebindable commands and their default key combos.
--   This is the single source of truth for both the generated
--   [keybindings] section of the config file and the fallback used
--   when a binding is missing or the file doesn't define it.
defaultKeybindings :: [(String, String)]
defaultKeybindings =
    [ ("command_palette", "ctrl+p")
    , ("save",            "ctrl+s")
    , ("open_file",       "ctrl+o")
    , ("settings",        "ctrl+g")
    , ("cycle_view_mode",  "ctrl+e")
    , ("reset_camera",     "ctrl+r")
    ]

-- | Sensible defaults matching the current hardcoded values
defaultConfig :: IronConfig
defaultConfig = IronConfig
    { cfgBgColor            = (0.02, 0.02, 0.05)
    , cfgDefaultCameraDist  = 20.0
    , cfgAutoOrbit          = True
    , cfgShadowEnabled      = True
    , cfgMarchSteps         = 150
    , cfgAutoCompileDelayMs = 500
    , cfgDefaultMaterial    = "matte"
    , cfgLaunchViewerOnStart = True
    , cfgRestoreLastFile    = False
    , cfgDefaultObjectColor = (0.8, 0.4, 0.1) -- Forge Orange
    , cfgEditorLineWrap     = False
    , cfgKeybindings        = defaultKeybindings
    }

-- | The default config file content (written on first run)
defaultConfigText :: String
defaultConfigText = unlines $
    [ "# IronSmith Configuration"
    , "# Edit this file to customise your forge."
    , "# Restart IronSmith (or press Ctrl+G) for changes to take effect."
    , ""
    , "[viewer]"
    , ""
    , "# Background colour of the 3D viewport."
    , "# Use a hex colour (e.g. #0a0a1a) or three floats 0.0-1.0"
    , "# (e.g. 0.02, 0.02, 0.05)"
    , "background_color = #05050d"
    , ""
    , "# Default colour for shapes that aren't wrapped in paint(...)."
    , "# Use a hex colour (e.g. #cc6619) or three floats 0.0-1.0"
    , "# (e.g. 0.8, 0.4, 0.1)"
    , "default_object_color = #cc6619"
    , ""
    , "# Starting camera distance from the origin"
    , "default_camera_dist = 20.0"
    , ""
    , "# Auto-orbit the camera on startup (true/false)"
    , "auto_orbit = true"
    , ""
    , "# Enable soft shadows (disable for performance on older GPUs)"
    , "shadow_enabled = true"
    , ""
    , "# Max raymarching steps (higher = more detail, lower = better performance)"
    , "# Recommended range: 64 - 256"
    , "march_steps = 150"
    , ""
    , "[editor]"
    , ""
    , "# Delay in milliseconds after you stop typing before the scene recompiles"
    , "auto_compile_delay_ms = 500"
    , ""
    , "# Default material for new shapes (matte / plastic / neon / metal)"
    , "default_material = matte"
    , ""
    , "# Wrap long lines in the code editor instead of scrolling horizontally"
    , "# (true/false). Not yet implemented - reserved for a future release."
    , "editor_line_wrap = false"
    , ""
    , "[startup]"
    , ""
    , "# Launch the 3D viewer window automatically on startup (true/false)"
    , "launch_viewer_on_start = true"
    , ""
    , "# Automatically re-open the last edited file on startup (true/false)"
    , "restore_last_file = false"
    , ""
    , "[keybindings]"
    , ""
    , "# Customise the editor's keyboard shortcuts."
    , "# Format: modifier+modifier+key, e.g. ctrl+p or ctrl+shift+p"
    , "# Modifiers: ctrl, alt, shift"
    , "# Keys: a single character, or one of:"
    , "#   esc enter tab space up down left right"
    , "#   backspace delete f1 .. f12"
    ] ++ [ key ++ " = " ++ combo | (key, combo) <- defaultKeybindings ]

-- ─── PARSER ──────────────────────────────────────────────────────────────────

-- | Read a file's contents strictly, closing the handle before returning.
--   Lazy 'readFile' can leave the handle open until GC'd, which on Windows
--   causes a later 'writeFile' to the same path to fail with
--   "resource busy (file is locked)".
readFileStrict :: FilePath -> IO String
readFileStrict path = do
    contents <- readFile path
    length contents `seq` return contents

-- | Load config from disk, writing defaults if it doesn't exist yet.
loadConfig :: FilePath -> IO IronConfig
loadConfig path = do
    exists <- doesFileExist path
    if exists
        then do
            raw <- readFileStrict path
            return (parseConfig raw)
        else do
            writeFile path defaultConfigText
            return defaultConfig

-- | Parse the TOML-style config text into an IronConfig.
--   Unknown keys are silently ignored; missing keys fall back to defaults.
parseConfig :: String -> IronConfig
parseConfig raw =
    let pairs = mapMaybe parseLine (lines raw)
    in IronConfig
        { cfgBgColor            = fromMaybe (cfgBgColor defaultConfig)
                                    (lookup "background_color" pairs >>= parseColor)
        , cfgDefaultCameraDist  = fromMaybe (cfgDefaultCameraDist defaultConfig)
                                    (lookup "default_camera_dist" pairs >>= readMaybeFloat)
        , cfgAutoOrbit          = fromMaybe (cfgAutoOrbit defaultConfig)
                                    (lookup "auto_orbit" pairs >>= readMaybeBool)
        , cfgShadowEnabled      = fromMaybe (cfgShadowEnabled defaultConfig)
                                    (lookup "shadow_enabled" pairs >>= readMaybeBool)
        , cfgMarchSteps         = fromMaybe (cfgMarchSteps defaultConfig)
                                    (lookup "march_steps" pairs >>= readMaybeInt)
        , cfgAutoCompileDelayMs = fromMaybe (cfgAutoCompileDelayMs defaultConfig)
                                    (lookup "auto_compile_delay_ms" pairs >>= readMaybeInt)
        , cfgDefaultMaterial    = fromMaybe (cfgDefaultMaterial defaultConfig)
                                    (lookup "default_material" pairs)
        , cfgLaunchViewerOnStart = fromMaybe (cfgLaunchViewerOnStart defaultConfig)
                                    (lookup "launch_viewer_on_start" pairs >>= readMaybeBool)
        , cfgRestoreLastFile    = fromMaybe (cfgRestoreLastFile defaultConfig)
                                    (lookup "restore_last_file" pairs >>= readMaybeBool)
        , cfgDefaultObjectColor = fromMaybe (cfgDefaultObjectColor defaultConfig)
                                    (lookup "default_object_color" pairs >>= parseColor)
        , cfgEditorLineWrap     = fromMaybe (cfgEditorLineWrap defaultConfig)
                                    (lookup "editor_line_wrap" pairs >>= readMaybeBool)
        , cfgKeybindings        = [ (cmdId, fromMaybe defaultCombo (lookup cmdId pairs))
                                   | (cmdId, defaultCombo) <- defaultKeybindings ]
        }

-- | Parse a single line into a (key, value) pair.
--   Returns Nothing for blank lines, section headers, and comments.
parseLine :: String -> Maybe (String, String)
parseLine line =
    let stripped = dropWhile isSpace line
    in if null stripped || head stripped == '#' || head stripped == '['
       then Nothing
       else case break (== '=') stripped of
                (k, '=':v) -> Just (trim k, trim v)
                _           -> Nothing

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Parse a colour value.
--   Accepts:  #rrggbb   or   r, g, b   (floats 0.0-1.0)
parseColor :: String -> Maybe (Float, Float, Float)
parseColor s
    | "#" `isPrefixOf` s =
        let hex = fromMaybe "" (stripPrefix "#" s)
        in if length hex == 6
           then let r = hexPairToFloat (take 2 hex)
                    g = hexPairToFloat (take 2 (drop 2 hex))
                    b = hexPairToFloat (drop 4 hex)
                in Just (r, g, b)
           else Nothing
    | otherwise =
        case map (readMaybeFloat . trim) (splitOn ',' s) of
            [Just r, Just g, Just b] -> Just (r, g, b)
            _                        -> Nothing

hexPairToFloat :: String -> Float
hexPairToFloat h = fromIntegral (hexVal h) / 255.0
  where
    hexVal [a, b] = hexDigit a * 16 + hexDigit b
    hexVal _      = 0
    hexDigit c
        | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
        | c >= 'a' && c <= 'f' = 10 + fromEnum c - fromEnum 'a'
        | c >= 'A' && c <= 'F' = 10 + fromEnum c - fromEnum 'A'
        | otherwise             = 0

splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn delim str =
    let (w, rest) = break (== delim) str
    in w : case rest of
               []     -> []
               (_:xs) -> splitOn delim xs

readMaybeFloat :: String -> Maybe Float
readMaybeFloat s = case reads s of
    [(v, "")] -> Just v
    _         -> Nothing

readMaybeInt :: String -> Maybe Int
readMaybeInt s = case reads s of
    [(v, "")] -> Just v
    _         -> Nothing

readMaybeBool :: String -> Maybe Bool
readMaybeBool "true"  = Just True
readMaybeBool "false" = Just False
readMaybeBool _       = Nothing

-- | Convert config colour to a "r,g,b" string for the CMD protocol
bgColorToCmd :: (Float, Float, Float) -> String
bgColorToCmd (r, g, b) =
    "CMD:SET_BG:" ++ show r ++ "," ++ show g ++ "," ++ show b