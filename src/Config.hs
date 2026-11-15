{-# LANGUAGE OverloadedStrings #-}
module Config where

import System.Directory (doesFileExist)
import System.IO (withFile, IOMode(..), hSetEncoding, hGetContents, hPutStr, utf8)
import Control.Exception (try, IOException)
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
    , cfgLaunchViewerOnStart :: Bool
    , cfgRestoreLastFile    :: Bool
    , cfgDefaultObjectColor :: (Float, Float, Float) -- default colour for unpainted shapes
    , cfgKeybindings        :: [(String, String)] -- command id -> key combo, e.g. "save" -> "ctrl+s"
    , cfgExportResolution   :: Int -- grid cells per axis for .obj export
    , cfgFilter             :: String -- post-process filter name ("none" = off)
    , cfgExposure           :: Float -- viewport brightness multiplier (1.0 = neutral)
    , cfgVideoFps           :: Int -- frames per second for animation video export
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
    , ("export_obj",       "ctrl+shift+e")
    , ("export_glb",       "ctrl+shift+g")
    , ("cycle_filter",     "ctrl+f")
    , ("toggle_animation", "ctrl+a")
    , ("export_video",     "ctrl+shift+v")
    , ("relaunch_viewer",  "ctrl+l")
    , ("undo",             "ctrl+z")
    , ("redo",             "ctrl+y")
    , ("copy",             "ctrl+c")
    , ("cut",              "ctrl+x")
    , ("paste",            "ctrl+v")
    ]

-- | Sensible defaults matching the current hardcoded values
defaultConfig :: IronConfig
defaultConfig = IronConfig
    { cfgBgColor            = (0.02, 0.02, 0.05)
    , cfgDefaultCameraDist  = 20.0
    , cfgAutoOrbit          = True
    , cfgShadowEnabled      = True
    , cfgMarchSteps         = 150
    , cfgAutoCompileDelayMs = 150
    , cfgLaunchViewerOnStart = True
    , cfgRestoreLastFile    = False
    , cfgDefaultObjectColor = (0.8, 0.4, 0.1) -- Forge Orange
    , cfgKeybindings        = defaultKeybindings
    , cfgExportResolution   = 128
    , cfgFilter             = "none"
    , cfgExposure           = 1.0
    , cfgVideoFps           = 30
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
    , "# Post-process filter applied to the viewport."
    , "# Filters are .glsl files in the \"filters\" folder next to this file;"
    , "# use the file name without extension, or \"none\" to disable."
    , "# Built-in examples: edge_detection, heatmap, ascii, passthrough."
    , "# Copy one of those files to create your own!"
    , "filter = none"
    , ""
    , "# Viewport exposure (brightness multiplier). 1.0 is neutral;"
    , "# raise it to brighten dim scenes (e.g. with a low-intensity"
    , "# light(...) in your script), lower it to darken."
    , "# Recommended range: 0.2 - 4.0"
    , "exposure = 1.0"
    , ""
    , "[editor]"
    , ""
    , "# Delay in milliseconds after you stop typing before the scene updates."
    , "# Changing only numbers (sizes, positions, colours) shows instantly, so"
    , "# this can be short; adding/removing shapes still compiles a shader."
    , "auto_compile_delay_ms = 150"
    , ""
    , "[startup]"
    , ""
    , "# Launch the 3D viewer window automatically on startup (true/false)."
    , "# If off (or you close the window), use 'Relaunch Viewer' (Ctrl+L)."
    , "launch_viewer_on_start = true"
    , ""
    , "# Re-open the most recently edited file on startup instead of showing"
    , "# the start screen (true/false)"
    , "restore_last_file = false"
    , ""
    , "[export]"
    , ""
    , "# Grid cells per axis when exporting meshes (.obj / .glb). Higher values produce"
    , "# much more detailed meshes but take significantly longer to export"
    , "# and produce larger files."
    , "# Recommended range: 64 (fast preview) - 256 (high detail, slow)"
    , "export_resolution = 128"
    , ""
    , "# Frames per second when exporting an animation to video."
    , "# Higher is smoother but exports take proportionally longer."
    , "video_fps = 30"
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

-- | Read a UTF-8 text file strictly, closing the handle before returning.
--   Lazy 'readFile' can leave the handle open until GC'd, which on Windows
--   causes a later 'writeFile' to the same path to fail with
--   "resource busy (file is locked)". The encoding is fixed to UTF-8 rather
--   than the system locale (e.g. a Windows code page), so a script with an
--   accented comment reads the same everywhere. A leading byte-order mark
--   (added by some Windows editors) is dropped.
readFileStrict :: FilePath -> IO String
readFileStrict path = withFile path ReadMode $ \h -> do
    hSetEncoding h utf8
    contents <- hGetContents h
    length contents `seq` return (dropBom contents)
  where
    dropBom ('\xFEFF':rest) = rest
    dropBom s = s

-- | Write a text file as UTF-8 (see 'readFileStrict').
writeFileUtf8 :: FilePath -> String -> IO ()
writeFileUtf8 path contents = withFile path WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h contents

-- | Load config from disk, writing defaults if it doesn't exist yet. Never
--   throws: an unreadable file or a folder we can't write to just means
--   running with the defaults.
loadConfig :: FilePath -> IO IronConfig
loadConfig path = do
    exists <- doesFileExist path
    if exists
        then either (const defaultConfig) parseConfig <$> tryIO' (readFileStrict path)
        else do
            _ <- tryIO' (writeFileUtf8 path defaultConfigText)
            return defaultConfig
  where
    tryIO' :: IO a -> IO (Either IOException a)
    tryIO' = try

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
        , cfgLaunchViewerOnStart = fromMaybe (cfgLaunchViewerOnStart defaultConfig)
                                    (lookup "launch_viewer_on_start" pairs >>= readMaybeBool)
        , cfgRestoreLastFile    = fromMaybe (cfgRestoreLastFile defaultConfig)
                                    (lookup "restore_last_file" pairs >>= readMaybeBool)
        , cfgDefaultObjectColor = fromMaybe (cfgDefaultObjectColor defaultConfig)
                                    (lookup "default_object_color" pairs >>= parseColor)
        , cfgKeybindings        = [ (cmdId, fromMaybe defaultCombo (lookup cmdId pairs))
                                   | (cmdId, defaultCombo) <- defaultKeybindings ]
        , cfgExportResolution   = fromMaybe (cfgExportResolution defaultConfig)
                                    (lookup "export_resolution" pairs >>= readMaybeInt)
        , cfgFilter             = fromMaybe (cfgFilter defaultConfig)
                                    (lookup "filter" pairs)
        , cfgExposure           = fromMaybe (cfgExposure defaultConfig)
                                    (lookup "exposure" pairs >>= readMaybeFloat)
        , cfgVideoFps           = fromMaybe (cfgVideoFps defaultConfig)
                                    (lookup "video_fps" pairs >>= readMaybeInt)
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