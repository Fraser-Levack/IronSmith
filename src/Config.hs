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
    } deriving (Show)

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
    }

-- | The default config file content (written on first run)
defaultConfigText :: String
defaultConfigText = unlines
    [ "# IronSmith Configuration"
    , "# Edit this file to customise your forge."
    , "# Restart IronSmith (or press Ctrl+G) for changes to take effect."
    , ""
    , "[viewer]"
    , ""
    , "# Background colour of the 3D viewport."
    , "# Use a hex colour (e.g. #0a0a1a) or three floats 0.0-1.0 (e.g. 0.02, 0.02, 0.05)"
    , "background_color = #05050d"
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
    , "[startup]"
    , ""
    , "# Launch the 3D viewer window automatically on startup (true/false)"
    , "launch_viewer_on_start = true"
    , ""
    , "# Automatically re-open the last edited file on startup (true/false)"
    , "restore_last_file = false"
    ]

-- ─── PARSER ──────────────────────────────────────────────────────────────────

-- | Load config from disk, writing defaults if it doesn't exist yet.
loadConfig :: FilePath -> IO IronConfig
loadConfig path = do
    exists <- doesFileExist path
    if exists
        then do
            raw <- readFile path
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
                                    (lookup "background_color" pairs >>= parseBgColor)
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

-- | Parse a background colour value.
--   Accepts:  #rrggbb   or   r, g, b   (floats 0.0-1.0)
parseBgColor :: String -> Maybe (Float, Float, Float)
parseBgColor s
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