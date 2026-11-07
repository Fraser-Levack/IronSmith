{-# LANGUAGE ScopedTypeVariables #-}
-- | Headless command-line mode: check, render and export .irsm models
--   without the TUI. Designed to be driven by scripts and AI agents (see
--   docs/AGENT_GUIDE.md): output is short and line-oriented, errors go to stderr with
--   a file:line prefix, and exit codes distinguish failure kinds.
module Cli (runCli, usage) where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.List (isPrefixOf, intercalate)
import Data.Maybe (fromMaybe)
import System.Directory (makeAbsolute, doesFileExist)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (replaceExtension)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (parse, errorBundlePretty)
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as C8

import AST (Script)
import Parser (pScript)
import Evaluator (validateScript, compileToBytecode)
import Exporter (estimateBounds)
import AppCore
import Config (loadConfig, cfgDefaultObjectColor, cfgExportResolution)

usage :: String
usage = unlines
    [ "IronSmith - code-driven 3D modelling"
    , ""
    , "Usage:"
    , "  ironsmith                              open the TUI editor"
    , "  ironsmith check  FILE.irsm             validate a script"
    , "  ironsmith render FILE.irsm [-o OUT.png] [--view YAW,PITCH,DIST[,X,Y,Z]]"
    , "                                         render a still image in the viewer"
    , "  ironsmith export FILE.irsm [-o OUT.obj] [--res N]"
    , "                                         export a Wavefront OBJ mesh"
    , "  ironsmith help                         show this help"
    , ""
    , "render: starts the viewer if it isn't running (and leaves it open so"
    , "  later renders are fast), shows the model there, and waits until the"
    , "  shader has compiled before saving. --view angles are in degrees;"
    , "  without it the camera frames the whole model from the front-right."
    , "  The viewer window's size sets the image size."
    , "Outputs default to the input path with a .png / .obj extension."
    , ""
    , "Exit codes: 0 ok, 1 script error, 2 bad usage, 3 viewer/IO failure."
    ]

runCli :: [String] -> IO ()
runCli args = case args of
    [cmd] | cmd `elem` ["help", "--help", "-h"] -> putStr usage
    ["check", file] -> do
        script <- loadScript file
        let bytecode = compileToBytecode (0, 0, 0) script
        putStrLn ("OK: " ++ file ++ " (" ++ show (length script) ++ " statements, "
                  ++ show (length bytecode `div` 4) ++ " bytecode instructions)")
        when (drawsNothing bytecode) $
            putStrLn "warning: the script draws nothing (no shape statements)"
    ("render" : file : opts) -> withOptions opts $ \o -> do
        view <- case lookup "--view" o of
            Nothing -> return Nothing
            Just spec -> maybe (usageError ("bad --view '" ++ spec ++ "'")) (return . Just)
                               (parseView spec)
        render file (lookup "-o" o) view
    ("export" : file : opts) -> withOptions opts $ \o -> do
        res <- case lookup "--res" o of
            Nothing -> return Nothing
            Just r -> maybe (usageError ("bad --res '" ++ r ++ "'")) (return . Just) (readMaybe r)
        export file (lookup "-o" o) res
    _ -> usageError ("unknown command: " ++ unwords args)

-- ─── COMMANDS ───────────────────────────────────────────────────────────────

render :: FilePath -> Maybe FilePath -> Maybe [Float] -> IO ()
render file out view = do
    script <- loadScript file
    cfg <- getConfigPath >>= loadConfig
    let bytecode = compileToBytecode (cfgDefaultObjectColor cfg) script
    when (drawsNothing bytecode) $
        failWith 1 (file ++ ": nothing to render (the script draws no shapes)")
    outPath <- makeAbsolute (fromMaybe (replaceExtension file "png") out)

    running <- viewerReachable
    unless running $ do
        launchViewerDetached
        up <- waitForViewer 20
        unless up $ failWith 3 "could not start the viewer (is ironsmith-viewer installed next to ironsmith or on PATH?)"
        -- A fresh viewer has default settings; match ironsmith.toml. A
        -- viewer that was already running keeps whatever the user set.
        mapM_ (sendViewer . C8.pack) (appearanceCommands cfg)

    sendViewer (bytecodeBytes bytecode)
    let poseRadians = viewToPose (fromMaybe [] view) (autoFrame bytecode)
        cmd = "CMD:SCREENSHOT:" ++ intercalate "," (map show poseRadians) ++ ":" ++ outPath
    -- Big scenes can take the GPU driver a while to compile.
    reply <- requestViewer 900 cmd
    case reply of
        Right r | "OK" `isPrefixOf` r -> putStrLn ("OK: wrote " ++ outPath)
        Right r -> failWith 3 ("viewer: " ++ r)
        Left e  -> failWith 3 e
  where
    sendViewer bytes = do
        r <- try (sendNetworkDataSync bytes)
        case r of
            Left (e :: SomeException) -> failWith 3 ("cannot reach the viewer: " ++ show e)
            Right () -> return ()

export :: FilePath -> Maybe FilePath -> Maybe Int -> IO ()
export file out res = do
    _ <- loadScript file
    code <- readFile file
    cfg <- getConfigPath >>= loadConfig
    outPath <- makeAbsolute (fromMaybe (replaceExtension file "obj") out)
    let resolution = fromMaybe (cfgExportResolution cfg) res
    result <- exportModelToOBJ (cfgDefaultObjectColor cfg) code outPath resolution (\_ -> return ())
    case result of
        Right p -> putStrLn ("OK: wrote " ++ p ++ " (resolution " ++ show resolution ++ ")")
        Left (e, line) -> failWith 1 (file ++ ":" ++ show line ++ ": " ++ e)

-- ─── HELPERS ────────────────────────────────────────────────────────────────

-- | Read, parse and validate a script, exiting with a file:line error.
loadScript :: FilePath -> IO Script
loadScript file = do
    exists <- doesFileExist file
    unless exists $ failWith 3 (file ++ ": no such file")
    code <- readFile file
    case parse pScript file code of
        Left bundle -> failWith 1 (errorBundlePretty bundle)
        Right script -> case validateScript script of
            Left (ident, msg) -> failWith 1 (file ++ ":" ++ show (findIdentLine ident code) ++ ": " ++ msg)
            Right () -> return script

-- | Bytecode that's just the halt (plus maybe light ops) draws nothing.
drawsNothing :: [Float] -> Bool
drawsNothing bytecode = all (`elem` [0, 40, 41]) (everyFourth bytecode)
  where
    everyFourth (x:xs) = x : everyFourth (drop 3 xs)
    everyFourth []     = []

-- | Default camera for a render: front-right three-quarter view (front is
--   +X), far enough back to fit the model's estimated bounds.
--   Returns [yawDeg, pitchDeg, dist, targetX, targetY, targetZ].
autoFrame :: [Float] -> [Float]
autoFrame bytecode = case estimateBounds bytecode of
    Nothing -> [35, 25, 20, 0, 0, 0]
    Just ((x0, y0, z0), (x1, y1, z1)) ->
        let radius = 0.5 * sqrt ((x1 - x0) ^ (2 :: Int) + (y1 - y0) ^ (2 :: Int) + (z1 - z0) ^ (2 :: Int))
            -- The raymarcher's vertical half-FOV is atan(1/1.5) ~ 33.7deg.
            dist = max 3 (radius / sin (atan (1 / 1.5)) * 1.05)
        in [35, 25, dist, (x0 + x1) / 2, (y0 + y1) / 2, (z0 + z1) / 2]

-- | Merge a user --view (3 or 6 numbers, degrees) over the automatic one
--   and convert to the viewer's radians.
viewToPose :: [Float] -> [Float] -> [Float]
viewToPose user auto =
    -- yaw and pitch are angles; dist and the target are lengths.
    zipWith ($) [rad, rad, id, id, id, id] (user ++ drop (length user) auto)
  where
    rad d = d * pi / 180

-- | "yaw,pitch,dist" or "yaw,pitch,dist,x,y,z".
parseView :: String -> Maybe [Float]
parseView spec = do
    nums <- mapM readNumber (splitOn ',' spec)
    if length nums `elem` [3, 6] then Just nums else Nothing
  where
    readNumber s = readMaybe (fixup (trim s))
    -- Haskell's read rejects "5." / ".5" / "+5"; normalise common forms.
    fixup s@('.':_) = '0' : s
    fixup ('-':'.':r) = "-0." ++ r
    fixup ('+':r) = r
    fixup s = if not (null s) && last s == '.' then s ++ "0" else s
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
    (a, [])       -> [a]
    (a, _ : rest) -> a : splitOn c rest

-- | Parse "-o X" / "--view X" / "--res X" pairs.
withOptions :: [String] -> ([(String, String)] -> IO ()) -> IO ()
withOptions opts k = go opts []
  where
    go [] acc = k acc
    go (flag : value : rest) acc
        | flag `elem` ["-o", "--view", "--res"] = go rest ((flag, value) : acc)
    go (flag : _) _ = usageError ("unexpected argument: " ++ flag)

usageError :: String -> IO a
usageError msg = do
    hPutStrLn stderr ("error: " ++ msg)
    hPutStrLn stderr ""
    hPutStrLn stderr usage
    exitWith (ExitFailure 2)

failWith :: Int -> String -> IO a
failWith code msg = hPutStrLn stderr ("error: " ++ msg) >> exitWith (ExitFailure code)
