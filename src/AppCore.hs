{-# LANGUAGE ScopedTypeVariables #-}
module AppCore where

import System.Directory (doesFileExist, getAppUserDataDirectory, createDirectoryIfMissing,
                         getHomeDirectory, getXdgDirectory, XdgDirectory(XdgConfig), copyFile)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory)
import System.Info (os)
import Data.Char (isAlphaNum)
import Data.List (nub, intercalate)
import Data.Maybe (isJust)
import Data.Version (showVersion)
import qualified Data.List.NonEmpty as NE
import Control.Exception (catch, bracket, evaluate, try, SomeException, IOException)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (when, forM_, void)
import System.Timeout (timeout)

import Paths_ironsmith (version)

import Text.Megaparsec hiding (try)
import Text.Megaparsec.Pos (sourceLine, unPos) 

import AST
import Parser
import Evaluator
import Exporter (exportToOBJ, exportToGLB, exportToSTL, exportRadius)
import Config

import System.Process (ProcessHandle, CreateProcess(..), createProcess, proc, StdStream(CreatePipe, Inherit, NoStream), terminateProcess, getProcessExitCode)

import Network.Socket
import Network.Socket.ByteString (sendAll, recv)

-- NEW: Binary Serialization Imports
import Data.ByteString.Builder (floatLE, toLazyByteString)
import qualified Data.ByteString.Lazy as BL

import qualified Data.ByteString.Char8 as C8


-- | The viewer binary's name on this platform. Windows builds get a
--   ".exe" suffix; Linux/macOS builds don't.
viewerExeName :: String
viewerExeName
    | os == "mingw32" = "ironsmith-viewer.exe"
    | otherwise       = "ironsmith-viewer"

-- | Where to find the Rust viewer: alongside this executable first (where
--   the release package puts it), falling back to the system PATH for
--   development builds.
viewerPath :: IO FilePath
viewerPath = do
    exeDir <- takeDirectory <$> getExecutablePath
    let bundledPath = exeDir </> viewerExeName
    bundled <- doesFileExist bundledPath
    return (if bundled then bundledPath else viewerExeName)

-- | Launch the viewer as a child of the TUI (stopped when the TUI exits).
--   Throws if the binary can't be started; see 'ensureViewer'.
launchViewer :: IO ProcessHandle
launchViewer = do
    path <- viewerPath
    (_, _, _, handle) <- createProcess (proc path [])
        { std_out = CreatePipe
        , std_err = CreatePipe
        }
    return handle

-- | Make sure a viewer is listening, for the TUI. A viewer that is already
--   running (e.g. one a CLI `render` left open) is reused, and Nothing is
--   returned because it isn't ours to stop. Otherwise one is launched and
--   we wait until it accepts connections, giving up early if it exits
--   (no GPU, missing libraries...). Never throws: a missing or broken
--   viewer becomes an error message the editor can show.
ensureViewer :: IO (Either String (Maybe ProcessHandle))
ensureViewer = do
    running <- viewerReachable
    if running then return (Right Nothing) else do
        launched <- try launchViewer
        case launched of
            Left (e :: SomeException) ->
                return (Left ("Could not start " ++ viewerExeName ++ ": " ++ show e
                              ++ " (it must be next to ironsmith or on your PATH)"))
            Right h -> do
                up <- waitUntilUp h (100 :: Int) -- 10 seconds
                return $ if up
                    then Right (Just h)
                    else Left ("The viewer didn't start (see forge.log in the config folder)."
                               ++ " Run 'Relaunch Viewer' from the command palette to retry.")
  where
    waitUntilUp h n = do
        up <- viewerReachable
        exited <- getProcessExitCode h
        case () of
            _ | up                 -> return True
              | isJust exited      -> return False
              | n <= 0             -> terminateProcess h >> return False
              | otherwise          -> threadDelay 100000 >> waitUntilUp h (n - 1)

-- | This build's version, from the cabal file, e.g. "1.0.0".
versionString :: String
versionString = showVersion version

-- | Keep a configured export resolution within the supported range.
clampExportResolution :: Int -> Int
clampExportResolution = max minExportResolution . min maxExportResolution

-- | Run a file operation, turning IO errors (missing folder, permissions,
--   a file locked by another program, bad encoding...) into a message.
tryIO :: IO a -> IO (Either String a)
tryIO act = do
    r <- try act
    return $ case r of
        Left (e :: IOException) -> Left (show e)
        Right v                 -> Right v

-- | Launch a viewer that outlives this process, for the CLI: later
--   `ironsmith render` calls then reuse the already-open window instead of
--   paying the startup cost each time.
launchViewerDetached :: IO ()
launchViewerDetached = do
    path <- viewerPath
    -- The long-lived viewer must not keep our stdout/stderr open, or a
    -- caller capturing `ironsmith render` output through a pipe (e.g.
    -- Python's subprocess.run(capture_output=True)) never sees EOF and
    -- hangs until the viewer is closed. On POSIX, NoStream + close_fds
    -- closes them in the child. On Windows the process library only turns
    -- handle inheritance off when close_fds is set *and* every std stream
    -- is Inherit; the viewer then starts with no std handles, which is
    -- fine because it logs to forge.log.
    let stdio = if os == "mingw32" then Inherit else NoStream
    _ <- createProcess (proc path [])
        { std_in = stdio
        , std_out = stdio
        , std_err = stdio
        , create_group = True
        , new_session = True     -- POSIX: survive the parent's terminal closing
        , detach_console = True  -- Windows: no console tied to ours
        , close_fds = True
        }
    return ()

stopViewer :: Maybe ProcessHandle -> IO ()
stopViewer Nothing = return ()
stopViewer (Just h) = terminateProcess h

-- | The IronSmith config folder. It must be the same folder the viewer
--   uses (Rust's `dirs::config_dir()/ironsmith`), since the viewer installs
--   filters and writes forge.log there and the TUI lists/reads them:
--
--   * Windows: %APPDATA%\ironsmith
--   * macOS:   ~/Library/Application Support/ironsmith
--   * Linux:   $XDG_CONFIG_HOME/ironsmith (default ~/.config/ironsmith)
--
--   Before 1.0 the TUI used ~/.ironsmith on Linux and macOS; settings and
--   recent files from there are copied over once (see 'migrateLegacyConfig').
getConfigDir :: IO FilePath
getConfigDir = do
    configDir <- platformConfigDir
    createDirectoryIfMissing True configDir
    migrateLegacyConfig configDir
    return configDir

platformConfigDir :: IO FilePath
platformConfigDir
    | os == "mingw32" = getAppUserDataDirectory "ironsmith"
    | os == "darwin"  = (\h -> h </> "Library" </> "Application Support" </> "ironsmith") <$> getHomeDirectory
    | otherwise       = getXdgDirectory XdgConfig "ironsmith"

-- | Copy settings, recent files and the startup demo from the pre-1.0
--   location (~/.ironsmith) if the new folder doesn't have settings yet.
--   The old folder is left in place. Failures are ignored: the worst case
--   is starting with default settings.
migrateLegacyConfig :: FilePath -> IO ()
migrateLegacyConfig newDir = do
    legacy <- getAppUserDataDirectory "ironsmith"
    alreadySet <- doesFileExist (newDir </> "ironsmith.toml")
    legacyHas <- doesFileExist (legacy </> "ironsmith.toml")
    when (legacy /= newDir && not alreadySet && legacyHas) $
        forM_ ["ironsmith.toml", ".ironsmith_recents", "demo.irsm"] $ \f -> do
            exists <- doesFileExist (legacy </> f)
            when exists $ void $ tryIO (copyFile (legacy </> f) (newDir </> f))

getCachePath :: IO FilePath
getCachePath = do
    dir <- getConfigDir
    return (dir </> ".ironsmith_recents")

getGlslPath :: IO FilePath
getGlslPath = do
    dir <- getConfigDir
    return (dir </> "output.glsl")

getDemoPath :: IO FilePath
getDemoPath = do
    dir <- getConfigDir
    return (dir </> "demo.irsm")

getConfigPath :: IO FilePath
getConfigPath = do
    dir <- getConfigDir
    return (dir </> "ironsmith.toml")

-- | Where the viewer installs and loads post-process filters from.
--   The viewer creates this and fills it with the built-in examples
--   on first launch.
getFiltersDir :: IO FilePath
getFiltersDir = do
    dir <- getConfigDir
    return (dir </> "filters")

loadRecents :: IO [FilePath]
loadRecents = do
    cachePath <- getCachePath
    exists <- doesFileExist cachePath
    if exists
        then either (const []) (filter (not . null) . lines) <$> tryIO (readFileStrict cachePath)
        else return []

-- | Put `path` at the top of the recent-files list. Failing to save the
--   list is harmless, so it never throws.
saveRecent :: FilePath -> [FilePath] -> IO [FilePath]
saveRecent path oldRecents = do
    cachePath <- getCachePath
    let newRecents = take 5 $ nub (path : oldRecents)
    _ <- tryIO (writeFileUtf8 cachePath (unlines newRecents))
    return newRecents

-- | Bytecode as the little-endian f32 stream the viewer expects.
bytecodeBytes :: [Float] -> C8.ByteString
bytecodeBytes floats = BL.toStrict (toLazyByteString (mconcat (map floatLE floats)))

-- | 1. SEND RAW BINARY FLOATS
sendBytecode :: [Float] -> IO ()
sendBytecode = sendNetworkData . bytecodeBytes

-- | 2. SEND TEXT COMMANDS
sendCommand :: String -> IO ()
sendCommand cmd = do
    sendNetworkData (C8.pack cmd)

-- | The viewer's command socket.
viewerAddr :: SockAddr
viewerAddr = SockAddrInet 7878 (tupleToHostAddress (127, 0, 0, 1))

-- | Run an action on a fresh connection to the viewer, always closing it.
withViewerSocket :: (Socket -> IO a) -> IO a
withViewerSocket act = withSocketsDo $
    bracket (socket AF_INET Stream 0) close $ \sock -> do
        connect sock viewerAddr
        act sock

-- | Fire-and-forget send (the TUI's path): never blocks the UI, and a
--   viewer that isn't running is silently ignored.
sendNetworkData :: C8.ByteString -> IO ()
sendNetworkData bytes = do
    _ <- forkIO $ sendNetworkDataSync bytes `catch` \(_ :: SomeException) -> return ()
    return ()

-- | Send and return once the bytes are written. Throws if the viewer is
--   unreachable. The CLI uses this so it doesn't exit before sending.
--
--   The payload is forced *before* connecting: the viewer gives each
--   connection 250ms to deliver its message, and a lazily built payload
--   (e.g. a big scene's bytecode) would otherwise be computed while the
--   socket sits idle, so the viewer closes it and the message is lost.
sendNetworkDataSync :: C8.ByteString -> IO ()
sendNetworkDataSync bytes = do
    payload <- evaluate bytes
    withViewerSocket (`sendAll` payload)

-- | Send a command and wait (up to `seconds`) for the viewer's one-line
--   reply ("OK ..." / "ERROR ..."). Half-closing our side tells the viewer
--   the request is complete while keeping the connection open for the reply.
requestViewer :: Int -> String -> IO (Either String String)
requestViewer seconds cmd = do
    -- Build the request before connecting (see sendNetworkDataSync): the
    -- CLI's screenshot command embeds an auto-framed camera pose, and
    -- computing that lazily after connect took ~1s on a 100-shape scene,
    -- long enough for the viewer to drop the idle connection.
    payload <- evaluate (C8.pack cmd)
    result <- timeout (seconds * 1000000) (withViewerSocket (exchange payload))
        `catch` \(e :: SomeException) -> return (Just (Left ("cannot reach the viewer: " ++ show e)))
    return $ case result of
        Nothing    -> Left ("no reply from the viewer within " ++ show seconds ++ "s")
        Just reply -> reply
  where
    exchange payload sock = do
        sendAll sock payload
        shutdown sock ShutdownSend
        Right . trimReply . C8.unpack . C8.concat <$> recvAll sock
    recvAll sock = do
        piece <- recv sock 4096
        if C8.null piece then return [] else (piece :) <$> recvAll sock
    trimReply = reverse . dropWhile (`elem` "\r\n ") . reverse

-- | Is a viewer listening right now?
viewerReachable :: IO Bool
viewerReachable =
    (withViewerSocket (const (return ())) >> return True)
        `catch` \(_ :: SomeException) -> return False

-- | Poll until the viewer accepts connections, for up to `seconds`.
waitForViewer :: Int -> IO Bool
waitForViewer seconds = go (seconds * 10 :: Int)
  where
    go n = do
        up <- viewerReachable
        if up || n <= 0 then return up else threadDelay 100000 >> go (n - 1)

-- | Commands that make the viewer look the way ironsmith.toml says
--   (colours, shadows, quality, filter) without touching the camera.
appearanceCommands :: IronConfig -> [String]
appearanceCommands cfg =
    [ bgColorToCmd (cfgBgColor cfg)
    , if cfgShadowEnabled cfg then "CMD:SHADOW_ON" else "CMD:SHADOW_OFF"
    , "CMD:SET_MARCH_STEPS:" ++ show (cfgMarchSteps cfg)
    , "CMD:SET_FILTER:" ++ cfgFilter cfg
    , "CMD:SET_EXPOSURE:" ++ show (cfgExposure cfg)
    ]

sendConfig :: IronConfig -> IO ()
sendConfig cfg = mapM_ sendCommand $
    [ "CMD:SET_CAMERA_DIST:" ++ show (cfgDefaultCameraDist cfg)
    , if cfgAutoOrbit cfg then "CMD:OrbitMode" else "CMD:StaticMode"
    ] ++ appearanceCommands cfg


-- | Find the first line (1-based) where an identifier appears as a whole
--   token, so validation errors can highlight it in the editor. Matching
--   whole tokens matters: a half-typed "cub" must point at its own line,
--   not at a "cube(...)" earlier in the file.
findIdentLine :: String -> String -> Int
findIdentLine name code =
    case [ i | (i, l) <- zip [1 ..] (lines code), name `elem` identTokens l ] of
        (i:_) -> i
        []    -> 0
  where
    isIdentChar c = isAlphaNum c || c == '_'
    identTokens [] = []
    identTokens s@(c:_)
        | isIdentChar c = let (tok, rest) = span isIdentChar s
                          in tok : identTokens rest
        | otherwise     = identTokens (dropWhile (not . isIdentChar) s)

-- | Parse then validate, sharing the error format both the live compiler
--   and the OBJ exporter report to the editor.
parseAndValidate :: String -> Either (String, Int) Script
parseAndValidate = parseAndValidateNamed "editor"

-- | 'parseAndValidate' with the file name used in syntax error headers.
parseAndValidateNamed :: FilePath -> String -> Either (String, Int) Script
parseAndValidateNamed name code =
    case parse pScriptLines name code of
        Left bundle ->
            let errStr = errorBundlePretty bundle
                firstErr = NE.head (bundleErrors bundle)
                (_, posState) = reachOffset (errorOffset firstErr) (bundlePosState bundle)
                lineNum = unPos (sourceLine (pstateSourcePos posState))
            in Left (errStr, lineNum)

        Right numbered ->
            let astScript = map snd numbered
            in case validateScriptAt astScript of
                -- Search for the name from the failing statement's first
                -- line, so an earlier, valid use of it isn't highlighted.
                Left (i, ident, msg) ->
                    let from = fst (numbered !! i)
                        within = findIdentLine ident (unlines (drop (from - 1) (lines code)))
                    in Left (msg, if within > 0 then from + within - 1 else findIdentLine ident code)
                Right () -> astScript <$ validateValues numbered

-- | COMPILER BRIDGE
compileAndSave :: (Float, Float, Float) -> Bool -> String -> IO (Maybe (String, Int))
compileAndSave defaultColor isHardSave code =
    case parseAndValidate code of
        Left err -> return (Just err)

        Right astScript -> do
            -- Generate the Bytecode instead of GLSL!
            let bytecode = compileToBytecode defaultColor astScript

            -- Beam the raw bytes to the Rust SSBO
            sendBytecode bytecode

            -- Camera keyframes ride alongside the geometry as a command.
            -- Always sent, so removing the last camera(...) statement
            -- clears the animation in the viewer too.
            sendCommand (animTrackCmd astScript)

            -- (Skipping the hard-save logic for the output.glsl file for now,
            -- since we aren't generating strings anymore)

            return Nothing

-- | Serialize a script's camera keyframes for the viewer:
--   CMD:SET_ANIM:t,yaw,pitch,dist,tx,ty,tz;t,...  (angles in radians)
animTrackCmd :: Script -> String
animTrackCmd script =
    "CMD:SET_ANIM:" ++ intercalate ";" (map (intercalate "," . map show) (cameraTrack script))

-- | True if the script defines a camera animation worth playing/exporting.
scriptHasAnimation :: Script -> Bool
scriptHasAnimation = not . null . cameraTrack

-- | EXPORT BRIDGE
--   Parses and compiles the script just like 'compileAndSave', then meshes
--   the resulting SDF scene with `exporter` and writes it to `outPath`.
--   Fails (writing nothing) if the script has errors or draws nothing.
exportModelWith
    :: ([Float] -> FilePath -> Int -> (Float -> IO ()) -> IO Bool)
    -> (Float, Float, Float) -> String -> FilePath -> Int -> (Float -> IO ())
    -> IO (Either (String, Int) FilePath)
exportModelWith exporter defaultColor code outPath resolution report =
    case parseAndValidate code of
        Left err -> return (Left err)
        Right astScript -> do
            let bytecode = compileToBytecode defaultColor astScript
            wrote <- exporter bytecode outPath resolution report
            return $ if wrote then Right outPath else Left nothingToExport

-- | Wavefront .obj: geometry only.
exportModelToOBJ :: (Float, Float, Float) -> String -> FilePath -> Int -> (Float -> IO ()) -> IO (Either (String, Int) FilePath)
exportModelToOBJ = exportModelWith exportToOBJ

-- | Binary glTF (.glb): keeps each shape's paint colour and material
--   (for three.js, Blender etc.).
exportModelToGLB :: (Float, Float, Float) -> String -> FilePath -> Int -> (Float -> IO ()) -> IO (Either (String, Int) FilePath)
exportModelToGLB = exportModelWith exportToGLB

-- | Binary STL for 3D printing: geometry only, rotated to Z-up.
exportModelToSTL :: (Float, Float, Float) -> String -> FilePath -> Int -> (Float -> IO ()) -> IO (Either (String, Int) FilePath)
exportModelToSTL = exportModelWith exportToSTL

-- | Export error for a scene with no surface to mesh. No file is written.
nothingToExport :: (String, Int)
nothingToExport =
    ("Nothing to export: the script draws no shapes (or none within "
        ++ show (round exportRadius :: Int) ++ " units of the origin)", 0)

-- | Smallest and largest mesh export resolutions accepted (grid cells
--   across the model's longest side).
minExportResolution, maxExportResolution :: Int
minExportResolution = 8
maxExportResolution = 1024
