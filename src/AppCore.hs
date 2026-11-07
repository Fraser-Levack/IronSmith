{-# LANGUAGE ScopedTypeVariables #-}
module AppCore where

import System.Directory (doesFileExist, getAppUserDataDirectory, createDirectoryIfMissing)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory)
import System.Info (os)
import Data.Char (isAlphaNum)
import Data.List (nub, intercalate)
import qualified Data.List.NonEmpty as NE
import Control.Exception (catch, bracket, evaluate, SomeException)
import Control.Concurrent (forkIO, threadDelay)
import System.Timeout (timeout)

import Text.Megaparsec
import Text.Megaparsec.Pos (sourceLine, unPos) 

import AST
import Parser
import Evaluator
import Exporter (exportToOBJ)
import Config

import System.Process (ProcessHandle, CreateProcess(..), createProcess, proc, StdStream(CreatePipe, Inherit, NoStream), terminateProcess)

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
launchViewer :: IO ProcessHandle
launchViewer = do
    path <- viewerPath
    (_, _, _, handle) <- createProcess (proc path [])
        { std_out = CreatePipe
        , std_err = CreatePipe
        }
    return handle

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

getConfigDir :: IO FilePath
getConfigDir = do
    configDir <- getAppUserDataDirectory "ironsmith"
    createDirectoryIfMissing True configDir 
    return configDir

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
        then lines <$> readFile cachePath
        else return []

saveRecent :: FilePath -> [FilePath] -> IO [FilePath]
saveRecent path oldRecents = do
    cachePath <- getCachePath
    let newRecents = take 5 $ nub (path : oldRecents)
    writeFile cachePath (unlines newRecents)
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
parseAndValidate code =
    case parse pScript "editor" code of
        Left bundle ->
            let errStr = errorBundlePretty bundle
                firstErr = NE.head (bundleErrors bundle)
                (_, posState) = reachOffset (errorOffset firstErr) (bundlePosState bundle)
                lineNum = unPos (sourceLine (pstateSourcePos posState))
            in Left (errStr, lineNum)

        Right astScript ->
            case validateScript astScript of
                Left (ident, msg) -> Left (msg, findIdentLine ident code)
                Right ()          -> Right astScript

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
--   the resulting SDF scene and writes it out as a Wavefront .obj file.
exportModelToOBJ :: (Float, Float, Float) -> String -> FilePath -> Int -> (Float -> IO ()) -> IO (Either (String, Int) FilePath)
exportModelToOBJ defaultColor code objPath resolution report =
    case parseAndValidate code of
        Left err -> return (Left err)

        Right astScript -> do
            let bytecode = compileToBytecode defaultColor astScript
            exportToOBJ bytecode objPath resolution report
            return $ Right objPath