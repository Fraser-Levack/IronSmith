{-# LANGUAGE ScopedTypeVariables #-}
module AppCore where

import System.Directory (doesFileExist, getAppUserDataDirectory, createDirectoryIfMissing)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory)
import System.Info (os)
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Control.Exception (catch, SomeException)
import Control.Concurrent (forkIO) 

import Text.Megaparsec
import Text.Megaparsec.Pos (sourceLine, unPos) 

import AST
import Parser
import Evaluator
import Exporter (exportToOBJ)
import Config

import System.Process (ProcessHandle, createProcess, proc, std_out, std_err, StdStream(CreatePipe), terminateProcess)

import Network.Socket
import Network.Socket.ByteString (sendAll)

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

-- | Launch the Rust viewer. Looks for it alongside this executable first
--   (where the release package puts it), falling back to the system PATH
--   for development builds.
launchViewer :: IO ProcessHandle
launchViewer = do
    exeDir <- takeDirectory <$> getExecutablePath
    let bundledPath = exeDir </> viewerExeName
    bundled <- doesFileExist bundledPath
    let viewerPath = if bundled then bundledPath else viewerExeName
        processConfig = (proc viewerPath [])
            { std_out = CreatePipe
            , std_err = CreatePipe
            }
    (_, _, _, handle) <- createProcess processConfig
    return handle

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

-- | 1. SEND RAW BINARY FLOATS
sendBytecode :: [Float] -> IO ()
sendBytecode floats = do
    let lazyBytes = toLazyByteString $ mconcat (map floatLE floats)
        strictBytes = BL.toStrict lazyBytes
    sendNetworkData strictBytes

-- | 2. SEND TEXT COMMANDS
sendCommand :: String -> IO ()
sendCommand cmd = do
    sendNetworkData (C8.pack cmd)

-- | SHARED TCP HELPER
sendNetworkData :: C8.ByteString -> IO ()
sendNetworkData bytes = do
    _ <- forkIO $ withSocketsDo $ do
        catch (do
            sock <- socket AF_INET Stream 0
            let addr = SockAddrInet 7878 (tupleToHostAddress (127, 0, 0, 1))
            connect sock addr
            sendAll sock bytes
            close sock
            ) (\(e :: SomeException) -> return ())
    return ()

sendConfig :: IronConfig -> IO ()
sendConfig cfg = do
    -- Background colour
    sendCommand (bgColorToCmd (cfgBgColor cfg))
    -- Camera distance
    sendCommand ("CMD:SET_CAMERA_DIST:" ++ show (cfgDefaultCameraDist cfg))
    -- Auto orbit
    sendCommand (if cfgAutoOrbit cfg then "CMD:OrbitMode" else "CMD:StaticMode")
    -- Shadow toggle
    sendCommand (if cfgShadowEnabled cfg then "CMD:SHADOW_ON" else "CMD:SHADOW_OFF")
    -- March steps
    sendCommand ("CMD:SET_MARCH_STEPS:" ++ show (cfgMarchSteps cfg))


-- | COMPILER BRIDGE
compileAndSave :: (Float, Float, Float) -> Bool -> String -> IO (Maybe (String, Int))
compileAndSave defaultColor isHardSave code =
    case parse pScript "editor" code of
        Left bundle -> do
            let errStr = errorBundlePretty bundle
                firstErr = NE.head (bundleErrors bundle)
                (_, posState) = reachOffset (errorOffset firstErr) (bundlePosState bundle)
                lineNum = unPos (sourceLine (pstateSourcePos posState))
            return $ Just (errStr, lineNum)

        Right astScript -> do
            -- Generate the Bytecode instead of GLSL!
            let bytecode = compileToBytecode defaultColor astScript

            -- Beam the raw bytes to the Rust SSBO
            sendBytecode bytecode

            -- (Skipping the hard-save logic for the output.glsl file for now,
            -- since we aren't generating strings anymore)

            return Nothing

-- | EXPORT BRIDGE
--   Parses and compiles the script just like 'compileAndSave', then meshes
--   the resulting SDF scene and writes it out as a Wavefront .obj file.
exportModelToOBJ :: (Float, Float, Float) -> String -> FilePath -> Int -> (Float -> IO ()) -> IO (Either (String, Int) FilePath)
exportModelToOBJ defaultColor code objPath resolution report =
    case parse pScript "editor" code of
        Left bundle -> do
            let errStr = errorBundlePretty bundle
                firstErr = NE.head (bundleErrors bundle)
                (_, posState) = reachOffset (errorOffset firstErr) (bundlePosState bundle)
                lineNum = unPos (sourceLine (pstateSourcePos posState))
            return $ Left (errStr, lineNum)

        Right astScript -> do
            let bytecode = compileToBytecode defaultColor astScript
            exportToOBJ bytecode objPath resolution report
            return $ Right objPath