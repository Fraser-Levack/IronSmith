{-# LANGUAGE ScopedTypeVariables #-}
module AppCore where

import System.Directory (doesFileExist, getAppUserDataDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Control.Exception (catch, SomeException)
import Control.Concurrent (forkIO) 

import Text.Megaparsec
import Text.Megaparsec.Pos (sourceLine, unPos) 

import AST
import Parser
import Evaluator

import System.Process (ProcessHandle, createProcess, proc, std_out, std_err, StdStream(CreatePipe), terminateProcess)

import Network.Socket
import Network.Socket.ByteString (sendAll)

-- NEW: Binary Serialization Imports
import Data.ByteString.Builder (floatLE, toLazyByteString)
import qualified Data.ByteString.Lazy as BL

import qualified Data.ByteString.Char8 as C8


launchViewer :: IO ProcessHandle
launchViewer = do
    let processConfig = (proc "IronSmith-Viewer.exe" []) 
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


-- | COMPILER BRIDGE
compileAndSave :: Bool -> String -> IO (Maybe (String, Int))
compileAndSave isHardSave code =
    case parse pScript "editor" code of
        Left bundle -> do
            let errStr = errorBundlePretty bundle
                firstErr = NE.head (bundleErrors bundle)
                (_, posState) = reachOffset (errorOffset firstErr) (bundlePosState bundle)
                lineNum = unPos (sourceLine (pstateSourcePos posState))
            return $ Just (errStr, lineNum)
            
        Right astScript -> do
            -- Generate the Bytecode instead of GLSL!
            let bytecode = compileToBytecode astScript
            
            -- Beam the raw bytes to the Rust SSBO
            sendBytecode bytecode
            
            -- (Skipping the hard-save logic for the output.glsl file for now, 
            -- since we aren't generating strings anymore)
            
            return Nothing