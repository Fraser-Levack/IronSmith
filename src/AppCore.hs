{-# LANGUAGE ScopedTypeVariables #-}
module AppCore where

import System.Directory (doesFileExist, getAppUserDataDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Control.Exception (catch, SomeException)
import Control.Concurrent (forkIO) -- FIX: Allows background threads!

import Text.Megaparsec
import Text.Megaparsec.Pos (sourceLine, unPos) 

import AST
import Parser
import Evaluator

import System.Process (ProcessHandle, createProcess, proc, std_out, std_err, StdStream(CreatePipe), terminateProcess)

import Network.Socket
import Network.Socket.ByteString (sendAll)
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

-- | PERSISTENCE HELPERS
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

-- | SEND GLSL OVER TCP (Asynchronous!)
sendToViewer :: String -> IO ()
sendToViewer glsl = do
    -- FIX: forkIO spawns this on a background thread so the TUI never stutters!
    _ <- forkIO $ withSocketsDo $ do
        catch (do
            -- Pre-define hints to avoid slow DNS lookups on Windows
            let hints = defaultHints { addrFamily = AF_INET, addrSocketType = Stream }
            addr <- head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "7878")
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            connect sock (addrAddress addr)
            sendAll sock (C8.pack glsl)
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
            let glslData = compileToGLSL astScript
            
            -- Beam instantly to RAM (Now running in the background)
            sendToViewer glslData
            
            if isHardSave 
                then do
                    glslPath <- getGlslPath
                    writeFile glslPath glslData
                else return ()
            
            return Nothing