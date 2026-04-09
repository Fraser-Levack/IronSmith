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

-- --- FIX: GLOBAL DEMO PATH ---
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

-- | SEND GLSL OVER TCP
sendToViewer :: String -> IO ()
sendToViewer glsl = do
    _ <- forkIO $ withSocketsDo $ do
        catch (do
            sock <- socket AF_INET Stream 0
            let addr = SockAddrInet 7878 (tupleToHostAddress (127, 0, 0, 1))
            connect sock addr
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
            
            -- Beam instantly to RAM
            sendToViewer glslData
            
            if isHardSave 
                then do
                    glslPath <- getGlslPath
                    writeFile glslPath glslData
                else return ()
            
            return Nothing