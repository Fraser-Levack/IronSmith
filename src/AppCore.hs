module AppCore where

import System.Directory (doesFileExist, getAppUserDataDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import Data.List (nub)
import qualified Data.List.NonEmpty as NE

import Text.Megaparsec
import Text.Megaparsec.Pos (sourceLine, unPos) 

import AST
import Parser
import Evaluator

import System.Process (ProcessHandle, createProcess, proc, std_out, std_err, StdStream(CreatePipe), terminateProcess)

launchViewer :: IO ProcessHandle
launchViewer = do
    -- Using 'proc' and 'createProcess' lets us capture stdout and stderr
    -- 'CreatePipe' swallows the output so it doesn't corrupt the Brick TUI
    let processConfig = (proc "IronSmith-Viewer.exe" []) 
            { std_out = CreatePipe
            , std_err = CreatePipe 
            }
    
    -- We ignore the stdin/stdout/stderr handles returned, we just want the ProcessHandle
    (_, _, _, handle) <- createProcess processConfig
    return handle

stopViewer :: Maybe ProcessHandle -> IO ()
stopViewer Nothing = return ()
stopViewer (Just h) = terminateProcess h

-- | PERSISTENCE HELPERS

-- 1. Grab the global OS folder and make sure it exists
getConfigDir :: IO FilePath
getConfigDir = do
    configDir <- getAppUserDataDirectory "ironsmith"
    createDirectoryIfMissing True configDir 
    return configDir

-- 2. Path for the recent files cache
getCachePath :: IO FilePath
getCachePath = do
    dir <- getConfigDir
    return (dir </> ".ironsmith_recents")

-- 3. Path for the hidden GLSL output
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

-- | COMPILER BRIDGE
compileAndSave :: String -> IO (Maybe (String, Int))
compileAndSave code =
    case parse pScript "editor" code of
        Left bundle -> do
            let errStr = errorBundlePretty bundle
                firstErr = NE.head (bundleErrors bundle)
                (_, posState) = reachOffset (errorOffset firstErr) (bundlePosState bundle)
                lineNum = unPos (sourceLine (pstateSourcePos posState))
                
            return $ Just (errStr, lineNum)
            
        Right astScript -> do
            let glslData = compileToGLSL astScript
            
            -- FIX: Save the GLSL to the hidden global config folder instead of CWD!
            glslPath <- getGlslPath
            writeFile glslPath glslData
            
            return Nothing