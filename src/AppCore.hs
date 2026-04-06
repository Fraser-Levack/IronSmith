module AppCore where

import System.Directory (doesFileExist, getAppUserDataDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import Data.List (nub)
import qualified Data.List.NonEmpty as NE

-- FIX: We import the core Megaparsec module and just the Position utilities
import Text.Megaparsec
import Text.Megaparsec.Pos (sourceLine, unPos) 

import AST
import Parser
import Evaluator

-- | PERSISTENCE HELPERS
getCachePath :: IO FilePath
getCachePath = do
    configDir <- getAppUserDataDirectory "ironsmith"
    createDirectoryIfMissing True configDir 
    return (configDir </> ".ironsmith_recents")

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
                -- 1. Grab the first error in the bundle
                firstErr = NE.head (bundleErrors bundle)
                -- 2. Fast-forward the parser state to exactly where the error happened
                (_, posState) = reachOffset (errorOffset firstErr) (bundlePosState bundle)
                -- 3. Extract the true line number
                lineNum = unPos (sourceLine (pstateSourcePos posState))
                
            return $ Just (errStr, lineNum)
            
        Right astScript -> do
            let glslData = compileToGLSL astScript
            writeFile "output.glsl" glslData
            return Nothing