module AppCore where

-- FIX: Added getAppUserDataDirectory and createDirectoryIfMissing
import System.Directory (doesFileExist, getAppUserDataDirectory, createDirectoryIfMissing)
import System.FilePath ((</>)) -- Safely joins paths with \ or /
import Data.List (nub)
import Text.Megaparsec (parse, errorBundlePretty)

import AST
import Parser
import Evaluator

-- | PERSISTENCE HELPERS
-- Finds the global OS-specific config folder (e.g., AppData/Roaming/ironsmith)
getCachePath :: IO FilePath
getCachePath = do
    configDir <- getAppUserDataDirectory "ironsmith"
    createDirectoryIfMissing True configDir -- Make sure the folder exists!
    return (configDir </> ".ironsmith_recents")

-- Reads the hidden global cache file
loadRecents :: IO [FilePath]
loadRecents = do
    cachePath <- getCachePath
    exists <- doesFileExist cachePath
    if exists
        then lines <$> readFile cachePath
        else return []

-- Adds a file to the top of the list, removes duplicates, keeps max 5, and saves globally
saveRecent :: FilePath -> [FilePath] -> IO [FilePath]
saveRecent path oldRecents = do
    cachePath <- getCachePath
    let newRecents = take 5 $ nub (path : oldRecents)
    writeFile cachePath (unlines newRecents)
    return newRecents

-- | COMPILER BRIDGE
-- Takes raw code, parses it, compiles it, writes GLSL to disk, returns Error String if failed
compileAndSave :: String -> IO (Maybe String)
compileAndSave code =
    case parse pScript "editor" code of
        Left bundle -> return $ Just (errorBundlePretty bundle)
        Right astScript -> do
            let glslData = compileToGLSL astScript
            
            -- This still saves to the Current Working Directory (like Vim!)
            writeFile "output.glsl" glslData
            
            return Nothing