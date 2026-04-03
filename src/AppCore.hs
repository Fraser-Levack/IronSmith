module AppCore where

import System.Directory (doesFileExist)
import Data.List (nub)
import Text.Megaparsec (parse, errorBundlePretty)

import AST
import Parser
import Evaluator

-- Reads the hidden cache file
loadRecents :: IO [FilePath]
loadRecents = do
    exists <- doesFileExist ".ironsmith_recents"
    if exists
        then lines <$> readFile ".ironsmith_recents"
        else return []

-- Adds a file to the top of the list, removes duplicates, keeps max 5, and saves it
saveRecent :: FilePath -> [FilePath] -> IO [FilePath]
saveRecent path oldRecents = do
    let newRecents = take 5 $ nub (path : oldRecents)
    writeFile ".ironsmith_recents" (unlines newRecents)
    return newRecents

-- Takes raw code, parses it, compiles it, writes GLSL to disk, returns Error String if failed
compileAndSave :: String -> IO (Maybe String)
compileAndSave code =
    case parse pScript "editor" code of
        Left bundle -> return $ Just (errorBundlePretty bundle)
        Right astScript -> do
            let glslData = compileToGLSL astScript
            writeFile "output.glsl" glslData
            return Nothing