-- app/Main.hs
module Main where

import AST
import Parser
import Evaluator
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.Map as Map

main :: IO ()
main = do
    let inputFile = "test.irsm"
    script <- readFile inputFile
    
    -- Parse the whole script
    case parse pScript inputFile script of
        Left bundle -> putStrLn (errorBundlePretty bundle)
        Right astScript -> do
            -- Evaluate script starting with an empty Environment AND 0 vertices
            let objData = runScript Map.empty 0 astScript
            writeFile "output.obj" objData
            putStrLn "Successfully forged output.obj with translations!"