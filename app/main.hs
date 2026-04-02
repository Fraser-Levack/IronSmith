-- app/Main.hs
module Main where

import AST
import Parser
import Evaluator
import Text.Megaparsec (parse, errorBundlePretty)

main :: IO ()
main = do
    let inputFile = "test.irsm"
    script <- readFile inputFile
    
    -- Parse the whole script
    case parse pScript inputFile script of
        Left bundle -> putStrLn (errorBundlePretty bundle)
        Right astScript -> do
            -- Compile the script directly to GLSL Shader code
            let glslData = compileToGLSL astScript
            writeFile "output.glsl" glslData
            putStrLn "Successfully forged output.glsl with SDF math!"