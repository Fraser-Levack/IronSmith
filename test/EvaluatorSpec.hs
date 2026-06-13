module EvaluatorSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import AST
import Evaluator

spec :: Spec
spec = do
    describe "evalExpr" $ do
        it "evaluates literals" $
            evalExpr Map.empty (Lit 5) `shouldBe` 5

        it "evaluates addition" $
            evalExpr Map.empty (Add (Lit 2) (Lit 3)) `shouldBe` 5

        it "evaluates multiplication" $
            evalExpr Map.empty (Mul (Lit 2) (Lit 3)) `shouldBe` 6

        it "looks up variables from the environment" $
            evalExpr (Map.singleton "x" (VNum 10)) (Var "x") `shouldBe` 10

        it "defaults unbound variables to 0" $
            evalExpr Map.empty (Var "missing") `shouldBe` 0

    describe "resolveMaterial" $ do
        it "maps known presets to their IDs" $ do
            resolveMaterial "matte"   `shouldBe` 0.0
            resolveMaterial "plastic" `shouldBe` 1.0
            resolveMaterial "neon"    `shouldBe` 2.0
            resolveMaterial "metal"   `shouldBe` 3.0

        it "defaults unknown presets to matte" $
            resolveMaterial "unknown" `shouldBe` 0.0

    describe "resolveColor" $ do
        it "maps preset colour names to RGB" $ do
            resolveColor "white" `shouldBe` [1.0, 1.0, 1.0]
            resolveColor "red"   `shouldBe` [1.0, 0.0, 0.0]

        it "parses 6-digit hex colours" $
            resolveColor "#ff0000" `shouldBe` [1.0, 0.0, 0.0]

        it "parses black and white hex colours" $ do
            resolveColor "#000000" `shouldBe` [0.0, 0.0, 0.0]
            resolveColor "#ffffff" `shouldBe` [1.0, 1.0, 1.0]

        it "falls back to forge orange for unknown names" $
            resolveColor "not-a-colour" `shouldBe` [0.8, 0.4, 0.1]

    describe "compileShape" $ do
        it "compiles a sphere into a single vec4" $
            compileShape Map.empty 0.0 (Sphere (Lit 5)) `shouldBe` [1.0, 5.0, 0.0, 0.0]

        it "compiles a cube into two vec4s with halved extents" $
            compileShape Map.empty 0.0 (Cube (Lit 2) (Lit 4) (Lit 6))
                `shouldBe` [2.0, 1.0, 2.0, 3.0, 0.0, 0.0, 0.0, 0.0]

        it "resolves shape references from the environment" $
            compileShape (Map.singleton "s" (VShape (Sphere (Lit 5)))) 0.0 (ShapeRef "s")
                `shouldBe` [1.0, 5.0, 0.0, 0.0]

    describe "compileToBytecode" $ do
        it "emits a halt instruction for an empty script" $
            compileToBytecode (0, 0, 0) [] `shouldBe` [0.0, 0.0, 0.0, 0.0]

        it "wraps a single draw in default-colour paint opcodes and ends with halt" $
            compileToBytecode (1, 0, 0) [Draw (Sphere (Lit 5))]
                `shouldBe` [30.0, 1.0, 0.0, 0.0, 1.0, 5.0, 0.0, 0.0, 31.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
