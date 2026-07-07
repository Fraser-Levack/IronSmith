module ParserSpec (spec) where

import Test.Hspec
import Text.Megaparsec (parse)
import AST
import Parser

-- Helper to run a parser and discard error messages for easy comparison
parseOk :: Parser a -> String -> Maybe a
parseOk p input = case parse p "" input of
    Left _    -> Nothing
    Right val -> Just val

spec :: Spec
spec = do
    describe "pExpr" $ do
        it "parses a single literal" $
            parseOk pExpr "5" `shouldBe` Just (Lit 5)

        it "parses a variable" $
            parseOk pExpr "size" `shouldBe` Just (Var "size")

        it "respects multiplication precedence over addition" $
            parseOk pExpr "2 * 3 + 1" `shouldBe` Just (Add (Mul (Lit 2) (Lit 3)) (Lit 1))

        it "parses parenthesised expressions" $
            parseOk pExpr "(1 + 2) * 3" `shouldBe` Just (Mul (Add (Lit 1) (Lit 2)) (Lit 3))

        it "parses negative numbers" $
            parseOk pExpr "-5" `shouldBe` Just (Lit (-5))

    describe "pCameraKey" $ do
        it "parses a 4-argument camera keyframe (target defaults to origin)" $
            parseOk pStatement "camera(0, 90, 20, 15)"
                `shouldBe` Just (CameraKey (Lit 0) (Lit 90) (Lit 20) (Lit 15) (Lit 0) (Lit 0) (Lit 0))

        it "parses a 7-argument camera keyframe with an explicit target" $
            parseOk pStatement "camera(2, 180, 30, 10, 1, 2, 3)"
                `shouldBe` Just (CameraKey (Lit 2) (Lit 180) (Lit 30) (Lit 10) (Lit 1) (Lit 2) (Lit 3))

        it "accepts expressions and variables in keyframes" $
            parseOk pStatement "camera(t, spin * 2, 20, 15)"
                `shouldBe` Just (CameraKey (Var "t") (Mul (Var "spin") (Lit 2)) (Lit 20) (Lit 15) (Lit 0) (Lit 0) (Lit 0))

    describe "pShape" $ do
        it "parses a sphere" $
            parseOk pShape "sphere(5)" `shouldBe` Just (Sphere (Lit 5))

        it "parses a cube" $
            parseOk pShape "cube(1, 2, 3)" `shouldBe` Just (Cube (Lit 1) (Lit 2) (Lit 3))

        it "allows whitespace before the opening paren" $
            parseOk pShape "cube (1, 2, 3)" `shouldBe` Just (Cube (Lit 1) (Lit 2) (Lit 3))

        it "parses a cone with explicit top radius" $
            parseOk pShape "cone(5, 2, 10)" `shouldBe` Just (Cone (Lit 5) (Lit 2) (Lit 10))

        it "parses a cone with only a height (defaults top radius to 0)" $
            parseOk pShape "cone(5, 10)" `shouldBe` Just (Cone (Lit 5) (Lit 0) (Lit 10))

        it "parses move wrapping an inner shape" $
            parseOk pShape "move(1, 2, 3, sphere(5))"
                `shouldBe` Just (Move (Lit 1) (Lit 2) (Lit 3) (Sphere (Lit 5)))

        it "parses rotateX wrapping an inner shape" $
            parseOk pShape "rotateX(90, cube(1, 1, 1))"
                `shouldBe` Just (RotateX (Lit 90) (Cube (Lit 1) (Lit 1) (Lit 1)))

        it "parses uniform scale (1 argument)" $
            parseOk pShape "scale(2, sphere(1))"
                `shouldBe` Just (Scale (Lit 2) (Lit 2) (Lit 2) (Sphere (Lit 1)))

        it "parses non-uniform scale (3 arguments)" $
            parseOk pShape "scale(1, 2, 3, sphere(1))"
                `shouldBe` Just (Scale (Lit 1) (Lit 2) (Lit 3) (Sphere (Lit 1)))

        it "parses union of two shapes" $
            parseOk pShape "union(cube(1,1,1), sphere(1))"
                `shouldBe` Just (Union (Cube (Lit 1) (Lit 1) (Lit 1)) (Sphere (Lit 1)))

        it "parses difference of two shapes" $
            parseOk pShape "difference(cube(1,1,1), sphere(1))"
                `shouldBe` Just (Difference (Cube (Lit 1) (Lit 1) (Lit 1)) (Sphere (Lit 1)))

        it "parses a group of shapes" $
            parseOk pShape "group(sphere(1), cube(1,1,1))"
                `shouldBe` Just (Group [Sphere (Lit 1), Cube (Lit 1) (Lit 1) (Lit 1)])

        it "parses paint with a preset colour" $
            parseOk pShape "paint(red, sphere(1))"
                `shouldBe` Just (Paint "red" [Sphere (Lit 1)])

        it "parses paint with a hex colour" $
            parseOk pShape "paint(#ff00aa, sphere(1))"
                `shouldBe` Just (Paint "#ff00aa" [Sphere (Lit 1)])

        it "parses material presets" $
            parseOk pShape "material(metal, sphere(1))"
                `shouldBe` Just (Material "metal" [Sphere (Lit 1)])

        it "parses a shape variable reference" $
            parseOk pShape "myShape" `shouldBe` Just (ShapeRef "myShape")

    describe "pScript" $ do
        it "parses variable assignment and a draw statement" $
            parseOk pScript "size = 5\nsphere(size)"
                `shouldBe` Just [Assign "size" (Lit 5), Draw (Sphere (Var "size"))]

        it "parses shape assignment followed by a reference" $
            parseOk pScript "myShape = sphere(1)\nmyShape"
                `shouldBe` Just [AssignShape "myShape" (Sphere (Lit 1)), Draw (ShapeRef "myShape")]

        it "ignores line comments" $
            parseOk pScript "// just a comment\nsphere(1)"
                `shouldBe` Just [Draw (Sphere (Lit 1))]

        it "rejects malformed input" $
            parseOk pScript "sphere(" `shouldBe` Nothing

        it "parses a light statement with default intensity" $
            parseOk pScript "light(10, 20, 5)\nsphere(1)"
                `shouldBe` Just [Light (Lit 10) (Lit 20) (Lit 5) (Lit 1), Draw (Sphere (Lit 1))]

        it "parses a light statement with explicit intensity" $
            parseOk pScript "light(10, 20, 5, 0.5)"
                `shouldBe` Just [Light (Lit 10) (Lit 20) (Lit 5) (Lit 0.5)]

        it "still allows light as a variable name" $
            parseOk pScript "light = 3\nsphere(light)"
                `shouldBe` Just [Assign "light" (Lit 3), Draw (Sphere (Var "light"))]
