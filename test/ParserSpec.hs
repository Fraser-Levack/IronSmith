module ParserSpec (spec) where

import Test.Hspec
import Text.Megaparsec (parse, errorBundlePretty)
import AST
import Parser

-- Helper to run a parser and discard error messages for easy comparison
parseOk :: Parser a -> String -> Maybe a
parseOk p input = case parse p "" input of
    Left _    -> Nothing
    Right val -> Just val

-- | The pretty-printed error for input that should fail to parse.
parseErr :: Parser a -> String -> String
parseErr p input = case parse p "" input of
    Left bundle -> errorBundlePretty bundle
    Right _     -> "<parsed successfully>"

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

        it "parses unary minus on a variable" $
            parseOk pExpr "-x" `shouldBe` Just (Sub (Lit 0) (Var "x"))

        it "parses unary minus on a parenthesised expression" $
            parseOk pExpr "-(a + 1)" `shouldBe` Just (Sub (Lit 0) (Add (Var "a") (Lit 1)))

        it "parses unary minus after a binary operator" $
            parseOk pExpr "2 * -x" `shouldBe` Just (Mul (Lit 2) (Sub (Lit 0) (Var "x")))

        it "parses subtraction" $
            parseOk pExpr "5 - 3" `shouldBe` Just (Sub (Lit 5) (Lit 3))

        it "parses division" $
            parseOk pExpr "6 / 3" `shouldBe` Just (Div (Lit 6) (Lit 3))

        it "respects division precedence over subtraction" $
            parseOk pExpr "10 - 2 * 3" `shouldBe` Just (Sub (Lit 10) (Mul (Lit 2) (Lit 3)))

        it "left-associates subtraction" $
            parseOk pExpr "10 - 2 - 3" `shouldBe` Just (Sub (Sub (Lit 10) (Lit 2)) (Lit 3))

        it "left-associates division" $
            parseOk pExpr "12 / 2 / 3" `shouldBe` Just (Div (Div (Lit 12) (Lit 2)) (Lit 3))

        it "combines all four operators with correct precedence" $
            parseOk pExpr "2 + 3 * 4 - 6 / 2"
                `shouldBe` Just (Sub (Add (Lit 2) (Mul (Lit 3) (Lit 4))) (Div (Lit 6) (Lit 2)))

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

        it "accepts quoted colours and materials" $ do
            parseOk pShape "paint(\"#ff00aa\", sphere(1))"
                `shouldBe` Just (Paint "#ff00aa" [Sphere (Lit 1)])
            parseOk pShape "material(\"metal\", sphere(1))"
                `shouldBe` Just (Material "metal" [Sphere (Lit 1)])

        it "uses a negated variable as a shape argument" $
            parseOk pShape "move(-x, 0, 0, sphere(1))"
                `shouldBe` Just (Move (Sub (Lit 0) (Var "x")) (Lit 0) (Lit 0) (Sphere (Lit 1)))

        it "still parses both cone forms" $ do
            parseOk pShape "cone(1, 3)" `shouldBe` Just (Cone (Lit 1) (Lit 0) (Lit 3))
            parseOk pShape "cone(1, 0.5, 3)" `shouldBe` Just (Cone (Lit 1) (Lit 0.5) (Lit 3))

        it "explains too many arguments with the correct form" $
            parseErr pShape "sphere(6, 16)" `shouldContain` "too many arguments; expected sphere(radius)"

        it "explains too few arguments with the correct form" $
            parseErr pShape "cube(1, 2)" `shouldContain` "too few arguments; expected cube(width, height, depth)"

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

        it "parses every form shown in the README syntax overview" $
            -- Keep in sync with README.md "Language Syntax Overview" so the
            -- docs can't drift from the parser again.
            fmap length (parseOk pScript (unlines
                [ "size = 2"
                , "gap = size * 2 + 1"
                , "a = cube(10, 10, 10)"
                , "b = sphere(size)"
                , "cylinder(1, 3)"
                , "cone(1, 3)"
                , "cone(1, 0.5, 3)"
                , "torus(4, 1)"
                , "move(-gap, 2, 3, rotateX(10, rotateY(20, rotateZ(30, a))))"
                , "scale(2, b)"
                , "scale(1, 2, 3, b)"
                , "repeat(4, 0, 4, b)"
                , "union(a, b)"
                , "intersection(a, b)"
                , "difference(cube(10, 10, 10), sphere(6))"
                , "group(a, b, torus(4, 1))"
                , "paint(#FF5733, a)"
                , "paint(red, b)"
                , "material(metal, a)"
                , "light(10, 20, 5)"
                , "light(10, 20, 5, 0.5)"
                , "camera(0, 45, 20, 15)"
                , "camera(4, 90, 20, 15, 0, 1, 0)"
                ])) `shouldBe` Just 23

        it "still allows light as a variable name" $
            parseOk pScript "light = 3\nsphere(light)"
                `shouldBe` Just [Assign "light" (Lit 3), Draw (Sphere (Var "light"))]
