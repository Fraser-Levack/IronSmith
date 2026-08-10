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

        it "evaluates subtraction" $
            evalExpr Map.empty (Sub (Lit 5) (Lit 3)) `shouldBe` 2

        it "evaluates division" $
            evalExpr Map.empty (Div (Lit 6) (Lit 3)) `shouldBe` 2

        it "looks up variables from the environment" $
            evalExpr (Map.singleton "x" (VNum 10)) (Var "x") `shouldBe` 10

        it "defaults unbound variables to 0" $
            evalExpr Map.empty (Var "missing") `shouldBe` 0

    describe "cameraTrack" $ do
        it "returns no keyframes for a script without camera statements" $
            cameraTrack [Draw (Sphere (Lit 1))] `shouldBe` []

        it "evaluates keyframes with variables and converts degrees to radians" $
            cameraTrack
                [ Assign "d" (Lit 15)
                , CameraKey (Lit 0) (Lit 180) (Lit 0) (Var "d") (Lit 0) (Lit 0) (Lit 0)
                ]
                `shouldBe` [[0, pi, 0, 15, 0, 0, 0]]

        it "sorts keyframes by time" $
            map head (cameraTrack
                [ CameraKey (Lit 2) (Lit 0) (Lit 0) (Lit 10) (Lit 0) (Lit 0) (Lit 0)
                , CameraKey (Lit 0) (Lit 0) (Lit 0) (Lit 20) (Lit 0) (Lit 0) (Lit 0)
                , CameraKey (Lit 1) (Lit 0) (Lit 0) (Lit 15) (Lit 0) (Lit 0) (Lit 0)
                ])
                `shouldBe` [0, 1, 2]

        it "camera keyframes never emit geometry bytecode" $
            compileToBytecode (1, 1, 1)
                [CameraKey (Lit 0) (Lit 0) (Lit 0) (Lit 20) (Lit 0) (Lit 0) (Lit 0)]
                `shouldBe` [0.0, 0.0, 0.0, 0.0]

    describe "validateScript with camera keyframes" $ do
        it "accepts keyframes using defined number variables" $
            validateScript
                [ Assign "d" (Lit 15)
                , CameraKey (Lit 0) (Lit 0) (Lit 0) (Var "d") (Lit 0) (Lit 0) (Lit 0)
                ]
                `shouldBe` Right ()

        it "rejects keyframes using undefined variables" $
            validateScript
                [CameraKey (Lit 0) (Lit 0) (Lit 0) (Var "nope") (Lit 0) (Lit 0) (Lit 0)]
                `shouldBe` Left ("nope", "Unknown variable 'nope'")

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

        it "appends light opcodes after all geometry, before halt" $
            compileToBytecode (1, 0, 0) [Light (Lit 10) (Lit 20) (Lit 5) (Lit 2), Draw (Sphere (Lit 5))]
                `shouldBe` [30.0, 1.0, 0.0, 0.0, 1.0, 5.0, 0.0, 0.0, 31.0, 0.0, 0.0, 0.0, 40.0, 10.0, 20.0, 5.0, 41.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

        it "lets the last light statement win" $
            compileToBytecode (0, 0, 0) [Light (Lit 1) (Lit 0) (Lit 0) (Lit 1), Light (Lit 0) (Lit 9) (Lit 0) (Lit 3)]
                `shouldBe` [40.0, 0.0, 9.0, 0.0, 41.0, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

        it "evaluates variables in light arguments" $
            compileToBytecode (0, 0, 0) [Assign "h" (Lit 20), Light (Lit 0) (Var "h") (Lit 0) (Lit 1)]
                `shouldBe` [40.0, 0.0, 20.0, 0.0, 41.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

        it "skips draws that produce no geometry instead of corrupting the union chain" $
            compileToBytecode (1, 0, 0) [Draw (Group []), Draw (Sphere (Lit 5))]
                `shouldBe` [30.0, 1.0, 0.0, 0.0, 1.0, 5.0, 0.0, 0.0, 31.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

        it "terminates on self-referencing shape redefinition (previous binding wins)" $
            -- a = sphere(5); a = union(a, cube(2,2,2)); a
            -- Before eager resolution this looped forever.
            compileToBytecode (0, 0, 0)
                [ AssignShape "a" (Sphere (Lit 5))
                , AssignShape "a" (Union (ShapeRef "a") (Cube (Lit 2) (Lit 2) (Lit 2)))
                , Draw (ShapeRef "a")
                ]
                `shouldBe` [ 30.0, 0.0, 0.0, 0.0
                           , 2.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0 -- cube (b, pushed first)
                           , 1.0, 5.0, 0.0, 0.0                     -- sphere (a)
                           , 10.0, 0.0, 0.0, 0.0                    -- union
                           , 31.0, 0.0, 0.0, 0.0
                           , 0.0, 0.0, 0.0, 0.0 ]

        it "supports aliasing a shape with plain assignment" $
            compileToBytecode (0, 0, 0)
                [ AssignShape "a" (Sphere (Lit 5))
                , Assign "b" (Var "a")
                , Draw (ShapeRef "b")
                ]
                `shouldBe` [30.0, 0.0, 0.0, 0.0, 1.0, 5.0, 0.0, 0.0, 31.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

    describe "validateScript" $ do
        it "accepts a script whose references are all defined" $
            validateScript
                [ Assign "size" (Lit 5)
                , AssignShape "a" (Sphere (Var "size"))
                , Draw (ShapeRef "a")
                ]
                `shouldBe` Right ()

        it "rejects a reference to an undefined shape (the half-typed 'cub' case)" $
            validateScript [Draw (Cube (Lit 1) (Lit 1) (Lit 1)), Draw (ShapeRef "cub")]
                `shouldBe` Left ("cub", "Unknown shape 'cub'")

        it "rejects an undefined variable inside a shape argument" $
            validateScript [Draw (Sphere (Var "siz"))]
                `shouldBe` Left ("siz", "Unknown variable 'siz'")

        it "rejects a number used as a shape" $
            validateScript [Assign "x" (Lit 5), Draw (ShapeRef "x")]
                `shouldBe` Left ("x", "'x' is a number, but it's used here as a shape")

        it "rejects a shape used as a number" $
            validateScript [AssignShape "a" (Sphere (Lit 1)), Draw (Sphere (Var "a"))]
                `shouldBe` Left ("a", "'a' is a shape, but it's used here as a number")

        it "rejects a shape definition referencing a not-yet-defined name" $
            validateScript [AssignShape "a" (Union (ShapeRef "a") (Sphere (Lit 1)))]
                `shouldBe` Left ("a", "Unknown shape 'a'")

        it "accepts self-reference when the name was previously defined" $
            validateScript
                [ AssignShape "a" (Sphere (Lit 5))
                , AssignShape "a" (Union (ShapeRef "a") (Cube (Lit 2) (Lit 2) (Lit 2)))
                ]
                `shouldBe` Right ()

        it "accepts shape aliases and validates references through them" $
            validateScript
                [ AssignShape "a" (Sphere (Lit 5))
                , Assign "b" (Var "a")
                , Draw (ShapeRef "b")
                ]
                `shouldBe` Right ()

        it "checks light arguments" $
            validateScript [Light (Var "nope") (Lit 0) (Lit 0) (Lit 1)]
                `shouldBe` Left ("nope", "Unknown variable 'nope'")

        it "accepts subtraction and division expressions" $
            validateScript
                [ Assign "size" (Sub (Lit 10) (Div (Lit 4) (Lit 2)))
                , Draw (Sphere (Var "size"))
                ]
                `shouldBe` Right ()

        it "rejects an undefined variable inside a subtraction expression" $
            validateScript [Assign "x" (Sub (Lit 1) (Var "nope"))]
                `shouldBe` Left ("nope", "Unknown variable 'nope'")
