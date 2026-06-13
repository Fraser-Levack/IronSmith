module ExporterSpec (spec) where

import Test.Hspec
import Data.Array.Unboxed (listArray)
import System.Directory (removeFile, doesFileExist)
import AST
import Evaluator (compileToBytecode)
import Exporter

spec :: Spec
spec = do
    describe "SDF primitives" $ do
        it "sdSphere is negative inside and positive outside" $ do
            sdSphere (0, 0, 0) 1 `shouldBe` (-1)
            sdSphere (2, 0, 0) 1 `shouldBe` 1
            sdSphere (1, 0, 0) 1 `shouldBe` 0

        it "sdBox is zero on the surface and negative at the centre" $ do
            sdBox (0, 0, 0) (1, 1, 1) `shouldSatisfy` (< 0)
            sdBox (1, 0, 0) (1, 1, 1) `shouldBe` 0

        it "sdCylinder is negative inside" $
            sdCylinder (0, 0, 0) 1 1 `shouldSatisfy` (< 0)

        it "sdTorus is negative on the ring centreline" $
            sdTorus (2, 0, 0) (2, 0.5) `shouldBe` (-0.5)

        it "sdCappedCone is negative at the centre of the base" $
            sdCappedCone (0, 0, 0) 1 1 1 `shouldSatisfy` (< 0)

    describe "evalSDF" $ do
        it "evaluates a single sphere instruction" $ do
            let bytecode = compileToBytecode (1, 1, 1) [Draw (Sphere (Lit 5))]
                arr = listArray (0, length bytecode - 1) bytecode
            evalSDF arr (0, 0, 0) `shouldBe` (-5)
            evalSDF arr (10, 0, 0) `shouldBe` 5

        it "evaluates a union of two spheres as the minimum distance" $ do
            let bytecode = compileToBytecode (1, 1, 1)
                    [Draw (Union (Sphere (Lit 1)) (Move (Lit 10) (Lit 0) (Lit 0) (Sphere (Lit 1))))]
                arr = listArray (0, length bytecode - 1) bytecode
            -- Near the origin, only the un-moved sphere is close
            evalSDF arr (0, 0, 0) `shouldBe` (-1)

    describe "exportToOBJ" $ do
        it "writes a non-empty mesh for a simple sphere" $ do
            let bytecode = compileToBytecode (1, 1, 1) [Draw (Sphere (Lit 5))]
                path = "test_sphere_export.obj"

            exportToOBJ bytecode path 8 (const (return ()))

            exists <- doesFileExist path
            exists `shouldBe` True

            contents <- readFile path
            let vertLines = filter (\l -> take 2 l == "v ") (lines contents)
                faceLines = filter (\l -> take 2 l == "f ") (lines contents)

            length vertLines `shouldSatisfy` (> 0)
            length faceLines `shouldSatisfy` (> 0)

            removeFile path
