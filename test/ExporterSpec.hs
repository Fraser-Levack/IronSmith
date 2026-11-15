module ExporterSpec (spec) where

import Test.Hspec
import Data.Array.Unboxed (listArray)
import System.Directory (removeFile, doesFileExist)
import qualified Data.ByteString as BS
import Data.List (isInfixOf, isPrefixOf, tails)
import GHC.Float (castWord32ToFloat)
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

            wrote <- exportToOBJ bytecode path 8 (const (return ()))
            wrote `shouldBe` True

            exists <- doesFileExist path
            exists `shouldBe` True

            contents <- readFile path
            let vertLines = filter (\l -> take 2 l == "v ") (lines contents)
                faceLines = filter (\l -> take 2 l == "f ") (lines contents)

            length vertLines `shouldSatisfy` (> 0)
            length faceLines `shouldSatisfy` (> 0)
            -- indexed: a closed mesh has about half as many vertices as faces
            length vertLines `shouldSatisfy` (< length faceLines)

            removeFile path

        it "gives a small model far from the origin full detail" $ do
            -- a 0.5-radius sphere at x = 20: the grid fits the sphere, not the scene
            let bytecode = compileToBytecode (1, 1, 1)
                    [Draw (Move (Lit 20) (Lit 0) (Lit 0) (Sphere (Lit 0.5)))]
                path = "test_small_export.obj"
            _ <- exportToOBJ bytecode path 16 (const (return ()))
            contents <- readFile path
            length contents `seq` removeFile path
            let verts = [ map read (words (drop 2 l)) :: [Float]
                        | l <- lines contents, take 2 l == "v " ]
                faces = filter (\l -> take 2 l == "f ") (lines contents)
                radius [x, y, z] = sqrt ((x - 20)^(2::Int) + y*y + z*z)
                radius _ = 0
            length faces `shouldSatisfy` (> 500)
            all (\v -> abs (radius v - 0.5) < 0.05) verts `shouldBe` True

    describe "evalHit" $ do
        let painted = compileToBytecode (1, 1, 1)
                [ Draw (Paint "red" [Material "metal" [Sphere (Lit 1)]])
                , Draw (Paint "blue" [Move (Lit 10) (Lit 0) (Lit 0) (Sphere (Lit 1))]) ]
            arr = listArray (0, length painted - 1) painted

        it "reports the colour and material of the nearest shape" $ do
            let (_, colA, matA) = evalHit arr (0, 0, 0)
                (_, colB, matB) = evalHit arr (10, 0, 0)
            (colA, matA) `shouldBe` ((1, 0, 0), 3)
            (colB, matB) `shouldBe` ((0, 0, 1), 0)

        it "agrees with evalSDF on distance" $ do
            let (d, _, _) = evalHit arr (4, 1, 0)
            d `shouldBe` evalSDF arr (4, 1, 0)

        it "colours a difference's cut faces like the shape being cut" $ do
            let bc = compileToBytecode (1, 1, 1)
                        [Draw (Difference (Paint "red" [Cube (Lit 4) (Lit 4) (Lit 4)])
                                          (Paint "blue" [Sphere (Lit 1.5)]))]
                a = listArray (0, length bc - 1) bc
                (_, col, _) = evalHit a (1.5, 0, 0) -- on the hole's wall
            col `shouldBe` (1, 0, 0)

    describe "exportToGLB" $ do
        it "writes a valid GLB with one material per colour" $ do
            let bytecode = compileToBytecode (1, 1, 1)
                    [ Draw (Paint "red" [Sphere (Lit 2)])
                    , Draw (Paint "blue" [Material "neon" [Move (Lit 6) (Lit 0) (Lit 0) (Sphere (Lit 2))]]) ]
                path = "test_export.glb"
            _ <- exportToGLB bytecode path 16 (const (return ()))
            bytes <- BS.readFile path
            removeFile path
            BS.take 4 bytes `shouldBe` BS.pack [0x67, 0x6C, 0x54, 0x46] -- "glTF"
            -- header's total length matches the file
            le32 (BS.take 4 (BS.drop 8 bytes)) `shouldBe` BS.length bytes
            let jsonLen = le32 (BS.take 4 (BS.drop 12 bytes))
                json = map (toEnum . fromIntegral) (BS.unpack (BS.take jsonLen (BS.drop 20 bytes))) :: String
            json `shouldSatisfy` isInfixOf "\"matte #ff0000\""
            json `shouldSatisfy` isInfixOf "\"neon #0000ff\""
            json `shouldSatisfy` isInfixOf "\"emissiveFactor\""
            -- indexed with uint32
            json `shouldSatisfy` isInfixOf "\"indices\""
            json `shouldSatisfy` isInfixOf "\"componentType\":5125"

        it "writes nothing for a scene with no shapes" $ do
            let path = "test_empty.glb"
            wrote <- exportToGLB (compileToBytecode (1, 1, 1) []) path 8 (const (return ()))
            wrote `shouldBe` False
            doesFileExist path `shouldReturn` False

    describe "exportToOBJ with no shapes" $
        it "reports False and creates no file" $ do
            let path = "test_empty.obj"
            wrote <- exportToOBJ (compileToBytecode (1, 1, 1) []) path 8 (const (return ()))
            wrote `shouldBe` False
            doesFileExist path `shouldReturn` False

    describe "estimateBounds" $ do
        it "finds an offset box to within a fraction of a unit" $ do
            -- move(2, 3, 0, cube(2, 4, 6)): spans x 1..3, y 1..5, z -3..3
            let bc = compileToBytecode (0, 0, 0)
                        [Draw (Move (Lit 2) (Lit 3) (Lit 0) (Cube (Lit 2) (Lit 4) (Lit 6)))]
                close a b = abs (a - b) < 0.5
            case estimateBounds bc of
                Nothing -> expectationFailure "expected bounds"
                Just ((x0, y0, z0), (x1, y1, z1)) ->
                    [close x0 1, close y0 1, close z0 (-3), close x1 3, close y1 5, close z1 3]
                        `shouldBe` replicate 6 True

        it "finds models bigger than the viewer's default scene" $ do
            -- a 60-unit tower standing on the ground: y 0..60
            let bc = compileToBytecode (0, 0, 0)
                        [Draw (Move (Lit 0) (Lit 30) (Lit 0) (Cube (Lit 4) (Lit 60) (Lit 4)))]
            case estimateBounds bc of
                Nothing -> expectationFailure "expected bounds"
                Just ((_, y0, _), (_, y1, _)) -> do
                    -- conservative by up to one scan cell (1/32 of the size)
                    y0 `shouldSatisfy` (\y -> abs y < 2.5)
                    y1 `shouldSatisfy` (\y -> abs (y - 60) < 2.5)

        it "returns Nothing for an empty scene" $
            estimateBounds (compileToBytecode (0, 0, 0) []) `shouldBe` Nothing

-- | Little-endian uint32 from 4 bytes.
    describe "smooth CSG" $ do
        let close a b = abs (a - b) < 1e-4
            sdfOf shape = let bc = compileToBytecode (1, 1, 1) [Draw shape]
                          in evalSDF (listArray (0, length bc - 1) bc)
            two = Move (Lit 1.5) (Lit 0) (Lit 0) (Sphere (Lit 1))

        it "matches the plain operation when k = 0" $ do
            let p = (0.7, 0.3, 0.1)
            sdfOf (SmoothUnion (Lit 0) (Sphere (Lit 1)) two) p
                `shouldSatisfy` close (sdfOf (Union (Sphere (Lit 1)) two) p)
            sdfOf (SmoothDifference (Lit 0) (Sphere (Lit 1)) two) p
                `shouldSatisfy` close (sdfOf (Difference (Sphere (Lit 1)) two) p)
            sdfOf (SmoothIntersection (Lit 0) (Sphere (Lit 1)) two) p
                `shouldSatisfy` close (sdfOf (Intersection (Sphere (Lit 1)) two) p)

        it "fills the seam between two shapes (union bulges outwards)" $ do
            -- Between the two spheres, at the neck: smooth union is inside
            -- more than the sharp one.
            let neck = (0.75, 0.6, 0)
            sdfOf (SmoothUnion (Lit 0.5) (Sphere (Lit 1)) two) neck
                `shouldSatisfy` (< sdfOf (Union (Sphere (Lit 1)) two) neck)

        it "blends the radius with scale, like the shapes" $
            -- scale(2, ...) of a smooth union: distances (and k) double.
            sdfOf (Scale (Lit 2) (Lit 2) (Lit 2) (SmoothUnion (Lit 0.5) (Sphere (Lit 1)) two)) (1.5, 1.2, 0)
                `shouldSatisfy` close (2 * sdfOf (SmoothUnion (Lit 0.5) (Sphere (Lit 1)) two) (0.75, 0.6, 0))

        it "blends paint colours across the seam" $ do
            let bc = compileToBytecode (1, 1, 1)
                        [Draw (SmoothUnion (Lit 1) (Paint "red" [Sphere (Lit 1)])
                                                   (Paint "blue" [Move (Lit 1.5) (Lit 0) (Lit 0) (Sphere (Lit 1))]))]
                (_, (r, _, b), _) = evalHit (listArray (0, length bc - 1) bc) (0.75, 0.9, 0)
            r `shouldSatisfy` (\x -> x > 0.2 && x < 0.8)
            b `shouldSatisfy` (\x -> x > 0.2 && x < 0.8)

        it "keeps the GLB material count small across a blended seam" $ do
            -- Every distinct colour becomes a glTF material, so blended seams
            -- are banded (16 steps) rather than giving each triangle its own.
            let bc = compileToBytecode (1, 1, 1)
                        [Draw (SmoothUnion (Lit 1) (Paint "red" [Sphere (Lit 1)])
                                                   (Paint "blue" [Move (Lit 1.5) (Lit 0) (Lit 0) (Sphere (Lit 1))]))]
                path = "test_blend.glb"
            _ <- exportToGLB bc path 32 (const (return ()))
            bytes <- BS.readFile path
            removeFile path
            let jsonLen = le32 (BS.take 4 (BS.drop 12 bytes))
                json = map (toEnum . fromIntegral) (BS.unpack (BS.take jsonLen (BS.drop 20 bytes))) :: String
                materials = length (filter ("\"matte #" `isPrefixOf`) (tails json))
            materials `shouldSatisfy` (\m -> m >= 3 && m <= 17)

    describe "exportToSTL" $ do
        it "writes a binary STL whose size matches its triangle count, Z up" $ do
            -- A tall box (Y is up in IronSmith) must come out tall in Z.
            let bytecode = compileToBytecode (1, 1, 1) [Draw (Cube (Lit 1) (Lit 4) (Lit 1))]
                path = "test_export.stl"
            wrote <- exportToSTL bytecode path 16 (const (return ()))
            wrote `shouldBe` True
            bytes <- BS.readFile path
            removeFile path
            BS.take 5 bytes `shouldNotBe` BS.pack (map (fromIntegral . fromEnum) "solid")
            let n = le32 (BS.take 4 (BS.drop 80 bytes))
            n `shouldSatisfy` (> 0)
            BS.length bytes `shouldBe` 84 + 50 * n
            let f32At o = castWord32ToFloat (fromIntegral (le32 (BS.take 4 (BS.drop o bytes))))
                -- vertex coordinates of every triangle: 12 floats after the normal
                zs = [ f32At (84 + 50 * t + 12 + 12 * v + 8) | t <- [0 .. n - 1], v <- [0 .. 2] ]
                ys = [ f32At (84 + 50 * t + 12 + 12 * v + 4) | t <- [0 .. n - 1], v <- [0 .. 2] ]
            (maximum zs - minimum zs) `shouldSatisfy` (> 3.5)
            (maximum ys - minimum ys) `shouldSatisfy` (< 1.5)

        it "writes nothing for a scene with no shapes" $ do
            wrote <- exportToSTL (compileToBytecode (1, 1, 1) []) "test_empty.stl" 8 (const (return ()))
            wrote `shouldBe` False
            doesFileExist "test_empty.stl" `shouldReturn` False

le32 :: BS.ByteString -> Int
le32 = foldr (\b acc -> acc * 256 + fromIntegral b) 0 . BS.unpack
