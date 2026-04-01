-- src/Evaluator.hs
module Evaluator where

import AST
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type Env = Map.Map String Float

type Point = (Float, Float, Float)

-- A function that takes a Point and returns a modified Point
type Transform = Point -> Point

-- Convert Degrees to Radians
degToRad :: Float -> Float
degToRad d = d * pi / 180

-- Translate (Move) a point
translatePt :: (Float, Float, Float) -> Transform
translatePt (dx, dy, dz) (x, y, z) = (x + dx, y + dy, z + dz)

-- Rotate a point around the X-axis
rotateXPt :: Float -> Transform
rotateXPt deg (x, y, z) = 
    let r = degToRad deg
        -- Matrix rotation logic for X axis
        newY = y * cos r - z * sin r
        newZ = y * sin r + z * cos r
    in (x, newY, newZ)

-- Rotate a point around the Y-axis
rotateYPt :: Float -> Transform
rotateYPt deg (x, y, z) = 
    let r = degToRad deg
        newX = x * cos r + z * sin r
        newZ = -(x * sin r) + z * cos r
    in (newX, y, newZ)

-- Rotate a point around the Z-axis
rotateZPt :: Float -> Transform
rotateZPt deg (x, y, z) = 
    let r = degToRad deg
        newX = x * cos r - y * sin r
        newY = x * sin r + y * cos r
    in (newX, newY, z)

evalExpr :: Env -> Expr -> Float
evalExpr _   (Lit val)   = val
evalExpr env (Var name)  = fromMaybe 0.0 (Map.lookup name env)
evalExpr env (Add e1 e2) = evalExpr env e1 + evalExpr env e2
evalExpr env (Mul e1 e2) = evalExpr env e1 * evalExpr env e2

generateObjString :: [Point] -> Int -> String
generateObjString vertices vOffset = 
    let 
        -- Convert each (x,y,z) point into a "v x y z" string
        vStrings = map (\(x,y,z) -> "v " ++ show x ++ " " ++ show y ++ " " ++ show z) vertices
        
        face a b c = "f " ++ show (a + vOffset) ++ " " ++ show (b + vOffset) ++ " " ++ show (c + vOffset)
        fStrings = [
            face 1 2 3, face 1 3 4, 
            face 2 6 7, face 2 7 3, 
            face 6 5 8, face 6 8 7, 
            face 5 1 4, face 5 4 8, 
            face 4 3 7, face 4 7 8, 
            face 5 6 2, face 5 2 1
            ]
    in unlines (vStrings ++ fStrings)

evalShape :: Env -> Shape -> Transform -> Int -> (String, Int)
evalShape env (Cube ex ey ez) transform vCount = 
    let x = evalExpr env ex
        y = evalExpr env ey
        z = evalExpr env ez
        
        -- Calculate the half-dimensions to center the cube
        hx = x / 2.0
        hy = y / 2.0
        hz = z / 2.0
        
        -- The 8 raw corners of the cube, centered around (0,0,0)
        rawPoints = [
            (-hx, -hy, -hz), ( hx, -hy, -hz), ( hx,  hy, -hz), (-hx,  hy, -hz),
            (-hx, -hy,  hz), ( hx, -hy,  hz), ( hx,  hy,  hz), (-hx,  hy,  hz)
            ]
            
        transformedPoints = map transform rawPoints
        objStr = generateObjString transformedPoints vCount
    in (objStr, vCount + 8)

-- For Move, we combine the current transform with a new translation
evalShape env (Move ex ey ez innerShape) currentTransform vCount =
    let mx = evalExpr env ex
        my = evalExpr env ey
        mz = evalExpr env ez
        -- Compose functions: apply translation first, THEN the current transform
        newTransform pt = currentTransform (translatePt (mx, my, mz) pt)
    in evalShape env innerShape newTransform vCount

-- For RotateX, we combine the current transform with a new rotation
evalShape env (RotateX edeg innerShape) currentTransform vCount =
    let deg = evalExpr env edeg
        -- Compose functions: apply rotation first, THEN the current transform
        newTransform pt = currentTransform (rotateXPt deg pt)
    in evalShape env innerShape newTransform vCount

evalShape env (RotateY edeg innerShape) currentTransform vCount =
    let deg = evalExpr env edeg
        newTransform pt = currentTransform (rotateYPt deg pt)
    in evalShape env innerShape newTransform vCount

evalShape env (RotateZ edeg innerShape) currentTransform vCount =
    let deg = evalExpr env edeg
        newTransform pt = currentTransform (rotateZPt deg pt)
    in evalShape env innerShape newTransform vCount

evalShape env (Group shapes) currentTransform vCount =
    -- We use foldl to thread the vCount and the string through the list of shapes
    foldl evaluateOneShape ("", vCount) shapes
  where
    -- This helper processes a single shape in the group
    evaluateOneShape (accString, currentV) shape =
        let (shapeString, nextV) = evalShape env shape currentTransform currentV
        in (accString ++ shapeString, nextV)


-- NEW: runScript now takes an Int representing the current vertex count
runScript :: Env -> Int -> Script -> String
runScript _ _ [] = "" 
runScript env vCount (stmt:rest) = case stmt of
    
    Assign name expr -> 
        let val = evalExpr env expr
            newEnv = Map.insert name val env
        in runScript newEnv vCount rest 
        
    Draw shape -> 
        -- Evaluate the shape starting with a (0,0,0) offset
        let (objString, newVCount) = evalShape env shape id vCount
        in objString ++ runScript env newVCount rest