-- src/Evaluator.hs
module Evaluator where

import AST
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data IronValue = VNum Float | VShape Shape

type Env = Map.Map String IronValue

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
evalExpr env (Var name)  = 
    case Map.lookup name env of
        Just (VNum val) -> val
        _               -> 0.0 -- If it's missing or is a shape, default to 0.0
evalExpr env (Add e1 e2) = evalExpr env e1 + evalExpr env e2
evalExpr env (Mul e1 e2) = evalExpr env e1 * evalExpr env e2

type Face = [Int]

generateMeshString :: [Point] -> [Face] -> Int -> String
generateMeshString vertices faces vOffset = 
    let 
        vStrings = map (\(x,y,z) -> "v " ++ show x ++ " " ++ show y ++ " " ++ show z) vertices
        
        -- Formats a list of integers into an OBJ face string: [1,2,3] -> "f 1 2 3"
        -- We add the vOffset to every index to support multiple shapes!
        formatFace f = "f " ++ unwords (map (\idx -> show (idx + vOffset)) f)
        fStrings = map formatFace faces
        
    in unlines (vStrings ++ fStrings)


-- Returns (Vertices, Faces)
generateCylinder :: Float -> Float -> Int -> ([Point], [Face])
generateCylinder r h def = (pts, bottomCap ++ topCap ++ sideFaces)
  where
    hd = h / 2.0
    -- Calculate the angles for the circumference
    angles = [ 2 * pi * fromIntegral i / fromIntegral def | i <- [0 .. def-1] ]
    
    -- Generate the rings
    bottomRing = [ (r * cos a, -hd, r * sin a) | a <- angles ]
    topRing    = [ (r * cos a,  hd, r * sin a) | a <- angles ]
    
    -- Vertex List: Center Bottom (1), Center Top (2), Bottom Ring (3 to def+2), Top Ring
    pts = [(0, -hd, 0), (0, hd, 0)] ++ bottomRing ++ topRing
    
    -- Helpers to safely wrap indices around the circle
    botIdx i = 3 + (i `mod` def)
    topIdx i = 3 + def + (i `mod` def)
    
    -- Triangle fans for the caps
    bottomCap = [ [1, botIdx i, botIdx (i+1)] | i <- [0 .. def-1] ]
    topCap    = [ [2, topIdx (i+1), topIdx i] | i <- [0 .. def-1] ]
    
    -- Quads (split into 2 triangles) for the sides
    sideFaces = concat [ 
        [ [botIdx i, topIdx i, topIdx (i+1)], 
          [botIdx i, topIdx (i+1), botIdx (i+1)] ] 
        | i <- [0 .. def-1] ]


generateSphere :: Float -> Int -> ([Point], [Face])
generateSphere r def = (pts, faces)
  where
    -- def acts as both horizontal and vertical resolution (minimum 3)
    res = max 3 def 
    
    -- Slices (longitude) and Stacks (latitude)
    lonAngles = [ 2 * pi * fromIntegral i / fromIntegral res | i <- [0 .. res-1] ]
    latAngles = [ pi * fromIntegral j / fromIntegral res | j <- [1 .. res-1] ]
    
    -- South Pole (1), North Pole (2)
    poles = [(0, r, 0), (0, -r, 0)]
    
    -- Generate the middle rings
    rings = [ (r * sin lat * cos lon, r * cos lat, r * sin lat * sin lon) 
            | lat <- latAngles, lon <- lonAngles ]
    
    pts = poles ++ rings
    
    -- Math to connect the rings into faces... (Simplified for brevity)
    idx ring slice = 3 + ring * res + (slice `mod` res)
    
    bottomCap = [ [1, idx 0 (i+1), idx 0 i] | i <- [0 .. res-1] ]
    topCap    = [ [2, idx (res-2) i, idx (res-2) (i+1)] | i <- [0 .. res-1] ]
    
    quads = concat [
        [ [idx j i, idx j (i+1), idx (j+1) (i+1)],
          [idx j i, idx (j+1) (i+1), idx (j+1) i] ]
        | j <- [0 .. res-3], i <- [0 .. res-1] ]
        
    faces = bottomCap ++ topCap ++ quads


generateCone :: Float -> Float -> Int -> Float -> ([Point], [Face])
generateCone r tr def h = (pts, faces)
    where
        hd = h / 2.0
        -- Calculate the angles for the circumference
        angles = [ 2 * pi * fromIntegral i / fromIntegral def | i <- [0 .. def-1] ]
        
        -- Generate the rings
        bottomRing = [ (r * cos a, -hd, r * sin a) | a <- angles ]
        topRing    = [ (tr * cos a,  hd, tr * sin a) | a <- angles ]
        
        -- Vertex List: Center Bottom (1), Center Top (2), Bottom Ring (3 to def+2), Top Ring
        pts = [(0, -hd, 0), (0, hd, 0)] ++ bottomRing ++ topRing
        
        -- Helpers to safely wrap indices around the circle
        botIdx i = 3 + (i `mod` def)
        topIdx i = 3 + def + (i `mod` def)
        
        -- Triangle fans for the caps
        bottomCap = [ [1, botIdx i, botIdx (i+1)] | i <- [0 .. def-1] ]
        topCap    = [ [2, topIdx (i+1), topIdx i] | i <- [0 .. def-1] ]
        
        -- Quads (split into 2 triangles) for the sides
        sideFaces = concat [ 
            [ [botIdx i, topIdx i, topIdx (i+1)], 
            [botIdx i, topIdx (i+1), botIdx (i+1)] ] 
            | i <- [0 .. def-1] ]
            
        faces = bottomCap ++ topCap ++ sideFaces

generateTorus :: Float -> Float -> Int -> ([Point], [Face])
generateTorus r tr def = (pts, faces)
    where
        -- Calculate the angles for the circumference
        angles = [ 2 * pi * fromIntegral i / fromIntegral def | i <- [0 .. def-1] ]
        
        -- Generate the rings
        pts = [ ((r + tr * cos a) * cos b, tr * sin a, (r + tr * cos a) * sin b) 
              | a <- angles, b <- angles ]
        
        -- FIX: 1-based indexing, AND wrap the ring back to 0!
        idx ring slice = 1 + (ring `mod` def) * def + (slice `mod` def)
        
        faces = concat [
            [ [idx j i, idx j (i+1), idx (j+1) (i+1)],
              [idx j i, idx (j+1) (i+1), idx (j+1) i] ]
            | j <- [0 .. def-1], i <- [0 .. def-1] ]


evalShape :: Env -> Shape -> Transform -> Int -> (String, Int)
evalShape env (ShapeRef name) currentTransform vCount =
    case Map.lookup name env of
        -- If it finds the shape, it recursively evaluates it!
        Just (VShape storedShape) -> evalShape env storedShape currentTransform vCount
        -- If the variable doesn't exist (or is a number), draw nothing
        _ -> ("", vCount)
evalShape env (Cube ex ey ez) transform vCount = 
    let x = evalExpr env ex
        y = evalExpr env ey
        z = evalExpr env ez
        hx = x / 2.0; hy = y / 2.0; hz = z / 2.0
        
        rawPoints = [
            (-hx, -hy, -hz), ( hx, -hy, -hz), ( hx,  hy, -hz), (-hx,  hy, -hz),
            (-hx, -hy,  hz), ( hx, -hy,  hz), ( hx,  hy,  hz), (-hx,  hy,  hz)
            ]
            
        -- The cube provides its own faces now!
        cubeFaces = [
            [1,2,3], [1,3,4], [2,6,7], [2,7,3], 
            [6,5,8], [6,8,7], [5,1,4], [5,4,8], 
            [4,3,7], [4,7,8], [5,6,2], [5,2,1]
            ]
            
        transformedPoints = map transform rawPoints
        objStr = generateMeshString transformedPoints cubeFaces vCount
    in (objStr, vCount + length rawPoints) -- Dynamically increment vertex count!

evalShape env (Cylinder er ed eh) transform vCount =
    let r = evalExpr env er
        def = round (evalExpr env ed) -- Convert float expression to Int
        h = evalExpr env eh
        
        (rawPoints, faces) = generateCylinder r h def
        transformedPoints = map transform rawPoints
        objStr = generateMeshString transformedPoints faces vCount
    in (objStr, vCount + length rawPoints)

evalShape env (Sphere er ed) transform vCount =
    let r = evalExpr env er
        def = round (evalExpr env ed)
        
        (rawPoints, faces) = generateSphere r def
        transformedPoints = map transform rawPoints
        objStr = generateMeshString transformedPoints faces vCount
    in (objStr, vCount + length rawPoints)

evalShape env (Cone er et ed eh) transform vCount =
    let r = evalExpr env er
        tr = evalExpr env et
        def = round (evalExpr env ed)
        h = evalExpr env eh
        
        (rawPoints, faces) = generateCone r tr def h
        transformedPoints = map transform rawPoints
        objStr = generateMeshString transformedPoints faces vCount
    in (objStr, vCount + length rawPoints)

evalShape env (Torus er et ed) transform vCount =
    let r = evalExpr env er
        tr = evalExpr env et
        def = round (evalExpr env ed)
        
        (rawPoints, faces) = generateTorus r tr def
        transformedPoints = map transform rawPoints
        objStr = generateMeshString transformedPoints faces vCount
    in (objStr, vCount + length rawPoints)

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
            newEnv = Map.insert name (VNum val) env -- Wrap in VNum
        in runScript newEnv vCount rest 
        
    -- NEW: Handle shape assignment by saving the AST into memory
    AssignShape name shape -> 
        let newEnv = Map.insert name (VShape shape) env -- Wrap in VShape
        in runScript newEnv vCount rest
        
    Draw shape -> 
        let (objString, newVCount) = evalShape env shape id vCount
        in objString ++ runScript env newVCount rest