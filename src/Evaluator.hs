-- src/Evaluator.hs
module Evaluator where

import AST
import qualified Data.Map as Map
import Numeric (readHex)

data IronValue = VNum Float | VFunc String
type Env = Map.Map String IronValue

-- Convert Degrees to Radians
degToRad :: Float -> Float
degToRad d = d * pi / 180.0

evalExpr :: Env -> Expr -> Float
evalExpr _   (Lit val)   = val
evalExpr env (Var name)  = 
    case Map.lookup name env of
        Just (VNum val) -> val
        _               -> 0.0 
evalExpr env (Add e1 e2) = evalExpr env e1 + evalExpr env e2
evalExpr env (Mul e1 e2) = evalExpr env e1 * evalExpr env e2

-- Helper for shapes nested deeply inside differences/intersections
balancedMin :: [String] -> String
balancedMin [] = "999999.0"
balancedMin [x] = x
balancedMin xs =
    let mid = length xs `div` 2
        (left, right) = splitAt mid xs
    in "min(" ++ balancedMin left ++ ", " ++ balancedMin right ++ ")"

-- Resolves our Hex strings and presets into GLSL RGB vectors
resolveColor :: String -> String
resolveColor "white" = "1.0, 1.0, 1.0"
resolveColor "black" = "0.0, 0.0, 0.0"
resolveColor "red"   = "1.0, 0.0, 0.0"
resolveColor "green" = "0.0, 1.0, 0.0"
resolveColor "blue"  = "0.0, 0.0, 1.0"
resolveColor ('#':hexStr) | length hexStr == 6 =
    let rStr = take 2 hexStr
        gStr = take 2 (drop 2 hexStr)
        bStr = drop 4 hexStr
        parseHex s = case readHex s of
                        [(val, "")] -> (fromIntegral (val :: Int) :: Float) / 255.0
                        _ -> 1.0
        r = parseHex rStr
        g = parseHex gStr
        b = parseHex bStr
    in show r ++ ", " ++ show g ++ ", " ++ show b
resolveColor _ = "0.8, 0.4, 0.1" -- Default Forge Orange

-- Helper for multi-shape groupings (Groups and Paints)
balancedOp :: String -> [String] -> String
balancedOp _ [] = "vec4(999999.0, 0.0, 0.0, 0.0)"
balancedOp _ [x] = x
balancedOp op xs =
    let mid = length xs `div` 2
        (left, right) = splitAt mid xs
    in op ++ "(" ++ balancedOp op left ++ ", " ++ balancedOp op right ++ ")"

-- --- FIX: THE AST FLATTENER ---
-- This mathematically distributes combinations and transforms into a flat list!
flattenShapes :: Shape -> [Shape]
flattenShapes (Group shapes)   = concatMap flattenShapes shapes
flattenShapes (Union a b)      = flattenShapes a ++ flattenShapes b
flattenShapes (Move x y z s)   = map (Move x y z) (flattenShapes s)
flattenShapes (RotateX d s)    = map (RotateX d) (flattenShapes s)
flattenShapes (RotateY d s)    = map (RotateY d) (flattenShapes s)
flattenShapes (RotateZ d s)    = map (RotateZ d) (flattenShapes s)
flattenShapes (Repeat x y z s) = map (Repeat x y z) (flattenShapes s)
flattenShapes (Paint c shapes) = map (\s -> Paint c [s]) (concatMap flattenShapes shapes)
flattenShapes other            = [other] -- Differences, Intersections, and Primitives stop the flattening.


evalShape :: Env -> String -> Shape -> String -> String
evalShape env _ (ShapeRef name) pVar =
    case Map.lookup name env of
        Just (VFunc funcName) -> funcName ++ "(" ++ pVar ++ ")"
        _ -> "vec4(999999.0, 0.0, 0.0, 0.0)" 

-- 1. PRIMITIVE SHAPES (Now wrapped in vec4)
evalShape env col (Cube ex ey ez) pVar = 
    let x = evalExpr env ex / 2.0; y = evalExpr env ey / 2.0; z = evalExpr env ez / 2.0
    in "vec4(sdBox(" ++ pVar ++ ", vec3(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")), " ++ col ++ ")"

evalShape env col (Sphere er) pVar =
    let r = evalExpr env er
    in "vec4(sdSphere(" ++ pVar ++ ", " ++ show r ++ "), " ++ col ++ ")"

evalShape env col (Cylinder er eh) pVar =
    let r = evalExpr env er; h = evalExpr env eh / 2.0
    in "vec4(sdCylinder(" ++ pVar ++ ", " ++ show h ++ ", " ++ show r ++ "), " ++ col ++ ")"

evalShape env col (Cone er et eh) pVar =
    let r1 = evalExpr env er; r2 = evalExpr env et; h = evalExpr env eh / 2.0
    in "vec4(sdCappedCone(" ++ pVar ++ ", " ++ show h ++ ", " ++ show r1 ++ ", " ++ show r2 ++ "), " ++ col ++ ")"

evalShape env col (Torus er et) pVar =
    let r = evalExpr env er; tr = evalExpr env et
    in "vec4(sdTorus(" ++ pVar ++ ", vec2(" ++ show r ++ ", " ++ show tr ++ ")), " ++ col ++ ")"

-- 2. TRANSFORMATIONS & PAINT
evalShape env col (Move ex ey ez innerShape) pVar =
    let mx = show (evalExpr env ex); my = show (evalExpr env ey); mz = show (evalExpr env ez)
        newP = "(" ++ pVar ++ " - vec3(" ++ mx ++ ", " ++ my ++ ", " ++ mz ++ "))"
    in evalShape env col innerShape newP

evalShape env col (RotateX edeg innerShape) pVar =
    let rad = degToRad (evalExpr env edeg)
        c = show (cos (-rad)); s = show (sin (-rad))
        newP = "(mat3(1.0, 0.0, 0.0, 0.0, " ++ c ++ ", " ++ s ++ ", 0.0, -(" ++ s ++ "), " ++ c ++ ") * " ++ pVar ++ ")"
    in evalShape env col innerShape newP

evalShape env col (RotateY edeg innerShape) pVar =
    let rad = degToRad (evalExpr env edeg)
        c = show (cos (-rad)); s = show (sin (-rad))
        newP = "(mat3(" ++ c ++ ", 0.0, -(" ++ s ++ "), 0.0, 1.0, 0.0, " ++ s ++ ", 0.0, " ++ c ++ ") * " ++ pVar ++ ")"
    in evalShape env col innerShape newP

evalShape env col (RotateZ edeg innerShape) pVar =
    let rad = degToRad (evalExpr env edeg)
        c = show (cos (-rad)); s = show (sin (-rad))
        newP = "(mat3(" ++ c ++ ", " ++ s ++ ", 0.0, -(" ++ s ++ "), " ++ c ++ ", 0.0, 0.0, 0.0, 1.0) * " ++ pVar ++ ")"
    in evalShape env col innerShape newP

evalShape env col (Scale ex ey ez innerShape) pVar =
    let sx = show (evalExpr env ex); sy = show (evalExpr env ey); sz = show (evalExpr env ez)
        newP = "(" ++ pVar ++ " / vec3(" ++ sx ++ ", " ++ sy ++ ", " ++ sz ++ "))"
        minScale = "min(" ++ sx ++ ", min(" ++ sy ++ ", " ++ sz ++ "))"
        inner = evalShape env col innerShape newP
    in "vec4((" ++ inner ++ ").x * " ++ minScale ++ ", (" ++ inner ++ ").yzw)"

evalShape env col (Repeat ex ey ez innerShape) pVar =
    let sx = show (evalExpr env ex); sy = show (evalExpr env ey); sz = show (evalExpr env ez)
        newP = "opRep(" ++ pVar ++ ", vec3(" ++ sx ++ ", " ++ sy ++ ", " ++ sz ++ "))"
    in evalShape env col innerShape newP

evalShape env _ (Paint colorName shapes) pVar =
    let newCol = resolveColor colorName
    in if null shapes then "vec4(999999.0, 0.0, 0.0, 0.0)"
       else balancedOp "opU" (map (\s -> evalShape env newCol s pVar) shapes)

-- 3. CONSTRUCTIVE SOLID GEOMETRY (CSG)
evalShape env col (Group shapes) pVar =
    if null shapes then "vec4(999999.0, 0.0, 0.0, 0.0)"
    else balancedOp "opU" (map (\s -> evalShape env col s pVar) shapes)

evalShape env col (Union a b) pVar =
    "opU(" ++ evalShape env col a pVar ++ ", " ++ evalShape env col b pVar ++ ")"

evalShape env col (Intersection a b) pVar =
    "opI(" ++ evalShape env col a pVar ++ ", " ++ evalShape env col b pVar ++ ")"

evalShape env col (Difference a b) pVar =
    "opS(" ++ evalShape env col a pVar ++ ", " ++ evalShape env col b pVar ++ ")"


-- 4. SCRIPT RUNNER
runScript :: Env -> Script -> ([String], [String])
runScript _ [] = ([], []) 
runScript env (stmt:rest) = case stmt of
    Assign name expr -> 
        let val = evalExpr env expr
            newEnv = Map.insert name (VNum val) env 
        in runScript newEnv rest 
        
    AssignShape name shape -> 
        let flatShapes = flattenShapes shape
            funcName = "shape_" ++ name
            funcBody = if null flatShapes 
                       then "    return vec4(999999.0, 0.0, 0.0, 0.0);\n"
                       else if length flatShapes == 1
                       then "    return " ++ evalShape env "0.8, 0.4, 0.1" (head flatShapes) "p" ++ ";\n"
                       else "    vec4 d = vec4(999999.0, 0.0, 0.0, 0.0);\n" ++
                            unlines (map (\s -> "    d = opU(d, " ++ evalShape env "0.8, 0.4, 0.1" s "p" ++ ");") flatShapes) ++
                            "    return d;\n"
                            
            funcDef = "vec4 " ++ funcName ++ "(vec3 p) {\n" ++ funcBody ++ "}"
            newEnv = Map.insert name (VFunc funcName) env 
            
            (funcs, draws) = runScript newEnv rest
        in (funcDef : funcs, draws)
        
    Draw shape -> 
        let flatShapes = flattenShapes shape
            sdfStrs = map (\s -> evalShape env "0.8, 0.4, 0.1" s "p") flatShapes
            (funcs, draws) = runScript env rest
        in (funcs, sdfStrs ++ draws)


-- 5. GLSL COMPILER
compileToGLSL :: Script -> String
compileToGLSL script = 
    let (funcs, draws) = runScript Map.empty script
        functionsStr = unlines funcs
        
        mapBody = if null draws 
                  then "    return vec4(999999.0, 0.0, 0.0, 0.0);\n"
                  else if length draws == 1
                  then "    return " ++ head draws ++ ";\n"
                  else "    vec4 d = vec4(999999.0, 0.0, 0.0, 0.0);\n" ++
                       unlines (map (\drawCall -> "    d = opU(d, " ++ drawCall ++ ");") draws) ++
                       "    return d;\n"
                       
    in glslPrimitives ++ "\n" ++ functionsStr ++ "\nvec4 map(vec3 p) {\n" ++ mapBody ++ "}\n"

glslPrimitives :: String
glslPrimitives = unlines [
    "vec4 opU(vec4 d1, vec4 d2) { return (d1.x < d2.x) ? d1 : d2; }",
    "vec4 opS(vec4 d1, vec4 d2) { return (d1.x > -d2.x) ? d1 : vec4(-d2.x, d1.yzw); }",
    "vec4 opI(vec4 d1, vec4 d2) { return (d1.x > d2.x) ? d1 : d2; }",
    "float sdSphere(vec3 p, float s) { return length(p)-s; }",
    "float sdBox(vec3 p, vec3 b) { vec3 q = abs(p) - b; return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0); }",
    "float sdCylinder(vec3 p, float h, float r) { vec2 d = abs(vec2(length(p.xz),p.y)) - vec2(r,h); return min(max(d.x,d.y),0.0) + length(max(d,0.0)); }",
    "float sdTorus(vec3 p, vec2 t) { vec2 q = vec2(length(p.xz)-t.x,p.y); return length(q)-t.y; }",
    "float sdCappedCone(vec3 p, float h, float r1, float r2) {",
    "    vec2 q = vec2(length(p.xz), p.y); vec2 k1 = vec2(r2,h); vec2 k2 = vec2(r2-r1,2.0*h);",
    "    vec2 ca = vec2(q.x-min(q.x,(q.y<0.0)?r1:r2), abs(q.y)-h);",
    "    vec2 cb = q - k1 + k2*clamp(dot(k1-q,k2)/dot(k2,k2), 0.0, 1.0);",
    "    float s = (cb.x<0.0 && ca.y<0.0) ? -1.0 : 1.0;",
    "    return s*sqrt(min(dot(ca,ca),dot(cb,cb)));",
    "}",
    "vec3 opRep(vec3 p, vec3 c) {",
    "    vec3 q = p;",
    "    if (c.x > 0.0) q.x = mod(p.x + 0.5*c.x, c.x) - 0.5*c.x;",
    "    if (c.y > 0.0) q.y = mod(p.y + 0.5*c.y, c.y) - 0.5*c.y;",
    "    if (c.z > 0.0) q.z = mod(p.z + 0.5*c.z, c.z) - 0.5*c.z;",
    "    return q;",
    "}"
    ]