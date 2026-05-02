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

-- Resolves material presets to GLSL integer IDs
resolveMaterial :: String -> String
resolveMaterial "matte"   = "0"
resolveMaterial "plastic" = "1"
resolveMaterial "neon"    = "2"
resolveMaterial "metal"   = "3"
resolveMaterial _         = "0"

-- Helper for multi-shape groupings (Groups, Paints, Materials)
balancedOp :: String -> [String] -> String
balancedOp _ [] = "Hit(999999.0, vec3(0.0), 0)"
balancedOp _ [x] = x
balancedOp op xs =
    let mid = length xs `div` 2
        (left, right) = splitAt mid xs
    in op ++ "(" ++ balancedOp op left ++ ", " ++ balancedOp op right ++ ")"

flattenShapes :: Shape -> [Shape]
flattenShapes (Group shapes)      = concatMap flattenShapes shapes
flattenShapes (Union a b)         = flattenShapes a ++ flattenShapes b
flattenShapes (Move x y z s)      = map (Move x y z) (flattenShapes s)
flattenShapes (RotateX d s)       = map (RotateX d) (flattenShapes s)
flattenShapes (RotateY d s)       = map (RotateY d) (flattenShapes s)
flattenShapes (RotateZ d s)       = map (RotateZ d) (flattenShapes s)
flattenShapes (Scale x y z s)     = map (Scale x y z) (flattenShapes s)
flattenShapes (Repeat x y z s)    = map (Repeat x y z) (flattenShapes s)
flattenShapes (Paint c shapes)    = map (\s -> Paint c [s]) (concatMap flattenShapes shapes) 
flattenShapes (Material m shapes) = map (\s -> Material m [s]) (concatMap flattenShapes shapes) 
flattenShapes other               = [other] 

-- evalShape now threads BOTH color and material ID down the tree
evalShape :: Env -> String -> String -> Shape -> String -> String
evalShape env _ _ (ShapeRef name) pVar =
    case Map.lookup name env of
        Just (VFunc funcName) -> funcName ++ "(" ++ pVar ++ ")"
        _ -> "Hit(999999.0, vec3(0.0), 0)" 

-- 1. PRIMITIVE SHAPES (Now generating Hit structs)
evalShape env col mat (Cube ex ey ez) pVar = 
    let x = evalExpr env ex / 2.0; y = evalExpr env ey / 2.0; z = evalExpr env ez / 2.0
    in "Hit(sdBox(" ++ pVar ++ ", vec3(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")), vec3(" ++ col ++ "), " ++ mat ++ ")"

evalShape env col mat (Sphere er) pVar =
    let r = evalExpr env er
    in "Hit(sdSphere(" ++ pVar ++ ", " ++ show r ++ "), vec3(" ++ col ++ "), " ++ mat ++ ")"

evalShape env col mat (Cylinder er eh) pVar =
    let r = evalExpr env er; h = evalExpr env eh / 2.0
    in "Hit(sdCylinder(" ++ pVar ++ ", " ++ show h ++ ", " ++ show r ++ "), vec3(" ++ col ++ "), " ++ mat ++ ")"

evalShape env col mat (Cone er et eh) pVar =
    let r1 = evalExpr env er; r2 = evalExpr env et; h = evalExpr env eh / 2.0
    in "Hit(sdCappedCone(" ++ pVar ++ ", " ++ show h ++ ", " ++ show r1 ++ ", " ++ show r2 ++ "), vec3(" ++ col ++ "), " ++ mat ++ ")"

evalShape env col mat (Torus er et) pVar =
    let r = evalExpr env er; tr = evalExpr env et
    in "Hit(sdTorus(" ++ pVar ++ ", vec2(" ++ show r ++ ", " ++ show tr ++ ")), vec3(" ++ col ++ "), " ++ mat ++ ")"

-- 2. TRANSFORMATIONS & MODIFIERS
evalShape env col mat (Move ex ey ez innerShape) pVar =
    let mx = show (evalExpr env ex); my = show (evalExpr env ey); mz = show (evalExpr env ez)
        newP = "(" ++ pVar ++ " - vec3(" ++ mx ++ ", " ++ my ++ ", " ++ mz ++ "))"
    in evalShape env col mat innerShape newP

evalShape env col mat (RotateX edeg innerShape) pVar =
    let rad = degToRad (evalExpr env edeg)
        c = show (cos (-rad)); s = show (sin (-rad))
        newP = "(mat3(1.0, 0.0, 0.0, 0.0, " ++ c ++ ", " ++ s ++ ", 0.0, -(" ++ s ++ "), " ++ c ++ ") * " ++ pVar ++ ")"
    in evalShape env col mat innerShape newP

evalShape env col mat (RotateY edeg innerShape) pVar =
    let rad = degToRad (evalExpr env edeg)
        c = show (cos (-rad)); s = show (sin (-rad))
        newP = "(mat3(" ++ c ++ ", 0.0, -(" ++ s ++ "), 0.0, 1.0, 0.0, " ++ s ++ ", 0.0, " ++ c ++ ") * " ++ pVar ++ ")"
    in evalShape env col mat innerShape newP

evalShape env col mat (RotateZ edeg innerShape) pVar =
    let rad = degToRad (evalExpr env edeg)
        c = show (cos (-rad)); s = show (sin (-rad))
        newP = "(mat3(" ++ c ++ ", " ++ s ++ ", 0.0, -(" ++ s ++ "), " ++ c ++ ", 0.0, 0.0, 0.0, 1.0) * " ++ pVar ++ ")"
    in evalShape env col mat innerShape newP

evalShape env col mat (Scale ex ey ez innerShape) pVar = 
    let sx = show (evalExpr env ex); sy = show (evalExpr env ey); sz = show (evalExpr env ez)
        newP = "(" ++ pVar ++ " / vec3(" ++ sx ++ ", " ++ sy ++ ", " ++ sz ++ "))"
        minScale = "min(" ++ sx ++ ", min(" ++ sy ++ ", " ++ sz ++ "))"
        inner = evalShape env col mat innerShape newP
    in "opScale(" ++ inner ++ ", " ++ minScale ++ ")"

evalShape env col mat (Repeat ex ey ez innerShape) pVar =
    let sx = show (evalExpr env ex); sy = show (evalExpr env ey); sz = show (evalExpr env ez)
        newP = "opRep(" ++ pVar ++ ", vec3(" ++ sx ++ ", " ++ sy ++ ", " ++ sz ++ "))"
    in evalShape env col mat innerShape newP

evalShape env _ mat (Paint colorName shapes) pVar =
    let newCol = resolveColor colorName
    in if null shapes then "Hit(999999.0, vec3(0.0), 0)"
       else balancedOp "opU" (map (\s -> evalShape env newCol mat s pVar) shapes)

evalShape env col _ (Material matName shapes) pVar =
    let newMat = resolveMaterial matName
    in if null shapes then "Hit(999999.0, vec3(0.0), 0)"
       else balancedOp "opU" (map (\s -> evalShape env col newMat s pVar) shapes)

-- 3. CONSTRUCTIVE SOLID GEOMETRY (CSG)
evalShape env col mat (Group shapes) pVar =
    if null shapes then "Hit(999999.0, vec3(0.0), 0)"
    else balancedOp "opU" (map (\s -> evalShape env col mat s pVar) shapes)

evalShape env col mat (Union a b) pVar =
    "opU(" ++ evalShape env col mat a pVar ++ ", " ++ evalShape env col mat b pVar ++ ")"

evalShape env col mat (Intersection a b) pVar =
    "opI(" ++ evalShape env col mat a pVar ++ ", " ++ evalShape env col mat b pVar ++ ")"

evalShape env col mat (Difference a b) pVar =
    "opS(" ++ evalShape env col mat a pVar ++ ", " ++ evalShape env col mat b pVar ++ ")"


-- 4. SCRIPT RUNNER
runScript :: Env -> Script -> ([String], [String])
runScript _ [] = ([], []) 
runScript env (stmt:rest) = case stmt of
    Assign name expr -> 
        let val = evalExpr env expr
            newEnv = Map.insert name (VNum val) env 
        in runScript newEnv rest 
        
    -- NEW: No more flattening! Just evaluate the single AST Shape directly
    AssignShape name shape -> 
        let funcName = "shape_" ++ name
            funcBody = "    return " ++ evalShape env "0.8, 0.4, 0.1" "0" shape "p" ++ ";\n"
            funcDef = "Hit " ++ funcName ++ "(vec3 p) {\n" ++ funcBody ++ "}"
            newEnv = Map.insert name (VFunc funcName) env 
            
            (funcs, draws) = runScript newEnv rest
        in (funcDef : funcs, draws)
        
    -- NEW: Evaluate the top-level shape directly
    Draw shape -> 
        let sdfStr = evalShape env "0.8, 0.4, 0.1" "0" shape "p"
            (funcs, draws) = runScript env rest
        in (funcs, sdfStr : draws)


-- 5. GLSL COMPILER
-- (You don't need to change compileToGLSL, it works perfectly with the new runScript!)
compileToGLSL :: Script -> String
compileToGLSL script = 
    let (funcs, draws) = runScript Map.empty script
        functionsStr = unlines funcs
        
        mapBody = if null draws 
                  then "    return Hit(999999.0, vec3(0.0), 0);\n"
                  else if length draws == 1
                  then "    return " ++ head draws ++ ";\n"
                  else "    Hit d = Hit(999999.0, vec3(0.0), 0);\n" ++
                       unlines (map (\drawCall -> "    d = opU(d, " ++ drawCall ++ ");") draws) ++
                       "    return d;\n"
                       
    in glslPrimitives ++ "\n" ++ functionsStr ++ "\nHit map(vec3 p) {\n" ++ mapBody ++ "}\n"

glslPrimitives :: String
glslPrimitives = unlines [
    "struct Hit { float d; vec3 col; int mat; };",
    "Hit opU(Hit h1, Hit h2) { return (h1.d < h2.d) ? h1 : h2; }",
    "Hit opS(Hit h1, Hit h2) { return (h1.d > -h2.d) ? h1 : Hit(-h2.d, h1.col, h1.mat); }",
    "Hit opI(Hit h1, Hit h2) { return (h1.d > h2.d) ? h1 : h2; }",
    "Hit opScale(Hit h, float s) { return Hit(h.d * s, h.col, h.mat); }",
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