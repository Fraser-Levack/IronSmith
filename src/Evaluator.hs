-- src/Evaluator.hs
module Evaluator where

import AST
import qualified Data.Map as Map

data IronValue = VNum Float | VShape Shape
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

-- --- FIX: BALANCED GLSL TREE ---
-- This prevents the GPU compiler from throwing a Stack Overflow on massive files
balancedMin :: [String] -> String
balancedMin [] = "999999.0"
balancedMin [x] = x
balancedMin xs =
    let mid = length xs `div` 2
        (left, right) = splitAt mid xs
    in "min(" ++ balancedMin left ++ ", " ++ balancedMin right ++ ")"
-- ---------------------------------

-- evalShape now takes the Point Variable name (e.g., "p") and returns a GLSL string
evalShape :: Env -> Shape -> String -> String
evalShape env (ShapeRef name) pVar =
    case Map.lookup name env of
        Just (VShape storedShape) -> evalShape env storedShape pVar
        _ -> "999999.0" -- If missing, return a huge distance so it renders nothing

-- 1. PRIMITIVE SHAPES (Note: Definition params are ignored for SDFs!)
evalShape env (Cube ex ey ez) pVar = 
    let x = evalExpr env ex / 2.0
        y = evalExpr env ey / 2.0
        z = evalExpr env ez / 2.0
    in "sdBox(" ++ pVar ++ ", vec3(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ "))"

evalShape env (Sphere er _) pVar =
    let r = evalExpr env er
    in "sdSphere(" ++ pVar ++ ", " ++ show r ++ ")"

evalShape env (Cylinder er _ eh) pVar =
    let r = evalExpr env er
        h = evalExpr env eh / 2.0
    in "sdCylinder(" ++ pVar ++ ", " ++ show h ++ ", " ++ show r ++ ")"

evalShape env (Cone er et _ eh) pVar =
    let r1 = evalExpr env er
        r2 = evalExpr env et
        h = evalExpr env eh / 2.0
    in "sdCappedCone(" ++ pVar ++ ", " ++ show h ++ ", " ++ show r1 ++ ", " ++ show r2 ++ ")"

evalShape env (Torus er et _) pVar =
    let r = evalExpr env er
        tr = evalExpr env et
    in "sdTorus(" ++ pVar ++ ", vec2(" ++ show r ++ ", " ++ show tr ++ "))"


-- 2. TRANSFORMATIONS (We transform the point 'pVar' in reverse!)
evalShape env (Move ex ey ez innerShape) pVar =
    let mx = evalExpr env ex
        my = evalExpr env ey
        mz = evalExpr env ez
        -- --- FIX: NEGATIVE ZERO BUG ---
        -- Wrapped the injected floats in extra parentheses so `-0.0` parses safely in GLSL
        newP = "(" ++ pVar ++ " - vec3((" ++ show mx ++ "), (" ++ show my ++ "), (" ++ show mz ++ ")))"
    in evalShape env innerShape newP

evalShape env (Repeat ex ey ez innerShape) pVar =
    let sx = show (evalExpr env ex)
        sy = show (evalExpr env ey)
        sz = show (evalExpr env ez)
        -- Wrap the point in our GLSL repetition function
        newP = "opRep(" ++ pVar ++ ", vec3(" ++ sx ++ ", " ++ sy ++ ", " ++ sz ++ "))"
    in evalShape env innerShape newP

evalShape env (RotateX edeg innerShape) pVar =
    let rad = degToRad (evalExpr env edeg)
        c = show (cos (-rad)) 
        s = show (sin (-rad))
        -- --- FIX: NEGATIVE ZERO BUG ---
        -- Added parenthesis around the negated sine: -(" ++ s ++ ")
        newP = "(mat3(1.0, 0.0, 0.0, 0.0, " ++ c ++ ", " ++ s ++ ", 0.0, -(" ++ s ++ "), " ++ c ++ ") * " ++ pVar ++ ")"
    in evalShape env innerShape newP

evalShape env (RotateY edeg innerShape) pVar =
    let rad = degToRad (evalExpr env edeg)
        c = show (cos (-rad))
        s = show (sin (-rad))
        newP = "(mat3(" ++ c ++ ", 0.0, -(" ++ s ++ "), 0.0, 1.0, 0.0, " ++ s ++ ", 0.0, " ++ c ++ ") * " ++ pVar ++ ")"
    in evalShape env innerShape newP

evalShape env (RotateZ edeg innerShape) pVar =
    let rad = degToRad (evalExpr env edeg)
        c = show (cos (-rad))
        s = show (sin (-rad))
        newP = "(mat3(" ++ c ++ ", " ++ s ++ ", 0.0, -(" ++ s ++ "), " ++ c ++ ", 0.0, 0.0, 0.0, 1.0) * " ++ pVar ++ ")"
    in evalShape env innerShape newP


-- 3. CONSTRUCTIVE SOLID GEOMETRY (CSG)
evalShape env (Group shapes) pVar =
    if null shapes then "999999.0"
    -- --- FIX: BALANCED GLSL TREE ---
    else balancedMin (map (\s -> evalShape env s pVar) shapes)

evalShape env (Union a b) pVar =
    "min(" ++ evalShape env a pVar ++ ", " ++ evalShape env b pVar ++ ")"

evalShape env (Intersection a b) pVar =
    "max(" ++ evalShape env a pVar ++ ", " ++ evalShape env b pVar ++ ")"

evalShape env (Difference a b) pVar =
    "max(" ++ evalShape env a pVar ++ ", -(" ++ evalShape env b pVar ++ "))"


-- 4. SCRIPT RUNNER (Collects all Draw calls into a list of strings)
runScript :: Env -> Script -> [String]
runScript _ [] = [] 
runScript env (stmt:rest) = case stmt of
    Assign name expr -> 
        let val = evalExpr env expr
            newEnv = Map.insert name (VNum val) env 
        in runScript newEnv rest 
        
    AssignShape name shape -> 
        let newEnv = Map.insert name (VShape shape) env 
        in runScript newEnv rest
        
    Draw shape -> 
        let sdfStr = evalShape env shape "p"
        in sdfStr : runScript env rest


-- 5. GLSL COMPILER (Wraps everything together)
compileToGLSL :: Script -> String
compileToGLSL script = 
    let draws = runScript Map.empty script
        -- --- FIX: BALANCED GLSL TREE ---
        sceneMap = if null draws then "999999.0" else balancedMin draws
    in glslPrimitives ++ "\nfloat map(vec3 p) {\n    return " ++ sceneMap ++ ";\n}\n"

glslPrimitives :: String
glslPrimitives = unlines [
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