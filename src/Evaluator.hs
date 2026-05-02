-- src/Evaluator.hs
module Evaluator where

import AST
import qualified Data.Map as Map

-- Environment now stores the raw Shape AST, not a GLSL string!
data IronValue = VNum Float | VShape Shape
type Env = Map.Map String IronValue

evalExpr :: Env -> Expr -> Float
evalExpr _   (Lit val)   = val
evalExpr env (Var name)  = 
    case Map.lookup name env of
        Just (VNum val) -> val
        _               -> 0.0 
evalExpr env (Add e1 e2) = evalExpr env e1 + evalExpr env e2
evalExpr env (Mul e1 e2) = evalExpr env e1 * evalExpr env e2

-- Map material names to the IDs the Rust shader expects
resolveMaterial :: String -> Float
resolveMaterial "matte"   = 0.0
resolveMaterial "plastic" = 1.0
resolveMaterial "neon"    = 2.0
resolveMaterial "metal"   = 3.0
resolveMaterial _         = 0.0

-- | THE POSTFIX COMPILER
-- Takes Environment, (X, Y, Z) Offset, Material ID, and the Shape
compileShape :: Env -> (Float, Float, Float) -> Float -> Shape -> [Float]
compileShape env (tx, ty, tz) mat shape = case shape of
    
    Cube ex ey ez -> 
        let w = evalExpr env ex / 2.0
            h = evalExpr env ey / 2.0
            d = evalExpr env ez / 2.0
        in [2.0, tx, ty, tz, w, h, d, mat] -- OP_BOX
        
    Sphere er -> 
        let r = evalExpr env er
        in [1.0, tx, ty, tz, r, mat]       -- OP_SPHERE
        
    Union a b -> 
        -- Postfix: Right child, Left child, then OP_UNION
        compileShape env (tx, ty, tz) mat b ++ 
        compileShape env (tx, ty, tz) mat a ++ 
        [10.0]
        
    Difference a b -> 
        compileShape env (tx, ty, tz) mat b ++ 
        compileShape env (tx, ty, tz) mat a ++ 
        [11.0]
        
    Move ex ey ez innerShape -> 
        let dx = evalExpr env ex
            dy = evalExpr env ey
            dz = evalExpr env ez
        -- Add the move to our running offset and recurse
        in compileShape env (tx + dx, ty + dy, tz + dz) mat innerShape
        
    Material mName innerShapes ->
        let newMat = resolveMaterial mName
        in compileGroup env (tx, ty, tz) newMat innerShapes
        
    Group shapes -> 
        compileGroup env (tx, ty, tz) mat shapes
        
    ShapeRef name ->
        case Map.lookup name env of
            Just (VShape s) -> compileShape env (tx, ty, tz) mat s
            _ -> []
            
    -- Ignored for MVP: Scale, Rotate, Paint, Torus, Cylinder, Cone. 
    _ -> [] 

-- Helper to turn a Group of N shapes into a chain of Unions
compileGroup :: Env -> (Float, Float, Float) -> Float -> [Shape] -> [Float]
compileGroup _ _ _ [] = []
compileGroup env pos mat [s] = compileShape env pos mat s
compileGroup env pos mat (s:ss) = 
    compileGroup env pos mat ss ++ compileShape env pos mat s ++ [10.0]


-- | SCRIPT RUNNER
runScript :: Env -> Script -> [Float]
runScript _ [] = []
runScript env (stmt:rest) = case stmt of
    Assign name expr -> 
        let val = evalExpr env expr
        in runScript (Map.insert name (VNum val) env) rest
        
    AssignShape name shape -> 
        runScript (Map.insert name (VShape shape) env) rest
        
    Draw shape -> 
        compileShape env (0.0, 0.0, 0.0) 0.0 shape ++ runScript env rest


-- | ENTRY POINT
compileToBytecode :: Script -> [Float]
compileToBytecode script = 
    let bytes = runScript Map.empty script
    in bytes ++ [0.0] -- Append OP_HALT at the very end