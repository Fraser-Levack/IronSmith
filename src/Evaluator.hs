-- src/Evaluator.hs
module Evaluator where

import AST
import qualified Data.Map as Map
import Numeric (readHex)

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

-- Map material names to the IDs the Rust shader expects
resolveMaterial :: String -> Float
resolveMaterial "matte"   = 0.0
resolveMaterial "plastic" = 1.0
resolveMaterial "neon"    = 2.0
resolveMaterial "metal"   = 3.0
resolveMaterial _         = 0.0

-- NEW: Resolve colors to raw RGB floats
resolveColor :: String -> [Float]
resolveColor "white" = [1.0, 1.0, 1.0]
resolveColor "black" = [0.0, 0.0, 0.0]
resolveColor "red"   = [1.0, 0.0, 0.0]
resolveColor "green" = [0.0, 1.0, 0.0]
resolveColor "blue"  = [0.0, 0.0, 1.0]
resolveColor ('#':hexStr) | length hexStr == 6 =
    let rStr = take 2 hexStr
        gStr = take 2 (drop 2 hexStr)
        bStr = drop 4 hexStr
        parseHex s = case readHex s of
                        [(val, "")] -> (fromIntegral (val :: Int) :: Float) / 255.0
                        _ -> 1.0
    in [parseHex rStr, parseHex gStr, parseHex bStr]
resolveColor _ = [0.8, 0.4, 0.1] -- Default Forge Orange

-- | THE POSTFIX COMPILER (Vec4 Packed for GPU Memory Banwidth)
-- | THE POSTFIX COMPILER (Pre-baked Math & Vec4 Packed)
compileShape :: Env -> Float -> Shape -> [Float]
compileShape env mat shape = case shape of
    
    Cube ex ey ez -> 
        let w = evalExpr env ex / 2.0; h = evalExpr env ey / 2.0; d = evalExpr env ez / 2.0
        in [2.0, w, h, d, mat, 0.0, 0.0, 0.0] -- 2 vec4s
        
    Sphere er -> 
        let r = evalExpr env er
        in [1.0, r, mat, 0.0] -- 1 vec4

    Cylinder er eh -> 
        let r = evalExpr env er; h = evalExpr env eh / 2.0
        in [3.0, r, h, mat] -- 1 vec4
        
    Cone er et eh -> 
        let r1 = evalExpr env er; r2 = evalExpr env et; h = evalExpr env eh / 2.0
        in [4.0, r1, r2, h, mat, 0.0, 0.0, 0.0] -- 2 vec4s
        
    Torus er et -> 
        let r = evalExpr env er; tr = evalExpr env et
        in [5.0, r, tr, mat] -- 1 vec4
        
    Union a b -> 
        compileShape env mat b ++ compileShape env mat a ++ [10.0, 0.0, 0.0, 0.0]
        
    Difference a b -> 
        compileShape env mat b ++ compileShape env mat a ++ [11.0, 0.0, 0.0, 0.0]

    Intersection a b -> 
        compileShape env mat b ++ compileShape env mat a ++ [12.0, 0.0, 0.0, 0.0]
        
    -- Transform Ops
    Move ex ey ez innerShape -> 
        let x = evalExpr env ex; y = evalExpr env ey; z = evalExpr env ez
        in [24.0, x, y, z] ++ compileShape env mat innerShape ++ [25.0, 0.0, 0.0, 0.0]
        
    -- PRE-BAKED TRIGONOMETRY: GPU no longer calculates sin/cos
    RotateX edeg innerShape ->
        let rads = degToRad (evalExpr env edeg)
            c = cos rads; s = sin rads
        in [20.0, c, s, 0.0] ++ compileShape env mat innerShape ++ [25.0, 0.0, 0.0, 0.0]

    RotateY edeg innerShape ->
        let rads = degToRad (evalExpr env edeg)
            c = cos rads; s = sin rads
        in [21.0, c, s, 0.0] ++ compileShape env mat innerShape ++ [25.0, 0.0, 0.0, 0.0]

    RotateZ edeg innerShape ->
        let rads = degToRad (evalExpr env edeg)
            c = cos rads; s = sin rads
        in [22.0, c, s, 0.0] ++ compileShape env mat innerShape ++ [25.0, 0.0, 0.0, 0.0]

    -- PRE-BAKED DIVISION: GPU now uses fast multiplication
    Scale ex ey ez innerShape ->
        let sx = evalExpr env ex; sy = evalExpr env ey; sz = evalExpr env ez
            min_s = min sx (min sy sz)
            isx = if sx == 0 then 0 else 1.0 / sx
            isy = if sy == 0 then 0 else 1.0 / sy
            isz = if sz == 0 then 0 else 1.0 / sz
        in [23.0, isx, isy, isz, min_s, 0.0, 0.0, 0.0] ++ compileShape env mat innerShape ++ [25.0, 0.0, 0.0, 0.0]

    Repeat ex ey ez innerShape ->
        let cx = evalExpr env ex; cy = evalExpr env ey; cz = evalExpr env ez
        in [26.0, cx, cy, cz] ++ compileShape env mat innerShape ++ [25.0, 0.0, 0.0, 0.0]

    -- Color Ops
    Paint cName innerShapes ->
        let rgb = resolveColor cName
        in [30.0] ++ rgb ++ compileGroup env mat innerShapes ++ [31.0, 0.0, 0.0, 0.0]
        
    Material mName innerShapes ->
        let newMat = resolveMaterial mName
        in compileGroup env newMat innerShapes
        
    Group shapes -> 
        compileGroup env mat shapes
        
    ShapeRef name ->
        case Map.lookup name env of
            Just (VShape s) -> compileShape env mat s
            _ -> []


-- Helper for evaluating lists of shapes
compileGroup :: Env -> Float -> [Shape] -> [Float]
compileGroup _ _ [] = []
compileGroup env mat [s] = compileShape env mat s
compileGroup env mat (s:ss) = 
    compileGroup env mat ss ++ compileShape env mat s ++ [10.0, 0.0, 0.0, 0.0]

-- | ENTRY POINT
compileToBytecode :: Script -> [Float]
compileToBytecode script = 
    let draws = filter (not . null) (runScript Map.empty script)
    in case draws of
        [] -> [0.0, 0.0, 0.0, 0.0] -- Halt vec4
        
        (firstDraw:restDraws) -> 
            firstDraw ++ concatMap (\d -> d ++ [10.0, 0.0, 0.0, 0.0]) restDraws ++ [0.0, 0.0, 0.0, 0.0]

-- | SCRIPT RUNNER
-- NEW: Now returns a List of Bytecode Arrays (one for each Draw statement)
runScript :: Env -> Script -> [[Float]]
runScript _ [] = []
runScript env (stmt:rest) = case stmt of
    Assign name expr -> 
        runScript (Map.insert name (VNum (evalExpr env expr)) env) rest
        
    AssignShape name shape -> 
        runScript (Map.insert name (VShape shape) env) rest
        
    Draw shape -> 
        compileShape env 0.0 shape : runScript env rest