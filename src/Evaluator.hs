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
evalExpr env (Sub e1 e2) = evalExpr env e1 - evalExpr env e2
evalExpr env (Div e1 e2) = evalExpr env e1 / evalExpr env e2

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

-- ─── VALIDATION ──────────────────────────────────────────────────────────────

-- | Check every identifier in the script resolves to something defined
--   earlier, so mid-edit scripts (e.g. a half-typed "cub" on its own line)
--   are reported as editor errors instead of compiling into structurally
--   broken bytecode. Returns the offending identifier (for line lookup)
--   and a human-readable message.
validateScript :: Script -> Either (String, String) ()
validateScript = go [] []
  where
    go :: [String] -> [String] -> Script -> Either (String, String) ()
    go _ _ [] = Right ()
    go nums shapes (stmt:rest) = case stmt of
        -- `a = b` where b is a shape defines a shape alias, not a number
        Assign name (Var ref) | ref `elem` shapes -> do
            go nums (name:shapes) rest
        Assign name expr -> do
            checkExpr nums shapes expr
            go (name:nums) shapes rest
        AssignShape name shape -> do
            checkShape nums shapes shape
            go nums (name:shapes) rest
        Draw shape -> do
            checkShape nums shapes shape
            go nums shapes rest
        Light ex ey ez ei -> do
            mapM_ (checkExpr nums shapes) [ex, ey, ez, ei]
            go nums shapes rest
        CameraKey et eyaw ep ed etx ety etz -> do
            mapM_ (checkExpr nums shapes) [et, eyaw, ep, ed, etx, ety, etz]
            go nums shapes rest

    checkExpr :: [String] -> [String] -> Expr -> Either (String, String) ()
    checkExpr nums shapes expr = case expr of
        Lit _ -> Right ()
        Var n
            | n `elem` nums   -> Right ()
            | n `elem` shapes -> Left (n, "'" ++ n ++ "' is a shape, but it's used here as a number")
            | otherwise       -> Left (n, "Unknown variable '" ++ n ++ "'")
        Add a b -> checkExpr nums shapes a >> checkExpr nums shapes b
        Mul a b -> checkExpr nums shapes a >> checkExpr nums shapes b
        Sub a b -> checkExpr nums shapes a >> checkExpr nums shapes b
        Div a b -> checkExpr nums shapes a >> checkExpr nums shapes b

    checkShape :: [String] -> [String] -> Shape -> Either (String, String) ()
    checkShape nums shapes shape = case shape of
        ShapeRef n
            | n `elem` shapes -> Right ()
            | n `elem` nums   -> Left (n, "'" ++ n ++ "' is a number, but it's used here as a shape")
            | otherwise       -> Left (n, "Unknown shape '" ++ n ++ "'")
        Cube a b c        -> mapM_ ce [a, b, c]
        Cylinder a b      -> mapM_ ce [a, b]
        Cone a b c        -> mapM_ ce [a, b, c]
        Torus a b         -> mapM_ ce [a, b]
        Sphere a          -> ce a
        Move a b c s      -> mapM_ ce [a, b, c] >> cs s
        RotateX a s       -> ce a >> cs s
        RotateY a s       -> ce a >> cs s
        RotateZ a s       -> ce a >> cs s
        Scale a b c s     -> mapM_ ce [a, b, c] >> cs s
        Repeat a b c s    -> mapM_ ce [a, b, c] >> cs s
        Group ss          -> mapM_ cs ss
        Union a b         -> cs a >> cs b
        Intersection a b  -> cs a >> cs b
        Difference a b    -> cs a >> cs b
        Paint _ ss        -> mapM_ cs ss
        Material _ ss     -> mapM_ cs ss
      where
        ce = checkExpr nums shapes
        cs = checkShape nums shapes

-- | Replace every ShapeRef with the shape it referred to at this point in
--   the script. Stored shapes are therefore self-contained: later
--   redefinitions can't change them, and a definition like
--   `a = union(a, ...)` refers to the previous `a` instead of looping
--   forever when compiled.
resolveShape :: Env -> Shape -> Shape
resolveShape env shape = case shape of
    ShapeRef name -> case Map.lookup name env of
        Just (VShape s) -> s -- already resolved when it was stored
        _               -> shape
    Move a b c s     -> Move a b c (go s)
    RotateX a s      -> RotateX a (go s)
    RotateY a s      -> RotateY a (go s)
    RotateZ a s      -> RotateZ a (go s)
    Scale a b c s    -> Scale a b c (go s)
    Repeat a b c s   -> Repeat a b c (go s)
    Group ss         -> Group (map go ss)
    Union a b        -> Union (go a) (go b)
    Intersection a b -> Intersection (go a) (go b)
    Difference a b   -> Difference (go a) (go b)
    Paint c ss       -> Paint c (map go ss)
    Material m ss    -> Material m (map go ss)
    _                -> shape -- primitives hold no inner shapes
  where
    go = resolveShape env

-- | THE POSTFIX COMPILER (Pre-baked Math & Vec4 Packed)
compileShape :: Env -> Float -> Shape -> [Float]
compileShape env mat shape = fst (compileShapeD env mat shape)

-- | Compiled bytecode paired with the peak hit-stack depth it needs. The
--   viewer turns every stack slot into a GLSL register, so keeping this
--   small keeps generated shaders small; see 'emitCommutative'.
type Compiled = ([Float], Int)

compileShapeD :: Env -> Float -> Shape -> Compiled
compileShapeD env mat shape = case shape of

    Cube ex ey ez ->
        let w = evalExpr env ex / 2.0; h = evalExpr env ey / 2.0; d = evalExpr env ez / 2.0
        in ([2.0, w, h, d, mat, 0.0, 0.0, 0.0], 1) -- 2 vec4s

    Sphere er ->
        let r = evalExpr env er
        in ([1.0, r, mat, 0.0], 1) -- 1 vec4

    Cylinder er eh ->
        let r = evalExpr env er; h = evalExpr env eh / 2.0
        in ([3.0, r, h, mat], 1) -- 1 vec4

    Cone er et eh ->
        let r1 = evalExpr env er; r2 = evalExpr env et; h = evalExpr env eh / 2.0
        in ([4.0, r1, r2, h, mat, 0.0, 0.0, 0.0], 1) -- 2 vec4s

    Torus er et ->
        let r = evalExpr env er; tr = evalExpr env et
        in ([5.0, r, tr, mat], 1) -- 1 vec4

    -- CSG ops compute (first pushed) OP (top of stack) in both the viewer
    -- and the OBJ exporter. difference(a, b) is "a with b cut out", so `a`
    -- is pushed first; its order is fixed, unlike the commutative ops.
    Union a b        -> emitCommutative 10.0 (go a) (go b)
    Difference a b   -> emitBinary 11.0 (go a) (go b)
    Intersection a b -> emitCommutative 12.0 (go a) (go b)

    -- Transform Ops
    Move ex ey ez innerShape ->
        let x = evalExpr env ex; y = evalExpr env ey; z = evalExpr env ez
        in wrapTransform [24.0, x, y, z] innerShape

    -- PRE-BAKED TRIGONOMETRY: GPU no longer calculates sin/cos
    RotateX edeg innerShape ->
        let rads = degToRad (evalExpr env edeg)
        in wrapTransform [20.0, cos rads, sin rads, 0.0] innerShape

    RotateY edeg innerShape ->
        let rads = degToRad (evalExpr env edeg)
        in wrapTransform [21.0, cos rads, sin rads, 0.0] innerShape

    RotateZ edeg innerShape ->
        let rads = degToRad (evalExpr env edeg)
        in wrapTransform [22.0, cos rads, sin rads, 0.0] innerShape

    -- PRE-BAKED DIVISION: GPU now uses fast multiplication
    Scale ex ey ez innerShape ->
        let sx = evalExpr env ex; sy = evalExpr env ey; sz = evalExpr env ez
            min_s = min sx (min sy sz)
            isx = if sx == 0 then 0 else 1.0 / sx
            isy = if sy == 0 then 0 else 1.0 / sy
            isz = if sz == 0 then 0 else 1.0 / sz
        in wrapTransform [23.0, isx, isy, isz, min_s, 0.0, 0.0, 0.0] innerShape

    Repeat ex ey ez innerShape ->
        let cx = evalExpr env ex; cy = evalExpr env ey; cz = evalExpr env ez
        in wrapTransform [26.0, cx, cy, cz] innerShape

    -- Color Ops
    Paint cName innerShapes ->
        let (code, depth) = compileGroupD env mat innerShapes
        in if null code
            then ([], 0) -- nothing to paint: don't emit an empty push/pop
            else ([30.0] ++ resolveColor cName ++ code ++ [31.0, 0.0, 0.0, 0.0], depth)

    Material mName innerShapes ->
        compileGroupD env (resolveMaterial mName) innerShapes

    Group shapes ->
        compileGroupD env mat shapes

    ShapeRef name ->
        case Map.lookup name env of
            Just (VShape s) -> go s
            _ -> ([], 0)
  where
    go = compileShapeD env mat
    wrapTransform header innerShape =
        let (code, depth) = go innerShape
        in (header ++ code ++ [25.0, 0.0, 0.0, 0.0], depth)

-- | Emit `first`, then `second`, then a binary CSG op. While `second` is
--   being evaluated, `first`'s result occupies one stack slot.
emitBinary :: Float -> Compiled -> Compiled -> Compiled
emitBinary op (c1, d1) (c2, d2) = (c1 ++ c2 ++ [op, 0.0, 0.0, 0.0], max d1 (d2 + 1))

-- | For commutative ops, evaluate the deeper operand first (Sethi-Ullman
--   ordering). Without this, a chain like `a = union(a, x)` repeated N times
--   needs N stack slots; with it, it needs 2. Ties keep the historical
--   operand order so existing bytecode is unchanged.
emitCommutative :: Float -> Compiled -> Compiled -> Compiled
emitCommutative op a b
    | snd a > snd b = emitBinary op a b
    | otherwise     = emitBinary op b a

-- | Union a list of shapes together. Members that compile to nothing (e.g.
--   an empty nested group) are skipped, so they can't leave the union with
--   a missing operand.
compileGroupD :: Env -> Float -> [Shape] -> Compiled
compileGroupD env mat shapes =
    case filter (not . null . fst) (map (compileShapeD env mat) shapes) of
        []     -> ([], 0)
        -- On ties the rest of the group is pushed first (historical order).
        pieces -> foldr1 (emitCommutative 10.0) pieces

-- | ENTRY POINT
--   defaultColor sets the colour drawn shapes get when they aren't
--   wrapped in their own paint(...) call.
--   Light opcodes (40/41) go at the very end of the bytecode, after all
--   geometry: the shader pre-scans for them, and the OBJ exporter's
--   distance walker stops there just like at a halt.
compileToBytecode :: (Float, Float, Float) -> Script -> [Float]
compileToBytecode defaultColor script =
    let (drawsRaw, lightOps) = runScript defaultColor Map.empty script
        draws = filter (not . null) drawsRaw
    in case draws of
        [] -> lightOps ++ [0.0, 0.0, 0.0, 0.0] -- Halt vec4

        (firstDraw:restDraws) ->
            firstDraw ++ concatMap (\d -> d ++ [10.0, 0.0, 0.0, 0.0]) restDraws
                      ++ lightOps ++ [0.0, 0.0, 0.0, 0.0]

-- | SCRIPT RUNNER
-- Returns one bytecode array per Draw statement, plus the lighting opcodes
-- from the last light(...) statement in the script (empty if there is none).
runScript :: (Float, Float, Float) -> Env -> Script -> ([[Float]], [Float])
runScript _ _ [] = ([], [])
runScript defaultColor env (stmt:rest) = case stmt of
    -- `a = b` where b is a shape variable defines a shape alias
    Assign name (Var ref) | Just (VShape s) <- Map.lookup ref env ->
        runScript defaultColor (Map.insert name (VShape s) env) rest

    Assign name expr ->
        runScript defaultColor (Map.insert name (VNum (evalExpr env expr)) env) rest

    -- Resolve references eagerly so stored shapes are self-contained;
    -- see resolveShape for why this matters.
    AssignShape name shape ->
        runScript defaultColor (Map.insert name (VShape (resolveShape env shape)) env) rest

    Draw shape ->
        let (r, g, b) = defaultColor
            inner = compileShape env 0.0 shape
            (draws, lights) = runScript defaultColor env rest
            -- Push the configured default colour before the shape, and pop
            -- it after, so an explicit paint(...) inside still overrides it.
            bytecode = [30.0, r, g, b] ++ inner ++ [31.0, 0.0, 0.0, 0.0]
        -- A draw that produced no geometry (e.g. an empty group()) must be
        -- skipped entirely: emitting its paint wrapper would make the
        -- union chain pop shapes that were never pushed.
        in if null inner
            then (draws, lights)
            else (bytecode : draws, lights)

    Light ex ey ez ei ->
        let x = evalExpr env ex; y = evalExpr env ey; z = evalExpr env ez
            i = evalExpr env ei
            ops = [40.0, x, y, z, 41.0, i, 0.0, 0.0]
            (draws, laterLights) = runScript defaultColor env rest
        in (draws, if null laterLights then ops else laterLights)

    -- Camera keyframes never become geometry bytecode; they travel to the
    -- viewer separately as a CMD:SET_ANIM command (see cameraTrack).
    CameraKey {} -> runScript defaultColor env rest

-- | Collect the camera animation keyframes from a script, evaluated with
--   the same variable environment the geometry sees. Each keyframe is
--   [t, yaw, pitch, dist, tx, ty, tz] with angles already converted to
--   radians (matching the viewer's internal camera units). Keyframes are
--   sorted by time; the last keyframe's time is the loop duration.
cameraTrack :: Script -> [[Float]]
cameraTrack = go Map.empty
  where
    go _ [] = []
    go env (stmt:rest) = case stmt of
        Assign name (Var ref) | Just (VShape s) <- Map.lookup ref env ->
            go (Map.insert name (VShape s) env) rest
        Assign name expr ->
            go (Map.insert name (VNum (evalExpr env expr)) env) rest
        AssignShape name shape ->
            go (Map.insert name (VShape (resolveShape env shape)) env) rest
        CameraKey et eyaw ep ed etx ety etz ->
            let key = [ evalExpr env et
                      , degToRad (evalExpr env eyaw)
                      , degToRad (evalExpr env ep)
                      , evalExpr env ed
                      , evalExpr env etx
                      , evalExpr env ety
                      , evalExpr env etz
                      ]
            in insertByTime key (go env rest)
        _ -> go env rest

    -- Insertion sort by keyframe time keeps out-of-order scripts working.
    insertByTime key [] = [key]
    insertByTime key (k:ks)
        | head key <= head k = key : k : ks
        | otherwise          = k : insertByTime key ks