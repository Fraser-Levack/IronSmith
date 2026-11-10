-- src/Exporter.hs
-- Exports the currently compiled CSG/SDF scene to a mesh: a Wavefront .obj
-- (geometry only) or a binary glTF .glb (geometry + paint colours and
-- materials, ready for three.js / Blender / any glTF viewer).
--
-- The scene is compiled to the same "bytecode" array that the GPU
-- raymarcher (IronSmith-Viewer/src/core.glsl, the `map` function) walks
-- as a tiny stack machine. This module is a CPU-side port of that
-- interpreter (evalSDF is distance-only; evalHit also tracks paint/material),
-- which is then sampled on a grid and triangulated with Marching
-- Tetrahedra (each grid cube split into 6 tetrahedra, each tetrahedron
-- has only 16 sign configurations) to produce a watertight surface
-- that correctly reflects CSG union/difference/intersection.
module Exporter
    ( exportToOBJ
    , exportToGLB
    , evalHit
    , sdSphere
    , sdBox
    , sdCylinder
    , sdTorus
    , sdCappedCone
    , evalSDF
    , estimateBounds
    ) where

import Data.Array.Unboxed (UArray, listArray, (!), bounds, rangeSize)
import Data.Array.IO (IOUArray)
import Data.Array.MArray (newArray, writeArray, freeze)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.List (foldl')
import Control.Monad (forM_, foldM)
import System.IO (withFile, IOMode(WriteMode), Handle, hPutStrLn)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as B
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import Numeric (showHex)

type Vec3 = (Float, Float, Float)

-- ─── VECTOR HELPERS ──────────────────────────────────────────────────────────

vadd, vsub :: Vec3 -> Vec3 -> Vec3
vadd (x0,y0,z0) (x1,y1,z1) = (x0+x1, y0+y1, z0+z1)
vsub (x0,y0,z0) (x1,y1,z1) = (x0-x1, y0-y1, z0-z1)

vabs :: Vec3 -> Vec3
vabs (x,y,z) = (abs x, abs y, abs z)

vlength :: Vec3 -> Float
vlength (x,y,z) = sqrt (x*x + y*y + z*z)

cross :: Vec3 -> Vec3 -> Vec3
cross (ax,ay,az) (bx,by,bz) = (ay*bz - az*by, az*bx - ax*bz, ax*by - ay*bx)

dot :: Vec3 -> Vec3 -> Float
dot (ax,ay,az) (bx,by,bz) = ax*bx + ay*by + az*bz

lerp :: Vec3 -> Vec3 -> Float -> Vec3
lerp (x0,y0,z0) (x1,y1,z1) t = (x0 + (x1-x0)*t, y0 + (y1-y0)*t, z0 + (z1-z0)*t)

centroid :: [Vec3] -> Vec3
centroid vs = let (sx,sy,sz) = foldl' vadd (0,0,0) vs
                  n = fromIntegral (length vs)
              in (sx/n, sy/n, sz/n)

-- ─── SDF PRIMITIVES (ported from IronSmith-Viewer/src/core.glsl) ─────────────

sdSphere :: Vec3 -> Float -> Float
sdSphere p s = vlength p - s

sdBox :: Vec3 -> Vec3 -> Float
sdBox p b =
    let (qx,qy,qz) = vsub (vabs p) b
        qmax0 = (max qx 0, max qy 0, max qz 0)
    in vlength qmax0 + min (max qx (max qy qz)) 0.0

-- | p h r, matching GLSL sdCylinder(p, h, r)
sdCylinder :: Vec3 -> Float -> Float -> Float
sdCylinder (px,py,pz) h r =
    let dx = sqrt (px*px + pz*pz) - r
        dy = abs py - h
    in min (max dx dy) 0.0 + sqrt (max dx 0.0 ** 2 + max dy 0.0 ** 2)

sdTorus :: Vec3 -> (Float, Float) -> Float
sdTorus (px,py,pz) (r, tr) =
    let qx = sqrt (px*px + pz*pz) - r
        qy = py
    in sqrt (qx*qx + qy*qy) - tr

-- | p h r1 r2, matching GLSL sdCappedCone(p, h, r1, r2)
sdCappedCone :: Vec3 -> Float -> Float -> Float -> Float
sdCappedCone (px,py,pz) h r1 r2 =
    let qx = sqrt (px*px + pz*pz)
        qy = py
        k1x = r2; k1y = h
        k2x = r2 - r1; k2y = 2*h
        caX = qx - min qx (if qy < 0 then r1 else r2)
        caY = abs qy - h
        t = clamp01 (((k1x-qx)*k2x + (k1y-qy)*k2y) / (k2x*k2x + k2y*k2y))
        cbX = qx - k1x + k2x*t
        cbY = qy - k1y + k2y*t
        s = if cbX < 0 && caY < 0 then -1 else 1
    in s * sqrt (min (caX*caX + caY*caY) (cbX*cbX + cbY*cbY))
  where
    clamp01 x = max 0 (min 1 x)

-- | mod(p + 0.5*c, c) - 0.5*c per axis, for axes where c > 0 (opRep in core.glsl)
opRep :: Vec3 -> Vec3 -> Vec3
opRep (px,py,pz) (cx,cy,cz) = (rep px cx, rep py cy, rep pz cz)
  where
    rep p c
      | c > 0     = glslMod (p + 0.5*c) c - 0.5*c
      | otherwise = p
    glslMod x y = x - y * fromIntegral (floor (x/y) :: Int)

rotX, rotY, rotZ :: (Float, Float) -> Vec3 -> Vec3
rotX (c,s) (x,y,z) = (x, y*c - z*s, y*s + z*c)
rotY (c,s) (x,y,z) = (x*c + z*s, y, -x*s + z*c)
rotZ (c,s) (x,y,z) = (x*c - y*s, x*s + y*c, z)

-- ─── BYTECODE INTERPRETER (CPU port of map() in core.glsl) ────────────────────

-- | Evaluate the scene's signed distance at a point, by walking the same
--   bytecode stack machine as the GPU raymarcher. Colour/material opcodes
--   (30/31) don't affect distance, so they're treated as no-ops here.
evalSDF :: UArray Int Float -> Vec3 -> Float
evalSDF instrs p0 = go 0 p0 1.0 [] []
  where
    numInstr = rangeSize (bounds instrs) `div` 4
    at pc k = instrs ! (pc*4 + k)

    go :: Int -> Vec3 -> Float -> [Float] -> [(Vec3,Float)] -> Float
    go pc p scale hits xforms
      | pc >= numInstr = finish hits
      | otherwise =
          case (round (at pc 0) :: Int) of
            0  -> finish hits
            1  -> go (pc+1) p scale (sdSphere p (at pc 1) * scale : hits) xforms
            2  -> go (pc+2) p scale (sdBox p (at pc 1, at pc 2, at pc 3) * scale : hits) xforms
            3  -> go (pc+1) p scale (sdCylinder p (at pc 2) (at pc 1) * scale : hits) xforms
            4  -> go (pc+2) p scale (sdCappedCone p (at pc 3) (at pc 1) (at pc 2) * scale : hits) xforms
            5  -> go (pc+1) p scale (sdTorus p (at pc 1, at pc 2) * scale : hits) xforms
            10 -> case hits of
                    (b:a:rest) -> go (pc+1) p scale (min a b : rest) xforms
                    _          -> go (pc+1) p scale hits xforms
            11 -> case hits of
                    (b:a:rest) -> go (pc+1) p scale (max a (-b) : rest) xforms
                    _          -> go (pc+1) p scale hits xforms
            12 -> case hits of
                    (b:a:rest) -> go (pc+1) p scale (max a b : rest) xforms
                    _          -> go (pc+1) p scale hits xforms
            20 -> let p' = rotX (at pc 1, at pc 2) p in go (pc+1) p' scale hits ((p,scale):xforms)
            21 -> let p' = rotY (at pc 1, at pc 2) p in go (pc+1) p' scale hits ((p,scale):xforms)
            22 -> let p' = rotZ (at pc 1, at pc 2) p in go (pc+1) p' scale hits ((p,scale):xforms)
            23 -> let (px,py,pz) = p
                      p' = (px * at pc 1, py * at pc 2, pz * at pc 3)
                      scale' = scale * at (pc+1) 0
                  in go (pc+2) p' scale' hits ((p,scale):xforms)
            24 -> let (px,py,pz) = p
                      p' = (px - at pc 1, py - at pc 2, pz - at pc 3)
                  in go (pc+1) p' scale hits ((p,scale):xforms)
            25 -> case xforms of
                    ((pp,ss):rest) -> go (pc+1) pp ss hits rest
                    []             -> go (pc+1) p scale hits xforms
            26 -> let p' = opRep p (at pc 1, at pc 2, at pc 3) in go (pc+1) p' scale hits ((p,scale):xforms)
            30 -> go (pc+1) p scale hits xforms
            31 -> go (pc+1) p scale hits xforms
            -- Lighting opcodes don't affect distance
            40 -> go (pc+1) p scale hits xforms
            41 -> go (pc+1) p scale hits xforms
            _  -> finish hits

    finish (d:_) = d
    finish []    = 999999.0

-- | A surface hit as the GPU sees it: distance, paint colour (linear RGB)
--   and material id (0 matte, 1 plastic, 2 neon, 3 metal).
type Hit = (Float, Vec3, Int)

-- | Like 'evalSDF', but also tracks which paint colour and material wins at
--   the point, using the same rules as the viewer's map()/opU/opS/opI in
--   core.glsl. A difference's cut faces take the colour of the shape being
--   cut, exactly as in the viewer. Slower than 'evalSDF' (it carries the
--   colour stack), so the exporter only calls it once per triangle.
evalHit :: UArray Int Float -> Vec3 -> Hit
evalHit instrs p0 = go 0 p0 1.0 (0.8, 0.8, 0.8) [] [] []
  where
    numInstr = rangeSize (bounds instrs) `div` 4
    at pc k = instrs ! (pc*4 + k)
    mat x = round x :: Int

    go :: Int -> Vec3 -> Float -> Vec3 -> [Vec3] -> [Hit] -> [(Vec3,Float)] -> Hit
    go pc p scale col cols hits xforms
      | pc >= numInstr = finish hits
      | otherwise =
          let next n hs = go (pc+n) p scale col cols hs xforms
              push n d m = next n ((d * scale, col, m) : hits)
              withP n p' scale' = go (pc+n) p' scale' col cols hits ((p,scale):xforms)
              binop f = case hits of
                  (b:a:rest) -> next 1 (f a b : rest)
                  _          -> next 1 hits
          in case (round (at pc 0) :: Int) of
            0  -> finish hits
            1  -> push 1 (sdSphere p (at pc 1)) (mat (at pc 2))
            2  -> push 2 (sdBox p (at pc 1, at pc 2, at pc 3)) (mat (at (pc+1) 0))
            3  -> push 1 (sdCylinder p (at pc 2) (at pc 1)) (mat (at pc 3))
            4  -> push 2 (sdCappedCone p (at pc 3) (at pc 1) (at pc 2)) (mat (at (pc+1) 0))
            5  -> push 1 (sdTorus p (at pc 1, at pc 2)) (mat (at pc 3))
            10 -> binop (\a@(da,_,_) b@(db,_,_) -> if da < db then a else b)
            11 -> binop (\a@(da,ca,ma) (db,_,_) -> if da > -db then a else (-db, ca, ma))
            12 -> binop (\a@(da,_,_) b@(db,_,_) -> if da > db then a else b)
            20 -> withP 1 (rotX (at pc 1, at pc 2) p) scale
            21 -> withP 1 (rotY (at pc 1, at pc 2) p) scale
            22 -> withP 1 (rotZ (at pc 1, at pc 2) p) scale
            23 -> let (px,py,pz) = p
                  in withP 2 (px * at pc 1, py * at pc 2, pz * at pc 3) (scale * at (pc+1) 0)
            24 -> let (px,py,pz) = p
                  in withP 1 (px - at pc 1, py - at pc 2, pz - at pc 3) scale
            25 -> case xforms of
                    ((pp,ss):rest) -> go (pc+1) pp ss col cols hits rest
                    []             -> next 1 hits
            26 -> withP 1 (opRep p (at pc 1, at pc 2, at pc 3)) scale
            30 -> go (pc+1) p scale (at pc 1, at pc 2, at pc 3) (col:cols) hits xforms
            31 -> case cols of
                    (c:rest) -> go (pc+1) p scale c rest hits xforms
                    []       -> next 1 hits
            40 -> next 1 hits
            41 -> next 1 hits
            _  -> finish hits

    finish (h:_) = h
    finish []    = (999999.0, (0.8, 0.8, 0.8), 0)

-- ─── SAMPLING GRID ─────────────────────────────────────────────────────────────

-- | The sampling grid: (gNx+1) x (gNy+1) x (gNz+1) points spaced `gCell`
--   apart, starting at `gOrigin`. It is fitted to the model's bounding box
--   ('fitGrid'), so every model gets the same detail across its longest
--   side, however small it is or wherever it sits.
data Grid = Grid
    { gOrigin :: !Vec3
    , gCell   :: !Float
    , gNx     :: !Int
    , gNy     :: !Int
    , gNz     :: !Int
    }

-- | Number of sample points in the grid.
gridPoints :: Grid -> Int
gridPoints g = (gNx g + 1) * (gNy g + 1) * (gNz g + 1)

-- | Sample points in one slab (all points with the same first index).
planeSize :: Grid -> Int
planeSize g = (gNy g + 1) * (gNz g + 1)

-- | Linear index of grid point (i, j, k).
pointIndex :: Grid -> Int -> Int -> Int -> Int
pointIndex g i j k = (i * (gNy g + 1) + j) * (gNz g + 1) + k

-- | World position of grid point (i, j, k).
pointPos :: Grid -> Int -> Int -> Int -> Vec3
pointPos g i j k =
    let (ox, oy, oz) = gOrigin g
        c = gCell g
    in (ox + fromIntegral i * c, oy + fromIntegral j * c, oz + fromIntegral k * c)

-- | A grid around `box` with `res` cells along its longest side. The box
--   from 'estimateBounds' can be up to half a scan cell short of the
--   surface, so it is padded by a scan cell plus two grid cells: the
--   surface never touches the edge of the grid, and the mesh stays closed.
fitGrid :: Int -> (Vec3, Vec3) -> Grid
fitGrid res ((x0,y0,z0), (x1,y1,z1)) =
    Grid (x0 - pad, y0 - pad, z0 - pad) cell (cells x0 x1) (cells y0 y1) (cells z0 z1)
  where
    longest = max 1e-3 (maximum [x1 - x0, y1 - y0, z1 - z0])
    scanPad = longest / fromIntegral boundingScanResolution
    cell = (longest + 2 * scanPad) / fromIntegral (max 1 res)
    pad = scanPad + 2 * cell
    cells lo hi = max 1 (ceiling ((hi - lo + 2 * pad) / cell))

-- | The scene's SDF and a grid fitted to it, or Nothing if it draws nothing.
exportGrid :: UArray Int Float -> Int -> Maybe (Vec3 -> Float, Grid)
exportGrid arr res
    | rangeSize (bounds arr) == 0 = Nothing
    | otherwise = do
        let sdf = evalSDF arr
        box <- sdfBounds sdf
        return (sdf, fitGrid res box)

-- | Sample the SDF at every grid point. Reports progress in [0, 0.5] as it
--   goes (sampling is roughly as expensive as the meshing pass that follows).
sampleGrid :: (Vec3 -> Float) -> Grid -> (Float -> IO ()) -> IO (UArray Int Float)
sampleGrid sdf g report = do
    arr <- newArray (0, gridPoints g - 1) 0 :: IO (IOUArray Int Float)
    forM_ [0 .. gNx g] $ \i -> do
        forM_ [0 .. gNy g] $ \j ->
            forM_ [0 .. gNz g] $ \k ->
                writeArray arr (pointIndex g i j k) (sdf (pointPos g i j k))
        report (0.5 * fromIntegral (i+1) / fromIntegral (gNx g + 1))
    freeze arr

-- ─── MARCHING TETRAHEDRA ───────────────────────────────────────────────────────

-- | A sampled grid point: its index, world position and SDF value.
data SVtx = SVtx { svIdx :: !Int, svPos :: !Vec3, svVal :: !Float }

-- | A mesh vertex. Every vertex lies on one grid edge, and `mvKey` names
--   that edge, so triangles that share an edge share the vertex: the
--   exporters use it to write each vertex once (an indexed mesh).
data MVtx = MVtx { mvKey :: !Int, mvPos :: !Vec3 }

-- Corner order matches the cube diagram used for the tet decomposition below:
--   0:(0,0,0) 1:(1,0,0) 2:(1,0,1) 3:(0,0,1) 4:(0,1,0) 5:(1,1,0) 6:(1,1,1) 7:(0,1,1)
cornerOffsets :: [(Int,Int,Int)]
cornerOffsets = [(0,0,0),(1,0,0),(1,0,1),(0,0,1),(0,1,0),(1,1,0),(1,1,1),(0,1,1)]

-- | Triangles of one grid slab (all cells with first index `i`), each
--   wound so its normal points out of the surface.
slabTriangles :: Grid -> UArray Int Float -> Int -> [(MVtx,MVtx,MVtx)]
slabTriangles g grid i =
    [ tri
    | j <- [0 .. gNy g - 1], k <- [0 .. gNz g - 1]
    , let corners = [ SVtx ix (pointPos g (i+dx) (j+dy) (k+dz)) (grid ! ix)
                    | (dx,dy,dz) <- cornerOffsets
                    , let ix = pointIndex g (i+dx) (j+dy) (k+dz) ]
    , tet <- cubeTets corners
    , tri <- tetTriangles (gridPoints g) tet
    ]

-- | Vertex keys below this belong to edges that start in slab `i` or
--   earlier. Once slab `i` is meshed no later triangle can use them (the
--   next slab only shares the points of plane i+1), so they can be dropped.
retiredBelow :: Grid -> Int -> Int
retiredBelow g i = (i + 1) * planeSize g * gridPoints g

-- | Split a cube (given its 8 corners in the order above) into 6
--   tetrahedra, all sharing the body diagonal between corners 0 and 6.
cubeTets :: [SVtx] -> [(SVtx,SVtx,SVtx,SVtx)]
cubeTets c =
    [ (c0, c !! a, c !! b, c6)
    | (a,b) <- zip ring (tail ring ++ [head ring]) ]
  where
    c0 = head c
    c6 = c !! 6
    ring = [1,2,3,7,4,5]

-- | Triangulate a single tetrahedron based on which corners are "inside"
--   (negative SDF). Each triangle's winding is oriented so its normal
--   points towards the "outside" of the surface. `npts` is the number of
--   grid points (for edge keys).
tetTriangles :: Int -> (SVtx,SVtx,SVtx,SVtx) -> [(MVtx,MVtx,MVtx)]
tetTriangles npts (v0,v1,v2,v3) =
    case (ins, outs) of
        ([a], [b,c,d]) ->
            let outward = vsub (centroid [svPos b, svPos c, svPos d]) (svPos a)
            in [orient (x a b, x a c, x a d) outward]
        ([a,b], [c,d]) ->
            let outward = vsub (centroid [svPos c, svPos d]) (centroid [svPos a, svPos b])
                pac = x a c; pad = x a d
                pbc = x b c; pbd = x b d
            in [ orient (pac, pad, pbd) outward
               , orient (pac, pbd, pbc) outward
               ]
        ([a,b,c], [d]) ->
            let outward = vsub (svPos d) (centroid [svPos a, svPos b, svPos c])
            in [orient (x a d, x b d, x c d) outward]
        _ -> [] -- all 4 corners on the same side: no surface in this tet
  where
    vs = [v0,v1,v2,v3]
    ins  = filter (\v -> svVal v < 0) vs
    outs = filter (\v -> svVal v >= 0) vs
    x = edgeXing npts

-- | The zero-crossing on the edge between two grid points, keyed by the
--   edge. Interpolates from the lower-indexed end so the position doesn't
--   depend on which tetrahedron asks.
edgeXing :: Int -> SVtx -> SVtx -> MVtx
edgeXing npts p0 q0 =
    let (p, q) = if svIdx p0 < svIdx q0 then (p0, q0) else (q0, p0)
        vp = svVal p; vq = svVal q
    in MVtx (svIdx p * npts + svIdx q) (lerp (svPos p) (svPos q) (vp / (vp - vq)))

-- | Flip a triangle's winding (if needed) so its normal points towards `outward`.
orient :: (MVtx,MVtx,MVtx) -> Vec3 -> (MVtx,MVtx,MVtx)
orient (p0,p1,p2) outward =
    let n = cross (vsub (mvPos p1) (mvPos p0)) (vsub (mvPos p2) (mvPos p0))
    in if dot n outward >= 0 then (p0,p1,p2) else (p0,p2,p1)

-- ─── BOUNDS ────────────────────────────────────────────────────────────────────

-- | Half-size of the cube (centred on the origin) that is searched for the
--   model. Matches the raymarcher's `scene_radius` in core.glsl.
exportRadius :: Float
exportRadius = 25.0

-- | Grid resolution used for the quick bounding-box scans below. Coarse is
--   fine here - it just needs to find roughly where the model sits.
boundingScanResolution :: Int
boundingScanResolution = 32

-- | Approximate axis-aligned bounds (min corner, max corner) of a compiled
--   scene, limited to the cube of half-size 'exportRadius'. Nothing if the
--   scene draws nothing. A coarse scan of the whole cube finds the model,
--   then a second scan of just that region tightens the box to a few
--   percent of the model's size. The CLI uses this to frame renders, and
--   the exporters fit their sampling grid to it.
estimateBounds :: [Float] -> Maybe (Vec3, Vec3)
estimateBounds [] = Nothing
estimateBounds bytecode = sdfBounds (evalSDF (listArray (0, length bytecode - 1) bytecode))

-- | 'estimateBounds' for an SDF.
sdfBounds :: (Vec3 -> Float) -> Maybe (Vec3, Vec3)
sdfBounds sdf = do
    let whole = ((-exportRadius, -exportRadius, -exportRadius), (exportRadius, exportRadius, exportRadius))
    coarse <- scanBounds sdf whole
    scanBounds sdf (grow (scanCell whole) coarse)
  where
    grow d ((x0,y0,z0), (x1,y1,z1)) = ((x0-d, y0-d, z0-d), (x1+d, y1+d, z1+d))

-- | Bounds of the sample points within one cell of the surface, on a
--   'boundingScanResolution' grid spanning `box`.
scanBounds :: (Vec3 -> Float) -> (Vec3, Vec3) -> Maybe (Vec3, Vec3)
scanBounds sdf box@((x0,y0,z0), (x1,y1,z1)) =
    case near of
        [] -> Nothing
        _  -> Just ( (minimum xs, minimum ys, minimum zs)
                   , (maximum xs, maximum ys, maximum zs) )
  where
    res = boundingScanResolution
    cell = scanCell box
    axis lo hi = [lo + (hi - lo) * fromIntegral i / fromIntegral res | i <- [0..res]]
    near = [ (x,y,z) | x <- axis x0 x1, y <- axis y0 y1, z <- axis z0 z1
                     , sdf (x,y,z) <= cell ]
    xs = [x | (x,_,_) <- near]
    ys = [y | (_,y,_) <- near]
    zs = [z | (_,_,z) <- near]

-- | Grid spacing along the box's longest side.
scanCell :: (Vec3, Vec3) -> Float
scanCell ((x0,y0,z0), (x1,y1,z1)) =
    maximum [x1 - x0, y1 - y0, z1 - z0] / fromIntegral boundingScanResolution

-- ─── OBJ OUTPUT ────────────────────────────────────────────────────────────────

-- | Compile the scene's bytecode into a mesh and stream it to `path` as a
--   Wavefront .obj file, with `res` grid cells across the model's longest
--   side. Calls `report` with progress in [0,1] as the export proceeds
--   (sampling is the first half, meshing/writing is the second half).
--
--   The mesh is indexed: each vertex is written once, just before the
--   first face that uses it, and shared by every face around it.
exportToOBJ :: [Float] -> FilePath -> Int -> (Float -> IO ()) -> IO ()
exportToOBJ bytecode path res report =
    withFile path WriteMode $ \h ->
        case exportGrid arr res of
            Nothing -> report 1
            Just (sdf, g) -> do
                grid <- sampleGrid sdf g report
                ids <- newIORef IM.empty
                count <- newIORef (0 :: Int)
                let vertexId v = do
                        known <- readIORef ids
                        case IM.lookup (mvKey v) known of
                            Just n -> return n
                            Nothing -> do
                                n <- (+ 1) <$> readIORef count
                                writeIORef count n
                                writeIORef ids (IM.insert (mvKey v) n known)
                                hPutStrLn h (vline (mvPos v))
                                return n
                forM_ [0 .. gNx g - 1] $ \i -> do
                    forM_ (slabTriangles g grid i) $ \(a,b,c) -> do
                        na <- vertexId a
                        nb <- vertexId b
                        nc <- vertexId c
                        hPutStrLn h ("f " ++ show na ++ " " ++ show nb ++ " " ++ show nc)
                    modifyIORef' ids (snd . IM.split (retiredBelow g i - 1))
                    report (0.5 + 0.5 * fromIntegral (i+1) / fromIntegral (gNx g))
  where
    arr = listArray (0, length bytecode - 1) bytecode :: UArray Int Float
    vline (x,y,z) = "v " ++ show x ++ " " ++ show y ++ " " ++ show z

-- ─── GLB (BINARY glTF 2.0) OUTPUT ──────────────────────────────────────────────

-- | One glTF primitive: every triangle sharing a paint colour and material.
--   Vertex and index data are packed little-endian into one chunk per
--   grid slab (newest first), so big exports stay compact in memory.
data Group = Group
    { gVerts   :: !Int               -- vertices so far
    , gIndices :: !Int               -- indices so far (3 per triangle)
    , gMin     :: !Vec3
    , gMax     :: !Vec3
    , gIds     :: !(IM.IntMap Int)   -- edge key -> vertex index (live edges only)
    , gNewPos  :: [Vec3]             -- this slab's new vertices, newest first
    , gNewNrm  :: [Vec3]
    , gNewIdx  :: [Int]              -- this slab's indices, newest first
    , gPos     :: [BS.ByteString]    -- packed chunks, newest first
    , gNrm     :: [BS.ByteString]
    , gIdx     :: [BS.ByteString]
    }

emptyGroup :: Group
emptyGroup = Group 0 0 (1/0, 1/0, 1/0) (-1/0, -1/0, -1/0) IM.empty [] [] [] [] [] []

-- | Material key: (material id, colour). Colours are compared exactly, which
--   is fine because they come straight from the bytecode.
type MatKey = (Int, Vec3)

-- | Compile the scene's bytecode into a mesh and write it to `path` as a
--   binary glTF 2.0 (.glb) file, with `res` grid cells across the model's
--   longest side.
--
--   Uses the same grid, sampling and Marching Tetrahedra as
--   'exportToOBJ', then:
--
--   * colours each triangle by evaluating the scene's paint/material at
--     its centroid (the same winner the raymarcher picks there),
--   * groups triangles into one glTF primitive per (colour, material),
--     each with a PBR material approximating the viewer's look
--     (matte, plastic, neon = emissive, metal = metallic),
--   * gives every vertex a smooth normal from the SDF gradient, so curved
--     surfaces shade smoothly instead of looking faceted,
--   * shares vertices between triangles (indexed, uint32 indices), which
--     makes the file about 4x smaller than a triangle soup.
--
--   Colours are written as linear RGB, matching the viewer, which treats
--   paint colours as linear and gamma-corrects at the end. Progress is
--   reported in [0,1] (sampling first half, meshing second half).
exportToGLB :: [Float] -> FilePath -> Int -> (Float -> IO ()) -> IO ()
exportToGLB bytecode path res report = do
    groups <- case exportGrid arr res of
        Nothing -> return []
        Just (sdf, g) -> do
            grid <- sampleGrid sdf g report
            let eps = 0.1 * gCell g
                normalAt p fallback =
                    let n = sdfNormal sdf eps p
                    in if vlength n > 1e-6 then vnorm n else fallback
                addTri m (a,b,c) =
                    let (_, col, matId) = evalHit arr (centroid (map mvPos [a,b,c]))
                        face = vnorm (cross (vsub (mvPos b) (mvPos a)) (vsub (mvPos c) (mvPos a)))
                        grp = M.findWithDefault emptyGroup (matId, col) m
                    in M.insert (matId, col) (foldl' (addVertex (`normalAt` face)) grp [a,b,c]) m
                keep i = snd . IM.split (retiredBelow g i - 1)
            final <- foldM (\acc i -> do
                        let acc' = M.map (flushGroup (keep i)) (foldl' addTri acc (slabTriangles g grid i))
                        acc' `seq` report (0.5 + 0.5 * fromIntegral (i+1) / fromIntegral (gNx g))
                        return acc')
                    M.empty [0 .. gNx g - 1]
            return (M.toList final)
    BL.writeFile path (B.toLazyByteString (glbFile groups))
  where
    arr = listArray (0, length bytecode - 1) bytecode :: UArray Int Float

-- | Add one triangle corner to a group: reuse the vertex if its edge has
--   been seen, otherwise append it with its normal.
addVertex :: (Vec3 -> Vec3) -> Group -> MVtx -> Group
addVertex normalAt grp v =
    case IM.lookup (mvKey v) (gIds grp) of
        Just ix -> grp { gNewIdx = ix : gNewIdx grp, gIndices = gIndices grp + 1 }
        Nothing ->
            let ix = gVerts grp
                p = mvPos v
            in grp { gVerts = ix + 1
                   , gIndices = gIndices grp + 1
                   , gMin = vmin (gMin grp) p
                   , gMax = vmax (gMax grp) p
                   , gIds = IM.insert (mvKey v) ix (gIds grp)
                   , gNewPos = p : gNewPos grp
                   , gNewNrm = normalAt p : gNewNrm grp
                   , gNewIdx = ix : gNewIdx grp }

-- | Pack a group's pending slab data into byte chunks and drop vertex keys
--   no later slab can reach (`prune`). The packed chunks are forced here,
--   so finished slabs don't hold on to thunks.
flushGroup :: (IM.IntMap Int -> IM.IntMap Int) -> Group -> Group
flushGroup prune grp =
    let pack f xs = BL.toStrict (B.toLazyByteString (foldMap f (reverse xs)))
        ps = pack vec3LE (gNewPos grp)
        ns = pack vec3LE (gNewNrm grp)
        is = pack (B.word32LE . fromIntegral) (gNewIdx grp)
        ids = prune (gIds grp)
    in ps `seq` ns `seq` is `seq` ids `seq`
       grp { gIds = ids, gNewPos = [], gNewNrm = [], gNewIdx = []
           , gPos = ps : gPos grp, gNrm = ns : gNrm grp, gIdx = is : gIdx grp }

-- | Outward surface normal (unnormalised) from the SDF gradient, using the
--   same 4-tap tetrahedral stencil as the viewer.
sdfNormal :: (Vec3 -> Float) -> Float -> Vec3 -> Vec3
sdfNormal sdf h p =
    foldl' vadd (0,0,0)
        [ vscale k (sdf (vadd p (vscale k h)))
        | k <- [(1,-1,-1), (-1,-1,1), (-1,1,-1), (1,1,1)] ]

vscale :: Vec3 -> Float -> Vec3
vscale (x,y,z) s = (x*s, y*s, z*s)

vnorm :: Vec3 -> Vec3
vnorm v = let l = vlength v in if l > 0 then vscale v (1/l) else (0,1,0)

-- | Component-wise min / max, evaluated eagerly so long folds don't build
--   chains of thunks.
vmin, vmax :: Vec3 -> Vec3 -> Vec3
vmin (a,b,c) (x,y,z) = strict3 (min a x) (min b y) (min c z)
vmax (a,b,c) (x,y,z) = strict3 (max a x) (max b y) (max c z)

strict3 :: Float -> Float -> Float -> Vec3
strict3 a b c = a `seq` b `seq` c `seq` (a, b, c)

vec3LE :: Vec3 -> B.Builder
vec3LE (x,y,z) = B.floatLE x <> B.floatLE y <> B.floatLE z

-- | Assemble the .glb container: 12-byte header, JSON chunk, BIN chunk.
--   Each primitive gets three buffer views and accessors: POSITION,
--   NORMAL and its uint32 indices.
glbFile :: [(MatKey, Group)] -> B.Builder
glbFile groups =
    B.word32LE 0x46546C67 <> B.word32LE 2 <> B.word32LE (fromIntegral total)
    <> B.word32LE (fromIntegral jsonLen) <> B.word32LE 0x4E4F534A
    <> B.lazyByteString jsonBytes
    <> binChunk
  where
    -- (positions, normals, indices) byte sizes per primitive
    sizes = [ (12 * gVerts g, 12 * gVerts g, 4 * gIndices g) | (_, g) <- groups ]
    offsets = scanl (\o (p, n, i) -> o + p + n + i) 0 sizes
    binLen = last offsets
    hasBin = binLen > 0
    -- every element is 4 bytes wide, so no padding is needed
    binChunk
        | hasBin = B.word32LE (fromIntegral binLen) <> B.word32LE 0x004E4942
                   <> mconcat [ chunks (gPos g) <> chunks (gNrm g) <> chunks (gIdx g) | (_, g) <- groups ]
        | otherwise = mempty
    chunks = foldMap B.byteString . reverse
    jsonBytes = padJson (B.toLazyByteString (B.stringUtf8 (glbJson groups sizes offsets binLen)))
    jsonLen = fromIntegral (BL.length jsonBytes) :: Int
    total = 12 + 8 + jsonLen + (if hasBin then 8 + binLen else 0)
    padJson b = let r = BL.length b `mod` 4
                in if r == 0 then b else b <> BL.replicate (4 - r) 0x20

glbJson :: [(MatKey, Group)] -> [(Int, Int, Int)] -> [Int] -> Int -> String
glbJson groups sizes offsets binLen = obj $
    [ ("asset", obj [("version", str "2.0"), ("generator", str "IronSmith")])
    , ("scene", "0") ]
    ++ if null groups
         then [ ("scenes", arrJ [obj [("nodes", "[]")]]) ]
         else
           [ ("scenes", arrJ [obj [("nodes", "[0]")]])
           , ("nodes", arrJ [obj [("mesh", "0"), ("name", str "IronSmith model")]])
           , ("meshes", arrJ [obj [ ("name", str "IronSmith model")
                                  , ("primitives", arrJ (zipWith prim [0..] groups)) ]])
           , ("materials", arrJ (map (material . fst) groups))
           , ("accessors", arrJ (concat (zipWith accessors [0..] groups)))
           , ("bufferViews", arrJ (concat (zipWith views sizes offsets)))
           , ("buffers", arrJ [obj [("byteLength", show binLen)]])
           ]
  where
    prim :: Int -> (MatKey, Group) -> String
    prim i _ = obj [ ("attributes", obj [("POSITION", show (3*i)), ("NORMAL", show (3*i+1))])
                   , ("indices", show (3*i+2)), ("material", show i), ("mode", "4") ]
    accessors :: Int -> (MatKey, Group) -> [String]
    accessors i (_, g) =
        [ obj [ ("bufferView", show (3*i)), ("componentType", "5126"), ("count", show (gVerts g))
              , ("type", str "VEC3"), ("min", vec (gMin g)), ("max", vec (gMax g)) ]
        , obj [ ("bufferView", show (3*i+1)), ("componentType", "5126"), ("count", show (gVerts g))
              , ("type", str "VEC3") ]
        , obj [ ("bufferView", show (3*i+2)), ("componentType", "5125"), ("count", show (gIndices g))
              , ("type", str "SCALAR") ] ]
    views (p, n, ix) off =
        [ obj [("buffer", "0"), ("byteOffset", show off), ("byteLength", show p), ("target", "34962")]
        , obj [("buffer", "0"), ("byteOffset", show (off + p)), ("byteLength", show n), ("target", "34962")]
        , obj [("buffer", "0"), ("byteOffset", show (off + p + n)), ("byteLength", show ix), ("target", "34963")] ]

-- | A PBR material approximating the viewer's shading for each material id.
material :: MatKey -> String
material (matId, col@(r,g,b)) = obj $
    [ ("name", str (matName ++ " " ++ hexColor col))
    , ("pbrMetallicRoughness", obj [ ("baseColorFactor", arrJ (map num [r,g,b,1]))
                                   , ("metallicFactor", num metallic)
                                   , ("roughnessFactor", num rough) ]) ]
    ++ [ ("emissiveFactor", vec col) | matId == 2 ]
  where
    (matName, metallic, rough) = case matId of
        1 -> ("plastic", 0.0, 0.35)
        2 -> ("neon",    0.0, 1.0)
        3 -> ("metal",   1.0, 0.2)
        _ -> ("matte",   0.0, 1.0) :: (String, Float, Float)

hexColor :: Vec3 -> String
hexColor (r,g,b) = '#' : concatMap byte [r,g,b]
  where byte c = let v = max 0 (min 255 (round (c * 255) :: Int))
                     h = showHex v ""
                 in if length h < 2 then '0':h else h

-- Tiny JSON helpers (the document is small and fully under our control).
obj :: [(String, String)] -> String
obj kvs = "{" ++ intercalate "," [ str k ++ ":" ++ v | (k, v) <- kvs ] ++ "}"

arrJ :: [String] -> String
arrJ xs = "[" ++ intercalate "," xs ++ "]"

str :: String -> String
str s = "\"" ++ concatMap esc s ++ "\""
  where esc '"'  = "\\\""
        esc '\\' = "\\\\"
        esc c    = [c]

-- | A JSON number: 'show' on a finite Float is valid JSON.
num :: Float -> String
num x | isNaN x || isInfinite x = "0"
      | otherwise = show x

vec :: Vec3 -> String
vec (x,y,z) = arrJ (map num [x,y,z])
