-- src/Exporter.hs
-- Exports the currently compiled CSG/SDF scene to a Wavefront .obj mesh.
--
-- The scene is compiled to the same "bytecode" array that the GPU
-- raymarcher (IronSmith-Viewer/src/core.glsl, the `map` function) walks
-- as a tiny stack machine. This module is a CPU-side port of that
-- interpreter (distance-only; colour/material opcodes are no-ops here),
-- which is then sampled on a grid and triangulated with Marching
-- Tetrahedra (each grid cube split into 6 tetrahedra, each tetrahedron
-- has only 16 sign configurations) to produce a watertight surface
-- that correctly reflects CSG union/difference/intersection.
module Exporter (exportToOBJ) where

import Data.Array.Unboxed (UArray, listArray, (!), bounds, rangeSize)
import Data.List (foldl')

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
            _  -> finish hits

    finish (d:_) = d
    finish []    = 999999.0

-- ─── MARCHING TETRAHEDRA ───────────────────────────────────────────────────────

-- | A sampled grid corner: its world position and the SDF value there.
data SVtx = SVtx { svPos :: !Vec3, svVal :: !Float }

-- | Sample the SDF over a cube grid spanning [-radius, radius]^3 with
--   `res` cells per axis, and triangulate the zero-isosurface.
--   Returns a flat triangle soup (no vertex welding - simplest correct
--   approach for an export tool).
marchingCubes :: (Vec3 -> Float) -> Float -> Int -> [(Vec3,Vec3,Vec3)]
marchingCubes sdf radius res =
    [ tri
    | i <- [0 .. res-1], j <- [0 .. res-1], k <- [0 .. res-1]
    , let corners = [ SVtx (coord (i+dx), coord (j+dy), coord (k+dz)) (gridVal (i+dx) (j+dy) (k+dz))
                    | (dx,dy,dz) <- cornerOffsets ]
    , tet <- cubeTets corners
    , tri <- tetTriangles tet
    ]
  where
    n = res + 1
    step = (2*radius) / fromIntegral res
    coord idx = -radius + fromIntegral idx * step

    grid :: UArray Int Float
    grid = listArray (0, n*n*n - 1)
        [ sdf (coord i, coord j, coord k) | i <- [0..res], j <- [0..res], k <- [0..res] ]

    gridVal i j k = grid ! ((i*n + j)*n + k)

    -- Corner order matches the cube diagram used for the tet decomposition below:
    --   0:(0,0,0) 1:(1,0,0) 2:(1,0,1) 3:(0,0,1) 4:(0,1,0) 5:(1,1,0) 6:(1,1,1) 7:(0,1,1)
    cornerOffsets :: [(Int,Int,Int)]
    cornerOffsets = [(0,0,0),(1,0,0),(1,0,1),(0,0,1),(0,1,0),(1,1,0),(1,1,1),(0,1,1)]

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
--   points towards the "outside" of the surface.
tetTriangles :: (SVtx,SVtx,SVtx,SVtx) -> [(Vec3,Vec3,Vec3)]
tetTriangles (v0,v1,v2,v3) =
    case (ins, outs) of
        ([a], [b,c,d]) ->
            let outward = vsub (centroid [svPos b, svPos c, svPos d]) (svPos a)
            in [orient (edgeXing a b, edgeXing a c, edgeXing a d) outward]
        ([a,b], [c,d]) ->
            let outward = vsub (centroid [svPos c, svPos d]) (centroid [svPos a, svPos b])
                pac = edgeXing a c; pad = edgeXing a d
                pbc = edgeXing b c; pbd = edgeXing b d
            in [ orient (pac, pad, pbd) outward
               , orient (pac, pbd, pbc) outward
               ]
        ([a,b,c], [d]) ->
            let outward = vsub (svPos d) (centroid [svPos a, svPos b, svPos c])
            in [orient (edgeXing a d, edgeXing b d, edgeXing c d) outward]
        _ -> [] -- all 4 corners on the same side: no surface in this tet
  where
    vs = [v0,v1,v2,v3]
    ins  = filter (\v -> svVal v < 0) vs
    outs = filter (\v -> svVal v >= 0) vs

-- | The zero-crossing point on the edge between two grid corners.
edgeXing :: SVtx -> SVtx -> Vec3
edgeXing p q =
    let vp = svVal p; vq = svVal q
    in lerp (svPos p) (svPos q) (vp / (vp - vq))

-- | Flip a triangle's winding (if needed) so its normal points towards `outward`.
orient :: (Vec3,Vec3,Vec3) -> Vec3 -> (Vec3,Vec3,Vec3)
orient (p0,p1,p2) outward =
    let n = cross (vsub p1 p0) (vsub p2 p0)
    in if dot n outward >= 0 then (p0,p1,p2) else (p0,p2,p1)

-- ─── OBJ OUTPUT ────────────────────────────────────────────────────────────────

-- | Format a triangle soup as Wavefront OBJ text (no vertex deduplication).
meshToOBJString :: [(Vec3,Vec3,Vec3)] -> String
meshToOBJString tris = unlines (vLines ++ fLines)
  where
    verts = concatMap (\(a,b,c) -> [a,b,c]) tris
    vLines = [ "v " ++ show x ++ " " ++ show y ++ " " ++ show z | (x,y,z) <- verts ]
    fLines = [ "f " ++ show (i*3+1) ++ " " ++ show (i*3+2) ++ " " ++ show (i*3+3)
             | i <- [0 .. length tris - 1] ]

-- | The radius of the cube (centred on the origin) that is sampled for
--   export. Matches the raymarcher's `scene_radius` in core.glsl.
exportRadius :: Float
exportRadius = 25.0

-- | Grid cells per axis. Higher = more detail, slower export.
exportResolution :: Int
exportResolution = 64

-- | Compile the scene's bytecode into a mesh and write it to `path` as a
--   Wavefront .obj file.
exportToOBJ :: [Float] -> FilePath -> IO ()
exportToOBJ bytecode path = writeFile path (meshToOBJString tris)
  where
    arr = listArray (0, length bytecode - 1) bytecode :: UArray Int Float
    tris = marchingCubes (evalSDF arr) exportRadius exportResolution
