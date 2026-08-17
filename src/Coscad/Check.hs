-- | `coscad check foo.assemble` — assembly interference + clearance
-- checking on real meshes.
--
-- Method:
--   1. For each top-level part, re-resolve the asm expression with every
--      OTHER part bound to Empty: the render is that part exactly as
--      placed by the assembly (all instances, e.g. a mirrored pair).
--   2. Split each render into connected bodies (instances).
--   3. Pair up bodies across (and within) parts; prune by clearance-
--      expanded AABB overlap.
--   4. For surviving pairs, both bodies are emitted as OpenSCAD
--      polyhedron() literals and intersected — OpenSCAD is the exact
--      boolean engine. Nonzero volume = OVERLAP (reported with volume).
--   5. Non-overlapping close pairs are classified by vertex-to-triangle
--      distance (grid-accelerated): CONTACT (< 0.02mm) is informational
--      (mating faces), a gap below the declared `clearance` is a WARNING.
--
-- Exit code 1 if any OVERLAP is found.
module Coscad.Check (module Coscad.Check) where

import Coscad.Assemble
import Coscad.Codegen
import Coscad.Next (parseStlAscii, runOpenscad, Tri)
import Coscad.Parser
import Coscad.Shape
import Control.Monad (forM, forM_, unless)
import Data.IORef
import Data.List (foldl', intercalate)
import qualified Data.Map as Map
import System.Exit (exitFailure)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

type V3 = (Double, Double, Double)

contactEps :: Double
contactEps = 0.02

volEps :: Double
volEps = 0.01 -- mm^3; below this an intersection is tessellation noise

-- ------------------------------------------------------------------
-- small vector kit (local to keep the module self-contained)
vsub3, vadd3, vcross3 :: V3 -> V3 -> V3
vsub3 (a, b, c) (x, y, z) = (a - x, b - y, c - z)
vadd3 (a, b, c) (x, y, z) = (a + x, b + y, c + z)
vcross3 (a, b, c) (x, y, z) = (b * z - c * y, c * x - a * z, a * y - b * x)

vdot3 :: V3 -> V3 -> Double
vdot3 (a, b, c) (x, y, z) = a * x + b * y + c * z

vscale3 :: Double -> V3 -> V3
vscale3 k (a, b, c) = (k * a, k * b, k * c)

vlen3 :: V3 -> Double
vlen3 v = sqrt (vdot3 v v)

-- ------------------------------------------------------------------
-- connected-body split (union-find over exact shared vertices)
splitBodies :: [Tri] -> [[Tri]]
splitBodies tris = Map.elems grouped
  where
    allVs = concat [[a, b, c] | (a, b, c) <- tris]
    vids = Map.fromList (zip (Map.keys (Map.fromList [(v, ()) | v <- allVs])) [(0 :: Int) ..])
    vid v = vids Map.! v
    n = Map.size vids
    parent0 = Map.fromList [(i, i) | i <- [0 .. n - 1]]
    find p i = if p Map.! i == i then i else find p (p Map.! i)
    unite p i j =
      let ri = find p i; rj = find p j
       in if ri == rj then p else Map.insert ri rj p
    pFinal =
      foldl'
        (\p (a, b, c) -> unite (unite p (vid a) (vid b)) (vid b) (vid c))
        parent0
        tris
    grouped =
      foldl'
        (\m t@(a, _, _) -> Map.insertWith (++) (find pFinal (vid a)) [t] m)
        Map.empty
        tris

triBounds :: [Tri] -> (V3, V3)
triBounds ts =
  let vs = concat [[a, b, c] | (a, b, c) <- ts]
      xs = map (\(x, _, _) -> x) vs
      ys = map (\(_, y, _) -> y) vs
      zs = map (\(_, _, z) -> z) vs
   in ((minimum xs, minimum ys, minimum zs), (maximum xs, maximum ys, maximum zs))

aabbGap :: (V3, V3) -> (V3, V3) -> Double
aabbGap ((ax0, ay0, az0), (ax1, ay1, az1)) ((bx0, by0, bz0), (bx1, by1, bz1)) =
  let g lo0 hi0 lo1 hi1 = max 0 (max (lo1 - hi0) (lo0 - hi1))
      gx = g ax0 ax1 bx0 bx1
      gy = g ay0 ay1 by0 by1
      gz = g az0 az1 bz0 bz1
   in sqrt (gx * gx + gy * gy + gz * gz)

-- ------------------------------------------------------------------
-- point-triangle distance (Ericson, Real-Time Collision Detection)
pointTriDist :: V3 -> Tri -> Double
pointTriDist p (a, b, c) =
  let ab = vsub3 b a
      ac = vsub3 c a
      ap = vsub3 p a
      d1 = vdot3 ab ap
      d2 = vdot3 ac ap
   in if d1 <= 0 && d2 <= 0
        then vlen3 (vsub3 p a)
        else
          let bp = vsub3 p b
              d3 = vdot3 ab bp
              d4 = vdot3 ac bp
           in if d3 >= 0 && d4 <= d3
                then vlen3 (vsub3 p b)
                else
                  let vc = d1 * d4 - d3 * d2
                   in if vc <= 0 && d1 >= 0 && d3 <= 0
                        then vlen3 (vsub3 p (vadd3 a (vscale3 (d1 / (d1 - d3)) ab)))
                        else
                          let cp = vsub3 p c
                              d5 = vdot3 ab cp
                              d6 = vdot3 ac cp
                           in if d6 >= 0 && d5 <= d6
                                then vlen3 (vsub3 p c)
                                else
                                  let vb = d5 * d2 - d1 * d6
                                   in if vb <= 0 && d2 >= 0 && d6 <= 0
                                        then vlen3 (vsub3 p (vadd3 a (vscale3 (d2 / (d2 - d6)) ac)))
                                        else
                                          let va = d3 * d6 - d5 * d4
                                           in if va <= 0 && (d4 - d3) >= 0 && (d5 - d6) >= 0
                                                then
                                                  let w = (d4 - d3) / ((d4 - d3) + (d5 - d6))
                                                   in vlen3 (vsub3 p (vadd3 b (vscale3 w (vsub3 c b))))
                                                else
                                                  let denom = 1 / (va + vb + vc)
                                                      v = vb * denom
                                                      w = vc * denom
                                                   in vlen3 (vsub3 p (vadd3 a (vadd3 (vscale3 v ab) (vscale3 w ac))))

-- | Minimum vertex<->triangle distance between two bodies, early-exit
-- once below `stop`. Grid-accelerated on the larger triangle set.
-- (Vertex sampling: exact for meshes whose closest features involve a
-- vertex; parallel offset faces are caught since their vertices project
-- onto the opposing face.)
meshDist :: Double -> [Tri] -> [Tri] -> Double
meshDist stop ta tb = min (side ta tb) (side tb ta)
  where
    cell = max (stop * 2) 8
    key (x, y, z) = (floor (x / cell) :: Int, floor (y / cell) :: Int, floor (z / cell) :: Int)
    gridOf ts =
      foldl'
        ( \m t ->
            let ((x0, y0, z0), (x1, y1, z1)) = triBounds [t]
                (i0, j0, k0) = key (x0, y0, z0)
                (i1, j1, k1) = key (x1, y1, z1)
             in foldl' (\m' k -> Map.insertWith (++) k [t] m') m [(i, j, kk) | i <- [i0 .. i1], j <- [j0 .. j1], kk <- [k0 .. k1]]
        )
        Map.empty
        ts
    side va vb =
      let grid = gridOf vb
          verts = concat [[a, b, c] | (a, b, c) <- va]
          go best [] = best
          go best (p : ps)
            | best < contactEps = best
            | otherwise =
                let (i, j, k) = key p
                    near = concat [Map.findWithDefault [] (i + di, j + dj, k + dk) grid | di <- [-1 .. 1], dj <- [-1 .. 1], dk <- [-1 .. 1]]
                    d = foldl' (\acc t -> min acc (pointTriDist p t)) best near
                 in go d ps
       in go (1 / 0) verts

-- ------------------------------------------------------------------
-- emit a body as an OpenSCAD polyhedron (STL winding is CCW-outward;
-- polyhedron wants CW from outside, so faces are reversed)
polyScad :: [Tri] -> String
polyScad ts =
  let vkeys = Map.keys (Map.fromList [(v, ()) | (a, b, c) <- ts, v <- [a, b, c]])
      idx = Map.fromList (zip vkeys [(0 :: Int) ..])
      pts = vkeys
      shw (x, y, z) = "[" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "]"
      face (a, b, c) = "[" ++ show (idx Map.! c) ++ "," ++ show (idx Map.! b) ++ "," ++ show (idx Map.! a) ++ "]"
   in "polyhedron(points=["
        ++ intercalate "," (map shw pts)
        ++ "], faces=["
        ++ intercalate "," (map face ts)
        ++ "]);"

meshVolume :: [Tri] -> Double
meshVolume = abs . sum . map (\(a, b, c) -> vdot3 a (vcross3 b c) / 6)

-- far-away marker: keeps OpenSCAD from failing on empty exports
marker :: String
marker = "translate([99999,99999,99999]) cube(0.001);"

-- | Like Next.runOpenscad but also reports CGAL trouble on stderr:
-- OpenSCAD exits 0 after a CGAL assertion and silently emits a
-- fallback (often the first operand), so the exit code alone lies.
runOpenscadChecked :: FilePath -> FilePath -> IO (Either String Bool)
runOpenscadChecked scadF stlF = do
  bin <- maybe "openscad" id <$> lookupEnv "COSCAD_OPENSCAD"
  (code, out, err) <- readProcessWithExitCode bin ["-o", stlF, scadF] ""
  let cgalBad = any (\l -> contains "CGAL error" l || contains "assertion" l) (lines err ++ lines out)
  return $ case code of
    ExitSuccess -> Right cgalBad
    ExitFailure n -> Left ("openscad exit " ++ show n ++ ": " ++ take 300 err)
  where
    contains pat l = pat `isInfixOfS` l
    isInfixOfS pat l = any (\i -> take (length pat) (drop i l) == pat) [0 .. length l - length pat]

-- ------------------------------------------------------------------
processCheck :: FilePath -> IO ()
processCheck path = do
  res <- loadAssembleFile [] path
  case res of
    Left err -> hPutStrLn stderr ("Error: " ++ err) >> exitFailure
    Right ar -> case arAsm ar of
      Nothing -> hPutStrLn stderr "Error: check needs an `asm = ...` definition." >> exitFailure
      Just _ -> do
        let base = dropExtension path
            clr = arClearance ar
            names = map fst (arParts ar)
        putStrLn ("check: " ++ show (length names) ++ " parts, clearance = " ++ show clr ++ "mm")
        -- 1. isolate each part inside the asm expression
        bodiesPer <- forM (arParts ar) $ \(pn, ps) -> do
          let table0 = Map.fromList [(n, if n == pn then s else Empty) | (n, s) <- arParts ar]
          case resolveVariables (arMode ar) (arDefs ar) table0 of
            Left err -> hPutStrLn stderr ("Error isolating " ++ pn ++ ": " ++ err) >> exitFailure >> return (pn, [])
            Right table -> case Map.lookup "asm" table of
              Nothing -> return (pn, [])
              Just asmS -> do
                let scadF = base ++ "_chk_" ++ pn ++ ".scad"
                    stlF = base ++ "_chk_" ++ pn ++ ".stl"
                writeScad asmS scadF
                appendFile scadF ("\n" ++ marker ++ "\n")
                r <- runOpenscad scadF stlF
                case r of
                  Left err -> hPutStrLn stderr ("Error rendering " ++ pn ++ ": " ++ err) >> exitFailure >> return (pn, [])
                  Right () -> do
                    tris <- parseStlAscii <$> readFile stlF
                    let real = [b | b <- splitBodies tris, farFromMarker b]
                    return (pn, real)
        let insts =
              [ (pn ++ (if length bs > 1 then "#" ++ show i else ""), b)
              | (pn, bs) <- bodiesPer
              , (i, b) <- zip [(1 :: Int) ..] bs
              ]
        putStrLn ("       " ++ show (length insts) ++ " placed instances: " ++ intercalate ", " (map fst insts))
        -- 2. pairwise checks
        overlapsR <- newIORef (0 :: Int)
        warnsR <- newIORef (0 :: Int)
        let pairs = [(x, y) | (x : rest) <- tails' insts, y <- rest]
        forM_ (zip [(0 :: Int) ..] pairs) $ \(i, ((na, ba), (nb, bb))) -> do
          let gap0 = aabbGap (triBounds ba) (triBounds bb)
          if gap0 > max clr contactEps
            then return ()
            else do
              -- exact overlap via OpenSCAD boolean
              let scadF = base ++ "_chk_pair" ++ show i ++ ".scad"
                  stlF = base ++ "_chk_pair" ++ show i ++ ".stl"
              let emit x y = writeFile scadF ("intersection() {\n union() { " ++ polyScad x ++ marker ++ " }\n union() { " ++ polyScad y ++ marker ++ " }\n}\n")
                  measure = do
                    tris <- parseStlAscii <$> readFile stlF
                    return (sum (map meshVolume (filter farFromMarkerNot (splitBodies tris))))
              emit ba bb
              r1 <- runOpenscadChecked scadF stlF
              volM <- case r1 of
                Right False -> Just <$> measure
                _ -> do
                  emit bb ba -- CGAL assertions are often operand-order dependent
                  r2 <- runOpenscadChecked scadF stlF
                  case r2 of
                    Right False -> Just <$> measure
                    _ -> return Nothing -- boolean unstable near coplanar faces
              let note = case volM of Nothing -> "  (boolean unstable; distance verdict)"; Just _ -> ""
              case volM of
                Just vol | vol > volEps -> do
                  modifyIORef' overlapsR (+ 1)
                  putStrLn ("OVERLAP  " ++ na ++ " x " ++ nb ++ "  volume " ++ show (rnd vol) ++ " mm^3")
                _ -> do
                  let d = meshDist (max clr contactEps) ba bb
                  if d < contactEps
                    then putStrLn ("contact  " ++ na ++ " x " ++ nb ++ note)
                    else
                      if d < clr
                        then do
                          modifyIORef' warnsR (+ 1)
                          putStrLn ("WARN     " ++ na ++ " x " ++ nb ++ "  gap " ++ show (rnd d) ++ " mm < clearance " ++ show clr ++ note)
                        else unless (gap0 > clr) $
                          putStrLn ("ok       " ++ na ++ " x " ++ nb ++ "  gap " ++ show (rnd d) ++ " mm" ++ note)
        no <- readIORef overlapsR
        nw <- readIORef warnsR
        putStrLn ("check: " ++ show no ++ " overlaps, " ++ show nw ++ " clearance warnings")
        unless (no == 0) exitFailure
  where
    rnd x = fromIntegral (round (x * 1000) :: Integer) / 1000 :: Double
    tails' [] = []
    tails' l@(_ : xs) = l : tails' xs
    farFromMarker b = let ((x, _, _), _) = triBounds b in x < 90000
    farFromMarkerNot = farFromMarker
