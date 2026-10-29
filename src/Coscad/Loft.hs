-- | Lofts, tessellated in Haskell exactly as the emitted OpenSCAD asks BOSL2's
-- @skin()@ to build them: the same profile vertex order (BOSL2's path
-- functions), the same @reindex@ / @distance@ vertex correspondence, the same
-- @min_edge@ quad split, the same degenerate-face cull and the same
-- orientation rule (flip everything when the signed volume is negative).
-- Anything BOSL2 would reject, or that this port does not cover, is an error.
module Coscad.Loft
  ( Path2
  , profilePath
  , loftTriangles
  , reindexPolygon
  , distanceMatch
  , triangulatePolygon
  ) where

import Control.Monad (unless, when)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as U
import Data.List (foldl')
import Coscad.Geometry (resolve)
import Coscad.Linalg (V3, cross, dot3, vlen, vsub)
import Coscad.Mesh (Tri)
import Coscad.Shape
import Coscad.Validate (finite)

type Path2 = [(Double, Double)]

-- | A loft profile as the list of 2D points BOSL2's path expression for it
-- evaluates to (see 'Coscad.Codegen.pathOf', which emits that expression).
-- Circles are BOSL2's @circle()@ *function*: clockwise from angle 0.
-- Polygons are @ccw_polygon@ of the given points. In-plane transforms map
-- the points; nothing is reordered, as in BOSL2.
profilePath :: Shape -> Either String Path2
profilePath s = case s of
  Shape2D n r
    | n < 3 -> Left "a loft profile needs at least three sides"
    | otherwise -> Right [(r * cosD a, r * sinD a) | i <- [0 .. n - 1], let a = 360 - fromIntegral i * 360 / fromIntegral n]
  Poly (PD pts [])
    | length pts < 3 -> Left "a loft profile polygon needs at least three points"
    | otherwise -> Right (ccwPolygon pts)
  Poly _ -> Left "a polygon with holes cannot be a loft profile"
  Tx d p -> map (\(x, y) -> (x + d, y)) <$> profilePath p
  Ty d p -> map (\(x, y) -> (x, y + d)) <$> profilePath p
  Translate (_, _, z) _ | z /= 0 -> Left "a loft profile cannot be moved in Z; give the loft its z value instead"
  Translate (dx, dy, _) p -> map (\(x, y) -> (x + dx, y + dy)) <$> profilePath p
  Rz a p -> map (rotD a) <$> profilePath p
  RotAxis a (0, 0, az) p | az /= 0 -> map (rotD (if az > 0 then a else -a)) <$> profilePath p
  Scale (sx, sy, _) p -> map (\(x, y) -> (x * sx, y * sy)) <$> profilePath p
  Mirror (0, 0, nz) p | nz /= 0 -> profilePath p
  Mirror (_, _, nz) _ | nz /= 0 -> Left "a loft profile must stay in the XY plane (mirror normal must be in XY or along Z)"
  Mirror (0, 0, 0) _ -> Left "a loft profile mirror needs a nonzero normal"
  Mirror (nx, ny, _) p -> map (mirror2 nx ny) <$> profilePath p
  Offset _ _ -> Left "native: an offset inside a loft profile is not supported yet (BOSL2's round path offset); offset the solid or the profile outside the loft instead"
  Tz _ _ -> Left "a loft profile cannot be moved in Z (ζ); give the loft its z value instead"
  Rx _ _ -> Left "a loft profile must stay in the XY plane (no θ rotation)"
  Ry _ _ -> Left "a loft profile must stay in the XY plane (no ϕ rotation)"
  RotAxis {} -> Left "a loft profile must stay in the XY plane (rotate only about Z)"
  Tag _ p -> profilePath p
  Datum _ _ p -> profilePath p
  Frame _ _ _ p -> profilePath p
  Anchor {} -> profilePath (resolve s)
  Position {} -> profilePath (resolve s)
  AttachTo {} -> profilePath (resolve s)
  CutAt {} -> profilePath (resolve s)
  Union _ -> boolErr
  Diff _ _ -> boolErr
  Intersection _ -> boolErr
  Hull _ -> boolErr
  Minkowski _ -> boolErr
  Empty -> Left "an empty shape cannot be a loft profile"
  _ -> Left "a loft profile must be a 2D outline (△ ⬠ ⭘ ✎, Circle/Triangle/Pentagon/Bezier), not a 3D solid"
  where
    boolErr = Left "a loft profile must be a single closed outline: booleans (⊕ ⊖ ∩ ⇓ ⊞) between profiles are not supported"
    rotD a (x, y) = let c = cosD a; sn = sinD a in (x * c - y * sn, x * sn + y * c)
    mirror2 nx ny (x, y) =
      let l = sqrt (nx * nx + ny * ny)
          ux = nx / l; uy = ny / l
          d = 2 * (x * ux + y * uy)
       in (x - d * ux, y - d * uy)

-- | Degree trigonometry with OpenSCAD's exact quarter-turn values, so a
-- vertex at 90 degrees lands on the axis exactly, as it does in OpenSCAD.
sinD, cosD :: Double -> Double
sinD a = case reduce a of
  0 -> 0
  90 -> 1
  180 -> 0
  270 -> -1
  r -> sin (r * pi / 180)
cosD a = case reduce a of
  0 -> 1
  90 -> 0
  180 -> -1
  270 -> 0
  r -> cos (r * pi / 180)

reduce :: Double -> Double
reduce a = let r = a - 360 * fromIntegral (floor (a / 360) :: Integer) in if r < 0 then r + 360 else r

-- | BOSL2 @ccw_polygon@: reverse a clockwise polygon keeping its first point.
ccwPolygon :: Path2 -> Path2
ccwPolygon poly = case poly of
  p : rest | clockwise poly -> p : reverse rest
  _ -> poly

-- | BOSL2 @is_polygon_clockwise@: the turn at the lowest-y of the lowest-x
-- vertices decides.
clockwise :: Path2 -> Bool
clockwise poly =
  let n = length poly
      arr = A.listArray (0, n - 1) poly
      at i = arr A.! (((i `mod` n) + n) `mod` n)
      minx = minimum (map fst poly)
      lowind = [i | (i, (x, _)) <- zip [0 ..] poly, x == minx]
      miny = minimum [snd (at i) | i <- lowind]
      extreme = case [i | i <- lowind, snd (at i) == miny] of
        i : _ -> i
        [] -> 0
      (ax, ay) = at (extreme + 1); (bx, by) = at (extreme - 1); (ex, ey) = at extreme
   in (ax - ex) * (by - ey) - (ay - ey) * (bx - ex) < 0

-- | BOSL2 @reindex_polygon@ for 3D paths of equal length: rotate the second
-- so the sum of corresponding distances to the reference is smallest (the
-- first smallest shift wins ties, like @min_index@).
reindexPolygon :: [V3] -> [V3] -> [V3]
reindexPolygon ref poly =
  let n = length poly
      cost k = sum (zipWith (\r p -> vlen (vsub r p)) ref (rotateLeft k poly))
      best = fst (foldl' (\acc@(_, bc) (k, c) -> if c < bc then (k, c) else acc) (0, 1 / 0) [(k, cost k) | k <- [0 .. n - 1]])
   in rotateLeft best poly

rotateLeft :: Int -> [a] -> [a]
rotateLeft k xs = let (a, b) = splitAt k xs in b ++ a

-- | BOSL2 @_skin_distance_match@: the vertex correspondence that minimises the
-- total length of the edges joining two profiles, allowing vertices to be
-- used more than once (dynamic programme over every cyclic shift of the
-- larger profile; the first best shift wins ties). Returns the two profiles
-- with vertices repeated so they have the same length and corresponding
-- positions.
distanceMatch :: [V3] -> [V3] -> Either String ([V3], [V3])
distanceMatch poly1 poly2 = do
  let swap = length poly1 > length poly2
      big = if swap then poly1 else poly2
      small = if swap then poly2 else poly1
      s = length small
      b = length big
  when (s < 3 || b < 3) (Left "native: loft profiles need at least three vertices")
  when (fromIntegral s * fromIntegral b * fromIntegral b > (2.0e8 :: Double)) $
    Left ("native: loft profiles with " ++ show s ++ " and " ++ show b ++ " vertices are too large for the distance method; give them equal vertex counts")
  let smallA = U.listArray (0, 3 * s - 1) (concatMap coords small) :: U.UArray Int Double
      smallArr = A.listArray (0, s - 1) small
      attempt k =
        let shifted = rotateLeft k big
            bigA = U.listArray (0, 3 * b - 1) (concatMap coords shifted) :: U.UArray Int Double
            (cost, path) = dp smallA s bigA b
         in (cost, shifted, path)
      (_, bestBig, bestPath) =
        foldl' (\acc@(bc, _, _) k -> let r@(c, _, _) = attempt k in if c < bc then r else acc)
          (1 / 0, big, []) [0 .. b - 1]
      bigArr = A.listArray (0, b - 1) bestBig
  when (null bestPath) (Left "native: loft distance matching found no finite correspondence")
  let newSmall = [smallArr A.! i | (i, _) <- bestPath]
      newBig = [bigArr A.! j | (_, j) <- bestPath]
  return (if swap then (newBig, newSmall) else (newSmall, newBig))
  where
    coords (x, y, z) = [x, y, z]

-- | The dynamic programme of @_dp_distance_array@ plus @_dp_extract_map@:
-- rows follow the small profile (index mod s), columns the big one (mod b),
-- both one past their length so the correspondence closes. Costs prefer
-- diagonal, then left, then up on ties, as BOSL2's @min_index@ does. The
-- path runs from (0,0) up to, but excluding, the closing corner (s,b).
dp :: U.UArray Int Double -> Int -> U.UArray Int Double -> Int -> (Double, [(Int, Int)])
dp smallA s bigA b =
  let dist i j =
        let si = 3 * (i `mod` s); bj = 3 * (j `mod` b)
            dx = bigA U.! bj - smallA U.! si
            dy = bigA U.! (bj + 1) - smallA U.! (si + 1)
            dz = bigA U.! (bj + 2) - smallA U.! (si + 2)
         in sqrt (dx * dx + dy * dy + dz * dz)
      row0 = U.listArray (0, b) (scanl (+) 0 [dist 0 j | j <- [1 .. b]]) :: U.UArray Int Double
      map0 = U.listArray (0, b) (replicate (b + 1) left) :: U.UArray Int Int
      step :: (U.UArray Int Double, [U.UArray Int Int]) -> Int -> (U.UArray Int Double, [U.UArray Int Int])
      step (prev, maps) i =
        let first = dist i 0 + prev U.! 0
            go (acc, ms, lft) j =
              let diag = prev U.! (j - 1); up = prev U.! j
                  (m, c) = pick diag lft up
                  value = c + dist i j
               in (value : acc, m : ms, value)
            (rowRev, mapRev, _) = foldl' go ([first], [up0], first) [1 .. b]
            rowA = U.listArray (0, b) (reverse rowRev) :: U.UArray Int Double
            mapA = U.listArray (0, b) (reverse mapRev) :: U.UArray Int Int
         in (rowA, mapA : maps)
      (lastRow, mapsRev) = foldl' step (row0, [map0]) [1 .. s]
      mapsA = A.listArray (0, s) (reverse mapsRev) :: A.Array Int (U.UArray Int Int)
      total = lastRow U.! b
      extract i j acc
        | i == 0 && j == 0 = acc
        | i < 0 || j < 0 = []
        | otherwise =
            let m = (mapsA A.! i) U.! j
                i' = if m == up0 || m == diag0 then i - 1 else i
                j' = if m == left || m == diag0 then j - 1 else j
             in extract i' j' ((i' `mod` s, j' `mod` b) : acc)
   in if finite total then (total, extract s b []) else (1 / 0, [])
  where
    diag0 = 0 :: Int
    left = 1 :: Int
    up0 = 2 :: Int
    pick d l u
      | d <= l && d <= u = (diag0, d)
      | l <= u = (left, l)
      | otherwise = (up0, u)

-- | Triangles of a loft over its profiles (z, outline), each already in BOSL2
-- order, using @method@ as 'Coscad.Codegen.loftMethod' chose it (@reindex@
-- for equal vertex counts, @distance@ otherwise), with @slices = 0@, caps,
-- and the @min_edge@ style. The result is a closed, consistently oriented
-- triangle soup whose signed volume is positive.
loftTriangles :: String -> [(Double, Path2)] -> Either String [Tri]
loftTriangles method layers = do
  when (length layers < 2) (Left "a loft needs at least two profiles")
  unless (all (\(z, p) -> finite z && all (\(x, y) -> finite x && finite y) p) layers) (Left "loft profiles must be finite")
  let profiles = [[(x, y, z) | (x, y) <- p] | (z, p) <- layers]
  blocks <- case (method, profiles) of
    ("reindex", p0 : rest) -> do
      unless (all ((== length p0) . length) rest) (Left "native: loft method reindex needs equal vertex counts")
      return [scanl reindexPolygon p0 rest]
    ("distance", _) -> mapM (\(a, c) -> (\(x, y) -> [x, y]) <$> distanceMatch a c) (zip profiles (drop 1 profiles))
    (other, _) -> Left ("native: unsupported loft method " ++ show other)
  let nBlocks = length blocks
  tris <- concat <$> mapM (\(k, block) -> vertexArray (k == 0) (k == nBlocks - 1) block) (zip [0 :: Int ..] blocks)
  when (null tris) (Left "native: loft produced no triangles")
  let volume = sum [dot3 a (cross bb c) | (a, bb, c) <- tris] / 6
  return (if volume < 0 then map (\(a, bb, c) -> (a, c, bb)) tris else tris)

-- | BOSL2 @vnf_vertex_array(rows, cap1, cap2, col_wrap=true, style="min_edge")@
-- as triangles; cap polygons are ear-clipped in their plane.
vertexArray :: Bool -> Bool -> [[V3]] -> Either String [Tri]
vertexArray cap1 cap2 rows = case rows of
  [] -> Left "native: loft block has no rows"
  firstRow : _ -> do
    let cols = length firstRow
        nRows = length rows
    unless (all ((== cols) . length) rows) (Left "native: loft rows have different lengths")
    when (cols < 3) (Left "native: loft rows need at least three vertices")
    let grid = A.listArray ((0, 0), (nRows - 1, cols - 1)) (concat rows) :: A.Array (Int, Int) V3
        eps = 1e-9
        quad r c =
          let c' = (c + 1) `mod` cols
              p1 = grid A.! (r, c); p2 = grid A.! (r + 1, c); p3 = grid A.! (r + 1, c'); p4 = grid A.! (r, c')
              d42 = vlen (vsub p4 p2); d13 = vlen (vsub p1 p3)
           in if d42 < d13 + eps then [(p1, p4, p2), (p2, p4, p3)] else [(p1, p3, p2), (p1, p4, p3)]
        sides = filter (\(a, bb, c) -> vlen (cross (vsub bb a) (vsub c a)) > eps)
          (concat [quad r c | r <- [0 .. nRows - 2], c <- [0 .. cols - 1]])
        lastRow = [grid A.! (nRows - 1, c) | c <- [0 .. cols - 1]]
    bottom <- if cap1 then capTriangles (reverse firstRow) else return []
    top <- if cap2 then capTriangles lastRow else return []
    return (bottom ++ top ++ sides)

-- | A horizontal cap polygon, oriented as listed (BOSL2 lists the bottom cap
-- reversed and the top cap forward), as triangles following that order.
capTriangles :: [V3] -> Either String [Tri]
capTriangles pts = do
  let flat = [(x, y) | (x, y, _) <- pts]
      z = case pts of ((_, _, z0) : _) -> z0; [] -> 0
  tris <- triangulatePolygon flat
  return [((ax, ay, z), (bx, by, z), (cx, cy, z)) | ((ax, ay), (bx, by), (cx, cy)) <- tris]

-- | Ear clipping of a simple polygon (either orientation; consecutive repeated
-- points are merged). The triangles follow the polygon's listed orientation.
-- A polygon with no ear (self-intersecting or numerically unresolved) is an
-- error rather than a guessed fan.
triangulatePolygon :: Path2 -> Either String [((Double, Double), (Double, Double), (Double, Double))]
triangulatePolygon raw = do
  let pts = dedupe raw
      n = length pts
  when (n < 3) (Left "native: a loft cap collapsed to fewer than three distinct points")
  let area2 = sum [x0 * y1 - x1 * y0 | ((x0, y0), (x1, y1)) <- zip pts (drop 1 pts ++ take 1 pts)]
  when (area2 == 0) (Left "native: a loft cap has zero area")
  let ccw = if area2 > 0 then pts else reverse pts
  tris <- clip ccw []
  return (if area2 > 0 then tris else map (\(a, b, c) -> (a, c, b)) tris)
  where
    dedupe xs =
      let merged = foldr (\p acc -> case acc of (q : _) | q == p -> acc; _ -> p : acc) [] xs
       in case merged of
            y : _ : _ | last merged == y -> init merged
            _ -> merged
    turn (ax, ay) (bx, by) (cx, cy) = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
    inside a b c p = turn a b p >= 0 && turn b c p >= 0 && turn c a p >= 0
    clip poly acc = case poly of
      [a, b, c] -> Right (reverse (if turn a b c > 0 then (a, b, c) : acc else acc))
      _ ->
        let n = length poly
            arr = A.listArray (0, n - 1) poly
            at i = arr A.! (i `mod` n)
            corner i = (at (i + n - 1), at i, at (i + 1))
            degenerate = [i | i <- [0 .. n - 1], let (a, b, c) = corner i, turn a b c == 0]
            ears = [i | i <- [0 .. n - 1], let (a, b, c) = corner i, turn a b c > 0
                      , not (any (\p -> p /= a && p /= b && p /= c && inside a b c p) poly)]
         in case (degenerate, ears) of
              (i : _, _) -> clip (dropAt i poly) acc
              (_, i : _) -> let (a, b, c) = corner i in clip (dropAt i poly) ((a, b, c) : acc)
              _ -> Left "native: a loft cap could not be triangulated (self-intersecting or overlapping profile?)"
    dropAt i xs = take i xs ++ drop (i + 1) xs
