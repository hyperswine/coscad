-- | Mesh-level manufacturing diagnostics. These are conservative warnings,
-- not a layer-by-layer support proof or a prediction of physical adhesion.
--
-- "Bed contact" has one definition, used both here and by the orientation
-- score in "Coscad.Next": the area of downward-facing triangles (normal
-- within about 26 degrees of straight down) whose three vertices all lie
-- within 'planeTolerance' of the mesh's lowest point. "Overhang faces" are
-- likewise shared: down-facing triangles that start above the first-layer
-- band. The score sums their area; the warning below keeps only the part
-- of them that is neither bridged nor within reach of a supporting wall.
module Coscad.Printability
  ( Printability (..)
  , PrintabilityConfig (..)
  , defaultPrintabilityConfig
  , analysePrintability
  , analysePrintabilityWith
  , bedContact
  , planeTolerance
  , downFacing
  , firstLayerBand
  , overhangFaces
  , unsupportedOverhang
  , firstLayerArea
  , minFootprintWidth
  , printabilityWarnings
  , printabilityCodes
  , recommendsBrim
  ) where

import Coscad.Format (showD)
import Coscad.Mesh (Tri, meshBounds, triArea, triNormal)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (foldl', sort, sortOn)

data Printability = Printability
  { bedContactMm2 :: Double
  , elevatedStartsMm :: [Double]
  , printabilityIssues :: [(String, String)] -- ^ (code, warning): small_contact, tall_narrow, flat_overhang, elevated_start
  , overhangMm2 :: Double -- ^ unsupported flat overhang (not bridged, out of reach of a wall)
  , firstLayerMm2 :: Double -- ^ down-facing area within the first-layer band, projected
  , minWidthMm :: Double -- ^ narrowest width of the footprint's convex hull
  } deriving (Show)

-- | The warnings as printed and written to the manifest, in order.
printabilityWarnings :: Printability -> [String]
printabilityWarnings = map snd . printabilityIssues

-- | One stable machine-readable code per warning, in the same order:
-- @small_contact@ and @tall_narrow@ are what a brim addresses,
-- @flat_overhang@ and @elevated_start@ what supports address.
printabilityCodes :: Printability -> [String]
printabilityCodes = map fst . printabilityIssues

-- | The part warns about its first layer or its height/width ratio, the
-- warnings whose advice is a brim (@plate … brim=N@ reserves one for it).
recommendsBrim :: Printability -> Bool
recommendsBrim p = any (`elem` ["small_contact", "tall_narrow"]) (printabilityCodes p)

-- | Warning thresholds, kept together so they can be tuned in one place.
data PrintabilityConfig = PrintabilityConfig
  { pcMinContact :: Double -- ^ warn below max this (pcContactPerHeight2 * h²) mm² of first layer
  , pcContactPerHeight2 :: Double
  , pcSlenderBase :: Double -- ^ height/width limit is base + pcSlenderShort / h
  , pcSlenderShort :: Double
  , pcBridgeMax :: Double -- ^ longest span bridged between two supported edges, mm
  , pcReach :: Double -- ^ how far a flat overhang may stick out from a supporting wall, mm
  , pcOverhangMin :: Double -- ^ warn above max this (pcOverhangFraction * surface) mm²
  , pcOverhangFraction :: Double
  } deriving (Show)

defaultPrintabilityConfig :: PrintabilityConfig
defaultPrintabilityConfig = PrintabilityConfig
  { pcMinContact = 25
  , pcContactPerHeight2 = 0.05
  , pcSlenderBase = 2.5
  , pcSlenderShort = 10
  , pcBridgeMax = 30
  , pcReach = 2
  , pcOverhangMin = 25
  , pcOverhangFraction = 0.02
  }

-- | How far a vertex may sit from the bed plane and still be on it:
-- 0.01 mm, a small fraction of any layer height (0.08 mm and up), growing
-- as 1e-5 of the largest dimension for meshes over a metre. Rendered STLs
-- are quantised (ASCII STL keeps about seven significant digits, and a
-- declared diagonal ▽ rotates every vertex), so an exact plane test loses
-- real contact on a rotated face.
planeTolerance :: [Tri] -> Double
planeTolerance [] = 0.01
planeTolerance tris =
  let ((x0, y0, z0), (x1, y1, z1)) = meshBounds tris
   in max 0.01 (1e-5 * maximum [x1 - x0, y1 - y0, z1 - z0])

-- | Downward-facing for contact and overhang purposes.
downFacing :: Tri -> Bool
downFacing t = let (_, _, nz) = triNormal t in nz < -0.9

-- | A down face that starts within this band above the bed is squashed
-- onto it by the first layer rather than needing support, so it is not an
-- overhang. It is deliberately not the contact tolerance: a face 0.2 mm up
-- does not hold the part down.
firstLayerBand :: Double
firstLayerBand = 0.3

lowest :: [Tri] -> Double
lowest tris = let ((_, _, z0), _) = meshBounds tris in z0

zs :: Tri -> [Double]
zs ((_, _, a), (_, _, b), (_, _, c)) = [a, b, c]

-- | Flat area on the bed plane, in mm², with the given plane tolerance.
bedContact :: Double -> [Tri] -> Double
bedContact _ [] = 0
bedContact tol tris =
  let z0 = lowest tris
   in sum [triArea t | t <- tris, downFacing t, all (\h -> h - z0 <= tol) (zs t)]

-- | Down faces that start above the first-layer band: the overhang term of
-- the orientation score, and the candidates for the overhang warning.
overhangFaces :: [Tri] -> [Tri]
overhangFaces [] = []
overhangFaces tris =
  let z0 = lowest tris
   in [t | t <- tris, downFacing t, minimum (zs t) > z0 + firstLayerBand]

-- | Projected area of the down faces lying wholly within the first-layer
-- band: what the first layer actually prints on. A lying cylinder has no
-- flat contact but a strip of first layer as wide as its lowest facets.
firstLayerArea :: [Tri] -> Double
firstLayerArea [] = 0
firstLayerArea tris =
  let z0 = lowest tris
   in sum [triArea t * negate nz | t <- tris, let (_, _, nz) = triNormal t, nz < 0, all (\h -> h - z0 <= firstLayerBand) (zs t)]

-- | Narrowest width of the convex hull of the XY footprint (rotating
-- calipers: the minimum over hull edges of the farthest point from that
-- edge). A 2 mm wall turned 45° is 2 mm wide, not its 43 mm bounding box.
minFootprintWidth :: [Tri] -> Double
minFootprintWidth tris = case hull (Set.toList (Set.fromList [(x, y) | (a, b, c) <- tris, (x, y, _) <- [a, b, c]])) of
  hs@(h0 : _ : _) ->
    let edges = zip hs (drop 1 hs ++ [h0])
        width ((ax, ay), (bx, by)) =
          let l = sqrt ((bx - ax) ^ (2 :: Int) + (by - ay) ^ (2 :: Int))
           in if l < 1e-12 then 1 / 0 else maximum [abs ((bx - ax) * (py - ay) - (by - ay) * (px - ax)) / l | (px, py) <- hs]
     in minimum (map width edges)
  _ -> 0
  where
    hull ps
      | length ps < 3 = ps
      | otherwise = let s = sort ps in init (chain s) ++ init (chain (reverse s))
    chain = reverse . foldl' (\acc p -> keep (p : acc)) []
    keep (p : q : r : rest) | turn r q p <= 0 = keep (p : r : rest)
    keep xs = xs
    turn (ox, oy) (ax, ay) (bx, by) = (ax - ox) * (by - oy) - (ay - oy) * (bx - ox)

-- | Area of flat overhang that is neither bridged nor close to support.
-- Overhang faces are grouped into edge-connected regions. A region edge is
-- supported when the face beyond it goes down (a wall or a self-supporting
-- slope); a region with no supported edge at all is an island, reported
-- as an elevated start instead. Each region is sampled on a ~1 mm grid; a
-- sample is fine if a supported edge is within 'pcReach', or if some line
-- through it meets supported edges on both sides within 'pcBridgeMax'.
unsupportedOverhang :: PrintabilityConfig -> [Tri] -> Double
unsupportedOverhang cfg tris = sum (map regionArea regions)
  where
    faces = overhangFaces tris
    faceSet = Set.fromList faces
    key (a, b) = if a <= b then (a, b) else (b, a)
    triEdges (a, b, c) = [((a, b), c), ((b, c), a), ((c, a), b)]
    -- every triangle of the mesh by edge, with its opposite vertex
    byEdge = Map.fromListWith (++) [(key e, [(t, o)]) | t <- tris, (e, o) <- triEdges t]
    neighbours t = [u | (e, _) <- triEdges t, (u, _) <- Map.findWithDefault [] (key e) byEdge, u /= t, Set.member u faceSet]
    regions = components Set.empty faces
    components _ [] = []
    components seen (t : ts)
      | Set.member t seen = components seen ts
      | otherwise = let r = grow Set.empty [t] in Set.toList r : components (Set.union seen r) ts
    grow acc [] = acc
    grow acc (t : ts)
      | Set.member t acc = grow acc ts
      | otherwise = grow (Set.insert t acc) (neighbours t ++ ts)
    regionArea region =
      let inRegion = Set.fromList region
          boundary =
            [ (flat a, flat b, supported)
            | t <- region, ((a, b), _) <- triEdges t
            , let others = [(u, o) | (u, o) <- Map.findWithDefault [] (key (a, b)) byEdge, u /= t]
            , not (any (\(u, _) -> Set.member u inRegion) others)
            , let (_, _, za) = a; (_, _, zb) = b
                  supported = any (\(_, (_, _, zo)) -> zo < min za zb - 1e-6) others
            ]
          supports = [(a, b) | (a, b, True) <- boundary]
       in if null supports then 0 else sum [w | (p, w) <- concatMap samples region, not (fine boundary supports p)]
    fine boundary supports p =
      any (\s -> segDist p s <= pcReach cfg) supports
        || or [ case (hit p d boundary, hit p (neg d) boundary) of
                  (Just (t1, True), Just (t2, True)) -> t1 + t2 <= pcBridgeMax cfg
                  _ -> False
              | d <- directions ]
    directions = [(cos a, sin a) | k <- [0 .. 7 :: Int], let a = pi * fromIntegral k / 8]
    neg (x, y) = (-x, -y)
    flat (x, y, _) = (x, y)
    -- nearest boundary crossing along a ray, and whether that edge is supported
    hit (px, py) (dx, dy) boundary =
      case sortOn fst [ (t, s) | ((ax, ay), (bx, by), s) <- boundary
                     , let ex = bx - ax; ey = by - ay
                           den = dx * ey - dy * ex
                     , abs den > 1e-12
                     , let t = ((ax - px) * ey - (ay - py) * ex) / den
                           u = ((ax - px) * dy - (ay - py) * dx) / den
                     , t > 1e-9, u >= -1e-9, u <= 1 + 1e-9 ] of
        h : _ -> Just h
        [] -> Nothing
    segDist (px, py) ((ax, ay), (bx, by)) =
      let ex = bx - ax; ey = by - ay
          l2 = ex * ex + ey * ey
          u = if l2 < 1e-18 then 0 else max 0 (min 1 (((px - ax) * ex + (py - ay) * ey) / l2))
          qx = ax + u * ex; qy = ay + u * ey
       in sqrt ((px - qx) ^ (2 :: Int) + (py - qy) ^ (2 :: Int))
    -- area-weighted sample points: each triangle split into n² pieces
    samples t@(a, b, c) =
      let len (p, q) = let (x, y) = (fst (flat p) - fst (flat q), snd (flat p) - snd (flat q)) in sqrt (x * x + y * y)
          n = max 1 (min 24 (ceiling (maximum (map len [(a, b), (b, c), (c, a)])) :: Int))
          w = triArea t / fromIntegral (n * n)
          (ax, ay) = flat a; (bx, by) = flat b; (cx, cy) = flat c
          at u v = (ax + u * (bx - ax) + v * (cx - ax), ay + u * (by - ay) + v * (cy - ay))
          fn = fromIntegral n
       in [ (at ((fromIntegral i + 1 / 3) / fn) ((fromIntegral j + 1 / 3) / fn), w) | i <- [0 .. n - 1], j <- [0 .. n - 1 - i] ]
            ++ [ (at ((fromIntegral i + 2 / 3) / fn) ((fromIntegral j + 2 / 3) / fn), w) | i <- [0 .. n - 2], j <- [0 .. n - 2 - i] ]

analysePrintability :: [Tri] -> Printability
analysePrintability = analysePrintabilityWith defaultPrintabilityConfig

analysePrintabilityWith :: PrintabilityConfig -> [Tri] -> Printability
analysePrintabilityWith cfg tris = Printability contact starts warnings overhang firstLayer width
  where
    ((_,_,z0),(_,_,z1)) = meshBounds tris
    eps = planeTolerance tris
    verts (a,b,c) = [a,b,c]
    z (_,_,h) = h
    height = z1 - z0
    -- any downward component: a cup's sloped underside still starts a
    -- layer in mid-air
    downward t = let (_,_,nz) = triNormal t in nz < -0.1
    -- Actual coplanar surface area, rather than the bounding rectangle. A
    -- rounded bottom can have zero flat area while still acquiring contact
    -- in the sliced first layer, which 'firstLayerArea' measures.
    contact = bedContact eps tris
    firstLayer = firstLayerArea tris
    overhang = unsupportedOverhang cfg tris
    width = if null tris then 0 else minFootprintWidth tris
    surface = sum (map triArea tris)
    adjacency = Map.fromListWith Set.union
      [(p, Set.fromList [q,r]) | t <- tris,
       (p,q,r) <- case t of (a,b,c) -> [(a,b,c),(b,a,c),(c,a,b)]]
    below = Set.fromList [p | t <- tris, downward t, p <- verts t]
    neighbours p = Map.findWithDefault Set.empty p adjacency
    plateau _ seen [] = seen
    plateau h seen (p:ps)
      | Set.member p seen = plateau h seen ps
      | otherwise = plateau h (Set.insert p seen)
          (Set.toList (Set.filter (\q -> abs (z q-h) <= eps) (neighbours p)) ++ ps)
    minima _ [] = []
    minima seen (p:ps)
      | Set.member p seen = minima seen ps
      | otherwise =
          let flat = plateau (z p) Set.empty [p]
              around = Set.unions (map neighbours (Set.toList flat))
              low = z p-z0 > eps
                    && all (\q -> z q >= z p-eps) (Set.toList around)
                    && not (Set.null (Set.intersection below flat))
           in [z p-z0 | low] ++ minima (Set.union seen flat) ps
    -- A downward-facing local minimum above the bed indicates material
    -- starting away from the bed (e.g. the tesseract's raised collars).
    starts = foldr (\h rest -> case rest of
                        r:_ | abs (h-r) <= eps -> rest
                        _ -> h:rest) [] (sort (minima Set.empty (Map.keys adjacency)))
    -- Narrowest footprint direction, not the axis-aligned box; the limit
    -- relaxes for short parts, which do not wobble.
    slender = height / max 1 width
    slenderLimit = pcSlenderBase cfg + pcSlenderShort cfg / max 1 height
    -- What holds the part is the first layer, flat or not; taller parts
    -- need more of it.
    contactNeeded = max (pcMinContact cfg) (pcContactPerHeight2 cfg * height * height)
    overhangLimit = max (pcOverhangMin cfg) (pcOverhangFraction cfg * surface)
    warnings =
      [("small_contact", "only " ++ showD contact ++ " mm^2 of flat bed contact (" ++ showD firstLayer ++ " mm^2 of first layer); check the first layer and use a brim or a broader print face")
       | not (null tris), firstLayer < contactNeeded]
      ++ [("tall_narrow", "tall, narrow print (height/width " ++ showD slender ++ " over its narrowest " ++ showD width ++ " mm); use a brim and conservative acceleration")
          | slender > slenderLimit]
      ++ [("flat_overhang", showD overhang ++ " mm^2 of flat overhang is neither bridged nor within " ++ showD (pcReach cfg) ++ " mm of a supporting wall; enable supports or reorient")
          | overhang > overhangLimit]
      ++ [("elevated_start", "elevated downward-facing start at " ++ showD h ++ " mm above the bed; inspect supports before printing")
          | h <- starts]
