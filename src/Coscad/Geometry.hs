-- | Vector math, bounding boxes, anchors, and the attachment
-- desugaring pass ('resolve'). Everything the compiler computes
-- upstream so the emitted OpenSCAD can stay dumb.
module Coscad.Geometry
  ( BBox
  , MatingFrame (..)
  , PointRef (..)
  , anchorPt
  , bbox
  , bcenter
  , bcorners
  , bhalf
  , bmerge
  , boverlap
  , datumPoints
  , frameOf
  , framePoints
  , fromCorners
  , makeFrame
  , matingPose
  , pointOf
  , resolve
  , rigidFrameBasis
  , xformBBox
  , module Coscad.Linalg
  )
where

import Coscad.Linalg
import Coscad.Shape
import Data.List (intercalate, isSuffixOf, nub)
import qualified Data.Map.Strict as Map

-- ANCHOR / ATTACHMENT ENGINE -----------------------------------
-- Anchors are BOSL2-style unit-ish vectors on the bounding box:
-- (0,0,1) = top face center, (1,0,1) = top-right edge center, etc.
-- Everything is computed here in the compiler and desugared to
-- plain translate/rotate, so ANY shape expression (including
-- unions and differences) has anchors and stays composable.

type BBox = (V3, V3) -- (min corner, max corner)

-- | A bounding-box anchor, point datum or frame origin. Point placement uses
-- only the position; oriented mating selects the complete frame separately.
data PointRef = BoxPoint V3 | NamedPoint String deriving (Show)

pointOf :: PointRef -> Shape -> Either String V3
pointOf (BoxPoint v) s = Right (anchorPt (bbox s) v)
pointOf (NamedPoint name) s = case Map.lookup name points of
  Nothing -> Left ("unknown datum '@" ++ name ++ "'; available datums: " ++ available)
  Just [p] -> Right p
  Just ps -> Left ("ambiguous datum '@" ++ name ++ "': " ++ show (length ps) ++ " copies; select a single feature before combining or repeating it, or define a new datum on the result")
  where
    points = datumPoints s
    available = case Map.keys points of [] -> "none"; ns -> unwords (map ('@' :) ns)

-- | Datums follow transforms, including transforms produced by attachments.
-- CSG retains construction points from all operands, even when material is
-- removed. Repeated names deliberately remain ambiguous, rather than picking
-- an arbitrary copy. Extrusion retains the profile's plane; loft lifts each
-- profile's points to its specified layer.
datumPoints :: Shape -> Map.Map String [V3]
datumPoints = Map.fromListWith (++) . map (\(n, p) -> (n, [p])) . go . resolve
  where
    moved f = map (\(n, p) -> (n, f p))
    go s = case s of
      Datum n p x -> (n, p) : go x
      Frame n p _ x -> (n, p) : go x
      Tx d x -> moved (vadd (d, 0, 0)) (go x)
      Ty d x -> moved (vadd (0, d, 0)) (go x)
      Tz d x -> moved (vadd (0, 0, d)) (go x)
      Translate v x -> moved (vadd v) (go x)
      Rx a x -> moved (mApply (rodMat a (1, 0, 0))) (go x)
      Ry a x -> moved (mApply (rodMat a (0, 1, 0))) (go x)
      Rz a x -> moved (mApply (rodMat a (0, 0, 1))) (go x)
      RotAxis a v x -> moved (mApply (rodMat a v)) (go x)
      Transform3 m t x -> moved (vadd t . mApply m) (go x)
      Scale v x -> moved (vmul v) (go x)
      Mirror n x -> moved (mApply (mirrorMat n)) (go x)
      Loft ps -> concat [moved (\(a, b, _) -> (a, b, z)) (go p) | (z, p) <- ps]
      _ -> concatMap go (shapeChildren s)

-- | A frame's +Z points out of the mating face. +Y is its in-plane up
-- direction. Columns are local axes in the containing shape's coordinates.
data MatingFrame = MatingFrame { frameOrigin :: V3, frameBasis :: M3 } deriving (Show)

makeFrame :: V3 -> V3 -> V3 -> Either String MatingFrame
makeFrame origin normal up
  | normal == (0,0,0) || up == (0,0,0) = Left "frame normal and up directions must be nonzero; ctr is not a direction"
  | vlen projected < 1e-9 = Left "frame normal and up directions must not be parallel; choose an in-plane up direction"
  | otherwise = Right (MatingFrame origin (matTranspose (cross y z, y, z)))
  where
    z = vnormed normal
    u = vnormed up
    projected = vsub u (scale3 (dot3 u z) z)
    y = vnormed projected

-- | Keep both origin and axes through transforms. A reflected or skewed
-- frame cannot describe a rigid mate. Selectors diagnose this explicitly;
-- authors can define a fresh frame after the geometry transform instead.
framePoints :: Shape -> Map.Map String [MatingFrame]
framePoints = Map.fromListWith (++) . map (\(n,f) -> (n,[f])) . go . resolve
  where
    moved m t = map (\(n, MatingFrame p b) -> (n, MatingFrame (vadd t (mApply m p)) (matMul m b)))
    go s = case s of
      Frame n p b x -> (n, MatingFrame p b) : go x
      -- an assembly instance publishes its frames twice: as themselves
      -- and qualified by its alias (@q.b, and @s.q.b one level up)
      Tag n x -> let fs = go x in fs ++ [(n ++ "." ++ k, f) | (k, f) <- fs]
      Tx d x -> moved matIdentity (d,0,0) (go x)
      Ty d x -> moved matIdentity (0,d,0) (go x)
      Tz d x -> moved matIdentity (0,0,d) (go x)
      Translate t x -> moved matIdentity t (go x)
      Rx a x -> moved (rodMat a (1,0,0)) (0,0,0) (go x)
      Ry a x -> moved (rodMat a (0,1,0)) (0,0,0) (go x)
      Rz a x -> moved (rodMat a (0,0,1)) (0,0,0) (go x)
      RotAxis a v x -> moved (rodMat a v) (0,0,0) (go x)
      Transform3 m t x -> moved m t (go x)
      Scale (a,b,c) x -> moved ((a,0,0),(0,b,0),(0,0,c)) (0,0,0) (go x)
      Mirror n x -> moved (mirrorMat n) (0,0,0) (go x)
      _ -> concatMap go (shapeChildren s)

frameOf :: String -> Shape -> Either String MatingFrame
frameOf name s = case Map.lookup name (framePoints s) of
  Nothing | Map.member name (datumPoints s) -> Left ("'@" ++ name ++ "' is a point datum; define an oriented frame with |> frame before mating")
          | otherwise -> Left ("unknown frame '@" ++ name ++ "'; available frames: " ++ available)
  Just [MatingFrame p b] -> case rigidFrameBasis b of
    Left err -> Left ("frame '@" ++ name ++ "': " ++ err)
    Right normalized -> Right (MatingFrame p normalized)
  Just fs -> Left ("ambiguous frame '@" ++ name ++ "': " ++ show (length fs) ++ " copies; " ++ qualify)
  where
    frames = framePoints s
    available = case [n | n <- Map.keys frames, '.' `notElem` n] of
      [] -> "none"
      ns -> unwords (map ('@':) ns) ++ case nub [takeWhile (/= '.') n | n <- Map.keys frames, '.' `elem` n] of
        [] -> ""
        is@(i : _) -> " (qualify with an instance alias: " ++ unwords is ++ ", e.g. @" ++ i ++ "." ++ head' ns ++ ")"
    head' ns = case ns of n : _ -> n; [] -> "name"
    -- the shortest unambiguous qualified names for this frame
    qualified = [q | (q, [_]) <- Map.toList frames, ('.' : name) `isSuffixOf` q, length (filter (== '.') q) == 1 + length (filter (== '.') name)]
    qualify = case qualified of
      [] -> "select one instance or define a new frame on the result"
      qs -> "qualify it with its instance: " ++ intercalate ", " (map ('@' :) qs)

rigidFrameBasis :: M3 -> Either String M3
rigidFrameBasis b
  | any bad (concatMap vec [a,c,d]) || any (== (0,0,0)) [a,c,d] = invalid
  | maximum (map abs [dot3 x y, dot3 x z, dot3 y z]) > 1e-8 || dot3 (cross x y) z < 1 - 1e-8 = invalid
  | otherwise = Right (matTranspose (x,y,z))
  where
    (a,c,d) = matTranspose b
    (x,y,z) = (vnormed a, vnormed c, vnormed d)
    vec (i,j,k) = [i,j,k]
    bad n = isNaN n || isInfinite n
    invalid = Left "frame axes are reflected, degenerate or skewed; define a new frame after the mirror or nonuniform scale"

-- | Child-to-parent pose. At zero roll, normals oppose and up axes agree.
-- Gap is measured along the parent's outward normal; roll is right-handed
-- about that normal. No geometry from the reference parent is returned.
matingPose :: MatingFrame -> MatingFrame -> D -> D -> (M3, V3)
matingPose (MatingFrame pp pb) (MatingFrame cp cb) roll gap = (r, vsub target (mApply r cp))
  where
    flipFace = ((-1,0,0),(0,1,0),(0,0,-1))
    r = pb `matMul` rodMat roll (0,0,1) `matMul` flipFace `matMul` matTranspose cb
    target = vadd pp (scale3 gap (mApply pb (0,0,1)))

bcorners :: BBox -> [V3]
bcorners ((x0, y0, z0), (x1, y1, z1)) =
  [(x, y, z) | x <- [x0, x1], y <- [y0, y1], z <- [z0, z1]]

fromCorners :: [V3] -> BBox
fromCorners pts =
  ( (minimum xs, minimum ys, minimum zs),
    (maximum xs, maximum ys, maximum zs)
  )
  where
    (xs, ys, zs) = unzip3 pts

bmerge :: BBox -> BBox -> BBox
bmerge a b = fromCorners (bcorners a ++ bcorners b)

-- | Conservative bbox of an intersection (componentwise overlap)
boverlap :: BBox -> BBox -> BBox
boverlap ((a,b,c),(d,e,f)) ((p,q,r),(s,t,u)) =
  ((max a p, max b q, max c r), (min d s, min e t, min f u))

bcenter :: BBox -> V3
bcenter (lo, hi) = vmul (0.5, 0.5, 0.5) (vadd lo hi)

bhalf :: BBox -> V3
bhalf (lo, hi) = vmul (0.5, 0.5, 0.5) (vsub hi lo)

-- | Point on the bounding box for an anchor vector (components in [-1,1])
anchorPt :: BBox -> V3 -> V3
anchorPt bb v = vadd (bcenter bb) (vmul (bhalf bb) (vclamp v))

xformBBox :: M3 -> BBox -> BBox
xformBBox m = fromCorners . map (mApply m) . bcorners

-- | Bounding box of a (resolved) shape tree
bbox :: Shape -> BBox
bbox s = case s of
  Empty -> ((0, 0, 0), (0, 0, 0))
  Hidden x -> bbox x
  Rectangle x y z -> ((0, 0, 0), (x, y, z))
  Sphere r -> ((-r, -r, -r), (r, r, r))
  Cylinder r h -> ((-r, -r, 0), (r, r, h))
  Cone r h -> ((-r, -r, 0), (r, r, h))
  Frustum h r1 r2 -> let r = max r1 r2 in ((-r, -r, 0), (r, r, h))
  Prism _ r h -> ((-r, -r, 0), (r, r, h))
  Poly (PD pts _) ->
    let (xs, ys) = unzip pts
     in ((minimum xs, minimum ys, 0), (maximum xs, maximum ys, 0))
  Shape2D _ r -> ((-r, -r, 0), (r, r, 0))
  Cuboid (x, y, z) _ _ -> ((-x / 2, -y / 2, -z / 2), (x / 2, y / 2, z / 2))
  Cyl r h _ _ -> ((-r, -r, -h / 2), (r, r, h / 2))
  XCyl r l -> ((-l / 2, -r, -r), (l / 2, r, r))
  YCyl r l -> ((-r, -l / 2, -r), (r, l / 2, r))
  ZCyl r l -> ((-r, -r, -l / 2), (r, r, l / 2))
  Tube ro _ h -> ((-ro, -ro, -h / 2), (ro, ro, h / 2))
  Prismoid (x1, y1) (x2, y2) h ->
    let hx = max x1 x2 / 2; hy = max y1 y2 / 2
     in ((-hx, -hy, -h / 2), (hx, hy, h / 2))
  Torus rj rn -> ((-(rj + rn), -(rj + rn), -rn), (rj + rn, rj + rn, rn))
  Wedge (x, y, z) -> ((-x / 2, -y / 2, -z / 2), (x / 2, y / 2, z / 2))
  Tx d x -> bshift (d, 0, 0) (bbox x)
  Ty d x -> bshift (0, d, 0) (bbox x)
  Tz d x -> bshift (0, 0, d) (bbox x)
  Translate v x -> bshift v (bbox x)
  Rx a x -> xformBBox (rodMat a (1, 0, 0)) (bbox x)
  Ry a x -> xformBBox (rodMat a (0, 1, 0)) (bbox x)
  Rz a x -> xformBBox (rodMat a (0, 0, 1)) (bbox x)
  RotAxis a v x -> xformBBox (rodMat a v) (bbox x)
  Transform3 m t x -> bshift t (xformBBox m (bbox x))
  Scale v x -> fromCorners (map (vmul v) (bcorners (bbox x)))
  Mirror n x -> xformBBox (mirrorMat n) (bbox x)
  Diff a _ -> bbox a
  Extrude h x -> let ((x0, y0, _), (x1, y1, _)) = bbox x in ((x0, y0, 0), (x1, y1, h))
  Loft ps -> fromCorners [(px, py, z) | (z, p) <- ps, (px, py, _) <- bcorners (bbox p)]
  Tag _ x -> bbox x
  Datum _ _ x -> bbox x
  Frame _ _ _ x -> bbox x
  Offset r x -> let ((x0, y0, z0), (x1, y1, z1)) = bbox x in ((x0 - r, y0 - r, z0), (x1 + r, y1 + r, z1))
  Union xs -> foldr1 bmerge (map bbox xs)
  Intersection xs -> foldr1 boverlap (map bbox xs)
  Hull xs -> foldr1 bmerge (map bbox xs)
  Minkowski xs -> foldr1 (\(l1, h1) (l2, h2) -> (vadd l1 l2, vadd h1 h2)) (map bbox xs)
  Anchor {} -> bbox (resolve s)
  Position {} -> bbox (resolve s)
  AttachTo {} -> bbox (resolve s)
  CutAt {} -> bbox (resolve s)
  where
    bshift v (lo, hi) = (vadd v lo, vadd v hi)

-- | Desugar attachment operations into plain transforms (bottom-up)
resolve :: Shape -> Shape
resolve s = case s of
  Hidden x -> Hidden (resolve x)
  Anchor v x ->
    let x' = resolve x
     in Translate (vneg (anchorPt (bbox x') v)) x'
  Position v off p c ->
    let p' = resolve p
        c' = resolve c
        pa = vadd off (anchorPt (bbox p') v)
        ca = anchorPt (bbox c') (vneg v)
     in Union [p', Translate (vsub pa ca) c']
  CutAt v off p c ->
    let p' = resolve p
        c' = resolve c
        target = vadd off (anchorPt (bbox p') v)
        cc = bcenter (bbox c')
     in Diff p' (Translate (vsub target cc) c')
  AttachTo v off p c ->
    let p' = resolve p
        c' = resolve c
        pa = vadd off (anchorPt (bbox p') v)
        cb = bbox c'
        ctr = bcenter cb
        bot = anchorPt cb (0, 0, -1)
        (ang, ax) = rotFromUp v
        rc =
          if ang == 0
            then c'
            else Translate ctr (RotAxis ang ax (Translate (vneg ctr) c'))
        matePt =
          if ang == 0
            then bot
            else vadd ctr (mApply (rodMat ang ax) (vsub bot ctr))
     in Union [p', Translate (vsub pa matePt) rc]
  -- plain recursion for everything else
  Tx d x -> Tx d (resolve x)
  Ty d x -> Ty d (resolve x)
  Tz d x -> Tz d (resolve x)
  Translate v x -> Translate v (resolve x)
  Rx a x -> Rx a (resolve x)
  Ry a x -> Ry a (resolve x)
  Rz a x -> Rz a (resolve x)
  RotAxis a v x -> RotAxis a v (resolve x)
  Transform3 m t x -> Transform3 m t (resolve x)
  Scale v x -> Scale v (resolve x)
  Mirror n x -> Mirror n (resolve x)
  Extrude h x -> Extrude h (resolve x)
  Loft ps -> Loft [(z, resolve p) | (z, p) <- ps]
  Tag n x -> Tag n (resolve x)
  Datum n p x -> Datum n p (resolve x)
  Frame n p b x -> Frame n p b (resolve x)
  Offset r x -> Offset r (resolve x)
  Diff a b -> Diff (resolve a) (resolve b)
  Union xs -> Union (map resolve xs)
  Intersection xs -> Intersection (map resolve xs)
  Hull xs -> Hull (map resolve xs)
  Minkowski xs -> Minkowski (map resolve xs)
  prim -> prim

