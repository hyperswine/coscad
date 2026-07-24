-- | Vector math, bounding boxes, anchors, and the attachment
-- desugaring pass ('resolve'). Everything the compiler computes
-- upstream so the emitted OpenSCAD can stay dumb.
module Coscad.Geometry (module Coscad.Geometry) where

import Coscad.Shape

-- ANCHOR / ATTACHMENT ENGINE -----------------------------------
-- Anchors are BOSL2-style unit-ish vectors on the bounding box:
-- (0,0,1) = top face center, (1,0,1) = top-right edge center, etc.
-- Everything is computed here in the compiler and desugared to
-- plain translate/rotate, so ANY shape expression (including
-- unions and differences) has anchors and stays composable.

type V3 = (D, D, D)

type BBox = (V3, V3) -- (min corner, max corner)

vadd, vsub, vmul :: V3 -> V3 -> V3
vadd (a, b, c) (x, y, z) = (a + x, b + y, c + z)
vsub (a, b, c) (x, y, z) = (a - x, b - y, c - z)
vmul (a, b, c) (x, y, z) = (a * x, b * y, c * z)

vneg :: V3 -> V3
vneg (a, b, c) = (-a, -b, -c)

vlen :: V3 -> D
vlen (a, b, c) = sqrt (a * a + b * b + c * c)

vnormed :: V3 -> V3
vnormed v@(a, b, c) = let l = vlen v in if l == 0 then (0, 0, 1) else (a / l, b / l, c / l)

vclamp :: V3 -> V3
vclamp (a, b, c) = (cl a, cl b, cl c)
  where
    cl = max (-1) . min 1

cross :: V3 -> V3 -> V3
cross (a1, a2, a3) (b1, b2, b3) = (a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1)

type M3 = (V3, V3, V3) -- rows

mApply :: M3 -> V3 -> V3
mApply (r1, r2, r3) v = (dot r1 v, dot r2 v, dot r3 v)
  where
    dot (a, b, c) (x, y, z) = a * x + b * y + c * z

-- | Rodrigues rotation matrix: angle in degrees about an axis
rodMat :: D -> V3 -> M3
rodMat aDeg axis =
  let a = aDeg * pi / 180
      (x, y, z) = vnormed axis
      c = cos a
      s = sin a
      t = 1 - c
   in ( (t * x * x + c, t * x * y - s * z, t * x * z + s * y),
        (t * x * y + s * z, t * y * y + c, t * y * z - s * x),
        (t * x * z - s * y, t * y * z + s * x, t * z * z + c)
      )

-- | Reflection matrix across the plane with normal n
mirrorMat :: V3 -> M3
mirrorMat n =
  let (x, y, z) = vnormed n
   in ( (1 - 2 * x * x, -2 * x * y, -2 * x * z),
        (-2 * x * y, 1 - 2 * y * y, -2 * y * z),
        (-2 * x * z, -2 * y * z, 1 - 2 * z * z)
      )

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

-- | Rotation (degrees, axis) taking +Z to the given direction
rotFromUp :: V3 -> (D, V3)
rotFromUp v =
  let d@(_, _, dz) = vnormed v
   in if vlen (vsub d (0, 0, 1)) < 1e-9
        then (0, (1, 0, 0))
        else
          if vlen (vsub d (0, 0, -1)) < 1e-9
            then (180, (1, 0, 0))
            else (acos (max (-1) (min 1 dz)) * 180 / pi, cross (0, 0, 1) d)

xformBBox :: M3 -> BBox -> BBox
xformBBox m = fromCorners . map (mApply m) . bcorners

-- | Bounding box of a (resolved) shape tree
bbox :: Shape -> BBox
bbox s = case s of
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
  Scale v x -> fromCorners (map (vmul v) (bcorners (bbox x)))
  Mirror n x -> xformBBox (mirrorMat n) (bbox x)
  Diff a _ -> bbox a
  Extrude h x -> let ((x0, y0, _), (x1, y1, _)) = bbox x in ((x0, y0, 0), (x1, y1, h))
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
  Scale v x -> Scale v (resolve x)
  Mirror n x -> Mirror n (resolve x)
  Extrude h x -> Extrude h (resolve x)
  Offset r x -> Offset r (resolve x)
  Diff a b -> Diff (resolve a) (resolve b)
  Union xs -> Union (map resolve xs)
  Intersection xs -> Intersection (map resolve xs)
  Hull xs -> Hull (map resolve xs)
  Minkowski xs -> Minkowski (map resolve xs)
  prim -> prim

