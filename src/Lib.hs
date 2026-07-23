{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Lib where

import Data.List (intercalate)

type D = Double

type I = Int

data Shape
  = Rectangle D D D
  | Sphere D
  | Cylinder D D
  | Cone D D
  | Frustum D D D
  | Prism I D D
  | Poly PD
  | Shape2D I D
  | -- BOSL2 shapes ----------------------------------------
    Cuboid (D, D, D) D D -- size, chamfer, rounding (0 = none)
  | Cyl D D D D -- radius, height, chamfer, rounding (0 = none)
  | XCyl D D -- radius, length (axis along X)
  | YCyl D D
  | ZCyl D D
  | Tube D D D -- outer radius, inner radius, height
  | Prismoid (D, D) (D, D) D -- bottom size, top size, height
  | Torus D D -- major radius, minor radius
  | Wedge (D, D, D) -- right-triangular prism, vertical face at -X
  | -- Transforms ------------------------------------------
    Tx D Shape
  | Ty D Shape
  | Tz D Shape
  | Rx D Shape
  | Ry D Shape
  | Rz D Shape
  | Scale (D, D, D) Shape
  | Mirror (D, D, D) Shape
  | Translate (D, D, D) Shape -- vector translate (used by attachment desugaring)
  | RotAxis D (D, D, D) Shape -- axis-angle rotation (used by attachment desugaring)
  | -- Attachment sugar (desugared by 'resolve' before codegen) ----
    Anchor (D, D, D) Shape -- re-origin shape at its own anchor point
  | Position (D, D, D) (D, D, D) Shape Shape -- anchor, offset, parent, child: snap child at anchor+offset
  | AttachTo (D, D, D) (D, D, D) Shape Shape -- anchor, offset, parent, child: rotate-mate at anchor+offset
  | CutAt (D, D, D) (D, D, D) Shape Shape -- anchor, offset, parent, cutter: SUBTRACT cutter centered there
  | Diff Shape Shape
  | Extrude D Shape
  | Union [Shape]
  | Intersection [Shape]
  | Hull [Shape]
  | Minkowski [Shape]
  | Offset D Shape
  deriving (Show)

-- | Polygon Data
data PD = PD [(D, D)] [[D]] deriving (Show)

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

-- | Helper function for polygon conversion
showPoints points = "[" ++ intercalate ", " (map showPoint points) ++ "]"
  where
    showPoint (x, y) = "[" ++ show x ++ ", " ++ show y ++ "]"

-- | Helper function for polygon conversion
showPaths paths = "[" ++ intercalate ", " (map showPath paths) ++ "]"
  where
    showPath path = "[" ++ intercalate ", " (map show path) ++ "]"

indent = unlines . map ("  " ++) . lines

-- | Emit optional BOSL2 chamfer/rounding arguments
boslMod ch ro =
  (if ch /= 0 then ", chamfer = " ++ show ch else "")
    ++ (if ro /= 0 then ", rounding = " ++ show ro else "")

gen (Rectangle x y z) = "cube(" ++ "[" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ "]" ++ ");"
gen (Sphere r) = "sphere(" ++ show r ++ ");"
gen (Shape2D n r) =
  "circle(r = " ++ show r ++ ", $fn = " ++ show n ++ ");"
gen (Cylinder r h) =
  "cylinder(h = " ++ show h ++ ", r = " ++ show r ++ ");"
gen (Cone r h) =
  "cylinder(h = " ++ show h ++ ", r1 = " ++ show r ++ ", r2 = 0);"
gen (Frustum h r1 r2) =
  "cylinder(h = " ++ show h ++ ", r1 = " ++ show r1 ++ ", r2 = " ++ show r2 ++ ");"
gen (Prism n r h) =
  "cylinder(h = " ++ show h ++ ", r = " ++ show r ++ ", $fn = " ++ show n ++ ");"
gen (Poly (PD points [])) =
  "polygon(points = " ++ showPoints points ++ ");"
gen (Poly (PD points paths)) =
  "polygon(points = " ++ showPoints points ++ ", paths = " ++ showPaths paths ++ ");"
-- BOSL2 primitives (all centered at origin, BOSL2 default anchoring)
gen (Cuboid (x, y, z) ch ro) =
  "cuboid([" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ "]" ++ boslMod ch ro ++ ");"
gen (Cyl r h ch ro) =
  "cyl(r = " ++ show r ++ ", h = " ++ show h ++ boslMod ch ro ++ ");"
gen (XCyl r l) =
  "xcyl(r = " ++ show r ++ ", l = " ++ show l ++ ");"
gen (YCyl r l) =
  "ycyl(r = " ++ show r ++ ", l = " ++ show l ++ ");"
gen (ZCyl r l) =
  "zcyl(r = " ++ show r ++ ", l = " ++ show l ++ ");"
gen (Tube ro ri h) =
  "tube(h = " ++ show h ++ ", or = " ++ show ro ++ ", ir = " ++ show ri ++ ");"
gen (Prismoid (x1, y1) (x2, y2) h) =
  "prismoid(size1 = [" ++ show x1 ++ ", " ++ show y1 ++ "], size2 = [" ++ show x2 ++ ", " ++ show y2 ++ "], h = " ++ show h ++ ", anchor = CENTER);"
gen (Torus rj rn) =
  "torus(r_maj = " ++ show rj ++ ", r_min = " ++ show rn ++ ");"
gen (Wedge (x, y, z)) =
  "wedge([" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ "], anchor = CENTER);"
-- Transforms
gen (Tx dx s) =
  "translate([" ++ show dx ++ ", 0, 0]) {\n" ++ indent (gen s) ++ "}"
gen (Ty dy s) =
  "translate([0, " ++ show dy ++ ", 0]) {\n" ++ indent (gen s) ++ "}"
gen (Tz dz s) =
  "translate([0, 0, " ++ show dz ++ "]) {\n" ++ indent (gen s) ++ "}"
gen (Rx ax s) =
  "rotate([" ++ show ax ++ ", 0, 0]) {\n" ++ indent (gen s) ++ "}"
gen (Ry ay s) =
  "rotate([0, " ++ show ay ++ ", 0]) {\n" ++ indent (gen s) ++ "}"
gen (Rz az s) =
  "rotate([0, 0, " ++ show az ++ "]) {\n" ++ indent (gen s) ++ "}"
gen (Scale (sx, sy, sz) s) =
  "scale([" ++ show sx ++ ", " ++ show sy ++ ", " ++ show sz ++ "]) {\n" ++ indent (gen s) ++ "}"
gen (Mirror (mx, my, mz) s) =
  "mirror([" ++ show mx ++ ", " ++ show my ++ ", " ++ show mz ++ "]) {\n" ++ indent (gen s) ++ "}"
gen (Translate (x, y, z) s) =
  "translate([" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ "]) {\n" ++ indent (gen s) ++ "}"
gen (RotAxis a (x, y, z) s) =
  "rotate(a = " ++ show a ++ ", v = [" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ "]) {\n" ++ indent (gen s) ++ "}"
-- attachment sugar should be resolved before codegen; desugar defensively
gen s@(Anchor {}) = gen (resolve s)
gen s@(Position {}) = gen (resolve s)
gen s@(AttachTo {}) = gen (resolve s)
gen s@(CutAt {}) = gen (resolve s)
gen (Extrude h s) =
  "linear_extrude(height = " ++ show h ++ ") {\n" ++ indent (gen s) ++ "}"
gen (Diff a b) =
  "difference() {\n" ++ indent (gen a) ++ indent (gen b) ++ "}"
gen (Union shapes) =
  "union() {\n" ++ concatMap (indent . gen) shapes ++ "}"
gen (Intersection shapes) =
  "intersection() {\n" ++ concatMap (indent . gen) shapes ++ "}"
gen (Hull shapes) =
  "hull() {\n" ++ concatMap (indent . gen) shapes ++ "}"
gen (Minkowski shapes) =
  "minkowski() {\n" ++ concatMap (indent . gen) shapes ++ "}"
gen (Offset r s) =
  "offset(r = " ++ show r ++ ") {\n" ++ indent (gen s) ++ "}"

-- | Does the shape tree use any BOSL2 primitives?
usesBosl2 :: Shape -> Bool
usesBosl2 s = case s of
  Cuboid {} -> True
  Cyl {} -> True
  XCyl {} -> True
  YCyl {} -> True
  ZCyl {} -> True
  Tube {} -> True
  Prismoid {} -> True
  Torus {} -> True
  Wedge {} -> True
  Tx _ x -> usesBosl2 x
  Ty _ x -> usesBosl2 x
  Tz _ x -> usesBosl2 x
  Rx _ x -> usesBosl2 x
  Ry _ x -> usesBosl2 x
  Rz _ x -> usesBosl2 x
  Scale _ x -> usesBosl2 x
  Mirror _ x -> usesBosl2 x
  Translate _ x -> usesBosl2 x
  RotAxis _ _ x -> usesBosl2 x
  Anchor _ x -> usesBosl2 x
  Position _ _ a b -> usesBosl2 a || usesBosl2 b
  AttachTo _ _ a b -> usesBosl2 a || usesBosl2 b
  CutAt _ _ a b -> usesBosl2 a || usesBosl2 b
  Extrude _ x -> usesBosl2 x
  Offset _ x -> usesBosl2 x
  Diff a b -> usesBosl2 a || usesBosl2 b
  Union xs -> any usesBosl2 xs
  Intersection xs -> any usesBosl2 xs
  Hull xs -> any usesBosl2 xs
  Minkowski xs -> any usesBosl2 xs
  _ -> False

writeScad shape filename = writeFile filename (header ++ fn50 (gen shape'))
  where
    shape' = resolve shape
    header = if usesBosl2 shape' then "include <BOSL2/std.scad>\n\n" else ""

-- Attachment aliases (Haskell-side DSL) ------------------------

-- | anchor vectors
top, bot, lft, rt, fwd, bak, ctr :: V3
top = (0, 0, 1)
bot = (0, 0, -1)
lft = (-1, 0, 0)
rt = (1, 0, 0)
fwd = (0, -1, 0)
bak = (0, 1, 0)
ctr = (0, 0, 0)

-- | posAt v parent child: child snapped to parent's anchor v (glyph ⌖)
posAt v = Position v (0, 0, 0)

-- | attachAt v parent child: child oriented +Z along v, bottom mated (glyph ⋈)
attachAt v = AttachTo v (0, 0, 0)

-- | cutAt v parent cutter: cutter centered at parent's anchor v, subtracted
cutAt v = CutAt v (0, 0, 0)

-- | anchorAt v shape: re-origin shape at its own anchor point (glyph ⚓)
anchorAt = Anchor

fn50 x = x ++ "\n$fn = 50;"

-- ACTUAL LANGUAGE
-- Glyph-style aliases

{-
cube = ■ 25A0
sphere = ● 25CF
cyl = ◎ 25CE
cone = ▻ 25BB
rec = ▬

diff = ⊖ 2296
union = ⊛ 229b

extrude = ⮕ 2B95

scale = ⬈ 2B08

BOSL2 layer:
cuboid (chamfer)  = ▣ 25A3   ▣ x y z c
cuboid (rounding) = ◙ 25D9   ◙ x y z r
cyl (chamfer)     = ⌭ 232D   ⌭ r h c
cyl (rounding)    = ⌽ 233D   ⌽ r h r2
tube              = ⊚ 229A   ⊚ or ir h
prismoid          = ⏢ 23E2   ⏢ x1 y1 x2 y2 h
torus             = ◉ 25C9   ◉ rmaj rmin
wedge             = ⊿ 22BF   ⊿ x y z
mirror            = ⇋ 21CB   ⇋ mx my mz shape
axis cylinders    = xcyl / ycyl / zcyl r l (keywords)
-}

-- | cube x
(■) x = Rectangle x x x

cube = (■)

rect x = Rectangle x x x

(▬) = rect

-- | sphere r
(●) = Sphere

sph = (●)

-- | cylinder radius height
(◎) = Cylinder

cyl = (◎)

-- | white up pointing triangle
-- | cone radius height
(▻) = Cone

cone = (▻)

-- | prism n radius height
prism = Prism

(⎏) = prism

-- 2D regular polygons

-- | equilateral triangle profile
(△) = Shape2D 3

-- | regular pentagon profile
(⬠) = Shape2D 5

-- | circle profile
(⭘) = Shape2D 100

-- | Create a polygon with points and paths
poly points paths = Poly (PD points paths)

-- | Unicode operator for polygon
(⟁) = poly

-- | Piecewise cubic bezier path -> closed polygon Shape, evaluated
-- here in the compiler (the emitted OpenSCAD is a plain polygon).
-- Takes 3k+1 control points; each consecutive group of 4 (sharing
-- endpoints) is one cubic segment. The path auto-closes.
bezPoly :: Int -> [(D, D)] -> Shape
bezPoly samples cps = Poly (PD pts [])
  where
    pts = concatMap sampleSeg (segsOf cps) ++ [last cps]
    segsOf (a : b : c : d : rest) = (a, b, c, d) : segsOf (d : rest)
    segsOf _ = []
    sampleSeg (p0, p1, p2, p3) =
      [cubic p0 p1 p2 p3 (fromIntegral i / fromIntegral samples) | i <- [0 .. samples - 1]]
    cubic (x0, y0) (x1, y1) (x2, y2) (x3, y3) t =
      let u = 1 - t
          f a b c d = u * u * u * a + 3 * u * u * t * b + 3 * u * t * t * c + t * t * t * d
       in (f x0 x1 x2 x3, f y0 y1 y2 y3)

-- | Bezier path glyph (Haskell side takes the point list directly)
(✎) = bezPoly 24

-- BOSL2 aliases -----------------------------------------------

-- | chamfered cuboid: ▣ x y z chamfer
(▣) x y z c = Cuboid (x, y, z) c 0

cuboid x y z = Cuboid (x, y, z) 0 0

-- | rounded cuboid: ◙ x y z rounding
(◙) x y z r = Cuboid (x, y, z) 0 r

-- | chamfered centered cylinder: ⌭ r h chamfer
(⌭) r h c = Cyl r h c 0

-- | rounded centered cylinder: ⌽ r h rounding
(⌽) r h ro = Cyl r h 0 ro

-- | axis-aligned cylinders: radius length
xcyl = XCyl

ycyl = YCyl

zcyl = ZCyl

-- | tube: outer-r inner-r height
(⊚) = Tube

tube = Tube

-- | prismoid: x1 y1 x2 y2 h
(⏢) x1 y1 x2 y2 = Prismoid (x1, y1) (x2, y2)

-- | torus: major-r minor-r
(◉) = Torus

torus = Torus

-- | wedge: x y z (vertical face at -X, hypotenuse in XZ)
(⊿) x y z = Wedge (x, y, z)

wedge x y z = Wedge (x, y, z)

-- Transform glyphs --------------------------------------------

χ = Tx

ψ = Ty

ζ = Tz

-- | Rotation glyphs (axis‑specific)
θ = Rx

ϕ = Ry

ω = Rz

-- | Scaling (vector form)
(⬈) = Scale

-- | Mirror across plane normal (mx, my, mz)
(⇋) mx my mz = Mirror (mx, my, mz)

-- | Linear extrusion (2‑D → 3‑D)
(⮕) = Extrude

infixl 0 ⊞

infixl 0 ⇓

infixl 1 ↯

(⇓) = Hull

(⊞) = Minkowski

(↯) = Offset

infixl 0 ⊖

infixl 0 ⊝

infixl 0 ⊛

infixl 0 ⊕

-- Boolean difference
a ⊖ b = Diff a b

(⊝) = (⊖)

a ⊕ b = Union [a, b]

infixl 0 ∩

a ∩ b = Intersection [a, b]

(⊛) = (⊕)

-- COMPOSE OPERATIONS

infixl 1 |>

infixl 1 ▷

x |> f = f x

x ▷ f = f x
