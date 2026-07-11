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
  | Diff Shape Shape
  | Extrude D Shape
  | Union [Shape]
  | Hull [Shape]
  | Minkowski [Shape]
  | Offset D Shape
  deriving (Show)

-- | Polygon Data
data PD = PD [(D, D)] [[D]] deriving (Show)

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
  "prismoid(size1 = [" ++ show x1 ++ ", " ++ show y1 ++ "], size2 = [" ++ show x2 ++ ", " ++ show y2 ++ "], h = " ++ show h ++ ");"
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
gen (Extrude h s) =
  "linear_extrude(height = " ++ show h ++ ") {\n" ++ indent (gen s) ++ "}"
gen (Diff a b) =
  "difference() {\n" ++ indent (gen a) ++ indent (gen b) ++ "}"
gen (Union shapes) =
  "union() {\n" ++ concatMap (indent . gen) shapes ++ "}"
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
  Extrude _ x -> usesBosl2 x
  Offset _ x -> usesBosl2 x
  Diff a b -> usesBosl2 a || usesBosl2 b
  Union xs -> any usesBosl2 xs
  Hull xs -> any usesBosl2 xs
  Minkowski xs -> any usesBosl2 xs
  _ -> False

writeScad shape filename = writeFile filename (header ++ fn50 (gen shape))
  where
    header = if usesBosl2 shape then "include <BOSL2/std.scad>\n\n" else ""

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

(⊛) = (⊕)

-- COMPOSE OPERATIONS

infixl 1 |>

infixl 1 ▷

x |> f = f x

x ▷ f = f x
