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
  | Prism I D D
  | Poly PD
  | Shape2D I D
  | Tx D Shape
  | Ty D Shape
  | Tz D Shape
  | Rx D Shape
  | Ry D Shape
  | Rz D Shape
  | Scale (D, D, D) Shape
  | Diff Shape Shape
  | Extrude D Shape
  | Union [Shape]
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

gen (Rectangle x y z) = "cube(" ++ "[" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ "]" ++ ");"
gen (Sphere r) = "sphere(" ++ show r ++ ");"
gen (Shape2D n r) =
  "circle(r = " ++ show r ++ ", $fn = " ++ show n ++ ");"
gen (Cylinder r h) =
  "cylinder(h = " ++ show h ++ ", r = " ++ show r ++ ");"
gen (Cone r h) =
  "cylinder(h = " ++ show h ++ ", r1 = " ++ show r ++ ", r2 = 0);"
gen (Prism n r h) =
  "cylinder(h = " ++ show h ++ ", r = " ++ show r ++ ", $fn = " ++ show n ++ ");"
gen (Poly (PD points paths)) =
  "polygon(points = " ++ showPoints points ++ ", paths = " ++ showPaths paths ++ ");"
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
gen (Extrude h s) =
  "linear_extrude(height = " ++ show h ++ ") {\n" ++ indent (gen s) ++ "}"
gen (Diff a b) =
  "difference() {\n" ++ indent (gen a) ++ indent (gen b) ++ "}"
gen (Union shapes) =
  "union() {\n" ++ concatMap (indent . gen) shapes ++ "}"

writeScad shape filename = writeFile filename (fn50 $ gen shape)

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

χ = Tx

ψ = Ty

ζ = Ty

-- | Rotation glyphs (axis‑specific)
θ = Rx

ϕ = Ry

ω = Rz

-- | Scaling (vector form)
(⬈) = Scale

-- | Linear extrusion (2‑D → 3‑D)
(⮕) = Extrude

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
