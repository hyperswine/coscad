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

generate (Rectangle x y z) = "cube(" ++ "[" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ "]" ++ ");"
generate (Sphere r) = "sphere(" ++ show r ++ ");"
generate (Shape2D n r) =
  "circle(r = " ++ show r ++ ", $fn = " ++ show n ++ ");"
generate (Cylinder r h) =
  "cylinder(h = " ++ show h ++ ", r = " ++ show r ++ ");"
generate (Cone r h) =
  "cylinder(h = " ++ show h ++ ", r1 = " ++ show r ++ ", r2 = 0);"
generate (Prism n r h) =
  "cylinder(h = " ++ show h ++ ", r = " ++ show r ++ ", $fn = " ++ show n ++ ");"
generate (Poly (PD points paths)) =
  "polygon(points = " ++ showPoints points ++ ", paths = " ++ showPaths paths ++ ");"
generate (Tx dx s) =
  "translate([" ++ show dx ++ ", 0, 0]) {\n" ++ indent (generate s) ++ "}"
generate (Ty dy s) =
  "translate([0, " ++ show dy ++ ", 0]) {\n" ++ indent (generate s) ++ "}"
generate (Tz dz s) =
  "translate([0, 0, " ++ show dz ++ "]) {\n" ++ indent (generate s) ++ "}"
generate (Rx ax s) =
  "rotate([" ++ show ax ++ ", 0, 0]) {\n" ++ indent (generate s) ++ "}"
generate (Ry ay s) =
  "rotate([0, " ++ show ay ++ ", 0]) {\n" ++ indent (generate s) ++ "}"
generate (Rz az s) =
  "rotate([0, 0, " ++ show az ++ "]) {\n" ++ indent (generate s) ++ "}"
generate (Scale (sx, sy, sz) s) =
  "scale([" ++ show sx ++ ", " ++ show sy ++ ", " ++ show sz ++ "]) {\n" ++ indent (generate s) ++ "}"
generate (Extrude h s) =
  "linear_extrude(height = " ++ show h ++ ") {\n" ++ indent (generate s) ++ "}"
generate (Diff a b) =
  "difference() {\n" ++ indent (generate a) ++ indent (generate b) ++ "}"
generate (Union shapes) =
  "union() {\n" ++ concatMap (indent . generate) shapes ++ "}"

-- ACTUAL LANGUAGE
-- Glyph-style aliases

{-
cube = black medium square

-}

-- | cube x
(◼) :: Double -> Shape
(◼) x = Rectangle x x x

-- | sphere r
(⚪) :: Double -> Shape
(⚪) = Sphere

-- | cylinder radius height
(⚈) :: Double -> Double -> Shape
(⚈) = Cylinder

-- | white up pointing triangle
-- | cone radius height
(△) :: Double -> Double -> Shape
(△) = Cone

-- | prism n radius height
prism :: Int -> Double -> Double -> Shape
prism = Prism
