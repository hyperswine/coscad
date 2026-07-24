-- | OpenSCAD emission: 'gen' renders a resolved Shape tree,
-- 'writeScad' resolves attachments first and prepends the BOSL2
-- include when any BOSL2 primitive is present.
module Coscad.Codegen (module Coscad.Codegen) where

import Coscad.Geometry
import Coscad.Shape
import Data.List (intercalate)

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


fn50 x = x ++ "\n$fn = 50;"
