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

gen Empty = "union() { }"
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
gen (Loft ps) =
  "skin([" ++ intercalate ", " (map (either error id . pathOf . snd) ps) ++ "], z = ["
    ++ intercalate ", " (map (show . fst) ps) ++ "], slices = 0, method = \"" ++ loftMethod (map snd ps) ++ "\");"
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

-- | A 2D profile as a BOSL2 *path expression* (a list of points), for
-- use inside skin(). Only single closed outlines qualify: the 2D
-- primitives, bezier polygons, and in-plane transforms/offsets of
-- them. Booleans between profiles have no single-outline path.
pathOf :: Shape -> Either String String
pathOf s = case s of
  Shape2D n r -> Right ("circle(r = " ++ show r ++ ", $fn = " ++ show n ++ ")")
  Poly (PD pts []) -> Right ("ccw_polygon(" ++ showPoints pts ++ ")")
  Poly _ -> Left "a polygon with holes cannot be a loft profile"
  Tx d p -> wrap ("move([" ++ show d ++ ", 0], p = ") p
  Ty d p -> wrap ("move([0, " ++ show d ++ "], p = ") p
  Translate (x, y, _) p -> wrap ("move([" ++ show x ++ ", " ++ show y ++ "], p = ") p
  Rz a p -> wrap ("zrot(" ++ show a ++ ", p = ") p
  RotAxis a (0, 0, az) p | az /= 0 -> wrap ("zrot(" ++ show (if az > 0 then a else -a) ++ ", p = ") p
  Scale (sx, sy, _) p -> wrap ("scale([" ++ show sx ++ ", " ++ show sy ++ "], p = ") p
  Mirror (nx, ny, _) p -> wrap ("mirror([" ++ show nx ++ ", " ++ show ny ++ "], p = ") p
  Offset r p -> (\q -> "offset(" ++ q ++ ", r = " ++ show r ++ ", closed = true)") <$> pathOf p
  Tz _ _ -> Left "a loft profile cannot be moved in Z (ζ); give the loft its z value instead"
  Rx _ _ -> Left "a loft profile must stay in the XY plane (no θ rotation)"
  Ry _ _ -> Left "a loft profile must stay in the XY plane (no ϕ rotation)"
  RotAxis {} -> Left "a loft profile must stay in the XY plane (rotate only about Z)"
  Anchor {} -> pathOf (resolve s)
  Position {} -> pathOf (resolve s)
  AttachTo {} -> pathOf (resolve s)
  CutAt {} -> pathOf (resolve s)
  Union _ -> boolErr
  Diff _ _ -> boolErr
  Intersection _ -> boolErr
  Hull _ -> boolErr
  Minkowski _ -> boolErr
  Empty -> Left "an empty shape cannot be a loft profile"
  _ -> Left "a loft profile must be a 2D outline (△ ⬠ ⭘ ✎, Circle/Triangle/Pentagon/Bezier), not a 3D solid"
  where
    wrap pre p = (\q -> pre ++ q ++ ")") <$> pathOf p
    boolErr = Left "a loft profile must be a single closed outline: booleans (⊕ ⊖ ∩ ⇓ ⊞) between profiles are not supported"

-- | Vertex count of a profile path when it is known at compile time
-- (offset may add vertices, so it is unknown).
profileVerts :: Shape -> Maybe Int
profileVerts s = case s of
  Shape2D n _ -> Just n
  Poly (PD pts _) -> Just (length pts)
  Tx _ p -> profileVerts p
  Ty _ p -> profileVerts p
  Translate _ p -> profileVerts p
  Rz _ p -> profileVerts p
  RotAxis _ _ p -> profileVerts p
  Scale _ p -> profileVerts p
  Mirror _ p -> profileVerts p
  Anchor {} -> profileVerts (resolve s)
  Position {} -> profileVerts (resolve s)
  _ -> Nothing

-- | BOSL2 skin() method: "reindex" when every profile has the same
-- known vertex count (cheap, exact correspondence), otherwise
-- "distance", which handles mismatched counts but runs an O(n*m)
-- dynamic program per profile pair in the OpenSCAD interpreter
-- (two 100-gons: ~30 s; a 100-gon and a triangle: instant).
loftMethod :: [Shape] -> String
loftMethod ps = case mapM profileVerts ps of
  Just (n : ns) | all (== n) ns -> "reindex"
  _ -> "distance"

-- | First reason (if any) that a shape tree's lofts cannot be emitted.
loftErrors :: Shape -> Either String ()
loftErrors s = case s of
  Loft ps -> mapM_ (\(_, p) -> either (\e -> Left ("in loft profile: " ++ e)) (const (Right ())) (pathOf p)) ps
  Tx _ x -> loftErrors x
  Ty _ x -> loftErrors x
  Tz _ x -> loftErrors x
  Rx _ x -> loftErrors x
  Ry _ x -> loftErrors x
  Rz _ x -> loftErrors x
  Scale _ x -> loftErrors x
  Mirror _ x -> loftErrors x
  Translate _ x -> loftErrors x
  RotAxis _ _ x -> loftErrors x
  Anchor _ x -> loftErrors x
  Position _ _ a b -> loftErrors a >> loftErrors b
  AttachTo _ _ a b -> loftErrors a >> loftErrors b
  CutAt _ _ a b -> loftErrors a >> loftErrors b
  Extrude _ x -> loftErrors x
  Offset _ x -> loftErrors x
  Diff a b -> loftErrors a >> loftErrors b
  Union xs -> mapM_ loftErrors xs
  Intersection xs -> mapM_ loftErrors xs
  Hull xs -> mapM_ loftErrors xs
  Minkowski xs -> mapM_ loftErrors xs
  _ -> Right ()

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
  Loft _ -> True
  Offset _ x -> usesBosl2 x
  Diff a b -> usesBosl2 a || usesBosl2 b
  Union xs -> any usesBosl2 xs
  Intersection xs -> any usesBosl2 xs
  Hull xs -> any usesBosl2 xs
  Minkowski xs -> any usesBosl2 xs
  _ -> False

-- | Full .scad text for a shape: attachments resolved, BOSL2 include
-- prepended when needed, $fn footer appended.
renderScad :: Shape -> String
renderScad shape = header ++ fn50 (gen shape')
  where
    shape' = resolve shape
    header = if usesBosl2 shape' then "include <BOSL2/std.scad>\n\n" else ""

writeScad :: Shape -> FilePath -> IO ()
writeScad shape filename = writeFile filename (renderScad shape)


fn50 x = x ++ "\n$fn = 50;"
