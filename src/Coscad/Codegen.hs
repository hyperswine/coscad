-- | OpenSCAD emission: 'gen' renders a resolved Shape tree,
-- 'writeScad' resolves attachments first and prepends the BOSL2
-- include when any BOSL2 primitive is present.
module Coscad.Codegen (module Coscad.Codegen) where

import Coscad.Geometry
import Coscad.IO (writeFileUtf8)
import Coscad.Shape
import Data.List (intercalate)

-- | Numbers as OpenSCAD reads them: rounded to 1e-9 (rotation noise
-- like 3.06e-16 becomes 0), integers without a trailing ".0", no "-0".
showD :: Double -> String
showD x
  | isNaN x || isInfinite x = show x
  | otherwise =
      let r = fromIntegral (round (x * 1e9) :: Integer) / 1e9 :: Double
          r' = if r == 0 then 0 else r
          i = round r' :: Integer
       in if fromIntegral i == r' then show i else show r'

-- | Helper function for polygon conversion
showPoints points = "[" ++ intercalate ", " (map showPoint points) ++ "]"
  where
    showPoint (x, y) = "[" ++ showD x ++ ", " ++ showD y ++ "]"

-- | Helper function for polygon conversion
showPaths paths = "[" ++ intercalate ", " (map showPath paths) ++ "]"
  where
    showPath path = "[" ++ intercalate ", " (map showD path) ++ "]"

indent = unlines . map ("  " ++) . lines

-- | Emit optional BOSL2 chamfer/rounding arguments
boslMod ch ro =
  (if ch /= 0 then ", chamfer = " ++ showD ch else "")
    ++ (if ro /= 0 then ", rounding = " ++ showD ro else "")

gen Empty = "union() { }"
gen (Hidden _) = gen Empty
gen (Rectangle x y z) = "cube(" ++ "[" ++ showD x ++ ", " ++ showD y ++ ", " ++ showD z ++ "]" ++ ");"
gen (Sphere r) = "sphere(" ++ showD r ++ ");"
gen (Shape2D n r) =
  "circle(r = " ++ showD r ++ ", $fn = " ++ show n ++ ");"
gen (Cylinder r h) =
  "cylinder(h = " ++ showD h ++ ", r = " ++ showD r ++ ");"
gen (Cone r h) =
  "cylinder(h = " ++ showD h ++ ", r1 = " ++ showD r ++ ", r2 = 0);"
gen (Frustum h r1 r2) =
  "cylinder(h = " ++ showD h ++ ", r1 = " ++ showD r1 ++ ", r2 = " ++ showD r2 ++ ");"
gen (Prism n r h) =
  "cylinder(h = " ++ showD h ++ ", r = " ++ showD r ++ ", $fn = " ++ show n ++ ");"
gen (Poly (PD points [])) =
  "polygon(points = " ++ showPoints points ++ ");"
gen (Poly (PD points paths)) =
  "polygon(points = " ++ showPoints points ++ ", paths = " ++ showPaths paths ++ ");"
-- BOSL2 primitives (all centered at origin, BOSL2 default anchoring)
gen (Cuboid (x, y, z) ch ro) =
  "cuboid([" ++ showD x ++ ", " ++ showD y ++ ", " ++ showD z ++ "]" ++ boslMod ch ro ++ ");"
gen (Cyl r h ch ro) =
  "cyl(r = " ++ showD r ++ ", h = " ++ showD h ++ boslMod ch ro ++ ");"
gen (XCyl r l) =
  "xcyl(r = " ++ showD r ++ ", l = " ++ showD l ++ ");"
gen (YCyl r l) =
  "ycyl(r = " ++ showD r ++ ", l = " ++ showD l ++ ");"
gen (ZCyl r l) =
  "zcyl(r = " ++ showD r ++ ", l = " ++ showD l ++ ");"
gen (Tube ro ri h) =
  "tube(h = " ++ showD h ++ ", or = " ++ showD ro ++ ", ir = " ++ showD ri ++ ");"
gen (Prismoid (x1, y1) (x2, y2) h) =
  "prismoid(size1 = [" ++ showD x1 ++ ", " ++ showD y1 ++ "], size2 = [" ++ showD x2 ++ ", " ++ showD y2 ++ "], h = " ++ showD h ++ ", anchor = CENTER);"
gen (Torus rj rn) =
  "torus(r_maj = " ++ showD rj ++ ", r_min = " ++ showD rn ++ ");"
gen (Wedge (x, y, z)) =
  "wedge([" ++ showD x ++ ", " ++ showD y ++ ", " ++ showD z ++ "], anchor = CENTER);"
-- Transforms
gen (Tx dx s) =
  "translate([" ++ showD dx ++ ", 0, 0]) {\n" ++ indent (gen s) ++ "}"
gen (Ty dy s) =
  "translate([0, " ++ showD dy ++ ", 0]) {\n" ++ indent (gen s) ++ "}"
gen (Tz dz s) =
  "translate([0, 0, " ++ showD dz ++ "]) {\n" ++ indent (gen s) ++ "}"
gen (Rx ax s) =
  "rotate([" ++ showD ax ++ ", 0, 0]) {\n" ++ indent (gen s) ++ "}"
gen (Ry ay s) =
  "rotate([0, " ++ showD ay ++ ", 0]) {\n" ++ indent (gen s) ++ "}"
gen (Rz az s) =
  "rotate([0, 0, " ++ showD az ++ "]) {\n" ++ indent (gen s) ++ "}"
gen (Scale (sx, sy, sz) s) =
  "scale([" ++ showD sx ++ ", " ++ showD sy ++ ", " ++ showD sz ++ "]) {\n" ++ indent (gen s) ++ "}"
gen (Mirror (mx, my, mz) s) =
  "mirror([" ++ showD mx ++ ", " ++ showD my ++ ", " ++ showD mz ++ "]) {\n" ++ indent (gen s) ++ "}"
-- identity transforms (from attachment desugaring) are elided
gen (Translate (0, 0, 0) s) = gen s
gen (RotAxis 0 _ s) = gen s
gen (Translate (x, y, z) s) =
  "translate([" ++ showD x ++ ", " ++ showD y ++ ", " ++ showD z ++ "]) {\n" ++ indent (gen s) ++ "}"
gen (RotAxis a (x, y, z) s) =
  "rotate(a = " ++ showD a ++ ", v = [" ++ showD x ++ ", " ++ showD y ++ ", " ++ showD z ++ "]) {\n" ++ indent (gen s) ++ "}"
-- attachment sugar should be resolved before codegen; desugar defensively
gen s@(Anchor {}) = gen (resolve s)
gen s@(Position {}) = gen (resolve s)
gen s@(AttachTo {}) = gen (resolve s)
gen s@(CutAt {}) = gen (resolve s)
gen (Extrude h s) =
  "linear_extrude(height = " ++ showD h ++ ") {\n" ++ indent (gen s) ++ "}"
gen (Loft ps) =
  "skin([" ++ intercalate ", " (map (either error id . pathOf . snd) ps) ++ "], z = ["
    ++ intercalate ", " (map (showD . fst) ps) ++ "], slices = 0, method = \"" ++ loftMethod (map snd ps) ++ "\");"
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
  "offset(r = " ++ showD r ++ ") {\n" ++ indent (gen s) ++ "}"

-- | A 2D profile as a BOSL2 *path expression* (a list of points), for
-- use inside skin(). Only single closed outlines qualify: the 2D
-- primitives, bezier polygons, and in-plane transforms/offsets of
-- them. Booleans between profiles have no single-outline path.
pathOf :: Shape -> Either String String
pathOf s = case s of
  Shape2D n r -> Right ("circle(r = " ++ showD r ++ ", $fn = " ++ show n ++ ")")
  Poly (PD pts []) -> Right ("ccw_polygon(" ++ showPoints pts ++ ")")
  Poly _ -> Left "a polygon with holes cannot be a loft profile"
  Tx d p -> wrap ("move([" ++ showD d ++ ", 0], p = ") p
  Ty d p -> wrap ("move([0, " ++ showD d ++ "], p = ") p
  Translate (_, _, z) _ | z /= 0 -> Left "a loft profile cannot be moved in Z; give the loft its z value instead"
  Translate (x, y, _) p -> wrap ("move([" ++ showD x ++ ", " ++ showD y ++ "], p = ") p
  Rz a p -> wrap ("zrot(" ++ showD a ++ ", p = ") p
  RotAxis a (0, 0, az) p | az /= 0 -> wrap ("zrot(" ++ showD (if az > 0 then a else -a) ++ ", p = ") p
  Scale (sx, sy, _) p -> wrap ("scale([" ++ showD sx ++ ", " ++ showD sy ++ "], p = ") p
  Mirror (0, 0, nz) p | nz /= 0 -> pathOf p -- reflection in XY leaves a 2D path unchanged
  Mirror (_, _, nz) _ | nz /= 0 -> Left "a loft profile must stay in the XY plane (mirror normal must be in XY or along Z)"
  Mirror (0, 0, 0) _ -> Left "a loft profile mirror needs a nonzero normal"
  Mirror (nx, ny, _) p -> wrap ("mirror([" ++ showD nx ++ ", " ++ showD ny ++ "], p = ") p
  Offset r p -> (\q -> "offset(" ++ q ++ ", r = " ++ showD r ++ ", closed = true)") <$> pathOf p
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
writeScad shape filename = writeFileUtf8 filename (renderScad shape)


fn50 x = x ++ "\n$fn = 50;"
