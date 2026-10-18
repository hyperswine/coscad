-- | The CoScad shape AST. Pure data: no geometry, no codegen.
module Coscad.Shape
  ( D
  , I
  , Matrix3
  , Shape (..)
  , PD (..)
  , bezPoly
  , shapeChildren
  )
where

type D = Double

type I = Int

type Matrix3 = ((D, D, D), (D, D, D), (D, D, D)) -- rows

data Shape
  = Empty -- renders nothing
  | Hidden Shape -- checker-only: retain geometry for anchors, emit no solid
  | Rectangle D D D
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
  | Transform3 Matrix3 (D, D, D) Shape -- rigid rotation and translation
  | -- Attachment sugar (desugared by 'resolve' before codegen) ----
    Anchor (D, D, D) Shape -- re-origin shape at its own anchor point
  | Position (D, D, D) (D, D, D) Shape Shape -- anchor, offset, parent, child: snap child at anchor+offset
  | AttachTo (D, D, D) (D, D, D) Shape Shape -- anchor, offset, parent, child: rotate-mate at anchor+offset
  | CutAt (D, D, D) (D, D, D) Shape Shape -- anchor, offset, parent, cutter: SUBTRACT cutter centered there
  | Diff Shape Shape
  | Extrude D Shape
  | Loft [(D, Shape)] -- (z, 2D profile) pairs, skinned bottom to top (BOSL2 skin)
  | Tag String Shape -- named subtree (an assembly part instance); identity for geometry
  | Datum String (D, D, D) Shape -- named construction point in this subtree's coordinates
  | Frame String (D, D, D) Matrix3 Shape -- origin and right-handed local axes
  | Union [Shape]
  | Intersection [Shape]
  | Hull [Shape]
  | Minkowski [Shape]
  | Offset D Shape
  deriving (Show)

-- | Direct children, including non-rendering metadata wrappers.
shapeChildren :: Shape -> [Shape]
shapeChildren s = case s of
  Hidden x -> [x]
  Tx _ x -> [x]
  Ty _ x -> [x]
  Tz _ x -> [x]
  Rx _ x -> [x]
  Ry _ x -> [x]
  Rz _ x -> [x]
  Scale _ x -> [x]
  Mirror _ x -> [x]
  Translate _ x -> [x]
  RotAxis _ _ x -> [x]
  Transform3 _ _ x -> [x]
  Anchor _ x -> [x]
  Position _ _ a b -> [a, b]
  AttachTo _ _ a b -> [a, b]
  CutAt _ _ a b -> [a, b]
  Diff a b -> [a, b]
  Extrude _ x -> [x]
  Loft ps -> map snd ps
  Tag _ x -> [x]
  Datum _ _ x -> [x]
  Frame _ _ _ x -> [x]
  Union xs -> xs
  Intersection xs -> xs
  Hull xs -> xs
  Minkowski xs -> xs
  Offset _ x -> [x]
  _ -> []

-- | Polygon Data
data PD = PD [(D, D)] [[D]] deriving (Show)


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

