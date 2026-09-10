-- | Dimensionality check: every shape is either a 2D profile or a 3D
-- solid, and OpenSCAD silently drops mismatched children (extruding a
-- sphere, offsetting a cube, unioning a circle with a box). This pass
-- turns those into compile errors before any .scad is written.
module Coscad.Dim (Dim (..), dimOf) where

import Coscad.Shape

data Dim = D2 | D3 | DAny deriving (Eq, Show)

-- | Dimension of a shape tree, or a message describing the first
-- 2D/3D mismatch found (innermost first).
dimOf :: Shape -> Either String Dim
dimOf s = case s of
  Empty -> Right DAny
  Shape2D {} -> Right D2
  Poly {} -> Right D2
  Extrude _ x -> do
    d <- dimOf x
    case d of
      D3 -> Left "cannot extrude a 3D solid: ⮕ / extrude needs a 2D profile (△ ⬠ ⭘ ✎, or Circle/Triangle/Pentagon/Bezier)"
      _ -> Right D3
  Loft ps -> do
    ds <- mapM (dimOf . snd) ps
    if length ps < 2
      then Left "loft needs at least two profiles (z profile z profile ...)"
      else
        if D3 `elem` ds
          then Left "loft profiles must be 2D outlines (△ ⬠ ⭘ ✎ ...), not 3D solids"
          else Right D3
  Offset _ x -> do
    d <- dimOf x
    case d of
      D3 -> Left "offset (↯ / Offset) needs a 2D profile, but was given a 3D solid"
      _ -> Right D2
  Tx _ x -> dimOf x
  Ty _ x -> dimOf x
  Tz _ x -> dimOf x
  Rx _ x -> dimOf x
  Ry _ x -> dimOf x
  Rz _ x -> dimOf x
  Scale _ x -> dimOf x
  Mirror _ x -> dimOf x
  Translate _ x -> dimOf x
  RotAxis _ _ x -> dimOf x
  Anchor _ x -> dimOf x
  Position _ _ a b -> same "position (⌖ / at)" [a, b]
  AttachTo _ _ a b -> same "attach (⋈ / on)" [a, b]
  CutAt _ _ a b -> same "cutat" [a, b]
  Diff a b -> same "difference" [a, b]
  Union xs -> same "union" xs
  Intersection xs -> same "intersection" xs
  Hull xs -> same "hull" xs
  Minkowski xs -> same "minkowski" xs
  _ -> Right D3
  where
    same op xs = do
      ds <- mapM dimOf xs
      let concrete = filter (/= DAny) ds
      if D2 `elem` concrete && D3 `elem` concrete
        then Left ("cannot " ++ op ++ " a 2D profile with a 3D solid; extrude the profile first (⮕ h / |> extrude h)")
        else Right (case concrete of (d : _) -> d; [] -> DAny)
