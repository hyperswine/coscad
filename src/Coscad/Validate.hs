-- | Front-end validation shared by shape and assembly parsers. No IO/rendering.
module Coscad.Validate (finite, readFinite, suggestion, maxQuantity, maxSides, validateShape) where

import Control.Monad (unless, when)
import Coscad.Dim (Dim (D2), dimOf)
import Coscad.Geometry (bbox, rigidFrameBasis)
import Coscad.Shape
import Data.List (sortOn)

finite :: Double -> Bool
finite x = not (isNaN x || isInfinite x)

readFinite :: String -> Maybe Double
readFinite raw = case reads (if take 1 raw == "." then '0' : raw else raw) of
  [(n, "")] | finite n -> Just n
  _ -> Nothing

-- Explicit front-end resource limits; also keep recursive arithmetic bounded.
maxQuantity, maxSides :: Int
maxQuantity = 10000
maxSides = 4096

-- Suggest only a unique close spelling. Never repair or change the input.
suggestion :: [String] -> String -> String
suggestion choices raw
  | length raw > 64 = ""
  | otherwise = case sortOn fst [(distance raw s, s) | s <- choices, s /= raw] of
      (n, s) : rest | n <= (if length raw > 4 then 2 else 1), all ((> n) . fst) rest -> "; did you mean '" ++ s ++ "'?"
      _ -> ""
  where
    distance a b = last (foldl row [0 .. length b] (zip [1 ..] a))
      where
        row prev (i, x) = scanl cell i (zip3 b prev (drop 1 prev))
          where cell left (y, diagonal, up) = minimum [left + 1, up + 1, diagonal + if x == y then 0 else 1]

validateShape :: Shape -> Either String ()
validateShape s = do
  check s
  let ((a, b, c), (d, e, f)) = bbox s
  unless (all finite [a, b, c, d, e, f]) $
    Left "geometry bounds overflowed; reduce dimensions, scale factors, or offsets"
  where
    pos name xs = unless (all (\v -> finite v && v > 0) xs) (Left (name ++ " must be finite and greater than zero"))
    nonneg name xs = unless (all (\v -> finite v && v >= 0) xs) (Left (name ++ " must be finite and non-negative"))
    nums name xs = unless (all finite xs) (Left (name ++ " must be finite"))
    vec (a, b, c) = [a, b, c]
    rows (a, b, c) = [a, b, c]
    sides n = unless (n >= 3 && n <= maxSides) (Left ("profile/prism side count must be a whole number from 3 to " ++ show maxSides))
    edge name limit value = do
      nonneg name [value]
      when (value > limit) (Left (name ++ " is too large; maximum for these dimensions is " ++ show limit ++ " mm"))
    check sh = case sh of
      Empty -> Right ()
      Hidden x -> check x
      Rectangle x y z -> pos "box width, depth and height" [x, y, z]
      Sphere r -> pos "sphere radius" [r]
      Cylinder r h -> pos "cylinder radius and height" [r, h]
      Cone r h -> pos "cone radius and height" [r, h]
      Frustum h r1 r2 -> pos "frustum height" [h] >> nonneg "frustum radii" [r1, r2] >> pos "at least one frustum radius" [max r1 r2]
      Prism n r h -> sides n >> pos "prism radius and height" [r, h]
      Shape2D n r -> sides n >> pos "profile radius" [r]
      Poly (PD points paths) -> nums "polygon coordinates/paths" (concatMap (\(x, y) -> [x, y]) points ++ concat paths)
      Cuboid dims ch ro -> do
        pos "box width, depth and height" (vec dims)
        let limit = minimum (vec dims) / 2
        edge "cuboid chamfer" limit ch
        edge "cuboid rounding" limit ro
      Cyl r h ch ro -> do
        pos "cylinder radius and height" [r, h]
        edge "cylinder chamfer" (min r (h / 2)) ch
        edge "cylinder rounding" (min r (h / 2)) ro
      XCyl r h -> pos "xcyl radius and length" [r, h]
      YCyl r h -> pos "ycyl radius and length" [r, h]
      ZCyl r h -> pos "zcyl radius and length" [r, h]
      Tube ro ri h -> do
        pos "tube outer radius and height" [ro, h]
        nonneg "tube inner radius" [ri]
        when (ri >= ro) (Left "tube inner radius must be smaller than its outer radius")
      Prismoid (a, b) (c, d) h -> nonneg "prismoid sizes" [a, b, c, d] >> pos "prismoid footprint and height" [max a c, max b d, h]
      Torus rj rn -> pos "torus major and minor radii" [rj, rn]
      Wedge dims -> pos "wedge width, depth and height" (vec dims)
      Scale dims x -> do
        nums "scale factors" (vec dims)
        let relevant = case dimOf x of Right D2 -> take 2 (vec dims); _ -> vec dims
        when (0 `elem` relevant) (Left "scale factors must be nonzero on the shape's axes; negative factors are valid reflections")
        check x
      Mirror normal x -> do
        nums "mirror normal" (vec normal)
        when (all (== 0) (vec normal)) (Left "mirror needs a nonzero normal")
        check x
      Loft ps -> do
        nums "loft heights" (map fst ps)
        unless (and (zipWith (<) (map fst ps) (drop 1 (map fst ps)))) (Left "loft heights must be strictly increasing; give each profile a higher Z than the previous one")
        mapM_ (check . snd) ps
      Tx v x -> nums "translation" [v] >> check x
      Ty v x -> nums "translation" [v] >> check x
      Tz v x -> nums "translation" [v] >> check x
      Rx v x -> nums "rotation" [v] >> check x
      Ry v x -> nums "rotation" [v] >> check x
      Rz v x -> nums "rotation" [v] >> check x
      Translate v x -> nums "translation" (vec v) >> check x
      RotAxis a v x -> nums "rotation" (a : vec v) >> check x
      Transform3 m t x -> do
        nums "rigid transform" (concatMap vec (rows m) ++ vec t)
        b <- rigidFrameBasis m
        when (maximum (map abs (zipWith (-) (concatMap vec (rows m)) (concatMap vec (rows b)))) > 1e-8) (Left "mate transform must be rigid (unit axes, no scale)")
        check x
      Anchor _ v x -> nums "anchor" (vec v) >> check x
      Tag _ x -> check x
      Datum _ p x -> nums "datum point" (vec p) >> check x
      Frame _ p b x -> nums "frame" (vec p ++ concatMap vec (rows b)) >> rigidFrameBasis b >> check x
      Position _ v off a b -> nums "position" (vec v ++ vec off) >> check a >> check b
      AttachTo _ v off a b -> nums "attachment" (vec v ++ vec off) >> check a >> check b
      CutAt _ v off a b -> nums "cutat" (vec v ++ vec off) >> check a >> check b
      Diff a b -> check a >> check b
      Extrude h x -> pos "extrusion height" [h] >> check x
      Offset r x -> nums "offset" [r] >> check x
      Union xs -> mapM_ check xs
      Intersection xs -> mapM_ check xs
      Hull xs -> mapM_ check xs
      Minkowski xs -> mapM_ check xs
