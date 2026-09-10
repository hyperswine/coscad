-- | Triangle meshes: ASCII STL in and out, bounds, volume, and the
-- small vector helpers the manufacturing and checking stages share.
module Coscad.Mesh
  ( Tri
  , parseStlAscii
  , stlAscii
  , triNormal
  , triArea
  , meshMap
  , meshBounds
  , meshVolume
  ) where

import Coscad.Geometry (BBox, cross, fromCorners, vlen, vnormed, vsub)

type Tri = ((Double, Double, Double), (Double, Double, Double), (Double, Double, Double))

parseStlAscii :: String -> [Tri]
parseStlAscii s = group3 verts
  where
    verts =
      [ toV (map read (take 3 (drop 1 ws)))
        | l <- lines s
        , let ws = words l
        , take 1 ws == ["vertex"]
      ]
    toV [x, y, z] = (x, y, z)
    toV _ = (0, 0, 0)
    group3 (a : b : c : r) = (a, b, c) : group3 r
    group3 _ = []

triNormal :: Tri -> (Double, Double, Double)
triNormal (a, b, c) = vnormed (cross (vsub b a) (vsub c a))

triArea :: Tri -> Double
triArea (a, b, c) = vlen (cross (vsub b a) (vsub c a)) / 2

meshMap :: ((Double, Double, Double) -> (Double, Double, Double)) -> [Tri] -> [Tri]
meshMap f = map (\(a, b, c) -> (f a, f b, f c))

meshBounds :: [Tri] -> BBox
meshBounds tris = fromCorners [v | (a, b, c) <- tris, v <- [a, b, c]]

-- | Signed-volume sum (absolute), exact for closed meshes.
meshVolume :: [Tri] -> Double
meshVolume = abs . sum . map (\(a, b, c) -> dot a (cross b c) / 6)
  where
    dot (x, y, z) (p, q, r) = x * p + y * q + z * r

stlAscii :: String -> [Tri] -> String
stlAscii name tris =
  "solid " ++ name ++ "\n"
    ++ concatMap facet tris
    ++ ("endsolid " ++ name ++ "\n")
  where
    facet t@(a, b, c) =
      let (nx, ny, nz) = triNormal t
       in "  facet normal " ++ unwords (map show [nx, ny, nz]) ++ "\n"
            ++ "    outer loop\n"
            ++ concatMap vtx [a, b, c]
            ++ "    endloop\n  endfacet\n"
    vtx (x, y, z) = "      vertex " ++ unwords (map show [x, y, z]) ++ "\n"
