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

import Coscad.Geometry (BBox, fromCorners)
import Coscad.Linalg (cross, dot3, vlen, vnormed, vsub)
import Coscad.Validate (readFinite)
import Data.Char (isSpace)
import Data.List (isPrefixOf)

type Tri = ((Double, Double, Double), (Double, Double, Double), (Double, Double, Double))

-- | Parse an ASCII STL. Left explains a binary file (OpenSCAD ignored or
-- was not given `--export-format asciistl`), a vertex that is not a finite
-- number (CGAL can emit nan/inf after a degenerate boolean), or a facet
-- with the wrong vertex count.
parseStlAscii :: String -> Either String [Tri]
parseStlAscii s
  | hasBinaryByte || not ("solid" `isPrefixOf` dropWhile isSpace s) =
      Left "STL is not ASCII (binary or truncated): OpenSCAD must be 2021.01 or newer and a COSCAD_OPENSCAD wrapper must pass its arguments through"
  | otherwise = do
      verts <- mapM vertexOf (zip [1 :: Int ..] (lines s))
      let vs = concat verts
      if length vs `mod` 3 /= 0
        then Left ("STL has " ++ show (length vs) ++ " vertices, not a multiple of three")
        else Right (group3 vs)
  where
    hasBinaryByte = any (\c -> c < ' ' && c `notElem` "\n\r\t") (take 4096 s)
    vertexOf (n, l) = case words l of
      ("vertex" : ws) -> case mapM readFinite ws of
        Just [x, y, z] -> Right [(x, y, z)]
        _ -> Left ("STL line " ++ show n ++ ": vertex is not three finite numbers: " ++ l)
      _ -> Right []
    group3 (a : b : c : r) = (a, b, c) : group3 r
    group3 _ = []

triNormal :: Tri -> (Double, Double, Double)
triNormal (a, b, c) = vnormed (cross (vsub b a) (vsub c a))

triArea :: Tri -> Double
triArea (a, b, c) = vlen (cross (vsub b a) (vsub c a)) / 2

meshMap :: ((Double, Double, Double) -> (Double, Double, Double)) -> [Tri] -> [Tri]
meshMap f = map (\(a, b, c) -> (f a, f b, f c))

-- | Bounds of a mesh; the empty mesh is a point at the origin.
meshBounds :: [Tri] -> BBox
meshBounds [] = ((0, 0, 0), (0, 0, 0))
meshBounds tris = fromCorners [v | (a, b, c) <- tris, v <- [a, b, c]]

-- | Signed-volume sum (absolute), exact for closed meshes.
meshVolume :: [Tri] -> Double
meshVolume = abs . sum . map (\(a, b, c) -> dot3 a (cross b c) / 6)

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
