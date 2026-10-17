-- | Mesh-level manufacturing diagnostics. These are conservative warnings,
-- not a layer-by-layer support proof or a prediction of physical adhesion.
module Coscad.Printability (Printability (..), analysePrintability) where

import Coscad.Codegen (showD)
import Coscad.Mesh (Tri, meshBounds, triArea, triNormal)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort)

data Printability = Printability
  { bedContactMm2 :: Double
  , elevatedStartsMm :: [Double]
  , printabilityWarnings :: [String]
  } deriving (Show)

analysePrintability :: [Tri] -> Printability
analysePrintability tris = Printability contact starts warnings
  where
    ((x0,y0,z0),(x1,y1,z1)) = meshBounds tris
    eps = 1e-5
    verts (a,b,c) = [a,b,c]
    z (_,_,h) = h
    downward t = let (_,_,nz) = triNormal t in nz < -0.1
    -- Actual coplanar surface area, rather than the bounding rectangle or
    -- sloped triangles within 0.3 mm of it. A rounded bottom can have zero
    -- flat area while still acquiring contact in the sliced first layer.
    contact = sum [triArea t | t <- tris, downward t,
                   all (\p -> abs (z p-z0) <= eps) (verts t)]
    adjacency = Map.fromListWith Set.union
      [(p, Set.fromList [q,r]) | t <- tris,
       (p,q,r) <- case t of (a,b,c) -> [(a,b,c),(b,a,c),(c,a,b)]]
    below = Set.fromList [p | t <- tris, downward t, p <- verts t]
    neighbours p = Map.findWithDefault Set.empty p adjacency
    plateau _ seen [] = seen
    plateau h seen (p:ps)
      | Set.member p seen = plateau h seen ps
      | otherwise = plateau h (Set.insert p seen)
          (Set.toList (Set.filter (\q -> abs (z q-h) <= eps) (neighbours p)) ++ ps)
    minima _ [] = []
    minima seen (p:ps)
      | Set.member p seen = minima seen ps
      | otherwise =
          let flat = plateau (z p) Set.empty [p]
              around = Set.unions (map neighbours (Set.toList flat))
              low = z p-z0 > 0.001
                    && all (\q -> z q >= z p-eps) (Set.toList around)
                    && not (Set.null (Set.intersection below flat))
           in [z p-z0 | low] ++ minima (Set.union seen flat) ps
    -- A downward-facing local minimum above the bed indicates material
    -- starting away from the bed (e.g. the tesseract's raised collars).
    -- Other overhangs and bridges still need the slicer's support analysis.
    starts = foldr (\h rest -> case rest of
                        r:_ | abs (h-r) <= eps -> rest
                        _ -> h:rest) [] (sort (minima Set.empty (Map.keys adjacency)))
    slender = (z1-z0) / max 1 (min (x1-x0) (y1-y0))
    warnings =
      ["only " ++ showD contact ++ " mm^2 of flat bed contact; check the first layer and use a brim or a broader print face"
       | contact < 25]
      ++ ["tall, narrow print (height/width " ++ showD slender ++ "); use a brim and conservative acceleration"
          | slender > 2.5]
      ++ ["elevated downward-facing start at " ++ showD h ++ " mm above the bed; inspect supports before printing"
          | h <- starts]
