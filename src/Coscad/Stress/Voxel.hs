{-# LANGUAGE BangPatterns #-}
-- | Voxel occupancy of a closed triangle mesh on a regular grid, by ray parity
-- along grid columns. The grid is what the part stress solver meshes; it is a
-- staircase approximation whose volume is reported against the mesh volume.
module Coscad.Stress.Voxel
  ( VoxelGrid (..), fillThreshold, voxelize, voxelCount, voxelVolume, cellCentre, cellCorner
  , exteriorFaces, Face (..), connectedCells
  ) where

import Control.Monad (unless)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort, foldl')
import Coscad.Linalg (V3)
import Coscad.Mesh (Tri, meshBounds)
import Coscad.Validate (finite)

data VoxelGrid = VoxelGrid
  { vgOrigin :: V3            -- ^ world position of grid corner (0,0,0)
  , vgCell :: Double          -- ^ cube edge, mm
  , vgCounts :: (Int,Int,Int) -- ^ cells per axis
  , vgCells :: Set.Set (Int,Int,Int) -- ^ occupied cells (fill fraction at least 'fillThreshold')
  , vgFill :: Map.Map (Int,Int,Int) Double -- ^ sampled solid fraction of each occupied cell, 27 samples
  } deriving (Eq, Show)

-- | Cells with less than this sampled solid fraction are treated as empty.
fillThreshold :: Double
fillThreshold = 0.2

-- | An exterior cell face: the cell, the outward axis (0,1,2) and sign (+1/-1).
data Face = Face { faceCell :: (Int,Int,Int), faceAxis :: Int, faceSign :: Int } deriving (Eq, Ord, Show)

voxelCount :: VoxelGrid -> Int
voxelCount = Set.size . vgCells

voxelVolume :: VoxelGrid -> Double
voxelVolume g = sum (Map.elems (vgFill g)) * vgCell g ^ (3::Int)

cellCorner :: VoxelGrid -> (Int,Int,Int) -> V3
cellCorner g (i,j,k) = let (ox,oy,oz)=vgOrigin g; h=vgCell g in (ox+h*fromIntegral i, oy+h*fromIntegral j, oz+h*fromIntegral k)

cellCentre :: VoxelGrid -> (Int,Int,Int) -> V3
cellCentre g c = let (x,y,z)=cellCorner g c; r=vgCell g/2 in (x+r,y+r,z+r)

-- | Voxelize with cube cells of edge h. Each cell is sampled at 3x3x3 points;
-- a sample is solid when it lies between an odd and an even surface crossing
-- of the +z ray through it. Cells with at least 'fillThreshold' solid samples
-- are kept, with their fraction, so a wall that is not a whole number of cells
-- thick is represented by a partially filled cell rather than rounded away.
-- Rays that cross the surface an odd number of times mean the mesh is not
-- closed and are an error.
voxelize :: Double -> [Tri] -> Either String VoxelGrid
voxelize h tris = do
  unless (finite h && h > 0) (Left "FEA: cell size must be a positive finite number")
  unless (not (null tris)) (Left "FEA: empty mesh")
  let ((x0,y0,z0),(x1,y1,z1)) = meshBounds tris
      count a b = max 1 (ceiling ((b-a)/h - 1e-9))
      nx = count x0 x1; ny = count y0 y1; nz = count z0 z1
  unless (all (\n -> n <= 4096) [nx,ny,nz]) (Left "FEA: more than 4096 cells along one axis; use a larger cell")
  unless (toInteger nx*toInteger ny*toInteger nz <= 50000000) (Left "FEA: grid exceeds fifty million candidate cells; use a larger cell")
  -- Sub-sample positions 1/6, 1/2, 5/6 of a cell, jittered so rays never pass
  -- exactly through triangle edges or vertices of axis-aligned geometry.
  let jx = h*1.3e-7; jy = h*2.9e-7
      sub = [1/6, 1/2, 5/6] :: [Double]
      sampleX i a = x0 + h*(fromIntegral i + a) + jx
      sampleY j b = y0 + h*(fromIntegral j + b) + jy
      key i j = i*ny + j
      buckets = IM.fromListWith (++)
        [ (key i j, [t])
        | t@((ax,ay,_),(bx,by,_),(cx,cy,_)) <- tris
        , let lo a b c = minimum [a,b,c]; hi a b c = maximum [a,b,c]
        , i <- [max 0 (floor ((lo ax bx cx - jx - x0)/h - 1)) .. min (nx-1) (ceiling ((hi ax bx cx - jx - x0)/h))]
        , j <- [max 0 (floor ((lo ay by cy - jy - y0)/h - 1)) .. min (ny-1) (ceiling ((hi ay by cy - jy - y0)/h))] ]
      crossing px py ((ax,ay,az),(bx,by,bz),(cx,cy,cz)) =
        let e0 = (bx-ax)*(py-ay) - (by-ay)*(px-ax)
            e1 = (cx-bx)*(py-by) - (cy-by)*(px-bx)
            e2 = (ax-cx)*(py-cy) - (ay-cy)*(px-cx)
            area2 = (bx-ax)*(cy-ay) - (by-ay)*(cx-ax)
        in if area2 == 0 || not ((e0 >= 0 && e1 >= 0 && e2 >= 0) || (e0 <= 0 && e1 <= 0 && e2 <= 0)) then Nothing
           else let u = e1/area2; v = e2/area2; w = e0/area2
                in Just (u*az + v*bz + w*cz)
      -- one ray: the number of solid samples it contributes to each cell k
      ray i j a b = do
        let zs = sort [z | t <- IM.findWithDefault [] (key i j) buckets, Just z <- [crossing (sampleX i a) (sampleY j b) t]]
        unless (even (length zs)) (Left ("FEA: mesh is not closed (odd surface crossings at column " ++ show (i,j) ++ "); render a watertight part"))
        let pairs = pairUp zs
            inside zc = any (\(lo',hi') -> zc > lo' && zc < hi') pairs
        return [ ((i,j,k), 1 :: Int) | not (null pairs), k <- [0..nz-1], c <- sub, inside (z0 + h*(fromIntegral k + c)) ]
      pairUp (a:b:rest) = (a,b) : pairUp rest
      pairUp _ = []
  samples <- mapM (\(i,j,a,b) -> ray i j a b) [(i,j,a,b) | i <- [0..nx-1], j <- [0..ny-1], a <- sub, b <- sub]
  let counts = Map.fromListWith (+) (concat samples)
      fill = Map.filter (>= fillThreshold) (Map.map (\n -> fromIntegral n / 27) counts)
  unless (not (Map.null fill)) (Left ("FEA: no cell is at least " ++ show (round (100*fillThreshold) :: Int) ++ "% inside the part at cell " ++ show h ++ " mm; use a smaller cell"))
  _ <- Right (x1,y1,z1)
  return (VoxelGrid (x0,y0,z0) h (nx,ny,nz) (Map.keysSet fill) fill)

-- | Faces of occupied cells whose neighbour across the face is empty.
exteriorFaces :: VoxelGrid -> [Face]
exteriorFaces g =
  [ Face c axis sgn
  | c@(i,j,k) <- Set.toList (vgCells g)
  , (axis, sgn, n) <- [ (0,-1,(i-1,j,k)), (0,1,(i+1,j,k)), (1,-1,(i,j-1,k)), (1,1,(i,j+1,k)), (2,-1,(i,j,k-1)), (2,1,(i,j,k+1)) ]
  , not (Set.member n (vgCells g)) ]

-- | Face-connected components of the occupied cells, largest first.
connectedCells :: VoxelGrid -> [Set.Set (Int,Int,Int)]
connectedCells g = reverse (sortOn' Set.size (go (vgCells g)))
  where
    go remaining
      | Set.null remaining = []
      | otherwise =
          let start = Set.findMin remaining
              comp = walk Set.empty [start]
          in comp : go (Set.difference remaining comp)
    walk !seen [] = seen
    walk !seen (c@(i,j,k):todo)
      | Set.member c seen = walk seen todo
      | otherwise = walk (Set.insert c seen)
          ([n | n <- [(i-1,j,k),(i+1,j,k),(i,j-1,k),(i,j+1,k),(i,j,k-1),(i,j,k+1)], Set.member n (vgCells g), not (Set.member n seen)] ++ todo)
    sortOn' f = map snd . sort' . map (\x -> (f x, x))
    sort' = foldl' ins []
    ins [] p = [p]
    ins (q:qs) p | fst p <= fst q = p:q:qs
                 | otherwise = q : ins qs p
