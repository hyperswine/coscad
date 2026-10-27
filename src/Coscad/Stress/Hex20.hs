{-# LANGUAGE BangPatterns #-}
-- | Small-strain isotropic elasticity on a voxel grid with 20-node serendipity
-- hexahedra (quadratic displacement, 3x3x3 Gauss integration). Every cell has
-- the same size, so one element matrix is computed per problem and assembled
-- into one CSR matrix, scaled per cell by the cell's sampled solid fraction so
-- partially filled boundary cells are softer rather than absent or whole.
-- Stresses are recovered with the full modulus, i.e. as material stress.
-- Nodes are vertices and edge midpoints, keyed in doubled grid coordinates:
-- all-even keys are vertices, one odd coordinate an edge.
module Coscad.Stress.Hex20
  ( Key, Hex20Problem (..), Hex20Result (..), CellStress (..)
  , solveHex20, elementStiffness, cellNodes, faceNodes, nodePosition, allNodes
  , naturalOf, shapeGradients, vonMises
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (ST)
import Data.Array.ST (newArray, readArray, writeArray, runSTUArray)
import Data.Array.Unboxed (UArray, (!), listArray, elems, accumArray)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (foldl')
import Coscad.Linalg (V3, vadd, cross, vlen, vsub)
import Coscad.Stress.Sparse
import Coscad.Stress.Voxel
import Coscad.Validate (finite)

type Key = (Int,Int,Int)

data Hex20Problem = Hex20Problem
  { hpGrid :: VoxelGrid
  , hpYoung :: Double
  , hpPoisson :: Double
  , hpClamped :: Set.Set Key          -- ^ nodes fixed in x, y and z
  , hpLoads :: Map.Map Key V3         -- ^ nodal forces in N
  , hpTolerance :: Double
  , hpIterations :: Int
  }

data CellStress = CellStress
  { csCentre :: [Double]        -- ^ xx yy zz xy yz zx at the cell centre, MPa
  , csCentreVonMises :: Double
  , csPeakVonMises :: Double    -- ^ largest von Mises over the 27 Gauss points
  , csPeakTensionZ :: Double    -- ^ largest tensile zz over the Gauss points (across printed layers)
  } deriving (Eq, Show)

data Hex20Result = Hex20Result
  { hrNodes :: [(Key, V3, V3)]            -- ^ key, position, displacement
  , hrCells :: [((Int,Int,Int), V3, CellStress)]
  , hrReaction :: V3
  , hrMoment :: V3                        -- ^ about the world origin
  , hrEnergy :: Double
  , hrIterations :: Int
  , hrResidual :: Double
  , hrDofs :: Int
  }

-- | Natural coordinates of local node n (0..19): corners first, then edges.
naturalOf :: Int -> (Int,Int,Int)
naturalOf n = table !! n
  where table = [(-1,-1,-1),(1,-1,-1),(1,1,-1),(-1,1,-1),(-1,-1,1),(1,-1,1),(1,1,1),(-1,1,1)
                ,(0,-1,-1),(1,0,-1),(0,1,-1),(-1,0,-1),(-1,-1,0),(1,-1,0),(1,1,0),(-1,1,0),(0,-1,1),(1,0,1),(0,1,1),(-1,0,1)]

-- | Keys of a cell's 20 nodes in local order.
cellNodes :: (Int,Int,Int) -> [Key]
cellNodes (i,j,k) = [ (2*i+1+a, 2*j+1+b, 2*k+1+c) | n <- [0..19], let (a,b,c) = naturalOf n ]

allNodes :: VoxelGrid -> Set.Set Key
allNodes g = Set.fromList (concatMap cellNodes (Set.toList (vgCells g)))

nodePosition :: VoxelGrid -> Key -> V3
nodePosition g (a,b,c) = let (ox,oy,oz) = vgOrigin g; h = vgCell g / 2
                         in (ox + h*fromIntegral a, oy + h*fromIntegral b, oz + h*fromIntegral c)

-- | The eight nodes of an exterior face with the consistent weights of a
-- uniform traction on a quadratic quad: corners -1/12, edge midpoints 1/3.
faceNodes :: Face -> [(Key, Double)]
faceNodes (Face cell axis sgn) =
  [ (key, if corner then -1/12 else 1/3)
  | n <- [0..19], let nat@(a,b,c) = naturalOf n, select axis nat == sgn
  , let corner = a /= 0 && b /= 0 && c /= 0
  , let key = cellNodes cell !! n ]
  where select 0 (a,_,_) = a
        select 1 (_,b,_) = b
        select _ (_,_,c) = c

-- | Physical gradients (dN/dx, dN/dy, dN/dz) of the 20 shape functions at a
-- natural point, for a cube of edge h.
shapeGradients :: Double -> (Double,Double,Double) -> [(Double,Double,Double)]
shapeGradients h (x,y,z) = [ scale (grad (naturalOf n)) | n <- [0..19] ]
  where
    s = 2/h
    scale (a,b,c) = (s*a, s*b, s*c)
    grad (ai,bi,ci)
      | ai /= 0 && bi /= 0 && ci /= 0 =
          let a = fromIntegral ai; b = fromIntegral bi; c = fromIntegral ci
              px = 1+a*x; py = 1+b*y; pz = 1+c*z
          in ( a*py*pz*(2*a*x + b*y + c*z - 1)/8
             , b*px*pz*(a*x + 2*b*y + c*z - 1)/8
             , c*px*py*(a*x + b*y + 2*c*z - 1)/8 )
      | ai == 0 = let b = fromIntegral bi; c = fromIntegral ci; py = 1+b*y; pz = 1+c*z
                  in ( -2*x*py*pz/4, (1-x*x)*b*pz/4, (1-x*x)*py*c/4 )
      | bi == 0 = let a = fromIntegral ai; c = fromIntegral ci; px = 1+a*x; pz = 1+c*z
                  in ( a*(1-y*y)*pz/4, -2*y*px*pz/4, px*(1-y*y)*c/4 )
      | otherwise = let a = fromIntegral ai; b = fromIntegral bi; px = 1+a*x; py = 1+b*y
                    in ( a*py*(1-z*z)/4, px*b*(1-z*z)/4, -2*z*px*py/4 )

gaussPoints :: [((Double,Double,Double), Double)]
gaussPoints = [ ((x,y,z), wx*wy*wz) | (x,wx) <- g, (y,wy) <- g, (z,wz) <- g ]
  where q = sqrt 0.6
        g = [(-q, 5/9), (0, 8/9), (q, 5/9)]

-- | The 60x60 element stiffness for edge h, row-major, indexed [(3a+i)*60 + 3b+j].
elementStiffness :: Double -> Double -> Double -> UArray Int Double
elementStiffness e nu h = accumArray (+) 0 (0, 3599)
  [ ((3*a+i)*60 + 3*b+j, w * detJ * (lam * gi a i * gj b j + mu * gi a j * gj b i + (if i == j then mu * dotg a b else 0)))
  | (pt, w) <- gaussPoints
  , let grads = shapeGradients h pt
        garr = listArray (0, 59) (concat [ [gx,gy,gz] | (gx,gy,gz) <- grads ]) :: UArray Int Double
        gi n d = garr ! (3*n+d)
        gj = gi
        dotg p q = sum [ garr!(3*p+d) * garr!(3*q+d) | d <- [0..2] ]
  , a <- [0..19], b <- [0..19], i <- [0..2], j <- [0..2] ]
  where
    lam = e*nu/((1+nu)*(1-2*nu)); mu = e/(2*(1+nu))
    detJ = (h/2)^(3::Int)

vonMises :: [Double] -> Double
vonMises [x,y,z,xy,yz,zx] = sqrt (((x-y)^two + (y-z)^two + (z-x)^two)/2 + 3*(xy^two + yz^two + zx^two)) where two = 2 :: Int
vonMises _ = 0

solveHex20 :: Hex20Problem -> Either String Hex20Result
solveHex20 problem = do
  let g = hpGrid problem; e = hpYoung problem; nu = hpPoisson problem; h = vgCell g
      cells = Set.toList (vgCells g)
  unless (finite e && e > 0 && e <= 1e9) (Left "FEA: Young's modulus must be positive and at most 1e9 MPa")
  unless (finite nu && nu >= 0 && nu <= 0.45) (Left "FEA: Poisson ratio must be in [0, 0.45]; nearly incompressible materials are unsupported")
  unless (not (null cells)) (Left "FEA: no cells")
  unless (length (connectedCells g) == 1) (Left ("FEA: the voxel mesh has " ++ show (length (connectedCells g)) ++ " separate bodies (features joined only diagonally or through thin walls below the cell size); use a smaller cell or analyse the bodies separately"))
  let nodeSet = allNodes g
      nodeList = Set.toAscList nodeSet
      index = Map.fromList (zip nodeList [0..])
      nodeCount = Map.size index
      nd = 3*nodeCount
      positions = listArray (0, 3*nodeCount-1) (concat [ let (x,y,z) = nodePosition g key in [x,y,z] | key <- nodeList ]) :: UArray Int Double
      pos n = (positions!(3*n), positions!(3*n+1), positions!(3*n+2))
      clamped = hpClamped problem
  unless (clamped `Set.isSubsetOf` nodeSet) (Left "FEA: a support refers to a node outside the mesh")
  unless (Set.size clamped >= 3) (Left "FEA: supports must hold at least three nodes")
  let fixedPts = map (nodePosition g) (Set.toList clamped)
      noncollinear = case fixedPts of
        (a:rest) -> any (\(b,c) -> vlen (cross (vsub b a) (vsub c a)) > 1e-9 * h*h) [(b,c) | b <- rest, c <- rest]
        _ -> False
  unless noncollinear (Left "FEA: supports clamp only collinear nodes, which leaves a free rotation; support a face or a second feature")
  let loads = hpLoads problem
  unless (all (`Set.member` nodeSet) (Map.keys loads)) (Left "FEA: a load refers to a node outside the mesh")
  -- Load on a clamped node goes straight into the support; it still counts in
  -- the reaction. Only a load that is entirely on clamped nodes is refused.
  unless (all (\(x,y,z) -> all finite [x,y,z]) (Map.elems loads)) (Left "FEA: nonfinite load")
  unless (any (\(k, v) -> Set.notMember k clamped && vlen v > 0) (Map.toList loads))
    (Left "FEA: every loaded node is clamped, so the load goes straight into the support; move the load or the support")
  let ke = elementStiffness e nu h
      fillOf c = Map.findWithDefault 1 c (vgFill g)
      cellGlobals = [ (c, map (index Map.!) (cellNodes c)) | c <- cells ]
      neighbours = IM.fromListWith IS.union [ (a, IS.fromList gs) | (_, gs) <- cellGlobals, a <- gs ]
      rowCols n = [ 3*m + j | m <- IS.toAscList (neighbours IM.! n), j <- [0..2] ]
      rowLengths = [ length (rowCols n) | n <- [0..nodeCount-1] ]
      ptrList = scanl (+) 0 (concatMap (replicate 3) rowLengths)
      ptr = listArray (0, nd) ptrList :: UArray Int Int
      nnz = last ptrList
      col = listArray (0, nnz-1) (concat [ rowCols n | n <- [0..nodeCount-1], _ <- [0::Int,1,2] ]) :: UArray Int Int
      val = runSTUArray $ do
        arr <- newArray (0, nnz-1) 0
        forM_ cellGlobals $ \(c, gs) -> do
          let garr = listArray (0,19) gs :: UArray Int Int
              fraction = fillOf c
          forM_ [0..19] $ \a -> forM_ [0..19] $ \b -> forM_ [0..2] $ \i -> forM_ [0..2] $ \j -> do
            let row = 3*(garr!a) + i; column = 3*(garr!b) + j
                v = fraction * ke ! ((3*a+i)*60 + 3*b+j)
            when (v /= 0) $ do
              k <- find ptr col row column
              old <- readArray arr k
              writeArray arr k (old + v)
        return arr
      matrix = CSR nd ptr col val
      freeMask = listArray (0, nd-1) [ not (Set.member key clamped) | key <- nodeList, _ <- [0::Int,1,2] ] :: UArray Int Bool
      rhs = vecFromList (concat [ maybe [0,0,0] (\(x,y,z) -> [x,y,z]) (Map.lookup key loads) | key <- nodeList ])
  (u, iterations, residual) <- pcgSolve (hpIterations problem) (hpTolerance problem) matrix freeMask rhs
  let ku = csrApply matrix u
      reactionOf n = (ku!(3*n) - rhs!(3*n), ku!(3*n+1) - rhs!(3*n+1), ku!(3*n+2) - rhs!(3*n+2))
      clampedIdx = [ index Map.! key | key <- Set.toList clamped ]
      reaction = foldl' vadd (0,0,0) (map reactionOf clampedIdx)
      moment = foldl' vadd (0,0,0) [ cross (pos n) (reactionOf n) | n <- clampedIdx ]
      energy = vecDot u ku / 2
      disp n = (u!(3*n), u!(3*n+1), u!(3*n+2))
      lam = e*nu/((1+nu)*(1-2*nu)); mu = e/(2*(1+nu))
      stressAt gs pt =
        let grads = shapeGradients h pt
            du :: Int -> Int -> Double
            du i j = sum [ component (disp n) i * component gr j | (n, gr) <- zip gs grads ]
            tr = du 0 0 + du 1 1 + du 2 2
            normal i = lam*tr + 2*mu*du i i
            shear i j = mu*(du i j + du j i)
        in [normal 0, normal 1, normal 2, shear 0 1, shear 1 2, shear 2 0]
      cellResult (c, gs) =
        let centre = stressAt gs (0,0,0)
            samples = [ stressAt gs pt | (pt,_) <- gaussPoints ]
        in (c, cellCentre g c, CellStress centre (vonMises centre) (maximum (map vonMises samples)) (maximum (map (!! 2) samples)))
      cellResults = map cellResult cellGlobals
      nodes = [ (key, pos n, disp n) | (key, n) <- zip nodeList [0..] ]
  unless (all finite (elems u) && all finite (concat [ csCentre s | (_,_,s) <- cellResults ])) (Left "FEA: nonfinite result")
  return (Hex20Result nodes cellResults reaction moment energy iterations residual nd)
  where
    component :: V3 -> Int -> Double
    component (x,_,_) 0 = x
    component (_,y,_) 1 = y
    component (_,_,z) _ = z

-- | Position of column c within row r of a CSR structure (binary search).
find :: UArray Int Int -> UArray Int Int -> Int -> Int -> ST s Int
find ptr col r c = go (ptr!r) (ptr!(r+1) - 1)
  where
    go lo hi
      | lo > hi = error "FEA: internal CSR structure mismatch"
      | otherwise = let mid = (lo+hi) `div` 2; v = col!mid
                    in if v == c then return mid else if v < c then go (mid+1) hi else go lo (mid-1)
