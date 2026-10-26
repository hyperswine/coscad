-- | A bounded, homogeneous, isotropic Hex8 cantilever experiment, in N/mm/MPa.
-- Not a general part stress checker. Full 2x2x2 integration is a baseline:
-- bending/volumetric locking and clamp singularities require refinement.
module Coscad.Stress.Experiment
  ( BeamProblem (..)
  , BeamResult (..)
  , defaultBeam
  , solveBeam
  , GridProblem (..)
  , GridResult (..)
  , solveGrid
  , elementMatrix
  ) where

import Control.Monad (unless)
import Coscad.Stress.Linear
import qualified Data.Array as A
import qualified Data.Array.Unboxed as U
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (foldl')
import Coscad.Linalg (V3, cross, vadd, vsub, vmul, vlen)
import Coscad.Validate (finite)


data BeamProblem = BeamProblem
  { beamSize :: V3
  , beamCells :: (Int,Int,Int)
  , youngMPa :: Double
  , poisson :: Double
  , tipForceN :: V3
  , relativeTolerance :: Double
  , iterationLimit :: Int
  } deriving (Eq, Show)

data BeamResult = BeamResult
  { tipDisplacementMM :: V3 -- traction-weighted end face average
  , supportReactionN :: V3
  , supportMomentNMM :: V3 -- about the beam's (0,0,0) corner
  , strainEnergyNMM :: Double
  , maxCellCentreVonMisesMPa :: Double -- not a surface/peak stress certificate
  , freeResidualRelative :: Double
  , solverIterations :: Int
  , elementCount :: Int
  , degreeCount :: Int
  } deriving (Eq, Show)

defaultBeam :: BeamProblem
defaultBeam = BeamProblem (20,4,4) (20,4,4) 2000 0.3 (0,0,-1) 1e-8 2000

-- | Validated 24x24 Hex8 matrix for element patch/rigid-mode checks and future
-- CAD grids. Nodes are lexicographic (x,y,z) over {0,h}; each has xyz DOFs.
elementMatrix :: V3 -> Double -> Double -> Either String (A.Array (Int,Int) Double)
elementMatrix dims e nu = do
  validate defaultBeam {beamSize=dims,beamCells=(1,1,1),youngMPa=e,poisson=nu}
  let matrix=stiffness e nu dims
  unless (all finite (A.elems matrix)) (Left "FEA: non-finite element stiffness")
  return matrix

-- | Fixed x=0 face; uniformly distributed TOTAL force at x=L. The input
-- cannot express an unconstrained body or silently apply a load to empty air.
solveBeam :: BeamProblem -> Either String BeamResult
solveBeam p = do
  validate p
  let (lx,ly,lz) = beamSize p
      (nx,ny,nz) = beamCells p
      hx = lx/fromIntegral nx; hy = ly/fromIntegral ny; hz = lz/fromIntegral nz
      node i j k = (i*(ny+1)+j)*(nz+1)+k
      n = 3*(nx+1)*(ny+1)*(nz+1)
      fixed = 3*(ny+1)*(nz+1)
      elements = [[3*node (i+a) (j+b) (k+c)+d | (a,b,c) <- corners, d <- [0..2]]
                 | i <- [0..nx-1], j <- [0..ny-1], k <- [0..nz-1]]
      ke = stiffness (youngMPa p) (poisson p) (hx,hy,hz)
      local = [(i,j,ke A.! (i,j)) | i <- [0..23], j <- [0..23]]
      entries = IM.fromListWith (+)
        [(ids A.! i*n + ids A.! j,v)
        | ds <- elements, let ids = A.listArray (0,23) ds, (i,j,v) <- local]
      rows :: Rows
      rows = A.accumArray (flip (:)) [] (0,n-1)
        [(key `div` n,(key `mod` n,v)) | (key,v) <- IM.toList entries]
      diagonal = vector [if i < fixed then 1 else sum [v | (j,v) <- rows A.! i, i==j] | i <- [0..n-1]]
      weight j k = ((if j==0 || j==ny then 1 else 2)*(if k==0 || k==nz then 1 else 2))
                   / fromIntegral (4*ny*nz)
      face = [(node nx j k, weight j k) | j <- [0..ny], k <- [0..nz]]
      rhs = U.accumArray (+) 0 (0,n-1)
        [(3*q+d,w*f) | (q,w) <- face, (d,f) <- zip [0..] (vec (tipForceN p))]
      apply x = vector [if i < fixed then 0 else rowDot row x | (i,row) <- A.assocs rows]
  unless (all (\d -> finite d && d > 0) (U.elems diagonal)
          && all (finite . snd) (IM.toList entries)) (Left "FEA: invalid stiffness; reduce numeric range")
  (u,it,residual) <- pcg (iterationLimit p) (relativeTolerance p) apply diagonal rhs
  let allForces = vector [rowDot row u | row <- A.elems rows]
      reaction = tuple [sum [allForces U.! i - rhs U.! i | i <- [d,d+3..fixed-1]] | d <- [0..2]]
      moment = foldl' vadd (0,0,0)
        [cross (0,fromIntegral j*hy,fromIntegral k*hz)
               (tuple [allForces U.! (3*node 0 j k+d) | d <- [0..2]])
        | j <- [0..ny], k <- [0..nz]]
      tip = tuple [sum [w*(u U.! (3*q+d)) | (q,w) <- face] | d <- [0..2]]
      stresses = [vonMises (cellStress (youngMPa p) (poisson p) (hx,hy,hz) [u U.! i | i <- ids]) | ids <- elements]
      energy = dot u allForces/2
      peak = maximum stresses
  unless (all finite (vec reaction ++ vec moment ++ vec tip ++ [energy,peak,residual])) (Left "FEA: non-finite solution")
  return (BeamResult tip reaction moment energy peak residual it (length elements) n)

validate :: BeamProblem -> Either String ()
validate p = do
  unless (all (\x -> finite x && x >= 1e-6 && x <= 1e6) (vec (beamSize p)))
    (Left "FEA: dimensions must be finite and in [1e-6, 1e6] mm")
  let (a,b,c) = beamCells p
      counts = map toInteger [a,b,c]
  unless (all (\x -> x>=1 && x<=128) counts && product counts <= 2048
          && 3*product (map (+1) counts) <= 10000)
    (Left "FEA: grid needs 1..128 cells per axis, at most 2048 elements and 10000 degrees of freedom")
  unless (finite (youngMPa p) && youngMPa p >= 1e-6 && youngMPa p <= 1e9)
    (Left "FEA: Young's modulus must be finite and in [1e-6, 1e9] MPa")
  unless (finite (poisson p) && poisson p >= 0 && poisson p <= 0.45)
    (Left "FEA: baseline Poisson ratio must be in [0, 0.45]; nearly incompressible materials are unsupported")
  unless (all (\x -> finite x && abs x <= 1e9) (vec (tipForceN p)) && vlen (tipForceN p) > 0)
    (Left "FEA: force must be finite, nonzero, and at most 1e9 N per component")
  unless (finite (relativeTolerance p) && relativeTolerance p >= 1e-12 && relativeTolerance p <= 1e-3)
    (Left "FEA: relative tolerance must be in [1e-12, 1e-3]")
  unless (iterationLimit p >= 1 && iterationLimit p <= 10000)
    (Left "FEA: iteration limit must be in [1, 10000]")

corners :: [(Int,Int,Int)]
corners = [(i,j,k) | i <- [0,1], j <- [0,1], k <- [0,1]]

-- Physical gradients of trilinear shape functions on an axis-aligned cell.
gradients :: V3 -> V3 -> [[Double]]
gradients (hx,hy,hz) (x,y,z) =
  [[a*(1+b*y)*(1+c*z)/(4*hx), b*(1+a*x)*(1+c*z)/(4*hy), c*(1+a*x)*(1+b*y)/(4*hz)]
  | (i,j,k) <- corners, let a=fromIntegral (2*i-1); b=fromIntegral (2*j-1); c=fromIntegral (2*k-1)]

stiffness :: Double -> Double -> V3 -> A.Array (Int,Int) Double
stiffness e nu dims@(hx,hy,hz) = A.array ((0,0),(23,23))
  [((3*a+i,3*b+j),sum [w*(lambda*(g A.! (a,i))*(g A.! (b,j))
                                +mu*(g A.! (a,j))*(g A.! (b,i))
                                +if i==j then mu*sum [g A.! (a,k)*g A.! (b,k) | k <- [0..2]] else 0)
                       | g <- samples])
  | a <- [0..7], b <- [0..7], i <- [0..2], j <- [0..2]]
  where
    lambda = e*nu/((1+nu)*(1-2*nu)); mu=e/(2*(1+nu))
    w=hx*hy*hz/8
    q=1/sqrt 3
    samples = [A.listArray ((0,0),(7,2)) (concat (gradients dims (x,y,z))) | x <- [-q,q], y <- [-q,q], z <- [-q,q]]

cellStress :: Double -> Double -> V3 -> [Double] -> [Double]
cellStress e nu dims values = [normal 0,normal 1,normal 2,shear 0 1,shear 1 2,shear 2 0]
  where
    u,g :: A.Array (Int,Int) Double
    u=A.listArray ((0,0),(7,2)) values
    g=A.listArray ((0,0),(7,2)) (concat (gradients dims (0,0,0)))
    du i j=sum [u A.! (a,i)*g A.! (a,j) | a <- [0..7]]
    tr=sum [du i i | i <- [0..2]]
    lambda=e*nu/((1+nu)*(1-2*nu)); mu=e/(2*(1+nu))
    normal i=lambda*tr+2*mu*du i i
    shear i j=mu*(du i j+du j i)

vonMises :: [Double] -> Double
vonMises [x,y,z,xy,yz,zx] = sqrt ((sq (x-y)+sq (y-z)+sq (z-x))/2+3*(sq xy+sq yz+sq zx))
  where sq a=a*a
vonMises _ = 0 -- internal fixed-length stress tensor

vec :: V3 -> [Double]
vec (x,y,z)=[x,y,z]

tuple :: [Double] -> V3
tuple [x,y,z]=(x,y,z)
tuple _=(0,0,0) -- internal fixed-length vector


-- | A conforming subset of one regular Hex8 grid. One face-connected solid,
-- fully clamped nodes, and nodal forces obtained by integrating surface traction.
-- Coordinates and force weights are validated at this library boundary too.
data GridProblem = GridProblem
  { gridParameters :: BeamProblem
  , gridOrigin :: V3
  , activeCells :: [(Int,Int,Int)]
  , clampedNodes :: [(Int,Int,Int)]
  , loadedNodes :: [((Int,Int,Int),Double)] -- nonnegative weights, sum = 1
  } deriving (Eq,Show)

data GridResult = GridResult
  { gridSummary :: BeamResult
  , nodeDisplacements :: [(V3,V3)]
  , cellCentreStresses :: [(V3,[Double])] -- xx, yy, zz, xy, yz, zx in MPa
  } deriving (Eq,Show)

solveGrid :: GridProblem -> Either String GridResult
solveGrid problem = do
  let p=gridParameters problem
      cells=activeCells problem
      cellSet=Set.fromList cells
      clamps=Set.fromList (clampedNodes problem)
      loads=loadedNodes problem
      (nx,ny,nz)=beamCells p
      (lx,ly,lz)=beamSize p
      h=(lx/fromIntegral nx,ly/fromIntegral ny,lz/fromIntegral nz)
      add (a,b,c) (i,j,k)=(a+i,b+j,c+k)
      nodeSet=Set.fromList [add q c | q <- cells,c <- corners]
      nodes=Set.toAscList nodeSet
      indices=Map.fromList (zip nodes [0..])
      ix q=indices Map.! q -- only called after membership checks below
      position (i,j,k)=vadd (gridOrigin problem) (vmul h (fromIntegral i,fromIntegral j,fromIntegral k))
      validCell (i,j,k)=i>=0 && i<nx && j>=0 && j<ny && k>=0 && k<nz
  validate p
  unless (all (\v -> finite v && abs v<=1e6) (vec (gridOrigin problem))) (Left "FEA: invalid grid origin")
  unless (not (null cells) && all validCell cells && length cells==Set.size cellSet)
    (Left "FEA: active cells must be nonempty, unique and inside the grid")
  let walk seen []=seen
      walk seen (q:todo)
        | Set.member q seen = walk seen todo
        | otherwise = walk (Set.insert q seen)
            ([r | d <- [(1,0,0),(-1,0,0),(0,1,0),(0,-1,0),(0,0,1),(0,0,-1)],let r=add q d,Set.member r cellSet]++todo)
  unless (Set.size (walk Set.empty (take 1 cells)) == length cells)
    (Left "FEA: disconnected solid or edge/point-only connection; analyse separate bodies or model contact explicitly")
  unless (clamps `Set.isSubsetOf` nodeSet) (Left "FEA: support refers to a missing node")
  let fixedPoints=map position (Set.toList clamps)
      noncollinear=case fixedPoints of
        a:rest -> any (\(b,c) -> vlen (cross (vsub b a) (vsub c a)) > 1e-10 * minimum (vec h)^ (2::Int)) [(b,c) | b<-rest,c<-rest]
        _ -> False
  unless noncollinear (Left "FEA: support must clamp at least three noncollinear nodes to remove rigid motion")
  unless (not (null loads) && all (\(q,w) -> Set.member q nodeSet && not (Set.member q clamps) && finite w && w>0) loads
          && abs (sum (map snd loads)-1)<1e-10)
    (Left "FEA: load weights must sum to one on existing, unclamped nodes")
  let n=3*length nodes
      isFixed i=Set.member (nodesArray A.! (i `div` 3)) clamps
      nodesArray=A.listArray (0,length nodes-1) nodes
      elements=[[3*ix (add q c)+d | c <- corners,d <- [0..2]] | q<-cells]
      ke=stiffness (youngMPa p) (poisson p) h
      local=[(i,j,ke A.! (i,j)) | i<-[0..23],j<-[0..23]]
      entries=IM.fromListWith (+) [(ids A.! i*n+ids A.! j,v) | ds<-elements,let ids=A.listArray (0,23) ds,(i,j,v)<-local]
      rows :: Rows
      rows=A.accumArray (flip (:)) [] (0,n-1) [(key `div` n,(key `mod` n,v)) | (key,v)<-IM.toList entries]
      diagonal=vector [if isFixed i then 1 else sum [v | (j,v)<-rows A.! i,i==j] | i<-[0..n-1]]
      rhs=U.accumArray (+) 0 (0,n-1) [(3*ix q+d,w*f) | (q,w)<-loads,(d,f)<-zip [0..] (vec (tipForceN p))]
      apply x=vector [if isFixed i then 0 else rowDot row x | (i,row)<-A.assocs rows]
  unless (all (\d -> finite d && d>0) (U.elems diagonal) && all (finite . snd) (IM.toList entries))
    (Left "FEA: invalid stiffness; reduce numeric range")
  (u,it,residual)<-pcg (iterationLimit p) (relativeTolerance p) apply diagonal rhs
  let allForces=vector [rowDot row u | row<-A.elems rows]
      reactionAt q=tuple [allForces U.! (3*ix q+d)-rhs U.! (3*ix q+d) | d<-[0..2]]
      reactions=[(position q,reactionAt q) | q<-Set.toList clamps]
      reaction=foldl' vadd (0,0,0) (map snd reactions)
      moment=foldl' vadd (0,0,0) [cross q f | (q,f)<-reactions]
      tip=tuple [sum [w*(u U.! (3*ix q+d)) | (q,w)<-loads] | d<-[0..2]]
      stresses=[cellStress (youngMPa p) (poisson p) h [u U.! i | i<-ids] | ids<-elements]
      energy=dot u allForces/2
      peak=maximum (map vonMises stresses)
      values=[(position q,tuple [u U.! (3*ix q+d) | d<-[0..2]]) | q<-nodes]
      centres=[vadd (position q) (vmul h (0.5,0.5,0.5)) | q<-cells]
      summary=BeamResult tip reaction moment energy peak residual it (length cells) n
  unless (all finite (U.elems u++vec reaction++vec moment++vec tip++[energy,peak,residual]++concat stresses))
    (Left "FEA: non-finite solution")
  return (GridResult summary values (zip centres stresses))
