-- | Experimental Tet4/Tet10 elasticity on a conforming affine volume mesh.
-- Uses the same residual-checked sparse PCG as Hex8. Boundary conditions select
-- complete exterior triangles; curved fixtures retain their actual mesh surface.
module Coscad.Stress.Tetra
  ( TetraProblem (..), parseTetraProblem, solveTetra, solveTetraQuadratic, tetraElementMatrix, quadraticTetraElementMatrix ) where

import Control.Monad (unless)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as U
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (foldl',sort)
import Coscad.Json
import Coscad.Linalg
import Coscad.Mesh (triArea, triNormal)
import Coscad.Stress.Cad (CadProblem (..),FaceRegion (..),parseCadProblem)
import Coscad.Stress.Experiment (BeamResult (..),GridResult (..),elementMatrix)
import Coscad.Stress.Linear
import Coscad.Validate (finite)

data TetraProblem = TetraProblem
  { tetraNodes :: [V3]
  , tetraElements :: [[Int]]
  , tetraConditions :: CadProblem -- only material/force/regions; grid counts unused
  } deriving (Eq,Show)

-- Positive orientation and normalized determinant are checked before inversion.
elementData :: Double -> Double -> [V3] -> Either String (Double,[V3],A.Array (Int,Int) Double)
elementData = elementDataWith False

elementDataWith :: Bool -> Double -> Double -> [V3] -> Either String (Double,[V3],A.Array (Int,Int) Double)
elementDataWith quadratic e nu [p0,p1,p2,p3] = do
  let a=vsub p1 p0; b=vsub p2 p0; c=vsub p3 p0
      det=dot3 a (cross b c)
      scale=maximum (map vlen [a,b,c,vsub p2 p1,vsub p3 p1,vsub p3 p2])
  unless (all finite (concatMap vec [p0,p1,p2,p3]) && finite det && finite scale && scale>0 && det>1e-10*scale^(3::Int))
    (Left "FEA tetra: inverted, degenerate or numerically unresolved element")
  let grads123=map (mul (1/det)) [cross b c,cross c a,cross a b]
      grads=mul (-1) (foldl' vadd (0,0,0) grads123):grads123
      nodesPerElement=if quadratic then 10 else 4
      gradient bary
        | not quadratic = grads
        | otherwise = zipWith (\l g -> mul (4*l-1) g) bary grads
            ++ [mul 4 (vadd (mul (bary!!i) (grads!!j)) (mul (bary!!j) (grads!!i))) | (i,j)<-edgePairs]
      at bary=A.listArray ((0,0),(nodesPerElement-1,2)) (concatMap vec (gradient bary))
      alpha=(5+3*sqrt 5)/20; beta=(5-sqrt 5)/20
      samples=if quadratic then [at [if i==j then alpha else beta | i<-[0::Int ..3]] | j<-[0..3]] else [at (replicate 4 0.25)]
      volume=det/6
      lambda=e*nu/((1+nu)*(1-2*nu)); mu=e/(2*(1+nu))
      k=A.array ((0,0),(3*nodesPerElement-1,3*nodesPerElement-1))
        [((3*i+d,3*j+q),volume/fromIntegral (length samples)*sum
          [lambda*(g A.! (i,d))*(g A.! (j,q)) +mu*(g A.! (i,q))*(g A.! (j,d))
           +if d==q then mu*sum [g A.! (i,l)*g A.! (j,l) | l<-[0..2]] else 0 | g<-samples])
        | i<-[0..nodesPerElement-1],j<-[0..nodesPerElement-1],d<-[0..2],q<-[0..2]]
  unless (all finite (A.elems k)) (Left "FEA tetra: stiffness overflow")
  return (volume,gradient (replicate 4 0.25),k)
elementDataWith _ _ _ _ = Left "FEA tetra: an element must have exactly four nodes"

tetraElementMatrix :: Double -> Double -> [V3] -> Either String (A.Array (Int,Int) Double)
tetraElementMatrix e nu points = do
  _<-elementMatrix (1,1,1) e nu
  (_,_,k)<-elementData e nu points
  return k

solveTetra :: TetraProblem -> Either String GridResult
solveTetra = solveWith False

quadraticTetraElementMatrix :: Double -> Double -> [V3] -> Either String (A.Array (Int,Int) Double)
quadraticTetraElementMatrix e nu points = do
  _<-elementMatrix (1,1,1) e nu
  (_,_,k)<-elementDataWith True e nu points
  return k

solveTetraQuadratic :: TetraProblem -> Either String GridResult
solveTetraQuadratic = solveWith True

edgePairs :: [(Int,Int)]
edgePairs=[(0,1),(0,2),(0,3),(1,2),(1,3),(2,3)]

solveWith :: Bool -> TetraProblem -> Either String GridResult
solveWith quadratic problem = do
  let points=tetraNodes problem; cells=tetraElements problem; spec=tetraConditions problem
      n=length points; ne=length cells
      p=A.listArray (0,n-1) points
      e=cadYoungMPa spec; nu=cadPoisson spec; force=cadForceN spec
  unless (n>=4 && n<=15000 && ne>=1 && ne<=30000) (Left "FEA tetra: mesh requires 4..15000 nodes and 1..30000 elements")
  unless (all (\v -> finite v && abs v<=1e6) (concatMap vec points) && Set.size (Set.fromList points)==n)
    (Left "FEA tetra: duplicate, nonfinite or out-of-range nodes")
  unless (all (\ids -> length ids==4 && Set.size (Set.fromList ids)==4 && all (\i -> i>=0 && i<n) ids) cells
          && Set.size (Set.fromList (map sort cells))==ne
          && Set.size (Set.fromList (concat cells))==n)
    (Left "FEA tetra: invalid/duplicate element indices or unused nodes")
  _<-elementMatrix (1,1,1) e nu
  unless (all (\f -> finite f && abs f<=1e9) (vec force) && vlen force>0) (Left "FEA tetra: invalid total force")
  let edges=if quadratic then Set.toAscList (Set.fromList [sort [qs!!i,qs!!j] | qs<-cells,(i,j)<-edgePairs]) else []
      edgeIndices=Map.fromList (zip edges [n..])
      midpoint a b=edgeIndices Map.! sort [a,b]
      analysisPoints=points++[mul 0.5 (vadd (p A.! a) (p A.! b)) | [a,b]<-edges]
      analysisCount=length analysisPoints
      nd=3*analysisCount
      positions=A.listArray (0,analysisCount-1) analysisPoints
      fullCells=if quadratic then [qs++[midpoint (qs!!i) (qs!!j) | (i,j)<-edgePairs] | qs<-cells] else cells
      elementDofs=if quadratic then 30 else 12
  unless (analysisCount<=20000 && (not quadratic || ne<=10000))
    (Left "FEA Tet10: elevated mesh exceeds 20000 nodes or 10000 elements")
  dataRows<-mapM (elementDataWith quadratic e nu . map (p A.!)) cells
  let faceRows=Map.fromListWith (++) [(sort face,[(q,face)]) | (q,ids)<-zip [0..] cells,face<-faces ids]
  unless (all (\rs -> length rs==1 || (length rs==2 && sum [faceSign f | (_,f)<-rs]==0)) (Map.elems faceRows))
    (Left "FEA tetra: nonmanifold or inconsistently oriented shared faces")
  let adjacency=IM.fromListWith (++) (concat [[(a,[b]),(b,[a])] | [(a,_),(b,_)]<-Map.elems faceRows])
      walk visited []=visited
      walk visited (x:xs)
        | Set.member x visited=walk visited xs
        | otherwise=walk (Set.insert x visited) (IM.findWithDefault [] x adjacency++xs)
  unless (Set.size (walk Set.empty [0])==ne) (Left "FEA tetra: disconnected bodies or edge/point contact require explicit joint/contact modeling")
  let boundary=[f | [(_,f)]<-Map.elems faceRows]
      triangle ids=case map (p A.!) ids of [a,b,c]->(a,b,c); _->((0,0,0),(0,0,0),(0,0,0))
      select name region=do
        let normal=faceNormal region; (lo,hi)=regionBounds region
            normals=vec normal; axis=[i | (i,x)<-zip [0::Int ..] normals,abs x==1]
            extents=vec (vsub hi lo)
        unless (all finite (normals++vec lo++vec hi) && length axis==1 && length (filter (/=0) normals)==1
                && and [if i `elem` axis then x==0 else x>0 | (i,x)<-zip [0..] extents])
          (Left ("FEA tetra: "++name++" must be an axis-aligned planar rectangle with a unit outward normal"))
        let eps=1e-7
            inside q=and (zipWith3 (\a x b -> x>=a-eps && x<=b+eps) (vec lo) (vec q) (vec hi))
            samePlane f=all (\q -> abs (dot3 normal (vsub (p A.! q) lo))<=eps) f
            aligned f=dot3 normal (triNormal (triangle f))>=1-1e-8
            candidates=[f | f<-boundary,samePlane f,aligned f]
            selected=[f | f<-candidates,all (inside . (p A.!)) f]
            overlaps f=let coords=map (vec . (p A.!)) f
                       in and [maximum [q!!i | q<-coords]>a+eps && minimum [q!!i | q<-coords]<b-eps
                              | (i,(a,b))<-zip [0..] (zip (vec lo) (vec hi)),i `notElem` axis]
        unless (all (\f -> not (overlaps f) || all (inside . (p A.!)) f) candidates)
          (Left ("FEA tetra: "++name++" cuts a boundary triangle; choose a whole face/region or remesh its boundary"))
        unless (not (null selected)) (Left ("FEA tetra: "++name++" selects no exterior triangles"))
        return selected
  support<-select "support" (cadSupport spec)
  loaded<-select "load" (cadLoad spec)
  let faceMids f=[midpoint (f!!i) (f!!j) | (i,j)<-[(0,1),(0,2),(1,2)]]
      fixed=Set.fromList (concat [if quadratic then f++faceMids f else f | f<-support])
      area=sum [triArea (triangle f) | f<-loaded]
      weights=IM.fromListWith (+) [(q,triArea (triangle f)/(3*area)) | f<-loaded,q<-(if quadratic then faceMids f else f)]
  unless (finite area && area>0 && all (`Set.notMember` fixed) (IM.keys weights))
    (Left "FEA tetra: zero load area or load overlaps the clamped support")
  let rhs=U.accumArray (+) 0 (0,nd-1) [(3*q+d,w*f) | (q,w)<-IM.toList weights,(d,f)<-zip [0..] (vec force)]
      entries=IM.fromListWith (+)
        [(ids A.! i*nd+ids A.! j,k A.! (i,j))
        | (qs,(_,_,k))<-zip fullCells dataRows,let ids=A.listArray (0,elementDofs-1) [3*q+d | q<-qs,d<-[0..2]],i<-[0..elementDofs-1],j<-[0..elementDofs-1]]
      rows :: Rows
      rows=A.accumArray (flip (:)) [] (0,nd-1) [(key `div` nd,(key `mod` nd,v)) | (key,v)<-IM.toList entries]
      constrained i=Set.member (i `div` 3) fixed
      diagonal=vector [if constrained i then 1 else sum [v | (j,v)<-rows A.! i,i==j] | i<-[0..nd-1]]
      apply u=vector [if constrained i then 0 else rowDot row u | (i,row)<-A.assocs rows]
  unless (all (\x -> finite x && x>0) (U.elems diagonal) && all (finite . snd) (IM.toList entries))
    (Left "FEA tetra: invalid assembled stiffness")
  (u,it,residual)<-pcg 8000 1e-8 apply diagonal rhs
  let allForces=vector [rowDot row u | row<-A.elems rows]
      displacement q=tuple [u U.! (3*q+d) | d<-[0..2]]
      reaction q=tuple [allForces U.! (3*q+d)-rhs U.! (3*q+d) | d<-[0..2]]
      totalReaction=foldl' vadd (0,0,0) [reaction q | q<-Set.toList fixed]
      moment=foldl' vadd (0,0,0) [cross (positions A.! q) (reaction q) | q<-Set.toList fixed]
      meanLoadDisplacement=foldl' vadd (0,0,0) [mul w (displacement q) | (q,w)<-IM.toList weights]
      stresses=[stress e nu grads (map displacement qs) | (qs,(_,grads,_))<-zip fullCells dataRows]
      energy=dot u allForces/2
      peak=maximum (map vonMises stresses)
      centres=[mul 0.25 (foldl' vadd (0,0,0) (map (p A.!) qs)) | qs<-cells]
      summary=BeamResult meanLoadDisplacement totalReaction moment energy peak residual it ne nd
  unless (all finite (U.elems u++vec totalReaction++vec moment++vec meanLoadDisplacement++concat stresses++[energy,peak,residual]))
    (Left "FEA tetra: nonfinite result")
  return (GridResult summary [(positions A.! q,displacement q) | q<-[0..analysisCount-1]] (zip centres stresses))

stress :: Double -> Double -> [V3] -> [V3] -> [Double]
stress e nu grads values=[normal 0,normal 1,normal 2,shear 0 1,shear 1 2,shear 2 0]
  where
    g,u :: A.Array (Int,Int) Double
    g=A.listArray ((0,0),(length grads-1,2)) (concatMap vec grads)
    u=A.listArray ((0,0),(length values-1,2)) (concatMap vec values)
    du i j=sum [u A.! (a,i)*g A.! (a,j) | a<-[0..length grads-1]]
    trace=sum [du i i | i<-[0..2]]
    lambda=e*nu/((1+nu)*(1-2*nu)); mu=e/(2*(1+nu))
    normal i=lambda*trace+2*mu*du i i
    shear i j=mu*(du i j+du j i)

faces :: [Int] -> [[Int]]
faces [a,b,c,d]=[[b,c,d],[a,d,c],[a,b,d],[a,c,b]]
faces _=[] -- checked before calling
faceSign :: [Int] -> Int
faceSign [a,b,c] | (a<b && b<c)||(b<c && c<a)||(c<a && a<b) = 1
faceSign _ = -1

parseTetraProblem :: String -> String -> Either String TetraProblem
parseTetraProblem meshText conditionsText = do
  mesh<-decode meshText
  case mesh of
    JObject xs -> unless (length xs==4 && Set.fromList (map fst xs)==Set.fromList ["schema","nodes_mm","tetrahedra","provenance"])
      (Left "FEA tetra: mesh requires exactly schema, nodes_mm, tetrahedra and provenance fields")
    _ -> Left "FEA tetra: expected mesh object"
  schema<-get "schema" mesh
  unless (schema==JString "coscad.fea-tet-mesh.v1") (Left "FEA tetra: unsupported mesh schema")
  nodeValues<-get "nodes_mm" mesh >>= array
  tetValues<-get "tetrahedra" mesh >>= array
  unless (length nodeValues>=4 && length nodeValues<=15000 && not (null tetValues) && length tetValues<=30000)
    (Left "FEA tetra: mesh exceeds node/element limits or is empty")
  nodes<-mapM point nodeValues
  tets<-mapM (array >=> mapM index) tetValues
  conditions<-decode conditionsText
  pairs<-case conditions of JObject xs -> Right xs; _ -> Left "FEA tetra: expected conditions object"
  unless (lookup "schema" pairs==Just (JString "coscad.fea-tet-loads.v1")) (Left "FEA tetra: unsupported conditions schema")
  spec<-parseCadProblem (encode (JObject (("cells",JArray (replicate 3 (JNumber 1))):[(key,if key=="schema" then JString "coscad.fea-cad.v1" else value) | (key,value)<-pairs])))
  return (TetraProblem nodes tets spec)
  where
    get key j=maybe (Left ("FEA tetra: missing "++key)) Right (lookupKey key j)
    array (JArray xs)=Right xs
    array _=Left "FEA tetra: expected array"
    number (JNumber x) | finite x=Right x
    number _=Left "FEA tetra: expected finite number"
    point (JArray [a,b,c])=(,,) <$> number a <*> number b <*> number c
    point _=Left "FEA tetra: expected 3D node"
    index j=do
      x<-number j
      unless (x>=0 && x<15000 && x==fromInteger (round x)) (Left "FEA tetra: invalid node index")
      return (round x)
    (>=>) f g x=f x >>= g

vec :: V3 -> [Double]
vec (a,b,c)=[a,b,c]
mul :: Double -> V3 -> V3
mul a (x,y,z)=(a*x,a*y,a*z)
tuple :: [Double] -> V3
tuple [x,y,z]=(x,y,z)
tuple _=(0,0,0)
vonMises :: [Double] -> Double
vonMises [x,y,z,xy,yz,zx]=sqrt (((x-y)^two+(y-z)^two+(z-x)^two)/2+3*(xy^two+yz^two+zx^two)) where two=2::Int
vonMises _=0
