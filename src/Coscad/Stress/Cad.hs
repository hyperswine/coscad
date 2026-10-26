-- | Exact rectilinear CAD subset for the experimental Hex8 solver. Every
-- primitive boundary must lie on grid planes; no centre-sampling approximation
-- is accepted for an unresolved feature. World coordinates are millimetres.
module Coscad.Stress.Cad
  ( FaceRegion (..), CadProblem (..), prepareCad, solveCad, parseCadProblem, gridJson, resultJson ) where

import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Coscad.Geometry (resolve, fromCorners, bcorners)
import Coscad.Linalg
import Coscad.Shape
import Coscad.Stress.Experiment
import Coscad.Validate (finite, validateShape)
import Coscad.Json

data FaceRegion = FaceRegion
  { faceNormal :: V3
  , regionBounds :: (V3,V3)
  } deriving (Eq,Show)

data CadProblem = CadProblem
  { cadCells :: (Int,Int,Int)
  , cadYoungMPa :: Double
  , cadPoisson :: Double
  , cadForceN :: V3
  , cadSupport :: FaceRegion
  , cadLoad :: FaceRegion
  } deriving (Eq,Show)

data Solid = Box V3 V3 | Add [Solid] | Sub Solid Solid | Common [Solid]

toSolid :: Shape -> Either String Solid
toSolid = go id
  where
    go f s = case s of
      Rectangle x y z -> box f (0,0,0) (x,y,z)
      Cuboid (x,y,z) 0 0 -> box f (-x/2,-y/2,-z/2) (x/2,y/2,z/2)
      Union [] -> bad "empty union"
      Union xs -> Add <$> mapM (go f) xs
      Diff a b -> Sub <$> go f a <*> go f b
      Intersection [] -> bad "empty intersection"
      Intersection xs -> Common <$> mapM (go f) xs
      Tx d x -> go (f . vadd (d,0,0)) x
      Ty d x -> go (f . vadd (0,d,0)) x
      Tz d x -> go (f . vadd (0,0,d)) x
      Translate v x -> go (f . vadd v) x
      Scale v x -> go (f . vmul v) x
      Mirror v x -> go (f . mApply (mirrorMat v)) x
      Rx a x -> go (f . mApply (rodMat a (1,0,0))) x
      Ry a x -> go (f . mApply (rodMat a (0,1,0))) x
      Rz a x -> go (f . mApply (rodMat a (0,0,1))) x
      RotAxis _ (0,0,0) _ -> bad "zero rotation axis"
      RotAxis a v x -> go (f . mApply (rodMat a v)) x
      Transform3 m v x -> go (f . vadd v . mApply m) x
      Tag _ x -> go f x
      Datum _ _ x -> go f x
      Frame _ _ _ x -> go f x
      _ -> bad ("unsupported "++takeWhile (/=' ') (show s)++"; exact grid currently requires rectangular CSG")
    box f lo hi = do
      let points=map f (bcorners (lo,hi))
          bounds@(a,b)=fromCorners points
          edges=map (\p -> vsub (f p) (f (0,0,0))) [(1,0,0),(0,1,0),(0,0,1)]
          axisAligned v=let scale=maximum (map abs (vec v)) in scale>0 && length (filter (>1e-10*scale) (map abs (vec v)))==1
      unless (all (all finite . vec) points && all axisAligned edges && all (>0) (vec (vsub b a)))
        (bad "transformed box is not axis aligned or has collapsed")
      return (uncurry Box bounds)
    bad s=Left ("FEA CAD: "++s)

boundsOf :: Solid -> (V3,V3)
boundsOf s = case s of
  Box a b -> (a,b)
  Add xs -> fromCorners (concatMap (bcorners . boundsOf) xs)
  Sub a _ -> boundsOf a
  Common xs -> (foldl1 (zipV max) (map (fst . boundsOf) xs),foldl1 (zipV min) (map (snd . boundsOf) xs))

boxes :: Solid -> [(V3,V3)]
boxes s = case s of
  Box a b -> [(a,b)]
  Add xs -> concatMap boxes xs
  Sub a b -> boxes a++boxes b
  Common xs -> concatMap boxes xs

contains :: Solid -> V3 -> Bool
contains s p = case s of
  Box a b -> and (zipWith3 (\lo x hi -> lo<x && x<hi) (vec a) (vec p) (vec b))
  Add xs -> any (`contains` p) xs
  Sub a b -> contains a p && not (contains b p)
  Common xs -> all (`contains` p) xs

prepareCad :: Shape -> CadProblem -> Either String GridProblem
prepareCad shape spec = do
  -- Check unsupported/empty containers before calling the general bbox validator.
  structure shape
  validateShape shape
  solid <- toSolid (resolve shape)
  let (lo,hi)=boundsOf solid
      dims=vsub hi lo
      (nx,ny,nz)=cadCells spec
      counts=[nx,ny,nz]
  unless (all (\n -> n>=1 && n<=128) counts && product (map toInteger counts)<=2048
          && 3*product (map ((+1) . toInteger) counts)<=10000)
    (Left "FEA CAD: grid exceeds limits (128 per axis, 2048 cells, 10000 DOFs)")
  unless (all (\x -> finite x && x>=1e-6 && x<=1e6) (vec dims) && all (\x -> finite x && abs x<=1e6) (vec lo++vec hi))
    (Left "FEA CAD: invalid or empty geometry bounds")
  let h=zipV (/) dims (fromIntegral nx,fromIntegral ny,fromIntegral nz)
      aligned a origin spacing = let q=(a-origin)/spacing in finite q && abs (q-fromInteger (round q))<=1e-8
      onGrid a=and (zipWith3 aligned (vec a) (vec lo) (vec h))
      -- Boundaries outside the domain cannot cut a cell; only test interior planes.
      leafAligned a=and [v<=l || v>=u || aligned v l spacing | (v,l,u,spacing)<-zip4 (vec a) (vec lo) (vec hi) (vec h)]
  unless (all (\(a,b) -> leafAligned a && leafAligned b) (boxes solid))
    (Left "FEA CAD: a feature boundary falls inside a cell; refine/change the grid, geometry is not approximated")
  let add (a,b,c) (i,j,k)=(a+i,b+j,c+k)
      position (i,j,k)=vadd lo (vmul h (fromIntegral i,fromIntegral j,fromIntegral k))
      cells=[q | i<-[0..nx-1],j<-[0..ny-1],k<-[0..nz-1],let q=(i,j,k),contains solid (vadd (position q) (vmul h (0.5,0.5,0.5)))]
      occupied=Set.fromList cells
      faces=[(normal,map (add q) cs) | q<-cells,(normal,dir,cs)<-faceDefs,not (Set.member (add q dir) occupied)]
      select name region = do
        let (a,b)=regionBounds region
            normal=faceNormal region
            ns=vec normal
            axis=[i | (i,x)<-zip [0::Int ..] ns,abs x==1]
            extent=vec (vsub b a)
        unless (all finite (vec a++vec b++ns) && length axis==1 && length (filter (/=0) ns)==1
                && and [if i `elem` axis then d==0 else d>0 | (i,d)<-zip [0..] extent]
                && onGrid a && onGrid b)
          (Left ("FEA CAD: "++name++" must be a nonempty grid-aligned planar region with an axis unit normal"))
        let inside p=and (zipWith3 (\l x u -> x>=l-1e-9 && x<=u+1e-9) (vec a) (vec p) (vec b))
            selected=[qs | (n,qs)<-faces,n==normal,all (inside . position) qs]
        unless (not (null selected)) (Left ("FEA CAD: "++name++" selects no exterior faces"))
        return selected
  supports<-select "support" (cadSupport spec)
  loaded<-select "load" (cadLoad spec)
  let fixed=Set.toList (Set.fromList (concat supports))
      weights=Map.toList (Map.fromListWith (+) [(q,1/fromIntegral (4*length loaded)) | qs<-loaded,q<-qs])
      params=defaultBeam {beamSize=dims,beamCells=cadCells spec,youngMPa=cadYoungMPa spec,poisson=cadPoisson spec,tipForceN=cadForceN spec}
  return (GridProblem params lo cells fixed weights)
  where
    structure s=do
      case s of
        Union [] -> bad
        Intersection [] -> bad
        Hull [] -> bad
        Minkowski [] -> bad
        Loft [] -> bad
        Poly (PD [] _) -> bad
        _ -> Right ()
      mapM_ structure (shapeChildren s)
    bad=Left "FEA CAD: empty shape/profile sequence"

solveCad :: Shape -> CadProblem -> Either String GridResult
solveCad shape spec=prepareCad shape spec >>= solveGrid

faceDefs :: [(V3,(Int,Int,Int),[(Int,Int,Int)])]
faceDefs=
  [ ((-1,0,0),(-1,0,0),[(0,j,k) | j<-[0,1],k<-[0,1]])
  , ((1,0,0),(1,0,0),[(1,j,k) | j<-[0,1],k<-[0,1]])
  , ((0,-1,0),(0,-1,0),[(i,0,k) | i<-[0,1],k<-[0,1]])
  , ((0,1,0),(0,1,0),[(i,1,k) | i<-[0,1],k<-[0,1]])
  , ((0,0,-1),(0,0,-1),[(i,j,0) | i<-[0,1],j<-[0,1]])
  , ((0,0,1),(0,0,1),[(i,j,1) | i<-[0,1],j<-[0,1]]) ]

parseCadProblem :: String -> Either String CadProblem
parseCadProblem input=do
  j<-decode input
  fields ["schema","cells","young_mpa","poisson","force_n","support","load"] j
  schema<-get "schema" j
  unless (schema==JString "coscad.fea-cad.v1") (Left "FEA CAD: unsupported input schema")
  cs<-get "cells" j >>= triple
  let counts=vec cs
  unless (all (\n -> n>=1 && n<=128 && n==fromInteger (round n)) counts) (Left "FEA CAD: cells must be integers in [1,128]")
  let (a,b,c)=cs
  CadProblem (round a,round b,round c) <$> (get "young_mpa" j >>= number) <*> (get "poisson" j >>= number)
    <*> (get "force_n" j >>= triple) <*> (get "support" j >>= face) <*> (get "load" j >>= face)
  where
    face j=do
      fields ["normal","bounds_mm"] j
      n<-get "normal" j >>= triple
      box<-get "bounds_mm" j
      case box of
        JArray [a,b] -> FaceRegion n <$> ((,) <$> triple a <*> triple b)
        _ -> Left "FEA CAD: bounds_mm requires two 3D points"
    number (JNumber n) | finite n = Right n
    number _=Left "FEA CAD: expected a finite number"
    triple (JArray [a,b,c])=(,,) <$> number a <*> number b <*> number c
    triple _=Left "FEA CAD: expected three numbers"
    get key j=maybe (Left ("FEA CAD: missing "++key)) Right (lookupKey key j)
    fields expected (JObject xs)=unless (length xs==length expected && Set.fromList (map fst xs)==Set.fromList expected)
      (Left ("FEA CAD: required fields exactly "++show expected++" (unknown/duplicate keys refused)"))
    fields _ _=Left "FEA CAD: expected an object"

-- Export the discrete mesh and BCs for independent solver comparison/auditing.
gridJson :: GridProblem -> JValue
gridJson g=JObject
  [("origin_mm",v3 (gridOrigin g)),("size_mm",v3 (beamSize p)),("cells",cell (beamCells p))
  ,("active_cells",JArray (map cell (activeCells g))), ("clamped_nodes",JArray (map cell (clampedNodes g)))
  ,("loaded_nodes",JArray [JObject [("node",cell q),("weight",JNumber w)] | (q,w)<-loadedNodes g])
  ,("young_mpa",JNumber (youngMPa p)),("poisson",JNumber (poisson p)),("force_n",v3 (tipForceN p))]
  where p=gridParameters g
        cell (a,b,c)=JArray (map (JNumber . fromIntegral) [a,b,c])

resultJson :: GridResult -> JValue
resultJson r=JObject
  [("load_average_displacement_mm",v3 (tipDisplacementMM s)),("support_reaction_n",v3 (supportReactionN s))
  ,("support_moment_n_mm",v3 (supportMomentNMM s)),("strain_energy_n_mm",JNumber (strainEnergyNMM s))
  ,("max_cell_centre_von_mises_mpa",JNumber (maxCellCentreVonMisesMPa s)),("free_residual_relative",JNumber (freeResidualRelative s))
  ,("iterations",JNumber (fromIntegral (solverIterations s))), ("elements",JNumber (fromIntegral (elementCount s)))
  ,("degrees_of_freedom",JNumber (fromIntegral (degreeCount s)))
  ,("nodes",JArray [JObject [("position_mm",v3 q),("displacement_mm",v3 u)] | (q,u)<-nodeDisplacements r])
  ,("cell_stresses",JArray [JObject [("centre_mm",v3 q),("xx_yy_zz_xy_yz_zx_mpa",JArray (map JNumber sigma))] | (q,sigma)<-cellCentreStresses r])]
  where s=gridSummary r

vec :: V3 -> [Double]
vec (a,b,c)=[a,b,c]
zipV :: (Double -> Double -> Double) -> V3 -> V3 -> V3
zipV f (a,b,c) (x,y,z)=(f a x,f b y,f c z)
v3 :: V3 -> JValue
v3=JArray . map JNumber . vec
zip4 :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4 (a:as) (b:bs) (c:cs) (d:ds)=(a,b,c,d):zip4 as bs cs ds
zip4 _ _ _ _=[]
