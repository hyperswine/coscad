-- | Explicit experimental bridge from physical assembly instances to bonded Hex20.
module Coscad.Stress.Assembly (assemblyProblem, processAssemblyStress, processAssemblyRegions, processAssemblyRefinement) where
import Coscad.Assembly.Instances
import Coscad.Instance
import Coscad.Backend (nativeCSGMesh)
import Coscad.Geometry (V3, vsub, mApply)
import Coscad.Mesh (Tri, meshMap, meshBounds, meshVolume)
import Coscad.Json
import Coscad.IO (writeFileUtf8Atomic)
import Coscad.Report
import Coscad.Stress.Hex20 (csCentre)
import Coscad.Stress.Bonded
import Coscad.Stress.Voxel
import Coscad.Validate (finite)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad (unless, forM)
import System.FilePath (dropExtension)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as BS
import qualified Crypto.Hash.SHA256 as SHA
import Numeric (showHex)
import GHC.Clock (getMonotonicTimeNSec)

check :: Bool -> String -> Either String ()
check ok msg = unless ok (Left ("assembly FEA: " ++ msg))
vec :: V3 -> [Double]
vec (x,y,z)=[x,y,z]
number :: JValue -> Either String Double
number (JNumber x) | finite x = Right x
number _ = Left "assembly FEA: expected finite number"
string :: JValue -> Either String String
string (JString s) = Right s
string _ = Left "assembly FEA: expected string"
array :: JValue -> Either String [JValue]
array (JArray xs) = Right xs
array _ = Left "assembly FEA: expected array"
field :: String -> JValue -> Either String JValue
field k v = maybe (Left ("assembly FEA: missing " ++ k)) Right (lookupKey k v)
object :: [String] -> JValue -> Either String ()
object keys (JObject xs) = check (all ((`elem` keys).fst) xs && length xs == Set.size (Set.fromList (map fst xs))) "unknown or duplicate field"
object _ _ = Left "assembly FEA: expected object"
triple :: JValue -> Either String V3
triple (JArray [x,y,z]) = (,,) <$> number x <*> number y <*> number z
triple _ = Left "assembly FEA: expected three finite coordinates"

-- Grid-exact orthogonal boundaries only. Sampling alone cannot prove a thin
-- void or a sloping surface is represented by fully filled cells.
exactGrid :: Double -> [Tri] -> Either String VoxelGrid
exactGrid h tris = do
  let vertices=concat [[a,b,c] | (a,b,c)<-tris]
      aligned x=finite x && abs (x/h)<=1e7 && abs (x/h-fromInteger (round (x/h)))<1e-7
      axisFace (a,b,c)=any (\k -> abs ((vec a!!k)-(vec b!!k))<h*1e-7 && abs ((vec a!!k)-(vec c!!k))<h*1e-7) [0..2]
  check (all (all aligned . vec) vertices && all axisFace tris) "geometry must have grid-aligned orthogonal boundaries; refine cell size or use a supported part"
  let snap (x,y,z)=let q a=fromInteger (round (a/h))*h in (q x,q y,q z)
      clean=meshMap snap tris
      (lo,hi)=meshBounds clean
      counts=map (\x -> round (x/h) :: Integer) (vec (vsub hi lo))
  check (all (>0) counts && product counts<=1000) "body exceeds the experimental 1000-cell grid budget"
  grid<-voxelize h clean
  check (all (==1) (Map.elems (vgFill grid))) "part contains partially filled cells"
  pure grid

region :: Map.Map String Body -> Double -> JValue -> Either String Patch
region bodies h v = do
  object ["instance","axis","side","plane_mm","bounds_mm"] v
  name<-field "instance" v >>= string
  body<-maybe (Left ("assembly FEA: unknown instance "++name)) Right (Map.lookup name bodies)
  axisName<-field "axis" v >>= string
  axis<-maybe (Left "assembly FEA: axis must be x, y or z") Right (lookup axisName [("x",0),("y",1),("z",2)])
  side<-field "side" v >>= number
  check (side==1 || side==(-1)) "side must be +1 or -1"
  plane<-field "plane_mm" v >>= number
  (lo,hi)<-field "bounds_mm" v >>= \b -> case b of
    JArray [a,c] -> (,) <$> triple a <*> triple c
    _ -> Left "assembly FEA: bounds_mm must contain lower and upper coordinates"
  let lower=vec lo; upper=vec hi; eps=h*1e-7; other=filter (/=axis) [0..2]
      aligned x=abs (x/h)<=1e7 && abs (x/h-fromInteger(round(x/h)))<1e-7
  check (all aligned (plane:lower++upper)) "region bounds must align with the common cell grid"
  check (abs (lower!!axis-plane)<eps && abs (upper!!axis-plane)<eps && all (\k -> upper!!k>lower!!k) other) "bounds must describe a nonempty rectangle in the selected plane"
  let grid=bodyGrid body
      selected f = let corner=vec(cellCorner grid (faceCell f)); facePlane=corner!!axis + if faceSign f>0 then h else 0
                   in faceAxis f==axis && faceSign f==round side && abs(facePlane-plane)<eps &&
                     all (\k -> corner!!k>=lower!!k-eps && corner!!k+h<=upper!!k+eps) other
      faces=filter selected (exteriorFaces grid)
      area=product [upper!!k-lower!!k | k<-other]
  check (not(null faces) && abs(fromIntegral(length faces)*h*h-area)<=max 1 area*1e-7)
    (name++": region is empty, partial or crosses a hole; specify a fully covered exterior rectangle")
  pure (Patch name faces)

-- A sampled cell may miss a narrow drilled hole. For a perfect bond,
-- intersect the original coplanar surface triangles with each face square.
-- Clipping preserves area even when triangles extend beyond the patch.
surfaceCovers :: [Tri] -> VoxelGrid -> Face -> Bool
surfaceCovers tris grid face = abs (sum (map area tris) - h*h) <= h*h*1e-6
  where
    h = vgCell grid
    axis = faceAxis face
    u = (axis+1) `mod` 3
    v = (axis+2) `mod` 3
    corner = vec (cellCorner grid (faceCell face))
    plane = corner!!axis + if faceSign face > 0 then h else 0
    project a = (vec a!!u - corner!!u, vec a!!v - corner!!v)
    edges [] = []
    edges ps@(x:xs) = zip ps (xs++[x])
    clip distance ps = concatMap cut (edges ps)
      where cut (a@(x,y),b@(xx,yy)) =
              let da=distance a; db=distance b; t=da/(da-db)
                  crossPoint=(x+t*(xx-x),y+t*(yy-y))
               in [crossPoint | (da>=0)/=(db>=0)] ++ [b | db>=0]
    area (a,b,c)
      | any (\p -> abs (vec p!!axis-plane)>h*1e-7) [a,b,c] = 0
      | otherwise = let poly=foldl (flip clip) (map project [a,b,c])
                               [fst,snd,(h-).fst,(h-).snd]
                        signed=sum [x*yy-y*xx | ((x,y),(xx,yy))<-edges poly]/2
                     in max 0 (fromIntegral (faceSign face)*signed)

assemblyProblem :: InstanceSnapshot -> JValue -> IO (Either String BondedProblem)
assemblyProblem snap config = fmap (fmap fst) (prepareAssembly StandardBudget snap config)

prepareAssembly :: AssemblyBudget -> InstanceSnapshot -> JValue -> IO (Either String (BondedProblem,JValue))
prepareAssembly budget snap config = case setup of
  Left e -> pure (Left e)
  Right (sampled,h,materials,tolerance) -> do
    meshes<-forM (isInstances snap) $ \r -> do
      if not(rigid (riTransform r)) then pure (Left "assembly FEA: scaled/sheared instance transforms are unsupported") else do
        mesh<-nativeCSGMesh (riShape r)
        pure $ do
          local<-mesh
          let placed=meshMap (affApply (riTransform r)) local
          grid<-(if sampled then voxelizeAligned else exactGrid) h placed
          (young,poisson)<-maybe (Left "assembly FEA: missing material") Right (Map.lookup (riId r) materials)
          pure (Body (riId r) grid young poisson,placed)
    pure $ do
      rows<-sequence meshes
      let bodies=map fst rows
          surfaces=Map.fromList [(bodyName b,(bodyGrid b,tris)) | (b,tris)<-rows]
      check (sum(map (voxelCount.bodyGrid) bodies)<=(if sampled then maxOccupiedCells budget else 1000)) "assembly exceeds the occupied-cell budget"
      let table=Map.fromList [(bodyName b,b)|b<-bodies]; patch=region table h
          bondPatch v=do
            p<-patch v
            let (grid,tris)=surfaces Map.! patchBody p
                whole f=Map.lookup (faceCell f) (vgFill grid)==Just 1 && surfaceCovers tris grid f
            check (not sampled || all whole (patchFaces p)) "bond crosses a hole, partial cell or nonplanar surface"
            pure p
      supports<-field "supports" config >>= array >>= mapM patch
      loads<-field "loads" config >>= array >>= mapM (\v -> do
        object (if sampled then ["region","force_n","pressure_mpa"] else ["region","force_n"]) v
        p<-field "region" v >>= patch
        f<-case (lookupKey "force_n" v,lookupKey "pressure_mpa" v) of
          (Just force,Nothing)->triple force
          (Nothing,Just pressure)->do
            q<-number pressure
            case patchFaces p of
              face:_ -> let total=negate q*h*h*fromIntegral (length (patchFaces p)*faceSign face)
                            component i=if i==faceAxis face then total else 0
                         in pure (component 0,component 1,component 2)
              [] -> Left "assembly FEA: empty pressure patch"
          _ -> Left "assembly FEA: specify exactly one force or pressure"
        pure (p,f))
      bonds<-field "bonds" config >>= array >>= mapM (\v -> do
        object (if sampled then ["name","a","b","stiffness_n_mm"] else ["name","a","b"]) v
        name<-field "name" v >>= string
        a<-field "a" v >>= bondPatch
        b<-field "b" v >>= bondPatch
        case lookupKey "stiffness_n_mm" v of
          Nothing -> pure (Bond name a b)
          Just k -> Spring name a b <$> triple k)
      let geometry (b,tris)=
            let grid=bodyGrid b; input=abs (meshVolume tris)
             in JObject [("instance",JString (bodyName b)),("source_volume_mm3",JNumber input),
                   ("sampled_volume_mm3",JNumber (voxelVolume grid)),
                   ("occupied_cells",JNumber (fromIntegral (voxelCount grid))),
                   ("xy_cell_mm",JNumber h),("xy_footprint_mm",JArray
                     [JArray [JNumber x,JNumber y] | (i,j)<-Set.toAscList
                       (Set.map (\(ix,iy,_) -> (ix,iy)) (vgCells grid)),
                       let (x,y,_)=cellCorner grid (i,j,0)])]
          selection role p=
            let grid=bodyGrid (table Map.! patchBody p)
                square f=let (x,y,z)=cellCorner grid (faceCell f)
                             corner=[x,y,z]; axis=faceAxis f
                             point u v=JArray [JNumber (c+h*component i) | (i,c)<-zip [0..] corner,
                               let component j | j==axis = if faceSign f>0 then 1 else 0
                                               | j==(axis+1) `mod` 3 = u
                                               | otherwise = v]
                          in JArray [point 0 0, point 1 0, point 1 1, point 0 1]
             in JObject [("role",JString role),("instance",JString (patchBody p)),
                         ("area_mm2",JNumber (h*h*fromIntegral (length (patchFaces p)))),
                         ("faces_mm",JArray (map square (patchFaces p)))]
          preview=JObject [("geometry",JArray (map geometry rows)),("regions",JArray
            (map (selection "support") supports ++ map (selection "load".fst) loads ++
             [selection (bondName b) side | b<-bonds,side<-[bondA b,bondB b]]))]
      pure (BondedProblem bodies bonds supports loads tolerance 50000,preview)
  where
    setup = do
      schema<-field "schema" config >>= string
      let sampled=schema=="coscad.assembly-sampled/1"
      object (["schema","cell_mm","bodies","supports","loads","bonds"] ++ ["tolerance" | sampled]) config
      tolerance<-maybe (Right 1e-9) number (lookupKey "tolerance" config)
      check (tolerance>=1e-12 && tolerance<=1e-4) "invalid solver tolerance"
      check (schema `elem` ["coscad.assembly-bonded/1","coscad.assembly-sampled/1"]) "unknown assembly analysis schema"
      h<-field "cell_mm" config >>= number
      check (h>0) "cell_mm must be positive"
      rows<-field "bodies" config >>= array
      check (not(null rows) && length rows<=32) "need 1..32 bodies"
      pairs<-mapM (\v -> do
        object (["instance","young_mpa","poisson"] ++ ["material_source" | sampled]) v
        case lookupKey "material_source" v of
          Nothing -> pure ()
          Just value -> string value >>= \label -> check (not (null label)) "material_source must identify the measurement or assumption"
        name<-field "instance" v >>= string
        e<-field "young_mpa" v >>= number
        nu<-field "poisson" v >>= number
        check (e>0 && e<=1e9 && nu>=0 && nu<=0.45) "invalid isotropic material constants"
        pure (name,(e,nu))) rows
      let materials=Map.fromList pairs
      check (Map.size materials==length pairs && Map.keysSet materials==Set.fromList(map riId (isInstances snap)))
        "declare exactly one material for every physical leaf instance; no implicit omissions"
      pure (sampled,h,materials,tolerance)

processAssemblyStress, processAssemblyRegions, processAssemblyRefinement :: Emit -> FilePath -> FilePath -> Maybe FilePath -> IO (Either Failure ())
processAssemblyStress = writeAssemblyReport StandardBudget False
processAssemblyRegions = writeAssemblyReport StandardBudget True
processAssemblyRefinement = writeAssemblyReport RefinementBudget False

writeAssemblyReport :: AssemblyBudget -> Bool -> Emit -> FilePath -> FilePath -> Maybe FilePath -> IO (Either Failure ())
writeAssemblyReport budget previewOnly emit input spec output = runCommand $ do
  start<-getMonotonicTimeNSec
  snap<-loadInstances emit input >>= either abort pure
  bytes<-BS.readFile spec
  source<-either (const (abort(inputFailure "assembly FEA: config is not UTF-8"))) (pure.Text.unpack) (Text.decodeUtf8' bytes)
  config<-either (abort.inputFailure) pure (decode source)
  (problem,preview)<-prepareAssembly budget snap config >>= either (abort.inputFailure) pure
  prepared<-getMonotonicTimeNSec
  result<-if previewOnly then pure [] else do
    solved<-either (abort.designFailure) pure (solveBondedWithBudget budget problem)
    let normals=Map.fromList [(riId r, fmap (\(m,_) -> mApply m (0,0,1)) (printToAssembly r)) | r<-isInstances snap]
        layer b=case Map.lookup (brName b) normals of
          Just (Just (x,y,z)) ->
            let tensions=[xx*x*x+yy*y*y+zz*z*z+2*(xy*x*y+yz*y*z+zx*z*x)
                         | (_,_,cell)<-brCells b,[xx,yy,zz,xy,yz,zx]<-[csCentre cell]]
             in JNumber (maximum (0:tensions))
          _ -> JNull
    pure [("result",bondedJson problem solved),("max_cell_centre_layer_normal_tension_mpa",
          JObject [(brName b,layer b) | b<-baBodies solved])]
  finished<-getMonotonicTimeNSec
  let seconds a b=JNumber (fromIntegral (b-a)/1e9)
      target=maybe (dropExtension input++if previewOnly then "_regions.json" else "_bonded_stress.json") id output
      hash=concatMap (\n -> let h=showHex n "" in if length h==1 then '0':h else h) (BS.unpack(SHA.hash bytes))
      report=JObject $ [("schema",JString (if previewOnly then "coscad.assembly-regions/1" else "coscad.assembly-stress/1")),("experimental",JBool True)
        ,("assembly",instancesJson snap),("analysis_source",JString spec),("analysis_sha256",JString hash)
        ,("prepare_seconds",seconds start prepared),("solve_seconds",if previewOnly then JNull else seconds prepared finished)
        ,("region_specification",config),("preview",preview)] ++ result
  protectInputs (spec:map fst (isSources snap)) target
  writeFileUtf8Atomic target (encodePretty report++"\n")
  emit (Note ("Wrote "++target++" (experimental bonded assembly; no strength verdict)"))
