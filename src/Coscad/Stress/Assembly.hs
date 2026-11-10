-- | Explicit experimental bridge from physical assembly instances to bonded Hex20.
module Coscad.Stress.Assembly (assemblyProblem, processAssemblyStress) where
import Coscad.Assembly.Instances
import Coscad.Instance
import Coscad.Backend (nativeCSGMesh)
import Coscad.Geometry (V3, vsub)
import Coscad.Mesh (Tri, meshMap, meshBounds)
import Coscad.Json
import Coscad.IO (writeFileUtf8Atomic)
import Coscad.Report
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

assemblyProblem :: InstanceSnapshot -> JValue -> IO (Either String BondedProblem)
assemblyProblem snap config = case setup of
  Left e -> pure (Left e)
  Right (h,materials) -> do
    meshes<-forM (isInstances snap) $ \r -> do
      if not(rigid (riTransform r)) then pure (Left "assembly FEA: scaled/sheared instance transforms are unsupported") else do
        mesh<-nativeCSGMesh (riShape r)
        pure $ do
          local<-mesh
          grid<-exactGrid h (meshMap (affApply (riTransform r)) local)
          (young,poisson)<-maybe (Left "assembly FEA: missing material") Right (Map.lookup (riId r) materials)
          pure (Body (riId r) grid young poisson)
    pure $ do
      bodies<-sequence meshes
      check (sum(map (voxelCount.bodyGrid) bodies)<=1000) "assembly exceeds the experimental 1000-cell budget"
      let table=Map.fromList [(bodyName b,b)|b<-bodies]; patch=region table h
      supports<-field "supports" config >>= array >>= mapM patch
      loads<-field "loads" config >>= array >>= mapM (\v -> do
        object ["region","force_n"] v
        p<-field "region" v >>= patch
        f<-field "force_n" v >>= triple
        pure (p,f))
      bonds<-field "bonds" config >>= array >>= mapM (\v -> do
        object ["name","a","b"] v
        Bond <$> (field "name" v >>= string) <*> (field "a" v >>= patch) <*> (field "b" v >>= patch))
      pure (BondedProblem bodies bonds supports loads 1e-9 50000)
  where
    setup = do
      object ["schema","cell_mm","bodies","supports","loads","bonds"] config
      schema<-field "schema" config >>= string
      check (schema=="coscad.assembly-bonded/1") "expected coscad.assembly-bonded/1"
      h<-field "cell_mm" config >>= number
      check (h>0) "cell_mm must be positive"
      rows<-field "bodies" config >>= array
      check (not(null rows) && length rows<=32) "need 1..32 bodies"
      pairs<-mapM (\v -> do
        object ["instance","young_mpa","poisson"] v
        name<-field "instance" v >>= string
        e<-field "young_mpa" v >>= number
        nu<-field "poisson" v >>= number
        check (e>0 && e<=1e9 && nu>=0 && nu<=0.45) "invalid isotropic material constants"
        pure (name,(e,nu))) rows
      let materials=Map.fromList pairs
      check (Map.size materials==length pairs && Map.keysSet materials==Set.fromList(map riId (isInstances snap)))
        "declare exactly one material for every physical leaf instance; no implicit omissions"
      pure (h,materials)

processAssemblyStress :: Emit -> FilePath -> FilePath -> Maybe FilePath -> IO (Either Failure ())
processAssemblyStress emit input spec output = runCommand $ do
  snap<-loadInstances emit input >>= either abort pure
  bytes<-BS.readFile spec
  source<-either (const (abort(inputFailure "assembly FEA: config is not UTF-8"))) (pure.Text.unpack) (Text.decodeUtf8' bytes)
  config<-either (abort.inputFailure) pure (decode source)
  problem<-assemblyProblem snap config >>= either (abort.inputFailure) pure
  result<-either (abort.designFailure) pure (solveBonded problem)
  let target=maybe (dropExtension input++"_bonded_stress.json") id output
      hash=concatMap (\n -> let h=showHex n "" in if length h==1 then '0':h else h) (BS.unpack(SHA.hash bytes))
      report=JObject [("schema",JString "coscad.assembly-stress/1"),("experimental",JBool True)
        ,("assembly",instancesJson snap),("analysis_source",JString spec),("analysis_sha256",JString hash)
        ,("region_specification",config),("result",bondedJson problem result)]
  protectInputs (spec:map fst (isSources snap)) target
  writeFileUtf8Atomic target (encodePretty report++"\n")
  emit (Note ("Wrote "++target++" (experimental bonded assembly; no strength verdict)"))
