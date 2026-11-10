-- | Bounded experimental bonded Hex20 assemblies. Identity is per instance;
-- coincident nodes are merged only by explicit matching exterior-face ties.
-- No assembly placement constraint is interpreted as a mechanical constraint.
module Coscad.Stress.Bonded
  ( Body (..), Patch (..), Bond (..), BondedProblem (..)
  , Wrench (..), InterfaceResult (..), BodyResult (..), BondedResult (..)
  , solveBonded, parseBondedProblem, bondedJson
  ) where

import Control.Monad (unless, forM_)
import Data.Array.Unboxed ((!), elems, listArray)
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Coscad.Json
import Coscad.Linalg (V3, vadd, vsub, cross, vlen)
import Coscad.Stress.Hex20
import Coscad.Stress.Sparse
import Coscad.Stress.Voxel
import Coscad.Validate (finite)

data Body = Body { bodyName :: String, bodyGrid :: VoxelGrid, bodyYoung :: Double, bodyPoisson :: Double }
  deriving (Eq, Show)
data Patch = Patch { patchBody :: String, patchFaces :: [Face] } deriving (Eq, Show)
data Bond = Bond { bondName :: String, bondA :: Patch, bondB :: Patch } deriving (Eq, Show)
data BondedProblem = BondedProblem
  { bpBodies :: [Body], bpBonds :: [Bond], bpSupports :: [Patch]
  , bpLoads :: [(Patch, V3)], bpTolerance :: Double, bpIterations :: Int
  } deriving (Eq, Show)

type Node = (String, Key)
data Wrench = Wrench { wrenchForce :: V3, wrenchMoment :: V3 } deriving (Eq, Show)
data InterfaceResult = InterfaceResult
  { irName :: String, irBodyA :: String, irBodyB :: String, irArea :: Double
  , irNodes :: Int, irOnA :: Wrench, irOnB :: Wrench
  } deriving (Eq, Show)
data BodyResult = BodyResult
  { brSpecification :: Body, brName :: String, brNodes :: [(Key, V3, V3)], brCells :: [(Key, V3, CellStress)]
  , brSupport :: Wrench, brEnergy :: Double
  }
data BondedResult = BondedResult
  { baBodies :: [BodyResult], baInterfaces :: [InterfaceResult]
  , baSupport :: Wrench, baApplied :: Wrench, baEnergy :: Double
  , baIterations :: Int, baResidual :: Double, baDofs :: Int
  }

check :: Bool -> String -> Either String ()
check ok msg = unless ok (Left ("FEA bonded: " ++ msg))
first :: String -> [a] -> Either String a
first _ (x:_) = Right x
first message [] = Left ("FEA bonded: " ++ message)

components :: V3 -> [Double]
components (x,y,z) = [x,y,z]
scale :: Double -> V3 -> V3
scale a (x,y,z) = (a*x,a*y,a*z)
zero :: V3
zero = (0,0,0)
addW :: Wrench -> Wrench -> Wrench
addW (Wrench f m) (Wrench g n) = Wrench (vadd f g) (vadd m n)
sumW :: [Wrench] -> Wrench
sumW = foldl' addW (Wrench zero zero)

solveBonded :: BondedProblem -> Either String BondedResult
solveBonded p = do
  let bodies = bpBodies p; names = map bodyName bodies
  check (not (null bodies) && length bodies <= 32) "need 1..32 bodies"
  check (all (not . null) names && Set.size (Set.fromList names) == length names) "body names must be nonempty and unique"
  check (sum (map (toInteger . voxelCount . bodyGrid) bodies) <= 1000) "assembly exceeds the experimental 1000-cell budget"
  check (finite (bpTolerance p) && bpTolerance p >= 1e-12 && bpTolerance p <= 1e-4) "tolerance must be in [1e-12,1e-4]"
  check (bpIterations p >= 1 && bpIterations p <= 50000) "iterations must be in [1,50000]"
  check (length (bpBonds p) <= 64 && length (bpSupports p) <= 64 && length (bpLoads p) <= 64) "at most 64 bonds, supports and loads each"
  firstBody <- first "no bodies" bodies
  let table = Map.fromList [(bodyName b,b) | b <- bodies]
      firstGrid = bodyGrid firstBody; h = vgCell firstGrid
      origin = vgOrigin firstGrid
      aligned x = finite x && abs x <= 1e7 && abs (x - fromInteger (round x)) <= 1e-8
      lattice g = map (/h) (components (vsub (vgOrigin g) origin))
  check (finite h && h > 0) "cell size must be finite and positive"
  forM_ bodies $ \b -> do
    let g = bodyGrid b; (nx,ny,nz) = vgCounts g
        inside (i,j,k) = i>=0 && i<nx && j>=0 && j<ny && k>=0 && k<nz
    check (all finite (components (vgOrigin g)) && vgCell g == h && all aligned (lattice g))
      (bodyName b ++ ": grids must have the same cell size and aligned finite origins")
    check (all (\n -> n>0 && n<=4096) [nx,ny,nz] && not (Set.null (vgCells g)) && all inside (Set.toList (vgCells g)))
      (bodyName b ++ ": invalid or empty grid")
    check (Map.keysSet (vgFill g) `Set.isSubsetOf` vgCells g && all (==1) (Map.elems (vgFill g)))
      (bodyName b ++ ": only fully filled cells are supported")
    check (length (connectedCells g) == 1) (bodyName b ++ ": body must be face-connected")
    check (finite (bodyYoung b) && bodyYoung b > 0 && bodyYoung b <= 1e9) (bodyName b ++ ": invalid Young's modulus")
    check (finite (bodyPoisson b) && bodyPoisson b >= 0 && bodyPoisson b <= 0.45) (bodyName b ++ ": Poisson ratio must be in [0,0.45]")
    check (all (all finite . components . nodePosition g) (Set.toList (allNodes g))) (bodyName b ++ ": nonfinite mesh coordinates")
  let offset g = case map (round :: Double -> Int) (lattice g) of [x,y,z] -> (x,y,z); _ -> (0,0,0)
      plus (a,b,c) (x,y,z) = (a+x,b+y,c+z)
      occupied = [plus (offset (bodyGrid b)) c | b <- bodies, c <- Set.toList (vgCells (bodyGrid b))]
  check (Set.size (Set.fromList occupied) == length occupied) "bodies overlap; a bond must join exterior faces, not overlapping solids"
  let getBody name = maybe (Left ("FEA bonded: unknown body '" ++ name ++ "'")) Right (Map.lookup name table)
      nodesOf (Patch n fs) = Set.fromList [(n,k) | f <- fs, (k,_) <- faceNodes f]
      gridOf n = bodyGrid (table Map.! n)
      pos :: Node -> V3
      pos (n,k) = nodePosition (gridOf n) k
      worldKey (n,(x,y,z)) = let (a,b,c) = offset (gridOf n) in (x+2*a,y+2*b,z+2*c)
      validatePatch patch = do
        b <- getBody (patchBody patch)
        let fs = patchFaces patch
        check (not (null fs)) (patchBody patch ++ ": empty interface/support/load patch")
        check (Set.size (Set.fromList fs) == length fs) "duplicate faces in patch"
        check (Set.fromList fs `Set.isSubsetOf` Set.fromList (exteriorFaces (bodyGrid b))) "patch contains a non-exterior or invalid face"
        f <- first "empty patch" fs
        let plane face = let c = components (cellCorner (bodyGrid b) (faceCell face))
                         in c !! faceAxis face + (if faceSign face > 0 then h else 0)
        check (all (\q -> faceAxis q == faceAxis f && faceSign q == faceSign f && abs (plane q - plane f) <= 1e-8*h) fs)
          "patch must be planar with one outward normal"
  mapM_ validatePatch (bpSupports p ++ map fst (bpLoads p) ++ concat [[bondA b,bondB b] | b <- bpBonds p])
  let bondNames = map bondName (bpBonds p)
  check (all (not . null) bondNames && Set.size (Set.fromList bondNames) == length bondNames) "bond names must be nonempty and unique"
  pairs <- mapM (\bond -> do
    let a = bondA bond; b = bondB bond
        keyed patch = Map.fromList [(worldKey n,n) | n <- Set.toList (nodesOf patch)]
        na = keyed a; nb = keyed b
    fa <- first "empty interface" (patchFaces a)
    fb <- first "empty interface" (patchFaces b)
    check (patchBody a /= patchBody b) "a bond must join two different bodies"
    check (faceAxis fa == faceAxis fb && faceSign fa == negate (faceSign fb)) (bondName bond ++ ": interface normals must oppose")
    check (Map.keysSet na == Map.keysSet nb && length (patchFaces a) == length (patchFaces b))
      (bondName bond ++ ": mismatched interface; matching coincident faces required (no gaps or interpolation)")
    -- Node equality plus equal face count is sufficient on this common grid:
    -- explicitly compare face-node sets as well, so holes cannot be filled in.
    let faceSets patch = Set.fromList [Set.fromList [worldKey (patchBody patch,k) | (k,_) <- faceNodes f] | f <- patchFaces patch]
    check (faceSets a == faceSets b) (bondName bond ++ ": mismatched interface faces")
    return [(na Map.! k, nb Map.! k) | k <- Map.keys na]) (bpBonds p)
  let tieNodes = concatMap (concatMap (\(a,b) -> [a,b])) pairs
      tied = Set.fromList tieNodes
      fixed = Set.unions (map nodesOf (bpSupports p))
  check (Set.size tied == length tieNodes) "shared interface nodes across bonds are unsupported (duplicate or multiway joint)"
  check (Set.null (Set.intersection fixed tied)) "supports on tied nodes make interface reaction attribution ambiguous"
  -- Every full-face bond removes all relative rigid modes. Each connected
  -- component therefore needs a noncollinear clamp; graph reachability alone
  -- would not suffice for springs, point ties or released connections.
  let neighbours n = [if a==n then b else a | bond <- bpBonds p, let a=patchBody (bondA bond); b=patchBody (bondB bond), a==n || b==n]
      reach seen [] = seen
      reach seen (n:ns) | Set.member n seen = reach seen ns
                        | otherwise = reach (Set.insert n seen) (neighbours n ++ ns)
      components' remaining | Set.null remaining = []
                            | otherwise = let group = reach Set.empty [Set.findMin remaining]
                                          in group : components' (Set.difference remaining group)
      noncollinear pts = case pts of
        a:rest -> case filter (\b -> vlen (vsub b a) > 1e-8*h) rest of
          b:_ -> any (\c -> vlen (cross (vsub b a) (vsub c a)) > 1e-9*h*h) rest
          [] -> False
        [] -> False
  forM_ (components' (Set.fromList names)) $ \group ->
    check (noncollinear [pos n | n@(name,_) <- Set.toList fixed, Set.member name group])
      ("floating or insufficiently supported component: " ++ unwords (Set.toList group))
  forM_ (bpLoads p) $ \(_,f) -> check (all finite (components f) && vlen f > 0) "load must be finite and nonzero"
  -- Aggregate each traction independently, then combine load cases. Equal
  -- and opposite tractions cancel exactly instead of leaving summation noise.
  let loads = Map.unionsWith vadd
        [Map.fromListWith vadd [((patchBody patch,k), scale (w / fromIntegral (length (patchFaces patch))) f)
                              | face <- patchFaces patch, (k,w) <- faceNodes face]
        | (patch,f) <- bpLoads p]
      aliases = Map.fromList [(n,min a b) | ps <- pairs, (a,b) <- ps, n <- [a,b]]
      representative n = Map.findWithDefault n n aliases
      nodeList = [(bodyName b,k) | b <- bodies, k <- Set.toList (allNodes (bodyGrid b))]
      reps = sortOn (\n -> (worldKey n,n)) (Set.toList (Set.fromList (map representative nodeList)))
      indices = Map.fromList (zip reps [0..])
      index n = indices Map.! representative n
      nd = 3 * length reps
      reducedLoads = Map.fromListWith vadd [(index n,f) | (n,f) <- Map.toList loads]
      fixedIndices = Set.map index fixed
  check (nd <= 60000) "assembly exceeds 60000 degrees of freedom"
  check (any (\(n,f) -> Set.notMember n fixedIndices && vlen f > 0) (Map.toList reducedLoads)) "no nonzero free load after reduction"
  let stiffness = Map.fromSet (\(e,nu) -> elementStiffness e nu h)
                    (Set.fromList [(bodyYoung b,bodyPoisson b) | b <- bodies])
      ke b = stiffness Map.! (bodyYoung b,bodyPoisson b)
      elements = [(b,c,[(bodyName b,k) | k <- cellNodes c]) | b <- bodies, c <- Set.toList (vgCells (bodyGrid b))]
      matrix = assembleElements (length reps) [(map index ns,1,ke b) | (b,_,ns) <- elements]
      rhs = vecFromList (concat [components (Map.findWithDefault zero i reducedLoads) | i <- [0..length reps-1]])
      free = listArray (0,nd-1) [Set.notMember i fixedIndices | i <- [0..length reps-1], _ <- [0::Int,1,2]]
  check (all finite (elems (csrVal matrix)) && all (>0) (elems (csrDiagonal matrix))) "invalid stiffness matrix"
  (u,it,res) <- pcgSolve (bpIterations p) (bpTolerance p) matrix free rhs
  let displacement n = let i = 3*index n in (u!i,u!(i+1),u!(i+2))
      contributions = [(n,f) | (b,_,ns) <- elements, (n,f) <- zip ns (elementForces 1 (ke b) (map displacement ns))]
      internal = Map.fromListWith vadd contributions
      residual n = vsub (Map.findWithDefault zero n internal) (Map.findWithDefault zero n loads)
      resultant ns = sumW [let f=residual n in Wrench f (cross (pos n) f) | n <- Set.toList ns]
      bodyResult b =
        let name = bodyName b
            ns = [(name,k) | k <- Set.toList (allNodes (bodyGrid b))]
            bodyEnergy = sum [sum (zipWith (*) (components (displacement n)) (components (internal Map.! n))) | n <- ns] / 2
        in BodyResult b name [(k,pos n,displacement n) | n@(_,k) <- ns]
                         [(c,cellCentre (bodyGrid b) c,recoverCellStress (bodyYoung b) (bodyPoisson b) h (map displacement es)) | (b',c,es) <- elements, bodyName b' == name]
                         (resultant (Set.filter ((==name) . fst) fixed)) bodyEnergy
      results = map bodyResult bodies
      interfaces = [InterfaceResult (bondName bond) (patchBody a) (patchBody b) (fromIntegral (length (patchFaces a))*h*h)
                      (Set.size (nodesOf a)) (resultant (nodesOf a)) (resultant (nodesOf b))
                    | bond <- bpBonds p, let a=bondA bond; b=bondB bond]
      applied = sumW [Wrench f (cross (pos n) f) | (n,f) <- Map.toList loads]
      support = resultant fixed
      energy = sum (map brEnergy results)
      wrenchNumbers (Wrench f m) = components f ++ components m
  check (all finite (elems u ++ [energy] ++ concatMap brNumbers results ++ concatMap interfaceNumbers interfaces ++ wrenchNumbers applied ++ wrenchNumbers support)) "nonfinite result"
  return (BondedResult results interfaces support applied energy it res nd)
  where
    brNumbers b = brEnergy b : concat [csCentre s ++ [csCentreVonMises s,csPeakVonMises s,csPeakTensionZ s] | (_,_,s) <- brCells b]
    interfaceNumbers i = concat [components f ++ components m | Wrench f m <- [irOnA i,irOnB i]]

-- | Strict, versioned scratch format. Rectangular grids and explicit local
-- cell faces avoid inventing .assemble region syntax before semantics settle.
parseBondedProblem :: String -> Either String BondedProblem
parseBondedProblem text = do
  value <- decode text
  object ["schema","bodies","bonds","supports","loads","tolerance","iterations"] value
  schema <- field "schema" value >>= string
  check (schema == "coscad.bonded-problem/1") "expected schema coscad.bonded-problem/1"
  bodies <- field "bodies" value >>= array >>= mapM body
  bonds <- field "bonds" value >>= array >>= mapM bond
  supports <- field "supports" value >>= array >>= mapM patch
  loads <- field "loads" value >>= array >>= mapM load
  tol <- maybe (Right 1e-9) number (lookupKey "tolerance" value)
  iterations <- maybe (Right 20000) integer (lookupKey "iterations" value)
  return (BondedProblem bodies bonds supports loads tol iterations)
  where
    body v = do
      object ["name","origin_mm","cell_mm","counts","young_mpa","poisson"] v
      name <- field "name" v >>= string
      origin <- field "origin_mm" v >>= vector
      h <- field "cell_mm" v >>= number
      counts@(nx,ny,nz) <- field "counts" v >>= triple integer
      check (all (>0) [nx,ny,nz] && toInteger nx*toInteger ny*toInteger nz <= 1000) "body counts must be positive and within 1000 cells"
      e <- field "young_mpa" v >>= number
      nu <- field "poisson" v >>= number
      let cells = Set.fromList [(i,j,k) | i <- [0..nx-1], j <- [0..ny-1], k <- [0..nz-1]]
      return (Body name (VoxelGrid origin h counts cells Map.empty) e nu)
    patch v = do
      object ["body","faces"] v
      n <- field "body" v >>= string
      fs <- field "faces" v >>= array >>= mapM face
      return (Patch n fs)
    face (JArray [a,b,c,d,e]) = Face <$> ((,,) <$> integer a <*> integer b <*> integer c) <*> integer d <*> integer e
    face _ = Left "FEA bonded: face must be [cell_x,cell_y,cell_z,axis,sign]"
    bond v = do
      object ["name","a","b"] v
      Bond <$> (field "name" v >>= string) <*> (field "a" v >>= patch) <*> (field "b" v >>= patch)
    load v = do
      object ["patch","force_n"] v
      (,) <$> (field "patch" v >>= patch) <*> (field "force_n" v >>= vector)

object :: [String] -> JValue -> Either String ()
object allowed (JObject xs) = check (all (`elem` allowed) (map fst xs) && Set.size (Set.fromList (map fst xs)) == length xs) "unknown or duplicate JSON field"
object _ _ = Left "FEA bonded: expected object"
field :: String -> JValue -> Either String JValue
field k v = maybe (Left ("FEA bonded: missing field " ++ k)) Right (lookupKey k v)
string :: JValue -> Either String String
string (JString s) = Right s
string _ = Left "FEA bonded: expected string"
array :: JValue -> Either String [JValue]
array (JArray xs) = Right xs
array _ = Left "FEA bonded: expected array"
number :: JValue -> Either String Double
number (JNumber x) | finite x = Right x
number _ = Left "FEA bonded: expected finite number"
integer :: JValue -> Either String Int
integer v = do
  x <- number v
  check (abs x <= 10000000 && x == fromInteger (round x)) "expected bounded integer"
  return (round x)
triple :: (JValue -> Either String a) -> JValue -> Either String (a,a,a)
triple f (JArray [x,y,z]) = (,,) <$> f x <*> f y <*> f z
triple _ _ = Left "FEA bonded: expected three components"
vector :: JValue -> Either String V3
vector = triple number

bondedJson :: BondedProblem -> BondedResult -> JValue
bondedJson p r = JObject
  [("schema",JString "coscad.bonded-result/1"),("experimental",JBool True)
  ,("degrees_of_freedom",num (baDofs r)),("iterations",num (baIterations r)),("free_residual_relative",JNumber (baResidual r))
  ,("strain_energy_n_mm",JNumber (baEnergy r)),("support",wrench (baSupport r)),("applied",wrench (baApplied r))
  ,("equilibrium_residual",wrench (addW (baSupport r) (baApplied r)))
  ,("bodies",JArray (map body (baBodies r))), ("interfaces",JArray (map interface (baInterfaces r)))
  ,("supports",JArray (map patch (bpSupports p)))
  ,("loads",JArray [JObject [("patch",patch ps),("force_n",vec f)] | (ps,f) <- bpLoads p])
  ,("bonds",JArray [JObject [("name",JString (bondName b)),("a",patch (bondA b)),("b",patch (bondB b))] | b <- bpBonds p])
  ,("solver",JObject [("tolerance",JNumber (bpTolerance p)),("iteration_limit",num (bpIterations p))])
  ,("limitations",JArray (map JString
    ["Experimental small-strain isotropic Hex20; common aligned grids, full cells and explicit matching planar perfect bonds only."
    ,"No contact, joint compliance, preload, infill, print-layer stiffness model or strength verdict."
    ,"Interface wrench is the force/moment applied to that body by the other body, after subtracting external loads; all moments about world origin."
    ,"Lengths mm, forces N, stresses MPa; peak stresses and geometry require refinement. No physical validation claimed."]))]
  where
    num = JNumber . fromIntegral
    vec = JArray . map JNumber . components
    wrench (Wrench f m) = JObject [("force_n",vec f),("moment_n_mm",vec m)]
    body b = let spec = brSpecification b
             in JObject [("name",JString (brName b)),("young_mpa",JNumber (bodyYoung spec)),("poisson",JNumber (bodyPoisson spec))
                        ,("origin_mm",vec (vgOrigin (bodyGrid spec))),("cell_mm",JNumber (vgCell (bodyGrid spec)))
                        ,("support",wrench (brSupport b)),("strain_energy_n_mm",JNumber (brEnergy b))
                        ,("nodes",JArray [JObject [("key",key k),("position_mm",vec x),("displacement_mm",vec u)] | (k,x,u) <- brNodes b])
                        ,("cells",JArray [JObject [("key",key k),("centre_mm",vec x),("stress_xx_yy_zz_xy_yz_zx_mpa",JArray (map JNumber (csCentre s))),("peak_von_mises_mpa",JNumber (csPeakVonMises s))] | (k,x,s) <- brCells b])]
    patch ps = JObject [("body",JString (patchBody ps)),("faces",JArray [JArray (map num [a,b,c,axis,sgn]) | Face (a,b,c) axis sgn <- patchFaces ps])]
    key (a,b,c) = JArray (map num [a,b,c])
    interface i = JObject [("name",JString (irName i)),("body_a",JString (irBodyA i)),("body_b",JString (irBodyB i))
                          ,("area_mm2",JNumber (irArea i)),("nodes_per_side",num (irNodes i))
                          ,("on_a",wrench (irOnA i)),("on_b",wrench (irOnB i)),("balance_residual",wrench (addW (irOnA i) (irOnB i)))]
