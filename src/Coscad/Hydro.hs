-- | Static hydrostatics of a floating part: waterline, draft, trim and the
-- righting-arm (GZ) curve over heel, with an optional downflooding opening.
--
-- The submerged volume and its centre come straight from the closed,
-- consistently oriented triangle mesh: each triangle is clipped to the half
-- space below the waterline and its signed tetrahedron is taken against an
-- apex on the waterline plane. The cap the clip would leave lies in that
-- plane, so its tetrahedra have zero volume and it never needs building.
--
-- Conventions (as the earlier verify.py scripts): the hull's length runs
-- along x and z is up. Heel is a rotation about +x, so positive heel lowers
-- the -y side; the righting arm GZ = g_y - b_y after rotation is positive
-- when the hull turns back upright. Trim is a rotation about -y, so positive
-- trim is bow (+x) up.
module Coscad.Hydro
  ( Hydro (..), HeelState (..), FloatSpec (..), Payload (..)
  , submerged, equilibrium, heelState, staticTrim
  , fillVoids, floatArgs, processFloat, defaultHeels, waterDensity
  ) where

import Coscad.Backend (nativeIndexedCSGMesh)
import Coscad.IO (readFileUtf8)
import Coscad.Json
import Coscad.Linalg (M3, V3, cross, dot3, mApply, matMul, rodMat, scale3, vadd, vsub)
import Coscad.Mesh (Tri, meshVolumeCentre)
import Coscad.Mesh.Indexed
import Coscad.Parser (parseProgramFull)
import Coscad.Report
import Coscad.Stress.Material (Material (..), materialNamed)
import Coscad.Validate (finite)
import Control.Monad (unless, when)
import Data.List (foldl', find)
import Data.Maybe (fromMaybe)
import System.FilePath (takeDirectory, takeExtension, (</>), isAbsolute)

-- | One loaded float problem: the solid that keeps water out (the displacer),
-- the total mass and centre of gravity, and points that flood the hull once
-- they are under water.
data Hydro = Hydro
  { hyDisplacer :: [Tri]
  , hyMass :: Double          -- ^ g, including payload
  , hyGravity :: V3           -- ^ mm, upright frame
  , hyWater :: Double         -- ^ g/cm3
  , hyOpening :: [V3]         -- ^ mm, upright frame; empty = no opening
  }

data HeelState = HeelState
  { hsHeel :: Double
  , hsWaterline :: Double
  , hsDisplacedG :: Double
  , hsBuoyancy :: V3
  , hsGravity :: V3
  , hsRighting :: Double
  , hsOpeningClearance :: Maybe Double
  } deriving (Show)

data Payload = Payload { plName :: String, plMass :: Double, plCentre :: V3 }

data FloatSpec = FloatSpec
  { fsBody :: FilePath
  , fsDisplacer :: Maybe FilePath
  , fsOpening :: Maybe FilePath
  , fsOpeningPoints :: [V3]
  , fsMaterial :: String
  , fsDensity :: Maybe Double
  , fsMass :: Maybe (Double, V3)
  , fsPayload :: [Payload]
  , fsWater :: (String, Double)
  , fsHeels :: [Double]
  }

defaultHeels :: [Double]
defaultHeels = [0,5..90] ++ [100,120,150,180]

waterDensity :: String -> Maybe Double
waterDensity w = lookup w [("fresh",1.000),("sea",1.025)]

-- ------------------------------------------------------------------
-- geometry

-- | Volume (mm^3) and centre of the part of a closed mesh below z = w.
submerged :: Double -> [Tri] -> (Double, Maybe V3)
submerged w tris
  | vol <= 0 = (max 0 vol, Nothing)
  | otherwise = (vol, Just (scale3 (1/vol) moment))
  where
    apex = case tris of
      ((x,y,_),_,_):_ -> (x,y,w)
      [] -> (0,0,w)
    (vol, moment) = foldl' add (0,(0,0,0)) (concatMap (clip w) tris)
    add (v,m) (a,b,c) =
      let t = dot3 (vsub a apex) (cross (vsub b apex) (vsub c apex)) / 6
          ctr = scale3 0.25 (vadd (vadd a b) (vadd c apex))
      in v `seq` m `seq` (v+t, vadd m (scale3 t ctr))

-- Sutherland-Hodgman against z <= w, fanned back into triangles.
clip :: Double -> Tri -> [Tri]
clip w (a,b,c)
  | all below [a,b,c] = [(a,b,c)]
  | not (any below [a,b,c]) = []
  | otherwise = fan (concatMap edge [(a,b),(b,c),(c,a)])
  where
    below (_,_,z) = z <= w
    edge (p,q)
      | below p && below q = [q]
      | below p = [cut p q]
      | below q = [cut p q, q]
      | otherwise = []
    cut p@(_,_,pz) q@(_,_,qz) = let t = (w-pz)/(qz-pz) in vadd p (scale3 t (vsub q p))
    fan (p:rest) = zipWith (\q r -> (p,q,r)) rest (drop 1 rest)
    fan [] = []

-- | Keep only the outward shells: sealed internal voids become part of the
-- displacer, the way air in a sealed chamber keeps water out.
fillVoids :: IndexedMesh -> [Tri]
fillVoids mesh = concat [ts | s <- indexedComponents mesh, let ts = indexedTriangles s, signedVolume ts > 0]
  where
    -- meshVolumeCentre reports magnitude; the sign tells outward from inward shells
    signedVolume ts = sum [dot3 a (cross b c) / 6 | (a,b,c) <- ts]

rotation :: Double -> Double -> M3
rotation heel trim = matMul (rodMat heel (1,0,0)) (rodMat trim (0,-1,0))

-- | Waterline z (mm) and submerged centre for the rotated displacer, by
-- bisection. Nothing when the displacer cannot carry the mass.
equilibrium :: Hydro -> M3 -> Maybe (Double, V3, Double)
equilibrium hy r = if total <= target then Nothing else go (40::Int) lo hi
  where
    tris = [(mApply r a, mApply r b, mApply r c) | (a,b,c) <- hyDisplacer hy]
    zs = concat [[z1,z2,z3] | ((_,_,z1),(_,_,z2),(_,_,z3)) <- tris]
    lo = minimum zs; hi = maximum zs
    target = hyMass hy / hyWater hy * 1000
    total = fst (submerged (hi+1) tris)
    go 0 l h = let w = (l+h)/2; (v,c) = submerged w tris in fmap (\b -> (w,b,v)) c
    go n l h = let w = (l+h)/2 in if fst (submerged w tris) < target then go (n-1) w h else go (n-1) l w

heelState :: Hydro -> Double -> Double -> Maybe HeelState
heelState hy trim heel = do
  let r = rotation heel trim
  (w, b, v) <- equilibrium hy r
  let g = mApply r (hyGravity hy)
      (_,gy,_) = g; (_,by,_) = b
      openings = [z | p <- hyOpening hy, let (_,_,z) = mApply r p]
  pure (HeelState heel w (v * hyWater hy / 1000) b g (gy - by)
          (if null openings then Nothing else Just (minimum openings - w)))

-- | Upright trim angle (degrees, bow up) where buoyancy sits under gravity.
staticTrim :: Hydro -> Maybe Double
staticTrim hy = arm (-20) >>= \a0 -> arm 20 >>= \a1 ->
  if signum a0 == signum a1 then Nothing else Just (go (30::Int) (-20) 20 a0)
  where
    arm p = do
      let r = rotation 0 p
      (_, (bx,_,_), _) <- equilibrium hy r
      let (gx,_,_) = mApply r (hyGravity hy)
      pure (bx - gx)
    go 0 l h _ = (l+h)/2
    go n l h al = let m = (l+h)/2 in case arm m of
      Nothing -> m
      Just am -> if signum am == signum al then go (n-1) m h am else go (n-1) l m al

-- ------------------------------------------------------------------
-- command

floatArgs :: [String] -> Either String (FilePath, Maybe FilePath)
floatArgs [f] = Right (f, Nothing)
floatArgs [f,"-o",o] = Right (f, Just o)
floatArgs _ = Left "usage: coscad float <part.coscad|float.json> [-o report.json]"

processFloat :: FilePath -> IO (Either Failure JValue)
processFloat input = runCommand $ do
  spec <- case takeExtension input of
    ".coscad" -> pure (FloatSpec input Nothing Nothing [] "pla" Nothing Nothing [] ("fresh",1) defaultHeels)
    ".json" -> do
      text <- readFileUtf8 input
      either (abort . inputFailure . ("float: " ++)) pure (decode text >>= floatSpec (takeDirectory input))
    _ -> abort (inputFailure "float: expected a .coscad part or a coscad.float/1 .json spec")
  let get = either (abort . inputFailure . ("float: " ++)) pure
  bodyMesh <- loadMesh (fsBody spec)
  let bodyTris = indexedTriangles bodyMesh
      (bodyVol, bodyCentre) = meshVolumeCentre bodyTris
  displacer <- maybe (pure (fillVoids bodyMesh)) (fmap indexedTriangles . loadMesh) (fsDisplacer spec)
  openingPts <- case fsOpening spec of
    Nothing -> pure (fsOpeningPoints spec)
    Just p -> (++ fsOpeningPoints spec) . indexedVertices <$> loadMesh p
  material <- get (materialNamed (fsMaterial spec))
  (partMass, partCentre) <- case fsMass spec of
    Just mc -> pure mc
    Nothing -> do
      c <- maybe (abort (inputFailure "float: body has no finite volume centre")) pure bodyCentre
      let rho = fromMaybe (matDensity material) (fsDensity spec)
      unless (bodyVol > 0) (abort (inputFailure "float: body volume must be positive"))
      pure (bodyVol * rho / 1000, c)
  let payload = fsPayload spec
      mass = partMass + sum (map plMass payload)
      gravity = scale3 (1/mass) (foldl' vadd (scale3 partMass partCentre) [scale3 (plMass p) (plCentre p) | p <- payload])
      (waterName, rhoW) = fsWater spec
      hy = Hydro displacer mass gravity rhoW openingPts
      dispVol = fst (meshVolumeCentre displacer)
      bottom = minimum [z | (a,b,c) <- displacer, (_,_,z) <- [a,b,c]]
  when (null displacer || dispVol <= 0) (abort (inputFailure "float: displacer has no volume"))
  let states = [(h, heelState hy 0 h) | h <- fsHeels spec]
      upright = lookup 0 states >>= id
      rows = [st | (_, Just st) <- states]
      positive = [st | st <- rows, hsHeel st > 0, hsHeel st <= 90]
      best = if null positive then Nothing else Just (foldr1 (\a b -> if hsRighting a >= hsRighting b then a else b) positive)
      vanish = hsHeel <$> find (\st -> hsRighting st <= 0) positive
      flood = hsHeel <$> find (\st -> maybe False (<= 0) (hsOpeningClearance st)) (filter ((>0) . hsHeel) rows)
      dry = [st | st <- positive, maybe True (\f -> hsHeel st < f) flood]
      bestDry = if null dry then Nothing else Just (foldr1 (\a b -> if hsRighting a >= hsRighting b then a else b) dry)
      trim = staticTrim hy
      num = JNumber; mnum = maybe JNull JNumber
      vec (x,y,z) = JArray (map JNumber [x,y,z])
      row (h, Nothing) = JObject [("heel_deg",num h),("floats",JBool False)]
      row (_, Just st) = JObject
        [ ("heel_deg",num (hsHeel st)),("floats",JBool True),("waterline_mm",num (hsWaterline st))
        , ("displaced_g",num (hsDisplacedG st)),("righting_arm_mm",num (hsRighting st))
        , ("opening_clearance_mm",mnum (hsOpeningClearance st))
        , ("buoyancy_center_mm",vec (hsBuoyancy st)),("gravity_center_mm",vec (hsGravity st)) ]
  pure $ JObject
    [ ("schema",JString "coscad.float-report/1")
    , ("source",JString input)
    , ("water",JObject [("name",JString waterName),("density_g_cm3",num rhoW)])
    , ("mass_g",num mass)
    , ("mass_basis",JString (case fsMass spec of
         Just _ -> "given"
         Nothing -> "uniform solid " ++ matName material ++ " at " ++ show (fromMaybe (matDensity material) (fsDensity spec)) ++ " g/cm3"))
    , ("payload",JArray [JObject [("name",JString (plName p)),("mass_g",num (plMass p)),("center_mm",vec (plCentre p))] | p <- payload])
    , ("gravity_center_mm",vec gravity)
    , ("displacer_volume_cm3",num (dispVol/1000))
    , ("reserve_buoyancy_g",num (dispVol/1000*rhoW - mass))
    , ("upright",case upright of
         Nothing -> JObject [("floats",JBool False)]
         Just st -> JObject
           [ ("floats",JBool True),("waterline_mm",num (hsWaterline st)),("draft_mm",num (hsWaterline st - bottom))
           , ("opening_freeboard_mm",mnum (hsOpeningClearance st)),("static_trim_deg_bow_up",mnum trim) ])
    , ("stability",JObject
        [ ("max_righting_arm_mm",mnum (hsRighting <$> best)),("heel_of_max_deg",mnum (hsHeel <$> best))
        , ("first_sampled_heel_with_no_righting_deg",mnum vanish)
        , ("downflooding_heel_deg",mnum flood)
        , ("max_righting_arm_before_downflooding_mm",mnum (hsRighting <$> bestDry))
        , ("heel_of_max_before_downflooding_deg",mnum (hsHeel <$> bestDry))
        , ("righting_arm_inverted_180_mm",mnum (lookup 180 states >>= fmap hsRighting)) ])
    , ("righting_curve",JArray (map row states))
    , ("assumptions",JArray (map JString
        [ "Static equilibrium only: no waves, surf, slamming, dynamic capsize or leakage rate."
        , "Heel is about the x axis (the hull's length) at zero trim; trim is solved upright only."
        , "Mass is uniform solid geometry unless given: printed shells and infill are not inferred."
        , "Sealed internal voids keep water out unless a displacer is given; an opening floods the hull only as a reported downflooding limit." ]))
    ]
  where
    base = takeDirectory input
    loadMesh p = do
      let path = if isAbsolute p || takeExtension input == ".coscad" then p else base </> p
      source <- readFileUtf8 path
      (_, shape, _, _) <- either (abort . inputFailure) pure (parseProgramFull path source)
      either (abort . inputFailure . (("float: " ++ path ++ ": ") ++)) pure =<< nativeIndexedCSGMesh shape

floatSpec :: FilePath -> JValue -> Either String FloatSpec
floatSpec _ root = do
  schema <- req "schema" asString
  unless (schema == "coscad.float/1") (Left "expected schema coscad.float/1")
  let known = ["schema","body","displacer","opening","opening_points","material","density_g_cm3","mass_g","center_mm","payload","water","heel_deg"]
  case root of
    JObject kvs -> case [k | (k,_) <- kvs, k `notElem` known] of
      [] -> pure ()
      ks -> Left ("unknown key(s): " ++ unwords ks)
    _ -> Left "spec must be an object"
  body <- req "body" asString
  displacer <- opt "displacer" asString
  opening <- opt "opening" asString
  pts <- maybe (Right []) (mapM point) =<< opt "opening_points" asArray
  material <- fromMaybe "pla" <$> opt "material" asString
  density <- opt "density_g_cm3" asNumber
  mass <- opt "mass_g" asNumber
  centre <- traverse point (lookupKey "center_mm" root)
  given <- case (mass, centre) of
    (Just m, Just c) | m > 0 && finite m -> Right (Just (m, c))
    (Nothing, Nothing) -> Right Nothing
    _ -> Left "mass_g and center_mm go together, and mass_g must be positive"
  when (density /= Nothing && given /= Nothing) (Left "give density_g_cm3 or mass_g, not both")
  when (maybe False (\d -> not (finite d) || d <= 0) density) (Left "density_g_cm3 must be positive")
  payload <- maybe (Right []) (mapM payloadItem) =<< opt "payload" asArray
  water <- case lookupKey "water" root of
    Nothing -> Right ("fresh", 1)
    Just (JString w) -> maybe (Left "water must be fresh, sea or a density in g/cm3") (\d -> Right (w,d)) (waterDensity w)
    Just (JNumber d) | d > 0 && finite d -> Right ("custom", d)
    _ -> Left "water must be fresh, sea or a density in g/cm3"
  heels <- maybe (Right defaultHeels) (mapM (maybe (Left "heel_deg must be numbers") Right . asNumber)) =<< opt "heel_deg" asArray
  when (null heels || any (\h -> not (finite h)) heels) (Left "heel_deg must be finite numbers")
  pure (FloatSpec body displacer opening pts material density given payload water heels)
  where
    req k f = maybe (Left ("missing or invalid " ++ k)) Right (lookupKey k root >>= f)
    opt k f = case lookupKey k root of
      Nothing -> Right Nothing
      Just JNull -> Right Nothing
      Just v -> maybe (Left ("invalid " ++ k)) (Right . Just) (f v)
    point v = case asArray v >>= mapM asNumber of
      Just [x,y,z] | all finite [x,y,z] -> Right (x,y,z)
      _ -> Left "points must be [x, y, z] in mm"
    payloadItem v = do
      m <- maybe (Left "payload needs mass_g") Right (lookupKey "mass_g" v >>= asNumber)
      unless (m > 0 && finite m) (Left "payload mass_g must be positive")
      c <- maybe (Left "payload needs center_mm") point (lookupKey "center_mm" v)
      pure (Payload (fromMaybe "payload" (lookupKey "name" v >>= asString)) m c)
