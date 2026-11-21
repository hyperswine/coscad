-- | Gear mesh analysis on the real meshes: sweep a driver part through its
-- motion with the driven part linked by a ratio, and at each step report
-- interference, the surface gap, the free play each way (backlash) and how
-- many tooth pairs touch under a small push (the sampled contact ratio).
-- Optionally a Lewis bending check of the teeth from a driving torque.
--
-- Parts are placed exactly as their .coscad files place them, in a common
-- frame. A motion is a rotation in degrees about an axis through a point, or
-- a translation in mm along a direction. The driven part moves `ratio`
-- driven units per driver degree.
module Coscad.Gear
  ( Motion (..), GearSpec (..), gearArgs, processGears, lewisFormFactor, moveMesh
  ) where

import Coscad.Backend (nativeIndexedCSGMesh)
import Coscad.IO (readFileUtf8)
import Coscad.Json
import Coscad.Linalg (V3, mApply, rodMat, scale3, vadd, vlen, vnormed, vsub)
import Coscad.Manifold (MeshOp (MeshIntersection), indexedOperation)
import Coscad.Mesh (meshVolume)
import Coscad.Mesh.Distance (meshDistance)
import Coscad.Mesh.Indexed
import Coscad.Parser (parseProgramFull)
import Coscad.Report
import Coscad.Stress.Material (Material (..), materialNamed)
import Coscad.Validate (finite)
import Control.Monad (forM, unless, when)
import Data.Maybe (fromMaybe, isJust)
import System.FilePath (isAbsolute, takeDirectory, (</>))

data Motion = Rotate V3 V3   -- ^ axis, a point on the axis
            | Translate V3   -- ^ direction (normalised)
  deriving (Eq, Show)

data GearSpec = GearSpec
  { gsDriver :: (FilePath, Motion)
  , gsDriven :: (FilePath, Motion)
  , gsRatio :: Double
  , gsStart :: Double
  , gsSweep :: Double
  , gsSteps :: Int
  , gsTeeth :: Maybe Int
  , gsDrivenTeeth :: Maybe Int       -- ^ 0 for a rack
  , gsModule :: Maybe Double
  , gsFace :: Maybe Double
  , gsPressure :: Double
  , gsTorque :: Maybe Double
  , gsMaterial :: String
  , gsMaxBacklash :: Maybe Double    -- ^ driver degrees
  , gsBacklash :: Bool
  , gsPush :: Maybe Double          -- ^ driven units past first contact when counting pairs
  }

-- Intersections below this are tessellation noise (mm^3).
overlapEps :: Double
overlapEps = 1.0e-4

moveMesh :: Motion -> Double -> IndexedMesh -> Either String IndexedMesh
moveMesh _ 0 m = Right m
moveMesh (Translate d) s m = mapIndexed (vadd (scale3 s d)) m
moveMesh (Rotate axis origin) a m = let r = rodMat a axis in mapIndexed (\p -> vadd origin (mApply r (vsub p origin))) m

unitName :: Motion -> String
unitName (Rotate _ _) = "deg"
unitName (Translate _) = "mm"

-- | Lewis form factor Y for 20 degree full-depth involute teeth, by tooth
-- count (Shigley, table 14-2); 0 or a count over 300 means a rack.
lewisFormFactor :: Int -> Double
lewisFormFactor n
  | n <= 0 || n > 300 = 0.485
  | n <= 12 = 0.245
  | otherwise = interp table
  where
    table = [(12,0.245),(13,0.261),(14,0.277),(15,0.290),(16,0.296),(17,0.303),(18,0.309),(19,0.314),(20,0.322)
            ,(21,0.328),(22,0.331),(24,0.337),(26,0.346),(28,0.353),(30,0.359),(34,0.371),(38,0.384),(43,0.397)
            ,(50,0.409),(60,0.422),(75,0.435),(100,0.447),(150,0.460),(300,0.472)]
    x = fromIntegral n :: Double
    interp ((a,ya):(b,yb):rest)
      | x <= fromIntegral b = ya + (yb-ya) * (x - fromIntegral a) / fromIntegral (b - a :: Int)
      | otherwise = interp ((b,yb):rest)
    interp _ = 0.472

gearArgs :: [String] -> Either String (FilePath, Maybe FilePath)
gearArgs [f] = Right (f, Nothing)
gearArgs [f,"-o",o] = Right (f, Just o)
gearArgs _ = Left "usage: coscad gears <mesh.json> [-o report.json]"

processGears :: FilePath -> IO (Either Failure JValue)
processGears input = runCommand $ do
  text <- readFileUtf8 input
  spec <- either (abort . inputFailure . ("gears: " ++)) pure (decode text >>= gearSpec)
  let (driverFile, driverMotion) = gsDriver spec
      (drivenFile, drivenMotion) = gsDriven spec
      ratio = gsRatio spec
      steps = gsSteps spec
      angles = [gsStart spec + gsSweep spec * fromIntegral i / fromIntegral steps | i <- [0 .. steps - 1]]
      -- driven motion over one driver tooth: the search range for free play
      perTooth = abs ratio * maybe (gsSweep spec) (\z -> 360 / fromIntegral z) (gsTeeth spec)
      push = fromMaybe (0.001 * perTooth) (gsPush spec)
      pitchR = (\m z -> m * fromIntegral z / 2) <$> gsModule spec <*> gsTeeth spec
      toMm deg = (\r -> deg * pi / 180 * r) <$> pitchR
  driver0 <- loadMesh driverFile
  driven0 <- loadMesh drivenFile
  material <- either (abort . inputFailure . ("gears: " ++)) pure (materialNamed (gsMaterial spec))
  let placed theta s = do
        a <- either (abort . inputFailure . ("gears: " ++)) pure (moveMesh driverMotion theta driver0)
        b <- either (abort . inputFailure . ("gears: " ++)) pure (moveMesh drivenMotion (ratio * theta + s) driven0)
        pure (a, b)
      intersect a b = indexedOperation MeshIntersection a b >>= either (abort . Failure 1 . (:[]) . ("gears: kernel: " ++)) pure
      overlapAt theta s = do
        (a, b) <- placed theta s
        m <- intersect a b
        pure (meshVolume (indexedTriangles m), m)
      touching theta s = (> overlapEps) . fst <$> overlapAt theta s
      -- smallest extra driven displacement (sign dir) that makes contact
      freePlay theta dir = do
        let grow s | s > perTooth = pure Nothing
                   | otherwise = do
                       hit <- touching theta (dir * s)
                       if hit then Just <$> bisect 0 s (20 :: Int) else grow (s * 2)
            bisect lo hi 0 = pure ((lo + hi) / 2)
            bisect lo hi n = do
              let mid = (lo + hi) / 2
              hit <- touching theta (dir * mid)
              if hit then bisect lo mid (n - 1) else bisect mid hi (n - 1)
        grow (perTooth / 256)
      pairsAt theta s = do
        (_, m) <- overlapAt theta s
        pure (length [c | c <- indexedComponents m, meshVolume (indexedTriangles c) > overlapEps])
  rows <- forM angles $ \theta -> do
    (vol, _) <- overlapAt theta 0
    (a, b) <- placed theta 0
    let gap = if vol > overlapEps then Nothing else meshDistance (indexedTriangles a) (indexedTriangles b)
    play <- if vol > overlapEps || not (gsBacklash spec) then pure Nothing else do
      fwd <- freePlay theta 1
      back <- freePlay theta (-1)
      pf <- maybe (pure Nothing) (\f -> Just <$> pairsAt theta (f + push)) fwd
      pb <- maybe (pure Nothing) (\f -> Just <$> pairsAt theta (negate (f + push))) back
      pure (Just (fwd, back, pf, pb))
    pure (theta, vol, gap, play)
  let interfering = [t | (t, v, _, _) <- rows, v > overlapEps]
      plays = [(f, bk) | (_, _, _, Just (Just f, Just bk, _, _)) <- rows]
      totals = [(f + bk) / abs ratio | (f, bk) <- plays]          -- driver degrees
      lost = [t | (t, _, _, Just (f, bk, _, _)) <- rows, not (isJust f && isJust bk)]
      pairs sel = [n | (_, _, _, Just q) <- rows, Just n <- [sel q]]
      fwdPairs = pairs (\(_, _, p, _) -> p)
      backPairs = pairs (\(_, _, _, p) -> p)
      mean xs = if null xs then Nothing else Just (fromIntegral (sum xs) / fromIntegral (length xs) :: Double)
      num = JNumber; mnum = maybe JNull JNumber
      stat xs = if null xs then JNull else JObject [("min",num (minimum xs)),("max",num (maximum xs))]
      lewis = do
        torque <- gsTorque spec
        m <- gsModule spec
        z <- gsTeeth spec
        face <- gsFace spec
        let r = m * fromIntegral z / 2
            ft = torque / r
            stress n = ft / (face * m * lewisFormFactor n)
            row label n = JObject [("part",JString label),("teeth",if n == 0 then JString "rack" else num (fromIntegral n))
                                  ,("lewis_y",num (lewisFormFactor n)),("root_stress_mpa",num (stress n))
                                  ,("safety_factor",num (matStrengthMPa material / stress n))]
        pure (JObject [("torque_n_mm",num torque),("tangential_force_n",num ft),("face_mm",num face)
                      ,("material",JString (matName material)),("strength_mpa",num (matStrengthMPa material))
                      ,("teeth",JArray (row "driver" z : maybe [] (\n -> [row "driven" n]) (gsDrivenTeeth spec)))])
      tooLoose = case (gsMaxBacklash spec, totals) of
        (Just limit, _:_) -> maximum totals > limit
        _ -> False
      verdict | not (null interfering) = "interference"
              | not (null lost) = "loses contact"
              | tooLoose = "backlash over limit"
              | otherwise = "ok"
      report = JObject
        [ ("schema",JString "coscad.gear-mesh-report/1")
        , ("source",JString input)
        , ("verdict",JString verdict)
        , ("driver",JObject [("part",JString driverFile),("motion",JString (unitName driverMotion))])
        , ("driven",JObject [("part",JString drivenFile),("motion",JString (unitName drivenMotion))])
        , ("ratio",JObject [("value",num ratio),("unit",JString (unitName drivenMotion ++ " per driver deg"))])
        , ("sweep_deg",JObject [("start",num (gsStart spec)),("span",num (gsSweep spec)),("steps",num (fromIntegral steps))])
        , ("interference_at_deg",JArray (map num interfering))
        , ("max_overlap_mm3",num (maximum (0 : [v | (_, v, _, _) <- rows])))
        , ("gap_mm",stat [g | (_, _, Just g, _) <- rows])
        , ("backlash",JObject
            [ ("driver_deg",stat totals)
            , ("driver_pitch_mm",stat (concatMap (maybe [] pure . toMm) totals))
            , ("driven",stat [(f + bk) | (f, bk) <- plays])
            , ("driven_unit",JString (unitName drivenMotion))
            , ("no_contact_within_one_tooth_at_deg",JArray (map num lost)) ])
        , ("contact_pairs",JObject
            [ ("push",JObject [("driven",num push),("unit",JString (unitName drivenMotion))])
            , ("forward",JObject [("min",maybe JNull (num . fromIntegral) (safeMin fwdPairs)),("mean",mnum (mean fwdPairs))])
            , ("backward",JObject [("min",maybe JNull (num . fromIntegral) (safeMin backPairs)),("mean",mnum (mean backPairs))]) ])
        , ("lewis",fromMaybe JNull lewis)
        , ("steps",JArray [JObject (("driver_deg",num t) : ("overlap_mm3",num v) : ("gap_mm",mnum g) : playFields p) | (t, v, g, p) <- rows])
        , ("assumptions",JArray (map JString
            [ "Rigid parts exactly as modelled: printed size error, tooth deflection and shaft play are not included."
            , "Backlash is the free play of the driven part each way before the meshes intersect, with the driver held."
            , "Contact pairs count separate intersections after pushing past first contact (contact_push, default 0.1% of one tooth's driven motion); the mean approximates the contact ratio but depends on the push and on how finely the tooth flanks are tessellated."
            , "Lewis stress is the classic root bending estimate for 20 degree full-depth teeth with the whole load on one tooth tip; no dynamic or stress-concentration factors." ]))
        ]
  pure report
  where
    base = takeDirectory input
    loadMesh p = do
      let path = if isAbsolute p then p else base </> p
      source <- readFileUtf8 path
      (_, shape, _, _) <- either (abort . inputFailure) pure (parseProgramFull path source)
      either (abort . inputFailure . (("gears: " ++ path ++ ": ") ++)) pure =<< nativeIndexedCSGMesh shape
    safeMin xs = if null xs then Nothing else Just (minimum xs)
    playFields Nothing = []
    playFields (Just (f, bk, pf, pb)) =
      [ ("free_play_forward",maybe JNull JNumber f), ("free_play_backward",maybe JNull JNumber bk)
      , ("pairs_forward",maybe JNull (JNumber . fromIntegral) pf), ("pairs_backward",maybe JNull (JNumber . fromIntegral) pb) ]

gearSpec :: JValue -> Either String GearSpec
gearSpec root = do
  schema <- req "schema" asString
  unless (schema == "coscad.gear-mesh/1") (Left "expected schema coscad.gear-mesh/1")
  let known = ["schema","driver","driven","ratio","start_deg","sweep_deg","steps","teeth","driven_teeth","module_mm"
              ,"face_mm","pressure_deg","torque_n_mm","material","max_backlash_deg","backlash","contact_push"]
  case root of
    JObject kvs -> case [k | (k, _) <- kvs, k `notElem` known] of
      [] -> pure ()
      ks -> Left ("unknown key(s): " ++ unwords ks)
    _ -> Left "spec must be an object"
  driver <- part "driver"
  driven <- part "driven"
  teeth <- optInt "teeth"
  drivenTeeth <- optInt "driven_teeth"
  modul <- optPos "module_mm"
  ratio <- case lookupKey "ratio" root >>= asNumber of
    Just r | finite r && r /= 0 -> Right r
    Just _ -> Left "ratio must be finite and nonzero"
    Nothing -> case (snd driven, teeth, drivenTeeth, modul) of
      (Translate _, Just z, _, Just m) -> Right (pi * m * fromIntegral z / 360)
      (Rotate _ _, Just z, Just z2, _) | z2 > 0 -> Right (negate (fromIntegral z / fromIntegral z2))
      _ -> Left "give ratio, or teeth and module_mm for a rack, or teeth and driven_teeth for a gear pair"
  start <- fromMaybe 0 <$> optNum "start_deg"
  sweep <- case lookupKey "sweep_deg" root >>= asNumber of
    Just s | finite s && s > 0 -> Right s
    Just _ -> Left "sweep_deg must be positive"
    Nothing -> maybe (Left "give sweep_deg or teeth") (\z -> Right (360 / fromIntegral z)) teeth
  steps <- fromMaybe 24 <$> optInt "steps"
  unless (steps >= 1 && steps <= 720) (Left "steps must be 1..720")
  face <- optPos "face_mm"
  pressure <- fromMaybe 20 <$> optNum "pressure_deg"
  torque <- optPos "torque_n_mm"
  material <- maybe (Right "pla") Right =<< opt "material" asString
  maxB <- optPos "max_backlash_deg"
  backlash <- maybe (Right True) Right =<< opt "backlash" asBool
  push <- optPos "contact_push"
  when (isJust torque && not (isJust modul && isJust teeth && isJust face))
    (Left "torque_n_mm needs module_mm, teeth and face_mm for the Lewis check")
  pure (GearSpec driver driven ratio start sweep steps teeth drivenTeeth modul face pressure torque material maxB backlash push)
  where
    req k f = maybe (Left ("missing or invalid " ++ k)) Right (lookupKey k root >>= f)
    opt k f = case lookupKey k root of
      Nothing -> Right Nothing
      Just JNull -> Right Nothing
      Just v -> maybe (Left ("invalid " ++ k)) (Right . Just) (f v)
    optNum k = opt k (\v -> asNumber v >>= \x -> if finite x then Just x else Nothing)
    optPos k = opt k (\v -> asNumber v >>= \x -> if finite x && x > 0 then Just x else Nothing)
    optInt k = opt k (\v -> asNumber v >>= \x -> if x == fromIntegral (round x :: Int) && x >= 0 then Just (round x) else Nothing)
    asBool (JBool b) = Just b
    asBool _ = Nothing
    vec v = case asArray v >>= mapM asNumber of
      Just [x, y, z] | all finite [x, y, z] -> Right (x, y, z)
      _ -> Left "vectors must be [x, y, z]"
    part k = do
      o <- maybe (Left ("missing " ++ k)) Right (lookupKey k root)
      file <- maybe (Left (k ++ " needs part")) Right (lookupKey "part" o >>= asString)
      motion <- case (lookupKey "rotate" o, lookupKey "translate" o) of
        (Just r, Nothing) -> do
          axis <- maybe (Left (k ++ ".rotate needs axis")) vec (lookupKey "axis" r)
          origin <- maybe (Right (0, 0, 0)) vec (lookupKey "origin" r)
          unless (vlen axis > 0) (Left (k ++ ".rotate axis must be nonzero"))
          Right (Rotate (vnormed axis) origin)
        (Nothing, Just t) -> do
          d <- vec t
          unless (vlen d > 0) (Left (k ++ ".translate must be nonzero"))
          Right (Translate (vnormed d))
        _ -> Left (k ++ " needs exactly one of rotate or translate")
      pure (file, motion)
