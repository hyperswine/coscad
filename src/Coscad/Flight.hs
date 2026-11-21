{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- | A lightweight flight model: a vortex-lattice solution (horseshoe vortices
-- on the mean surfaces) of every lifting surface together, then static
-- longitudinal stability, trim and a glide estimate.
--
-- Frame: x aft (downstream), y to the right, z up, millimetres. Each
-- surface is a list of sections (leading edge point, chord, incidence);
-- positive incidence raises the leading edge. A section's zero-lift angle
-- stands in for camber: the panel is solved as a flat plate at incidence
-- minus that angle. Pitching moment is positive nose up.
--
-- Method (Katz and Plotkin, ch. 12): bound vortex on each panel's quarter
-- chord, collocation at three quarters, trailing legs along +x. The linear
-- system is solved twice, for a freestream along x and along z, so every
-- coefficient is linear in angle of attack. Induced drag comes from the
-- Trefftz plane; profile drag from each surface's cd0 and a parasite area.
module Coscad.Flight
  ( Section (..), Surface (..), FlightSpec (..), Lattice (..), Panel (..)
  , lattice, solveLattice, luSolve, flightArgs, processFlight, flightSpec
  ) where

import Coscad.IO (readFileUtf8)
import Coscad.Json
import Coscad.Linalg (V3, cross, dot3, scale3, vadd, vlen, vsub)
import Coscad.Mass (combineMass, componentManifest)
import Coscad.Report
import Coscad.Validate (finite)
import Control.Monad (unless, when)
import Control.Monad.ST (ST, runST)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.ST (STUArray, newListArray, getElems)
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import System.FilePath (isAbsolute, takeDirectory, (</>))

data Section = Section { secLe :: V3, secChord :: Double, secIncidence :: Double } deriving (Eq, Show)

data Surface = Surface
  { surfName :: String
  , surfSections :: [Section]     -- ^ ordered along the span
  , surfMirror :: Bool            -- ^ reflect across y = 0
  , surfZeroLift :: Double        -- ^ deg
  , surfCm0 :: Double
  , surfCd0 :: Double
  , surfClMax :: Double
  } deriving (Eq, Show)

data FlightSpec = FlightSpec
  { flSurfaces :: [Surface]
  , flMass :: Double              -- ^ g
  , flCg :: V3
  , flRho :: Double               -- ^ kg/m^3
  , flParasite :: Double          -- ^ mm^2 of drag area (CD * area) outside the surfaces
  , flChordwise :: Int
  , flPanel :: Double             -- ^ target spanwise panel width, mm
  }

data Panel = Panel
  { pSurface :: Int
  , pStrip :: Int                 -- ^ global spanwise strip index
  , pA :: V3, pB :: V3            -- ^ bound vortex endpoints
  , pControl :: V3
  , pNormal :: V3
  , pArea :: Double               -- ^ projected on the xy plane
  , pChord :: Double              -- ^ local chord of its strip
  , pWidth :: Double              -- ^ strip width along the bound vortex, projected on yz
  , pLeadX :: Double              -- ^ x of the strip's leading edge at mid-strip
  , pHorizontal :: Bool           -- ^ the strip spans y more than z
  }

data Lattice = Lattice { latPanels :: [Panel], latLegLength :: Double }

-- ------------------------------------------------------------------
-- geometry

chordVector :: Double -> Double -> V3
chordVector c incDeg = let a = incDeg * pi / 180 in (c * cos a, 0, negate (c * sin a))

-- | Panels for every surface, both halves of a mirrored one, ordered so
-- each strip's bound vortex runs toward +y (or +z on a vertical surface).
lattice :: Int -> Double -> [Surface] -> Lattice
lattice nc panelW surfaces = Lattice (concat (zipWith3 surfPanels [0 ..] stripStarts halves)) legLength
  where
    halves = map expand surfaces
    expand s =
      let secs = [sec {secIncidence = secIncidence sec - surfZeroLift s} | sec <- surfSections s]
          mirrored = reverse [sec {secLe = (\(x, y, z) -> (x, negate y, z)) (secLe sec)} | sec <- secs]
          ordered = if surfMirror s then dropRoot mirrored ++ secs else secs
          dropRoot ms = case (reverse ms, secs) of
            (lastM : restM, firstS : _) | secLe lastM == secLe firstS -> reverse restM
            _ -> ms
      in orientPlusY ordered
    orientPlusY ss = case ss of
      (a : _ : _) | spanDir (secLe (last ss)) (secLe a) < 0 -> reverse ss
      _ -> ss
    spanDir (_, y1, z1) (_, y0, z0) = if abs (y1 - y0) >= abs (z1 - z0) then y1 - y0 else z1 - z0
    segCounts ss = [max 1 (ceiling (vlen (yz (vsub (secLe b) (secLe a))) / panelW)) | (a, b) <- zip ss (drop 1 ss)]
    stripStarts = scanl (+) 0 [sum (segCounts ss) | ss <- halves]
    allY = concat [[y | Section (_, y, _) _ _ <- ss] | ss <- halves]
    legLength = 1.0e5 * max 1 (maximum (1 : allY) - minimum (0 : allY))
    yz (_, y, z) = (0, y, z)
    surfPanels si start ss =
      let segs = zip ss (drop 1 ss)
          strips = concat [[(a, b, spacing k n, spacing (k + 1) n) | k <- [0 .. n - 1 :: Int]]
                          | ((a, b), n) <- zip segs (segCounts ss)]
      in concat [stripPanels si (start + j) st | (j, st) <- zip [0 ..] strips]
    -- cosine spacing bunches strips toward both ends of each segment, where
    -- the loading changes fastest (tips, roots, kinks)
    spacing k n = (1 - cos (pi * fromIntegral k / fromIntegral n)) / 2
    stripPanels si strip (a, b, t0, t1) =
      let lerp t = (vadd (secLe a) (scale3 t (vsub (secLe b) (secLe a))),
                    let ca = chordVector (secChord a) (secIncidence a); cb = chordVector (secChord b) (secIncidence b)
                    in vadd ca (scale3 t (vsub cb ca)))
          at t s = let (le, cv) = lerp t in vadd le (scale3 s cv)
          tm = (t0 + t1) / 2
          chord = vlen (snd (lerp tm))
          ds = 1 / fromIntegral nc
          width = vlen (yz (vsub (at t1 0) (at t0 0)))
          (leadX, _, _) = at tm 0
          horizontal = let (_, ya, za) = at t0 0; (_, yb, zb) = at t1 0 in abs (yb - ya) >= abs (zb - za)
      in [ Panel si strip (at t0 q) (at t1 q) (at tm (s0 + 0.75 * ds)) (normal p00 p10 p11 p01) (areaXY p00 p10 p11 p01) chord width leadX horizontal
         | k <- [0 .. nc - 1]
         , let s0 = fromIntegral k * ds; s1 = s0 + ds; q = s0 + 0.25 * ds
               p00 = at t0 s0; p10 = at t1 s0; p11 = at t1 s1; p01 = at t0 s1 ]
    normal p00 p10 p11 p01 =
      let d1 = vsub p11 p00; d2 = vsub p01 p10; n = cross d2 d1; l = vlen n
      in if l > 0 then scale3 (1 / l) n else (0, 0, 1)
    areaXY p00 p10 p11 p01 =
      let flat (x, y, _) = (x, y, 0); (_, _, zc) = cross (vsub (flat p11) (flat p00)) (vsub (flat p01) (flat p10))
      in abs zc / 2

-- Biot-Savart velocity of a unit-strength straight segment a->b at p.
segment :: V3 -> V3 -> V3 -> V3
segment a b p =
  let r1 = vsub p a; r2 = vsub p b; r0 = vsub b a
      c = cross r1 r2; c2 = dot3 c c
      l1 = vlen r1; l2 = vlen r2
  in if c2 < 1.0e-12 * max 1 (dot3 r0 r0) || l1 < 1.0e-9 || l2 < 1.0e-9 then (0, 0, 0)
     else scale3 (dot3 r0 (vsub (scale3 (1 / l1) r1) (scale3 (1 / l2) r2)) / (4 * pi * c2)) c

horseshoe :: Double -> Panel -> V3 -> V3
horseshoe legL pn p =
  let a = pA pn; b = pB pn; far q = vadd q (legL, 0, 0)
  in vadd (segment (far a) a p) (vadd (segment a b p) (segment b (far b) p))

trailing :: Double -> Panel -> V3 -> V3
trailing legL pn p = let a = pA pn; b = pB pn; far q = vadd q (legL, 0, 0) in vadd (segment (far a) a p) (segment b (far b) p)

-- | Dense LU with partial pivoting; several right-hand sides. Row-major,
-- tight loops on an unboxed array: about a second for 1500 unknowns.
luSolve :: Int -> [Double] -> [[Double]] -> Either String [[Double]]
luSolve n entries rhss = runST $ do
  m <- newListArray (0, n * n - 1) entries :: ST s (STUArray s Int Double)
  piv <- newListArray (0, n - 1) [0 .. n - 1] :: ST s (STUArray s Int Int)
  ok <- factor m piv 0
  if not ok then pure (Left "flight: singular vortex-lattice system (coincident or degenerate panels)") else do
    order <- getElems piv
    sols <- mapM (solveOne m order) rhss
    pure (Right sols)
  where
    factor :: STUArray s Int Double -> STUArray s Int Int -> Int -> ST s Bool
    factor m piv k
      | k >= n = pure True
      | otherwise = do
          let search !i !bi !bv
                | i >= n = pure (bi, bv)
                | otherwise = do
                    v <- abs <$> unsafeRead m (i * n + k)
                    if v > bv then search (i + 1) i v else search (i + 1) bi bv
          (p, best) <- search k k (-1)
          if best < 1.0e-14 then pure False else do
            when (p /= k) $ do
              let swap !j | j >= n = pure ()
                          | otherwise = do
                              a <- unsafeRead m (k * n + j); b <- unsafeRead m (p * n + j)
                              unsafeWrite m (k * n + j) b; unsafeWrite m (p * n + j) a; swap (j + 1)
              swap 0
              pk <- unsafeRead piv k; pp <- unsafeRead piv p
              unsafeWrite piv k pp; unsafeWrite piv p pk
            d <- unsafeRead m (k * n + k)
            let rows !i | i >= n = pure ()
                        | otherwise = do
                            f0 <- unsafeRead m (i * n + k)
                            let f = f0 / d
                            unsafeWrite m (i * n + k) f
                            let cols !j | j >= n = pure ()
                                        | otherwise = do
                                            a <- unsafeRead m (i * n + j); b <- unsafeRead m (k * n + j)
                                            unsafeWrite m (i * n + j) (a - f * b); cols (j + 1)
                            when (f /= 0) (cols (k + 1))
                            rows (i + 1)
            rows (k + 1)
            factor m piv (k + 1)
    solveOne :: STUArray s Int Double -> [Int] -> [Double] -> ST s [Double]
    solveOne m order rhs = do
      let b = listArray (0, n - 1) rhs :: UArray Int Double
      x <- newListArray (0, n - 1) [b ! p | p <- order] :: ST s (STUArray s Int Double)
      let fwd !i | i >= n = pure ()
                 | otherwise = do
                     let acc !j !s | j >= i = pure s
                                   | otherwise = do
                                       a <- unsafeRead m (i * n + j); xj <- unsafeRead x j; acc (j + 1) (s + a * xj)
                     s <- acc 0 0
                     xi <- unsafeRead x i
                     unsafeWrite x i (xi - s); fwd (i + 1)
          back !i | i < 0 = pure ()
                  | otherwise = do
                      let acc !j !s | j >= n = pure s
                                    | otherwise = do
                                        a <- unsafeRead m (i * n + j); xj <- unsafeRead x j; acc (j + 1) (s + a * xj)
                      s <- acc (i + 1) 0
                      xi <- unsafeRead x i; d <- unsafeRead m (i * n + i)
                      unsafeWrite x i ((xi - s) / d); back (i - 1)
      fwd 1
      back (n - 1)
      getElems x

-- | Circulations for a unit freestream along x and along z.
solveLattice :: Lattice -> Either String ([Double], [Double])
solveLattice (Lattice ps legL) = do
  let n = length ps
      entries = [dot3 (horseshoe legL pj (pControl pi')) (pNormal pi') | pi' <- ps, pj <- ps]
      rhs v = [negate (dot3 v (pNormal p)) | p <- ps]
  when (n == 0) (Left "flight: no panels")
  when (n > 4000) (Left ("flight: " ++ show n ++ " panels is more than 4000; raise panel_mm or lower chordwise_panels"))
  sols <- luSolve n entries [rhs (1, 0, 0), rhs (0, 0, 1)]
  case sols of
    [g0, g1] -> Right (g0, g1)
    _ -> Left "flight: solver returned the wrong number of solutions"

-- ------------------------------------------------------------------
-- command

flightArgs :: [String] -> Either String (FilePath, Maybe FilePath)
flightArgs [f] = Right (f, Nothing)
flightArgs [f, "-o", o] = Right (f, Just o)
flightArgs _ = Left "usage: coscad flight <flight.json> [-o report.json]"

processFlight :: FilePath -> IO (Either Failure JValue)
processFlight input = runCommand $ do
  text <- readFileUtf8 input
  root <- either (abort . inputFailure . ("flight: " ++)) pure (decode text)
  massFile <- case lookupKey "mass_components" root >>= asString of
    Nothing -> pure Nothing
    Just p -> Just <$> readFileUtf8 (if isAbsolute p then p else takeDirectory input </> p)
  spec <- either (abort . inputFailure . ("flight: " ++)) pure (flightSpec root massFile)
  let surfaces = flSurfaces spec
      lat = lattice (flChordwise spec) (flPanel spec) surfaces
      ps = latPanels lat
      legL = latLegLength lat
  (g0, g1) <- either (abort . Failure 1 . (: [])) pure (solveLattice lat)
  let indexed = zip [0 :: Int ..] ps
      gArr g = listArray (0, length g - 1) g :: UArray Int Double
      -- strips: (first panel, panel indices), in strip order
      stripMap = Map.fromListWith (flip (++)) [(pStrip p, [(i, p)]) | (i, p) <- indexed]
      strips = [(p0, map fst q) | q@((_, p0) : _) <- Map.elems stripMap]
      mainStrips = [(p0, is) | (p0, is) <- strips, pSurface p0 == 0]
      mainPanels = [p | p <- ps, pSurface p == 0]
      sRef = sum (map pArea mainPanels)
      chordWeight = sum [pChord p0 * pWidth p0 | (p0, _) <- mainStrips]
      mac = sum [pChord p0 * pChord p0 * pWidth p0 | (p0, _) <- mainStrips] / chordWeight
      macLe = sum [pLeadX p0 * pChord p0 * pWidth p0 | (p0, _) <- mainStrips] / chordWeight
      ys = concat [[y1, y2] | p <- mainPanels, let (_, y1, _) = pA p; (_, y2, _) = pB p]
      spanRef = maximum ys - minimum ys
      ar = spanRef * spanRef / sRef
      cg = flCg spec
      surfaceOf p = surfaces !! pSurface p
      -- force on each bound vortex for circulation g, unit freestream along x, rho = 1
      force g = [scale3 gi (cross (1, 0, 0) (vsub (pB p) (pA p))) | (gi, p) <- zip g ps]
      mid p = scale3 0.5 (vadd (pA p) (pB p))
      liftOf g = sum [fz | (_, _, fz) <- force g]
      pitchOf g = sum [let (x, _, z) = vsub (mid p) cg; (fx, _, fz) = f in z * fx - x * fz | (f, p) <- zip (force g) ps]
      q0 = 0.5 :: Double
      cl g = liftOf g / (q0 * sRef)
      cmCamber = sum [surfCm0 (surfaceOf p0) * pChord p0 * pChord p0 * pWidth p0 | (p0, _) <- strips, pHorizontal p0] / (sRef * mac)
      cm g = pitchOf g / (q0 * sRef * mac)
      cl0 = cl g0; cla = cl g1               -- per radian
      cm0 = cm g0 + cmCamber; cma = cm g1
      (cgX, _, _) = cg
      xNp = cgX - mac * cma / cla
      margin = (xNp - cgX) / mac
      alphaTrim = if cma /= 0 then negate cm0 / cma else 0 / 0
      clTrim = cl0 + cla * alphaTrim
      gTrim = zipWith (\a b -> a + alphaTrim * b) g0 g1
      -- Trefftz-plane induced drag
      trefftz p = let (_, y, z) = mid p in (legL / 2, y, z)
      wT g p = foldl' vadd (0, 0, 0) [scale3 gj (trailing legL pj (trefftz p)) | (gj, pj) <- zip g ps]
      inducedDrag g = negate 0.5 * sum [gi * dot3 (wT g p) (cross (1, 0, 0) (vsub (pB p) (pA p))) | (gi, p) <- zip g ps]
      cdi = inducedDrag gTrim / (q0 * sRef)
      -- span efficiency of the loading that angle of attack adds
      e = let d = inducedDrag g1 / (q0 * sRef) in if d > 0 then cla * cla / (pi * ar * d) else 0 / 0
      cd0 = sum [surfCd0 (surfaceOf p) * pArea p | p <- ps] / sRef + flParasite spec / sRef
      cdTotal = cd0 + cdi
      massKg = flMass spec / 1000
      sM2 = sRef * 1.0e-6
      speed = sqrt (2 * massKg * 9.80665 / (flRho spec * sM2 * clTrim))
      ld = clTrim / cdTotal
      sink = speed / ld
      -- local section lift coefficient at trim: cl = 2 G / c for a unit freestream
      gt = gArr gTrim
      localCl = [ (surfName (surfaceOf p0), y, 2 * sum [gt ! i | i <- is] / pChord p0, surfClMax (surfaceOf p0))
                | (p0, is) <- strips, pHorizontal p0, let (_, y, _) = mid p0 ]
      worst = case localCl of
        [] -> Nothing
        _ -> Just (foldr1 (\a@(_, _, ca, ma) b@(_, _, cb, mb) -> if ca / ma >= cb / mb then a else b) localCl)
      stalls = maybe False (\(_, _, c, m) -> c > m) worst
      surfaceSlope i = sum [fz | ((_, _, fz), p) <- zip (force g1) ps, pSurface p == i] / (q0 * sRef)
      verdict
        | not (finite cla) || cla <= 0 = "no lift"
        | cma >= 0 = "unstable"
        | not (finite clTrim) || clTrim <= 0 = "no positive trim"
        | stalls = "stalls at trim"
        | otherwise = "ok"
      num x = if finite x then JNumber x else JNull
      deg r = r * 180 / pi
      vec (x, y, z) = JArray (map num [x, y, z])
  pure $ JObject
    [ ("schema", JString "coscad.flight-report/1")
    , ("source", JString input)
    , ("verdict", JString verdict)
    , ("panels", num (fromIntegral (length ps)))
    , ("reference", JObject [("surface", JString (case surfaces of { s0 : _ -> surfName s0; [] -> "" })), ("area_mm2", num sRef), ("span_mm", num spanRef)
                            , ("mac_mm", num mac), ("mac_le_x_mm", num macLe), ("aspect_ratio", num ar)])
    , ("mass_g", num (flMass spec)), ("cg_mm", vec cg)
    , ("stability", JObject
        [ ("cl_alpha_per_rad", num cla), ("cl_at_zero_alpha", num cl0)
        , ("cm_alpha_per_rad", num cma), ("cm_at_zero_alpha", num cm0)
        , ("neutral_point_x_mm", num xNp), ("static_margin", num margin), ("span_efficiency", num e)
        , ("neutral_point_fraction_mac", num ((xNp - macLe) / mac))
        , ("cg_fraction_mac", num (let (x, _, _) = cg in (x - macLe) / mac))
        , ("lift_slope_by_surface_per_rad", JObject [(surfName s, num (surfaceSlope i)) | (i, s) <- zip [0 ..] surfaces]) ])
    , ("trim", JObject
        [ ("alpha_deg", num (deg alphaTrim)), ("cl", num clTrim), ("speed_m_s", num speed)
        , ("cd_induced", num cdi), ("cd_profile", num cd0), ("cd", num cdTotal)
        , ("glide_ratio", num ld), ("glide_angle_deg", num (deg (atan (1 / ld)))), ("sink_m_s", num sink)
        , ("wing_loading_g_dm2", num (flMass spec / (sRef / 1.0e4))) ])
    , ("worst_section", case worst of
        Nothing -> JNull
        Just (n, y, c, m) -> JObject [("surface", JString n), ("y_mm", num y), ("cl", num c), ("cl_max", num m)])
    , ("assumptions", JArray (map JString
        [ "Inviscid, linear vortex lattice on the mean surfaces; thickness, fuselage lift and viscous lift loss are not modelled."
        , "Camber enters only through each surface's zero-lift angle and cm0; profile drag is a constant cd0 per surface plus the parasite area."
        , "Trailing vortices run straight aft along +x; the tail sees the resulting downwash, but wake roll-up and deflection are ignored."
        , "Static longitudinal trim at zero sideslip; dynamic stability, stall progression and Reynolds-number effects are not covered."
        , "Worst section compares the local cl at trim with the surface's cl_max; above 1 means that part of the span stalls first." ]))
    ]

flightSpec :: JValue -> Maybe String -> Either String FlightSpec
flightSpec root massText = do
  schema <- maybe (Left "missing schema") Right (lookupKey "schema" root >>= asString)
  unless (schema == "coscad.flight/1") (Left "expected schema coscad.flight/1")
  let known = ["schema", "surfaces", "mass_g", "cg_mm", "mass_components", "rho_kg_m3", "parasite_drag_area_mm2", "chordwise_panels", "panel_mm", "frame"]
  case root of
    JObject kvs -> case [k | (k, _) <- kvs, k `notElem` known] of
      [] -> pure ()
      ks -> Left ("unknown key(s): " ++ unwords ks)
    _ -> Left "spec must be an object"
  surfacesJ <- maybe (Left "missing surfaces") Right (lookupKey "surfaces" root >>= asArray)
  surfaces <- mapM surface surfacesJ
  when (null surfaces) (Left "give at least one surface; the first is the reference")
  (mass, cg) <- case (massText, lookupKey "mass_g" root >>= asNumber, lookupKey "cg_mm" root) of
    (Just t, Nothing, Nothing) -> decode t >>= componentManifest >>= combineMass
    (Nothing, Just m, Just c) | m > 0 && finite m -> (,) m <$> vec c
    _ -> Left "give mass_g with cg_mm, or mass_components (a coscad.mass-components/1 file in this frame)"
  rho <- pos "rho_kg_m3" 1.225
  parasite <- nonneg "parasite_drag_area_mm2" 0
  nc <- case lookupKey "chordwise_panels" root >>= asNumber of
    Nothing -> Right 6
    Just v | v >= 1 && v <= 40 && v == fromIntegral (round v :: Int) -> Right (round v)
    _ -> Left "chordwise_panels must be 1..40"
  panel <- pos "panel_mm" 0
  let spanOf s = let ys = [y | Section (_, y, _) _ _ <- surfSections s] in (if surfMirror s then 2 else 1) * (maximum (0 : ys) - minimum (0 : ys))
      panelW = if panel > 0 then panel else case surfaces of
        s0 : _ -> max 1 (spanOf s0 / 64)
        [] -> 8
  pure (FlightSpec surfaces mass cg rho parasite nc panelW)
  where
    num k d = maybe (Right d) (\v -> maybe (Left ("invalid " ++ k)) Right (asNumber v)) (lookupKey k root)
    pos k d = num k d >>= \x -> if finite x && x >= 0 && (x > 0 || x == d) then Right x else Left (k ++ " must be positive")
    nonneg k d = num k d >>= \x -> if finite x && x >= 0 then Right x else Left (k ++ " must not be negative")
    vec v = case asArray v >>= mapM asNumber of
      Just [x, y, z] | all finite [x, y, z] -> Right (x, y, z)
      _ -> Left "points must be [x, y, z] in mm"
    surface v = do
      name <- maybe (Left "surface needs name") Right (lookupKey "name" v >>= asString)
      secsJ <- maybe (Left (name ++ " needs sections")) Right (lookupKey "sections" v >>= asArray)
      secs <- mapM (section name) secsJ
      when (length secs < 2) (Left (name ++ " needs at least two sections"))
      let f k d = maybe (Right d) (\x -> maybe (Left (name ++ ": invalid " ++ k)) Right (asNumber x)) (lookupKey k v)
      mirror <- case lookupKey "mirror" v of
        Nothing -> Right False
        Just (JBool b) -> Right b
        _ -> Left (name ++ ": mirror must be true or false")
      zl <- f "zero_lift_deg" 0
      cm0 <- f "cm0" 0
      cd0 <- f "cd0" 0.02
      clmax <- f "cl_max" 1.0
      unless (all finite [zl, cm0, cd0, clmax] && cd0 >= 0 && clmax > 0) (Left (name ++ ": coefficients must be finite, cd0 >= 0, cl_max > 0"))
      pure (Surface name secs mirror zl cm0 cd0 clmax)
    section name v = do
      le <- maybe (Left (name ++ ": section needs le")) vec (lookupKey "le" v)
      c <- maybe (Left (name ++ ": section needs chord")) Right (lookupKey "chord" v >>= asNumber)
      unless (finite c && c > 0) (Left (name ++ ": chord must be positive"))
      i <- maybe (Right 0) (\x -> maybe (Left (name ++ ": invalid incidence_deg")) Right (asNumber x)) (lookupKey "incidence_deg" v)
      pure (Section le c i)
