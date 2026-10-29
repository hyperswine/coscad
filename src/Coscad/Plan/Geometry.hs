-- | `coscad plan`, part 1b: the geometry. Meshes every part with the
-- native kernel and measures, once, everything the search asks about
-- shapes: real mass and centre of mass, bench footprints and part-on-part
-- contact patches, every way each part can come in (swept against every
-- other part at its final pose), the alignment work of each joint, the room
-- left for fingers, and how far a screw joint turns and what its rotation
-- would hit. The search then only looks these up.
--
-- All tolerances come from the plan settings: `contact` is the distance
-- that counts as touching, `sweep_step` the finest step of a sweep, and
-- `finger_size` the fingertip. Each sweep advances conservatively: a part
-- that is g mm from another can move g mm without touching it, so the step
-- is the gap (never below `sweep_step`); where the parts touch, the overlap
-- volume decides whether the motion pushes one into the other.
module Coscad.Plan.Geometry (prepareGeometry) where

import Coscad.Plan.Model

import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.MVar
import Control.Exception (try)
import Control.Monad (foldM, forM)
import System.IO.Error (ioeGetErrorString, tryIOError)
import Coscad.Backend (nativeIndexedCSGMesh)
import Coscad.Geometry
import Coscad.Mesh (triArea)
import Coscad.Mesh.Indexed
import Coscad.Solid
import Data.IORef
import GHC.Clock (getMonotonicTime)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (intercalate, nubBy)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- | Everything the queries need about one placed instance.
data Placed = Placed
  { plInst :: Inst
  , plSolid :: Solid
  , plPose :: Pose
  , plVerts :: [V3] -- world vertices
  , plBox :: BBox -- tight world box of the mesh
  }

type Cache = IORef (Map.Map [Integer] Double)

prepareGeometry :: Model -> IO (Either String Model)
prepareGeometry m = do
  meshed <- meshParts (mInsts m)
  case meshed of
    Left e -> return (Left e)
    Right byPart -> do
      cache <- newIORef Map.empty
      measure m cache byPart

meshParts :: [Inst] -> IO (Either String (Map.Map String (IndexedMesh, Solid)))
meshParts insts = foldM step (Right Map.empty) insts
  where
    step (Left e) _ = return (Left e)
    step (Right acc) i
      | Map.member (iPart i) acc = return (Right acc)
      | otherwise = do
          r <- nativeIndexedCSGMesh (iShape i)
          case r of
            Left e -> return (Left ("plan: cannot mesh part '" ++ iPart i ++ "': " ++ e))
            Right mesh | null (indexedFaces mesh) -> return (Left ("plan: part '" ++ iPart i ++ "' has an empty mesh"))
            Right mesh -> do
              s <- solidFromMesh mesh
              return (fmap (\sol -> Map.insert (iPart i) (mesh, sol) acc) s)

-- Raise on the first kernel error; the plan command reports it.
orFail :: IO (Either String a) -> IO a
orFail act = act >>= either (ioError . userError) return

measure :: Model -> Cache -> Map.Map String (IndexedMesh, Solid) -> IO (Either String Model)
measure m cache byPart = do
  let sets = mSettings m
      ctol = setContact sets
  -- every instance posed once in the kernel; motions are then relative to
  -- that pose (pure translations stay cheap for the kernel's collision tree)
  posedSolids <- forM (mInsts m) $ \i -> solidPosed (snd (byPart Map.! iPart i)) (iAff i)
  case sequence posedSolids of
    Left e -> return (Left ("plan: " ++ e))
    Right sols -> do
      -- the queries below run on several threads: build every tree first
      _ <- parallel [solidPrepare sol ctol | sol <- sols]
      measurePosed m cache byPart sets ctol sols

measurePosed :: Model -> Cache -> Map.Map String (IndexedMesh, Solid) -> Settings -> Double -> [Solid] -> IO (Either String Model)
measurePosed m cache byPart sets ctol sols = do
  let placedOf i sol =
        let (mesh, _) = byPart Map.! iPart i
            vs = map (affApply (iAff i)) (indexedVertices mesh)
         in Placed i sol identityPose vs (fromCorners vs)
      pls = zipWith placedOf (mInsts m) sols
      -- mass and centre of mass from the world mesh
      massed = [ withMass sets (fst (byPart Map.! iPart (plInst p))) p | p <- pls ]
      insts' = map fst massed
      pls' = [p {plInst = i} | (p, (i, _)) <- zip pls massed]
      byName' = Map.fromList [(iName (plInst p), p) | p <- pls']
      ext = Map.fromList [((iName (plInst p), k), (minimum ds, maximum ds)) | p <- pls', (k, a) <- zip [0 ..] axes6, let ds = map (dot3 a) (plVerts p)]
      foot = Map.fromList
        [ ((iName (plInst p), k), convexHull [project a v | v <- plVerts p, dot3 a v >= hi - ctol])
        | p <- pls', (k, a) <- zip [0 ..] axes6, let hi = maximum (map (dot3 a) (plVerts p)) ]
  result <- tryIO $ do
    contacts <- contactPatches sets cache pls'
    dbg0 <- lookupEnv "COSCAD_PLAN_DEBUG"
    approaches <- parallel $ flip map pls' $ \p -> do
      t0 <- getMonotonicTime
      as <- approachesOf m sets cache byName' p
      t1 <- getMonotonicTime
      (gq, oq, tg, to) <- readIORef queryCount
      if dbg0 == Just "progress" then hPutStrLn stderr ("  approaches of " ++ iName (plInst p) ++ ": " ++ show (length as) ++ " ways, " ++ show (t1 - t0) ++ " s; so far " ++ show gq ++ " gaps " ++ show tg ++ " s, " ++ show oq ++ " overlaps " ++ show to ++ " s") else return ()
      return (iName (plInst p), as)
    dbg <- lookupEnv "COSCAD_PLAN_DEBUG"
    (gq, oq, tg, to) <- readIORef queryCount
    -- COSCAD_PLAN_DEBUG=progress prints per-part timings and query counts;
    -- COSCAD_PLAN_DEBUG=<instance> prints that instance's approach table
    if dbg == Nothing then return () else
      hPutStrLn stderr ("plan geometry: " ++ show gq ++ " gap queries (" ++ show tg ++ " s) and " ++ show oq ++ " overlap queries (" ++ show to ++ " s) for approaches")
    case dbg of
      Just who -> sequence_ [ hPutStrLn stderr (who ++ " out " ++ show (apDir a) ++ ": " ++ show [(q, swBlocked sw, swBits sw) | (q, sw) <- Map.toList (apObstacles a)] ++ " zone " ++ show (apZoneVol a, apZoneNeed a, Map.toList (apZone a)))
                            | (n, as) <- approaches, n == who, a <- as ]
      Nothing -> return ()
    twists <- twistsOf m sets byName'
    let approachMap = Map.fromList approaches
        jointBits = Map.fromList
          [ (fId f, minimum (1e9 : bitsFor (fClamped f) (fHost f) ++ bitsFor (fHost f) (fClamped f)))
          | f <- mFasts m ]
        bitsFor mover other =
          [ swBits sw | a <- Map.findWithDefault [] mover approachMap, Just sw <- [Map.lookup other (apObstacles a)], not (swBlocked sw) ]
        geo = Geo True ext foot contacts approachMap twists (Map.map (\b -> if b >= 1e9 then 0 else b) jointBits)
        byNameI = Map.fromList [(iName i, i) | i <- insts']
        m' = m {mInsts = insts', mByName = byNameI, mGeo = geo}
    either (ioError . userError) return (inferOrder m')
  return result
  where
    tryIO act = either (\e -> Left (ioeGetErrorString e)) Right <$> try act

-- ------------------------------------------------------------------
-- order from geometry

-- | Parts that must come later because they would bury another. Once all
-- of p's certain predecessors are placed, p still has some ways in; a
-- part q that alone closes every one of them (blocks the path, takes the
-- finger room, or is in the way of p turning on its screw joint) must
-- come after p. Repeated until nothing changes; a cycle is a design that
-- rigid parts cannot be put together in any order.
inferOrder :: Model -> Either String Model
inferOrder m = go (mPreds m) (0 :: Int)
  where
    insts = mInsts m
    go preds k
      | k > length insts = Right m {mPreds = preds}
      | otherwise = do
          new <- concat <$> mapM (edgesFor preds) insts
          let fresh = [(q, p) | (q, p) <- new, NPart p `notElem` Map.findWithDefault [] (NPart q) preds]
          if null fresh then Right m {mPreds = preds} else do
            let preds' = foldr (\(q, p) -> Map.insertWith (++) (NPart q) [NPart p]) preds fresh
            case cycleOf preds' of
              Just cyc -> Left ("design: these parts each block another's way in, so no order puts them together: " ++ intercalate " before " (cyc ++ take 1 cyc)
                                ++ " (" ++ reasons cyc ++ ")")
              Nothing -> go preds' (k + 1)
    before preds n = closure preds (Map.findWithDefault [] (NPart n) preds) Set.empty
    closure preds stack seen = case stack of
      [] -> Set.fromList [x | NPart x <- Set.toList seen]
      x : xs | Set.member x seen -> closure preds xs seen
             | otherwise -> closure preds (Map.findWithDefault [] x preds ++ xs) (Set.insert x seen)
    -- (q, p): q must come after p
    edgesFor preds p = do
      let n = iName p
          earlier = before preds n
          screwAxes = [if fClamped f == n then fFace f else vneg (fFace f) | (o, f) <- jointPartners m n, Set.member o earlier, isScrewJoint f]
          screwPartners = [o | (o, f) <- jointPartners m n, isScrewJoint f]
          approaches = Map.findWithDefault [] n (gApproach (mGeo m))
          closedBy a q = maybe False swBlocked (Map.lookup q (apObstacles a)) && q `notElem` screwPartners
            || apZoneVol a - Map.findWithDefault 0 q (apZone a) < apZoneNeed a
          stillOpen a = all (\ax -> vlen (vsub ax (apDir a)) < 1e-6) screwAxes
            && not (any (closedBy a) (Set.toList earlier))
            && apZoneVol a - sum [Map.findWithDefault 0 q (apZone a) | q <- Set.toList earlier] >= apZoneNeed a
          open = filter stillOpen approaches
          turning = [tw | (o, f) <- jointPartners m n, isScrewJoint f, fClamped f == n, Set.member o earlier, Just tw <- [Map.lookup (fId f, n) (gTwist (mGeo m))]]
          others = [iName q | q <- insts, iName q /= n, not (Set.member (iName q) earlier)]
          buries q = (not (null open) && all (`closedBy` q) open)
            || any (\tw -> q `elem` twBlockers tw || twZoneVol tw - Map.findWithDefault 0 q (twZone tw) < twZoneNeed tw) turning
      if null approaches then Right []
      else if null open then Left ("design: " ++ n ++ " cannot go in once " ++ intercalate ", " (Set.toList earlier) ++ " are in place: every way in is blocked")
      else Right [(q, n) | q <- others, buries q]
    reasons cyc = intercalate "; " [b ++ " would bury " ++ a | (a, b) <- zip cyc (drop 1 cyc ++ take 1 cyc)]
    cycleOf preds =
      let partEdges = [(x, x, [y | NPart y <- deps]) | (NPart x, deps) <- Map.toList (closeOver preds)]
       in case [ns | CyclicSCC ns <- stronglyConnComp partEdges] of
            c : _ -> Just c
            [] -> Nothing
    -- part-to-part edges, through nut and fastener nodes
    closeOver preds = Map.fromList [(NPart (iName i), map NPart (Set.toList (before preds (iName i)))) | i <- insts]
    isScrewJoint f = case fKind f of Screw _ -> True; _ -> False

-- ------------------------------------------------------------------
-- mass

-- | Printed parts are a solid perimeter shell (area times `shell`) around
-- `infill`; metal parts are solid. A 2020 rail modelled as a plain bar
-- counts as the extrusion's 37% fill. `mass=` on the part wins.
withMass :: Settings -> IndexedMesh -> Placed -> (Inst, ())
withMass sets mesh p =
  let i = plInst p
      tris = map (\(a, b, c) -> (affApply (iAff i) a, affApply (iAff i) b, affApply (iAff i) c)) (indexedTriangles mesh)
      signed = sum [dot3 a (cross b c) / 6 | (a, b, c) <- tris]
      vol = abs signed
      area = sum (map triArea tris)
      com = if abs signed < 1e-12 then bcenter (plBox p)
            else scale3 (1 / signed) (foldr vadd (0, 0, 0) [scale3 (dot3 a (cross b c) / 24) (vadd a (vadd b c)) | (a, b, c) <- tris])
      ((x0, y0, z0), (x1, y1, z1)) = bbox (iShape i)
      boxVol = (x1 - x0) * (y1 - y0) * (z1 - z0)
      effective = case iMat i of
        Printed -> let shell = min vol (area * setShell sets) in shell + setInfill sets * (vol - shell)
        _ | iRail i && vol > 0.9 * boxVol -> 0.37 * vol
          | otherwise -> vol
      density = case iMat i of Aluminium -> 0.0027; Printed -> 0.00125; Steel -> 0.0079
      mass = fromMaybe (effective * density) (iMassHint i)
   in (i {iMass = max 1e-6 mass, iCom = com}, ())

-- ------------------------------------------------------------------
-- poses and queries

translated :: V3 -> Pose -> Pose
translated d (r, t) = (r, vadd t d)

-- | Rotate a pose about the line through c along unit a, by radians.
rotatedAbout :: V3 -> V3 -> Double -> Pose -> Pose
rotatedAbout c a rad (r, t) =
  let q = rodMat (rad * 180 / pi) a
   in (matMul q r, vadd c (mApply q (vsub t c)))

identityPose :: Pose
identityPose = (matIdentity, (0, 0, 0))

volTol :: Double
volTol = 0.05

{-# NOINLINE queryCount #-}
queryCount :: IORef (Int, Int, Double, Double)
queryCount = unsafePerformIO (newIORef (0, 0, 0, 0))

timedGap :: IO a -> IO a
timedGap act = do
  t0 <- getMonotonicTime
  r <- act
  t1 <- getMonotonicTime
  atomicModifyIORef' queryCount (\(g, o, tg, to) -> ((g + 1, o, tg + t1 - t0, to), ()))
  return r

timedOverlap :: IO a -> IO a
timedOverlap act = do
  t0 <- getMonotonicTime
  r <- act
  t1 <- getMonotonicTime
  atomicModifyIORef' queryCount (\(g, o, tg, to) -> ((g, o + 1, tg, to + t1 - t0), ()))
  return r

-- | Advance the moving solid along a one-parameter motion from x = 0 to
-- lim, against one obstacle. `scale` is the most distance any point moves
-- per unit of x. Returns the last x that was clear (Nothing when the whole
-- motion is clear) and the largest x at which the two were within `near`.
advance :: Settings -> Solid -> (Double -> Pose) -> Double -> Double -> Solid -> Pose -> Double -> IO (Maybe Double, Double)
advance sets mover poseAt scale lim obstacle opose near = do
  g0 <- gapAt 0
  -- Where the two touch, a shrunken copy of the mover tells touching from
  -- pushing in. A pair that already overlaps deeper than `contact` at the
  -- seat (a press fit) is judged by overlap volume instead.
  deep <- if g0 <= ctol then overlapping 0 else return False
  v0 <- if deep then fst <$> timedOverlap (orFail (solidOverlap mover (poseAt 0) obstacle opose False)) else return 0
  go deep 0 0 v0 (if g0 <= near then 0 else -1) (0 :: Int)
  where
    ctol = setContact sets
    h = setSweepStep sets
    search = max 2 (4 * h)
    -- best-first: the exact gap (it sets the next step), stopping early
    -- once the two are known to touch
    gapAt x = timedGap (orFail (solidGap mover (poseAt x) obstacle opose search (min ctol near) 0))
    -- does the shrunken mover still overlap? (only a tiny radius to search)
    overlapping x = (<= 1e-9) <$> timedGap (orFail (solidGap mover (poseAt x) obstacle opose (ctol / 2) 0 ctol))
    go deep x lastClear v0 lastNear n
      | x > lim || n > 4000 = return (Nothing, max 0 lastNear)
      | otherwise = do
          g <- gapAt x
          let lastNear' = if g <= near then x else lastNear
          if g > ctol
            then go deep (x + max g h / scale) x v0 lastNear' (n + 1)
            else do
              pushing <- if x == 0 then return False
                else if deep then (\(v, _) -> v > v0 + volTol) <$> timedOverlap (orFail (solidOverlap mover (poseAt x) obstacle opose False))
                else overlapping x
              if pushing
                then return (Just lastClear, max 0 lastNear')
                else go deep (x + h / scale) x v0 lastNear' (n + 1)

-- | The furthest distance along a, up to lim, at which the moved solid is
-- still within `near` of the other (collisions are not judged).
engagedLength :: Settings -> Solid -> V3 -> Double -> Solid -> Double -> IO Double
engagedLength sets mover a lim obstacle near = go 0 0
  where
    h = setSweepStep sets
    go x lastNear
      | x > lim = return lastNear
      | otherwise = do
          g <- timedGap (orFail (solidGap mover (translated (scale3 x a) identityPose) obstacle identityPose (max 2 (4 * h)) near 0))
          go (x + max h (g - near)) (if g <= near then x else lastNear)

-- The cache key of a query between two instances: their parts, the pose of
-- the obstacle relative to the mover, and a direction in the mover's frame.
-- Symmetric copies (eight hubs at eight corners) share one measurement.
relKey :: String -> Inst -> Inst -> [V3] -> Maybe [Integer]
relKey tag p q dirs =
  let (rp, tp) = iAff p
      (rq, tq) = iAff q
      rt = matTranspose rp
      isometry = all (\(x, y) -> abs (x - y) < 1e-9) (zip (flat (matMul rt rp)) (flat matIdentity))
      relR = matMul rt rq
      relT = mApply rt (vsub tq tp)
      key = map (\x -> round (x * 1e4)) (flat relR ++ v3 relT ++ concatMap (v3 . mApply rt) dirs)
   in if isometry then Just (fromIntegral (length tag) : map (fromIntegral . fromEnum) tag ++ hashName (iPart p) ++ hashName (iPart q) ++ key) else Nothing
  where
    flat ((a, b, c), (d, e, f), (g, hh, i)) = [a, b, c, d, e, f, g, hh, i]
    v3 (x, y, z) = [x, y, z]
    hashName s = fromIntegral (length s) : map (fromIntegral . fromEnum) s

cached :: Cache -> Maybe [Integer] -> IO Double -> IO Double
cached _ Nothing act = act
cached cache (Just k) act = do
  hit <- Map.lookup k <$> readIORef cache
  case hit of
    Just v -> return v
    Nothing -> do
      v <- act
      atomicModifyIORef' cache (\c -> (Map.insert k v c, ()))
      return v

-- | Run independent measurements on every core, keeping their order. The
-- first failure is re-raised once all have finished.
parallel :: [IO a] -> IO [a]
parallel acts = do
  n <- getNumCapabilities
  let indexed = zip [0 :: Int ..] acts
  queue <- newMVar indexed
  results <- newIORef Map.empty
  dones <- forM [1 .. max 1 (min n (length acts))] $ \_ -> do
    done <- newEmptyMVar
    _ <- forkIO $ do
      let loop = do
            next <- modifyMVar queue (\q -> return (drop 1 q, take 1 q))
            case next of
              [] -> return ()
              (k, act) : _ -> do
                r <- tryIOError act
                atomicModifyIORef' results (\m' -> (Map.insert k r m', ()))
                loop
      loop
      putMVar done ()
    return done
  mapM_ takeMVar dones
  rs <- Map.elems <$> readIORef results
  mapM (either ioError return) rs

boxesNear :: Double -> BBox -> BBox -> Bool
boxesNear k ((a, b, c), (d, e, f)) ((p, q, r), (s, t, u)) =
  a <= s + k && p <= d + k && b <= t + k && q <= e + k && c <= u + k && r <= f + k

-- ------------------------------------------------------------------
-- contact patches

-- | For each pair that touches and each rest direction with the second
-- below the first, the patch where the first rests on the second: the
-- overlap of the first, lowered by `settle` (it drops that far to come to
-- rest in a seat with print clearance), with the second.
contactPatches :: Settings -> Cache -> [Placed] -> IO (Map.Map (String, String, Int) [(Double, Double)])
contactPatches sets _ pls = do
  let ctol = setContact sets + setSettle sets
      pairs = [ (p, q, k, a)
              | p <- pls, q <- pls, iName (plInst p) /= iName (plInst q), boxesNear ctol (plBox p) (plBox q)
              , (k, a) <- zip [0 :: Int ..] axes6
              , let pLow = maximum (map (dot3 a) (plVerts p)) -- deepest point along "down"
                    qLow = maximum (map (dot3 a) (plVerts q))
                    qTop = minimum (map (dot3 a) (plVerts q))
              , qTop <= pLow + ctol, qLow > pLow - ctol ]
  found <- parallel $ flip map pairs $ \(p, q, k, a) -> do
    (v, pts) <- orFail (solidOverlap (plSolid p) (translated (scale3 ctol a) (plPose p)) (plSolid q) (plPose q) True)
    return [((iName (plInst p), iName (plInst q), k), convexHull (map (project a) pts)) | v > 1e-6, not (null pts)]
  return (Map.fromList (concat found))

-- ------------------------------------------------------------------
-- approaches

-- | The 26 directions of a cube lattice: axes, face and space diagonals.
-- With the part's own axes these cover sideways and diagonal entries
-- (a bar that enters its sockets at 45 degrees to the frame).
lattice :: [V3]
lattice = [vnormed (x, y, z) | x <- [-1, 0, 1], y <- [-1, 0, 1], z <- [-1, 0, 1], (x, y, z) /= (0, 0, 0)]

-- | Candidate ways out: the lattice directions, both directions of every
-- joint, mate-frame and own axis the part has, and the combined way out of
-- two or three joints. Each is swept against every instance it comes near.
approachesOf :: Model -> Settings -> Cache -> Map.Map String Placed -> Placed -> IO [Approach]
approachesOf m sets cache byName p = do
  let i = plInst p
      n = iName i
      -- the way out of each joint, the mated frames' normals, the part's own
      -- axes, and the combined way out of two or three joints at once (a
      -- corner block that takes three bars goes on along their diagonal)
      outOf f = vnormed (if fClamped f == n then fFace f else vneg (fFace f))
      outs = nubBy close (map (outOf . snd) (jointPartners m n))
      (rot, _) = iAff i
      ownAxes = [vnormed (mApply rot e) | e <- [(1, 0, 0), (0, 1, 0), (0, 0, 1)], vlen (mApply rot e) > 1e-9]
      combos = [vnormed s' | k <- [2, 3 :: Int], c <- choose k outs, let s' = foldr vadd (0, 0, 0) c, vlen s' > 1e-6]
      both a = [a, vneg a]
      dirs = nubBy close (axes6 ++ concatMap both (outs ++ Map.findWithDefault [] n (mMateAxes m) ++ ownAxes) ++ combos ++ lattice)
      close a b = vlen (vsub a b) < 1e-6
      partners = map fst (jointPartners m n)
      screwPartners = [o | (o, f) <- jointPartners m n, isScrew f]
      others = [q | q <- Map.elems byName, iName (plInst q) /= n]
  forM dirs $ \a -> do
    let ds = map (dot3 a) (plVerts p)
        (pa0, pa1) = (minimum ds, maximum ds)
    obs <- forM others $ \q -> do
      let qs = map (dot3 a) (plVerts q)
          qa1 = maximum qs
          travel = qa1 - pa0 + setContact sets
          swept = fromCorners (bcorners (plBox p) ++ map (vadd (scale3 (max 0 travel) a)) (bcorners (plBox p)))
          qn = iName (plInst q)
      if travel <= 0 || not (boxesNear (setContact sets) swept (plBox q))
        then return Nothing
        else do
          -- a screw partner interlocks by design: its thread is checked by
          -- turning, not by a straight pull
          (blocked, touching) <- if qn `elem` screwPartners then return (False, max 0 (qa1 - pa0)) else do
            b <- cached cache (relKey "path" i (plInst q) [a]) $ do
              (r, lastNear) <- advance sets (plSolid p) (\t -> translated (scale3 t a) (plPose p)) 1 travel (plSolid q) (plPose q) (setContact sets)
              -- encoded: blocked as a negative number, else how far it stays touching
              return (maybe lastNear (const (-1)) r)
            return (b < 0, max 0 b)
          bits <- if qn `elem` partners && qn `notElem` screwPartners && not blocked
            then cached cache (relKey "bits" i (plInst q) [a]) (alignmentBits sets p q a (max 0 (qa1 - pa0)))
            else return 0
          return (Just (qn, Sweep blocked bits touching))
    zone <- pushZone sets byName p a (pa1)
    let (zv, need, occ) = zone
    let obsMap = Map.fromList [o | Just o <- obs]
    return (Approach a obsMap zv need occ [q | (q, sw) <- Map.toList obsMap, swBlocked sw])
  where
    isScrew f = case fKind f of Screw _ -> True; _ -> False
    choose 0 _ = [[]]
    choose _ [] = []
    choose k (x : xs) = map (x :) (choose (k - 1) xs) ++ choose k xs

-- | Fitts index of seating a part against a joint partner: log2(1 + D/W),
-- D the length over which the part is engaged with the partner along its
-- path, W its play at the seat (the smaller of the two transverse ranges
-- it can shift before running into the partner, each capped at D).
alignmentBits :: Settings -> Placed -> Placed -> V3 -> Double -> IO Double
alignmentBits sets p q a d
  | d <= setContact sets = return 0
  | otherwise = do
      let (u, v) = basis a
      let room e = do
            (r, _) <- advance sets (plSolid p) (\s -> translated (scale3 s e) (plPose p)) 1 d (plSolid q) (plPose q) (setContact sets)
            return (fromMaybe d r)
      su <- room u
      su' <- room (vneg u)
      sv <- room v
      sv' <- room (vneg v)
      let w = max (setContact sets) (min (su + su') (sv + sv'))
      return (logBase 2 (1 + d / w))

-- | Room for a fingertip behind the part along its way in (the face it is
-- pushed by): a box `finger_size` deep over the part's cross-section, at
-- least a fingertip wide. Returns its volume, what a fingertip needs, and
-- what each other instance takes of it.
pushZone :: Settings -> Map.Map String Placed -> Placed -> V3 -> Double -> IO (Double, Double, Map.Map String Double)
pushZone sets byName p a top = do
  let f = setFinger sets
      (u, v) = basis a
      us = map (dot3 u) (plVerts p)
      vs = map (dot3 v) (plVerts p)
      (cu, wu) = ((minimum us + maximum us) / 2, max (f / 2) ((maximum us - minimum us) / 2))
      (cv, wv) = ((minimum vs + maximum vs) / 2, max (f / 2) ((maximum vs - minimum vs) / 2))
      corner x y z = vadd (scale3 x u) (vadd (scale3 y v) (scale3 z a))
      corners = [corner x y z | x <- [cu - wu, cu + wu], y <- [cv - wv, cv + wv], z <- [top, top + f]]
      zoneVol = 4 * wu * wv * f
      need = pi * (f / 2) ^ (2 :: Int) * f
      zbox = fromCorners corners
      n = iName (plInst p)
  zone <- boxSolid u v a (cu - wu, cu + wu) (cv - wv, cv + wv) (top, top + f)
  occ <- forM [q | q <- Map.elems byName, iName (plInst q) /= n, boxesNear 0 zbox (plBox q)] $ \q -> do
    (vol, _) <- orFail (solidOverlap zone identityPose (plSolid q) (plPose q) False)
    return [(iName (plInst q), vol) | vol > volTol]
  return (zoneVol, need, Map.fromList (concat occ))

-- | A box solid spanning the given ranges along the right-handed basis (u, v, a).
boxSolid :: V3 -> V3 -> V3 -> (Double, Double) -> (Double, Double) -> (Double, Double) -> IO Solid
boxSolid u v a (u0, u1) (v0, v1) (a0, a1) = orFail $ do
  let at x y z = vadd (scale3 x u) (vadd (scale3 y v) (scale3 z a))
      vs = [at x y z | z <- [a0, a1], y <- [v0, v1], x <- [u0, u1]]
      -- vertex index: x + 2y + 4z; outward-facing triangles of a unit cube
      fs = [ (0, 2, 3), (0, 3, 1), (4, 5, 7), (4, 7, 6), (0, 1, 5), (0, 5, 4)
           , (2, 6, 7), (2, 7, 3), (0, 4, 6), (0, 6, 2), (1, 3, 7), (1, 7, 5) ]
  either (return . Left) solidFromMesh (indexedMesh vs fs)

-- | An n-gon prism of radius r about the line through c along unit a,
-- from a0 to a1 along a.
prismSolid :: V3 -> V3 -> Double -> (Double, Double) -> IO Solid
prismSolid c a r (a0, a1) = orFail $ do
  let n = 48 :: Int
      (u, v) = basis a
      ring z = [vadd c (vadd (scale3 (r * cos t) u) (vadd (scale3 (r * sin t) v) (scale3 z a))) | k <- [0 .. n - 1], let t = 2 * pi * fromIntegral k / fromIntegral n]
      vs = ring a0 ++ ring a1 ++ [vadd c (scale3 a0 a), vadd c (scale3 a1 a)]
      bot = 2 * n
      top = 2 * n + 1
      nxt k = (k + 1) `mod` n
      fs = concat [ [(k, nxt k + n, k + n), (k, nxt k, nxt k + n), (bot, nxt k, k), (top, k + n, nxt k + n)] | k <- [0 .. n - 1] ]
  either (return . Left) solidFromMesh (indexedMesh vs fs)

-- ------------------------------------------------------------------
-- screw joints

-- | Both sides of every screw joint: the turns (engaged length over pitch),
-- what the part would hit turning at its seat, and room to grip it.
twistsOf :: Model -> Settings -> Map.Map String Placed -> IO (Map.Map (Int, String) Twist)
twistsOf m sets byName = do
  rs <- parallel $ flip map [(f, pitch, mover, other, dir) | f <- mFasts m, Screw pitch <- [fKind f]
             , (mover, other, dir) <- [(fClamped f, fHost f, fFace f), (fHost f, fClamped f, vneg (fFace f))]] $ \(f, pitch, mover, other, dir) -> do
    let p = byName Map.! mover
        q = byName Map.! other
        a = vnormed dir
        c = fPos f
        radial x = let w = vsub x c in vlen (vsub w (scale3 (dot3 w a) a))
        rmax = maximum (map radial (plVerts p))
        ds = map (\x -> dot3 (vsub x c) a) (plVerts p)
        (pa0, pa1) = (minimum ds, maximum ds)
        qa1 = maximum (map (\x -> dot3 (vsub x c) a) (plVerts q))
        travel = max 0 (qa1 - pa0) + setContact sets
    -- engaged length: how far along its axis the part stays within a
    -- quarter pitch of the partner's thread (a straight pull runs into the
    -- flanks; this measures through them)
    engaged <- engagedLength sets (plSolid p) a travel (plSolid q) (max (setContact sets) (pitch / 4))
    let reach = fromCorners [vadd c (vadd (scale3 z a) (vadd (scale3 x u) (scale3 y v))) | z <- [pa0, pa1], x <- [-rmax, rmax], y <- [-rmax, rmax]]
        (u, v) = basis a
        others = [o | o <- Map.elems byName, iName (plInst o) `notElem` [mover, other], boxesNear (setContact sets) reach (plBox o)]
    blockers <- forM others $ \o -> do
      (r, _) <- advance sets (plSolid p) (\th -> rotatedAbout c a th (plPose p)) (max 1 rmax) (2 * pi) (plSolid o) (plPose o) (setContact sets)
      return [iName (plInst o) | r /= Nothing]
    -- grip room: a ring one fingertip wide around the part, over its length
    let fsz = setFinger sets
        n = 48 :: Double
        ngon r = (n / 2) * r * r * sin (2 * pi / n)
        len = pa1 - pa0
        zoneVol = (ngon (rmax + fsz) - ngon rmax) * len
        need = 2 * pi * (fsz / 2) ^ (2 :: Int) * fsz
        ring = fromCorners [vadd c (vadd (scale3 z a) (vadd (scale3 x u) (scale3 y v))) | z <- [pa0, pa1], x <- [-(rmax + fsz), rmax + fsz], y <- [-(rmax + fsz), rmax + fsz]]
    outer <- prismSolid c a (rmax + fsz) (pa0, pa1)
    inner <- prismSolid c a rmax (pa0, pa1)
    occ <- forM [o | o <- Map.elems byName, iName (plInst o) /= mover, boxesNear 0 ring (plBox o)] $ \o -> do
      (vo, _) <- orFail (solidOverlap outer identityPose (plSolid o) (plPose o) False)
      (vi, _) <- orFail (solidOverlap inner identityPose (plSolid o) (plPose o) False)
      return [(iName (plInst o), vo - vi) | vo - vi > volTol]
    return ((fId f, mover), Twist (engaged / pitch) (concat blockers) zoneVol need (Map.fromList (concat occ)))
  return (Map.fromList rs)

