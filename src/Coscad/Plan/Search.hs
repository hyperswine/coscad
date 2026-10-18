-- | `coscad plan`, part 2: the search. Stability, support, driver access
-- and nut-entry predicates, then A* or beam search over placement
-- orders, merged into numbered steps.
module Coscad.Plan.Search
  ( St (..)
  , Step (..)
  , runSearch
  , toSteps
  )
where

import Coscad.Plan.Model

import Control.Monad (unless, when)
import Coscad.Assemble
import Coscad.Geometry
import Data.List (foldl', intercalate, nub, sort, sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set


-- ------------------------------------------------------------------
-- states and predicates

data St = St {sPlaced :: Set.Set Node, sRest :: Maybe V3, sCost :: Double, sMoves :: [Move]}

placedInsts :: Map.Map String Inst -> St -> [Inst]
placedInsts byName st = [i | NPart n <- Set.toList (sPlaced st), Just i <- [Map.lookup n byName]]

heightMin :: V3 -> BBox -> Double
heightMin d bb = minimum [negate (dot3 c d) | c <- bcorners bb]

benchLevel :: V3 -> [Inst] -> Double
benchLevel d is = minimum (map (heightMin d . iBox) is)

-- 2D projection basis perpendicular to d
basis :: V3 -> (V3, V3)
basis d = let u = if abs (dot3 d (0, 0, 1)) > 0.9 then (1, 0, 0) else (0, 0, 1); u' = vnormed (vsub u (scale3 (dot3 u d) d)) in (u', cross d u')

project :: V3 -> V3 -> (Double, Double)
project d p = let (u, v) = basis d in (dot3 p u, dot3 p v)

convexHull :: [(Double, Double)] -> [(Double, Double)]
convexHull pts
  | length ps < 3 = ps
  | otherwise = init lower ++ init upper
  where
    ps = nub (sort pts)
    crossZ (ox, oy) (ax, ay) (bx, by) = (ax - ox) * (by - oy) - (ay - oy) * (bx - ox)
    build = foldl' (\acc p -> add acc p) []
    add acc p = case acc of
      (b : a : rest) | crossZ a b p <= 0 -> add (a : rest) p
      _ -> p : acc
    lower = reverse (build ps)
    upper = reverse (build (reverse ps))

polyArea :: [(Double, Double)] -> Double
polyArea ps | length ps < 3 = 0
polyArea ps = abs (sum [x1 * y2 - x2 * y1 | ((x1, y1), (x2, y2)) <- zip ps (drop 1 ps ++ take 1 ps)]) / 2

insidePoly :: (Double, Double) -> [(Double, Double)] -> Bool
insidePoly (px, py) ps
  | length ps < 3 = case ps of
      [(x, y)] -> abs (x - px) < 2 && abs (y - py) < 2
      [(x1, y1), (x2, y2)] -> let t = ((px - x1) * (x2 - x1) + (py - y1) * (y2 - y1)) / max 1e-9 ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int)); tc = max 0 (min 1 t)
                                  in sqrt ((x1 + tc * (x2 - x1) - px) ^ (2 :: Int) + (y1 + tc * (y2 - y1) - py) ^ (2 :: Int)) < 2
      _ -> False
  | otherwise = all (\((x1, y1), (x2, y2)) -> (x2 - x1) * (py - y1) - (y2 - y1) * (px - x1) >= -eps * 4) (zip ps (drop 1 ps ++ take 1 ps))

supportPoly :: V3 -> [Inst] -> [(Double, Double)]
supportPoly d is =
  let lvl = benchLevel d is
      onBench = [i | i <- is, heightMin d (iBox i) < lvl + eps]
   in convexHull [project d c | i <- onBench, c <- bcorners (iBox i), negate (dot3 c d) < lvl + eps]

stable :: V3 -> [Inst] -> Bool
stable _ [] = True
stable d is =
  let poly = supportPoly d is
      m = sum (map iMass is)
      com = foldl' vadd (0, 0, 0) [scale3 (iMass i / m) (bcenter (iBox i)) | i <- is]
   in insidePoly (project d com) poly

-- | A new part may not reach under the partial assembly (that needs a
-- flip). A part that will be screwed to something already placed may be
-- held against it; anything else must rest from below (on the bench or on
-- placed parts) and balance on that support.
supported :: V3 -> [Inst] -> Inst -> Bool -> Either String ()
supported d placed p held
  | not (null placed) && heightMin d (iBox p) < benchLevel d placed - eps = Left (iName p ++ " would go under the assembly; flip first")
  | onBench = Right ()
  | held && any (bIntersects (iBox p) . iBox) placed = Right ()
  | null below = Left (iName p ++ " would float: nothing under it to rest on and nothing placed to screw it to")
  | not balanced = Left (iName p ++ " would tip: it does not balance on what is under it")
  | otherwise = Right ()
  where
    lvl = if null placed then heightMin d (iBox p) else benchLevel d placed
    onBench = abs (heightMin d (iBox p) - lvl) < eps
    myBottom = heightMin d (iBox p)
    below = [q | q <- placed, abs (heightMax d (iBox q) - myBottom) < eps, bIntersects (iBox q) (iBox p)]
    contact q = fromCorners [c | c <- bcorners (boverlap (iBox p) (iBox q))]
    poly = convexHull [project d c | q <- below, c <- bcorners (contact q)]
    balanced = insidePoly (project d (bcenter (iBox p))) poly

heightMax :: V3 -> BBox -> Double
heightMax d bb = maximum [negate (dot3 c d) | c <- bcorners bb]

driverBox :: Fast -> BBox
driverBox f =
  let n = fFace f
      start = fHeadPos f
      end = vadd start (scale3 (fDriverLen f) n)
      r = fDriverRad f
      (u, v) = basis n
      corners = [vadd p (vadd (scale3 a u) (scale3 b v)) | p <- [start, end], a <- [-r, r], b <- [-r, r]]
   in fromCorners corners

accessible :: Map.Map String Inst -> [Inst] -> Fast -> Maybe String
accessible _byName placed f =
  -- the driver must really enter another part's box, not merely touch it:
  -- a hemisphere shell whose box brushes the neighbouring shell's box
  -- (the ball example) does not put that shell in the driver's way.
  -- bIntersects itself allows eps of slack, so shrink by twice that.
  -- The part's box is its own box under its placement (oriented), not the
  -- axis-aligned one: a diagonal strut's aligned box is a block that
  -- swallows every corner of the frame, its oriented box is a thin rod.
  let db = bExpand (negate (2 * eps)) (driverBox f)
      hits = [iName i | i <- placed, iName i /= fClamped f, iName i /= fHost f, bIntersects (iBox i) db, obbIntersects db i]
   in listToMaybe hits

-- | Separating-axis test between an axis-aligned box and a part's oriented
-- box (its local box carried by its placement transform).
obbIntersects :: BBox -> Inst -> Bool
obbIntersects ab i =
  let (m, _) = iAff i
      lb = bbox (iShape i)
      ((lx0, ly0, lz0), (lx1, ly1, lz1)) = lb
      cA = bcenter ab
      ((ax0, ay0, az0), (ax1, ay1, az1)) = ab
      hA = [(ax1 - ax0) / 2, (ay1 - ay0) / 2, (az1 - az0) / 2]
      cB = affApply (iAff i) (bcenter lb)
      ((m11, m12, m13), (m21, m22, m23), (m31, m32, m33)) = m
      cols = [(m11, m21, m31), (m12, m22, m32), (m13, m23, m33)] -- images of the unit axes
      axesB = [vnormed c | c <- cols]
      hB = zipWith (\c h -> vlen c * h) cols [(lx1 - lx0) / 2, (ly1 - ly0) / 2, (lz1 - lz0) / 2]
      axesA = [(1, 0, 0), (0, 1, 0), (0, 0, 1)]
      d = vsub cB cA
      radius axes hs a = sum (zipWith (\e h -> abs (dot3 a e) * h) axes hs)
      separated a = abs (dot3 a d) > radius axesA hA a + radius axesB hB a
      candidates = axesA ++ axesB ++ [c | ea <- axesA, eb <- axesB, let c = cross ea eb, vlen c > 1e-6]
   in not (any separated candidates)

nutCanEnter :: Map.Map String Inst -> Maybe V3 -> [Inst] -> [Fast] -> String -> Maybe String
nutCanEnter byName rest placed fasts h =
  let host = byName Map.! h
      here = [f | f <- fasts, nutHolder f == h, fNut f /= NoNut]
      others = [i | i <- placed, iName i /= h]
      -- a hex nut drops into its pocket from the pocket face: that face
      -- must not be on the bench, and nothing may cover it there
      hexBlocked f = case fPocket f of
        Nothing -> Nothing
        Just _ | fPocketInClamped f -> Nothing -- loaded with the clamped part in hand
        Just (pf, n)
          | Just d <- rest, dot3 n d > 0.5 -> Just ("the " ++ pf ++ " face of " ++ h ++ " (its nut pocket) is on the bench")
          | otherwise ->
              let (u, v) = basis n
                  plane = snd (extentAlong n (iBox host))
                  base = fromMaybe (vadd (fPos f) (scale3 (plane - dot3 (fPos f) n) n)) (fPocketEntry f)
                  box = fromCorners [vadd base (vadd (scale3 a u) (vadd (scale3 b v) (scale3 c n))) | a <- [-6, 6], b <- [-6, 6], c <- [0, 3]]
               in case [iName i | i <- others, bIntersects (iBox i) box] of
                    (b : _) -> Just ("the " ++ pf ++ " face of " ++ h ++ " (its nut pocket) is already covered by " ++ b)
                    [] -> Nothing
      dropInOk f =
        let n = fFace f
            (u, v) = basis n
            box = fromCorners [vadd (fPos f) (vadd (scale3 a u) (vadd (scale3 b v) (scale3 c n))) | a <- [-6, 6], b <- [-6, 6], c <- [0, 3]]
         in [iName i | i <- others, bIntersects (iBox i) box]
      endOpen (open, cap) = open && not (any (bIntersects cap . iBox) others)
      slideOk = or (zipWith (curry endOpen) [fst (iEnds host), snd (iEnds host)] (endCaps host))
   in if not (any ((== h) . iName) placed) then Nothing -- preload an independent part in hand
      else case [why | f <- here, fNut f == Hex, Just why <- [hexBlocked f]] of
        (why : _) -> Just why
        [] -> case [b | f <- here, fNut f == DropIn, b <- take 1 (dropInOk f)] of
          (b : _) -> Just ("the " ++ maybe "?" fFaceName (listToMaybe here) ++ " slot of " ++ h ++ " is already covered by " ++ b)
          [] -> if any ((== SlideIn) . fNut) here && not slideOk then Just ("both ends of " ++ h ++ " are sealed; slide-in nuts cannot enter") else Nothing

-- ------------------------------------------------------------------
-- search

available :: Model -> St -> [Node]
available m st = [n | n <- mNodes m, not (Set.member n (sPlaced st)), all (`Set.member` sPlaced st) (Map.findWithDefault [] n (mPreds m))]

-- | Try one placement; Nothing when a hard predicate fails (with reason).
tryPlace :: Model -> St -> Node -> Either String St
tryPlace m st node = case sRest st of
  Nothing -> Left "no rest face chosen"
  Just d -> do
    let placed = placedInsts (mByName m) st
        w = mWeights m
        polyBefore = polyArea (supportPoly d placed)
    (cost, warns) <- case node of
      NPart n -> do
        let p = mByName m Map.! n
            -- a part about to be screwed to something already placed may be held
            -- against it, whichever side of the joint it is on; a friction peg
            -- only holds the part that is pushed onto it
            held = any (\f -> (fClamped f == n && Set.member (NPart (fHost f)) (sPlaced st))
                            || (not (isPegF f) && fHost f == n && Set.member (NPart (fClamped f)) (sPlaced st))) (mFasts m)
        supported d placed p held
        let placed' = p : placed
        unless (stable d placed') (Left ("assembly would tip after placing " ++ n))
        let hostsOfP = nub [fHost f | f <- mFasts m, fClamped f == n]
            vertical = [h | h <- hostsOfP, Just hi <- [Map.lookup h (mByName m)], iRail hi, abs (dot3 (iAxis hi) d) > 0.3]
            standing = (iRail p || slender p) && abs (dot3 (iAxis p) d) > 0.3 -- a rail or bar stood on end: its nuts slide, it wobbles
            shrink = polyArea (supportPoly d placed') < polyBefore - 1
        Right ( (if null vertical && not standing then 0 else wVertical w) + (if shrink then wShrink w else 0)
              , [h ++ " is not lying flat in this step; hold its T-nut with a finger while starting the screw" | h <- vertical]
                  ++ [n ++ " stands on end in this step; steady it until it is braced" | standing] )
      NNuts h -> case nutCanEnter (mByName m) (Just d) placed (mFasts m) h of
        Just why -> Left why
        Nothing -> Right (0, ["Load the nuts into " ++ h ++ " in hand, turning it to reach the pockets or slots; retain them while placing the part" | not (any ((== h) . iName) placed)])
      NFast i -> do
        let f = mFastById m Map.! i
        unless (isPegF f) $ case accessible (mByName m) placed f of
          Just blocker -> Left ("driver for " ++ fSpec f ++ " into " ++ fHost f ++ " is blocked by " ++ blocker)
          Nothing -> Right ()
        -- the driver cannot come up through the bench
        unless (isPegF f) $
          when (dot3 (fFace f) d > 0.5 && heightMin d (driverBox f) < benchLevel d placed + 3) $
            Left ("driver for " ++ fSpec f ++ " into " ++ fHost f ++ " would come from under the bench; flip first")
        let up = vneg d
            offAxis = not (isPegF f) && dot3 (fFace f) up < cos (30 * pi / 180)
            siblings = [g | g <- mFasts m, fClamped g == fClamped f, fId g /= i, not (Set.member (NFast (fId g)) (sPlaced st))]
            pocketDown = case fPocket f of
              Just (pf, n) | fPocketInClamped f, dot3 n d > 0.5 -> [pf]
              _ -> []
        Right ( (if offAxis then wDriver w else 0) + (if null siblings then 0 else wSibling w)
              , ["driver axis is not vertical here (" ++ fFaceName f ++ " face of " ++ fHost f ++ "); keep the driver square to the bracket" | offAxis]
                  ++ ["the nut pocket on the " ++ pf ++ " face of " ++ fClamped f ++ " faces down here; hold the nut in it while the screw starts" | pf <- pocketDown] )
    let placed' = Set.insert node (sPlaced st)
        loose = wLoose w * fromIntegral (length (looseParts m {mRest = Just d} placed'))
    Right st {sPlaced = placed', sCost = sCost st + cost + loose, sMoves = MPlace node warns : sMoves st}

-- | Placed parts nothing holds yet: brackets waiting for their screws, and
-- parts resting on other parts (not the bench) with no screw in them.
looseParts :: Model -> Set.Set Node -> [String]
looseParts m placed =
  -- a clamped part waiting for its screws is loose unless it rests on the
  -- bench (a corner block set down before its bars is going nowhere)
  nub ([fClamped f | f <- mFasts m, Set.member (NPart (fClamped f)) placed, not (Set.member (NFast (fId f)) placed)
                   , Just i <- [Map.lookup (fClamped f) (mByName m)], offBench i]
       ++ [iName i | NPart n <- Set.toList placed, Just i <- [Map.lookup n (mByName m)], not (fixed n), offBench i])
  where
    fixed n = any (\f -> (fHost f == n || fClamped f == n) && Set.member (NFast (fId f)) placed) (mFasts m)
    placedI = [i | NPart n <- Set.toList placed, Just i <- [Map.lookup n (mByName m)]]
    offBench i = case mRestNow of
      Nothing -> False
      Just d -> heightMin d (iBox i) > benchLevel d placedI + eps
    mRestNow = mRest m

-- | Cheapest single-orientation finish: driver costs for the remaining
-- fasteners under each rest face, plus one flip if it is not the current one.
heuristic :: Model -> St -> Double
heuristic m st =
  let w = mWeights m
      remaining = [f | f <- mFasts m, not (isPegF f), not (Set.member (NFast (fId f)) (sPlaced st))]
      groups = Map.fromListWith (+) [(fFace f, 1 :: Int) | f <- remaining]
      verticalNow n = maybe False (\d -> dot3 n (vneg d) >= cos (30 * pi / 180)) (sRest st)
      placed = placedInsts (mByName m) st
      -- parts still to come that lie below what is already built need a flip
      underneath = case sRest st of
        Just d | not (null placed) ->
          let lvl = benchLevel d placed
           in if any (\i -> not (Set.member (NPart (iName i)) (sPlaced st)) && heightMin d (iBox i) < lvl - eps) (mInsts m) then wFlip w else 0
        _ -> 0
      -- clamped parts still to place whose host rail stands vertical on this rest face
      vertHosts = case sRest st of
        Just d -> sum [wVertical w | i <- mInsts m, not (Set.member (NPart (iName i)) (sPlaced st))
                      , let hosts = [h | f <- mFasts m, fClamped f == iName i, Just h <- [Map.lookup (fHost f) (mByName m)]]
                      , not (null hosts), any (\h -> iRail h && abs (dot3 (iAxis h) d) > 0.3) hosts]
        Nothing -> 0
   in underneath + min vertHosts (2 * wFlip w) + sum [min (fromIntegral k * wDriver w) (wFlip w) | (n, k) <- Map.toList groups, not (verticalNow n)]

tryFlip :: Model -> St -> V3 -> Maybe St
tryFlip m st d
  | sRest st == Just d = Nothing
  | not (stable d (placedInsts (mByName m) st)) = Nothing
  | otherwise =
      -- the first rest face is free; a tiny tie-break prefers the listed order (bottom first)
      let idx = fromIntegral (length (takeWhile (/= d) (mRestDirs m))) * 0.001
          loose = looseParts m {mRest = Just d} (sPlaced st)
          warns = ["Hold " ++ intercalate ", " loose ++ " against the assembly while turning it; their joints are not fastened yet" | sRest st /= Nothing, not (null loose)]
       in Just st {sRest = Just d, sCost = sCost st + (if sRest st == Nothing then idx else wFlip (mWeights m) + idx), sMoves = MFlip d warns : sMoves st}

-- | After any move, do the obvious follow-ups: preload nuts that can now
-- go in, and drive every screw whose driver is clear. Repeats until
-- nothing more is available.
settle :: Model -> St -> St
settle m st =
  case [s | n <- available m st, isFollowUp n, Right s <- [tryPlace m st n]] of
    [] -> st
    (s : _) -> settle m s
  where
    isFollowUp n = case n of NPart _ -> False; _ -> True

-- | Macro moves: place one part (then settle), or flip (then settle).
-- A free move (cost 0, e.g. laying another rail on the bench) is taken
-- alone: the permutations of free placements are all equivalent, and
-- branching on them is what blows the search up.
successors :: Model -> St -> [St]
successors m st =
  case [s' | (Just p, s') <- placements, sCost s' <= sCost st + 1e-9, not (isRailStanding p)] of
    (free : _) -> [free]
    [] ->
      map snd placements
        ++ [s' | d <- mRestDirs m, Just s <- [tryFlip m st d], not (lastWasFlip st), let s' = settle m s, enables st s']
        ++ [settle m st | not (null (followUps st))] -- leftovers (a screw that became reachable without a flip)
  where
    placements = [(Just p, settle m s) | NPart p <- available m st, Right s <- [tryPlace m st (NPart p)]]
    isRailStanding _ = False
    lastWasFlip s = case sMoves s of (MFlip _ _ : _) -> True; _ -> False
    followUps s = [n | n <- available m s, case n of NPart _ -> False; _ -> True, Right _ <- [tryPlace m s n]]
    placeable s = [p | NPart p <- available m s, Right _ <- [tryPlace m s (NPart p)]]
    -- a flip is worth considering when it is the first rest face, drove
    -- something, or lets a part in that could not be placed before
    enables before after =
      sRest before == Nothing
        || Set.size (sPlaced after) > Set.size (sPlaced before)
        || any (`notElem` placeable before) (placeable after)

-- | A* over (placed set, rest face) with the completion heuristic; gives
-- up after `cap` expansions (the caller then falls back to beam search).
astar :: Model -> Int -> Maybe St
astar m cap = go (Set.singleton (heuristic m st0, 0 :: Int)) (Map.singleton 0 st0) Map.empty 1 0
  where
    st0 = St Set.empty Nothing 0 []
    total = length (mNodes m)
    key s = (Set.toList (sPlaced s), sRest s)
    go open states closed nextId expanded
      | Set.null open || expanded > cap = Nothing
      | otherwise =
          let ((_, i), open') = Set.deleteFindMin open
              s = states Map.! i
              states' = Map.delete i states
           in if Set.size (sPlaced s) == total
                then Just s
                else case Map.lookup (key s) closed of
                  Just g | g <= sCost s -> go open' states' closed nextId expanded
                  _ ->
                    let closed' = Map.insert (key s) (sCost s) closed
                        succs = [x | x <- successors m s, maybe True (> sCost x) (Map.lookup (key x) closed')]
                        ids = [nextId ..]
                        open'' = foldl' (\o (j, x) -> Set.insert (sCost x + heuristic m x, j) o) open' (zip ids succs)
                        states'' = foldl' (\st (j, x) -> Map.insert j x st) states' (zip ids succs)
                     in go open'' states'' closed' (nextId + length succs) (expanded + 1)

-- | One rest-face phase: keep taking the cheapest macro move while it is
-- cheap (marginal cost at most `tau`); the outer search decides flips.
phaseGreedy :: Model -> Double -> St -> St
phaseGreedy m tau st =
  case sortOn (\s -> (sCost s - sCost st, negate (Set.size (sPlaced s)))) (macroPlacements m st) of
    (s : _) | sCost s - sCost st <= tau + 1e-9 -> phaseGreedy m tau s
    _ -> st

macroPlacements :: Model -> St -> [St]
macroPlacements m st =
  [settle m s | NPart p <- available m st, Right s <- [tryPlace m st (NPart p)]]
    ++ [settle m st | not (null [n | n <- available m st, case n of NPart _ -> False; _ -> True, Right _ <- [tryPlace m st n]])]

-- | Phase successors: flip to another rest face and run a cheap phase
-- there, or stay and accept exactly one expensive move (then continue cheaply).
phaseSuccessors :: Model -> St -> [St]
phaseSuccessors m st = concatMap afterFlip flips ++ expensive st
  where
    tau = 2.5
    flips = [phaseGreedy m tau (settle m s) | d <- mRestDirs m, Just s <- [tryFlip m st d]]
    -- a flip that opened cheap work stands on its own; one that did not
    -- must carry the expensive move it was for, so flip ping-pong costs real points
    afterFlip s
      | Set.size (sPlaced s) > Set.size (sPlaced st) = [s]
      | otherwise = expensive s
    expensive s = [phaseGreedy m tau x | x <- take 3 (sortOn sCost (macroPlacements m s)), sCost x > sCost s + tau]

beamSearch :: Model -> Int -> Either [String] St
beamSearch m width = go (0 :: Int) [St Set.empty Nothing 0 []] [] Nothing
  where
    total = length (mNodes m)
    complete s = Set.size (sPlaced s) == total
    key s = (Set.toList (sPlaced s), sRest s)
    dedupe = Map.elems . Map.fromListWith (\a b -> if sCost a <= sCost b then a else b) . map (\s -> (key s, s))
    -- `prev` is the last non-empty beam: when every state dies, the
    -- explanation comes from what those states could not do
    go round' beam prev best
      | round' > total * 2 + 20 = maybe (Left (stuck round' beam)) Right best
      | null beam = maybe (Left ("no states left" : stuck round' prev)) Right best
      | otherwise =
          let next = dedupe (concatMap (phaseSuccessors m) beam)
              (done, todo) = (filter complete next, filter (not . complete) next)
              best' = foldl' (\b s -> case b of Nothing -> Just s; Just x -> Just (if sCost s < sCost x then s else x)) best done
              pruned = take width (sortOn (\s -> (sCost s + heuristic m s, negate (Set.size (sPlaced s)))) todo)
           in case best' of
                Just b | all (\s -> sCost s >= sCost b) pruned -> Right b
                _ -> go (round' + 1) pruned beam best'
    stuck _ [] = []
    stuck round' beam@(b0 : rest) =
      -- the first of the most-placed states, as sortOn would give
      let placedCount = Set.size . sPlaced
          s = foldl' (\a x -> if placedCount x > placedCount a then x else a) b0 rest
       in ("planner stuck after placing " ++ show (Set.size (sPlaced s)) ++ " of " ++ show total ++ " nodes on the " ++ maybe "?" dirName (sRest s) ++ " (round " ++ show round' ++ ", beam " ++ show (length beam) ++ ")")
            : [describe n ++ ": " ++ why | n <- available m s, Left why <- [tryPlace m s n]]
            ++ ["flip to " ++ dirName d ++ ": " ++ maybe "not stable" (const "ok") (tryFlip m s d) | d <- mRestDirs m]
    describe n = case n of
      NPart p -> "place " ++ p
      NNuts h -> "preload " ++ h
      NFast i -> let f = mFastById m Map.! i in fSpec f ++ " into " ++ fHost f ++ " (" ++ fClamped f ++ ")"

-- ------------------------------------------------------------------
-- steps

data Step = Step {stRest :: V3, stFlip :: Bool, stParts :: [String], stNuts :: [String], stFasts :: [Int], stWarn :: [String]}

toSteps :: [Move] -> [Step]
toSteps = finish . foldl' add (Nothing, [])
  where
    finish (cur, acc) = reverse (maybe acc (: acc) cur)
    add (cur, acc) mv = case mv of
      MFlip d warns -> (Just (Step d (maybe False (const True) cur) [] [] [] warns), maybe acc (: acc) cur)
      MPlace node warns -> case cur of
        Nothing -> (Nothing, acc) -- cannot happen: a flip always comes first
        Just s -> case node of
          NPart p
            | null (stFasts s) && null (stNuts s) && length (stParts s) < 4 -> (Just s {stParts = stParts s ++ [p], stWarn = stWarn s ++ warns}, acc)
            | otherwise -> (Just (Step (stRest s) False [p] [] [] warns), s : acc)
          NNuts h
            | null (stFasts s) -> (Just s {stNuts = stNuts s ++ [h], stWarn = stWarn s ++ warns}, acc)
            | otherwise -> (Just (Step (stRest s) False [] [h] [] warns), s : acc)
          NFast i -> (Just s {stFasts = stFasts s ++ [i], stWarn = stWarn s ++ warns}, acc)

runSearch :: AsmResult -> Model -> Either [String] St
runSearch ar model =
  let width = maybe 30 round (lookup "beam" (arPlanOpts ar) >>= readD)
   in case lookup "astar" (arPlanOpts ar) >>= readD of
        Just cap | Just s <- astar model (round cap) -> Right s
        _ -> beamSearch model width
