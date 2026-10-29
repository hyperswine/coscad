-- | `coscad plan`, part 2: the search. Support, stability, the way each
-- part comes in, screw joints turning, hands, driver access and nut entry,
-- then A* or beam search over placement orders, merged into numbered steps.
--
-- Shapes come from the tables Coscad.Plan.Geometry measured on the meshes:
-- heights, bench footprints and contact patches, centres of mass, and the
-- approach sweeps. Every move records its cost terms (Coscad.Plan.Model
-- 'Terms'); the plan cost is their weighted sum.
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
import Data.List (foldl', intercalate, nub, sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set


-- ------------------------------------------------------------------
-- states and predicates

-- | sCost is the plan cost so far; sOrder the part of it that depends on
-- the order (everything but the joints' own work, see 'workTerms').
data St = St {sPlaced :: Set.Set Node, sRest :: Maybe V3, sCost :: Double, sOrder :: Double, sMoves :: [Move], sLoose :: [[String]]}

placedInsts :: Map.Map String Inst -> St -> [Inst]
placedInsts byName st = [i | NPart n <- Set.toList (sPlaced st), Just i <- [Map.lookup n byName]]

placedNames :: St -> Set.Set String
placedNames st = Set.fromList [n | NPart n <- Set.toList (sPlaced st)]

ctolOf :: Model -> Double
ctolOf = setContact . mSettings

-- | The instance's mesh extent along a rest direction (box before the
-- geometry is measured).
extent :: Model -> Inst -> V3 -> (Double, Double)
extent m i d = fromMaybe (extentAlong d (iBox i)) (axisIndex d >>= \k -> Map.lookup (iName i, k) (gExt (mGeo m)))

-- heights measured up, away from the bench under rest direction d
heightMin, heightMax :: Model -> V3 -> Inst -> Double
heightMin m d i = negate (snd (extent m i d))
heightMax m d i = negate (fst (extent m i d))

benchLevel :: Model -> V3 -> [Inst] -> Double
benchLevel m d is = minimum (map (heightMin m d) is)

footprint :: Model -> V3 -> Inst -> [(Double, Double)]
footprint m d i = fromMaybe boxFoot (axisIndex d >>= \k -> Map.lookup (iName i, k) (gFoot (mGeo m)))
  where
    boxFoot = let lo = heightMin m d i in convexHull [project d c | c <- bcorners (iBox i), negate (dot3 c d) < lo + eps]

contactPatch :: Model -> V3 -> Inst -> Inst -> Maybe [(Double, Double)]
contactPatch m d p q = axisIndex d >>= \k -> Map.lookup (iName p, iName q, k) (gContact (mGeo m))

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

onBench :: Model -> V3 -> [Inst] -> Inst -> Bool
onBench m d is i = heightMin m d i < benchLevel m d is + ctolOf m

supportPoly :: Model -> V3 -> [Inst] -> [(Double, Double)]
supportPoly m d is = convexHull (concat [footprint m d i | i <- is, onBench m d is i])

centreOfMass :: [Inst] -> V3
centreOfMass is = let total = sum (map iMass is) in foldl' vadd (0, 0, 0) [scale3 (iMass i / total) (iCom i) | i <- is]

stable :: Model -> V3 -> [Inst] -> Bool
stable _ _ [] = True
stable m d is = insidePoly (project d (centreOfMass is)) (supportPoly m d is)

-- | A part rests on others when its contact patches with parts under it
-- hold its centre of mass.
restsOn :: Model -> V3 -> [Inst] -> Inst -> Bool
restsOn m d under p =
  let patches = [ps | q <- under, iName q /= iName p, Just ps <- [contactPatch m d p q]]
   in not (null patches) && insidePoly (project d (iCom p)) (convexHull (concat patches))

-- | A new part may not reach under the partial assembly (that needs a
-- flip). A part that will be joined to something already placed may be
-- held against it; anything else must rest from below (on the bench or on
-- placed parts) and balance on that support.
supported :: Model -> V3 -> [Inst] -> Inst -> Bool -> Either String ()
supported m d placed p held
  | not (null placed) && heightMin m d p < benchLevel m d placed - ctolOf m = Left (iName p ++ " would go under the assembly; flip first")
  | onBenchNow = Right ()
  | held && any (bIntersects (iBox p) . iBox) placed = Right ()
  | null below = Left (iName p ++ " would float: nothing under it to rest on and nothing placed to join it to")
  | not (restsOn m d below p) = Left (iName p ++ " would tip: it does not balance on what is under it")
  | otherwise = Right ()
  where
    lvl = if null placed then heightMin m d p else benchLevel m d placed
    onBenchNow = abs (heightMin m d p - lvl) < ctolOf m
    below = [q | q <- placed, contactPatch m d p q /= Nothing]

-- ------------------------------------------------------------------
-- holding: what keeps each placed part where it is

-- | Placed parts that stay put on their own in rest direction d: on the
-- bench, or (repeatedly) joined by a made joint that holds, resting
-- balanced on, or captured by parts already known to stay.
grounded :: Model -> V3 -> Set.Set Node -> [Inst] -> Set.Set String
grounded m d placedNodes placed = grow (Set.fromList [iName i | i <- placed, onBench m d placed i])
  where
    grow g =
      let more = [iName i | i <- placed, not (Set.member (iName i) g), joinedTo placedNodes m g i || restsOn m d [q | q <- placed, Set.member (iName q) g] i || captured m (Just d) g i]
       in if null more then g else grow (foldr Set.insert g more)

joinedTo :: Set.Set Node -> Model -> Set.Set String -> Inst -> Bool
joinedTo placedNodes m g i = or [Set.member o g && retains f && Set.member (NFast (fId f)) placedNodes | (o, f) <- jointPartners m (iName i)]

-- | Every way out of its seat runs into a part of g (or, with a rest
-- direction, down through the bench).
captured :: Model -> Maybe V3 -> Set.Set String -> Inst -> Bool
captured m rest g i = case Map.findWithDefault [] (iName i) (gApproach (mGeo m)) of
  [] -> False
  as -> all exitBlocked as
  where
    exitBlocked a = any (`Set.member` g) (apBlockers a)
      || maybe False (\d -> throughBench m d [q | q <- mInsts m, Set.member (iName q) g] g i a) rest

-- | Does leaving along this approach, far enough to clear the placed parts
-- in its way (and at least 1 mm), take the part down through the bench?
throughBench :: Model -> V3 -> [Inst] -> Set.Set String -> Inst -> Approach -> Bool
throughBench m d placed placedSet p a =
  let down = dot3 (apDir a) d
      need = 1 + maximum (0 : [swTravel sw | (q, sw) <- Map.toList (apObstacles a), Set.member q placedSet])
   in down > 1e-6 && not (null placed) && heightMin m d p - need * down < benchLevel m d placed - ctolOf m

-- | Group parts into pieces that move together (made joints that hold).
pieces :: Model -> Set.Set Node -> [String] -> [[String]]
pieces m placedNodes names = go names
  where
    inSet = Set.fromList names
    go [] = []
    go (x : xs) = let c = closure (Set.singleton x) [x] in Set.toList c : go (filter (`Set.notMember` c) xs)
    closure seen [] = seen
    closure seen (y : ys) =
      let next = [o | (o, f) <- jointPartners m y, Set.member o inSet, not (Set.member o seen), retains f, Set.member (NFast (fId f)) placedNodes]
       in closure (foldr Set.insert seen next) (next ++ ys)

-- | Pieces nothing holds yet: each needs a hand.
loosePieces :: Model -> V3 -> Set.Set Node -> [Inst] -> [[String]]
loosePieces m d placedNodes placed =
  let g = grounded m d placedNodes placed
   in pieces m placedNodes [iName i | i <- placed, not (Set.member (iName i) g)]

-- | While the assembly turns, gravity and the bench hold nothing: the
-- pieces not joined to or captured by the heaviest piece must be held.
turnPieces :: Model -> Set.Set Node -> [Inst] -> [[String]]
turnPieces _ _ [] = []
turnPieces m placedNodes placed =
  let ps = pieces m placedNodes (map iName placed)
      massOf ns = sum [maybe 0 iMass (Map.lookup n (mByName m)) | n <- ns]
      body0 = Set.fromList (snd (maximum [(massOf p, p) | p <- ps]))
      grow g =
        let more = [iName i | i <- placed, not (Set.member (iName i) g), joinedTo placedNodes m g i || captured m Nothing g i]
         in if null more then g else grow (foldr Set.insert g more)
      body = grow body0
   in pieces m placedNodes [iName i | i <- placed, not (Set.member (iName i) body)]

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

isScrewJoint :: Fast -> Bool
isScrewJoint f = case fKind f of Screw _ -> True; _ -> False

term :: String -> Double -> Terms
term k v = if v == 0 then Map.empty else Map.singleton k v

-- | The way a part comes in, given what is placed: every approach that is
-- clear of placed parts, does not come up through the bench, keeps to the
-- axis of each screw joint it makes, and leaves a fingertip room to push
-- it home. The cheapest wins (alignment work, then cramped fingers, then
-- from above). Returns its direction and terms, or why none works.
chooseApproach :: Model -> V3 -> Set.Set String -> Inst -> Either String (Maybe V3, Terms)
chooseApproach m d placedSet p = case Map.findWithDefault [] n (gApproach (mGeo m)) of
  [] -> Right (Nothing, Map.empty)
  as ->
    let judged = map judge as
        ok = [(a, t) | (a, Right t) <- judged]
        w = mWeights m
     in case sortOn (\(a, t) -> (price w t, negate (dot3 (apDir a) (vneg d)))) ok of
          (a, t) : _ -> Right (Just (apDir a), t)
          [] -> Left (why [r | (_, Left r) <- judged])
  where
    n = iName p
    nowJoints = [(o, f) | (o, f) <- jointPartners m n, Set.member o placedSet]
    screwDirs = [if fClamped f == n then fFace f else vneg (fFace f) | (_, f) <- nowJoints, isScrewJoint f]
    screwPartners = [o | (o, f) <- nowJoints, isScrewJoint f]
    placedList = [q | q <- mInsts m, Set.member (iName q) placedSet]
    judge a
      | throughBench m d placedList placedSet p a = (a, Left Nothing)
      | any (\ax -> vlen (vsub ax (apDir a)) > 1e-6) screwDirs = (a, Left Nothing)
      | not (null hits) = (a, Left (Just ("from " ++ wayName (apDir a) ++ " it runs into " ++ intercalate ", " hits)))
      | apZoneVol a - occupied < apZoneNeed a = (a, Left (Just ("from " ++ wayName (apDir a) ++ " there is no room for a finger to push it home")))
      | otherwise = (a, Right (term "insert" bits `Map.union` term "finger" (occupied / apZoneVol a)))
      where
        obs = [(q, sw) | (q, sw) <- Map.toList (apObstacles a), Set.member q placedSet, q `notElem` screwPartners]
        hits = [q | (q, sw) <- obs, swBlocked sw]
        bits = sum [swBits sw | (_, sw) <- obs]
        occupied = sum [v | (q, v) <- Map.toList (apZone a), Set.member q placedSet]
    wayName a = "the " ++ (if vlen (vsub a (vneg d)) < 1e-6 then "top" else dirSide a)
    dirSide a = case dirName a of
      s | take 1 s == "(" -> "direction " ++ s
        | otherwise -> takeWhile (/= ' ') s ++ " side"
    why rs = case [r | Just r <- rs] of
      [] -> n ++ " cannot go in: the only ways in come up through the bench or leave a screw joint's axis"
      xs -> n ++ " cannot go in: " ++ intercalate "; " (nub xs)

-- | Can every part still to place still go in? Placed parts only ever
-- accumulate, so a part whose every way in is already blocked (by placed
-- parts, along any screw axis it must keep, with room for a finger), or
-- whose screw joint can no longer turn, never will: such a state is a dead
-- end, whatever the rest face. (The bench is not counted: a flip moves it.)
viable :: Model -> St -> Bool
viable m st = all canStillGo [i | i <- mInsts m, not (Set.member (iName i) placedSet)]
  where
    placedSet = placedNames st
    canStillGo p =
      let n = iName p
          nowJoints = [(o, f) | (o, f) <- jointPartners m n, Set.member o placedSet]
          screwDirs = [if fClamped f == n then fFace f else vneg (fFace f) | (_, f) <- nowJoints, isScrewJoint f]
          screwPartners = [o | (o, f) <- nowJoints, isScrewJoint f]
          open a = all (\ax -> vlen (vsub ax (apDir a)) < 1e-6) screwDirs
            && not (or [swBlocked sw | (q, sw) <- Map.toList (apObstacles a), Set.member q placedSet, q `notElem` screwPartners])
            && apZoneVol a - sum [v | (q, v) <- Map.toList (apZone a), Set.member q placedSet] >= apZoneNeed a
          approaches = Map.findWithDefault [] n (gApproach (mGeo m))
       in (null approaches || any open approaches) && either (const False) (const True) (twistTerms m placedSet n)

-- | A screw joint made by turning this part: it must spin clear of placed
-- parts and leave room for two fingertips to grip it.
twistTerms :: Model -> Set.Set String -> String -> Either String Terms
twistTerms m placedSet n = do
  ts <- mapM one [(o, f) | (o, f) <- jointPartners m n, Set.member o placedSet, isScrewJoint f]
  Right (Map.unionsWith (+) ts)
  where
    one (o, f) = case Map.lookup (fId f, n) (gTwist (mGeo m)) of
      Nothing -> Right Map.empty
      Just tw -> do
        case [b | b <- twBlockers tw, Set.member b placedSet] of
          b : _ -> Left (n ++ " cannot be turned onto " ++ o ++ ": it would hit " ++ b ++ " as it turns")
          [] -> Right ()
        let occupied = sum [v | (q, v) <- Map.toList (twZone tw), Set.member q placedSet]
        when (twZoneVol tw - occupied < twZoneNeed tw) $ Left ("there is no room to grip " ++ n ++ " to turn it onto " ++ o)
        Right (Map.unionWith (+) (term "turn" (twTurns tw)) (term "finger" (occupied / max 1e-9 (twZoneVol tw))))

-- | Try one placement; Left when a hard predicate fails (with reason).
tryPlace :: Model -> St -> Node -> Either String St
tryPlace m st node = case sRest st of
  Nothing -> Left "no rest face chosen"
  Just d -> do
    let placed = placedInsts (mByName m) st
        placedSet = placedNames st
        sets = mSettings m
        standingSin = sin (setStandingAngle sets * pi / 180)
        polyBefore = polyArea (supportPoly m d placed)
    (terms, warns, entry) <- case node of
      NPart n -> do
        let p = mByName m Map.! n
            -- a part about to be joined to something already placed may be
            -- held against it, whichever side of a screw it is on; a peg or
            -- a socket only holds the part that goes onto it
            held = any (\f -> (fClamped f == n && Set.member (fHost f) placedSet)
                            || (fHost f == n && Set.member (fClamped f) placedSet && (fKind f == Bolt || isScrewJoint f))) (mFasts m)
        supported m d placed p held
        let placed' = p : placed
        unless (stable m d placed') (Left ("assembly would tip after placing " ++ n))
        (entry, approachTerms) <- chooseApproach m d placedSet p
        turnTerms <- twistTerms m placedSet n
        let hostsOfP = nub [fHost f | f <- mFasts m, fClamped f == n]
            vertical = [h | h <- hostsOfP, Just hi <- [Map.lookup h (mByName m)], iRail hi, abs (dot3 (iAxis hi) d) > standingSin]
            -- a rail or bar stood on end wobbles until something braces it; a
            -- joint that holds it (made now, by settling) braces it at once
            bracedNow = any (\(o, f) -> Set.member o placedSet && retains f && (isJoint f || fKind f == Bolt && accessibleNow f)) (jointPartners m n)
            accessibleNow f = accessible (mByName m) placed' f == Nothing
            standing = (iRail p || slender (setSlender sets) p) && abs (dot3 (iAxis p) d) > standingSin && not bracedNow
            shrink = polyArea (supportPoly m d placed') < polyBefore - 1
        Right ( Map.unionsWith (+) [term "part" 1, approachTerms, turnTerms, term "vertical" (if null vertical && not standing then 0 else 1), term "shrink" (if shrink then 1 else 0)]
              , [h ++ " is not lying flat in this step; hold its T-nut with a finger while starting the screw" | h <- vertical]
                  ++ [n ++ " stands on end in this step; steady it until it is braced" | standing]
              , entry )
      NNuts h -> case nutCanEnter (mByName m) (Just d) placed (mFasts m) h of
        Just why -> Left why
        Nothing -> Right (term "nut" 1, ["Load the nuts into " ++ h ++ " in hand, turning it to reach the pockets or slots; retain them while placing the part" | not (any ((== h) . iName) placed)], Nothing)
      NFast i -> do
        let f = mFastById m Map.! i
        unless (isPegF f) $ case accessible (mByName m) placed f of
          Just blocker -> Left ("driver for " ++ fSpec f ++ " into " ++ fHost f ++ " is blocked by " ++ blocker)
          Nothing -> Right ()
        -- the driver cannot come up through the bench
        unless (isPegF f) $
          when (dot3 (fFace f) d > 0.5 && minimum [negate (dot3 c d) | c <- bcorners (driverBox f)] < benchLevel m d placed + 3) $
            Left ("driver for " ++ fSpec f ++ " into " ++ fHost f ++ " would come from under the bench; flip first")
        let up = vneg d
            offAxis = not (isPegF f) && dot3 (fFace f) up < cos (setDriverAngle sets * pi / 180)
            siblings = [g | g <- mFasts m, fClamped g == fClamped f, fId g /= i, not (isPegF g), not (Set.member (NFast (fId g)) (sPlaced st))]
            pocketDown = case fPocket f of
              Just (pf, n) | fPocketInClamped f, dot3 n d > 0.5 -> [pf]
              _ -> []
        Right ( Map.unionsWith (+) [term "fastener" 1, term "driver" (if offAxis then 1 else 0), term "sibling" (if isPegF f || null siblings then 0 else 1)]
              , ["driver axis is not vertical here (" ++ fFaceName f ++ " face of " ++ fHost f ++ "); keep the driver square to the bracket" | offAxis]
                  ++ ["the nut pocket on the " ++ pf ++ " face of " ++ fClamped f ++ " faces down here; hold the nut in it while the screw starts" | pf <- pocketDown]
              , Nothing )
    let placedNodes' = Set.insert node (sPlaced st)
        placedI' = [i | NPart n <- Set.toList placedNodes', Just i <- [Map.lookup n (mByName m)]]
        before = sLoose st
        after = loosePieces m d placedNodes' placedI'
        -- one hand does this move; every loose piece needs another
        extra = max 0 (1 + length before - 2)
        handWarn = [ "This needs another pair of hands (or a jig): hold " ++ intercalate ", " (map (intercalate "+") before) ++ " while doing it" | extra > 0 ]
        allTerms = Map.unionsWith (+) [terms, term "hand" (fromIntegral extra), term "loose" (fromIntegral (length after))]
        w = mWeights m
    Right st { sPlaced = placedNodes', sCost = sCost st + price w allTerms, sOrder = sOrder st + price w (orderOnly allTerms)
             , sMoves = MPlace node (warns ++ handWarn) allTerms entry : sMoves st, sLoose = after }

orderOnly :: Terms -> Terms
orderOnly = Map.filterWithKey (\k _ -> k `notElem` workTerms)

-- | Cheapest single-orientation finish: driver costs for the remaining
-- fasteners under each rest face, plus one flip if it is not the current
-- one, plus the least work the remaining joints still need.
heuristic :: Model -> St -> Double
heuristic m st = heuristicWork m st + heuristicOrder m st

-- | The order-dependent part of the estimate (flips, standing rails,
-- tilted drivers, hands through a coming turn).
heuristicOrder :: Model -> St -> Double
heuristicOrder m st = heuristicParts m st False

-- | The least work the remaining joints still need.
heuristicWork :: Model -> St -> Double
heuristicWork m st = heuristicParts m st True

-- | What a move really changes: its order-dependent cost plus the change
-- in what is still expected (a bar dropped in now that must be held
-- through a later flip is not cheap).
marginal :: Model -> St -> St -> Double
marginal m before after = (sOrder after + heuristicOrder m after) - (sOrder before + heuristicOrder m before)

heuristicParts :: Model -> St -> Bool -> Double
heuristicParts m st wantWork =
  let w = mWeights m
      remaining = [f | f <- mFasts m, not (isPegF f), not (Set.member (NFast (fId f)) (sPlaced st))]
      groups = Map.fromListWith (+) [(fFace f, 1 :: Int) | f <- remaining]
      verticalNow n = maybe False (\d -> dot3 n (vneg d) >= cos (setDriverAngle (mSettings m) * pi / 180)) (sRest st)
      placed = placedInsts (mByName m) st
      -- parts still to come that lie below what is already built, or that
      -- can only come in from below, need a flip
      underneath = case sRest st of
        Just d | not (null placed) ->
          let lvl = benchLevel m d placed
              unplaced = [i | i <- mInsts m, not (Set.member (NPart (iName i)) (sPlaced st))]
              fromBelowOnly i = case Map.findWithDefault [] (iName i) (gApproach (mGeo m)) of
                [] -> False
                as -> all (\a -> dot3 (apDir a) d > 1e-6) as
           in if any (\i -> heightMin m d i < lvl - ctolOf m || fromBelowOnly i) unplaced then weight w "flip" else 0
        _ -> 0
      standingSin = sin (setStandingAngle (mSettings m) * pi / 180)
      -- clamped parts still to place whose host rail stands vertical on this rest face
      vertHosts = case sRest st of
        Just d -> sum [weight w "vertical" | i <- mInsts m, not (Set.member (NPart (iName i)) (sPlaced st))
                      , let hosts = [h | f <- mFasts m, fClamped f == iName i, Just h <- [Map.lookup (fHost f) (mByName m)]]
                      , not (null hosts), any (\h -> iRail h && abs (dot3 (iAxis h) d) > standingSin) hosts]
        Nothing -> 0
      unplacedJoints = [f | f <- mFasts m, not (Set.member (NFast (fId f)) (sPlaced st))]
      work = weight w "insert" * sum [Map.findWithDefault 0 (fId f) (gJointBits (mGeo m)) | f <- unplacedJoints]
        + weight w "turn" * sum [minimum (1e9 : [twTurns tw | side <- [fClamped f, fHost f], Just tw <- [Map.lookup (fId f, side) (gTwist (mGeo m))]]) | f <- unplacedJoints, isScrewJoint f, Map.member (fId f, fClamped f) (gTwist (mGeo m))]
        + weight w "part" * fromIntegral (length [i | i <- mInsts m, not (Set.member (NPart (iName i)) (sPlaced st))])
        + weight w "fastener" * fromIntegral (length unplacedJoints)
        + weight w "nut" * fromIntegral (length [() | n@(NNuts _) <- mNodes m, not (Set.member n (sPlaced st))])
      -- a flip still to come must hold every loose piece through the turn
      turnHands = if underneath > 0 then weight w "hand" * fromIntegral (max 0 (length (turnPieces m (sPlaced st) placed) - 2)) else 0
   in if wantWork then work else underneath + turnHands + min vertHosts (2 * weight w "flip") + sum [min (fromIntegral k * weight w "driver") (weight w "flip") | (n, k) <- Map.toList groups, not (verticalNow n)]

tryFlip :: Model -> St -> V3 -> Maybe St
tryFlip m st d
  | sRest st == Just d = Nothing
  | not (stable m d placed) = Nothing
  | otherwise =
      -- the first rest face is free; a tiny tie-break prefers the listed order (bottom first)
      let idx = fromIntegral (length (takeWhile (/= d) (mRestDirs m))) * 0.001
          loose = if sRest st == Nothing then [] else turnPieces m (sPlaced st) placed
          -- the hands turning the assembly can each press one loose piece to it
          extra = max 0 (length loose - 2)
          warns = ["Hold " ++ intercalate ", " (map (intercalate "+") loose) ++ " against the assembly while turning it; their joints do not hold them yet" | not (null loose)]
                  ++ ["This turn needs another pair of hands (or a jig) to keep every loose piece in place" | extra > 0]
          terms = Map.unionsWith (+) [term "flip" (if sRest st == Nothing then 0 else 1), term "hand" (fromIntegral extra)]
          w = mWeights m
       in Just st {sRest = Just d, sCost = sCost st + price w terms + idx, sOrder = sOrder st + price w terms + idx, sMoves = MFlip d warns terms : sMoves st
                  , sLoose = loosePieces m d (sPlaced st) placed}
  where
    placed = placedInsts (mByName m) st

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
-- A free move (no order-dependent cost, e.g. laying another rail on the
-- bench) is taken alone: the permutations of free placements are all
-- equivalent, and branching on them is what blows the search up.
successors :: Model -> St -> [St]
successors m st =
  case [s' | s' <- placements, marginal m st s' <= 1e-9] of
    (free : _) -> [free]
    [] ->
      placements
        ++ [s' | d <- mRestDirs m, Just s <- [tryFlip m st d], not (lastWasFlip st), let s' = settle m s, enables st s']
        ++ [settle m st | not (null (followUps st))] -- leftovers (a screw that became reachable without a flip)
  where
    placements = [s' | NPart p <- available m st, Right s <- [tryPlace m st (NPart p)], let s' = settle m s, viable m s']
    lastWasFlip s = case sMoves s of (MFlip {} : _) -> True; _ -> False
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
    st0 = St Set.empty Nothing 0 0 [] []
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
-- cheap (order-dependent cost at most `phase_tau`); the outer search
-- decides flips.
phaseGreedy :: Model -> Double -> St -> St
phaseGreedy m tau st =
  case sortOn (\s -> (marginal m st s, sCost s - sCost st, negate (Set.size (sPlaced s)))) (macroPlacements m st) of
    (s : _) | marginal m st s <= tau + 1e-9 -> phaseGreedy m tau s
    _ -> st

macroPlacements :: Model -> St -> [St]
macroPlacements m st =
  [s' | NPart p <- available m st, Right s <- [tryPlace m st (NPart p)], let s' = settle m s, viable m s']
    ++ [settle m st | not (null [n | n <- available m st, case n of NPart _ -> False; _ -> True, Right _ <- [tryPlace m st n]])]

-- | Phase successors: flip to another rest face and run a cheap phase
-- there, or stay and accept exactly one expensive move (then continue cheaply).
phaseSuccessors :: Model -> St -> [St]
phaseSuccessors m st = concatMap afterFlip flips ++ expensive st
  where
    tau = setPhaseTau (mSettings m)
    flips = [phaseGreedy m tau (settle m s) | d <- mRestDirs m, Just s <- [tryFlip m st d]]
    -- a flip that opened cheap work stands on its own; one that did not
    -- must carry the expensive move it was for, so flip ping-pong costs real points
    afterFlip s
      | Set.size (sPlaced s) > Set.size (sPlaced st) = [s]
      | otherwise = expensive s
    expensive s = [phaseGreedy m tau x | x <- take 3 (sortOn (marginal m s) (macroPlacements m s)), marginal m s x > tau]

beamSearch :: Model -> Int -> Either [String] St
beamSearch m width = go (0 :: Int) [St Set.empty Nothing 0 0 [] []] [] Nothing
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
            : ("placed so far: " ++ intercalate ", " [n | MPlace (NPart n) _ _ _ <- reverse (sMoves s)])
            : [describe n ++ ": " ++ why | n <- available m s, Left why <- [tryPlace m s n]]
            ++ ["flip to " ++ dirName d ++ ": " ++ maybe "not stable" (const "ok") (tryFlip m s d) | d <- mRestDirs m]
    describe n = case n of
      NPart p -> "place " ++ p
      NNuts h -> "preload " ++ h
      NFast i -> let f = mFastById m Map.! i in fSpec f ++ " into " ++ fHost f ++ " (" ++ fClamped f ++ ")"

-- ------------------------------------------------------------------
-- steps

-- | stEntry: each placed part with the direction it leaves by (it goes in
-- along the opposite); stTerms: the step's cost terms.
data Step = Step {stRest :: V3, stFlip :: Bool, stParts :: [String], stNuts :: [String], stFasts :: [Int], stWarn :: [String], stTerms :: Terms, stEntry :: [(String, V3)]}

toSteps :: Int -> [Move] -> [Step]
toSteps maxParts = finish . foldl' add (Nothing, [])
  where
    finish (cur, acc) = reverse (maybe acc (: acc) cur)
    plus s ts = s {stTerms = Map.unionWith (+) (stTerms s) ts}
    add (cur, acc) mv = case mv of
      MFlip d warns ts -> (Just (Step d (maybe False (const True) cur) [] [] [] warns ts []), maybe acc (: acc) cur)
      MPlace node warns ts entry -> case cur of
        Nothing -> (Nothing, acc) -- cannot happen: a flip always comes first
        Just s -> case node of
          NPart p
            | null (stFasts s) && null (stNuts s) && length (stParts s) < maxParts -> (Just (plus s ts) {stParts = stParts s ++ [p], stWarn = stWarn s ++ warns, stEntry = stEntry s ++ ent p entry}, acc)
            | otherwise -> (Just (Step (stRest s) False [p] [] [] warns ts (ent p entry)), s : acc)
          NNuts h
            | null (stFasts s) -> (Just (plus s ts) {stNuts = stNuts s ++ [h], stWarn = stWarn s ++ warns}, acc)
            | otherwise -> (Just (Step (stRest s) False [] [h] [] warns ts []), s : acc)
          NFast i -> (Just (plus s ts) {stFasts = stFasts s ++ [i], stWarn = stWarn s ++ warns}, acc)
    ent p = maybe [] (\e -> [(p, e)])

runSearch :: AsmResult -> Model -> Either [String] St
runSearch ar model =
  let width = maybe 30 round (lookup "beam" (arPlanOpts ar) >>= readD)
   in case lookup "astar" (arPlanOpts ar) >>= readD of
        Just cap | Just s <- astar model (round cap) -> Right s
        _ -> beamSearch model width
