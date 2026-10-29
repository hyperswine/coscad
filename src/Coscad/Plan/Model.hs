-- | `coscad plan`, part 1: the model. Part instances and their placed
-- boxes, fasteners and nut batches, the requires-before relation, and
-- design errors in the hardware.
module Coscad.Plan.Model
  ( Fast (..)
  , Inst (..)
  , JointKind (..)
  , Material (..)
  , Model (..)
  , Move (..)
  , Node (..)
  , NutKind (..)
  , Settings (..)
  , Weights
  , Terms
  , Geo (..)
  , Approach (..)
  , Sweep (..)
  , Twist (..)
  , emptyGeo
  , axes6
  , axisIndex
  , restName
  , weight
  , price
  , workTerms
  , defaultWeights
  , isJoint
  , retains
  , jointPartners
  , affApply
  , basis
  , project
  , convexHull
  , bExpand
  , bIntersects
  , buildModel
  , dirName
  , endCaps
  , eps
  , extentAlong
  , fmtMm
  , headRadius
  , isPegF
  , nutHolder
  , nutName
  , readD
  , slender
  , torqueWords
  )
where


import Control.Monad (unless, when)
import Coscad.Assemble
import Coscad.Constraint (Constraint (..))
import Coscad.Codegen (showD)
import Coscad.Geometry
import Coscad.Validate (finite, readFinite, maxQuantity)
import Coscad.Shape
import Data.Char (isDigit, toLower)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (foldl', intercalate, nub, sort, sortOn, zip4)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Text.Printf (printf)


-- ------------------------------------------------------------------
-- affine transforms and instance enumeration

type Aff = (M3, V3)

affApply :: Aff -> V3 -> V3
affApply (m, t) p = vadd (mApply m p) t

affCompose :: Aff -> Aff -> Aff
affCompose (m1, t1) (m2, t2) = (matMul m1 m2, vadd (mApply m1 t2) t1)

diag :: V3 -> M3
diag (x, y, z) = ((x, 0, 0), (0, y, 0), (0, 0, z))

-- | Every Tag'd subtree with its accumulated world transform.
instancesOf :: Shape -> [(String, Aff, Shape)]
instancesOf = go (matIdentity, (0, 0, 0))
  where
    go aff s = case s of
      Tag n x -> [(n, aff, x)]
      Datum _ _ x -> go aff x
      Frame _ _ _ x -> go aff x
      Tx d x -> go (affCompose aff (matIdentity, (d, 0, 0))) x
      Ty d x -> go (affCompose aff (matIdentity, (0, d, 0))) x
      Tz d x -> go (affCompose aff (matIdentity, (0, 0, d))) x
      Translate v x -> go (affCompose aff (matIdentity, v)) x
      Rx a x -> go (affCompose aff (rodMat a (1, 0, 0), (0, 0, 0))) x
      Ry a x -> go (affCompose aff (rodMat a (0, 1, 0), (0, 0, 0))) x
      Rz a x -> go (affCompose aff (rodMat a (0, 0, 1), (0, 0, 0))) x
      RotAxis a v x -> go (affCompose aff (rodMat a v, (0, 0, 0))) x
      Transform3 m t x -> go (affCompose aff (m,t)) x
      Scale v x -> go (affCompose aff (diag v, (0, 0, 0))) x
      Mirror n x -> go (affCompose aff (mirrorMat n, (0, 0, 0))) x
      Diff a b -> go aff a ++ go aff b
      Union xs -> concatMap (go aff) xs
      Intersection xs -> concatMap (go aff) xs
      Hull xs -> concatMap (go aff) xs
      Minkowski xs -> concatMap (go aff) xs
      Extrude _ x -> go aff x
      Offset _ x -> go aff x
      _ -> []

-- ------------------------------------------------------------------
-- model

data Material = Aluminium | Printed | Steel deriving (Eq, Show)

data Inst = Inst
  { iName :: String
  , iPart :: String
  , iAff :: Aff
  , iShape :: Shape
  , iBox :: BBox
  , iMat :: Material
  , iRail :: Bool
  , iAxis :: V3 -- unit vector along the longest dimension (rails: slot direction)
  , iEnds :: (Bool, Bool) -- (min end open, max end open)
  , iTorque :: Maybe Double
  , iMass :: Double -- grams (from the mesh once the geometry is prepared)
  , iMassHint :: Maybe Double -- `mass=` on the part, which the mesh never overrides
  , iCom :: V3 -- centre of mass, world frame (box centre until the geometry is prepared)
  , iPreloadInHand :: Bool
  }

-- | DropIn / SlideIn are T-nuts in a rail slot; Hex is a plain nut captive
-- in a pocket of the host (`nut=hex pocket=<face>`: the face the pocket opens on).
-- NoNut: the screw threads into the host itself (`nut=none`), or the joint is a
-- friction peg (spec `peg`): nothing to preload.
data NutKind = DropIn | SlideIn | Hex | NoNut deriving (Eq, Show)

-- | The nut kind as it appears in plan JSON and on the site: the same
-- words the `.assemble` `nut=` option takes.
nutName :: NutKind -> String
nutName k = case k of
  DropIn -> "dropin"
  SlideIn -> "slidein"
  Hex -> "hex"
  NoNut -> "none"

-- | How a joint is made. Bolt: a screw with a driver (`fastener M5x8`).
-- Peg: `fastener peg`, a friction joint pushed home along its face normal.
-- The rest come from `joint` lines and move along the mate's axis: Push
-- (a press or snap fit that holds once seated), Drop (an open socket the
-- part only rests in: gravity or capture keeps it), Screw (an integral
-- thread, pitch in mm: the moving part turns as it goes in).
data JointKind = Bolt | Peg | Push | Drop | Screw Double deriving (Eq, Show)

data Fast = Fast
  { fId :: Int
  , fKind :: JointKind
  , fSpec :: String -- "M5x8"
  , fDiam :: Double
  , fLen :: Double
  , fHead :: String -- button | socket
  , fClamped :: String
  , fHost :: String
  , fFaceName :: String
  , fInset :: Double -- peg seating plane below the named outer face
  , fFace :: V3 -- outward normal of the host face = driver axis (head outward)
  , fNut :: NutKind
  , fPocket :: Maybe (String, V3) -- hex nuts: the face the pocket opens on
  , fPocketInClamped :: Bool -- the pocket is in the clamped part (`pocket=clamped.top`), not the host
  , fPos :: V3 -- point on the host face
  , fHeadPos :: V3 -- bearing seat under the screw head (also the driver start)
  , fPocketSeat :: Maybe V3 -- explicit hex-nut bearing seat, when frame-based
  , fPocketEntry :: Maybe V3 -- ray from that seat to the holder's local box boundary
  , fSeatName :: Maybe String
  , fAlong :: Double -- mm from the host's marked end (min end along its axis)
  , fTorque :: Double
  , fDriverLen :: Double
  , fDriverRad :: Double
  , fReach :: Double -- thread inside the host = screw length minus clamped thickness
  }

data Node = NPart String | NNuts String | NFast Int deriving (Eq, Ord, Show)

-- | A placement carries its warnings, its cost terms and, for a part, the
-- direction it leaves by (it comes in along the opposite).
data Move = MPlace Node [String] Terms (Maybe V3) | MFlip V3 [String] Terms

-- | Cost weights by term name; a plan's cost is the sum of each term times
-- its weight. Terms are counted per move (see 'Terms').
type Weights = Map.Map String Double

-- | What one move costs, before weighting: term name to amount.
type Terms = Map.Map String Double

-- | The defaults, with their units. They are guesses until calibrated
-- against timed builds (`coscad calibrate`); after that they are seconds.
--
-- flip: per flip. vertical: per move with a host rail or slender part on end.
-- driver: per screw driven off vertical. shrink: per move that shrinks the
-- bench footprint. sibling: per screw that leaves another on the same part
-- undone. loose: per loose piece after each move. insert: per bit of the
-- Fitts index of a joint's alignment (log2 of engaged length over play).
-- turn: per turn of a screw joint. hand: per hand needed beyond two.
-- finger: per move, times the fraction of the finger room other parts take.
-- part, fastener, nut: base handling per part, screw or joint, and nut batch
-- (zero by default: every order pays them equally; calibration sets them).
defaultWeights :: Weights
defaultWeights = Map.fromList
  [ ("flip", 10), ("vertical", 6), ("driver", 3), ("shrink", 2), ("sibling", 1), ("loose", 1)
  , ("insert", 0.5), ("turn", 0.3), ("hand", 20), ("finger", 2)
  , ("part", 0), ("fastener", 0), ("nut", 0) ]

weight :: Weights -> String -> Double
weight w k = Map.findWithDefault 0 k w

price :: Weights -> Terms -> Double
price w ts = sum [weight w k * v | (k, v) <- Map.toList ts]

-- | Terms that every complete plan pays about equally: the work of the
-- joints themselves. The phase search's "cheap move" test ignores them,
-- and the completion heuristic estimates what is left of them.
workTerms :: [String]
workTerms = ["insert", "turn", "part", "fastener", "nut"]

-- | Thresholds and the hand and print models, each a `plan` option.
data Settings = Settings
  { setDriverAngle :: Double -- degrees off vertical before a driver costs `driver`
  , setStandingAngle :: Double -- degrees above horizontal at which a rail or bar counts as standing
  , setSlender :: Double -- length over width at which a part is slender
  , setDriverLength :: Double -- mm, the driver's reach above the screw head
  , setDriverMargin :: Double -- mm added to the screw head radius for the driver
  , setPhaseTau :: Double -- the phase search's "cheap move" threshold
  , setStepParts :: Int -- parts placed together in one step at most
  , setFinger :: Double -- mm, fingertip size (room to push or grip)
  , setContact :: Double -- mm, distance that counts as touching
  , setSettle :: Double -- mm a part may drop to come to rest in its seat (print clearance)
  , setSweepStep :: Double -- mm, the finest step of an insertion sweep
  , setShell :: Double -- mm of solid perimeter on a printed part
  , setInfill :: Double -- infill fraction inside that shell
  }

settingsFrom :: [(String, String)] -> Settings
settingsFrom opts = Settings (o "driver_angle" 30) (o "standing_angle" 17.5) (o "slender" 3) (o "driver_length" 45)
  (o "driver_margin" 2) (o "phase_tau" 2.5) (round (o "step_parts" 4)) (o "finger_size" 16) (o "contact" 0.1) (o "settle" 0.5)
  (o "sweep_step" 0.5) (o "shell" 0.85) (o "infill" 0.15)
  where o k d = fromMaybe d (lookup k opts >>= readD)

-- | The six world faces, in a fixed order.
axes6 :: [V3]
axes6 = [(0, 0, -1), (0, 0, 1), (-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0)]

-- | A rest direction's index in the model's tables.
axisIndex :: Model -> V3 -> Maybe Int
axisIndex m d = lookup True (zip [vlen (vsub d a) < 1e-9 | (a, _) <- mAxes m] [0 ..])

-- | A rest direction in words: a world face, or a face of the base part
-- when the assembly is not square to the world.
restName :: Model -> V3 -> String
restName m d = maybe (dirName d) id (lookup True [(vlen (vsub d a) < 1e-9, nm) | (a, nm) <- mAxes m])

-- | One way a part can come in: it travels along -apDir to its seat (apDir
-- is the way it would leave). Measured once against every other part.
data Approach = Approach
  { apDir :: V3
  , apObstacles :: Map.Map String Sweep -- instances this path comes near
  , apZoneVol :: Double -- mm³ of finger room behind the part along apDir
  , apZoneNeed :: Double -- mm³ a fingertip needs there
  , apZone :: Map.Map String Double -- mm³ of that room each instance takes
  , apBlockers :: [String] -- the instances in apObstacles that block it
  }

-- | The path against one other instance at its final pose.
data Sweep = Sweep
  { swBlocked :: Bool -- the part cannot pass it this way
  , swBits :: Double -- for a joint partner: log2(1 + engaged length / play)
  , swTravel :: Double -- mm the part stays in touch with it moving this way (to clear it)
  }

-- | The moving side of a screw joint.
data Twist = Twist
  { twTurns :: Double
  , twBlockers :: [String] -- instances its rotation at the seat runs into
  , twZoneVol :: Double -- mm³ of grip room around it
  , twZoneNeed :: Double -- mm³ two fingertips need there
  , twZone :: Map.Map String Double
  }

-- | Mesh-derived tables, filled by Coscad.Plan.Geometry before the search.
data Geo = Geo
  { gReady :: Bool
  , gExt :: Map.Map (String, Int) (Double, Double) -- an instance's extent along rest direction k (mAxes)
  , gFoot :: Map.Map (String, Int) [(Double, Double)] -- its lowest vertices with rest direction k down, projected
  , gContact :: Map.Map (String, String, Int) [(Double, Double)] -- p resting on q, rest direction k down: the contact patch
  , gApproach :: Map.Map String [Approach]
  , gTwist :: Map.Map (Int, String) Twist -- (screw joint, moving instance)
  , gJointBits :: Map.Map Int Double -- the least alignment work of each joint over its sides and approaches
  }

emptyGeo :: Geo
emptyGeo = Geo False Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

eps :: Double
eps = 0.5

-- ------------------------------------------------------------------
-- small geometry

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


bIntersects :: BBox -> BBox -> Bool
bIntersects ((a, b, c), (d, e, f)) ((p, q, r), (s, t, u)) =
  a <= s + eps && p <= d + eps && b <= t + eps && q <= e + eps && c <= u + eps && r <= f + eps

bExpand :: Double -> BBox -> BBox
bExpand k ((a, b, c), (d, e, f)) = ((a - k, b - k, c - k), (d + k, e + k, f + k))

-- Exact distance between finite screw axes. Their enclosing boxes can
-- overlap when oblique shanks are well clear of one another.
segmentDistance :: (V3,V3) -> (V3,V3) -> Double
segmentDistance (p,q) (r,s) = minimum (ends ++ interior)
  where
    u = vsub q p; v = vsub s r; w = vsub p r
    a = dot3 u u; b = dot3 u v; c = dot3 v v
    d = dot3 u w; e = dot3 v w; den = a*c-b*b
    clamp01 = max 0 . min 1
    pointSeg x y z = let yz = vsub z y; len2 = dot3 yz yz
                         in vlen (vsub x (vadd y (scale3 (if len2 < 1e-16 then 0 else clamp01 (dot3 (vsub x y) yz / len2)) yz)))
    ends = [pointSeg p r s, pointSeg q r s, pointSeg r p q, pointSeg s p q]
    t = (b*e-c*d)/den; h = (a*e-b*d)/den
    interior = [vlen (vsub (vadd p (scale3 t u)) (vadd r (scale3 h v))) | den > 1e-12*a*c, t >= 0, t <= 1, h >= 0, h <= 1]

anchorDir :: String -> Maybe V3
anchorDir w = case w of
  "top" -> Just (0, 0, 1); "up" -> Just (0, 0, 1)
  "bot" -> Just (0, 0, -1); "down" -> Just (0, 0, -1); "dn" -> Just (0, 0, -1)
  "rt" -> Just (1, 0, 0); "right" -> Just (1, 0, 0)
  "lft" -> Just (-1, 0, 0); "left" -> Just (-1, 0, 0)
  "fwd" -> Just (0, -1, 0); "front" -> Just (0, -1, 0)
  "bak" -> Just (0, 1, 0); "back" -> Just (0, 1, 0)
  _ -> Nothing

dirName :: V3 -> String
dirName d
  | d == (0, 0, -1) = "bottom (-Z) face"
  | d == (0, 0, 1) = "top (+Z) face"
  | d == (-1, 0, 0) = "left (-X) face"
  | d == (1, 0, 0) = "right (+X) face"
  | d == (0, -1, 0) = "front (-Y) face"
  | d == (0, 1, 0) = "back (+Y) face"
  | otherwise = show d

-- | The extent of a box along a unit axis direction.
extentAlong :: V3 -> BBox -> (Double, Double)
extentAlong n bb = let ds = map (dot3 n) (bcorners bb) in (minimum ds, maximum ds)

longAxis :: BBox -> V3
longAxis ((x0,y0,z0),(x1,y1,z1)) = snd (last (sortOn fst [(x1-x0,(1,0,0)),(y1-y0,(0,1,0)),(z1-z0,(0,0,1))]))

-- ------------------------------------------------------------------
-- building the model from an AsmResult

materialOf :: Int -> [(String, String)] -> Material
materialOf cnt hints = case map toLower <$> lookup "material" hints of
  Just "aluminium" -> Aluminium
  Just "aluminum" -> Aluminium
  Just "printed" -> Printed
  Just "steel" -> Steel
  Just m | m `elem` ["printed", "pla", "petg", "abs", "asa", "tpu", "nylon", "pc"] -> Printed
  _ -> if cnt > 0 then Printed else Aluminium

readD :: String -> Maybe Double
readD = readFinite

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, _ : r) -> a : splitOn c r

mkInstances :: AsmResult -> Shape -> Either String [Inst]
mkInstances ar asmShape = do
  let occ = instancesOf (resolve asmShape)
      counts = Map.fromListWith (+) [(n, 1 :: Int) | (n, _, _) <- occ]
      numbered = snd (foldl' step (Map.empty, []) occ)
      step (seen, acc) (n, aff, sh) =
        let k = Map.findWithDefault (0 :: Int) n seen + 1
            nm = if Map.findWithDefault 1 n counts > 1 then n ++ "#" ++ show k else n
         in (Map.insert n k seen, acc ++ [(nm, n, aff, sh)])
  mapM mk numbered
  where
    mk (nm, part, aff, sh) = do
      let (cnt, hints) = fromMaybe (1, []) (lookup part (arPartMeta ar))
          bb = fromCorners (map (affApply aff) (bcorners (bbox sh)))
          ((x0, y0, z0), (x1, y1, z1)) = bb
          dims = [(x1 - x0, (1, 0, 0)), (y1 - y0, (0, 1, 0)), (z1 - z0, (0, 0, 1))]
          (_, axis) = last (sortOn fst dims)
          isRail = lookup "profile" hints == Just "2020"
          ends = case lookup "ends" hints of
            Just e -> let ws = map (map toLower) (splitOn ',' e) in (take 1 ws /= ["blocked"], take 1 (drop 1 ws) /= ["blocked"])
            Nothing -> (True, True)
          mat = materialOf cnt hints
          vol = (x1 - x0) * (y1 - y0) * (z1 - z0)
          density = case mat of Aluminium -> 0.0027; Printed -> 0.00125; Steel -> 0.0079
          fill = if isRail then 0.37 else if mat == Printed then 0.3 else 1
          mass = fromMaybe (vol * density * fill) (lookup "mass" hints >>= readD)
      mapM_ validatePartHint hints
      when (not (finite mass) || mass <= 0) $ Left ("part '" ++ nm ++ "': mass estimate must be finite and greater than zero; reduce dimensions or supply mass= in grams")
      Right Inst
        { iName = nm, iPart = part, iAff = aff, iShape = sh, iBox = bb, iMat = mat, iRail = isRail
        , iAxis = axis, iEnds = ends, iTorque = lookup "torque" hints >>= readD, iMass = mass
        , iMassHint = lookup "mass" hints >>= readD, iCom = bcenter bb
        , iPreloadInHand = (map toLower <$> lookup "preload" hints) == Just "hand"
        }

-- fastener library -------------------------------------------------
headRadius :: String -> Double -> Double
headRadius hd d = case hd of
  "socket" -> d * 0.85
  _ -> d * 0.95 -- button head

defaultTorque :: Material -> Double -> Double
defaultTorque mat d = case mat of
  Printed -> if d <= 3 then 0.4 else 0.6
  _ -> if d <= 3 then 1.0 else if d <= 4 then 2.0 else if d <= 5 then 3.0 else 5.0

torqueWords :: Material -> Double -> String
torqueWords mat nm = case mat of
  Printed -> printf "%.1f Nm (printed part: finger tight + ¼ turn, no power driver)" nm
  _ -> printf "%.1f Nm" nm

parseSpec :: String -> Maybe (Double, Double)
parseSpec s = case map toLower s of
  "peg" -> Just (0, 0) -- friction joint: no screw, no nut, no driver
  ('m' : rest) -> case break (== 'x') rest of
    (d, 'x' : l) | all isDigitOrDot d, all isDigitOrDot l, not (null d), not (null l) -> do
      diameter <- readD d
      len <- readD l
      if diameter > 0 && len > 0 then Just (diameter, len) else Nothing
    _ -> Nothing
  _ -> Nothing
  where
    isDigitOrDot c = isDigit c || c == '.'

mkFasteners :: Settings -> [Constraint] -> FilePath -> [Inst] -> [FastDecl] -> Either String [Fast]
mkFasteners sets constraints srcFile insts decls = do
  fs <- concat <$> mapM one (zip [1 ..] decls)
  when (length fs > maxQuantity) $ Left ("total fastener count is too large; maximum is " ++ show maxQuantity)
  -- IDs must remain unique when one declaration expands to 100+ joints.
  Right [f {fId = i} | (i, f) <- zip [1 ..] fs]
  where
    byName = Map.fromList [(iName i, i) | i <- insts]
    byPart = Map.fromListWith (++) [(iPart i, [i]) | i <- insts]
    find who = case Map.lookup who byName of
      Just i -> Right i
      Nothing -> case Map.lookup who byPart of
        Just [i] -> Right i
        Just is -> Left ("'" ++ who ++ "' is placed " ++ show (length is) ++ " times; refer to one instance: " ++ intercalate ", " (map iName is))
        Nothing -> Left ("unknown part '" ++ who ++ "' (not placed in asm)")
    one (k, fd) = do
      let at = srcFile ++ ":" ++ show (fdLine fd) ++ ":1: fastener: "
          at' = srcFile ++ ":" ++ show (fdLine fd) ++ ":1: joint: "
          isJointDecl = take 6 (fdSpec fd) == "joint:"
      unless isJointDecl $ mapM_ (either (Left . (at ++)) Right . validateFastenerHint) (fdOpts fd)
      when (fdCount fd < 1 || fdCount fd > maxQuantity) $ Left (at ++ "fastener count must be from 1 to " ++ show maxQuantity)
      (d, l) <- if isJointDecl then Right (0, 0) else maybe (Left (at ++ "cannot read screw spec '" ++ fdSpec fd ++ "'; screw diameter and length must be positive (expected e.g. M5x8, or use peg for a peg joint)")) Right (parseSpec (fdSpec fd))
      clamped <- either (Left . ((if isJointDecl then at' else at) ++)) Right (find (fdClamped fd))
      host <- either (Left . ((if isJointDecl then at' else at) ++)) Right (find (fdHost fd))
      when (iName clamped == iName host) $ Left (at ++ "clamped part and host must be different instances")
      if take 6 (fdSpec fd) == "joint:"
        then either (Left . (at' ++)) Right (jointFastener constraints k fd clamped host)
      else if take 1 (fdFace fd) == "@"
        then either (Left . (at ++)) Right (framedFastener sets k fd d l clamped host)
        else legacy k fd at d l clamped host
    legacy k fd at d l clamped host = do
      when (lookup "seat" (fdOpts fd) /= Nothing) $ Left (at ++ "seat= requires a host @frame instead of a world face")
      when (maybe False (elem '@') (lookup "pocket" (fdOpts fd))) $ Left (at ++ "a pocket @frame requires a host @frame instead of a world face")
      n <- maybe (Left (at ++ "unknown face '" ++ fdFace fd ++ "' (top bot lft rt fwd bak)")) Right (anchorDir (fdFace fd))
      inset <- case lookup "inset" (fdOpts fd) of
        Nothing -> Right 0
        Just s -> maybe (Left (at ++ "inset= needs a distance in mm")) Right (readD s)
      let hb = iBox host
          cb = iBox clamped
          (hostLo, hostHi) = extentAlong n hb
          facePlane = hostHi - inset
          (cMin, cMax) = extentAlong n cb
      when (inset < 0 || inset > hostHi - hostLo) $ Left (at ++ "inset= must lie within the host")
      when (inset /= 0 && l > 0) $ Left (at ++ "inset= only applies to peg joints")
      when (cMin > facePlane + 1.5 || cMax < facePlane - 1.5) $
        Left (at ++ iName clamped ++ " does not sit on the " ++ fdFace fd ++ " face of " ++ iName host)
      -- overlap rectangle in the two in-plane axes
      let axes = [a | a <- [(1, 0, 0), (0, 1, 0), (0, 0, 1)], abs (dot3 a n) < 0.5]
          overlap a = let (h0, h1) = extentAlong a hb; (c0, c1) = extentAlong a cb in (max h0 c0, min h1 c1)
          ranges = map overlap axes
      when (any (\(lo, hi) -> hi < lo) ranges) $
        Left (at ++ iName clamped ++ " does not overlap " ++ iName host ++ " on its " ++ fdFace fd ++ " face")
      axis0 <- case axes of
        a : _ -> Right a
        [] -> Left (at ++ "internal error: the " ++ fdFace fd ++ " face of " ++ iName host ++ " has no in-plane axis")
      let along = if abs (dot3 (iAxis host) n) < 0.5 then iAxis host else axis0
      other <- case [a | a <- axes, a /= along] of
        a : _ -> Right a
        [] -> Left (at ++ "internal error: the " ++ fdFace fd ++ " face of " ++ iName host ++ " has one in-plane axis")
      let (l0, l1) = overlap along
          (o0, o1) = overlap other
          (hostMin, _) = extentAlong along hb
          -- A 2020 side slot is centred on the host cross-section. Cropping
          -- the overlap with an inset panel must not move the screw off it.
          otherPos = if iRail host && abs (dot3 (iAxis host) n) < 0.5
            then dot3 (bcenter hb) other else (o0 + o1) / 2
          nutKind = case map toLower <$> lookup "nut" (fdOpts fd) of
            Just "slidein" -> SlideIn
            Just "slide-in" -> SlideIn
            Just "hex" -> Hex
            Just "captive" -> Hex
            Just "none" -> NoNut
            _ -> if isPeg then NoNut else DropIn
          isPeg = l <= 0
          hd = if isPeg then "peg" else map toLower (fromMaybe "button" (lookup "head" (fdOpts fd)))
          count = fdCount fd
          positions = case lookup "at" (fdOpts fd) of
            Just s | Just ps <- mapM readD (splitOn ',' s) -> map (+ hostMin) ps
            _ -> [l0 + (l1 - l0) * (fromIntegral i - 0.5) / fromIntegral count | i <- [1 .. count]]
          torque = fromMaybe (fromMaybe (defaultTorque (iMat host) d) (iTorque host)) (lookup "torque" (fdOpts fd) >>= readD)
      let (pocketInClamped, pocketFace) = case lookup "pocket" (fdOpts fd) of
            Just ('c' : 'l' : 'a' : 'm' : 'p' : 'e' : 'd' : '.' : pf) -> (True, Just pf)
            Just ('h' : 'o' : 's' : 't' : '.' : pf) -> (False, Just pf)
            plain -> (False, plain)
      pocket <- case pocketFace of
        Nothing -> Right Nothing
        Just pf -> maybe (Left (at ++ "unknown pocket face '" ++ pf ++ "' (top bot lft rt fwd bak, optionally clamped.<face>)")) (Right . Just . (,) pf) (anchorDir pf)
      when (nutKind /= Hex && pocket /= Nothing) $ Left (at ++ "pocket= only applies to nut=hex")
      let
          thickness = fromMaybe (cMax - max cMin facePlane) (lookup "through" (fdOpts fd) >>= readD)
          mkF (j, p) =
            let pos = vadd (scale3 p along) (vadd (scale3 otherPos other) (scale3 facePlane n))
             in Fast
                  { fId = k * 100 + j, fKind = if isPeg then Peg else Bolt, fSpec = fdSpec fd, fDiam = d, fLen = l, fHead = hd
                  , fClamped = iName clamped, fHost = iName host, fFaceName = fdFace fd, fInset = inset, fFace = n
                  , fNut = nutKind, fPocket = pocket, fPocketInClamped = pocketInClamped, fPos = pos, fAlong = p - hostMin, fTorque = if isPeg then 0 else torque
                  , fHeadPos = vadd pos (scale3 (cMax - facePlane) n), fPocketSeat = Nothing, fPocketEntry = Nothing, fSeatName = Nothing
                  , fDriverLen = if isPeg then 0 else setDriverLength sets, fDriverRad = if isPeg then 0 else headRadius hd d + setDriverMargin sets
                  , fReach = if isPeg then 0 else l - thickness
                  }
      when (length positions > maxQuantity) $ Left (at ++ "at= contains too many positions; maximum is " ++ show maxQuantity)
      when (otherPos < o0 - 1e-6 || otherPos > o1 + 1e-6) $ Left (at ++ iName clamped ++ " does not cover the 2020 slot centre of " ++ iName host)
      when (any (not . finite) positions) $ Left (at ++ "fastener positions overflowed; reduce at= distances or host dimensions")
      Right (map mkF (zip [1 ..] positions))

-- A frame names a real hole/seat in the part's own coordinates. Carry both
-- origin and axis through the solved instance pose; never guess a transverse
-- hole coordinate or a recessed bearing seat from a bounding box.
instanceFrame :: Inst -> String -> Either String MatingFrame
instanceFrame i ref = do
  validFrameOption ref
  local <- frameOf (drop 1 ref) (iShape i)
  let p = frameOrigin local
      (lo, hi) = bbox (iShape i)
      inside (x,y,z) (a,b,c) (d,e,f) = and [x >= a-1e-6, x <= d+1e-6, y >= b-1e-6, y <= e+1e-6, z >= c-1e-6, z <= f+1e-6]
  unless (inside p lo hi) $ Left (ref ++ " is outside part '" ++ iName i ++ "'")
  let (m,t) = iAff i
  frameOf (drop 1 ref) (Transform3 m t (iShape i))

pocketEntry :: Inst -> String -> Either String V3
pocketEntry i ref = do
  local <- frameOf (drop 1 ref) (iShape i)
  let p = frameOrigin local
      n = mApply (frameBasis local) (0,0,1)
      (lo,hi) = bbox (iShape i)
      coords (x,y,z) = [x,y,z]
      distances = [(if a > 0 then h-x else l-x)/a | (x,a,l,h) <- zip4 (coords p) (coords n) (coords lo) (coords hi), abs a > 1e-12]
  Right (affApply (iAff i) (vadd p (scale3 (minimum distances) n)))

framedFastener :: Settings -> Int -> FastDecl -> Double -> Double -> Inst -> Inst -> Either String [Fast]
framedFastener sets k fd d l clamped host = do
  when (fdCount fd /= 1 || lookup "at" opts /= Nothing) $ Left "a host @frame locates one fastener; omit ×count/at= and declare each hole explicitly"
  when (lookup "inset" opts /= Nothing || lookup "through" opts /= Nothing) $ Left "a host @frame uses seat= and pocket= frames; omit inset=/through="
  when (l <= 0) $ Left "host @frames currently describe screws; use a face and inset= for peg joints"
  hf <- instanceFrame host (fdFace fd)
  let pos = frameOrigin hf
      n = mApply (frameBasis hf) (0,0,1)
      nutKind = case map toLower <$> lookup "nut" opts of
        Just "none" -> NoNut
        Just "hex" -> Hex
        Just "captive" -> Hex
        Just "slidein" -> SlideIn
        Just "slide-in" -> SlideIn
        _ -> DropIn
      hd = map toLower (fromMaybe "button" (lookup "head" opts))
      torque = fromMaybe (fromMaybe (defaultTorque (iMat host) d) (iTorque host)) (lookup "torque" opts >>= readD)
  seatRef <- maybe (Left "a screw at a host @frame needs seat=@frame on the clamped part (under its head)") Right (lookup "seat" opts)
  sf <- instanceFrame clamped seatRef
  let headPos = frameOrigin sf
      gripTo p = dot3 (vsub headPos p) n
      coaxial p = vlen (vsub (vsub p pos) (scale3 (dot3 (vsub p pos) n) n)) < 1e-5
      sn = mApply (frameBasis sf) (0,0,1)
  unless (coaxial headPos && dot3 n sn > 1-1e-8) $ Left "head seat and host hole frames must be coaxial with matching outward normals"
  when (gripTo pos < -1e-6) $ Left "head seat is behind the host interface; reverse the hole frame or fix the seating geometry"
  (inClamped, pocket, nutSeat, entry) <- case lookup "pocket" opts of
    Nothing | nutKind == Hex -> Left "a framed hex-nut joint needs pocket=@frame at the nut bearing seat"
            | otherwise -> Right (False, Nothing, Nothing, Nothing)
    Just raw -> do
      when (nutKind /= Hex) $ Left "pocket= only applies to nut=hex"
      let (inC, ref) = case break (== '.') raw of
            ("clamped", '.':r) -> (True,r)
            ("host", '.':r) -> (False,r)
            _ -> (False,raw)
      pf <- instanceFrame (if inC then clamped else host) ref
      let p = frameOrigin pf
          pn = mApply (frameBasis pf) (0,0,1)
      unless (coaxial p && dot3 n pn < -1+1e-8) $ Left "nut seat and host hole frames must be coaxial with opposing outward normals"
      when (gripTo p < -1e-6) $ Left "nut seat must be behind the head seat along the screw axis"
      opening <- pocketEntry (if inC then clamped else host) ref
      Right (inC, Just (ref,pn), Just p, Just opening)
  -- A rail frame must still lie on its real slot centre and outside face.
  local <- frameOf (drop 1 (fdFace fd)) (iShape host)
  let localPos = frameOrigin local
      localNormal = mApply (frameBasis local) (0,0,1)
      localBox = bbox (iShape host)
      localAxis = longAxis localBox
  when (iRail host) $ do
    unless (any (\axis -> abs(dot3 axis localNormal) > 1-1e-8) [(1,0,0),(0,1,0),(0,0,1)]) $
      Left "a 2020 hole frame normal must point out of a rail face"
    when (abs (dot3 localAxis localNormal) < 0.5) $ do
      let transverse = vnormed (cross localNormal localAxis)
          (_, surface) = extentAlong localNormal localBox
      when (abs (dot3 (vsub localPos (bcenter localBox)) transverse) > 1e-5 || abs (dot3 localPos localNormal - surface) > 1e-5) $
        Left "host hole frame must lie on the 2020 slot centre and outer face"
  let thickness = gripTo (fromMaybe pos nutSeat)
      (hostMin, _) = extentAlong (iAxis host) (iBox host)
      (localMin, _) = extentAlong localAxis localBox
      along = if iRail host then (dot3 localPos localAxis - localMin) * vlen(mApply (fst(iAff host)) localAxis) else dot3 pos (iAxis host) - hostMin
  Right [Fast { fId = k, fKind = Bolt, fSpec = fdSpec fd, fDiam = d, fLen = l, fHead = hd
    , fClamped = iName clamped, fHost = iName host, fFaceName = fdFace fd
    , fInset = 0, fFace = n, fNut = nutKind, fPocket = pocket, fPocketInClamped = inClamped
    , fPos = pos, fHeadPos = headPos, fPocketSeat = nutSeat, fPocketEntry = entry, fSeatName = Just seatRef
    , fAlong = along, fTorque = torque
    , fDriverLen = setDriverLength sets, fDriverRad = headRadius hd d + setDriverMargin sets, fReach = l - thickness }]
  where opts = fdOpts fd

-- | A `joint` line: the axis is the host's outward frame normal at the
-- mate between the two parts (the moving part leaves along it), or the
-- named world face of the host with axis=.
jointFastener :: [Constraint] -> Int -> FastDecl -> Inst -> Inst -> Either String [Fast]
jointFastener constraints k fd moving host = do
  let kindWord = drop 6 (fdSpec fd)
      opts = fdOpts fd
  kind <- case kindWord of
    "push" -> Right Push
    "drop" -> Right Drop
    "screw" -> maybe (Left "a screw joint needs pitch=mm") (Right . Screw) (lookup "pitch" opts >>= readD)
    _ -> Left ("unknown joint kind '" ++ kindWord ++ "'")
  let names i = [iName i, iPart i]
      mates = [(c, pa, pf, cf) | Mate _ c cf pa pf _ _ <- constraints]
      between = [(pf, True) | (c, pa, pf, _) <- mates, c `elem` names moving, pa `elem` names host]
             ++ [(pf, False) | (c, pa, pf, _) <- mates, c `elem` names host, pa `elem` names moving]
  (pos, axis, label) <- case lookup "axis" opts of
    Just face -> do
      n <- maybe (Left ("unknown axis face '" ++ face ++ "'")) Right (anchorDir face)
      let (_, hi) = extentAlong n (iBox host)
          c = bcenter (iBox host)
      Right (vadd c (scale3 (hi - dot3 c n) n), n, face)
    Nothing -> case between of
      (pf, hostIsParent) : _ -> do
        fr <- instanceFrame (if hostIsParent then host else moving) ('@' : pf)
        let z = mApply (frameBasis fr) (0, 0, 1)
        Right (frameOrigin fr, if hostIsParent then z else vneg z, '@' : pf)
      [] -> Left ("no mate between " ++ iName moving ++ " and " ++ iName host ++ " to take the joint axis from; add axis=face")
  Right [Fast { fId = k, fKind = kind, fSpec = kindWord, fDiam = 0, fLen = 0, fHead = "peg"
    , fClamped = iName moving, fHost = iName host, fFaceName = label, fInset = 0, fFace = vnormed axis
    , fNut = NoNut, fPocket = Nothing, fPocketInClamped = False, fPos = pos, fHeadPos = pos
    , fPocketSeat = Nothing, fPocketEntry = Nothing, fSeatName = Nothing, fAlong = 0, fTorque = 0
    , fDriverLen = 0, fDriverRad = 0, fReach = 0 }]

-- ------------------------------------------------------------------
-- requires-before graph

type Preds = Map.Map Node [Node]

-- Explicit capture/access ordering that boxes cannot infer, e.g. an arm
-- must enter a journal before the next axle covers its open end.
partPreds :: AsmResult -> [Inst] -> Either String Preds
partPreds ar insts = do
  pairs <- mapM one insts
  let edges = [(iName i, ps) | (i, ps) <- pairs]
      cycles = [ns | CyclicSCC ns <- stronglyConnComp [(n, n, ps) | (n, ps) <- edges]]
  unless (null cycles) $ Left ("cyclic after= ordering: " ++ intercalate "; " (map (intercalate ", ") cycles))
  Right (Map.fromList [(NPart (iName i), map NPart ps) | (i, ps) <- pairs])
  where
    one i = do
      let (_, hints) = fromMaybe (1, []) (lookup (iPart i) (arPartMeta ar))
      ps <- case lookup "after" hints of
        Nothing -> Right []
        Just "" -> Left ("after= for " ++ iName i ++ " needs an instance name")
        Just s -> mapM (find (iName i)) (splitOn ',' s)
      Right (i, ps)
    find who name = case [iName i | i <- insts, iName i == name] of
      [n] -> Right n
      _ -> case [iName i | i <- insts, iPart i == name] of
        [n] -> Right n
        [] -> Left ("after= for " ++ who ++ ": unknown part '" ++ name ++ "'")
        ns -> Left ("after= for " ++ who ++ ": '" ++ name ++ "' has multiple instances; use " ++ intercalate ", " ns)

buildPreds :: [Inst] -> [Fast] -> Preds
buildPreds insts fasts = Map.fromListWith (++) (parts ++ nuts ++ screws ++ blockers ++ hostsFirst)
  where
    byName = Map.fromList [(iName i, i) | i <- insts]
    hosts = nutHosts fasts
    parts = [(NPart (iName i), []) | i <- insts]
    -- nuts in a host go in after the host is placed (its slot or pocket must
    -- be reachable); nuts in the clamped part go in with the part in hand,
    -- before it is placed
    inHand = nub ([fClamped f | f <- fasts, fNut f /= NoNut, fPocketInClamped f]
                 ++ [iName i | i <- insts, iPreloadInHand i, iName i `elem` hosts])
    nuts = [(NNuts h, if h `elem` inHand then [] else [NPart h]) | h <- hosts]
    screws = [(NPart h, [NNuts h]) | h <- inHand] ++ concat
      [ if fNut f == NoNut
          then [(NFast (fId f), [NPart (fClamped f), NPart (fHost f)])]
          else [(NFast (fId f), [NPart (fClamped f), NPart (fHost f), NNuts (nutHolder f)]), (NPart (fClamped f), [NNuts (nutHolder f)])]
      | f <- fasts ]
    -- a `joint` line moves its first part onto the second: the host is there first
    hostsFirst = [(NPart (fClamped f), [NPart (fHost f)]) | f <- fasts, fKind f `elem` [Push, Drop] || isScrewKind f]
    isScrewKind f = case fKind f of Screw _ -> True; _ -> False
    -- slide-in nuts must go in before anything seals a slot end of that host
    blockers =
      [ (NPart (iName p), [NNuts h])
      | f <- fasts, fNut f == SlideIn, let h = fHost f, Just host <- [Map.lookup h byName]
      , p <- insts, iName p /= h, any (bIntersects (iBox p)) (endCaps host) ]

-- | The part a fastener's nut is loaded into: the host, or the clamped
-- part when its pocket is there (`pocket=clamped.face`).
nutHolder :: Fast -> String
nutHolder f = if fPocketInClamped f then fClamped f else fHost f

-- | Parts that have nuts to preload (T-nuts or captive hex nuts).
nutHosts :: [Fast] -> [String]
nutHosts fasts = nub [nutHolder f | f <- fasts, fNut f /= NoNut]

isPegF :: Fast -> Bool
isPegF f = fLen f <= 0

-- | A joint made by moving one part onto another (no screw and no driver).
isJoint :: Fast -> Bool
isJoint = isPegF

-- | Does the joint hold its parts together once made? A drop-in socket
-- does not: gravity or a part that captures it must.
retains :: Fast -> Bool
retains f = fKind f /= Drop

-- | The instances joined to this one, with the joint.
jointPartners :: Model -> String -> [(String, Fast)]
jointPartners m n = Map.findWithDefault [] n (mPartners m)

-- | A part at least three times longer than it is wide (a bar, a strut):
-- stood on end it is as unsteady as a rail.
slender :: Double -> Inst -> Bool
slender ratio i =
  let ((x0, y0, z0), (x1, y1, z1)) = bbox (iShape i)
      ds = reverse (sort [x1 - x0, y1 - y0, z1 - z0])
   in case ds of
        (a : b : _) -> b > 0 && a / b >= ratio
        _ -> False

endCaps :: Inst -> [BBox]
endCaps host =
  let (lo, hi) = extentAlong (iAxis host) (iBox host)
      a = iAxis host
      ((x0, y0, z0), (x1, y1, z1)) = iBox host
      capAt c = let base = ((x0, y0, z0), (x1, y1, z1)) in clampAlong a c base
      clampAlong ax c ((p0, q0, r0), (p1, q1, r1)) = case ax of
        (1, 0, 0) -> ((c - 1, q0, r0), (c + 1, q1, r1))
        (0, 1, 0) -> ((p0, c - 1, r0), (p1, c + 1, r1))
        _ -> ((p0, q0, c - 1), (p1, q1, c + 1))
   in [capAt (lo - 1), capAt (hi + 1)]

data Model = Model
  { mInsts :: [Inst]
  , mByName :: Map.Map String Inst
  , mFasts :: [Fast]
  , mFastById :: Map.Map Int Fast
  , mPreds :: Preds
  , mNodes :: [Node]
  , mRestDirs :: [V3]
  , mWeights :: Weights
  , mSettings :: Settings
  , mGeo :: Geo
  , mMateAxes :: Map.Map String [V3] -- world normals of the frames each instance is mated by
  , mPartners :: Map.Map String [(String, Fast)] -- each instance's joints, with the other instance
  , mAxes :: [(V3, String)] -- the rest directions the tables are measured for, with their names
  }

-- design errors that are properties of the model, reported before planning
-- | Millimetres to one decimal, for messages and the plan text.
fmtMm :: Double -> String
fmtMm x = showD (fromIntegral (round (x * 10) :: Integer) / 10)

designErrors :: [Inst] -> [Fast] -> [String]
designErrors _ fasts =
  [ printf "%s through %s into %s: only %s mm of thread reaches the nut (screw length minus the clamped part's thickness); use a longer screw" (fSpec f) (fClamped f) (fHost f) (fmtMm (fReach f))
  | f <- fasts, not (isPegF f), fReach f < 3 ] ++
  [ printf "screws collide: %s into %s (%s @ %s mm) and %s into %s (%s @ %s mm) share the same spot" (fSpec a) (fHost a) (fFaceName a) (fmtMm (fAlong a)) (fSpec b) (fHost b) (fFaceName b) (fmtMm (fAlong b))
  | (a, b) <- pairs screws, fHost a == fHost b, fFace a == fFace b, vlen (vsub (fPos a) (fPos b)) < (fDiam a + fDiam b) / 2 ]
  ++ [ printf "screws collide inside %s: %s from the %s face and %s from the %s face cross" (fHost a) (fSpec a) (fFaceName a) (fSpec b) (fFaceName b)
     | (a, b) <- pairs screws, fHost a == fHost b, fFace a /= fFace b, vlen (vsub (fFace a) (fFace b)) > 0.1
     , let shank f = (vsub (fHeadPos f) (scale3 (fLen f) (fFace f)), fHeadPos f)
     , segmentDistance (shank a) (shank b) < (fDiam a + fDiam b)/2 - 1e-6 ]
  where
    screws = filter (not . isPegF) fasts
    pairs xs = [(a, b) | (i, a) <- zip [0 :: Int ..] xs, b <- drop (i + 1) xs]

-- ------------------------------------------------------------------
-- model construction shared by the CLI and the test suite

buildModel :: AsmResult -> Either String (Model, [String])
buildModel ar = do
  asmDef <- maybe (Left "plan needs an `asm = ...` definition (it gives every part its place)") Right (arAsm ar)
  let _ = asmDef
      table0 = Map.fromList [(n, Tag n s) | (n, s) <- arParts ar]
  table <- resolveAssembly ar table0
  asmS <- maybe (Left "no asm") Right (Map.lookup "asm" table)
  insts <- mkInstances ar asmS
  let sets = settingsFrom (arPlanOpts ar)
  fasts <- mkFasteners sets (arConstraints ar) (arSource ar) insts (arFasteners ar)
  ordered <- partPreds ar insts
  let byName = Map.fromList [(iName i, i) | i <- insts]
      errs = designErrors insts fasts
      -- the world faces, and the faces of the largest part when the
      -- assembly is not square to the world (it rests on its own faces)
      largest = case sortOn (\i -> negate (boxVolume (bbox (iShape i)))) insts of
        i : _ -> Just i
        [] -> Nothing
      boxVolume ((x0, y0, z0), (x1, y1, z1)) = (x1 - x0) * (y1 - y0) * (z1 - z0)
      worldAxes = [(a, dirName a) | a <- axes6]
      ownAxes = case largest of
        Just i -> [ (a, "the " ++ iName i ++ "'s " ++ face ++ " face")
                  | (e, face) <- [((0, 0, -1), "bottom"), ((0, 0, 1), "top"), ((-1, 0, 0), "left"), ((1, 0, 0), "right"), ((0, -1, 0), "front"), ((0, 1, 0), "back")]
                  , let v = mApply (fst (iAff i)) e, vlen v > 1e-9, let a = vnormed v
                  , all (\(w, _) -> vlen (vsub a w) > 1e-6) worldAxes ]
        Nothing -> []
      allAxes = worldAxes ++ ownAxes
      restDirs = case lookup "rest" (arPlanOpts ar) of
        Just s -> mapMaybe anchorDir (splitOn ',' s)
        Nothing -> map fst allAxes
      weights = Map.union (Map.fromList [(k, v) | (k, raw) <- arPlanOpts ar, Map.member k defaultWeights, Just v <- [readD raw]]) defaultWeights
      preds = Map.unionWith (++) (buildPreds insts fasts) ordered
      nodes = [NPart (iName i) | i <- insts] ++ [NNuts h | h <- nutHosts fasts] ++ [NFast (fId f) | f <- fasts]
      frameAxis i ref = either (const Nothing) (\fr -> Just (vnormed (mApply (frameBasis fr) (0, 0, 1)))) (instanceFrame i ('@' : ref))
      lookupInst n = case [i | i <- insts, iName i == n] ++ [i | i <- insts, iPart i == n] of
        i : _ -> Just i
        [] -> Nothing
      mateAxes = Map.fromListWith (++) (concat
        [ [(iName ci, [a]) | Just a <- [frameAxis ci cf]] ++ [(iName ci, [a]) | Just a <- [frameAxis pi' pf]]
          ++ [(iName pi', [a]) | Just a <- [frameAxis pi' pf]] ++ [(iName pi', [a]) | Just a <- [frameAxis ci cf]]
        | Mate _ c cf pa pf _ _ <- arConstraints ar, Just ci <- [lookupInst c], Just pi' <- [lookupInst pa] ])
      partners = Map.fromListWith (flip (++)) (concat [[(fClamped f, [(fHost f, f)]), (fHost f, [(fClamped f, f)])] | f <- fasts])
  Right (Model insts byName fasts (Map.fromList [(fId f, f) | f <- fasts]) preds nodes restDirs weights sets emptyGeo mateAxes partners allAxes, errs)
