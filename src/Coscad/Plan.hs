-- | `coscad plan foo.assemble`: turn an assembly into a numbered build
-- sequence a person can follow without backtracking.
--
-- Model (docs/PLAN.md): nodes are part instances, per-rail T-nut
-- batches, and fasteners; edges are derived requires-before relations
-- (host and clamped part before a screw, nuts before the screw and
-- before the part that covers the slot, slide-in nuts before anything
-- that seals a slot end). A state is the set of placed nodes plus the
-- face resting on the bench. Hard predicates prune (stable, supported,
-- driver accessible, nut can enter); a cost table picks among the
-- remaining linear extensions via beam search; consecutive moves on
-- one rest face merge into numbered steps.
module Coscad.Plan (processPlan, planSummary, PlanSummary (..), StepSummary (..)) where

import Control.Monad (forM_, unless, when)
import Coscad.Assemble
import Coscad.Codegen (gen, renderScad, showD, usesBosl2)
import Coscad.Geometry
import Coscad.IO (writeFileUtf8)
import Coscad.OpenScad (runOpenscadRaw)
import Coscad.Parser (resolveVariables)
import Coscad.Shape
import Data.Char (isDigit, toLower)
import Data.List (foldl', intercalate, nub, sort, sortBy, sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (dropExtension, takeFileName)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- ------------------------------------------------------------------
-- affine transforms and instance enumeration

type Aff = (M3, V3)

mMul :: M3 -> M3 -> M3
mMul a (r1, r2, r3) =
  let cols = transpose3 (r1, r2, r3)
      row r = let (c1, c2, c3) = cols in (dot r c1, dot r c2, dot r c3)
      (a1, a2, a3) = a
   in (row a1, row a2, row a3)
  where
    dot (x, y, z) (p, q, r) = x * p + y * q + z * r
    transpose3 ((a11, a12, a13), (a21, a22, a23), (a31, a32, a33)) = ((a11, a21, a31), (a12, a22, a32), (a13, a23, a33))

affApply :: Aff -> V3 -> V3
affApply (m, t) p = vadd (mApply m p) t

affCompose :: Aff -> Aff -> Aff
affCompose (m1, t1) (m2, t2) = (mMul m1 m2, vadd (mApply m1 t2) t1)

identity :: M3
identity = ((1, 0, 0), (0, 1, 0), (0, 0, 1))

diag :: V3 -> M3
diag (x, y, z) = ((x, 0, 0), (0, y, 0), (0, 0, z))

-- | Every Tag'd subtree with its accumulated world transform.
instancesOf :: Shape -> [(String, Aff, Shape)]
instancesOf = go (identity, (0, 0, 0))
  where
    go aff s = case s of
      Tag n x -> [(n, aff, x)]
      Tx d x -> go (affCompose aff (identity, (d, 0, 0))) x
      Ty d x -> go (affCompose aff (identity, (0, d, 0))) x
      Tz d x -> go (affCompose aff (identity, (0, 0, d))) x
      Translate v x -> go (affCompose aff (identity, v)) x
      Rx a x -> go (affCompose aff (rodMat a (1, 0, 0), (0, 0, 0))) x
      Ry a x -> go (affCompose aff (rodMat a (0, 1, 0), (0, 0, 0))) x
      Rz a x -> go (affCompose aff (rodMat a (0, 0, 1), (0, 0, 0))) x
      RotAxis a v x -> go (affCompose aff (rodMat a v, (0, 0, 0))) x
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
  , iMass :: Double -- grams
  }

-- | DropIn / SlideIn are T-nuts in a rail slot; Hex is a plain nut captive
-- in a pocket of the host (`nut=hex pocket=<face>`: the face the pocket opens on).
data NutKind = DropIn | SlideIn | Hex deriving (Eq, Show)

data Fast = Fast
  { fId :: Int
  , fSpec :: String -- "M5x8"
  , fDiam :: Double
  , fLen :: Double
  , fHead :: String -- button | socket
  , fClamped :: String
  , fHost :: String
  , fFaceName :: String
  , fFace :: V3 -- outward normal of the host face = driver axis (head outward)
  , fNut :: NutKind
  , fPocket :: Maybe (String, V3) -- hex nuts: the host face the pocket opens on
  , fPos :: V3 -- point on the host face
  , fAlong :: Double -- mm from the host's marked end (min end along its axis)
  , fTorque :: Double
  , fDriverLen :: Double
  , fDriverRad :: Double
  , fReach :: Double -- thread inside the host = screw length minus clamped thickness
  }

data Node = NPart String | NNuts String | NFast Int deriving (Eq, Ord, Show)

data Move = MPlace Node [String] | MFlip V3 deriving (Show)

data Weights = Weights {wFlip, wVertical, wDriver, wShrink, wSibling, wLoose :: Double}

defaultWeights :: Weights
defaultWeights = Weights 10 6 3 2 1 1

eps :: Double
eps = 0.5

-- ------------------------------------------------------------------
-- small geometry

vdot :: V3 -> V3 -> Double
vdot (a, b, c) (x, y, z) = a * x + b * y + c * z

vscale :: Double -> V3 -> V3
vscale k (a, b, c) = (k * a, k * b, k * c)

bIntersects :: BBox -> BBox -> Bool
bIntersects ((a, b, c), (d, e, f)) ((p, q, r), (s, t, u)) =
  a <= s + eps && p <= d + eps && b <= t + eps && q <= e + eps && c <= u + eps && r <= f + eps

bExpand :: Double -> BBox -> BBox
bExpand k ((a, b, c), (d, e, f)) = ((a - k, b - k, c - k), (d + k, e + k, f + k))

boxAround :: V3 -> V3 -> BBox
boxAround p q = fromCorners [p, q]

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
extentAlong n bb = let ds = map (vdot n) (bcorners bb) in (minimum ds, maximum ds)

-- ------------------------------------------------------------------
-- building the model from an AsmResult

materialOf :: Int -> [(String, String)] -> Material
materialOf cnt hints = case map toLower <$> lookup "material" hints of
  Just "aluminium" -> Aluminium
  Just "aluminum" -> Aluminium
  Just "printed" -> Printed
  Just "steel" -> Steel
  _ -> if cnt > 0 then Printed else Aluminium

readD :: String -> Maybe Double
readD s = case reads (if take 1 s == "." then '0' : s else s) of
  [(v, "")] -> Just v
  _ -> Nothing

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
        let k = Map.findWithDefault 0 n seen + 1
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
            Just e -> let ws = splitOn ',' e in (take 1 ws /= ["blocked"], take 1 (drop 1 ws) /= ["blocked"])
            Nothing -> (True, True)
          mat = materialOf cnt hints
          vol = (x1 - x0) * (y1 - y0) * (z1 - z0)
          density = case mat of Aluminium -> 0.0027; Printed -> 0.00125; Steel -> 0.0079
          fill = if isRail then 0.37 else if mat == Printed then 0.3 else 1
          mass = fromMaybe (vol * density * fill) (lookup "mass" hints >>= readD)
      Right Inst
        { iName = nm, iPart = part, iAff = aff, iShape = sh, iBox = bb, iMat = mat, iRail = isRail
        , iAxis = axis, iEnds = ends, iTorque = lookup "torque" hints >>= readD, iMass = mass
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
  ('m' : rest) -> case break (== 'x') rest of
    (d, 'x' : l) | all isDigitOrDot d, all isDigitOrDot l, not (null d), not (null l) -> (,) <$> readD d <*> readD l
    _ -> Nothing
  _ -> Nothing
  where
    isDigitOrDot c = isDigit c || c == '.'

mkFasteners :: [Inst] -> [FastDecl] -> Either String [Fast]
mkFasteners insts decls = concat <$> mapM one (zip [1 ..] decls)
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
      let at = "fastener line " ++ show (fdLine fd) ++ ": "
      (d, l) <- maybe (Left (at ++ "cannot read screw spec '" ++ fdSpec fd ++ "' (expected e.g. M5x8)")) Right (parseSpec (fdSpec fd))
      clamped <- either (Left . (at ++)) Right (find (fdClamped fd))
      host <- either (Left . (at ++)) Right (find (fdHost fd))
      n <- maybe (Left (at ++ "unknown face '" ++ fdFace fd ++ "' (top bot lft rt fwd bak)")) Right (anchorDir (fdFace fd))
      let hb = iBox host
          cb = iBox clamped
          facePlane = snd (extentAlong n hb) -- coordinate of the host face along n
          (cMin, cMax) = extentAlong n cb
      when (cMin > facePlane + 1.5 || cMax < facePlane - 1.5) $
        Left (at ++ iName clamped ++ " does not sit on the " ++ fdFace fd ++ " face of " ++ iName host)
      -- overlap rectangle in the two in-plane axes
      let axes = [a | a <- [(1, 0, 0), (0, 1, 0), (0, 0, 1)], abs (vdot a n) < 0.5]
          overlap a = let (h0, h1) = extentAlong a hb; (c0, c1) = extentAlong a cb in (max h0 c0, min h1 c1)
          ranges = map overlap axes
      when (any (\(lo, hi) -> hi < lo) ranges) $
        Left (at ++ iName clamped ++ " does not overlap " ++ iName host ++ " on its " ++ fdFace fd ++ " face")
      let along = if abs (vdot (iAxis host) n) < 0.5 then iAxis host else head axes
          other = head [a | a <- axes, a /= along]
          (l0, l1) = overlap along
          (o0, o1) = overlap other
          (hostMin, _) = extentAlong along hb
          nutKind = case map toLower <$> lookup "nut" (fdOpts fd) of
            Just "slidein" -> SlideIn
            Just "slide-in" -> SlideIn
            Just "hex" -> Hex
            Just "captive" -> Hex
            _ -> DropIn
          hd = fromMaybe "button" (lookup "head" (fdOpts fd))
          count = max 1 (fdCount fd)
          positions = case lookup "at" (fdOpts fd) of
            Just s | Just ps <- mapM readD (splitOn ',' s) -> map (+ hostMin) ps
            _ -> [l0 + (l1 - l0) * (fromIntegral i - 0.5) / fromIntegral count | i <- [1 .. count]]
          torque = fromMaybe (fromMaybe (defaultTorque (iMat host) d) (iTorque host)) (lookup "torque" (fdOpts fd) >>= readD)
      pocket <- case lookup "pocket" (fdOpts fd) of
        Nothing -> Right Nothing
        Just pf -> maybe (Left (at ++ "unknown pocket face '" ++ pf ++ "' (top bot lft rt fwd bak)")) (Right . Just . (,) pf) (anchorDir pf)
      when (nutKind /= Hex && pocket /= Nothing) $ Left (at ++ "pocket= only applies to nut=hex")
      let
          thickness = fromMaybe (cMax - max cMin facePlane) (lookup "through" (fdOpts fd) >>= readD)
          mkF (j, p) =
            let pos = vadd (vscale p along) (vadd (vscale ((o0 + o1) / 2) other) (vscale facePlane n))
             in Fast
                  { fId = k * 100 + j, fSpec = fdSpec fd, fDiam = d, fLen = l, fHead = hd
                  , fClamped = iName clamped, fHost = iName host, fFaceName = fdFace fd, fFace = n
                  , fNut = nutKind, fPocket = pocket, fPos = pos, fAlong = p - hostMin, fTorque = torque
                  , fDriverLen = 45, fDriverRad = headRadius hd d + 2, fReach = l - thickness
                  }
      Right (map mkF (zip [1 ..] positions))

-- ------------------------------------------------------------------
-- requires-before graph

type Preds = Map.Map Node [Node]

buildPreds :: [Inst] -> [Fast] -> Preds
buildPreds insts fasts = Map.fromListWith (++) (parts ++ nuts ++ screws ++ blockers)
  where
    byName = Map.fromList [(iName i, i) | i <- insts]
    hosts = nub (map fHost fasts)
    parts = [(NPart (iName i), []) | i <- insts]
    nuts = [(NNuts h, [NPart h]) | h <- hosts]
    screws = concat
      [ [(NFast (fId f), [NPart (fClamped f), NPart (fHost f), NNuts (fHost f)]), (NPart (fClamped f), [NNuts (fHost f)])]
      | f <- fasts ]
    -- slide-in nuts must go in before anything seals a slot end of that host
    blockers =
      [ (NPart (iName p), [NNuts h])
      | f <- fasts, fNut f == SlideIn, let h = fHost f, Just host <- [Map.lookup h byName]
      , p <- insts, iName p /= h, any (bIntersects (iBox p)) (endCaps host) ]

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

-- ------------------------------------------------------------------
-- states and predicates

data St = St {sPlaced :: Set.Set Node, sRest :: Maybe V3, sCost :: Double, sMoves :: [Move]}

placedInsts :: Map.Map String Inst -> St -> [Inst]
placedInsts byName st = [i | NPart n <- Set.toList (sPlaced st), Just i <- [Map.lookup n byName]]

heightMin :: V3 -> BBox -> Double
heightMin d bb = minimum [negate (vdot c d) | c <- bcorners bb]

benchLevel :: V3 -> [Inst] -> Double
benchLevel d is = minimum (map (heightMin d . iBox) is)

-- 2D projection basis perpendicular to d
basis :: V3 -> (V3, V3)
basis d = let u = if abs (vdot d (0, 0, 1)) > 0.9 then (1, 0, 0) else (0, 0, 1); u' = vnormed (vsub u (vscale (vdot u d) d)) in (u', cross d u')

project :: V3 -> V3 -> (Double, Double)
project d p = let (u, v) = basis d in (vdot p u, vdot p v)

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
polyArea ps = abs (sum [x1 * y2 - x2 * y1 | ((x1, y1), (x2, y2)) <- zip ps (tail ps ++ [head ps])]) / 2

insidePoly :: (Double, Double) -> [(Double, Double)] -> Bool
insidePoly (px, py) ps
  | length ps < 3 = case ps of
      [(x, y)] -> abs (x - px) < 2 && abs (y - py) < 2
      [(x1, y1), (x2, y2)] -> let t = ((px - x1) * (x2 - x1) + (py - y1) * (y2 - y1)) / max 1e-9 ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int)); tc = max 0 (min 1 t)
                                  in sqrt ((x1 + tc * (x2 - x1) - px) ^ (2 :: Int) + (y1 + tc * (y2 - y1) - py) ^ (2 :: Int)) < 2
      _ -> False
  | otherwise = all (\((x1, y1), (x2, y2)) -> (x2 - x1) * (py - y1) - (y2 - y1) * (px - x1) >= -eps * 4) (zip ps (tail ps ++ [head ps]))

supportPoly :: V3 -> [Inst] -> [(Double, Double)]
supportPoly d is =
  let lvl = benchLevel d is
      onBench = [i | i <- is, heightMin d (iBox i) < lvl + eps]
   in convexHull [project d c | i <- onBench, c <- bcorners (iBox i), negate (vdot c d) < lvl + eps]

stable :: V3 -> [Inst] -> Bool
stable _ [] = True
stable d is =
  let poly = supportPoly d is
      m = sum (map iMass is)
      com = foldl' vadd (0, 0, 0) [vscale (iMass i / m) (bcenter (iBox i)) | i <- is]
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
heightMax d bb = maximum [negate (vdot c d) | c <- bcorners bb]

driverBox :: Map.Map String Inst -> Fast -> BBox
driverBox byName f =
  let n = fFace f
      clampedTop = case Map.lookup (fClamped f) byName of
        Just c -> snd (extentAlong n (iBox c))
        Nothing -> vdot (fPos f) n
      start = vadd (fPos f) (vscale (clampedTop - vdot (fPos f) n) n)
      end = vadd start (vscale (fDriverLen f) n)
      r = fDriverRad f
      (u, v) = basis n
      corners = [vadd p (vadd (vscale a u) (vscale b v)) | p <- [start, end], a <- [-r, r], b <- [-r, r]]
   in fromCorners corners

accessible :: Map.Map String Inst -> [Inst] -> Fast -> Maybe String
accessible byName placed f =
  -- the driver must really enter another part's box, not merely touch it:
  -- a hemisphere shell whose box brushes the neighbouring shell's box
  -- (the ball example) does not put that shell in the driver's way.
  -- bIntersects itself allows eps of slack, so shrink by twice that.
  let db = bExpand (negate (2 * eps)) (driverBox byName f)
      hits = [iName i | i <- placed, iName i /= fClamped f, iName i /= fHost f, bIntersects (iBox i) db]
   in if null hits then Nothing else Just (head hits)

nutCanEnter :: Map.Map String Inst -> Maybe V3 -> [Inst] -> [Fast] -> String -> Maybe String
nutCanEnter byName rest placed fasts h =
  let host = byName Map.! h
      here = [f | f <- fasts, fHost f == h]
      others = [i | i <- placed, iName i /= h]
      -- a hex nut drops into its pocket from the pocket face: that face
      -- must not be on the bench, and nothing may cover it there
      hexBlocked f = case fPocket f of
        Nothing -> Nothing
        Just (pf, n)
          | Just d <- rest, vdot n d > 0.5 -> Just ("the " ++ pf ++ " face of " ++ h ++ " (its nut pocket) is on the bench")
          | otherwise ->
              let (u, v) = basis n
                  plane = snd (extentAlong n (iBox host))
                  base = vadd (fPos f) (vscale (plane - vdot (fPos f) n) n)
                  box = fromCorners [vadd base (vadd (vscale a u) (vadd (vscale b v) (vscale c n))) | a <- [-6, 6], b <- [-6, 6], c <- [0, 3]]
               in case [iName i | i <- others, bIntersects (iBox i) box] of
                    (b : _) -> Just ("the " ++ pf ++ " face of " ++ h ++ " (its nut pocket) is already covered by " ++ b)
                    [] -> Nothing
      dropInOk f =
        let n = fFace f
            (u, v) = basis n
            box = fromCorners [vadd (fPos f) (vadd (vscale a u) (vadd (vscale b v) (vscale c n))) | a <- [-6, 6], b <- [-6, 6], c <- [0, 3]]
         in [iName i | i <- others, bIntersects (iBox i) box]
      endOpen (open, cap) = open && not (any (bIntersects cap . iBox) others)
      slideOk = or (zipWith (curry endOpen) [fst (iEnds host), snd (iEnds host)] (endCaps host))
   in case [why | f <- here, fNut f == Hex, Just why <- [hexBlocked f]] of
        (why : _) -> Just why
        [] -> case [b | f <- here, fNut f == DropIn, b <- take 1 (dropInOk f)] of
          (b : _) -> Just ("the " ++ fFaceName (head here) ++ " slot of " ++ h ++ " is already covered by " ++ b)
          [] -> if any ((== SlideIn) . fNut) here && not slideOk then Just ("both ends of " ++ h ++ " are sealed; slide-in nuts cannot enter") else Nothing

-- ------------------------------------------------------------------
-- search

data Model = Model
  { mInsts :: [Inst]
  , mByName :: Map.Map String Inst
  , mFasts :: [Fast]
  , mFastById :: Map.Map Int Fast
  , mPreds :: Preds
  , mNodes :: [Node]
  , mRestDirs :: [V3]
  , mWeights :: Weights
  , mRest :: Maybe V3 -- the rest face of the state being evaluated (set by tryPlace)
  }

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
            held = any (\f -> fClamped f == n && Set.member (NPart (fHost f)) (sPlaced st)) (mFasts m)
        supported d placed p held
        let placed' = p : placed
        unless (stable d placed') (Left ("assembly would tip after placing " ++ n))
        let hostsOfP = nub [fHost f | f <- mFasts m, fClamped f == n]
            vertical = [h | h <- hostsOfP, Just hi <- [Map.lookup h (mByName m)], iRail hi, abs (vdot (iAxis hi) d) > 0.3]
            standing = iRail p && abs (vdot (iAxis p) d) > 0.3 -- a rail stood on end: its nuts slide, it wobbles
            shrink = polyArea (supportPoly d placed') < polyBefore - 1
        Right ( (if null vertical && not standing then 0 else wVertical w) + (if shrink then wShrink w else 0)
              , [h ++ " is not lying flat in this step; hold its T-nut with a finger while starting the screw" | h <- vertical]
                  ++ [n ++ " stands on end in this step; steady it until it is braced" | standing] )
      NNuts h -> case nutCanEnter (mByName m) (Just d) placed (mFasts m) h of
        Just why -> Left why
        Nothing -> Right (0, [])
      NFast i -> do
        let f = mFastById m Map.! i
        case accessible (mByName m) placed f of
          Just blocker -> Left ("driver for " ++ fSpec f ++ " into " ++ fHost f ++ " is blocked by " ++ blocker)
          Nothing -> Right ()
        let up = vneg d
            offAxis = vdot (fFace f) up < cos (30 * pi / 180)
            siblings = [g | g <- mFasts m, fClamped g == fClamped f, fId g /= i, not (Set.member (NFast (fId g)) (sPlaced st))]
        Right ( (if offAxis then wDriver w else 0) + (if null siblings then 0 else wSibling w)
              , ["driver axis is not vertical here (" ++ fFaceName f ++ " face of " ++ fHost f ++ "); keep the driver square to the bracket" | offAxis] )
    let placed' = Set.insert node (sPlaced st)
        loose = wLoose w * fromIntegral (length (looseParts m {mRest = Just d} placed'))
    Right st {sPlaced = placed', sCost = sCost st + cost + loose, sMoves = MPlace node warns : sMoves st}

-- | Placed parts nothing holds yet: brackets waiting for their screws, and
-- parts resting on other parts (not the bench) with no screw in them.
looseParts :: Model -> Set.Set Node -> [String]
looseParts m placed =
  nub ([fClamped f | f <- mFasts m, Set.member (NPart (fClamped f)) placed, not (Set.member (NFast (fId f)) placed)]
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
      remaining = [f | f <- mFasts m, not (Set.member (NFast (fId f)) (sPlaced st))]
      groups = Map.fromListWith (+) [(fFace f, 1 :: Int) | f <- remaining]
      verticalNow n = maybe False (\d -> vdot n (vneg d) >= cos (30 * pi / 180)) (sRest st)
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
                      , not (null hosts), any (\h -> iRail h && abs (vdot (iAxis h) d) > 0.3) hosts]
        Nothing -> 0
   in underneath + min vertHosts (2 * wFlip w) + sum [min (fromIntegral k * wDriver w) (wFlip w) | (n, k) <- Map.toList groups, not (verticalNow n)]

tryFlip :: Model -> St -> V3 -> Maybe St
tryFlip m st d
  | sRest st == Just d = Nothing
  | not (stable d (placedInsts (mByName m) st)) = Nothing
  | otherwise =
      -- the first rest face is free; a tiny tie-break prefers the listed order (bottom first)
      let idx = fromIntegral (length (takeWhile (/= d) (mRestDirs m))) * 0.001
       in Just st {sRest = Just d, sCost = sCost st + (if sRest st == Nothing then idx else wFlip (mWeights m) + idx), sMoves = MFlip d : sMoves st}

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
    lastWasFlip s = case sMoves s of (MFlip _ : _) -> True; _ -> False
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
    stuck round' beam =
      let s = head (sortOn (negate . Set.size . sPlaced) beam)
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
      MFlip d -> (Just (Step d (maybe False (const True) cur) [] [] [] []), maybe acc (: acc) cur)
      MPlace node warns -> case cur of
        Nothing -> (Nothing, acc) -- cannot happen: a flip always comes first
        Just s -> case node of
          NPart p
            | null (stFasts s) && null (stNuts s) && length (stParts s) < 4 -> (Just s {stParts = stParts s ++ [p], stWarn = stWarn s ++ warns}, acc)
            | otherwise -> (Just (Step (stRest s) False [p] [] [] warns), s : acc)
          NNuts h
            | null (stFasts s) -> (Just s {stNuts = stNuts s ++ [h]}, acc)
            | otherwise -> (Just (Step (stRest s) False [] [h] [] []), s : acc)
          NFast i -> (Just s {stFasts = stFasts s ++ [i], stWarn = stWarn s ++ warns}, acc)

-- ------------------------------------------------------------------
-- outputs

fmtMm :: Double -> String
fmtMm x = showD (fromIntegral (round (x * 10) :: Integer) / 10)

planMarkdown :: String -> Model -> [Step] -> [String] -> String
planMarkdown title m steps designErrors = unlines $
  [ "# Build plan: " ++ title, ""
  , printf "%d parts, %d fasteners, %d steps, %d flips." (length (mInsts m)) (length (mFasts m)) (length steps) (length (filter stFlip steps))
  , "" ]
  ++ (if null designErrors then [] else "## Design errors" : "" : map ("- " ++) designErrors ++ [""])
  ++ ["## Before you start", "", "### Bill of materials", ""] ++ bom ++ [""]
  ++ ["### Preload sheet (nuts)", ""] ++ preload ++ [""]
  ++ ["## Steps", ""] ++ concat (zipWith stepMd [1 :: Int ..] steps)
  where
    byId = mFastById m
    bom =
      [ printf "- %d × %s %s head screw" c s h | ((s, h), c) <- Map.toList (Map.fromListWith (+) [((fSpec f, fHead f), 1 :: Int) | f <- mFasts m]) ]
      ++ [ printf "- %d × M%s %s" c (showD d) (nutName k) | ((d, k), c) <- Map.toList (Map.fromListWith (+) [((fDiam f, show (fNut f)), 1 :: Int) | f <- mFasts m]) ]
      ++ [ printf "- %d × %s (%s)" c p (matName (iMat i)) | (p, (c, i)) <- Map.toList (Map.fromListWith (\(a, x) (b, _) -> (a + b, x)) [(iPart i, (1 :: Int, i)) | i <- mInsts m]) ]
    matName mt = case mt of Aluminium -> "aluminium"; Printed -> "printed"; Steel -> "steel"
    preload =
      let hosts = Map.fromListWith (++) [(fHost f, [f]) | f <- mFasts m]
       in if Map.null hosts then ["(no nuts)"] else
            [ if all ((== Hex) . fNut) fs
                then printf "- **%s**: %d hex nut%s — %s." h (length fs) (if length fs == 1 then "" else "s")
                       (intercalate "; " [pocketWords f | f <- fs])
                else printf "- **%s**: %d nut%s — %s. Mark the %s end; positions are mm from the mark." h (length fs) (if length fs == 1 then "" else "s")
                       (intercalate "; " [printf "%s slot: %s" face (intercalate ", " (map (fmtMm . fAlong) gs)) | (face, gs) <- Map.toList (Map.fromListWith (flip (++)) [(fFaceName f, [f]) | f <- fs])])
                       (markedEnd h)
            | (h, fs) <- Map.toList hosts ]
    pocketWords f = "M" ++ showD (fDiam f) ++ " into the pocket " ++ maybe "" (\(pf, _) -> "on the " ++ pf ++ " face ") (fPocket f) ++ "for the " ++ fFaceName f ++ " screw (" ++ fClamped f ++ ")"
    markedEnd h = case Map.lookup h (mByName m) of
      Just i -> case iAxis i of (1, 0, 0) -> "-X (left)"; (0, 1, 0) -> "-Y (front)"; _ -> "-Z (bottom)"
      Nothing -> "lower"
    stepMd :: Int -> Step -> [String]
    stepMd k s =
      [ printf "### Step %d — %s down%s" k (dirName (stRest s)) (if stFlip s then " (flip the assembly)" else "" :: String) ]
      ++ [ "- Place: " ++ p ++ hostsNote p | p <- stParts s ]
      ++ [ "- Preload: " ++ h ++ " — " ++ nutsOn h | h <- stNuts s ]
      ++ [ if fNut f == Hex
             then printf "- Tighten: %s %s head through %s into the hex nut in %s (%s face) — %s" (fSpec f) (fHead f) (fClamped f) (fHost f) (fFaceName f) (torqueWords (hostMat f) (fTorque f))
             else printf "- Tighten: %s %s head into %s T-nut on %s %s slot at %s mm — %s" (fSpec f) (fHead f) (nutWord f) (fHost f) (fFaceName f) (fmtMm (fAlong f)) (torqueWords (hostMat f) (fTorque f))
         | i <- stFasts s, let f = byId Map.! i ]
      ++ [ "- Note: " ++ w | w <- nub (stWarn s) ]
      ++ [""]
    hostsNote p = let hs = nub [fHost f | f <- mFasts m, fClamped f == p] in if null hs then "" else " (on " ++ intercalate ", " hs ++ ")"
    nutsOn :: String -> String
    nutsOn h = let fs = [f | f <- mFasts m, fHost f == h] in
      if all ((== Hex) . fNut) fs
        then printf "%d × M%s hex nut%s into the pocket%s %s" (length fs) (showD (fDiam (head fs))) (if length fs == 1 then "" else "s" :: String) (if length fs == 1 then "" else "s" :: String)
               (intercalate ", " [maybe ("for the " ++ fFaceName f ++ " screw") (\(pf, _) -> "on the " ++ pf ++ " face") (fPocket f) | f <- fs])
        else printf "%d × M%s %s nut%s (%s)" (length fs) (showD (fDiam (head fs))) (nutWord (head fs)) (if length fs == 1 then "" else "s" :: String) (intercalate ", " [fFaceName f ++ " @ " ++ fmtMm (fAlong f) | f <- fs])
    nutWord f = case fNut f of DropIn -> "drop-in"; SlideIn -> "slide-in"; Hex -> "hex"
    nutName k = case k of "DropIn" -> "drop-in T-nut"; "SlideIn" -> "slide-in T-nut"; _ -> "hex nut"
    hostMat f = maybe Aluminium iMat (Map.lookup (fHost f) (mByName m))

planJson :: String -> Model -> [Step] -> [String] -> String
planJson src m steps errs =
  "{\n  \"source\": " ++ jstr src ++ ",\n  \"design_errors\": [" ++ intercalate ", " (map jstr errs) ++ "],\n  \"steps\": [\n"
    ++ intercalate ",\n" (zipWith stepJ [1 :: Int ..] steps) ++ "\n  ]\n}\n"
  where
    stepJ k s =
      "    {\"step\": " ++ show k ++ ", \"rest\": " ++ jstr (dirName (stRest s)) ++ ", \"flip\": " ++ (if stFlip s then "true" else "false")
        ++ ", \"parts\": [" ++ intercalate ", " (map jstr (stParts s)) ++ "]"
        ++ ", \"preload\": [" ++ intercalate ", " (map jstr (stNuts s)) ++ "]"
        ++ ", \"fasteners\": [" ++ intercalate ", " [fj (mFastById m Map.! i) | i <- stFasts s] ++ "]"
        ++ ", \"warnings\": [" ++ intercalate ", " (map jstr (nub (stWarn s))) ++ "]}"
    fj f = "{\"spec\": " ++ jstr (fSpec f) ++ ", \"head\": " ++ jstr (fHead f) ++ ", \"nut\": " ++ jstr (show (fNut f)) ++ maybe "" (\(pf, _) -> ", \"pocket\": " ++ jstr pf) (fPocket f) ++ ", \"clamped\": " ++ jstr (fClamped f)
      ++ ", \"host\": " ++ jstr (fHost f) ++ ", \"face\": " ++ jstr (fFaceName f) ++ ", \"at_mm\": " ++ jnum (fAlong f) ++ ", \"torque_nm\": " ++ jnum (fTorque f) ++ "}"

-- per-step OpenSCAD scene: everything placed so far, this step highlighted,
-- the bench under the rest face, scene rotated so the rest face is down.
stepScad :: Model -> [Step] -> Int -> String
stepScad m steps k =
  (if any (usesBosl2 . iShape) (mInsts m) then "include <BOSL2/std.scad>\n\n" else "")
    ++ "$vpr = [55, 0, 35];\n$vpt = [" ++ showV c ++ "];\n$vpd = " ++ showD (2.6 * vlen (vsub hi lo) + 50) ++ ";\n\n"
    ++ "multmatrix(" ++ mat4 (rot, (0, 0, 0)) ++ ") {\n"
    ++ concat [ "  color(\"" ++ col ++ "\") " ++ instScad i | (i, col) <- zip prev (repeat "LightGray") ++ zip now (repeat "Orange") ]
    ++ concat [ "  color(\"" ++ col ++ "\") " ++ fastScad f | (f, col) <- zip prevF (repeat "DarkRed") ++ zip nowF (repeat "Red") ]
    ++ "}\n"
    ++ "// the bench, under the rest face (scene coordinates: rest face is -Z)\n"
    ++ "%translate([" ++ showV (bx0 - 20, by0 - 20, bench - 2) ++ "]) cube([" ++ showV (bx1 - bx0 + 40, by1 - by0 + 40, 2) ++ "]);\n"
    ++ "$fn = 24;\n"
  where
    s = steps !! (k - 1)
    d = stRest s
    rot = downMat d
    before = concatMap stParts (take (k - 1) steps)
    now = mapMaybe (`Map.lookup` mByName m) (stParts s)
    prev = mapMaybe (`Map.lookup` mByName m) before
    nowF = [mFastById m Map.! i | i <- stFasts s]
    prevF = [mFastById m Map.! i | i <- concatMap stFasts (take (k - 1) steps)]
    world = fromCorners (concatMap (map (mApply rot) . bcorners . iBox) (prev ++ now))
    ((bx0, by0, bench), (bx1, by1, _)) = world
    (lo, hi) = world
    c = bcenter world
    showV (x, y, z) = showD x ++ ", " ++ showD y ++ ", " ++ showD z
    instScad i = "multmatrix(" ++ mat4 (iAff i) ++ ") { " ++ oneLine (gen (resolve (iShape i))) ++ " }\n"
    fastScad f =
      let n = fFace f
          (ang, ax) = rotFromUp n
          top = maybe (vdot (fPos f) n) (snd . extentAlong n . iBox) (Map.lookup (fClamped f) (mByName m))
          start = vadd (fPos f) (vscale (top - vdot (fPos f) n) n)
       in "translate([" ++ showV start ++ "]) rotate(a = " ++ showD ang ++ ", v = [" ++ showV ax ++ "]) cylinder(h = " ++ showD (fDiam f * 0.7) ++ ", r = " ++ showD (headRadius (fHead f) (fDiam f)) ++ ");\n"
    oneLine = unwords . words
    mat4 ((r1, r2, r3), (tx, ty, tz)) = "[" ++ row r1 tx ++ ", " ++ row r2 ty ++ ", " ++ row r3 tz ++ ", [0, 0, 0, 1]]"
    row (a, b, cc) t = "[" ++ showV (a, b, cc) ++ ", " ++ showD t ++ "]"

-- | Rotation taking direction v to straight down (same as Next.downMat).
downMat :: V3 -> M3
downMat v =
  let dd = vnormed v
   in if vlen (vsub dd (0, 0, -1)) < 1e-9 then identity
      else if vlen (vsub dd (0, 0, 1)) < 1e-9 then rodMat 180 (1, 0, 0)
      else let (_, _, dz) = dd in rodMat (acos (max (-1) (min 1 (-dz))) * 180 / pi) (cross dd (0, 0, -1))

-- design errors that are properties of the model, reported before planning
designErrors :: [Inst] -> [Fast] -> [String]
designErrors _ fasts =
  [ printf "%s through %s into %s: only %s mm of thread reaches the nut (screw length minus the clamped part's thickness); use a longer screw" (fSpec f) (fClamped f) (fHost f) (fmtMm (fReach f))
  | f <- fasts, fReach f < 3 ] ++
  [ printf "screws collide: %s into %s (%s @ %s mm) and %s into %s (%s @ %s mm) share the same spot" (fSpec a) (fHost a) (fFaceName a) (fmtMm (fAlong a)) (fSpec b) (fHost b) (fFaceName b) (fmtMm (fAlong b))
  | (a, b) <- pairs fasts, fHost a == fHost b, fFace a == fFace b, vlen (vsub (fPos a) (fPos b)) < (fDiam a + fDiam b) / 2 ]
  ++ [ printf "screws collide inside %s: %s from the %s face and %s from the %s face cross" (fHost a) (fSpec a) (fFaceName a) (fSpec b) (fFaceName b)
     | (a, b) <- pairs fasts, fHost a == fHost b, fFace a /= fFace b, vlen (vsub (fFace a) (fFace b)) > 0.1
     , let shank f = let n = fFace f in boxAround (vadd (fPos f) (vscale (-(fReach f)) n)) (fPos f)
     , bIntersects (bExpand (fDiam a / 2) (shank a)) (shank b) ]
  where
    pairs xs = [(a, b) | (i, a) <- zip [0 :: Int ..] xs, b <- drop (i + 1) xs]

-- ------------------------------------------------------------------
-- model construction shared by the CLI and the test suite

buildModel :: AsmResult -> Either String (Model, [String])
buildModel ar = do
  asmDef <- maybe (Left "plan needs an `asm = ...` definition (it gives every part its place)") Right (arAsm ar)
  let _ = asmDef
      table0 = Map.fromList [(n, Tag n s) | (n, s) <- arParts ar]
  table <- resolveVariables (arMode ar) (arDefs ar) table0
  asmS <- maybe (Left "no asm") Right (Map.lookup "asm" table)
  insts <- mkInstances ar asmS
  fasts <- mkFasteners insts (arFasteners ar)
  let byName = Map.fromList [(iName i, i) | i <- insts]
      errs = designErrors insts fasts
      restDirs = case lookup "rest" (arPlanOpts ar) of
        Just s -> mapMaybe anchorDir (splitOn ',' s)
        Nothing -> [(0, 0, -1), (0, 0, 1), (-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0)]
      wOpt k def = fromMaybe def (lookup k (arPlanOpts ar) >>= readD)
      weights = Weights (wOpt "flip" 10) (wOpt "vertical" 6) (wOpt "driver" 3) (wOpt "shrink" 2) (wOpt "sibling" 1) (wOpt "loose" 1)
      preds = buildPreds insts fasts
      nodes = [NPart (iName i) | i <- insts] ++ [NNuts h | h <- nub (map fHost fasts)] ++ [NFast (fId f) | f <- fasts]
  Right (Model insts byName fasts (Map.fromList [(fId f, f) | f <- fasts]) preds nodes restDirs weights Nothing, errs)

runSearch :: AsmResult -> Model -> Either [String] St
runSearch ar model =
  let width = maybe 30 round (lookup "beam" (arPlanOpts ar) >>= readD)
   in case lookup "astar" (arPlanOpts ar) >>= readD of
        Just cap | Just s <- astar model (round cap) -> Right s
        _ -> beamSearch model width

data StepSummary = StepSummary
  { ssRest :: String
  , ssFlip :: Bool
  , ssParts :: [String]
  , ssPreload :: [String]
  , ssFasteners :: [(String, String, String)] -- (spec, clamped, host)
  }
  deriving (Show)

data PlanSummary = PlanSummary
  { psParts :: [String]
  , psFastenerCount :: Int
  , psDesignErrors :: [String]
  , psSteps :: [StepSummary]
  , psCost :: Double
  }
  deriving (Show)

-- | Plan an assembly file without writing anything (for tests and tools).
planSummary :: FilePath -> IO (Either String PlanSummary)
planSummary path = do
  res <- loadAssembleFile [] path
  return $ do
    ar <- res
    (model, errs) <- buildModel ar
    st <- either (Left . unlines) Right (runSearch ar model)
    let steps = toSteps (reverse (sMoves st))
        summ s = StepSummary (dirName (stRest s)) (stFlip s) (stParts s) (stNuts s)
          [(fSpec f, fClamped f, fHost f) | i <- stFasts s, let f = mFastById model Map.! i]
    Right (PlanSummary (map iName (mInsts model)) (length (mFasts model)) errs (map summ steps) (sCost st))

-- ------------------------------------------------------------------
processPlan :: Bool -> FilePath -> IO ()
processPlan png path = do
  res <- loadAssembleFile [] path
  ar <- either (\e -> hPutStrLn stderr ("Error: " ++ e) >> exitFailure) return res
  (model, errs) <- either (\e -> hPutStrLn stderr ("Error: " ++ e) >> exitFailure) return (buildModel ar)
  let insts = mInsts model
      fasts = mFasts model
      base = dropExtension path
      title = takeFileName base
  printf "plan: %d part instances, %d fasteners on %d hosts\n" (length insts) (length fasts) (length (nub (map fHost fasts)))
  unless (null errs) $ mapM_ (putStrLn . ("DESIGN ERROR  " ++)) errs
  case runSearch ar model of
    Left why -> do
      mapM_ (hPutStrLn stderr . ("Error: " ++)) why
      writeFileUtf8 (base ++ "_plan.md") (planMarkdown title model [] (errs ++ why))
      exitFailure
    Right st -> do
      let steps = toSteps (reverse (sMoves st))
      writeFileUtf8 (base ++ "_plan.md") (planMarkdown title model steps errs)
      writeFileUtf8 (base ++ "_plan.json") (planJson path model steps errs)
      printf "Wrote %s_plan.md (%d steps, %d flips, cost %s)\n" base (length steps) (length (filter stFlip steps)) (showD (sCost st))
      forM_ (zip [1 :: Int ..] steps) $ \(k, _) -> do
        let f = base ++ "_step" ++ show k ++ ".scad"
        writeFileUtf8 f (stepScad model steps k)
        when png $ do
          r <- runOpenscadRaw ["-o", base ++ "_step" ++ show k ++ ".png", "--imgsize=1200,900", "--colorscheme=Tomorrow", "--projection=p", f]
          case r of
            Right (ExitSuccess, _, _) -> return ()
            Right (_, _, e) -> hPutStrLn stderr ("warning: png for step " ++ show k ++ ": " ++ take 200 e)
            Left e -> hPutStrLn stderr ("warning: " ++ e)
      putStrLn ("Wrote " ++ base ++ "_step1.." ++ show (length steps) ++ ".scad" ++ (if png then " and .png" else ""))
      unless (null errs) exitFailure
