-- | `coscad plan`, part 1: the model. Part instances and their placed
-- boxes, fasteners and nut batches, the requires-before relation, and
-- design errors in the hardware.
module Coscad.Plan.Model
  ( Fast (..)
  , Inst (..)
  , Material (..)
  , Model (..)
  , Move (..)
  , Node (..)
  , NutKind (..)
  , Weights (..)
  , affApply
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
  , iMass :: Double -- grams
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

data Fast = Fast
  { fId :: Int
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

data Move = MPlace Node [String] | MFlip V3 [String] deriving (Show)

data Weights = Weights {wFlip, wVertical, wDriver, wShrink, wSibling, wLoose :: Double}

eps :: Double
eps = 0.5

-- ------------------------------------------------------------------
-- small geometry

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

mkFasteners :: FilePath -> [Inst] -> [FastDecl] -> Either String [Fast]
mkFasteners srcFile insts decls = do
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
      mapM_ (either (Left . (at ++)) Right . validateFastenerHint) (fdOpts fd)
      when (fdCount fd < 1 || fdCount fd > maxQuantity) $ Left (at ++ "fastener count must be from 1 to " ++ show maxQuantity)
      (d, l) <- maybe (Left (at ++ "cannot read screw spec '" ++ fdSpec fd ++ "'; screw diameter and length must be positive (expected e.g. M5x8, or use peg for a peg joint)")) Right (parseSpec (fdSpec fd))
      clamped <- either (Left . (at ++)) Right (find (fdClamped fd))
      host <- either (Left . (at ++)) Right (find (fdHost fd))
      when (iName clamped == iName host) $ Left (at ++ "clamped part and host must be different instances")
      if take 1 (fdFace fd) == "@"
        then either (Left . (at ++)) Right (framedFastener k fd d l clamped host)
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
                  { fId = k * 100 + j, fSpec = fdSpec fd, fDiam = d, fLen = l, fHead = hd
                  , fClamped = iName clamped, fHost = iName host, fFaceName = fdFace fd, fInset = inset, fFace = n
                  , fNut = nutKind, fPocket = pocket, fPocketInClamped = pocketInClamped, fPos = pos, fAlong = p - hostMin, fTorque = if isPeg then 0 else torque
                  , fHeadPos = vadd pos (scale3 (cMax - facePlane) n), fPocketSeat = Nothing, fPocketEntry = Nothing, fSeatName = Nothing
                  , fDriverLen = if isPeg then 0 else 45, fDriverRad = if isPeg then 0 else headRadius hd d + 2
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

framedFastener :: Int -> FastDecl -> Double -> Double -> Inst -> Inst -> Either String [Fast]
framedFastener k fd d l clamped host = do
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
  Right [Fast { fId = k, fSpec = fdSpec fd, fDiam = d, fLen = l, fHead = hd
    , fClamped = iName clamped, fHost = iName host, fFaceName = fdFace fd
    , fInset = 0, fFace = n, fNut = nutKind, fPocket = pocket, fPocketInClamped = inClamped
    , fPos = pos, fHeadPos = headPos, fPocketSeat = nutSeat, fPocketEntry = entry, fSeatName = Just seatRef
    , fAlong = along, fTorque = torque
    , fDriverLen = 45, fDriverRad = headRadius hd d + 2, fReach = l - thickness }]
  where opts = fdOpts fd

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
buildPreds insts fasts = Map.fromListWith (++) (parts ++ nuts ++ screws ++ blockers)
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

-- | A part at least three times longer than it is wide (a bar, a strut):
-- stood on end it is as unsteady as a rail.
slender :: Inst -> Bool
slender i =
  let ((x0, y0, z0), (x1, y1, z1)) = bbox (iShape i)
      ds = reverse (sort [x1 - x0, y1 - y0, z1 - z0])
   in case ds of
        (a : b : _) -> b > 0 && a / b >= 3
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
  , mRest :: Maybe V3 -- the rest face of the state being evaluated (set by tryPlace)
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
  fasts <- mkFasteners (arSource ar) insts (arFasteners ar)
  ordered <- partPreds ar insts
  let byName = Map.fromList [(iName i, i) | i <- insts]
      errs = designErrors insts fasts
      restDirs = case lookup "rest" (arPlanOpts ar) of
        Just s -> mapMaybe anchorDir (splitOn ',' s)
        Nothing -> [(0, 0, -1), (0, 0, 1), (-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0)]
      wOpt k def = fromMaybe def (lookup k (arPlanOpts ar) >>= readD)
      weights = Weights (wOpt "flip" 10) (wOpt "vertical" 6) (wOpt "driver" 3) (wOpt "shrink" 2) (wOpt "sibling" 1) (wOpt "loose" 1)
      preds = Map.unionWith (++) (buildPreds insts fasts) ordered
      nodes = [NPart (iName i) | i <- insts] ++ [NNuts h | h <- nutHosts fasts] ++ [NFast (fId f) | f <- fasts]
  Right (Model insts byName fasts (Map.fromList [(fId f, f) | f <- fasts]) preds nodes restDirs weights Nothing, errs)
