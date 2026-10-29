-- | `coscad plan`, part 3: output. The plan as Markdown and JSON, and an
-- OpenSCAD scene per step.
module Coscad.Plan.Render
  ( planJson
  , planMarkdown
  , stepScad
  , stepScene
  )
where

import Coscad.Plan.Model
import Coscad.Plan.Search

import Coscad.Codegen (gen, showD, usesBosl2)
import Coscad.Geometry
import Coscad.Image (Scene (..), SceneObject (..), defaultView)
import Coscad.Shape (Shape (..))
import Data.List (intercalate, nub)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Coscad.Json
import Text.Printf (printf)


-- ------------------------------------------------------------------
-- outputs

planMarkdown :: String -> Model -> [Step] -> [String] -> String
planMarkdown title m steps errs = unlines $
  [ "# Build plan: " ++ title, ""
  , printf "%d parts, %d fasteners, %d steps, %d flips." (length (mInsts m)) (length (mFasts m)) (length steps) (length (filter stFlip steps))
  , "" ]
  ++ (if null errs then [] else "## Design errors" : "" : map ("- " ++) errs ++ [""])
  ++ ["## Before you start", "", "### Bill of materials", ""] ++ bom ++ [""]
  ++ ["### Preload sheet (nuts)", ""] ++ preload ++ [""]
  ++ effort
  ++ ["## Steps", ""] ++ concat (zipWith stepMd [1 :: Int ..] steps)
  where
    totals = Map.unionsWith (+) (map stTerms steps)
    w = mWeights m
    effort = if Map.null totals then [] else
      [ "### Effort", ""
      , "What the plan costs, term by term (amount × weight; see `plan` weights):", "" ]
      ++ [ printf "- %s: %s × %s = %s" k (showD (round3 v)) (showD (weight w k)) (showD (round3 (v * weight w k))) | (k, v) <- Map.toList totals ]
      ++ [ printf "- total: %s" (showD (round3 (price w totals))), "" ]
    round3 x = fromIntegral (round (x * 1000) :: Integer) / 1000 :: Double
    byId = mFastById m
    bom =
      [ printf "- %d × %s %s head screw" c s h | ((s, h), c) <- Map.toList (Map.fromListWith (+) [((fSpec f, fHead f), 1 :: Int) | f <- mFasts m, not (isPegF f)]) ]
      ++ [ printf "- %d × M%s %s" c (showD d) (nutLabel k) | ((d, k), c) <- Map.toList (Map.fromListWith (+) [((fDiam f, show (fNut f)), 1 :: Int) | f <- mFasts m, fNut f /= NoNut]) ]
      ++ [ printf "- %d × %s (%s)" c p (matName (iMat i)) | (p, (c, i)) <- Map.toList (Map.fromListWith (\(a, x) (b, _) -> (a + b, x)) [(iPart i, (1 :: Int, i)) | i <- mInsts m]) ]
    matName mt = case mt of Aluminium -> "aluminium"; Printed -> "printed"; Steel -> "steel"
    preload =
      let hosts = Map.fromListWith (++) [(nutHolder f, [f]) | f <- mFasts m, fNut f /= NoNut]
       in if Map.null hosts then ["(no nuts to preload)"] else
            [ if all ((== Hex) . fNut) fs
                then printf "- **%s**: %d hex nut%s — %s." h (length fs) (if length fs == 1 then "" else "s")
                       (intercalate "; " [pocketWords f | f <- fs])
                else printf "- **%s**: %d nut%s — %s. Mark the %s end; positions are mm from the mark." h (length fs) (if length fs == 1 then "" else "s")
                       (intercalate "; " [printf "%s slot: %s" face (intercalate ", " (map (fmtMm . fAlong) gs)) | (face, gs) <- Map.toList (Map.fromListWith (flip (++)) [(fFaceName f, [f]) | f <- fs])])
                       (markedEnd h)
            | (h, fs) <- Map.toList hosts ]
    pocketLocation pf = if take 1 pf == "@" then "at " ++ pf else "on the " ++ pf ++ " face"
    interfaceName f = if take 1 (fFaceName f) == "@" then "hole " ++ fFaceName f else fFaceName f ++ " face"
    pocketWords f = "M" ++ showD (fDiam f) ++ " into the pocket " ++ maybe "" (\(pf, _) -> pocketLocation pf ++ " ") (fPocket f)
      ++ (if fPocketInClamped f then "for the screw into " ++ fHost f else "for the " ++ fFaceName f ++ " screw (" ++ fClamped f ++ ")")
    markedEnd h = case Map.lookup h (mByName m) of
      Just i -> case iAxis i of (1, 0, 0) -> "-X (left)"; (0, 1, 0) -> "-Y (front)"; _ -> "-Z (bottom)"
      Nothing -> "lower"
    stepMd :: Int -> Step -> [String]
    stepMd k s =
      [ printf "### Step %d — %s down%s" k (dirName (stRest s)) (if stFlip s then " (flip the assembly)" else "" :: String) ]
      ++ [ "- Place: " ++ p ++ hostsNote p ++ maybe "" (\a -> ", " ++ entryWords (stRest s) a) (lookup p (stEntry s)) | p <- stParts s ]
      ++ [ "- Preload: " ++ h ++ " — " ++ nutsOn h | h <- stNuts s ]
      ++ [ if fKind f == Drop
             then printf "- Drop: %s into %s (%s); it rests in the socket until something captures it" (fClamped f) (fHost f) (fFaceName f)
           else if isScrewF f
             then printf "- Screw: %s into %s (%s), about %s turns%s" (fClamped f) (fHost f) (fFaceName f) (showD (turnsOf f)) (pitchWords f)
           else if isPegF f
             then printf "- Push: %s into %s (%s entry%s) until it seats" (fClamped f) (fHost f) (fFaceName f)
                    (if fInset f == 0 then "" else ", seat " ++ fmtMm (fInset f) ++ " mm below the face")
           else if fNut f == NoNut
             then printf "- Tighten: %s %s head through %s into %s (%s, threads into the part) — %s" (fSpec f) (fHead f) (fClamped f) (fHost f) (interfaceName f) (torqueWords (hostMat f) (fTorque f))
           else if fNut f == Hex
             then printf "- Tighten: %s %s head through %s into the hex nut in %s (%s) — %s" (fSpec f) (fHead f) (fClamped f) (fHost f) (interfaceName f) (torqueWords (hostMat f) (fTorque f))
             else printf "- Tighten: %s %s head into %s T-nut on %s %s slot at %s mm — %s" (fSpec f) (fHead f) (nutWord f) (fHost f) (fFaceName f) (fmtMm (fAlong f)) (torqueWords (hostMat f) (fTorque f))
         | i <- stFasts s, let f = byId Map.! i ]
      ++ [ "- Note: " ++ w | w <- nub (stWarn s) ]
      ++ [""]
    hostsNote p = let hs = nub [fHost f | f <- mFasts m, fClamped f == p] in if null hs then "" else " (on " ++ intercalate ", " hs ++ ")"
    turnsOf f = fromIntegral (round (10 * maybe 0 twTurns (Map.lookup (fId f, fClamped f) (gTwist (mGeo m)))) :: Integer) / 10 :: Double
    pitchWords f = case fKind f of Screw p -> " (" ++ showD p ++ " mm pitch); turn it by hand, it must spin freely"; _ -> ""
    nutsOn :: String -> String
    nutsOn h = case [f | f <- mFasts m, nutHolder f == h, fNut f /= NoNut] of
      [] -> "no nuts"
      fs@(f0 : _) -> if all ((== Hex) . fNut) fs
        then printf "%d × M%s hex nut%s into the pocket%s %s" (length fs) (showD (fDiam f0)) (if length fs == 1 then "" else "s" :: String) (if length fs == 1 then "" else "s" :: String)
               (intercalate ", " [maybe ("for the " ++ fFaceName f ++ " screw") (\(pf, _) -> pocketLocation pf) (fPocket f) | f <- fs])
        else printf "%d × M%s %s nut%s (%s)" (length fs) (showD (fDiam f0)) (nutWord f0) (if length fs == 1 then "" else "s" :: String) (intercalate ", " [fFaceName f ++ " @ " ++ fmtMm (fAlong f) | f <- fs])
    nutWord f = case fNut f of DropIn -> "drop-in"; SlideIn -> "slide-in"; Hex -> "hex"; NoNut -> "no"
    nutLabel k = case k of "DropIn" -> "drop-in T-nut"; "SlideIn" -> "slide-in T-nut"; _ -> "hex nut"
    hostMat f = maybe Aluminium iMat (Map.lookup (fHost f) (mByName m))

-- | How a part goes in, in words: the opposite of the way it would leave.
entryWords :: V3 -> V3 -> String
entryWords rest out
  | dot3 out up > 0.95 = "lowering it in from above"
  | abs (dot3 out up) < 0.05, Just side <- axisSide out = "sliding it in from the " ++ side ++ " side"
  | otherwise = "bringing it in diagonally from the " ++ commas (parts' out)
  where
    up = vneg rest
    axisSide v = case [nm | (a, nm) <- named, dot3 v a > 0.999] of nm : _ -> Just nm; [] -> Nothing
    named = [((1, 0, 0), "right (+X)"), ((-1, 0, 0), "left (-X)"), ((0, 1, 0), "back (+Y)"), ((0, -1, 0), "front (-Y)"), ((0, 0, 1), "top (+Z)"), ((0, 0, -1), "bottom (-Z)")]
    parts' v = [ nm | (a, nm) <- named, dot3 v a > 0.3 ]
    commas xs = case reverse xs of
      [] -> "side"
      [x] -> x
      (x : rest) -> intercalate ", " (reverse rest) ++ " and " ++ x

planJson :: String -> Model -> [Step] -> [String] -> String
planJson src m steps errs =
  encodePretty $
    jobj
      [ ("source", JString src)
      , ("parts", jlist (map partJ (mInsts m)))
      , ("design_errors", jlist (map JString errs))
      , ("weights", jobj [(k, jnum3 v) | (k, v) <- Map.toList (mWeights m)])
      , ("cost", jnum3 (price (mWeights m) (Map.unionsWith (+) (map stTerms steps))))
      , ("steps", jlist (zipWith stepJ [1 :: Int ..] steps))
      ]
  where
    v3 (x, y, z) = jlist (map jnum3 [x, y, z])
    partJ i = jobj [("name", JString (iName i)), ("part", JString (iPart i)), ("material", JString (case iMat i of Aluminium -> "aluminium"; Printed -> "printed"; Steel -> "steel")), ("mass_g", jnum3 (iMass i)), ("centre_of_mass_mm", v3 (iCom i))]
    stepJ k s =
      jobj
        [ ("step", JNumber (fromIntegral k))
        , ("rest", JString (dirName (stRest s)))
        , ("flip", JBool (stFlip s))
        , ("parts", jlist (map JString (stParts s)))
        , ("preload", jlist (map JString (stNuts s)))
        , ("fasteners", jlist [fj (mFastById m Map.! i) | i <- stFasts s])
        , ("warnings", jlist (map JString (nub (stWarn s))))
        , ("entries", jlist [jobj [("part", JString p), ("leaves_along", v3 a)] | (p, a) <- stEntry s])
        , ("terms", jobj [(k, jnum3 v) | (k, v) <- Map.toList (stTerms s)])
        , ("cost", jnum3 (price (mWeights m) (stTerms s)))
        ]
    fj f =
      jobj $
        [ ("spec", JString (fSpec f))
        , ("head", JString (fHead f))
        , ("nut", JString (nutName (fNut f)))
        ]
          ++ maybe [] (\(pf, _) -> [("pocket", JString ((if fPocketInClamped f then "clamped." else "") ++ pf))]) (fPocket f)
          ++ [ ("clamped", JString (fClamped f))
             , ("host", JString (fHost f))
             , ("face", JString (fFaceName f))
             , ("position_mm", v3 (fPos f))
             , ("axis", v3 (fFace f))
             , ("head_position_mm", v3 (fHeadPos f))
             , ("thread_reach_mm", jnum3 (fReach f))
             ]
          ++ maybe [] (\ref -> [("seat", JString ref)]) (fSeatName f)
          ++ maybe [] (\p -> [("nut_seat_mm", v3 p)]) (fPocketSeat f)
          ++ [("inset_mm", jnum3 (fInset f)), ("at_mm", jnum3 (fAlong f)), ("torque_nm", jnum3 (fTorque f))]
          ++ [("joint", JString (jointName (fKind f)))]
          ++ [("turns", jnum3 t) | Just tw <- [Map.lookup (fId f, fClamped f) (gTwist (mGeo m))], let t = twTurns tw]
    jointName k = case k of Bolt -> "bolt"; Peg -> "peg"; Push -> "push"; Drop -> "drop"; Screw _ -> "screw"

isScrewF :: Fast -> Bool
isScrewF f = case fKind f of Screw _ -> True; _ -> False

-- per-step OpenSCAD scene: everything placed so far, this step highlighted,
-- the bench under the rest face, scene rotated so the rest face is down.
stepScad :: Model -> [Step] -> Int -> String
stepScad m steps k =
  (if any (usesBosl2 . iShape) (mInsts m) then "include <BOSL2/std.scad>\n\n" else "")
    -- Fit the bounding sphere within the narrower vertical field of view
    -- of the 1200 x 900 renders, including tall final assemblies.
    ++ "$vpr = [55, 0, 35];\n$vpt = [" ++ showV c ++ "];\n$vpd = " ++ showD (3.5 * vlen (vsub hi lo) + 50) ++ ";\n\n"
    ++ "multmatrix(" ++ mat4 (rot, (0, 0, 0)) ++ ") {\n"
    ++ concat [ "  color(\"" ++ col ++ "\") " ++ instScad i | (i, col) <- zip prev (repeat "LightGray") ++ zip now (repeat "Orange") ]
    ++ concat [ "  color(\"" ++ col ++ "\") " ++ fastScad f | (f, col) <- zip prevF (repeat "DarkRed") ++ zip nowF (repeat "Red"), not (isPegF f) ]
    ++ "}\n"
    -- The bench is a solid slab (a `%` background object would vanish from
    -- a full render), stopped 0.5 mm short of the rest face so the preview
    -- cannot z-fight it against the parts' bottom faces.
    ++ "// the bench, under the rest face (scene coordinates: rest face is -Z)\n"
    ++ "color(\"Gainsboro\") translate([" ++ showV (bx0 - 20, by0 - 20, bench - 2.5) ++ "]) cube([" ++ showV (bx1 - bx0 + 40, by1 - by0 + 40, 2) ++ "]);\n"
    ++ "$fn = 24;\n"
  where
    (rot, world, prev, now, prevF, nowF) = stepContents m steps k
    ((bx0, by0, bench), (bx1, by1, _)) = world
    (lo, hi) = world
    c = bcenter world
    showV (x, y, z) = showD x ++ ", " ++ showD y ++ ", " ++ showD z
    instScad i = "multmatrix(" ++ mat4 (iAff i) ++ ") { " ++ oneLine (gen (resolve (iShape i))) ++ " }\n"
    fastScad f =
      let n = fFace f
          (ang, ax) = rotFromUp n
          start = fHeadPos f
       in "translate([" ++ showV start ++ "]) rotate(a = " ++ showD ang ++ ", v = [" ++ showV ax ++ "]) cylinder(h = " ++ showD (fDiam f * 0.7) ++ ", r = " ++ showD (headRadius (fHead f) (fDiam f)) ++ ");\n"
    oneLine = unwords . words
    mat4 ((r1, r2, r3), (tx, ty, tz)) = "[" ++ row r1 tx ++ ", " ++ row r2 ty ++ ", " ++ row r3 tz ++ ", [0, 0, 0, 1]]"
    row (a, b, cc) t = "[" ++ showV (a, b, cc) ++ ", " ++ showD t ++ "]"

-- | Both image paths share the placed parts, step colours, rest orientation,
-- fastener head proxies and separated bench. Hardware proxies are explanatory
-- markers, exactly as in the original SCAD scenes, not threaded screw meshes.
stepScene :: Model -> [Step] -> Int -> Scene
stepScene m steps k = Scene (parts ++ fasteners ++ [table]) defaultView
  where
    (rot, ((x0,y0,z0),(x1,y1,_)), prev, now, prevF, nowF) = stepContents m steps k
    parts = [SceneObject (iName i) (iShape i) (rot `matMul` r, mApply rot p) color
            | (i,color) <- [(i,(211,211,211)) | i <- prev] ++ [(i,(255,165,0)) | i <- now]
            , let (r,p) = iAff i]
    fasteners = [SceneObject ("fastener " ++ fSpec f)
                  (Cylinder (headRadius (fHead f) (fDiam f)) (fDiam f*0.7))
                  (rot `matMul` rodMat angle axis, mApply rot (fHeadPos f)) color
                | (f,color) <- [(f,(139,0,0)) | f <- prevF] ++ [(f,(255,0,0)) | f <- nowF]
                , not (isPegF f), let (angle,axis) = rotFromUp (fFace f)]
    table = SceneObject "bench" (Rectangle (x1-x0+40) (y1-y0+40) 2)
              (matIdentity,(x0-20,y0-20,z0-2.5)) (220,220,220)

stepContents :: Model -> [Step] -> Int -> (M3, BBox, [Inst], [Inst], [Fast], [Fast])
stepContents m steps k = (rot, world, prev, now, prevF, nowF)
  where
    s = steps !! (k-1)
    rot = downMat (stRest s)
    before = concatMap stParts (take (k-1) steps)
    now = mapMaybe (`Map.lookup` mByName m) (stParts s ++ [h | h <- stNuts s, h `notElem` (before ++ stParts s)])
    prev = mapMaybe (`Map.lookup` mByName m) before
    nowF = [mFastById m Map.! i | i <- stFasts s]
    prevF = [mFastById m Map.! i | i <- concatMap stFasts (take (k-1) steps)]
    world = if null (prev ++ now) then ((0,0,0),(1,1,1))
            else fromCorners (concatMap (map (mApply rot) . bcorners . iBox) (prev ++ now))
