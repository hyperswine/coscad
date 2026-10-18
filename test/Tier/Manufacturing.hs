-- | Manufacturing tiers: printability, brims, packing, bed height and
-- variant names.
module Tier.Manufacturing
  ( printabilityTier
  , brimTier
  , packTier
  , lyingRod
  , printabilityProbes
  , bedHeightProbes
  , boxTris
  , adhesionRetryContact
  , variantNameTier
  , writeVariantCollisions
  , manifestVariants
  , distinctVariants
  )
where

import Control.Monad (forM, forM_)
import Coscad.Assemble (AsmResult (..), defaultMaxZ, loadAssembleFile)
import Coscad.Variants (FlatPart (..), Placement (..), brimHint, lookupVariant, packBeds, packBedsBrim, placedVariants, printInstances, printVariants, turnZ, variantMap, variantNames)
import Coscad.Geometry (mApply, rodMat)
import Coscad.Linalg (downMat)
import Coscad.Json (asArray, asNumber, asString, decode, lookupKey)
import Coscad.Mesh (Tri, meshBounds, meshVolume)
import Coscad.Next (FffTerms (..), fffTerms, instanceBrim, processNext)
import Coscad.Printability
import Coscad.Report (silent)
import Coscad.Shape (Shape (..))
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Maybe (fromMaybe)
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath
import System.Process (readProcessWithExitCode)
import Harness

printabilityTier :: T -> IO ()
printabilityTier t = do
  let patch x y h w = [((x,y,h),(x+w,y+w,h),(x+w,y,h)),
                        ((x,y,h),(x,y+w,h),(x+w,y+w,h))]
      base = patch 0 0 0 10
      flat = analysePrintability base
      tiny = analysePrintability (patch 0 0 0 2)
      raised = analysePrintability (base ++ patch 20 0 0.8 2 ++ patch 30 0 0.8 2)
      translated = analysePrintability (patch 50 80 100 10)
      upward = map (\(a,b,c) -> (a,c,b)) (patch 20 0 0.8 2)
  assertT t "printability: true contact area, not bounding footprint" (abs (bedContactMm2 flat-100) < 1e-9) (show flat)
  assertT t "printability: small contact warns" (any ("flat bed contact" `isInfixOf`) (printabilityWarnings tiny)) (show tiny)
  assertT t "printability: elevated starts are reported once per height" (elevatedStartsMm raised == [0.8]) (show raised)
  assertT t "printability: translation preserves contact and relative heights"
    (bedContactMm2 translated == 100 && null (elevatedStartsMm translated)) (show translated)
  assertT t "printability: an upward-facing floor is not an unsupported start"
    (null (elevatedStartsMm (analysePrintability (base ++ upward)))) ""
  assertT t "printability: empty input has no invented starts"
    (bedContactMm2 (analysePrintability []) == 0 && null (elevatedStartsMm (analysePrintability []))) ""
  -- one contact definition, tolerant of quantised and rotated meshes
  let cube = boxTris 20 20 20
      turn deg axis = map (\(a,b,c) -> (mApply (rodMat deg axis) a, mApply (rodMat deg axis) b, mApply (rodMat deg axis) c))
      orient v = turn' (downMat v)
      turn' m = map (\(a,b,c) -> (mApply m a, mApply m b, mApply m c))
      contactOf = bedContactMm2 . analysePrintability
      near a b = abs (a - b) < 1e-6 * max 1 (abs b)
      tilted = turn 37 (1, 2, 3) cube
      -- the face that was the cube's bottom, now pointing along R·(0,0,-1)
      diagDown = orient (mApply (rodMat 37 (1, 2, 3)) (0, 0, -1)) tilted
      -- ASCII-STL-like quantisation and float noise up to 4 µm on every vertex
      jitter = map (\(a,b,c) -> (wob a, wob b, wob c))
      wob (x, y, z) = (x, y, z + 0.004 * sin (13 * x + 7 * y))
      pad = boxTris 20 20 10 ++ map (\(a,b,c) -> (lift a, lift b, lift c)) (boxTris 4 4 1)
      lift (x, y, z) = (x + 30, y, z + 0.2)
  assertT t "printability: closed cube has its whole bottom, 400 mm², on the bed" (near (contactOf cube) 400) (show (contactOf cube))
  assertT t "printability: turning the cube about z keeps 400 mm²" (near (contactOf (turn 30 (0, 0, 1) cube)) 400) (show (contactOf (turn 30 (0, 0, 1) cube)))
  assertT t "printability: the cube on its corner (▽lft+fwd+bot) has no flat contact"
    (contactOf (orient (-1, -1, -1) cube) < 1e-9) (show (contactOf (orient (-1, -1, -1) cube)))
  assertT t "printability: a face put down along a skew diagonal is found at 400 mm²" (abs (contactOf diagDown - 400) < 1e-3) (show (contactOf diagDown))
  assertT t "printability: up to 4 µm of vertex noise on the bed face keeps the contact"
    (abs (contactOf (jitter cube) - 400) < 1e-3 && abs (contactOf (jitter diagDown) - 400) < 1e-3) (show (contactOf (jitter cube), contactOf (jitter diagDown)))
  assertT t "printability: a face 0.2 mm above the bed is not contact" (near (contactOf pad) 400) (show (contactOf pad))
  -- slenderness over the narrowest footprint direction, scaled with height
  let wall = turn 45 (0, 0, 1) (boxTris 40 2 30)
      warns kind = any (kind `isInfixOf`) . printabilityWarnings . analysePrintability
      rodMesh = lyingRod 5 40 64
  assertT t "printability: a 2×40 mm wall turned 45° is 2 mm wide (rotating calipers), not its 29.7 mm box"
    (abs (minFootprintWidth wall - 2) < 1e-6) (show (minFootprintWidth wall))
  assertT t "printability: the turned 2 mm wall is flagged as tall and narrow" (warns "tall, narrow" wall) (show (analysePrintability wall))
  assertT t "printability: a 6×6×20 pillar is still tall and narrow" (warns "tall, narrow" (boxTris 6 6 20)) ""
  assertT t "printability: the 7×7×18.8 fit-test bar is not (limit 2.5 + 10/h)" (not (warns "tall, narrow" (boxTris 7 7 18.8))) (show (analysePrintability (boxTris 7 7 18.8)))
  assertT t "printability: a short 2×2×6 peg does not wobble enough to warn" (not (warns "tall, narrow" (boxTris 2 2 6))) ""
  assertT t "printability: a lying rod has no flat contact but a first-layer strip, and does not warn"
    (bedContactMm2 (analysePrintability rodMesh) < 1e-9 && firstLayerMm2 (analysePrintability rodMesh) > 25 && null (printabilityWarnings (analysePrintability rodMesh)))
    (show (analysePrintability rodMesh))
  assertT t "printability: a tall part needs more first layer (0.05·h²): 10×10×60 on 100 mm² warns"
    (warns "flat bed contact" (boxTris 10 10 60) && not (warns "flat bed contact" (boxTris 10 10 20))) ""
  assertT t "printability: a closed box has no overhang" (overhangMm2 (analysePrintability cube) == 0) ""
  forM_ [("cube", cube), ("corner", orient (-1, -1, -1) cube), ("diagonal", diagDown), ("noisy", jitter diagDown), ("raised pad", pad)] $ \(name, m) ->
    assertT t ("orientation score and printability use the same contact: " ++ name)
      (fffContact (fffTerms m) == contactOf m) (show (fffContact (fffTerms m), contactOf m))

-- | Brim reservation: the packer grows each footprint by its brim, the
-- plate and part options parse, and which instances get a brim.
brimTier :: T -> IO ()
brimTier t = do
  section "brim reservation"
  let k = ("b", "top")
      pos r = [[(plInstance p, plAt p, plBrim p) | p <- bed] | bed <- r]
      items b = [("a", k, (10, 10), b), ("b", k, (10, 10), b)]
  assertT t "packBedsBrim with no brim places exactly as packBeds"
    (packBedsBrim (100, 100, 6) (items 0) == packBeds (100, 100, 6) [(i, kk, wd) | (i, kk, wd, _) <- items (0 :: Double)])
    (show (pos <$> packBedsBrim (100, 100, 6) (items 0)))
  case packBedsBrim (100, 100, 6) (items 4) of
    Right [[Placement _ _ _ (ax, ay) 4 0, Placement _ _ _ (bx, by) 4 0]] ->
      assertT t "a 4 mm brim keeps the 6 mm margin outside it: part at 10 mm, 14 mm (6 + 2·4) between parts"
        (ax == 10 && ay == 10 && by == 10 && bx - (ax + 10) == 14) (show ((ax, ay), (bx, by)))
    r -> failT t "a 4 mm brim keeps the 6 mm margin outside it" (show (pos <$> r))
  case packBedsBrim (100, 100, 6) [("a", k, (10, 10), 0), ("b", k, (10, 10), 3)] of
    Right [[Placement i1 _ _ (x1, _) _ _, Placement i2 _ _ (x2, _) _ _]] ->
      assertT t "the brim counts in the area order: the 16×16 reservation packs first, then 6 + 3 mm to the bare part"
        (i1 == "b" && x1 == 9 && i2 == "a" && x2 == 9 + 10 + 3 + 6) (show (i1, x1, i2, x2))
    r -> failT t "the brim counts in the area order" (show (pos <$> r))
  let many = [("p" ++ show i, k, (fromIntegral (7 + i `mod` 5), fromIntegral (5 + i `mod` 3)), fromIntegral (i `mod` 3)) | i <- [1 .. 30 :: Int]]
  assertT t "brim packing is deterministic" (fmap pos (packBedsBrim (120, 120, 6) many) == fmap pos (packBedsBrim (120, 120, 6) many)) ""
  case packBedsBrim (100, 100, 6) [("long", k, (86, 10), 2)] of
    Left e -> assertT t "a footprint that fits only without its brim is refused, naming the brim" ("with a 2.0 mm brim exceeds" `isInfixOf` e) e
    Right _ -> failT t "a footprint that fits only without its brim is refused" "packed"
  assertT t "a negative brim is refused" (either ("brim must be" `isInfixOf`) (const False) (packBedsBrim (100, 100, 6) [("a", k, (1, 1), -1)])) ""
  -- parsing: plate brim=N|auto and the per-part brim=N hint
  dir <- tempDir "brim-parse"
  writeFile (dir </> "p.coscad") "main = box 6 6 20\n"
  let load name src = do
        writeFile (dir </> name) src
        loadAssembleFile [] (dir </> name)
  r1 <- load "a.assemble" "plate 200 200 6 brim=8\np ← p.coscad ×2\nq ← p.coscad ×1 brim=0\nr ← p.coscad ×1 brim=3.5\n"
  case r1 of
    Right ar -> do
      assertT t "plate … brim=8 parses" (arPlateBrim ar == Just 8 && arPlate ar == (200, 200, 6)) (show (arPlateBrim ar))
      let hint n = [brimHint fp | fp <- arFlat ar, fpName fp == n]
          tall = analysePrintability (boxTris 6 6 20)
          cube = analysePrintability (boxTris 20 20 20)
          fpOf n = [fp | fp <- arFlat ar, fpName fp == n]
          brims = [(n, instanceBrim (arPlateBrim ar) fp pr) | n <- ["p", "q", "r"], fp <- fpOf n, pr <- [tall, cube]]
      assertT t "brim=N part hints parse" (hint "p" == [Nothing] && hint "q" == [Just 0] && hint "r" == [Just 3.5]) (show (map hint ["p", "q", "r"]))
      assertT t "plate brim applies only where printability recommends one; a part's brim=N wins, brim=0 opts out"
        (brims == [("p", 8), ("p", 0), ("q", 0), ("q", 0), ("r", 3.5), ("r", 3.5)]) (show brims)
    Left e -> failT t "plate … brim=8 parses" e
  r2 <- load "b.assemble" "plate 200 200 brim=auto\np ← p.coscad ×1\n"
  assertT t "plate W D brim=auto keeps the default margin and reserves Bambu's 5 mm"
    (either (const False) (\ar -> arPlateBrim ar == Just 5 && arPlate ar == (200, 200, 6)) r2) (either id (show . arPlateBrim) r2)
  r3 <- load "c.assemble" "plate 200 200 6\np ← p.coscad ×1\n"
  assertT t "a plate without brim= reserves none" (either (const False) ((== Nothing) . arPlateBrim) r3) (either id (show . arPlateBrim) r3)
  forM_ [ ("plate 200 200 6 brim=-1\np ← p.coscad ×1\n", "plate brim must be a finite distance in mm, non-negative")
        , ("plate 200 200 6 brim=wide\np ← p.coscad ×1\n", "plate brim in mm")
        , ("p ← p.coscad ×1 brim=x\n", "brim=x: expected a finite brim width in mm")
        , ("p ← p.coscad ×1 brim=-2\n", "brim=-2: expected a finite brim width in mm, non-negative") ] $ \(src, want) -> do
    r <- load "bad.assemble" src
    assertT t ("brim errors: " ++ want) (either (want `isInfixOf`) (const False) r) (either id (const "loaded") r)

-- | Bed height and packing quality: the 4th `plate` argument, and a
-- packer that fills gaps with later items and turns parts 90° about Z
-- before it opens another plate.
packTier :: T -> IO ()
packTier t = do
  section "bed height and packing"
  let k = ("b", "top")
      ids r = [[plInstance p | p <- bed] | bed <- r]
  -- tall item first: A takes a 60-deep shelf, B a new one; C is too
  -- deep for B's shelf, so the old next-fit packer opened plate 2 for it.
  -- It fits the gap after A.
  case packBeds (100, 100, 5) [("a", k, (75, 60)), ("b", k, (70, 25)), ("c", k, (10, 30))] of
    Right [[pa, pb, pc]] ->
      assertT t "packer: a later item fills the gap an earlier shelf left (1 plate, was 2)"
        (map plInstance [pa, pb, pc] == ["a", "b", "c"] && plAt pa == (5, 5) && plAt pb == (5, 70) && plAt pc == (85, 5) && all ((== 0) . plRotZ) [pa, pb, pc])
        (show [pa, pb, pc])
    r -> failT t "packer: a later item fills the gap an earlier shelf left" (show (ids <$> r))
  -- an item that fits nowhere waits for the next plate, and the rest
  -- still fill this one
  case packBeds (100, 100, 5) [("a", k, (60, 60)), ("b", k, (50, 40)), ("c", k, (20, 50))] of
    Right r -> assertT t "packer: an item that fits nowhere moves to the next plate without closing this one" (ids r == [["a", "c"], ["b"]]) (show (ids r))
    Left e -> failT t "packer: an item that fits nowhere moves to the next plate" e
  -- rotation: B (25×80) fits neither beside A nor below it as declared,
  -- but turned (80×25) it fits a new shelf below A
  case packBeds (100, 100, 5) [("a", k, (90, 50)), ("b", k, (25, 80))] of
    Right [[pa, pb]] ->
      assertT t "packer: a 90° turn avoids a new plate; the placement records rot_z and the turned size"
        (plRotZ pa == 0 && plRotZ pb == 90 && plSize pb == (80, 25) && plAt pb == (5, 60)) (show [pa, pb])
    r -> failT t "packer: a 90° turn avoids a new plate" (show (ids <$> r))
  case packBeds (80, 120, 5) [("long", k, (100, 60))] of
    Right [[p]] -> assertT t "packer: a part wider than the plate but not deeper is turned instead of refused" (plRotZ p == 90 && plSize p == (60, 100) && plAt p == (5, 5)) (show p)
    r -> failT t "packer: a part that only fits turned" (show (ids <$> r))
  case packBeds (100, 100, 5) [("sq", k, (95, 95))] of
    Left e -> assertT t "packer: a part that fits neither way is refused, saying it was tried turned" ("even turned 90 degrees" `isInfixOf` e && "sq footprint" `isInfixOf` e) e
    Right r -> failT t "packer: a part that fits neither way is refused" (show (ids r))
  -- layouts the old packer could finish without gaps are unchanged:
  -- seven 43×43 brackets on 120×120 still need 4 + 3
  case packBeds (120, 120, 6) [("br_" ++ show i, k, (43, 42.98)) | i <- [1 .. 7 :: Int]] of
    Right r -> assertT t "packer: square-ish parts never turn, and a full plate still spills" (map length r == [4, 3] && all ((== 0) . plRotZ) (concat r)) (show (map length r))
    Left e -> failT t "packer: square-ish parts" e
  let many = [("p" ++ show i, k, (fromIntegral (7 + i `mod` 11) * 3, fromIntegral (5 + i `mod` 7) * 4)) | i <- [1 .. 60 :: Int]]
      packed = packBeds (150, 150, 4) many
      inside p = let (x, y) = plAt p; (w, d) = plSize p in x >= 4 - 1e-9 && y >= 4 - 1e-9 && x + w <= 146 + 1e-9 && y + d <= 146 + 1e-9
      apart a b = let ((ax, ay), (aw, ad)) = (plAt a, plSize a); ((bx, by), (bw, bd)) = (plAt b, plSize b)
                   in ax + aw + 4 <= bx + 1e-9 || bx + bw + 4 <= ax + 1e-9 || ay + ad + 4 <= by + 1e-9 || by + bd + 4 <= ay + 1e-9
  assertT t "packer: deterministic, every instance placed once, inside the margin, with the margin between parts"
    (packed == packBeds (150, 150, 4) many
      && either (const False) (\r -> sort (concat (ids r)) == sort [i | (i, _, _) <- many]
        && all (all inside) r && and [apart a b | bed <- r, (j, a) <- zip [0 :: Int ..] bed, (l, b) <- zip [0 ..] bed, j < l]) packed)
    (either id (show . map length) packed)
  assertT t "turnZ maps a w×d footprint at the origin onto d×w at the origin"
    (map (turnZ 20) [(0, 0, 1), (30, 0, 1), (0, 20, 1), (30, 20, 1)] == [(20, 0, 1), (20, 30, 1), (0, 0, 1), (0, 30, 1)]) ""
  -- the 4th plate argument
  dir <- tempDir "plate-height"
  writeFile (dir </> "p.coscad") "main = box 6 6 20\n"
  let load name src = do
        writeFile (dir </> name) src
        loadAssembleFile [] (dir </> name)
      maxZOf = either (const Nothing) (Just . arPlateMaxZ)
  ra <- load "a.assemble" "plate 180 180 6 180\np ← p.coscad ×1\n"
  assertT t "plate W D margin maxZ parses the max print height (A1 mini: 180)"
    (maxZOf ra == Just 180 && either (const False) ((== (180, 180, 6)) . arPlate) ra) (either id (show . arPlateMaxZ) ra)
  rb <- load "b.assemble" "plate 256 256 6 brim=8\np ← p.coscad ×1\n"
  rc <- load "c.assemble" "plate 256 256\np ← p.coscad ×1\n"
  rd <- load "d.assemble" "p ← p.coscad ×1\n"
  assertT t "without a 4th argument the max height stays 250 mm (also with brim=, no margin, or no plate line)"
    (defaultMaxZ == 250 && all ((== Just 250) . maxZOf) [rb, rc, rd]) (show (map maxZOf [rb, rc, rd]))
  re <- load "e.assemble" "plate 256 256 6 220 brim=auto\np ← p.coscad ×1\n"
  assertT t "plate W D margin maxZ brim=auto: both options parse"
    (maxZOf re == Just 220 && either (const False) ((== Just 5) . arPlateBrim) re) (either id (show . arPlateMaxZ) re)
  forM_ [ ("plate 256 256 6 0\np ← p.coscad ×1\n", "plate max height must be a finite distance in mm, greater than zero")
        , ("plate 256 256 6 -10\np ← p.coscad ×1\n", "plate max height must be a finite distance in mm, greater than zero")
        , ("plate 256 256 6 180 7\np ← p.coscad ×1\n", "bad.assemble:1:") ] $ \(src, want) -> do
    r <- load "bad.assemble" src
    assertT t ("plate max height errors: " ++ want) (either (want `isInfixOf`) (const False) r) (either id (const "loaded") r)

-- | A closed n-gon prism of radius r along x, lying on the bed with a
-- vertex at the bottom.
lyingRod :: Double -> Double -> Int -> [Tri]
lyingRod r len n =
  let ring x = [(x, r * sin a, r - r * cos a) | k <- [0 .. n - 1], let a = 2 * pi * fromIntegral k / fromIntegral n]
      l0 = ring 0
      l1 = ring len
      nxt k = (k + 1) `mod` n
      side = concat [[(l0 !! k, l1 !! nxt k, l1 !! k), (l0 !! k, l0 !! nxt k, l1 !! nxt k)] | k <- [0 .. n - 1]]
      c0 = (0, 0, r)
      c1 = (len, 0, r)
      caps = concat [[(c0, l0 !! k, l0 !! nxt k), (c1, l1 !! nxt k, l1 !! k)] | k <- [0 .. n - 1]]
   in side ++ caps

-- | The printability probes from the review (edge/pr), rendered and run
-- through `coscad next`: each one warns exactly as expected.
printabilityProbes :: T -> IO ()
printabilityProbes t = do
  dir <- tempDir "printability-probes"
  let parts =
        [ ("cube", "main = box 20 20 20"), ("ball", "main = sphere 10"), ("rod", "main = xcyl 5 40")
        , ("mushroom", "main = zcyl 3 4 |> at top (box 30 30 3)"), ("island", "main = box 20 20 4 ⊕ ζ 10 (box 6 6 4)")
        , ("diagwall", "main = box 60 2 40 |> rotz 45"), ("pillar", "main = box 6 6 20")
        , ("cup", "main = box 30 30 10 |> cut (box 20 20 8 |> z -1)"), ("hhole", "main = box 30 20 20 |> cut (xcyl 4 40)")
        , ("chamf", "main = ▣ 20 20 10 2"), ("dome", "main = box 30 30 10 |> cut (sphere 8 |> z 5)")
        , ("hook", "main = (box 30 10 3) ⊕ (box 3 10 20 |> x 13.5 |> z 8.5) ⊕ (box 10 10 2 |> x 20 |> z 17.5)") ]
  forM_ parts $ \(n, src) -> writeFile (dir </> n <.> "coscad") (src ++ "\n")
  writeFile (dir </> "a.assemble") (unlines ("plate 256 256 6" : [n ++ " ← " ++ f ++ ".coscad ×1 " ++ d | (n, f, d) <-
    [("blk", "cube", "▽bot"), ("corner", "cube", "▽lft+fwd+bot"), ("ball", "ball", "▽bot"), ("rod", "rod", "▽bot"), ("mushroom", "mushroom", "▽bot")
    , ("island", "island", "▽bot"), ("diagwall", "diagwall", "▽bot"), ("pillar", "pillar", "▽bot"), ("cup", "cup", "▽bot")]]))
  writeFile (dir </> "b.assemble") (unlines ("plate 256 256 6" : [n ++ " ← " ++ n ++ ".coscad ×1 ▽bot" | n <- ["hhole", "chamf", "dome", "hook"]]))
  let expected =
        [ ("blk_bot", [], Nothing), ("corner_lft_fwd_bot", ["flat bed contact"], Nothing), ("ball_bot", ["flat bed contact"], Nothing)
        , ("rod_bot", [], Nothing), ("mushroom_bot", ["flat overhang"], Just (800, 870)), ("island_bot", ["elevated downward-facing start at 10 mm"], Nothing)
        , ("diagwall_bot", ["tall, narrow"], Nothing), ("pillar_bot", ["tall, narrow"], Nothing), ("cup_bot", [], Nothing)
        , ("hhole_bot", [], Nothing), ("chamf_bot", [], Nothing), ("dome_bot", [], Nothing), ("hook_bot", ["flat overhang"], Just (70, 90)) ]
  variants <- fmap concat $ forM ["a", "b"] $ \asm -> do
    r <- processNext silent (dir </> asm <.> "assemble")
    case r of
      Left e -> failT t ("printability probes: coscad next " ++ asm) (show e) >> return []
      Right () -> do
        m <- decode <$> readFile (dir </> asm ++ "_manifest.json")
        return (either (const []) (\v -> fromMaybe [] (asArray =<< lookupKey "variants" v)) m)
  forM_ expected $ \(vid, want, ohRange) -> case [v | v <- variants, (asString =<< lookupKey "id" v) == Just vid] of
    [v] -> do
      let p = lookupKey "printability" v
          ws = [w | Just arr <- [asArray =<< lookupKey "warnings" =<< p], x <- arr, Just w <- [asString x]]
          oh = fromMaybe (-1) (asNumber =<< lookupKey "overhang_mm2" =<< p)
          label = if null want then "no warnings" else unwords want
      assertT t ("printability probe " ++ vid ++ ": " ++ label)
        (length ws == length want && and [any (w `isInfixOf`) ws | w <- want]
          && maybe True (\(lo, hi) -> oh >= lo && oh <= hi) ohRange)
        (show (ws, oh))
      let cs = [c | Just arr <- [asArray =<< lookupKey "warning_codes" =<< p], x <- arr, Just c <- [asString x]]
      assertT t ("printability probe " ++ vid ++ ": one warning code per warning") (length cs == length ws && all (`elem` ["small_contact", "tall_narrow", "flat_overhang", "elevated_start"]) cs) (show (cs, ws))
    _ -> failT t ("printability probe " ++ vid) ("variant missing: " ++ show [asString =<< lookupKey "id" v | v <- variants])
  -- `coscad next --strict`: exit 2 on any warning, after writing the outputs; 0 on a clean assembly
  exe <- findExecutable "coscad"
  case exe of
    Nothing -> failT t "next --strict: coscad on PATH" "run the suite with `cabal test`"
    Just bin -> do
      removeFile (dir </> "a_manifest.json")
      (ca, oa, ea) <- readProcessWithExitCode bin ["next", "--strict", dir </> "a.assemble"] ""
      written <- doesFileExist (dir </> "a_manifest.json")
      assertT t "next --strict on edge/pr a.assemble exits 2 naming the warned variants, and still writes the manifest"
        (ca == ExitFailure 2 && written && "Wrote " `isInfixOf` oa
          && any (\l -> "Error: " `isPrefixOf` l && "--strict: 6 printability warnings on" `isInfixOf` l && "mushroom_bot" `isInfixOf` l) (lines ea))
        (show (ca, ea))
      (cn, _, en) <- readProcessWithExitCode bin ["next", dir </> "a.assemble"] ""
      assertT t "next without --strict still exits 0 on the same warnings" (cn == ExitSuccess && "Warning: " `isInfixOf` en) (show (cn, en))
      writeFile (dir </> "clean.assemble") "blk ← cube.coscad ×1 ▽bot\ncup ← cup.coscad ×1 ▽bot\n"
      (cc, _, ec) <- readProcessWithExitCode bin ["next", "--strict", dir </> "clean.assemble"] ""
      assertT t "next --strict on a clean assembly exits 0" (cc == ExitSuccess && not ("Warning: " `isInfixOf` ec)) (show (cc, ec))
      -- plate brim=8 reserves a brim for the tall pillar only; brim=0 on one instance opts out
      writeFile (dir </> "brim.assemble") "plate 256 256 6 brim=8\nblk ← cube.coscad ×1 ▽bot\npillar ← pillar.coscad ×2 ▽bot\nbare ← pillar.coscad ×1 ▽bot brim=0\n"
      (cb, _, eb) <- readProcessWithExitCode bin ["next", dir </> "brim.assemble"] ""
      mb <- decode <$> readFile (dir </> "brim_manifest.json")
      let placementsB = either (const []) (\v -> [pl | Just bs <- [asArray =<< lookupKey "beds" v], b <- bs, Just ps <- [asArray =<< lookupKey "placements" b], pl <- ps]) mb
          brimOf i = firstOr (-1) [n | pl <- placementsB, (asString =<< lookupKey "instance" pl) == Just i, Just n <- [asNumber =<< lookupKey "brim_mm" pl]]
          xyOf i = firstOr (-1, -1) [(x, y) | pl <- placementsB, (asString =<< lookupKey "instance" pl) == Just i, Just x <- [asNumber =<< lookupKey "x" pl], Just y <- [asNumber =<< lookupKey "y" pl]]
          bedBrim = either (const Nothing) (\v -> asNumber =<< lookupKey "brim_mm" =<< lookupKey "bed" v) mb
      assertT t "plate brim=8: the warned pillars reserve 8 mm, the clean block and the brim=0 instance none"
        (cb == ExitSuccess && map brimOf ["pillar_1", "pillar_2", "blk_1", "bare_1"] == [8, 8, 0, 0] && bedBrim == Just 8)
        (show (cb, eb, map brimOf ["pillar_1", "pillar_2", "blk_1", "bare_1"], bedBrim))
      -- each pillar's 22×22 reservation outranks the 20×20 block: pillars 8 mm
      -- inside their cells (14, then 6 + 22 + 6 + 8 = 42), block at 6 + 2·(22 + 6) = 62,
      -- the bare pillar 6 mm after it
      assertT t "plate brim=8: placements step by margin + brim exactly"
        (map xyOf ["blk_1", "pillar_1", "pillar_2", "bare_1"] == [(62, 6), (14, 14), (42, 14), (88, 6)])
        (show (map xyOf ["blk_1", "pillar_1", "pillar_2", "bare_1"]))

-- | `coscad next` against the plate's max height (the 4th `plate`
-- argument) and with a part the packer turns 90° about Z.
bedHeightProbes :: T -> IO ()
bedHeightProbes t = do
  exe <- findExecutable "coscad"
  dir <- tempDir "bed-height"
  forM_ [("post", "box 10 10 200"), ("cube", "box 190 190 190"), ("slab", "box 90 50 10"), ("bar", "box 25 80 10")] $ \(n, src) ->
    writeFile (dir </> n <.> "coscad") ("main = " ++ src ++ "\n")
  forM_ [ ("declared", "plate 256 256 6 180\npost ← post.coscad ×1 ▽bot\n"), ("auto", "plate 256 256 6 180\npost ← post.coscad ×1\n")
        , ("default", "plate 256 256 6\npost ← post.coscad ×1 ▽bot\n"), ("cube", "plate 256 256 6 180\nblock ← cube.coscad ×1\n")
        , ("turn", "plate 100 100 5\nslab ← slab.coscad ×1 ▽bot\nbar ← bar.coscad ×1 ▽bot\n") ] $ \(n, src) ->
    writeFile (dir </> n <.> "assemble") src
  case exe of
    Nothing -> failT t "bed height: coscad on PATH" "run the suite with `cabal test`"
    Just bin -> do
      let next n = readProcessWithExitCode bin ["next", dir </> n <.> "assemble"] ""
          manifest n = decode <$> readFile (dir </> n ++ "_manifest.json")
          maxZ = either (const Nothing) (\v -> asNumber =<< lookupKey "max_z" =<< lookupKey "bed" v)
          downs = either (const []) (\v -> [d | Just vs <- [asArray =<< lookupKey "variants" v], x <- vs, Just d <- [asString =<< lookupKey "down" x]])
      (cd, od, ed) <- next "declared"
      wroteD <- doesFileExist (dir </> "declared_manifest.json")
      assertT t "next: a declared ▽ taller than plate maxZ exits 2 with the height, the limit and the faces that fit; nothing is written"
        (cd == ExitFailure 2 && not wroteD && not (("Wrote " ++ dir </> "declared_bed1") `isInfixOf` od)
          && any (\l -> ("Error: " ++ dir </> "declared.assemble: post_bot: ") `isPrefixOf` l && "200 mm tall, over the plate's max height of 180 mm" `isInfixOf` l
                    && "fits standing on ▽rt, ▽lft, ▽fwd, ▽bak" `isInfixOf` l) (lines ed))
        (show (cd, ed))
      (ca, _, ea) <- next "auto"
      ma <- manifest "auto"
      assertT t "next: without ▽ the part lies down to fit maxZ 180, and the manifest records bed.max_z 180"
        (ca == ExitSuccess && maxZ ma == Just 180 && all (`notElem` ["bot", "top"]) (downs ma) && not (null (downs ma))) (show (ca, ea, maxZ ma, downs ma))
      (cf, _, ef) <- next "default"
      mf <- manifest "default"
      assertT t "next: without a 4th plate argument the same 200 mm post fits the default 250 mm" (cf == ExitSuccess && maxZ mf == Just 250 && downs mf == ["bot"]) (show (cf, ef, maxZ mf))
      (cc, _, ec) <- next "cube"
      assertT t "next: a part too tall on every face exits 2 naming the lowest height" (cc == ExitFailure 2 && "190 mm tall, over the plate's max height of 180 mm on every face" `isInfixOf` ec) (show (cc, ec))
      (ct, _, et) <- next "turn"
      mt <- manifest "turn"
      bed <- readStl (dir </> "turn_bed1.stl")
      let placements = either (const []) (\v -> [(n, pl) | Just bs <- [asArray =<< lookupKey "beds" v], b <- bs, Just ps <- [asArray =<< lookupKey "placements" b], pl <- ps, let n = length bs]) mt
          field k i = firstOr (-1) [x | (_, pl) <- placements, (asString =<< lookupKey "instance" pl) == Just i, Just x <- [asNumber =<< lookupKey k pl]]
          ((x0, y0, z0), (x1, y1, z1)) = meshBounds bed
          near a b = abs (a - b) < 1e-6
      assertT t "next: the packer turns the 25×80 bar 90° to share one bed; rot_z, the turned size and the bed mesh agree"
        (ct == ExitSuccess && map fst placements == [1, 1] && map (`field` "bar_1") ["rot_z", "w", "d", "x", "y"] == [90, 80, 25, 5, 60]
          && field "rot_z" "slab_1" == 0 && and (zipWith near [x0, y0, z0, x1, y1, z1] [5, 5, 0, 95, 85, 10]))
        (show (ct, et, placements, meshBounds bed))
      assertT t "next: the turned bed conserves volume" (abs (meshVolume bed - (90 * 50 * 10 + 25 * 80 * 10)) < 1e-6 * meshVolume bed) (show (meshVolume bed))

-- | Closed axis-aligned box with its min corner at the origin, outward
-- normals (counter-clockwise seen from outside).
boxTris :: Double -> Double -> Double -> [Tri]
boxTris w d h = concatMap quad
  [ [(0,0,0),(0,d,0),(w,d,0),(w,0,0)], [(0,0,h),(w,0,h),(w,d,h),(0,d,h)]
  , [(0,0,0),(w,0,0),(w,0,h),(0,0,h)], [(0,d,0),(0,d,h),(w,d,h),(w,d,0)]
  , [(0,0,0),(0,0,h),(0,d,h),(0,d,0)], [(w,0,0),(w,d,0),(w,d,h),(w,0,h)] ]
  where quad [a,b,c,e] = [(a,b,c),(a,c,e)]
        quad _ = []

-- | `coscad next` on the tesseract adhesion retry: the manifest's contact
-- areas match the independent trimesh measurement in verification.json,
-- and the printability block equals the warnings printed on stderr.
adhesionRetryContact :: T -> IO ()
adhesionRetryContact t = do
  exe <- findExecutable "coscad"
  dir <- tempDir "adhesion-contact"
  _ <- readProcessWithExitCode "cp" ["-R", "examples/assemble/tesseract", dir] ""
  let work = dir </> "tesseract" </> "adhesion-retry"
  verification <- decode <$> readFile "examples/assemble/tesseract/adhesion-retry/verification.json"
  case exe of
    Nothing -> failT t "adhesion retry contact: coscad on PATH" "run the suite with `cabal test`"
    Just bin -> do
      (c, _, e) <- readProcessWithExitCode bin ["next", work </> "fit-test.assemble"] ""
      m <- decode <$> readFile (work </> "fit-test_manifest.json")
      case (c, m, verification) of
        (ExitSuccess, Right mv, Right vv) -> do
          let variants = fromMaybe [] (asArray =<< lookupKey "variants" mv)
              field k v = asString =<< lookupKey k v
              printab v = lookupKey "printability" v
              contactOf part = firstOr (-1) [n | v <- variants, field "part" v == Just part, Just n <- [asNumber =<< lookupKey "bed_contact_mm2" =<< printab v]]
              measured inst = fromMaybe (-1) (asNumber =<< lookupKey "bed_contact_mm2" =<< lookupKey inst =<< lookupKey "measured_parts" vv)
          forM_ [("cap", "cap_1"), ("corner", "corner_1"), ("bar", "bar_1"), ("fullbar", "fullbar_1")] $ \(part, inst) ->
            assertT t ("coscad next: " ++ part ++ " bed contact matches the trimesh measurement within 0.05 mm²")
              (abs (contactOf part - measured inst) < 0.05) (show (contactOf part, measured inst))
          assertT t "coscad next: tesseract cap contact is 69.73 mm²" (abs (contactOf "cap" - 69.73) < 0.05) (show (contactOf "cap"))
          let warningsOf part = [w | v <- variants, field "part" v == Just part, Just arr <- [asArray =<< lookupKey "warnings" =<< printab v], x <- arr, Just w <- [asString x]]
          assertT t "coscad next: the 7×7×18.8 fit-test bar no longer warns as tall and narrow" (null (warningsOf "bar")) (show (warningsOf "bar"))
          let placements = [pl | Just bs <- [asArray =<< lookupKey "beds" mv], b <- bs, Just ps <- [asArray =<< lookupKey "placements" b], pl <- ps]
              num k pl = fromMaybe (-1) (asNumber =<< lookupKey k pl)
          assertT t "adhesion retry: every part reserves its 8 mm brim inside the default 6 mm margin (no hand-raised 12 mm margin)"
            (length placements == 4 && all ((== 8) . num "brim_mm") placements && minimum (map (num "x") placements) == 14
              && minimum (map (num "y") placements) == 14 && (asNumber =<< lookupKey "margin" =<< lookupKey "bed" mv) == Just 6)
            (show placements)
          let manifestWarnings = sort [ "Warning: " ++ i ++ ": " ++ w | v <- variants, Just i <- [field "id" v]
                                      , w <- maybe [] (mapMaybeString) (asArray =<< lookupKey "warnings" =<< printab v)]
              mapMaybeString = foldr (\x acc -> maybe acc (: acc) (asString x)) []
              printed = sort (filter ("Warning: " `isPrefixOf`) (lines e))
          assertT t "coscad next: the manifest printability warnings are exactly the ones printed" (manifestWarnings == printed && not (null printed)) (show (manifestWarnings, printed))
        _ -> failT t "adhesion retry contact: coscad next fit-test" (show (c, e, either id (const "parsed") m, either id (const "parsed") verification))

-- ------------------------------------------------------------------
-- constraint loops: the whole loop, blamed on the line that closed it

variantNameTier :: T -> IO ()
variantNameTier t = do
  section "variant names"
  let fp trail n src down = FlatPart trail n src 1 down [] Empty
      lf = Just ((-1, 0, -1), "lft+fwd")
      fw = Just ((0, -1, 0), "fwd")
      names = map snd . variantNames
  assertT t "variant names: a unique part keeps its historical name"
    (names [fp [] "bar" "bar.coscad" fw] == ["bar_fwd"]) ""
  assertT t "variant names: one source used twice is one variant"
    (names [fp ["a"] "base" "s/base.coscad" Nothing, fp ["b"] "base" "s/base.coscad" Nothing] == ["base_asis"]) ""
  let nested = names [fp ["a"] "base" "s1/base.coscad" Nothing, fp ["b"] "base" "s2/base.coscad" Nothing]
  assertT t "variant names: the same part name from two sources is qualified by its trail"
    (nested == ["a_base_asis", "b_base_asis"]) (show nested)
  let sanitized = names [fp [] "p_lft" "p.coscad" fw, fp [] "p" "q.coscad" lf]
  assertT t "variant names: names equal after sanitizing are numbered apart"
    (sanitized == ["p_lft_fwd_1", "p_lft_fwd_2"]) (show sanitized)
  let mixed = names [fp [] "base" "x/base.coscad" Nothing, fp ["a"] "base" "s1/base.coscad" Nothing, fp ["b"] "base" "s2/base.coscad" Nothing, fp [] "p_lft" "p.coscad" fw, fp [] "p" "q.coscad" lf]
  assertT t "variant names: every name is distinct"
    (length mixed == 5 && and [length (filter (== n) mixed) == 1 | n <- mixed]) (show mixed)
  let parts = [fp ["a"] "base" "s1/base.coscad" Nothing, fp ["b"] "base" "s2/base.coscad" Nothing, (fp [] "bar" "bar.coscad" fw) {fpCount = 3}, fp ["c"] "base" "s1/base.coscad" Nothing]
      pv = printVariants parts
  assertT t "print variants: keys and names match variantNames, one first part per variant"
    ([(k, n) | (k, n, _) <- pv] == variantNames parts && [fpTrail f | (_, _, f) <- pv] == [["a"], ["b"], []]) (show [(k, n) | (k, n, _) <- pv])
  let insts = printInstances parts
  assertT t "print instances: counts expanded, one id per copy"
    (length insts == 6 && [i | (i, _, _) <- insts] == ["a/base_1", "b/base_1", "bar_1", "bar_2", "bar_3", "c/base_1"]) (show [i | (i, _, _) <- insts])
  let m = variantMap fst [(k, n) | (k, n, _) <- pv]
  assertT t "print variants: a missing variant is an internal error, not a crash"
    (either ("internal error: no print variant for nope.coscad" `isPrefixOf`) (const False) (lookupVariant m ("nope.coscad", "top"))) ""
  case packBeds (100, 100, 6) [(i, k, (10, 10)) | (i, k, _) <- insts] of
    Left e -> failT t "print variants: every placement resolves to its variant" e
    Right beds -> assertT t "print variants: every placement resolves to its variant"
      (fmap (map (map (\(p, (k, _)) -> plKey p == k))) (placedVariants m beds) == Right (map (map (const True)) beds)) ""

-- | Two subassemblies that each declare `base`, from different sources,
-- plus two top-level names that sanitize alike (p_lft ▽fwd, p ▽lft+fwd).
writeVariantCollisions :: FilePath -> IO ()
writeVariantCollisions dir = do
  forM_ [("s1", 10 :: Int), ("s2", 30)] $ \(sub, size) -> do
    createDirectoryIfMissing True (dir </> sub)
    writeFile (dir </> sub </> "base.coscad") ("main = box " ++ unwords (replicate 3 (show size)) ++ "\n")
    writeFile (dir </> sub </> "sub.assemble") "base ← base.coscad ×1\nasm = base\n"
  writeFile (dir </> "top.assemble") "a ← s1/sub.assemble ×1\nb ← s2/sub.assemble ×1\nasm = a ⊕ χ 50 b\n"
  writeFile (dir </> "p.coscad") "main = box 5 5 5\n"
  writeFile (dir </> "q.coscad") "main = box 9 9 9\n"
  writeFile (dir </> "alike.assemble") "p_lft ← p.coscad ×1 ▽fwd\np ← q.coscad ×1 ▽lft+fwd\nasm = p_lft ⊕ χ 20 p\n"

-- | Variant ids, their artefact paths, and the variant each placement names.
manifestVariants :: String -> String -> Either String ([String], [String], [String])
manifestVariants artefact json = do
  v <- decode json
  let arr k x = maybe [] id (asArray =<< lookupKey k x)
      str k x = maybe "" id (asString =<< lookupKey k x)
      variants = arr "variants" v
      placements = arr "placements" v ++ concatMap (arr "placements") (arr "beds" v)
  return (map (str "id") variants, map (str artefact) variants, map (str "variant") placements)

distinctVariants :: ([String], [String], [String]) -> Bool
distinctVariants (ids, paths, used) =
  allDistinct ids && allDistinct paths && all (\u -> length (filter (== u) ids) == 1) used && not (null used)
  where
    allDistinct xs = and [length (filter (== x) xs) == 1 | x <- xs]
