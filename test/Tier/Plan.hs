-- | `coscad plan` tiers: build plans and framed fasteners.
module Tier.Plan
  ( planTier
  , framedFastenerTier
  , checkInvariants
  )
where

import Control.Monad (forM_)
import Coscad.Geometry (mApply, vlen, vsub, vadd, rodMat)
import Coscad.Plan (PlanSummary (..), StepSummary (..), FastenerSummary (..), planSummary, processPlanSummary)
import Coscad.Site (buildPageHtml, indexHtml)
import Data.List (isInfixOf, isPrefixOf, sort)
import System.Directory
import System.FilePath
import Harness

-- | Generate a plan's Markdown in a scratch copy of its sources. Plan output
-- is not committed (step 15), so the test must not read a checked-in copy.
generatedPlanMd :: String -> FilePath -> String -> IO String
generatedPlanMd tag srcDir name = do
  tmp <- tempDir tag
  fs <- listDirectory srcDir
  forM_ [f | f <- fs, takeExtension f `elem` [".coscad", ".assemble"]] $ \f -> copyFile (srcDir </> f) (tmp </> f)
  _ <- processPlanSummary False (tmp </> name <.> "assemble")
  readFile (tmp </> name ++ "_plan.md")

planTier :: T -> IO ()
planTier t = do
  section "build plans"
  -- the design document's two-rail corner
  rc <- planSummary "examples/assemble/plan/corner_pair.assemble"
  case rc of
    Left e -> failT t "plan corner_pair" e
    Right ps -> do
      assertT t "corner_pair: 3 parts, 2 fasteners, no design errors" (length (psParts ps) == 3 && psFastenerCount ps == 2 && null (psDesignErrors ps)) (show ps)
      checkInvariants t "corner_pair" ps
      assertT t "corner_pair: both screws driven, no flips" (sum (map (length . ssFasteners) (psSteps ps)) == 2 && not (any ssFlip (psSteps ps))) (show (psSteps ps))
  -- the 2020 cube: rails, brackets, panel
  tmp <- tempDir "plan-cube"
  src <- readFile "examples/assemble/plan/cube.assemble"
  writeFile (tmp </> "cube.assemble") (src ++ "\nplan beam=10\n") -- narrow beam keeps the test fast
  forM_ ["rail200.coscad", "rail160.coscad", "flat90.coscad", "panel190.coscad"] $ \f -> copyFile ("examples/assemble/plan" </> f) (tmp </> f)
  rq <- planSummary (tmp </> "cube.assemble")
  case rq of
    Left e -> failT t "plan cube" e
    Right ps -> do
      assertT t "cube: 29 instances, 38 fasteners, no design errors" (length (psParts ps) == 29 && psFastenerCount ps == 38 && null (psDesignErrors ps)) (show (length (psParts ps), psFastenerCount ps, psDesignErrors ps))
      checkInvariants t "cube" ps
      assertT t "cube: every fastener driven" (sum (map (length . ssFasteners) (psSteps ps)) == 38) (show (sum (map (length . ssFasteners) (psSteps ps))))
      assertT t "cube: a handful of flips, not one per bracket" (length (filter ssFlip (psSteps ps)) <= 8) (show (length (filter ssFlip (psSteps ps))))
  -- design errors are reported: two screws at the same spot
  let bad = src ++ "\nfastener M5x10 br_bottom_fl bottom_front bot at=30\n"
  writeFile (tmp </> "bad.assemble") bad
  rb <- planSummary (tmp </> "bad.assemble")
  assertT t "plan: colliding screws are a design error" (either (const False) (any ("collide" `isInfixOf`) . psDesignErrors) rb) (either id (show . psDesignErrors) rb)
  writeFile (tmp </> "clamp.coscad") "main = box 5 2 5\n"
  writeFile (tmp </> "miss-slot.assemble") "rail ← rail200.coscad ×0 profile=2020\nclamp ← clamp.coscad ×1\nasm = (rail |> move 100 10 10) ⊕ (clamp |> move 100 -1 18)\nfastener M5x10 clamp rail fwd at=100\n"
  miss <- planSummary (tmp </> "miss-slot.assemble")
  expectErr t "plan: partial overlap cannot move a screw off the rail slot" miss ["does not cover the 2020 slot centre"] []
  -- the companion site pages embed the plan and reference the step images
  let page = buildPageHtml "cube" "{\"steps\": []}" "cube_step" 3
      idx = indexHtml [("cube", "cube", ["M5x10", "material=printed"], 29, 38, 29)]
  assertT t "site: build page embeds plan JSON, images, safe-area header, both themes"
    (all (`isInfixOf` page) ["const PLAN = {\"steps\": []}", "cube_step", "NSTEPS = 3", "env(safe-area-inset-top", "data-theme=dark", "prefers-color-scheme:dark"]) (take 400 page)
  assertT t "site: index lists builds with searchable tags" (all (`isInfixOf` idx) ["\"cube\"", "M5x10 material=printed", "\"steps\": 29"]) (take 400 idx)
  -- The bow now describes all four real bolts and captive nuts.
  rw <- planSummary "examples/assemble/bow3/bow3.assemble"
  assertT t "plan: bow3 places all three parts and makes all four M4 joints" (either (const False) (\ps -> sum (map (length . ssParts) (psSteps ps)) == 3 && psFastenerCount ps == 4 && null (psDesignErrors ps)) rw) (either id show rw)
  -- the bolted ball: enclosing shells, captive hex nuts in pockets
  rl <- planSummary "examples/assemble/ball/ball.assemble"
  case rl of
    Left e -> failT t "plan ball" e
    Right ps -> do
      assertT t "ball: 3 parts, 2 fasteners, no design errors" (length (psParts ps) == 3 && psFastenerCount ps == 2 && null (psDesignErrors ps)) (show ps)
      checkInvariants t "ball" ps
      assertT t "ball: both screws driven" (sum (map (length . ssFasteners) (psSteps ps)) == 2) (show (psSteps ps))
  ballMd <- generatedPlanMd "plan-ball-md" "examples/assemble/ball" "ball"
  assertT t "ball: plan speaks of hex nuts and pockets, not T-slots" (all (`isInfixOf` ballMd) ["M5 hex nut", "pocket on the top face", "socket head through shell1"] && not ("T-nut" `isInfixOf` ballMd)) (take 600 ballMd)
  ballSrc <- readFile "examples/assemble/ball/ball.assemble"
  tmpB <- tempDir "plan-ball"
  forM_ ["shell.coscad", "core.coscad"] $ \f -> copyFile ("examples/assemble/ball" </> f) (tmpB </> f)
  writeFile (tmpB </> "bad.assemble") (ballSrc ++ "\nfastener M5x20 shell1 core rt pocket=top\n")
  rp <- planSummary (tmpB </> "bad.assemble")
  assertT t "plan: pocket= without nut=hex is an error" (either ("pocket= only applies to nut=hex" `isInfixOf`) (const False) rp) (either id show rp)
  -- Preserve the old bolted tesseract regression alongside the new captive design.
  rt <- planSummary "examples/assemble/tesseract/bolted/tesseract.assemble"
  case rt of
    Left e -> failT t "plan tesseract" e
    Right ps -> do
      assertT t "tesseract: 29 parts, 40 joints, no design errors" (length (psParts ps) == 29 && psFastenerCount ps == 40 && null (psDesignErrors ps)) (show (length (psParts ps), psFastenerCount ps, psDesignErrors ps))
      checkInvariants t "tesseract" ps
      assertT t "tesseract: every joint made, every block preloaded with its nuts" (sum (map (length . ssFasteners) (psSteps ps)) == 40 && sort (concatMap ssPreload (psSteps ps)) == sort ["corner" ++ show i | i <- [1 .. 8 :: Int]]) (show (psSteps ps))
      assertT t "tesseract: starts with the corner blocks on the bench" (take 4 (concatMap ssParts (psSteps ps)) == ["corner1", "corner2", "corner3", "corner4"]) (show (take 6 (concatMap ssParts (psSteps ps))))
      assertT t "tesseract: no screw is driven up through the bench" (all (\st -> ssRest st /= "bottom (-Z) face" || null [f | f@(_, _, _, _) <- ssFasteners st, False]) (psSteps ps)) "n/a"
      assertT t "tesseract: at most 2 flips" (length (filter ssFlip (psSteps ps)) <= 2) (show (length (filter ssFlip (psSteps ps))))
  tessMd <- generatedPlanMd "plan-tesseract-md" "examples/assemble/tesseract/bolted" "tesseract"
  assertT t "tesseract: plan speaks of pegs and nut pockets in the blocks, no T-nuts" (all (`isInfixOf` tessMd) ["Push: strut1 into", "24 × M3 hex nut", "into the pocket on the top face"] && not ("T-nut" `isInfixOf` tessMd)) (take 500 tessMd)
  -- A rotor must enter its recessed journal before the next axle captures it.
  ro <- planSummary "models/orbit-tower/orbit-tower.assemble"
  case ro of
    Left e -> failT t "plan orbit tower" e
    Right ps -> do
      checkInvariants t "orbit tower" ps
      let steps = zip [1 :: Int ..] (psSteps ps)
          stepOf p = firstOr maxBound [k | (k, s) <- steps, p `elem` ssParts s]
          joints = concatMap ssFasteners (psSteps ps)
      assertT t "orbit tower: 20 parts and all 19 peg joints, no hardware or errors"
        (length (psParts ps) == 20 && length joints == 19 && all (\(s, _, _, n) -> s == "peg" && n == "none") joints && null (psDesignErrors ps) && all (null . ssPreload) (psSteps ps)) (show ps)
      assertT t "orbit tower: stays upright throughout assembly"
        (all (\s -> ssRest s == "bottom (-Z) face" && not (ssFlip s)) (psSteps ps)) (show (psSteps ps))
      assertT t "orbit tower: every rotor enters before the axle or cap that captures it"
        (and [stepOf ("rotor" ++ show k) < stepOf (if k == 6 then "cap" else "axle" ++ show (k + 1)) | k <- [1 .. 6 :: Int]]) (show (psSteps ps))
      assertT t "orbit tower: each rotor and satellite follows its supporting part"
        (and [stepOf ("axle" ++ show k) < stepOf ("rotor" ++ show k) && stepOf ("rotor" ++ show k) < stepOf ("satellite" ++ show k) | k <- [1 .. 6 :: Int]]) (show (psSteps ps))
  orbitSrc <- readFile "models/orbit-tower/orbit-tower.assemble"
  tmpO <- tempDir "plan-orbit"
  forM_ ["base.coscad", "axle.coscad", "rotor.coscad", "satellite.coscad", "cap.coscad"] $ \f -> copyFile ("models/orbit-tower" </> f) (tmpO </> f)
  let withBaseHint hint = unlines [if "base ←" `isPrefixOf` l then l ++ " after=" ++ hint else l | l <- lines orbitSrc]
      invalidPlans =
        [ ("unknown after reference", withBaseHint "missing", "unknown part 'missing'")
        , ("unknown group after reference", withBaseHint "rotor", "unknown part")
        , ("self after cycle", withBaseHint "base", "cyclic after= ordering")
        , ("two-part after cycle", withBaseHint "axle1", "cyclic after= ordering")
        , ("negative peg inset", orbitSrc ++ "\nfastener peg rotor1 axle1 top inset=-1\n", "inset= must lie within the host")
        , ("peg inset beyond host", orbitSrc ++ "\nfastener peg rotor1 axle1 top inset=25\n", "inset= must lie within the host")
        , ("invalid peg inset", orbitSrc ++ "\nfastener peg rotor1 axle1 top inset=bad\n", "inset= needs a distance")
        , ("screw with peg inset", orbitSrc ++ "\nfastener M4x10 rotor1 axle1 top inset=1\n", "inset= only applies to peg joints")
        ]
  forM_ invalidPlans $ \(name, source, message) -> do
    writeFile (tmpO </> "bad.assemble") source
    result <- planSummary (tmpO </> "bad.assemble")
    assertT t ("plan: " ++ name ++ " is rejected") (either (message `isInfixOf`) (const False) result) (either id show result)

-- Independent synthetic joints: positions, directions and reach must follow
-- arbitrary rigid poses and renamed parts, rather than an example name.
framedFastenerTier :: T -> IO ()
framedFastenerTier t = do
  section "frame-based hardware geometry and diagnostics"
  dir <- tempDir "framed-fasteners"
  let host = "main = box 20 20 10 |> frame hole top top bak -4 -4 0 |> frame nut bot bot bak -4 -4 3\n"
      clamp = "main = box 20 20 4 |> frame head top top bak -4 -4 0\n"
      joint = "fastener M4x16 lid block @hole seat=@head head=socket nut=hex pocket=@nut\n"
      src = "block ← block.coscad ×1 preload=hand\nlid ← lid.coscad ×1\nasm = block ⊕ (lid |> move 0 0 7)\n" ++ joint
      run source = writeFile (dir </> "test.assemble") source >> planSummary (dir </> "test.assemble")
      near a b = vlen (vsub a b) < 1e-6
      writeParts h c = writeFile (dir </> "block.coscad") h >> writeFile (dir </> "lid.coscad") c
  writeParts host clamp
  good <- run src
  case good of
    Left e -> failT t "framed joint plans" e
    Right ps -> do
      checkInvariants t "framed joint" ps
      assertT t "framed joint: measured grip and nut reach"
        (case psFasteners ps of [f] -> near (fsPosition f) (-4,-4,5) && near (fsHeadPosition f) (-4,-4,9) && maybe False (`near` (-4,-4,-2)) (fsNutSeat f) && abs(fsThreadReach f - 5) < 1e-7; _ -> False) (show ps)
      assertT t "framed joint: in-hand preload precedes the host placement"
        (take 1 (concatMap ssPreload (psSteps ps)) == ["block"] && all (null . ssParts) (take 1 (psSteps ps))) (show (psSteps ps))
      assertT t "framed joint: preload preparation warning survives step grouping"
        (any ("in hand" `isInfixOf`) (concatMap ssWarnings (psSteps ps))) (show(psSteps ps))
  let transform = " |> rotx 23 |> rotz 37 |> move 31 -17 9"
      rotated = "block ← block.coscad ×1 preload=hand\nlid ← lid.coscad ×1\nasm = (block ⊕ (lid |> move 0 0 7))" ++ transform ++ "\n" ++ joint
      move p = vadd (31,-17,9) (mApply (rodMat 37 (0,0,1)) (mApply (rodMat 23 (1,0,0)) p))
      axis = mApply (rodMat 37 (0,0,1)) (mApply (rodMat 23 (1,0,0)) (0,0,1))
  tilted <- run rotated
  assertT t "framed joint: arbitrary rotations carry hole, head, nut and axis"
    (either (const False) (\ps -> case psFasteners ps of [f] -> near (fsPosition f) (move (-4,-4,5)) && near (fsHeadPosition f) (move (-4,-4,9)) && near (fsAxis f) axis && maybe False (`near` move (-4,-4,-2)) (fsNutSeat f) && maybe False (`near` move (-4,-4,-5)) (fsNutEntry f) && abs(fsThreadReach f-5) < 1e-7; _ -> False) tilted) (either id show tilted)
  renamed <- run "core ← block.coscad ×1 preload=hand\ncover ← lid.coscad ×1\nasm = core ⊕ (cover |> move 0 0 7)\nfastener M4x16 cover core @hole seat=@head head=socket nut=hex pocket=@nut\n"
  assertT t "framed joint: aliases do not affect hardware placement"
    (either (const False) (\ps -> case psFasteners ps of [f] -> near (fsPosition f) (-4,-4,5) && abs(fsThreadReach f-5) < 1e-7; _ -> False) renamed) (either id show renamed)
  writeParts "main = box 20 20 16 |> frame hole top top bak -4 -4 0 |> frame nut bot bot bak -4 -4 3\n" clamp
  resized <- run "block ← block.coscad ×1 preload=hand\nlid ← lid.coscad ×1\nasm = block ⊕ (lid |> move 0 0 10)\nfastener M4x22 lid block @hole seat=@head head=socket nut=hex pocket=@nut\n"
  assertT t "framed joint: resized host updates both bearing planes and grip"
    (either (const False) (\ps -> case psFasteners ps of [f] -> near (fsPosition f) (-4,-4,8) && near (fsHeadPosition f) (-4,-4,12) && maybe False (`near` (-4,-4,-5)) (fsNutSeat f) && abs(fsThreadReach f-5) < 1e-7; _ -> False) resized) (either id show resized)
  writeParts "main = box 60 20 10 |> frame hole top top bak -4 0 0\n" "main = box 20 20 4 |> frame head top top bak -4 0 0\n"
  let railSrc = "block ← block.coscad ×0 profile=2020 preload=hand\nlid ← lid.coscad ×1\nasm = (block ⊕ (lid |> move 0 0 7))" ++ transform ++ "\nfastener M4x10 lid block @hole seat=@head\n"
  rail <- run railSrc
  assertT t "framed rail: slot checks use part coordinates under arbitrary rotation"
    (either (const False) (\ps -> case psFasteners ps of [f] -> near (fsPosition f) (move (-4,0,5)) && near (fsAxis f) axis && null(psDesignErrors ps); _ -> False) rail) (either id show rail)
  writeParts "main = box 60 20 10 |> frame hole top top bak -4 2 0\n" "main = box 20 20 4 |> frame head top top bak -4 2 0\n"
  offSlot <- run railSrc
  expectErr t "framed rail: an off-centre hole remains invalid after rotation" offSlot ["2020 slot centre"] []
  writeParts host clamp
  -- Longer solid above the counterbore must not change the bearing seat or
  -- produce a phantom driver collision with an object behind that seat.
  let recessed = "main = box 20 20 4 |> frame head ctr top bak -4 -4 0\n"
  writeParts host recessed
  recess <- run src
  assertT t "framed joint: recessed head uses its seat, not the outside box"
    (either (const False) (\ps -> case psFasteners ps of [f] -> near (fsHeadPosition f) (-4,-4,7) && abs(fsThreadReach f-7) < 1e-7; _ -> False) recess) (either id show recess)
  writeParts host clamp
  let invalid =
        [ ("missing head seat", "fastener M4x16 lid block @hole nut=hex pocket=@nut\n", "needs seat=@frame")
        , ("unknown hole", "fastener M4x16 lid block @missing seat=@head nut=hex pocket=@nut\n", "unknown frame '@missing'")
        , ("missing nut seat", "fastener M4x16 lid block @hole seat=@head nut=hex\n", "needs pocket=@frame")
        , ("mixed offset conventions", "fastener M4x16 lid block @hole seat=@head at=4 nut=hex pocket=@nut\n", "omit ×count/at=")
        , ("group at one hole", "fastener M4x16 lid block @hole ×2 seat=@head nut=hex pocket=@nut\n", "locates one fastener")
        , ("mixed grip conventions", "fastener M4x16 lid block @hole seat=@head through=4 nut=hex pocket=@nut\n", "omit inset=/through=")
        , ("frame option with old face", "fastener M4x16 lid block top seat=@head nut=hex pocket=@nut\n", "requires a host @frame")
        , ("frame peg", "fastener peg lid block @hole seat=@head\n", "currently describe screws")
        ]
      base = unlines (take 3 (lines src))
  forM_ invalid $ \(name,line,msg) -> do
    r <- run (base ++ line)
    expectErr t ("framed joint: " ++ name) r [msg] []
  forM_ [ ("misaligned head", "main = box 20 20 4 |> frame head top top bak -3 -4 0\n", "must be coaxial")
        , ("reversed head", "main = box 20 20 4 |> frame head top bot bak -4 -4 0\n", "matching outward normals")
        , ("head outside solid", "main = box 20 20 4 |> frame head top top bak 40 0 0\n", "outside part")
        , ("point instead of frame", "main = box 20 20 4 |> datum head top -4 -4 0\n", "point datum")
        , ("ambiguous frame", "main = (box 20 20 4 |> frame head top top bak -4 -4 0) ⊕ (box 20 20 4 |> frame head top top bak 4 4 0)\n", "ambiguous frame")
        ] $ \(name,c,msg) -> do
    writeParts host c
    r <- run src
    expectErr t ("framed joint: " ++ name) r [msg] []
  writeParts "main = box 20 20 10 |> frame hole top top bak -4 -4 0 |> frame nut bot top bak -4 -4 3\n" clamp
  wrongNut <- run src
  expectErr t "framed joint: nut bearing normal must oppose the head" wrongNut ["opposing outward normals"] []
  writeParts host clamp
  short <- run (base ++ "fastener M4x12 lid block @hole seat=@head nut=hex pocket=@nut\n")
  assertT t "framed joint: short screw checks actual distance to the nut seat"
    (either (const False) (any ("only 1 mm" `isInfixOf`) . psDesignErrors) short) (either id show short)
  duplicate <- run (src ++ joint)
  assertT t "framed joint: duplicate hole is a hardware collision"
    (either (const False) (any ("collide" `isInfixOf`) . psDesignErrors) duplicate) (either id show duplicate)
  -- A driver obstruction must remain an error, even though a frame can
  -- locate a recessed seat independently of the clamped part's outside box.
  writeFile (dir </> "obstacle.coscad") "main = box 2 2 30 |> move -4 -4 10\n"
  blocked <- run ("block ← block.coscad ×1 preload=hand\nlid ← lid.coscad ×1 after=obstacle\nobstacle ← obstacle.coscad ×1\nasm = block ⊕ (lid |> move 0 0 7) ⊕ obstacle\n" ++ joint)
  expectErr t "framed joint: another part can block the actual driver path" blocked ["blocked by obstacle"] []
  originalCorner <- readFile "examples/assemble/corner.assemble"
  forM_ ["rail200.coscad","rail120.coscad","inner90.coscad","bracket90.coscad"] $ \file -> copyFile ("examples/assemble" </> file) (dir </> file)
  let wrongCorner = unlines [if "mate bracket1 @other_bore railH" `isPrefixOf` line
                             then "mate bracket1 @outer_bore_railH railH @outer_bore_bracket1 roll=180"
                             else line | line <- lines originalCorner]
  badRotation <- run wrongCorner
  expectErr t "corner: closing both bracket bores rejects its original wrong rotation" badRotation ["conflicting assembly constraints"] []
  bow <- planSummary "examples/assemble/bow3/bow3.assemble"
  case bow of
    Left e -> failT t "bow frame hardware" e
    Right ps -> do
      checkInvariants t "bow hardware" ps
      assertT t "bow: four distinct holes, recessed heads, 4 mm nut reach"
        (sort (map fsPosition (psFasteners ps)) == sort [(x,y,0) | x <- [-42,42], y <- [-8,8]] && all (\f -> abs(fsThreadReach f-4) < 1e-7) (psFasteners ps) && null(psDesignErrors ps)) (show ps)
  corner <- planSummary "examples/assemble/corner.assemble"
  case corner of
    Left e -> failT t "corner frame hardware" e
    Right ps -> do
      checkInvariants t "corner hardware" ps
      assertT t "corner: all six screws planned, clear of each other"
        (psFastenerCount ps == 6 && sum(map(length . ssFasteners)(psSteps ps)) == 6 && null(psDesignErrors ps)) (show ps)

-- nut-first, host-and-clamp-before-screw, every part exactly once
checkInvariants :: T -> String -> PlanSummary -> IO ()
checkInvariants t name ps = do
  let steps = zip [1 :: Int ..] (psSteps ps)
      stepOfPart p = firstOr maxBound [k | (k, s) <- steps, p `elem` ssParts s]
      stepOfPreload h = firstOr maxBound [k | (k, s) <- steps, h `elem` ssPreload s]
      screws = [(k, f) | (k, s) <- steps, f <- ssFasteners s]
      lateHost = [f | (k, f@(_, c, h, _)) <- screws, stepOfPart h > k || stepOfPart c > k]
      lateNut = [f | (k, f@(_, c, h, nut)) <- screws, nut /= "none", min (stepOfPreload h) (stepOfPreload c) > k]
      placedTwice = [p | p <- psParts ps, length [() | (_, s) <- steps, p `elem` ssParts s] /= 1]
  assertT t (name ++ ": host and clamped part are placed before each screw") (null lateHost) (show lateHost)
  assertT t (name ++ ": every rail is preloaded before its first screw (nut-first)") (null lateNut) (show lateNut)
  assertT t (name ++ ": every part placed exactly once") (null placedTwice) (show placedTwice)
