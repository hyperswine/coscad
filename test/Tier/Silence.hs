-- | Things that used to pass silently: an anchor that does not sit on
-- the geometry it names, a print count that differs from the placements,
-- and two parser messages that pointed at the wrong cause.
module Tier.Silence
  ( silenceTier
  )
where

import Coscad.Geometry (anchorWarnings, formatAnchorWarning)
import Coscad.Parser (parseProgramNamed)
import Data.List (isInfixOf)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Harness

silenceTier :: T -> IO ()
silenceTier t = do
  section "silent cases now reported"
  let p = parseProgramNamed "s.coscad"
      warns src = either (const ["<parse error>"]) (map (formatAnchorWarning "s.coscad") . anchorWarnings . snd) (p src)
  -- messages
  expectErr t "'^' in a numeric binding is named, not 'expecting a shape'"
    (p "k = 2 ^ 3\nmain = box k 1 1\n") ["s.coscad:1:7:", "'^' is not an arithmetic operator", "+ - * /"] ["expecting a shape"]
  expectErr t "'**' in a numeric binding is named"
    (p "w = 4\nk = w ** 2\nmain = box k 1 1\n") ["s.coscad:2:7:", "'**' is not an arithmetic operator"] ["expecting a shape"]
  expectErr t "a boolean operator after a pipeline asks for parentheses"
    (p "main = box 10 10 10 |> move 1 2 3 ⊕ box 1 1 1\n") ["s.coscad:1:35:", "cannot follow a pipeline", "(a |> move 1 2 3) ⊕ b"] ["expecting \"|>\""]
  assertT t "a parenthesised pipeline still combines" (null (warns "main = (box 10 10 10 |> move 1 2 3) ⊕ box 1 1 1\n")) ""
  -- anchors on removed faces
  let cutFace = warns "main = box 10 10 10 |> cut (box 20 20 20 |> move 0 0 10) |> at top 0 0 0 (box 1 1 1)\n"
  assertT t "anchor: at top on a face a cut removed warns, at the 'at' keyword"
    (any (\w -> "s.coscad:1:61: 'at top'" `isInfixOf` w && "inside a cut" `isInfixOf` w) cutFace) (show cutFace)
  assertT t "anchor: a cut that starts at the face leaves it, no warning"
    (null (warns "main = box 10 10 10 |> cut (box 20 20 20 |> move 0 0 15) |> at top 0 0 0 (box 1 1 1)\n")) ""
  assertT t "anchor: a hole drilled at the face, no warning"
    (null (warns "main = box 10 10 4 |> cutat top 0 0 0 (zcyl 1 10) |> at top 0 0 0 (box 1 1 1)\n")) ""
  assertT t "anchor: cutat at a removed corner cuts air around a valid point, no warning"
    (null (warns "main = box 10 10 10 |> cut (box 4 4 40 |> move 5 5 0) |> cutat top+rt+bak 0 0 0 (sphere 1)\n")) ""
  -- anchors on turned parts
  let turned = warns "main = box 10 10 10 |> roty 45 |> at top 0 0 0 (box 1 1 1)\n"
  assertT t "anchor: at top on a part turned 45 degrees warns, at the 'at' keyword"
    (any (\w -> "s.coscad:1:35: 'at top'" `isInfixOf` w && "non-right angle" `isInfixOf` w) turned) (show turned)
  let continued = warns "base = box 10 10 10\n  |> roty 30\nmain = base\n  |> on top (box 1 1 1)\n"
  assertT t "anchor: position on a continuation line of a definition"
    (any ("s.coscad:4:6: 'on top'" `isInfixOf`) continued) (show continued)
  let elsewhere = warns "part = box 10 10 10 |> rotz 30 |> anchor bot\nmain = part ⊕ box 1 1 1\n"
  assertT t "anchor: position inside a definition other than main, reported once"
    (length elsewhere == 1 && any ("s.coscad:1:35: 'anchor bot'" `isInfixOf`) elsewhere) (show elsewhere)
  let glyph = warns "main = θ 45 (box 10 10 10) ⌖ top box 1 1 1\n"
  assertT t "anchor: the ⌖ glyph carries its position too"
    (any ("s.coscad:1:28: 'at top'" `isInfixOf`) glyph) (show glyph)
  assertT t "anchor: a part turned 90 degrees, no warning"
    (null (warns "main = box 10 20 4 |> rotz 90 |> at top 0 0 0 (box 1 1 1)\n")) ""
  assertT t "anchor: two 45 degree turns compose to a right angle, no warning"
    (null (warns "main = box 10 10 10 |> roty 45 |> roty 45 |> at top 0 0 0 (box 1 1 1)\n")) ""
  -- print counts against placements, through the CLI (warnings go to stderr)
  exe <- findExecutable "coscad"
  case exe of
    Nothing -> failT t "coscad executable on PATH" "run the suite with `cabal test`"
    Just bin -> do
      dir <- tempDir "silence"
      writeFile (dir </> "blk.coscad") "main = box 20 20 20\n"
      writeFile (dir </> "under.assemble") "blk ← blk.coscad ×3\nasm = blk ⊕ (blk |> move 30 0 0)\n"
      writeFile (dir </> "unplaced.assemble") "blk ← blk.coscad ×1\nextra ← blk.coscad ×2\nasm = blk\n"
      writeFile (dir </> "ok.assemble") "blk ← blk.coscad ×2\nnut ← blk.coscad ×0\nasm = blk ⊕ (blk |> move 30 0 0) ⊕ (nut |> move 60 0 0) ⊕ (nut |> move 90 0 0)\n"
      writeFile (dir </> "nested.assemble") "pair ← ok.assemble ×2\nasm = pair ⊕ (pair |> move 0 40 0)\n"
      let run f = readProcessWithExitCode bin [dir </> f] ""
      writeFile (dir </> "floating.coscad") "main = box 10 10 10\n  |> roty 45\n  |> at top 0 0 0 (box 1 1 1)\n"
      (cv, ov, ev) <- readProcessWithExitCode bin ["validate", dir </> "floating.coscad"] ""
      assertT t "validate: prints anchor warnings with file:line:col on stderr, exit 0 (the editor reads them)"
        (cv == ExitSuccess && "Valid part" `isInfixOf` ov && ("Warning: " ++ (dir </> "floating.coscad") ++ ":3:6: 'at top'") `isInfixOf` ev) (show (cv, ov, ev))
      (c1, _, e1) <- run "under.assemble"
      assertT t "count: ×3 placed twice warns at the declaration and still succeeds"
        (c1 == ExitSuccess && any (\l -> "under.assemble:1:1: 'blk' is declared ×3 but asm places it 2 times" `isInfixOf` l) (lines e1)) (show (c1, e1))
      (c2, _, e2) <- run "unplaced.assemble"
      assertT t "count: a printed part never placed warns"
        (c2 == ExitSuccess && "'extra' is declared ×2 but asm places it nowhere" `isInfixOf` e2) (show (c2, e2))
      (c3, _, e3) <- run "ok.assemble"
      assertT t "count: matching counts and ×0 parts placed freely, no warning"
        (c3 == ExitSuccess && not ("is declared" `isInfixOf` e3)) (show (c3, e3))
      (c4, _, e4) <- run "nested.assemble"
      assertT t "count: a subassembly counts its own placements, no warning"
        (c4 == ExitSuccess && not ("is declared" `isInfixOf` e4)) (show (c4, e4))
