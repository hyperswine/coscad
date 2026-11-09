-- | The words renderer: every shape the language can spell renders to
-- word syntax and reparses to the same geometry; what the language
-- cannot spell yet (chamfered BOSL2 primitives, frames, arbitrary-axis
-- rotations) is an honest error naming the construct, never a guess.
module Tier.Words (wordsTier) where

import Data.List (isInfixOf)
import Coscad.Backend (nativeCSGMesh)
import Coscad.Geometry (PointRef (..), pointOf)
import Coscad.Mesh (meshBounds, meshVolume)
import Coscad.Parser (parseProgramFull, parseProgramNamed)
import Coscad.Words (renderWords, wordsShape)
import Harness

wordsTier :: T -> IO ()
wordsTier t = do
  section "words renderer"
  let p = parseProgramNamed "t.coscad"

      -- exact rendered text, the format's documentation
      golden name src want = case p src of
        Left e -> failT t name e
        Right (_, mainShape) -> case wordsShape mainShape of
          Left e -> failT t ("render: " ++ name) e
          Right txt -> assertT t name (txt == want) (txt ++ "\n  wanted: " ++ want)

      -- a refusal naming the construct, never a guess
      refuses name src needle = case p src of
        Left e -> failT t name e
        Right (_, mainShape) -> case wordsShape mainShape of
          Right txt -> failT t name ("expected a refusal, rendered: " ++ txt)
          Left e -> assertT t name (needle `isInfixOf` e) e

      -- round trip: the rendered text must reparse to the same geometry
      roundTrip name src = case p src of
        Left e -> failT t name e
        Right (_, mainShape) -> case wordsShape mainShape of
          Left e -> failT t ("render: " ++ name) e
          Right txt -> case p ("!simple\nmain = " ++ txt ++ "\n") of
            Left e -> failT t ("reparse: " ++ name ++ " <- " ++ txt) e
            Right (_, main2) -> sameMesh name mainShape main2 ""

  -- exact rendered text
  golden "cube glyph renders block" "main = ■ 3\n" "block 3 3 3"
  golden "cylinder glyph renders pillar" "main = ◎ 29 4\n" "pillar 29 4"
  golden "cone glyph renders spike" "main = ▻ 5 2\n" "spike 5 2"
  golden "prism glyph renders prism" "main = ⎏ 6 2 8\n" "prism 6 2 8"
  golden "box word stays box" "main = box 2 3 4\n" "box 2 3 4"
  golden "frustum stays frustum" "main = frustum 3 1 7\n" "frustum 3 1 7"
  golden "prismoid" "main = ⏢ 1 2 3 4 5\n" "Prismoid 1 2 3 4 5"
  golden "transform becomes a stage" "main = ◎ 4 2 |> move 1 2 3\n" "pillar 4 2 |> move 1 2 3"
  golden "difference is a minus chain" "main = ■ 10 ⊖ zcyl 2 20\n" "block 10 10 10 - zcyl 2 20"
  golden "pipelined union operand is parenthesised"
    "a = ⭘ 3 |> extrude 1\nmain = a ⊕ block 1 1 1 ⊕ cyl 2 2\n"
    "(Circle 3 |> extrude 1) * block 1 1 1 * cyl 2 2"
  golden "right-nested difference keeps its group"
    "main = block 4 4 4 ⊖ (zcyl 1 8 ⊕ zcyl 1 8)\n"
    "block 4 4 4 - (zcyl 1 8 * zcyl 1 8)"
  golden "loft prefix with bare circle profiles" "main = loft 0 (⭘ 10) 30 (⭘ 5)\n" "loft 0 Circle 10 30 Circle 5"
  golden "loft prefix parenthesises variadic poly profile" "main = loft 0 (poly 0 0 10 0 5 8) 30 (⭘ 5)\n" "loft 0 (poly 0 0 10 0 5 8) 30 Circle 5"
  golden "datum offsets from the centre" "main = ▬ 2 2 2 |> datum p top 0 0 1\n" "block 2 2 2 |> datum p ctr 0 0 2"
  golden "offset prefix" "main = (△ 4 ↯ ⭘ 1) |> extrude 5\n" "Offset 1 Triangle 4 |> extrude 5"

  -- honest refusals
  refuses "chamfered cuboid has no word" "main = ▣ 1 2 3 0.5\n" "cuboid"
  refuses "chamfered cyl has no word" "main = ⌭ 2 5 0.5\n" "cyl"
  refuses "mating frames have no word" "main = box 1 1 1 |> frame f ctr top bak\n" "frame"
  case renderWords "t.coscad" "!glyph\nSphere = ● 5\nmain = Sphere\n" of
    Right txt -> failT t "glyph-mode definition colliding with !simple keyword refused" txt
    Left e -> assertT t "glyph-mode definition colliding with !simple keyword refused" ("reserved word in !simple mode" `isInfixOf` e) e

  -- round trips through the kernel
  roundTrip "composite" "plate = ◎ 8 2\nmain = plate ⊕ (frustum 6 4 3 |> move 0 0 4)\n"
  roundTrip "three-way union of pipelines" "a = ⭘ 3 |> extrude 1\nmain = a ⊕ block 2 3 4 ⊕ cyl 2 2\n"
  roundTrip "right-nested group" "main = block 4 4 4 ⊖ (zcyl 1 8 ⊕ zcyl 1 8)\n"
  roundTrip "loft" "main = loft 0 (⭘ 10) 10 (△ 5)\n"
  roundTrip "loft with poly profile" "main = loft 0 (poly 0 0 10 0 5 8) 20 (⭘ 5)\n"
  -- a centred circle's on-axis chord fails partial revolve in the kernel
  -- bridge (found by this tier; see CHANGELOG) — △ has no axis edge
  roundTrip "revolve with segments" "main = △ 4 |> revolve 90 64\n"
  roundTrip "offset and extrude" "main = (△ 4 ↯ ⭘ 1) |> extrude 5\n"
  roundTrip "bezier becomes a polygon" "main = ✎ 0 0 5 3 10 0 5 -3 |> extrude 2\n"
  roundTrip "attachment sugar resolves away" "main = box 4 4 4 ⌖ top (block 1 1 1)\n"

  -- file level: definitions keep their lines, numbers stay numbers, directives survive
  let src = "sharp = △ 15\noff = ⭘ 3\nrounded = sharp ↯ off\nmain = rounded |> extrude 8\nsupport bot\nload top 0 0 -10\nstress material=pla safety=2\n"
  case parseProgramFull "t.coscad" src of
    Left e -> failT t "words file" e
    Right (_, mainShape, a1, _) -> case renderWords "t.coscad" src of
      Left e -> failT t "words file render" e
      Right txt -> do
        assertT t "definitions and directives keep their lines"
          (all (`isInfixOf` txt) ["sharp = Triangle 15", "off = Circle 3", "rounded = Offset 3 Triangle 15", "support bot", "load top 0 0 -10", "stress material=pla safety=2"]) txt
        case parseProgramFull "t.coscad" txt of
          Left e -> failT t ("words file reparse <- " ++ txt) e
          Right (_, main2, a2, _) -> do
            assertT t "words file analysis directives survive" (a1 == a2) (show (a1, a2))
            sameMesh "words file geometry survives" mainShape main2 txt
  -- a datum's stored point survives the file round trip
  case p "main = ▬ 2 2 2 |> datum p top 0 0 1\n" of
    Left e -> failT t "datum round trip" e
    Right (_, mainShape) -> case renderWords "t.coscad" "main = ▬ 2 2 2 |> datum p top 0 0 1\n" of
      Left e -> failT t "datum render" e
      Right txt -> case p txt of
        Left e -> failT t ("datum reparse <- " ++ txt) e
        Right (_, main2) -> case (,) <$> pointOf (NamedPoint "p") mainShape <*> pointOf (NamedPoint "p") main2 of
          Left e -> failT t "datum point" e
          Right ((ax, ay, az), (bx, by, bz)) ->
            assertT t "datum point survives" (all nearD [ax - bx, ay - by, az - bz]) (show (bx, by, bz))
  where
    sameMesh name m1s m2s detail = do
      m1 <- nativeCSGMesh m1s
      m2 <- nativeCSGMesh m2s
      case (,) <$> m1 <*> m2 of
        Left e -> failT t ("mesh: " ++ name) e
        Right (t1, t2) ->
          assertT t name
            (nearV (meshVolume t1) (meshVolume t2) && boundsNear (meshBounds t1) (meshBounds t2))
            (detail ++ "\n  " ++ show (meshVolume t1, meshBounds t1) ++ "\n  got: " ++ show (meshVolume t2, meshBounds t2))
    nearV a b = abs (a - b) < 1e-6 * max 1 (abs a)
    boundsNear ((a, b, c), (d, e, f)) ((a', b', c'), (d', e', f')) =
      all nearD [a - a', b - b', c - c', d - d', e - e', f - f']
    nearD x = abs x < 1e-6
