-- | Name resolution: definitions resolve in dependency order whatever
-- their order in the file, a long chain stays fast, and the error that
-- wins (syntax, undefined, circular) is fixed.
module Tier.Resolution
  ( resolutionTier
  )
where

import Control.Exception (evaluate)
import Coscad.Codegen (renderScad)
import Coscad.Parser (parseProgramNamed)
import Data.List (isInfixOf)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Harness

resolutionTier :: T -> IO ()
resolutionTier t = do
  section "name resolution"
  let p = parseProgramNamed "t.coscad"
      scad = fmap (renderScad . snd) . p
      -- the shape itself (renderScad indents every nesting level, which
      -- is quadratic in a chain's depth)
      shape = fmap (show . snd) . p
      chainDefs n = "s0 = box 1 1 1" : ["s" ++ show i ++ " = s" ++ show (i - 1) ++ " |> move 0 0 1" | i <- [1 .. n - 1]]
      mainOf n = "main = s" ++ show (n - 1 :: Int)
      forward n = unlines (chainDefs n ++ [mainOf n])
      backward n = unlines (mainOf n : reverse (chainDefs n))
  -- performance: a 1200-definition chain used to take 6.5 s (every
  -- round re-parsed every pending definition)
  t0 <- getCurrentTime
  n1200 <- evaluate (either length length (shape (forward 1200)))
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  assertT t "a 1200-definition chain resolves in under 0.5 s" (n1200 > 0 && secs < 0.5) (show secs ++ " s")
  assertT t "definition order does not matter: a chain written backwards gives the same scad"
    (shape (forward 300) == shape (backward 300) && either (const False) (const True) (shape (forward 300))) ""
  let mixed = "main = box w d 1 ⊕ (s |> move w 0 0)\ns = ● r\nr = d / 4\nd = w * 2\nw = 3\n"
      ordered = "w = 3\nd = w * 2\nr = d / 4\ns = ● r\nmain = box w d 1 ⊕ (s |> move w 0 0)\n"
  assertT t "numbers and shapes resolve in dependency order in any file order"
    (scad mixed == scad ordered && either (const False) (const True) (scad mixed)) (either id (const "") (scad mixed))
  -- which error wins
  expectErr t "a cycle is reported with only its members, not what depends on it"
    (p "main = c\nc = a ⊕ ● 1\na = b |> move 1 0 0\nb = a |> move 0 1 0\n")
    ["circular dependency", "  a  (t.coscad:3:5)", "  b  (t.coscad:4:5)"] ["  c  (", "  main  ("]
  expectErr t "a numeric cycle is a circular dependency"
    (p "w = h + 1\nh = w * 2\nmain = box w 1 1\n") ["circular dependency", "  w  (t.coscad:1:5)", "  h  (t.coscad:2:5)"] []
  expectErr t "an undefined name outside any cycle wins over circular"
    (p "main = ghost ⊕ a\na = b |> move 1 0 0\nb = a |> move 0 1 0\n") ["t.coscad:1:8:", "undefined variable 'ghost'"] ["circular"]
  expectErr t "a syntax error wins over an undefined name"
    (p "main = ghost ⊕ ● 1\nx = ● 5 ⊕ ▼ 3\n") ["t.coscad:2:", "unexpected"] ["undefined variable 'ghost'", "circular"]
  expectErr t "a cycle member that only fails on its partner is still circular (the first missing name decides)"
    (p "main = a\na = b ⊕ ● 1\nb = a ⊕ ▼ 3\n") ["circular dependency", "  a  (t.coscad:2:5)", "  b  (t.coscad:3:5)"] []
  let shadowed = p "main = top ⊕ ● 1\ntop = box 1 1 1\n"
  assertT t "a binding may reuse an anchor word and is found wherever it is defined"
    (either (const False) (const True) shadowed) (either id (const "") shadowed)
  assertT t "the same error whichever order the cycle is written in"
    (fmap (const ()) (p "main = a\na = b\nb = a\n") /= Right () && isCircular (p "main = a\nb = a\na = b\n")) ""
  where
    isCircular r = either ("circular dependency" `isInfixOf`) (const False) r
