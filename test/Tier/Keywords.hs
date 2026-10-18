-- | Contextual keywords (docs/SYNTAX.md "Names and keywords"): anchor,
-- pipeline-stage and statement words are keywords only in their own
-- positions, so a binding may reuse them; only primitive shape words are
-- reserved. Each case must give exactly the scad of the same program with
-- the binding renamed, so the rule cannot drift silently.
module Tier.Keywords
  ( keywordsTier
  )
where

import Control.Monad (forM_)
import Coscad.Codegen (renderScad)
import Coscad.Parser (parseProgramNamed)
import Harness

keywordsTier :: T -> IO ()
keywordsTier t = do
  section "contextual keywords"
  let scad = fmap (renderScad . snd) . parseProgramNamed "t.coscad"
      base = "base = box 10 10 1\nc = box 1 1 1\n"
      cases =
        [ ( "a shape named top after an anchor top (⌖)"
          , base ++ "top = box 2 2 2\nmain = base ⌖ top top\n"
          , base ++ "lid = box 2 2 2\nmain = base ⌖ top lid\n" )
        , ( "a number named top as the offset after anchor top (|> at)"
          , base ++ "top = 3\nmain = base |> at top top 0 0 c\n"
          , base ++ "dx = 3\nmain = base |> at top dx 0 0 c\n" )
        , ( "bindings named like stages (x, move) as their own arguments"
          , "x = 4\nmove = 2\nmain = box 1 1 1 |> x x |> move move 0 0\n"
          , "a = 4\nb = 2\nmain = box 1 1 1 |> x a |> move b 0 0\n" )
        , ( "a shape named hull as the |> hull child"
          , "hull = box 1 1 1\nmain = box 2 2 2 |> hull hull\n"
          , "h = box 1 1 1\nmain = box 2 2 2 |> hull h\n" )
        , ( "a shape named at as the |> at child"
          , "at = box 1 1 1\nmain = box 2 2 2 |> at top at\n"
          , "q = box 1 1 1\nmain = box 2 2 2 |> at top q\n" )
        , ( "a shape named center (anchor alias of ctr) with |> anchor center"
          , "center = box 1 1 1\nmain = center |> anchor center\n"
          , "middle = box 1 1 1\nmain = middle |> anchor ctr\n" )
        , ( "lowercase cone is not a shape word, so it can be a binding"
          , "cone = box 1 1 1\nmain = cone\n"
          , "k = box 1 1 1\nmain = k\n" )
        ]
  forM_ cases $ \(name, src, renamed) ->
    assertT t ("contextual keyword: " ++ name)
      (either (const False) (const True) (scad src) && scad src == scad renamed)
      (either id (const "differs from the renamed program") (scad src))
  -- the eighth case: a primitive shape word stays reserved
  expectErr t "contextual keyword: a primitive shape word (cyl) stays reserved"
    (parseProgramNamed "t.coscad" "cyl = box 1 1 1\nmain = cyl\n")
    ["t.coscad:1:", "reserved word 'cyl'", "definition name"] []
