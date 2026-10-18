-- | Diagnostics tier (pure): error messages carry file:line:col and say
-- what is wrong.
module Tier.Diagnostics
  ( diagnostics
  , remainingDiagnostics
  )
where

import Control.Monad (forM_)
import Coscad.Assemble (AsmResult (..), loadAssembleFile)
import Coscad.Variants (packBeds)
import Coscad.Codegen (renderScad, showD)
import Coscad.Geometry (bbox, resolve)
import Coscad.Parser (parseProgramNamed)
import Coscad.Plan (PlanSummary (..), StepSummary (..), planSummary)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import System.FilePath
import Harness

diagnostics :: T -> IO ()
diagnostics t = do
  section "diagnostics"
  let p = parseProgramNamed "t.coscad"
  expectErr t "undefined variable: position + name"
    (p "main = foo ⊕ ● 5") ["t.coscad:1:8:", "undefined variable 'foo'"] ["circular"]
  expectErr t "syntax error: position, not 'circular'"
    (p "main = ● 5 ⊕ ▼ 3") ["t.coscad:1:14:", "unexpected", "a transform", "a shape"] ["circular", "expecting \"Anchor\""]
  expectErr t "error on a |> continuation line reports that line"
    (p "main = box 2 2 2\n  |> at top ▼\n") ["t.coscad:2:13:"] ["circular"]
  expectErr t "unclosed paren: end of input reported on the expression's last line"
    (p "main = plate\n  |> cutat top (zcyl 1 50\nplate = box 20 20 4\n") ["t.coscad:2:26:", "end of input", "an operator"] ["<empty line>", "circular"]
  expectErr t "circular dependency lists only the cycle"
    (p "a = b ⊕ ● 1\nb = a ⊕ ● 2\nmain = a\n")
    ["circular", "a  (t.coscad:1:5)", "b  (t.coscad:2:5)"] ["main  ("]
  expectErr t "duplicate definition"
    (p "a = ● 1\na = ● 2\nmain = a\n") ["t.coscad:2:5", "duplicate definition of 'a'", "t.coscad:1:5"] []
  expectErr t "missing main"
    (p "a = ● 5\n") ["no 'main'"] ["Error: Error"]
  expectErr t "dimension: extrude a solid"
    (p "main = ⮕ 2 (● 1)") ["t.coscad:1:8", "in 'main'", "extrude", "3D"] []
  expectErr t "dimension: offset a solid"
    (p "main = ■ 10 ↯ ● 2") ["offset", "3D"] []
  expectErr t "dimension: union of profile and solid"
    (p "main = ⭘ 3 ⊕ ■ 2") ["2D profile with a 3D"] []
  expectErr t "dimension: main is 2D"
    (p "main = △ 8") ["'main' is a 2D profile", "t.coscad:1:8"] []
  expectErr t "error inside a helper names the helper"
    (p "hole = ⮕ 2 (● 1)\nmain = ■ 10 ⊖ hole\n") ["t.coscad:1:8", "in 'hole'"] ["Undefined", "circular"]
  -- numeric bindings
  expectErr t "number used as a shape"
    (p "r = 5\nmain = r ⊕ ● 1\n") ["t.coscad:2:8", "'r' is a number, not a shape"] ["circular", "undefined"]
  expectErr t "main aliased to a number"
    (p "r = 5\nmain = r\n") ["'main' is a number"] []
  expectErr t "shape used as a number"
    (p "plate = box 1 1 1\nmain = ● plate\n") ["t.coscad:2:10", "'plate' is a shape, not a number"] ["circular"]
  expectErr t "main defined as a number"
    (p "main = 5\n") ["'main' is a number"] []
  expectErr t "undefined name in arithmetic"
    (p "r = q * 2\nmain = ● r\n") ["undefined variable 'q'"] ["circular"]
  expectGen t "numeric binding as argument" (p "r = 5\nmain = ● r\n") "sphere(5);"
  expectGen t "arithmetic in parens, forward numeric reference"
    (p "main = box (w / 2) w (w - 2 * 3)\nw = 20\n") "cuboid([10, 20, 14]);"
  expectGen t "numbers depend on numbers, negated name"
    (p "a = b * 2 + 1\nb = 3\nmain = χ -a (● 1)\n") "translate([-7, 0, 0])"
  expectGen t "numeric offset in a pipeline stage (identity translate elided)"
    (p "t = 4\nmain = box 20 20 t |> cutat top 0 0 (-t / 2) (zcyl 1 50)\n") "difference() {\n  cuboid([20, 20, 4]);\n  zcyl(r = 1, l = 50);"
  -- front-end review: reject silent reinterpretation without changing arithmetic
  expectErr t "spaced minus in primitive arguments"
    (p "w = 10\nmain = box w - 2 3\n") ["t.coscad:2:14:", "negative argument", "(w - 2)"] ["circular"]
  expectErr t "spaced minus in pipeline arguments"
    (p "main = box 1 1 1\n  |> move - 2 0 0\n") ["t.coscad:2:11:", "negative argument"] []
  expectErr t "tab after unary minus"
    (p "main = χ -\t2 (sphere 1)\n") ["negative argument"] []
  expectGen t "tight negation and spaced arithmetic remain valid"
    (p "w = 10\nk = - 2\nmain = box (w - 2) 3 4 |> move -w k -(w / 2)\n")
    "translate([-10, -2, -5])"
  expectGen t "compact glyph and parenthesized argument"
    (p "main = ●15 ⊕ ●(2 + 3)\n") "sphere(15);"
  forM_ [("division by zero", "10 / 0"), ("NaN", "0 / 0"), ("overflow", "1e308 * 10")] $ \(label, rhs) -> do
    expectErr t ("non-finite numeric binding: " ++ label)
      (p ("r = " ++ rhs ++ "\nmain = sphere r\n")) ["t.coscad:1:5", "in 'r'", "numeric binding must be finite"] ["circular", "undefined"]
    expectErr t ("non-finite argument: " ++ label)
      (p ("main = sphere (" ++ rhs ++ ")\n")) ["t.coscad:1:15", "numeric argument must be finite"] []
  expectErr t "non-finite forward dependency names the originating binding"
    (p "a = b * 2\nb = 1 / 0\nmain = sphere a\n") ["in 'b'", "must be finite"] ["circular"]
  expectErr t "unused non-finite binding is still an error"
    (p "bad = 0 / 0\nmain = sphere 1\n") ["in 'bad'", "must be finite"] []
  expectErr t "overflowing literal before prism side-count conversion"
    (p "main = ⎏ 1e999 2 3\n") ["must be finite"] []
  forM_ ["lft+rt", "top+top", "top+up", "ctr+top", "center+ctr"] $ \anchor ->
    expectErr t ("invalid anchor: " ++ anchor)
      (p ("main = box 10 10 10 |> at " ++ anchor ++ " (box 1 1 1)\n"))
      ["t.coscad:1:27:", "invalid anchor combination", "one direction per axis"] []
  expectGen t "three-axis anchor with aliases"
    (p "main = box 10 10 10 |> at up+right+front (box 2 2 2)\n") "translate([6, -6, 6])"
  expectGen t "center remains a valid anchor"
    (p "main = box 10 10 10 |> at ctr (box 2 2 2)\n") "cuboid([2, 2, 2]);"
  forM_ ["cube", "sphere", "xcyl", "loft", "Box", "Translate", "Offset"] $ \name ->
    expectErr t ("reserved definition: " ++ name)
      (p (name ++ " = 2\nmain = sphere 1\n")) ["t.coscad:1:", "reserved word '" ++ name ++ "'", "definition name"] []
  expectGen t "stage, anchor, and keyword-prefix bindings remain usable"
    (p "x = 2\ntop = 3\ncube2 = 4\nmain = box x top cube2\n") "cuboid([2, 3, 4]);"
  expectGen t "inactive simple keyword can be a glyph-mode binding"
    (p "!glyph\nBox = 2\nmain = sphere Box\n") "sphere(2);"
  expectErr t "simple-mode definition keyword is reserved"
    (p "!simple\nBox = 2\nmain = Sphere 1\n") ["t.coscad:2:", "reserved word 'Box'"] []
  expectErr t "glyph-mode common shape keyword is reserved"
    (p "!glyph\nsphere = 2\nmain = ● 1\n") ["reserved word 'sphere'"] []
  expectErr t "non-finite transform tuple argument"
    (p "!simple\nmain = Translate (0, (1 / 0), 0) (Sphere 1)\n") ["t.coscad:2:22", "numeric argument must be finite"] []
  -- lofts
  expectGen t "loft prefix form emits skin"
    (p "main = loft 0 (⭘ 10) 30 (⭘ 5)\n") "skin([circle(r = 10, $fn = 100), circle(r = 5, $fn = 100)], z = [0, 30], slices = 0, method = \"reindex\");"
  expectGen t "loft pipeline form starts at z = 0 and chains"
    (p "main = ⭘ 10 |> loft 20 (△ 5) |> loft 35 (⬠ 3)\n") "z = [0, 20, 35]"
  expectGen t "loft profile transforms become path functions"
    (p "main = loft 0 (χ 2 (ω 30 (⬈ 1 0.5 1 (⭘ 6)))) 10 (△ 4 ↯ ⭘ 1)\n")
    "move([2, 0], p = zrot(30, p = scale([1, 0.5], p = circle(r = 6, $fn = 100)))), offset(circle(r = 4, $fn = 3), r = 1, closed = true)"
  expectGen t "loft with mismatched vertex counts uses method distance"
    (p "main = loft 0 (⭘ 10) 30 (△ 5)\n") "method = \"distance\""
  expectGen t "loft with an offset profile (unknown count) uses method distance"
    (p "main = loft 0 (⭘ 10) 30 (⭘ 10 ↯ ⭘ 1)\n") "method = \"distance\""
  expectGen t "loft emits the BOSL2 include" (p "main = ⟰ 0 (⭘ 1) 1 (⭘ 2)\n") "include <BOSL2/std.scad>"
  expectErr t "loft of a solid"
    (p "main = loft 0 (● 3) 10 (⭘ 5)\n") ["in 'main'", "loft profiles must be 2D"] []
  expectErr t "loft with a boolean profile"
    (p "main = loft 0 (⭘ 3 ⊕ χ 5 (⭘ 3)) 10 (⭘ 5)\n") ["in loft profile", "single closed outline"] []
  expectErr t "loft with one profile"
    (p "main = loft 0 (⭘ 3)\n") ["at least two profiles"] []
  expectErr t "loft profile rotated out of plane"
    (p "main = loft 0 (θ 90 (⭘ 3)) 10 (⭘ 5)\n") ["XY plane"] []
  expectErr t "loft rejects vector Z translation"
    (p "main = loft 0 (⭘ 3 |> move 0 0 7) 10 (⭘ 5)\n") ["t.coscad:1:8", "cannot be moved in Z"] []
  expectErr t "loft rejects simple vector Z translation"
    (p "!simple\nmain = Loft 0 (Translate (0, 0, 7) (Circle 3)) 10 (Circle 5)\n") ["cannot be moved in Z"] []
  expectErr t "loft rejects oblique reflection"
    (p "main = loft 0 (⭘ 3 |> mirror 1 0 1) 10 (⭘ 5)\n") ["XY plane"] []
  expectErr t "loft rejects zero mirror normal"
    (p "main = loft 0 (⭘ 3 |> mirror 0 0 0) 10 (⭘ 5)\n") ["nonzero normal"] []
  expectGen t "loft preserves XY translation"
    (p "main = loft 0 (⭘ 3 |> move 2 4 0) 10 (⭘ 5)\n") "move([2, 4], p = circle(r = 3"
  expectGen t "loft preserves XY reflection"
    (p "main = loft 0 (△ 3 |> mirror 1 0 0) 10 (⭘ 5)\n") "mirror([1, 0], p = circle(r = 3"
  expectGen t "loft reflection in XY is identity"
    (p "main = loft 0 (△ 3 |> mirror 0 0 1) 10 (⭘ 5)\n") "skin([circle(r = 3, $fn = 3), circle(r = 5"
  -- success paths
  case p "main = box 2 2 2 // base\n  |> at top (box 1 1 1)\n" of
    Left e -> failT t "trailing comment does not swallow continuation lines" e
    Right (_, m) ->
      let (_, (_, _, zmax)) = bbox (resolve m)
       in assertT t "trailing comment does not swallow continuation lines" (abs (zmax - 2) < 1e-9) ("zmax = " ++ show zmax)
  case p "main = χ 5 $ ● 3 ⊕ ● 3" of
    Left e -> failT t "$ application" e
    Right (_, m) -> assertT t "$ application" ("translate([5, 0, 0]) {\n  union()" `isInfixOf` renderScad m) (renderScad m)
  case p "  main = ● 5\n" of
    Left e -> failT t "indented definition" e
    Right _ -> pass t
  -- number formatting in emitted .scad
  assertT t "showD: rotation noise, integers, -0, decimals"
    (map showD [3.061616997868383e-16, 10, -0.0, 2.5, -7, 0.1] == ["0", "10", "0", "2.5", "-7", "0.1"])
    (show (map showD [3.061616997868383e-16, 10, -0.0, 2.5, -7, 0.1]))
  -- packing: seven 43x43 brackets on a 120x120 plate with 6 mm margins = 4 + 3
  case packBeds (120, 120, 6) [("br_" ++ show i, ("b", "top"), (43, 42.98)) | i <- [1 .. 7 :: Int]] of
    Left e -> failT t "packBeds spills to a second plate" e
    Right plates -> assertT t "packBeds spills to a second plate" (map length plates == [4, 3]) (show (map length plates))
  case packBeds (100, 100, 6) [("big", ("b", "top"), (200, 10))] of
    Left e -> assertT t "packBeds rejects an oversized footprint with a clear message" ("exceeds the 100.0x100.0 plate" `isInfixOf` e) e
    Right _ -> failT t "packBeds rejects an oversized footprint with a clear message" "packed"
  -- the manual must mention every command, flag, and environment variable
  manPage <- readFile "man/coscad.1"
  let manMissing = [w | w <- ["Cm stl", "Cm next", "Cm check", "Cm doctor", "Cm plan", "Cm site", "keep-temp", "Fl -png", "Fl -strict", "Fl -version", "Fl -help", "COSCAD_OPENSCAD", "COSCAD_BOSL2", "loft", "cutat", "fastener", "Fl o"], not (w `isInfixOf` manPage)]
  assertT t "man/coscad.1 documents every command, flag, and env var" (null manMissing) (show manMissing)
  expectErr t "empty definition cannot swallow the next definition"
    (parseProgramNamed "empty.coscad" "a =\nmain = ■ 10\n")
    ["empty.coscad:1:4:", "missing expression after '=' in 'a'"] ["undefined", "circular"]
  expectErr t "comment after empty definition names the missing expression"
    (parseProgramNamed "empty.coscad" "a = // todo\nmain = ■ 10\n")
    ["missing expression after '=' in 'a'"] ["undefined"]
  -- .assemble diagnostics
  tmp <- tempDir "asm-diag"
  let asm1 = tmp </> "d1.assemble"
      asm2 = tmp </> "d2.assemble"
  writeFile asm1 "p ← x.coscad ×2 ▽ nowhere\nasm = p\n"
  r1 <- loadAssembleFile [] asm1
  expectErr t "assemble: bad part option at its real position" r1 ["d1.assemble:1:19:"] []
  writeFile asm2 "asm = q ⊕ ● 1\n"
  r2 <- loadAssembleFile [] asm2
  expectErr t "assemble: undefined part name" r2 ["d2.assemble:1:7:", "undefined variable 'q'"] ["circular"]
  writeFile asm1 "p ← x.coscad ×2 ▽top+up\nasm = p\n"
  r3 <- loadAssembleFile [] asm1
  expectErr t "assemble: invalid print-orientation anchor at declaration" r3
    ["d1.assemble:1:18:", "invalid anchor combination"] []
  writeFile asm2 "cube = box 1 1 1\nasm = cube\n"
  r4 <- loadAssembleFile [] asm2
  expectErr t "assemble: reserved helper name" r4 ["reserved word 'cube'"] []
  writeFile (tmp </> "part.coscad") "main = box 10 10 10\n"
  let twoParts = "a ← part.coscad\nb ← part.coscad\nasm = a ⌖ top b\n"
      badPlanOpts =
        [ ("beam=0", "positive whole number")
        , ("beam=-2", "positive whole number")
        , ("beam=1.5", "positive whole number")
        , ("beam=oops", "positive whole number")
        , ("beam=NaN", "positive whole number")
        , ("beam=Infinity", "positive whole number")
        , ("beam=1e100", "positive whole number")
        , ("astar=-1", "positive whole number")
        , ("astar=NaN", "positive whole number")
        , ("rest=bot,tpo", "unknown rest face 'tpo'")
        , ("rest=ctr", "unknown rest face 'ctr'")
        , ("flip=-1", "non-negative cost weight")
        , ("flip=NaN", "finite, non-negative")
        , ("flip=oops", "finite, non-negative")
        , ("beem=30", "unknown plan option 'beem'")
        , ("×3", "plan takes key=value")
        , ("beam=10 beam=20", "duplicate plan option 'beam'")
        ]
  forM_ badPlanOpts $ \(option, msg) -> do
    writeFile asm1 ("plan " ++ option ++ "\n" ++ twoParts)
    r <- loadAssembleFile [] asm1
    expectErr t ("assemble: rejects plan " ++ option) r ["d1.assemble:1:", msg] []
  forM_ ["a ← part.coscad ×18446744073709551616\nasm = a\n", twoParts ++ "fastener peg b a top ×18446744073709551616\n"] $ \src -> do
    writeFile asm1 src
    r <- loadAssembleFile [] asm1
    expectErr t "assemble: oversized count cannot wrap to zero" r ["count is too large", "maximum is"] []
  writeFile asm1 ("a ← part.coscad ×0 seam=rear custom=retained\nplan beam=1 astar=30 rest=bottom\nasm = a\n")
  r5 <- loadAssembleFile [] asm1
  -- `bottom` is intentionally not an anchor alias; check the documented one.
  expectErr t "assemble: unsupported rest alias is explicit" r5 ["unknown rest face 'bottom'"] []
  writeFile asm1 ("a ← part.coscad ×0 seam=rear custom=retained\nplan beam=1 astar=30 rest=down,up flip=0 vertical=.5\nasm = a\n")
  r6 <- loadAssembleFile [] asm1
  assertT t "assemble: valid controls, premade count and free part hints survive"
    (either (const False) (\ar -> isJust (arAsm ar) && lookup "a" (arPartMeta ar) == Just (0, [("seam", "rear"), ("custom", "retained")])) r6) (either id (const "valid") r6)
  writeFile asm1 ("a ← part.coscad\nplan\nasm = a\n")
  r7 <- loadAssembleFile [] asm1
  assertT t "assemble: empty plan line cannot consume the next definition"
    (either (const False) (\ar -> isJust (arAsm ar) && null (arPlanOpts ar)) r7) (either id (const "valid") r7)

-- Regression cases from the malformed-input audit: assert cause/location and
-- retain valid boundary forms, rather than merely asserting a nonzero CLI exit.
remainingDiagnostics :: T -> IO ()
remainingDiagnostics t = do
  section "remaining audit diagnostics"
  let p = parseProgramNamed "audit.coscad"
      invalidShapes =
        [ ("sphere -1", "sphere radius")
        , ("box 0 1 1", "box width, depth and height")
        , ("box -1 1 1", "greater than zero")
        , ("tube 3 5 10", "inner radius must be smaller")
        , ("tube 5 -1 10", "inner radius")
        , ("◙ 10 10 1 5", "rounding is too large")
        , ("▣ 10 10 1 5", "chamfer is too large")
        , ("⌽ 2 4 3", "rounding is too large")
        , ("⌭ 2 4 3", "chamfer is too large")
        , ("⎏ 2 5 10", "side count must be a whole number")
        , ("⎏ 3.5 5 10", "side count must be a whole number")
        , ("⎏ 1e100 5 10", "4096")
        , ("■ 10 |> scale 0 1 1", "scale factors must be nonzero")
        , ("■ 10 |> mirror 0 0 0", "nonzero normal")
        , ("⮕ 0 (⭘ 5)", "extrusion height")
        , ("loft 0 (⭘ 5) 0 (⭘ 3)", "strictly increasing")
        , ("loft 10 (⭘ 5) 0 (⭘ 3)", "strictly increasing")
        , ("box 10 20", "box is missing or has an invalid height argument")
        , ("■ 10 |> mov 1 2 3", "did you mean 'move'?")
        , ("■ 10 |> at top 1 2 (● 3)", "third offset z")
        , ("χ −2 (■ 5)", "Unicode minus U+2212")
        , ("cuube 10", "did you mean 'cube'?")
        , ("■ 1 |> scale 1e308 1e308 1e308 |> scale 10 10 10", "bounds overflowed")
        ]
  forM_ invalidShapes $ \(expr, msg) ->
    expectErr t ("audit: " ++ expr) (p ("main = " ++ expr ++ "\n")) ["audit.coscad:1:", msg] ["circular"]
  forM_ [("!simpel\nmain = cube 1\n", "unknown syntax mode"), ("!simple\n!glyph\nmain = cube 1\n", "only one syntax mode")] $ \(src, msg) ->
    expectErr t "audit: invalid mode directive" (p src) ["audit.coscad:", msg] []
  forM_ ["tube 5 0 10", "⏢ 10 10 0 0 5", "◙ 10 10 2 1", "■ 10 |> scale -1 1 1", "■ 10 |> mirror 1e200 1 0", "⎏ 4096 5 10"] $ \expr ->
    assertT t ("audit: valid boundary " ++ expr) (either (const False) (const True) (p ("main = " ++ expr ++ "\n"))) "unexpected rejection"
  expectGen t "audit: zero Z scale on a 2D profile is harmless"
    (p "main = ⮕ 2 (⭘ 5 |> scale 1 1 0)\n") "scale([1, 1, 0])"
  expectGen t "audit: pragma with trailing comment"
    (p "!simple // ASCII mode\nmain = Box 10 20 5\n") "cuboid([10, 20, 5]);"
  assertT t "audit: finite large number emission cannot overflow its formatter"
    (showD 1e308 == "1.0e308") (showD 1e308)
  expectErr t "audit: invalid empty plate is rejected before packing"
    (packBeds (100, 100, -1) []) ["plate dimensions", "margin"] []
  expectErr t "audit: infinite plate is rejected before packing"
    (packBeds (1 / 0, 100, 6) []) ["plate dimensions", "finite"] []
  expectErr t "audit: invalid footprint cannot stall packing"
    (packBeds (100, 100, 6) [("bad", ("b", "top"), (0 / 0, 10))]) ["bad footprint must be finite and positive"] []
  tmp <- tempDir "audit"
  writeFile (tmp </> "part.coscad") "main = box 10 10 10\n"
  let file = tmp </> "bad.assemble"
      base = "a ← part.coscad\nb ← part.coscad\nasm = a ⌖ top b\n"
      badAssemblies =
        [ (base ++ "fastener M0x0 b a top\n", "diameter and length must be positive")
        , (base ++ "fastener peg b a top ×0\n", "count must be at least 1")
        , (base ++ "fastener peg b a top at=oops\n", "at=oops: expected a finite")
        , (base ++ "fastener peg b a top at=NaN\n", "at=NaN: expected a finite")
        , (base ++ "fastener peg b a top inset=NaN\n", "inset= needs a distance")
        , (base ++ "fastener M4x20 b a top torque=oops\n", "torque=oops: expected a finite")
        , (base ++ "fastener M4x20 b a top torque=-2\n", "torque=-2: expected a finite")
        , (base ++ "fastener M4x20 b a top through=oops\n", "through=oops: expected a finite")
        , (base ++ "fastener M4x20 b a top nut=drpin\n", "did you mean 'dropin'?")
        , (base ++ "fastener M4x20 b a top head=buton\n", "did you mean 'button'?")
        , (base ++ "fastener M4x20 b a top nutt=hex\n", "unknown fastener option 'nutt'")
        , (base ++ "fastener M4x20 b a top head=button head=socket\n", "duplicate fastener option")
        , ("a ← part.coscad\na ← part.coscad\nasm = a\n", "duplicate part declaration 'a'")
        , ("cube ← part.coscad\nasm = cube\n", "reserved word 'cube' cannot be a part name")
        , ("plan beam=10\n" ++ base ++ "plan beam=20\n", "duplicate plan option 'beam'")
        , ("plate 1e999 220 6\n" ++ base, "plate width must be a finite")
        , ("plate 220 220 -6\n" ++ base, "plate margin must be a finite")
        , ("plate 10 10 5\n" ++ base, "plate margin must leave usable space")
        , ("clearance -1\n" ++ base, "clearance must be a finite")
        , ("clearance 1e999\n" ++ base, "clearance must be a finite")
        , ("a ← part.coscad mass=NaN\nasm = a\n", "mass=NaN: expected a finite")
        , ("a ← part.coscad mass=-1\nasm = a\n", "mass=-1: expected a finite")
        , ("a ← part.coscad material=stel\nasm = a\n", "unknown material 'stel'")
        , ("a ← part.coscad profile=202O\nasm = a\n", "unknown profile '202O'")
        , ("a ← part.coscad ends=blockd,open\nasm = a\n", "unknown rail end 'blockd'")
        , ("a ← part.coscad mass=1 mass=2\nasm = a\n", "duplicate part option 'mass'")
        , ("a ← part.coscad seam=\nasm = a\n", "missing value after 'seam='")
        , ("a ← part.coscad ×10001\nasm = a\n", "count is too large")
        , ("a ← part.coscad ×6000\nb ← part.coscad ×6000\nasm = a ⌖ top b\n", "total expanded print count is too large")
        , ("fastener peg b a\n" ++ base, "host face")
        ]
  forM_ badAssemblies $ \(src, msg) -> do
    writeFile file src
    result <- if "M0x0" `isInfixOf` src then fmap (fmap (const ())) (planSummary file) else fmap (fmap (const ())) (loadAssembleFile [] file)
    expectErr t ("audit assembly: " ++ msg) result [msg] []
  writeFile file "a ← part.coscad\na ← part.coscad\nasm = a\n"
  dup <- loadAssembleFile [] file
  expectErr t "audit: duplicate part names both source locations" dup ["bad.assemble:2:1:", "first declared at", "bad.assemble:1:1"] []
  writeFile (tmp </> "child.assemble") "a ← part.coscad ×10000\nasm = a\n"
  writeFile file "child ← child.assemble ×10000\nasm = child\n"
  overflow <- loadAssembleFile [] file
  expectErr t "audit: recursive counts validated before multiplication" overflow ["expanded print count", "too large"] []
  writeFile file "a ← part.coscad ×0 material=PLA seam=rear custom=retained\nasm = a\n"
  control <- planSummary file
  assertT t "audit: premade PLA and free part metadata survive" (either (const False) (const True) control) (either id show control)
  writeFile file (base ++ "fastener M4x20 b a top head=SOCKET nut=NONE through=0 torque=1\n")
  screw <- planSummary file
  assertT t "audit: validated enum values are case insensitive" (either (const False) (const True) screw) (either id show screw)
  writeFile file (base ++ "fastener peg b a top ×101\nfastener peg b a top\n")
  manyF <- planSummary file
  case manyF of
    Left e -> failT t "audit: expanded fastener IDs stay unique" e
    Right ps -> assertT t "audit: expanded fastener IDs stay unique"
      (psFastenerCount ps == 102 && sum (map (length . ssFasteners) (psSteps ps)) == 102) (show ps)

-- ------------------------------------------------------------------
-- 2. examples + snapshots
