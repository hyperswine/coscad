-- | CoScad regression suite. Three tiers:
--
--   1. diagnostics   pure: error messages carry file:line:col and say
--                    what is wrong (syntax, undefined, circular,
--                    duplicate, 2D/3D mismatch)
--   2. examples      pure: every example compiles, every .assemble
--                    loads, and the emitted .scad matches the snapshot
--                    under test/golden/scad/
--   3. geometry      needs OpenSCAD: every example renders, and its
--                    mesh volume + bounds match test/golden/geometry.txt;
--                    plus a few cross-checks (tbracket == bracket,
--                    `coscad next` conserves volume, `coscad check`
--                    passes on bow3). Skipped when OpenSCAD is not
--                    found or COSCAD_RENDER=0.
--
-- COSCAD_UPDATE_GOLDEN=1 rewrites the snapshots from the current output
-- (review the diff before committing). Missing snapshots are created
-- on first run.
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless, when)
import Coscad.Assemble (loadAssembleFile, packBeds, processAssemble)
import Coscad.Check (processCheckWith, splitBodies)
import Coscad.Codegen (renderScad, showD)
import Coscad.Geometry (bbox, resolve)
import Coscad.Mesh (Tri, meshBounds, meshVolume, parseStlAscii)
import Coscad.Next (processNext)
import Coscad.OpenScad (bosl2Version, findOpenscad, openscadVersion, renderStlFile)
import Coscad.Parser (parseProgramNamed)
import Coscad.Part (renderPart)
import Coscad.Shape (Shape)
import Data.IORef
import Data.List (isInfixOf, isPrefixOf, sort)
import qualified Data.Map as Map
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.IO.Encoding (setLocaleEncoding)
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath
import System.IO
import Text.Printf (printf)

-- ------------------------------------------------------------------
-- tiny harness

data T = T {tPassed :: IORef Int, tFailed :: IORef [String]}

pass :: T -> IO ()
pass t = modifyIORef' (tPassed t) (+ 1)

failT :: T -> String -> String -> IO ()
failT t name detail = do
  putStrLn ("FAIL " ++ name)
  putStrLn (unlines (map ("     " ++) (lines detail)))
  modifyIORef' (tFailed t) (++ [name])

assertT :: T -> String -> Bool -> String -> IO ()
assertT t name ok detail = if ok then pass t else failT t name detail

-- | Expect a Left whose message contains every `yes` and no `no`.
expectErr :: T -> String -> Either String a -> [String] -> [String] -> IO ()
expectErr t name r yes no = case r of
  Right _ -> failT t name "expected an error, but it parsed"
  Left msg ->
    let missing = [y | y <- yes, not (y `isInfixOf` msg)]
        present = [n | n <- no, n `isInfixOf` msg]
     in assertT t name (null missing && null present) $
          "message:\n" ++ msg ++ "\nmissing: " ++ show missing ++ "\nunwanted: " ++ show present

-- | Expect a Right whose rendered .scad contains the fragment.
expectGen :: T -> String -> Either String (a, Shape) -> String -> IO ()
expectGen t name r frag = case r of
  Left e -> failT t name e
  Right (_, m) -> assertT t name (frag `isInfixOf` renderScad m) ("wanted fragment: " ++ frag ++ "\n" ++ renderScad m)

section :: String -> IO ()
section s = putStrLn ("\n== " ++ s)

-- ------------------------------------------------------------------
-- 1. diagnostics

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

-- ------------------------------------------------------------------
-- 2. examples + snapshots

exampleFiles :: String -> IO [FilePath]
exampleFiles ext = do
  fs <- concat <$> mapM walk ["examples", "examples-next"]
  return (sort [normKey f | f <- fs, takeExtension f == ext, not ("examples/archive/" `isPrefixOf` normKey f)])
  where
    walk d = do
      es <- listDirectory d
      concat <$> forM es (\e -> do
        let f = d </> e
        isD <- doesDirectoryExist f
        if isD then walk f else return [f])

-- | Golden keys always use forward slashes so the files are shared across OSes.
normKey :: FilePath -> FilePath
normKey = map (\c -> if c == '\\' then '/' else c)

goldenScad :: FilePath -> FilePath
goldenScad f = "test/golden/scad" </> replaceExtension f ".scad"

examples :: T -> Bool -> IO ()
examples t update = do
  section "examples compile + .scad snapshots"
  fs <- exampleFiles ".coscad"
  forM_ fs $ \f -> do
    src <- readFile f
    case parseProgramNamed f src of
      Left e -> failT t ("compile " ++ f) e
      Right (_, m) -> do
        let out = renderScad m
            g = goldenScad f
        exists <- doesFileExist g
        if update || not exists
          then do
            createDirectoryIfMissing True (takeDirectory g)
            writeFile g out
            putStrLn ((if exists then "updated " else "created ") ++ g)
            pass t
          else do
            want <- readFile g
            assertT t ("snapshot " ++ f) (want == out) (firstDiff want out ++ "\n(COSCAD_UPDATE_GOLDEN=1 stack test to accept)")
  section "assemblies load"
  as <- exampleFiles ".assemble"
  forM_ as $ \f -> do
    r <- loadAssembleFile [] f
    case r of
      Left e -> failT t ("load " ++ f) e
      Right _ -> pass t

firstDiff :: String -> String -> String
firstDiff a b =
  case [(i, x, y) | (i, x, y) <- zip3 [(1 :: Int) ..] (lines a ++ repeat "<eof>") (lines b ++ repeat "<eof>"), x /= y] of
    ((i, x, y) : _) -> "first difference at line " ++ show i ++ "\n  golden: " ++ x ++ "\n  now:    " ++ y
    [] -> "(no line differs; whitespace/eof?)"

-- ------------------------------------------------------------------
-- 3. geometry (OpenSCAD)

type Geo = (Double, (Double, Double, Double), (Double, Double, Double))

renderMesh :: FilePath -> FilePath -> String -> IO (Either String [Tri])
renderMesh _ dir scadText = do
  let scadF = dir </> "part.scad"
      stlF = dir </> "part.stl"
  writeFile scadF scadText
  r <- renderStlFile scadF stlF
  removePathForcibly stlF
  return r

geoOf :: [Tri] -> Geo
geoOf tris = (meshVolume tris, lo, hi)
  where
    (lo, hi) = meshBounds tris

showGeo :: Geo -> String
showGeo (v, (a, b, c), (d, e, f)) = unwords (map (printf "%.3f") [v, a, b, c, d, e, f])

readGeo :: [String] -> Maybe Geo
readGeo [v, a, b, c, d, e, f] = Just (read v, (read a, read b, read c), (read d, read e, read f))
readGeo _ = Nothing

geoClose :: Geo -> Geo -> Bool
geoClose (v1, lo1, hi1) (v2, lo2, hi2) =
  abs (v1 - v2) <= max 0.01 (1e-3 * abs v2) && cl lo1 lo2 && cl hi1 hi2
  where
    cl (a, b, c) (x, y, z) = all (\(p, q) -> abs (p - q) <= 0.01) [(a, x), (b, y), (c, z)]

geometry :: T -> Bool -> FilePath -> IO ()
geometry t update bin = do
  section ("geometry via " ++ bin)
  t0 <- getCurrentTime
  let goldenF = "test/golden/geometry.txt"
  haveG <- doesFileExist goldenF
  golden <- if haveG then Map.fromList . concatMap parseLine . lines <$> readFile goldenF else return Map.empty
  dir <- tempDir "geo"
  fs <- exampleFiles ".coscad"
  rows <- forM fs $ \f -> do
    src <- readFile f
    case parseProgramNamed f src of
      Left e -> failT t ("render " ++ f) e >> return Nothing
      Right (_, m) -> do
        r <- renderMesh bin dir (renderScad m)
        case r of
          Left e -> failT t ("render " ++ f) e >> return Nothing
          Right tris -> do
            let g = geoOf tris
            case Map.lookup f golden of
              Just want | not update ->
                assertT t ("geometry " ++ f) (geoClose want g)
                  ("volume/bounds drifted\n  golden: " ++ showGeo want ++ "\n  now:    " ++ showGeo g ++ "\n(COSCAD_UPDATE_GOLDEN=1 stack test to accept)")
              _ -> putStrLn ("recorded " ++ f) >> pass t
            return (Just (f, g))
  let table = Map.fromList [x | Just x <- rows]
      merged = if update then Map.union table golden else Map.union golden table
  when (update || Map.size merged /= Map.size golden) $
    writeFile goldenF (unlines ["# path volume xmin ymin zmin xmax ymax zmax (regenerate: COSCAD_UPDATE_GOLDEN=1 stack test)"] ++ unlines [f ++ " " ++ showGeo g | (f, g) <- Map.toList merged])
  -- cross-check: the topological bracket is the coordinate bracket
  case (Map.lookup "examples/bracket.coscad" table, Map.lookup "examples/topological/tbracket.coscad" table) of
    (Just (v1, lo1, hi1), Just (v2, lo2, hi2)) ->
      let ext (a, b, c) (x, y, z) = (x - a, y - b, z - c)
          sameExt (a, b, c) (x, y, z) = abs (a - x) < 0.01 && abs (b - y) < 0.01 && abs (c - z) < 0.01
       in assertT t "tbracket is the same solid as bracket" (abs (v1 - v2) < 0.5 && sameExt (ext lo1 hi1) (ext lo2 hi2))
            (printf "bracket: vol %.3f ext %s\ntbracket: vol %.3f ext %s" v1 (show (ext lo1 hi1)) v2 (show (ext lo2 hi2)))
    _ -> failT t "tbracket is the same solid as bracket" "one of the two did not render"
  -- cross-check: a circle-to-circle loft is the frustum of the two 100-gons
  case parseProgramNamed "loft.coscad" "main = loft 0 (⭘ 10) 30 (⭘ 5)\n" of
    Left e -> failT t "loft frustum volume" e
    Right (_, m) -> do
      r <- renderMesh bin dir (renderScad m)
      case r of
        Left e -> failT t "loft frustum volume" e
        Right tris ->
          let k = (100 / (2 * pi)) * sin (2 * pi / 100) -- 100-gon area / disc area
              a1 = k * pi * 100
              a2 = k * pi * 25
              want = 30 / 3 * (a1 + a2 + sqrt (a1 * a2))
              got = meshVolume tris
           in assertT t "loft frustum volume" (abs (got - want) < 1e-3 * want) (printf "got %.3f want %.3f" got want)
  -- cross-check: `coscad next` conserves volume across bed packing
  bow <- tempDir "bow3"
  srcs <- listDirectory "examples/assemble/bow3"
  forM_ srcs $ \s -> copyFile ("examples/assemble/bow3" </> s) (bow </> s)
  rn <- try (processNext (bow </> "bow3.assemble")) :: IO (Either SomeException ())
  case rn of
    Left e -> failT t "coscad next bow3" (show e)
    Right () -> do
      bed <- parseStlAscii <$> readFile (bow </> "bow3_bed1.stl")
      parts <- mapM (\n -> parseStlAscii <$> readFile (bow </> ("bow3_next_" ++ n ++ "_asis.stl"))) ["center", "larch", "rarch"]
      let vb = meshVolume bed
          vp = sum (map meshVolume parts)
          ((x0, y0, z0), (x1, y1, _)) = meshBounds bed
      assertT t "coscad next: bed volume == sum of variant volumes" (abs (vb - vp) < 1e-6 * vp) (printf "bed %.3f parts %.3f" vb vp)
      assertT t "coscad next: placements inside 250x250 bed with 6mm margin, on z=0"
        (x0 >= 6 - 1e-6 && y0 >= 6 - 1e-6 && x1 <= 244 + 1e-6 && y1 <= 244 + 1e-6 && abs z0 < 1e-6)
        (show (meshBounds bed))
  rc <- try (processCheckWith False (bow </> "bow3.assemble")) :: IO (Either SomeException ())
  assertT t "coscad check bow3: no overlaps" (either (const False) (const True) rc) (either show (const "") rc)
  leftovers <- filter ("chk" `isInfixOf`) <$> listDirectory bow
  assertT t "coscad check leaves no scratch files next to the assembly" (null leftovers) (show leftovers)
  -- Hidden parts must retain their anchors during isolation, including
  -- chained attachments and transformed assemblies. Verify the rendered
  -- isolated child's actual bounds, not the bbox containing hidden parts.
  rel <- tempDir "relational-check"
  writeFile (rel </> "a.coscad") "main = box 20 20 20\n"
  writeFile (rel </> "b.coscad") "main = box 4 4 4\n"
  forM_ [ ("top", "a |> at top b", 10, 14)
        , ("on", "a |> on top b", 10, 14)
        , ("chain", "a |> at top b |> at top b", 10, 18)
        , ("moved", "(a |> at top b) |> z 30", 40, 44)
        , ("anchor", "(a |> at top b) |> anchor bot", 20, 24)
        ] $ \(name, expr, zlo, zhi) -> do
    let f = rel </> (name ++ ".assemble")
    writeFile f ("a ← a.coscad ×1\nb ← b.coscad ×1\nasm = " ++ expr ++ "\n")
    result <- try (processCheckWith True f) :: IO (Either SomeException ())
    case result of
      Left e -> failT t ("relational check " ++ name) (show e)
      Right () -> do
        tmp <- getTemporaryDirectory
        tris <- parseStlAscii <$> readFile (tmp </> "coscad-check" </> name </> "chk_b.stl")
        let bodies = filter (\body -> let ((x, _, _), _) = meshBounds body in x < 90000) (splitBodies tris)
            ((_, _, lo), (_, _, hi)) = meshBounds (concat bodies)
        assertT t ("relational check " ++ name) (abs (lo - zlo) < 1e-6 && abs (hi - zhi) < 1e-6)
          (show (lo, hi))
  let overlapF = rel </> "overlap.assemble"
  writeFile overlapF "a ← a.coscad ×1\nb ← b.coscad ×1\nasm = a |> at top 0 0 -1 b\n"
  overlap <- try (processCheckWith False overlapF) :: IO (Either ExitCode ())
  assertT t "relational check still detects real overlap" (overlap == Left (ExitFailure 1)) (show overlap)
  -- design stage spills across plates instead of failing
  sp <- tempDir "spill"
  copyFile "examples/assemble/spill.assemble" (sp </> "spill.assemble")
  copyFile "examples/assemble/bracket90.coscad" (sp </> "bracket90.coscad")
  ra <- try (processAssemble (sp </> "spill.assemble")) :: IO (Either SomeException ())
  case ra of
    Left e -> failT t "design stage: multi-plate spill" (show e)
    Right () -> do
      p1 <- doesFileExist (sp </> "spill_plate1.scad")
      p2 <- doesFileExist (sp </> "spill_plate2.scad")
      mf <- readFile (sp </> "spill_manifest.json")
      assertT t "design stage: multi-plate spill" (p1 && p2 && "\"plate\": 2" `isInfixOf` mf && "\"count\": 2" `isInfixOf` mf) (show (p1, p2))
  -- `coscad stl` end to end
  st <- tempDir "stl"
  writeFile (st </> "p.coscad") "r = 5\nmain = ● r\n"
  rs <- try (renderPart (st </> "p.coscad") Nothing) :: IO (Either SomeException ())
  case rs of
    Left e -> failT t "coscad stl renders a part" (show e)
    Right () -> do
      tris <- parseStlAscii <$> readFile (st </> "p.stl")
      let v = meshVolume tris
      assertT t "coscad stl renders a part" (abs (v - 4 / 3 * pi * 125) < 0.03 * (4 / 3 * pi * 125)) (printf "sphere volume %.2f" v)
  -- toolchain probes used by `coscad doctor`
  ov <- openscadVersion
  assertT t "openscad --version is parsed" (maybe False (not . null) ov) (show ov)
  bv <- bosl2Version dir
  assertT t "BOSL2 answers the version probe" (either (const False) (not . null) bv) (show bv)
  t1 <- getCurrentTime
  printf "(geometry tier: %.1fs)\n" (realToFrac (diffUTCTime t1 t0) :: Double)
  where
    parseLine l
      | "#" `isPrefixOf` l = []
      | otherwise = case words l of
          (f : rest) | Just g <- readGeo rest -> [(f, g)]
          _ -> []

-- ------------------------------------------------------------------

tempDir :: String -> IO FilePath
tempDir name = do
  base <- getTemporaryDirectory
  let d = base </> "coscad-test" </> name
  removePathForcibly d
  createDirectoryIfMissing True d
  return d

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  t <- T <$> newIORef 0 <*> newIORef []
  update <- (== Just "1") <$> lookupEnv "COSCAD_UPDATE_GOLDEN"
  diagnostics t
  examples t update
  render <- lookupEnv "COSCAD_RENDER"
  bin <- findOpenscad
  case (render, bin) of
    (Just "0", _) -> putStrLn "\n(geometry tier skipped: COSCAD_RENDER=0)"
    (_, Nothing) -> putStrLn "\n(geometry tier skipped: OpenSCAD not found; set COSCAD_OPENSCAD)"
    (_, Just b) -> geometry t update b
  n <- readIORef (tPassed t)
  fails <- readIORef (tFailed t)
  putStrLn ""
  printf "%d passed, %d failed\n" n (length fails)
  unless (null fails) $ do
    mapM_ (putStrLn . ("  - " ++)) fails
    exitFailure
