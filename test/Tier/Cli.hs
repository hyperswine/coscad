-- | CLI tier, and the export-list guard.
module Tier.Cli
  ( exportListTier
  , cliTier
  )
where

import Control.Monad (forM, forM_)
import Coscad.Json (JValue (..), asArray, asNumber, asString, decode, lookupKey)
import qualified Coscad.Json
import Coscad.Mesh (meshBounds, parseStlAscii)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, fromMaybe)
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath
import System.Process (readProcessWithExitCode)
import Harness
import Tier.Manufacturing
import Tier.Assembly

-- | Every library module states its interface: no `module X (module X)`
-- self-export, which would defeat -Wmissing-export-lists.
exportListTier :: T -> IO ()
exportListTier t = do
  section "export lists"
  let walk d = do
        es <- listDirectory d
        fmap concat $ forM es $ \e -> do
          let p = d </> e
          isDir <- doesDirectoryExist p
          if isDir then walk p else return [p | takeExtension p == ".hs"]
  files <- walk "src/Coscad"
  selfExports <- fmap concat $ forM files $ \f -> do
    src <- readFile f
    return [f | l <- lines src, "module " `isPrefixOf` l, let ws = words l, length ws > 2, "(module" `elem` ws, (ws !! 1) `elem` map (filter (`notElem` ",)")) (drop 2 ws)]
  assertT t "every module under src/Coscad has an explicit export list" (null selfExports) (show selfExports)
  assertT t "coscad plan is split into Model, Search and Render"
    (all (`elem` files) ["src/Coscad/Plan/Model.hs", "src/Coscad/Plan/Search.hs", "src/Coscad/Plan/Render.hs"]) (show files)

cliTier :: T -> IO ()
cliTier t = do
  section "cli"
  exe <- findExecutable "coscad"
  case exe of
    Nothing -> failT t "coscad executable on PATH" "cabal puts it there via build-tool-depends; run the suite with `cabal test`"
    Just bin -> do
      dir <- tempDir "cli"
      let run args = readProcessWithExitCode bin args ""
          firstLine = takeWhile (/= '\n')
      (c0, o0, _) <- run ["--version"]
      assertT t "--version exits 0 and names the version" (c0 == ExitSuccess && "coscad " `isPrefixOf` o0) (show (c0, o0))
      (c1, _, e1) <- run []
      assertT t "no arguments is a usage error (exit 1)" (c1 == ExitFailure 1 && not (null e1)) (show (c1, e1))
      (c2, o2, e2) <- run [dir </> "missing.coscad"]
      assertT t "missing file: exit 1, Error: on stderr, nothing on stdout" (c2 == ExitFailure 1 && "Error: " `isPrefixOf` e2 && "not found" `isInfixOf` e2 && null o2) (show (c2, o2, e2))
      (c2v, o2v, e2v) <- run ["validate", dir </> "missing.coscad"]
      assertT t "validate missing file: exit 1, Error: on stderr" (c2v == ExitFailure 1 && "Error: " `isPrefixOf` e2v && "not found" `isInfixOf` e2v && null o2v) (show (c2v, o2v, e2v))
      let variantsDir = dir </> "variants"
      writeVariantCollisions variantsDir
      (cv, _, ev) <- run [variantsDir </> "top.assemble"]
      design <- manifestVariants "scad" <$> readFile (variantsDir </> "top_design.json")
      scads <- mapM (doesFileExist . (variantsDir </>)) ["top_part_a_base_asis.scad", "top_part_b_base_asis.scad"]
      assertT t "design stage: same part name in two subassemblies gets two variant ids and scads"
        (cv == ExitSuccess && and scads && design == Right (["a_base_asis", "b_base_asis"], ["top_part_a_base_asis.scad", "top_part_b_base_asis.scad"], ["b_base_asis", "a_base_asis"]))
        (show (cv, ev, design))
      -- design stage and the plate's max height / size
      writeFile (dir </> "post.coscad") "main = box 10 10 200\n"
      writeFile (dir </> "tallplate.assemble") "plate 180 180 6 180\npost ← post.coscad ×1 ▽bot\n"
      (ch, _, eh) <- run [dir </> "tallplate.assemble"]
      dh <- readJson (dir </> "tallplate_design.json")
      assertT t "design stage: a part over the plate's max height warns (exit 0) and plate.max_z is recorded"
        (ch == ExitSuccess && any (\l -> "Warning: post_bot: about 200 mm tall standing on ▽bot, over the plate's max height of 180 mm" `isPrefixOf` l) (lines eh)
          && either (const False) (\v -> (asNumber =<< lookupKey "max_z" =<< lookupKey "plate" v) == Just 180) dh)
        (show (ch, eh))
      writeFile (dir </> "wide.coscad") "main = box 300 20 5\n"
      writeFile (dir </> "wide.assemble") "plate 180 180 6\nwide ← wide.coscad ×1\n"
      (cw, _, ew) <- run [dir </> "wide.assemble"]
      assertT t "design stage: a footprint bigger than the plate either way is a design error (exit 2) naming the file and part"
        (cw == ExitFailure 2 && ("Error: " ++ dir </> "wide.assemble: wide_1 footprint") `isPrefixOf` firstLine ew && "even turned 90 degrees" `isInfixOf` ew) (show (cw, ew))
      writeFile (dir </> "ok.coscad") "r = 5\nmain = ● r\n"
      (c3, o3, e3) <- run ["validate", dir </> "ok.coscad"]
      assertT t "validate good part: exit 0, Valid part, empty stderr" (c3 == ExitSuccess && o3 == "Valid part\n" && null e3) (show (c3, o3, e3))
      writeFile (dir </> "bad.coscad") "main = ■ 10 |> mov 1 2 3\n"
      (c4, o4, e4) <- run ["validate", dir </> "bad.coscad"]
      let want4 = "Error: " ++ dir </> "bad.coscad" ++ ":1:16: unknown pipeline stage 'mov'"
      assertT t "validate bad part: exit 1, first stderr line is Error: file:line:col: reason" (c4 == ExitFailure 1 && want4 `isPrefixOf` firstLine e4 && "did you mean 'move'" `isInfixOf` firstLine e4 && null o4) (show (c4, o4, e4))
      assertT t "validate bad part: the excerpt follows the reason" (any ("| main = " `isInfixOf`) (lines e4) && any ("^" `isInfixOf`) (lines e4)) e4
      writeFile (dir </> "syntax.coscad") "main = box 10 20\n"
      (c5, _, e5) <- run ["validate", dir </> "syntax.coscad"]
      assertT t "validate syntax error: position and reason on one line" (c5 == ExitFailure 1 && ("Error: " ++ dir </> "syntax.coscad" ++ ":1:") `isPrefixOf` firstLine e5 && "height" `isInfixOf` firstLine e5) (show (c5, e5))
      -- constraint design errors exit 2 from every command that loads an assembly; input errors exit 1
      let loops = dir </> "loops"
      createDirectoryIfMissing True loops
      writeLink loops
      writeFile (loops </> "ring.assemble") (ringAssembly " gap=0.000002")
      writeFile (loops </> "float.assemble") "l1 ← link.coscad ×1\nl2 ← link.coscad ×1\nl3 ← link.coscad ×1\nfixed l1\nmate l2 @a l1 @b\n"
      writeFile (loops </> "roots.assemble") "l1 ← link.coscad ×1\nl2 ← link.coscad ×1\nfixed l1\nfixed l2\nmate l2 @a l1 @b\n"
      writeFile (loops </> "typo.assemble") "l1 ← link.coscad ×1\nl2 ← link.coscad ×1\nfixed l1\nmate l9 @a l1 @b\n"
      let ring = loops </> "ring.assemble"
          ringFirst = "Error: " ++ ring ++ ":17:1: conflicting assembly constraints"
      forM_ [ (["validate", ring], "validate"), ([ring], "design stage"), (["next", ring], "next")
            , (["plan", ring], "plan"), (["check", ring], "check"), (["site", loops </> "site", ring], "site") ] $ \(args, label) -> do
        (c, o, e) <- run args
        assertT t (label ++ ": constraint loop conflict exits 2 with the closing mate first and every mate as a note")
          (c == ExitFailure 2 && ringFirst `isPrefixOf` firstLine e && length (filter ("  note: " `isPrefixOf`) (lines e)) == 8 && not ("Wrote" `isInfixOf` o))
          (show (c, o, e))
      forM_ [("float.assemble", ":3:1: underconstrained"), ("roots.assemble", ":5:1: conflicting assembly constraints")] $ \(f, want) -> do
        (c, o, e) <- run ["validate", loops </> f]
        assertT t ("validate " ++ f ++ ": design error exits 2 at the right line") (c == ExitFailure 2 && ("Error: " ++ loops </> f ++ want) `isPrefixOf` firstLine e && null o) (show (c, o, e))
      forM_ [["validate"], [], ["next"], ["plan"], ["check"]] $ \cmd -> do
        (c, o, e) <- run (cmd ++ [loops </> "typo.assemble"])
        assertT t (unwords ("coscad" : cmd) ++ " with an unknown instance: input error exits 1")
          (c == ExitFailure 1 && ("Error: " ++ loops </> "typo.assemble" ++ ":4:1: unknown constrained instance") `isPrefixOf` firstLine e && not ("Wrote" `isInfixOf` o)) (show (c, o, e))
      writeFile (dir </> "nopart.assemble") "a ← gone.coscad ×1\nasm = a\n"
      (c6, _, e6) <- run ["validate", dir </> "nopart.assemble"]
      assertT t "validate assembly with a missing part names the file" (c6 == ExitFailure 1 && ("Error: " ++ dir </> "gone.coscad" ++ ": part file not found") `isPrefixOf` firstLine e6) (show (c6, e6))
      (c7, _, e7) <- run ["next", dir </> "ok.coscad"]
      assertT t "next on a .coscad is a usage error" (c7 == ExitFailure 1 && "Error: " `isPrefixOf` e7) (show (c7, e7))
      (c7s, _, e7s) <- run ["next", "--strict"]
      assertT t "next --strict without a file is a usage error naming --strict" (c7s == ExitFailure 1 && "next [--strict]" `isInfixOf` e7s) (show (c7s, e7s))
      (_, helpOut, _) <- run ["--help"]
      assertT t "--help documents next --strict and its exit 2" ("next [--strict]" `isInfixOf` helpOut && "next --strict printability warnings" `isInfixOf` helpOut) helpOut
      -- design stage + plan on the corner pair fixture, read back as JSON
      planFiles <- listDirectory "examples/assemble/plan"
      forM_ ("corner_pair.assemble" : filter ((== ".coscad") . takeExtension) planFiles) $ \f -> copyFile ("examples/assemble/plan" </> f) (dir </> f)
      (c8, o8, e8) <- run [dir </> "corner_pair.assemble"]
      mf <- readJson (dir </> "corner_pair_design.json")
      noNext <- not <$> doesFileExist (dir </> "corner_pair_manifest.json")
      assertT t "design stage: exit 0, corner_pair_design.json parses with schema, plate and placements; no _manifest.json" (c8 == ExitSuccess && null e8 && noNext && ("Wrote " ++ dir </> "corner_pair_design.json") `isInfixOf` o8 && either (const False) (\v -> (asString =<< lookupKey "schema" v) == Just "coscad.design/1" && isJust (lookupKey "plate" v) && maybe False (not . null) (asArray =<< lookupKey "placements" v)) mf) (show (c8, o8, e8, noNext, either id (const "parsed") mf))
      -- a design manifest left under the old name by coscad 1.2 gets a migration note, and is kept
      let legacy = "{\"source\": \"corner_pair.assemble\", \"plate\": {\"w\": 220, \"d\": 220}, \"placements\": []}\n"
      writeFile (dir </> "corner_pair_manifest.json") legacy
      (c8b, _, e8b) <- run [dir </> "corner_pair.assemble"]
      kept <- readFile (dir </> "corner_pair_manifest.json")
      assertT t "design stage: old design manifest under _manifest.json gets a migration note on stderr and is left in place"
        (c8b == ExitSuccess && "Note: " `isPrefixOf` e8b && "corner_pair_design.json" `isInfixOf` e8b && "coscad next" `isInfixOf` e8b && kept == legacy) (show (c8b, e8b))
      removeFile (dir </> "corner_pair_manifest.json")
      (c9, o9, e9) <- run ["plan", dir </> "corner_pair.assemble"]
      pj <- readJson (dir </> "corner_pair_plan.json")
      let nuts v = [n | st <- fromMaybe [] (asArray =<< lookupKey "steps" v), f <- fromMaybe [] (asArray =<< lookupKey "fasteners" st), Just n <- [asString =<< lookupKey "nut" f]]
          steps v = fromMaybe [] (asArray =<< lookupKey "steps" v)
      step1 <- readFile (dir </> "corner_pair_step1.scad")
      assertT t "plan: step scene draws the bench as a solid slab, no background modifier" ("color(\"Gainsboro\") translate(" `isInfixOf` step1 && not ("\n%" `isInfixOf` step1) && "color(\"Orange\")" `isInfixOf` step1) (take 600 step1)
      assertT t "plan: exit 0, plan JSON parses, nut kinds use the nut= words" (c9 == ExitSuccess && null e9 && "Wrote" `isInfixOf` o9 && either (const False) (\v -> not (null (steps v)) && all (`elem` ["dropin", "slidein", "hex", "none"]) (nuts v) && not (null (nuts v))) pj) (show (c9, e9, either id (const "parsed") pj))
      -- a design error: two screws at the same spot. Exit 2, stderr, files still written.
      srcPair <- readFile (dir </> "corner_pair.assemble")
      let fastLines = [l | l <- lines srcPair, "fastener " `isPrefixOf` l]
      case fastLines of
        (fl : _) -> do
          writeFile (dir </> "collide.assemble") (srcPair ++ "\n" ++ fl ++ "\n")
          (c10, o10, e10) <- run ["validate", dir </> "collide.assemble"]
          assertT t "validate design error: exit 2, Error: on stderr" (c10 == ExitFailure 2 && "Error: " `isPrefixOf` e10 && "collide" `isInfixOf` e10 && null o10) (show (c10, o10, e10))
          (c11, _, e11) <- run ["plan", dir </> "collide.assemble"]
          pj2 <- readJson (dir </> "collide_plan.json")
          assertT t "plan design error: exit 2, design: on stderr, design_errors in the JSON" (c11 == ExitFailure 2 && (": design: " `isInfixOf` e11) && either (const False) (maybe False (not . null) . (asArray =<<) . lookupKey "design_errors") pj2) (show (c11, e11, either id (const "parsed") pj2))
        [] -> failT t "corner_pair has a fastener line" srcPair
      -- the STL reader diagnoses what OpenSCAD can hand back
      let nanStl = "solid x\n facet normal 0 0 1\n outer loop\n vertex 0 0 0\n vertex 1 0 0\n vertex nan 1 0\n endloop\n endfacet\nendsolid x\n"
          binStl = "solid bin" ++ replicate 80 '\0' ++ "\1\0\0\0"
      assertT t "STL: non-finite vertex is an error naming the line" (either ("line 6" `isInfixOf`) (const False) (parseStlAscii nanStl)) (show (parseStlAscii nanStl))
      assertT t "STL: binary file is an error, not a crash" (either ("not ASCII" `isInfixOf`) (const False) (parseStlAscii binStl)) (show (parseStlAscii binStl))
      assertT t "STL: empty solid parses to no triangles" (parseStlAscii "solid e\nendsolid e\n" == Right []) (show (parseStlAscii "solid e\nendsolid e\n"))
      assertT t "STL: bounds of the empty mesh are a point" (meshBounds [] == ((0, 0, 0), (0, 0, 0))) ""
      -- JSON round trip of awkward strings
      let awkward = JObject [("s", JString "quote\" back\\ nl\n tab\t uni\x2028 </script>"), ("n", JArray [JNumber 1.5, JNumber (-2), JNull, JBool True])]
      assertT t "Coscad.Json round-trips escapes" (decode (Coscad.Json.encode awkward) == Right awkward) (Coscad.Json.encode awkward)
      _ <- return (asNumber JNull)
      return ()

-- ------------------------------------------------------------------
