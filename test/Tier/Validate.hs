-- | `coscad validate`, structured: diagnostics as records and as JSON
-- (`--json`), and unsaved text (`--stdin`), which the editor relies on.
module Tier.Validate
  ( validateTier
  )
where

import Coscad.Diagnose
import Coscad.Json (JValue (..), asArray, asNumber, asString, decode, encode, lookupKey)
import Coscad.Report (Diag (..), Failure (..), collecting, inputFailure, designFailures)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import System.Directory (findExecutable, canonicalizePath, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Harness

validateTier :: T -> IO ()
validateTier t = do
  section "validate: structured diagnostics"
  -- structuring the text commands print
  let parseErr = inputFailure "p.coscad:2:8: unexpected 'j'\n  |\n2 | main = box 1 1 1 junk\n  |        ^"
  case failureDiagnostics parseErr of
    [d] -> assertT t "error: file, line, column, message and excerpt are split out"
      (dgSeverity d == SevError && dgFile d == Just "p.coscad" && dgLine d == Just 2 && dgColumn d == Just 8
        && dgMessage d == "unexpected 'j'" && length (dgExcerpt d) == 3) (show d)
    ds -> failT t "error: one diagnostic per Error line" (show ds)
  case failureDiagnostics (inputFailure "a.assemble:3:1: constraint loop\n  note: closes at b") of
    [d] -> assertT t "error: a note line extends the message, not the excerpt"
      (dgMessage d == "constraint loop\nnote: closes at b" && null (dgExcerpt d)) (show d)
    ds -> failT t "error: note lines stay with their error" (show ds)
  case failureDiagnostics (inputFailure "missing.assemble: file not found") of
    [d] -> assertT t "error: file-level message keeps the file, no position"
      (dgFile d == Just "missing.assemble" && dgLine d == Nothing && dgMessage d == "file not found") (show d)
    ds -> failT t "error: file-level" (show ds)
  assertT t "design errors: one diagnostic each"
    (length (failureDiagnostics (designFailures ["a.assemble: design: x", "a.assemble: design: y"])) == 2) ""
  let w = diagDiagnostic (Warning "p.coscad:1:35: 'at top' is taken on a part turned")
  assertT t "warning: positioned, warning severity"
    (dgSeverity w == SevWarning && dgLine w == Just 1 && dgColumn w == Just 35 && "'at top'" `isInfixOf` dgMessage w) (show w)
  let un = diagDiagnostic (Note "step images use the OpenSCAD preview")
  assertT t "note: unpositioned text stays whole" (dgFile un == Nothing && dgSeverity un == SevNote) (show un)
  -- validating unsaved text through the library
  dir <- tempDir "validate"
  writeFile (dir </> "blk.coscad") "main = box 20 20 20\n"
  (emit, collected) <- collecting
  part <- validateSource emit (dir </> "unsaved.coscad") (Just "main = box 10 10 10 |> roty 45 |> at top 0 0 0 (box 1 1 1)\n")
  asm <- validateSource emit (dir </> "unsaved.assemble") (Just "blk ← blk.coscad ×3\nasm = blk ⊕ (blk |> move 30 0 0)\n")
  bad <- validateSource emit (dir </> "broken.coscad") (Just "main = box 1 2\n")
  diags <- collected
  assertT t "library: an unsaved part validates, its anchor warning is collected"
    (part == Right "Valid part" && any (\d -> case d of Warning m -> "unsaved.coscad:1:35:" `isInfixOf` m; _ -> False) diags) (show (part, diags))
  assertT t "library: an unsaved assembly resolves its parts from its directory"
    (asm == Right "Valid assembly" && any (\d -> case d of Warning m -> "declared ×3" `isInfixOf` m; _ -> False) diags) (show asm)
  assertT t "library: a syntax error in unsaved text is a Failure (exit 1)"
    (case bad of Left (Failure 1 (l : _)) -> "broken.coscad:1:" `isInfixOf` l; _ -> False) (show bad)
  root <- canonicalizePath (dir </> "project.assemble")
  sub <- canonicalizePath (dir </> "nested.assemble")
  leaf <- canonicalizePath (dir </> "virtual.coscad")
  let overlays = [(root,"sub ← nested.assemble ×1\nasm = sub\n"),
                  (sub,"leaf ← virtual.coscad ×1\nasm = leaf\n"),
                  (leaf,"main = box 10 10 10\nsupport bot\nload top 0 0 -10\nstress material=pla\n")]
  (validProject, dependencies) <- validateProject emit root overlays
  exists <- doesFileExist leaf
  assertT t "project: nested unsaved references including nonexistent files; no writes"
    (validProject == Right "Valid assembly" && all (`elem` dependencies) [root,sub,leaf] && not exists) (show (validProject,dependencies))
  (invalidProject,_) <- validateProject emit root ((leaf,"main = box 1 2\n") : filter ((/=leaf) . fst) overlays)
  assertT t "project: errors in an unsaved referenced part retain its source path"
    (case invalidProject of Left f -> any ((==Just leaf) . dgFile) (failureDiagnostics f); _ -> False) (show invalidProject)
  (badMaterial,_) <- validateProject emit root ((leaf,"main = box 1 2 3\nstress material=unknown\n") : filter ((/=leaf) . fst) overlays)
  assertT t "project: referenced stress settings are checked without rendering"
    (case badMaterial of Left f -> "unknown material" `isInfixOf` show f; _ -> False) (show badMaterial)
  (badDatum,_) <- validateProject emit leaf [(leaf,"main = box 1 2 3\nload @absent 0 0 -1\n")]
  assertT t "validate: unknown analysis datum is diagnosed"
    (case badDatum of Left f -> "unknown datum" `isInfixOf` show f; _ -> False) (show badDatum)
  (duplicate,_) <- validateProject emit leaf [(leaf,"main = box 1 1 1"),(dir </> "." </> "virtual.coscad","main = box 2 2 2")]
  assertT t "project: canonical duplicate identities are rejected"
    (case duplicate of Left f -> "duplicate buffer" `isInfixOf` show f; _ -> False) (show duplicate)
  (cycleResult,_) <- validateProject emit root [(root,"sub ← nested.assemble ×1\nasm = sub\n"),(sub,"root ← ./project.assemble ×1\nasm = root\n")]
  assertT t "project: unsaved recursive aliases cannot bypass cycle detection"
    (case cycleResult of Left f -> "cycle" `isInfixOf` show f; _ -> False) (show cycleResult)
  assertT t "project protocol: relative buffer paths rejected"
    (case projectSources "{\"schema\":\"coscad.project/1\",\"files\":[{\"path\":\"part.coscad\",\"text\":\"\"}]}" of Left _ -> True; _ -> False) ""
  -- the CLI
  exe <- findExecutable "coscad"
  case exe of
    Nothing -> failT t "coscad executable on PATH" "run the suite with `cabal test`"
    Just bin -> do
      let snapshot = encode (JObject [("schema",JString "coscad.project/1"),
                         ("files",JArray [JObject [("path",JString p),("text",JString s)] | (p,s) <- overlays])])
      (cp, op, ep) <- readProcessWithExitCode bin ["validate", "--json", "--project-stdin", root] snapshot
      assertT t "CLI project snapshot: nested virtual buffers and canonical dependency list"
        (cp == ExitSuccess && null ep && case decode op of
          Right j -> lookupKey "valid" j == Just (JBool True) && all (\p -> JString p `elem` fromMaybe [] (lookupKey "dependencies" j >>= asArray)) [root,sub,leaf]
          _ -> False) op
      (cc, oc, ec) <- readProcessWithExitCode bin ["capabilities", "--json"] ""
      assertT t "capabilities: versioned build features and material registry"
        (cc == ExitSuccess && null ec && case decode oc of
          Right j -> lookupKey "schema" j == Just (JString "coscad.capabilities/1") &&
            maybe False (JString "project-buffers" `elem`) (lookupKey "features" j >>= asArray) &&
            maybe False (not . null) (lookupKey "materials" j >>= asArray)
          _ -> False) oc
      (cm, _, _) <- readProcessWithExitCode bin ["validate", "--stdin", "--project-stdin", root] snapshot
      assertT t "CLI snapshot modes are mutually exclusive" (cm == ExitFailure 1) (show cm)
      writeFile (dir </> "tilt.coscad") "main = box 10 10 10\n  |> roty 45\n  |> at top 0 0 0 (box 1 1 1)\n"
      (c1, o1, e1) <- readProcessWithExitCode bin ["validate", "--json", dir </> "tilt.coscad"] ""
      let j1 = decode o1
          field k = either (const Nothing) (lookupKey k) j1
          diag1 = either (const []) (\v -> fromMaybe [] (asArray =<< lookupKey "diagnostics" v)) j1
      assertT t "--json: one JSON document on stdout, nothing on stderr, exit 0 for a valid part"
        (c1 == ExitSuccess && null e1 && (asString =<< field "schema") == Just "coscad.validate/1" && field "valid" == Just (JBool True)) (show (c1, o1, e1))
      assertT t "--json: the warning has severity, line and column"
        (case diag1 of
           [d] -> (asString =<< lookupKey "severity" d) == Just "warning" && (asNumber =<< lookupKey "line" d) == Just 3 && (asNumber =<< lookupKey "column" d) == Just 6
           _ -> False) (show diag1)
      (c2, o2, _) <- readProcessWithExitCode bin ["validate", "--json", "--stdin", dir </> "tilt.coscad"] "main = box 1 2\n"
      let j2 = decode o2
          diag2 = either (const []) (\v -> fromMaybe [] (asArray =<< lookupKey "diagnostics" v)) j2
      assertT t "--stdin: the buffer is checked, not the file; errors carry excerpts; exit 1"
        (c2 == ExitFailure 1 && either (const False) (\v -> lookupKey "valid" v == Just (JBool False) && (asNumber =<< lookupKey "exit" v) == Just 1) j2
          && case diag2 of
               [d] -> (asString =<< lookupKey "severity" d) == Just "error" && (asNumber =<< lookupKey "line" d) == Just 1
                        && maybe False (not . null) (asArray =<< lookupKey "excerpt" d)
               _ -> False) (show (c2, o2))
      (c3, _, e3) <- readProcessWithExitCode bin ["validate", "--bogus", dir </> "tilt.coscad"] ""
      assertT t "validate: an unknown option is refused (exit 1)" (c3 == ExitFailure 1 && "unknown option --bogus" `isInfixOf` e3) (show (c3, e3))
