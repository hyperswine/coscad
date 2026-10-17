-- | CLI entry point. All real logic lives in the Coscad.* library
-- modules; this file only parses arguments and dispatches.
module Main (main) where

import Coscad.Assemble (failAsm, loadAssembly, processAssemble)
import Coscad.Check (processCheckWith)
import Coscad.Doctor (runDoctor)
import Coscad.Next (processNextWith)
import Coscad.Part (compilePart, renderPart)
import Coscad.Plan (processPlan, validateHardware)
import Coscad.Site (processSite)
import Coscad.Parser (parseProgramNamed)
import Coscad.IO (readFileUtf8)
import Data.List (isInfixOf)
import Data.Version (showVersion)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Paths_coscad (version)
import System.Environment (getArgs)
import Control.Monad (unless)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.FilePath (takeExtension)
import System.IO (Handle, hPutStrLn, hSetEncoding, stderr, stdout)
import System.IO.Error (catchIOError, ioeGetFileName)

main :: IO ()
main = runCLI `catchIOError` reportIO
  where
    reportIO err
      | "cannot decode byte sequence" `isInfixOf` show err =
          bad (maybe "source" id (ioeGetFileName err) ++ ": source is not valid UTF-8; save .coscad and .assemble files as UTF-8")
      | otherwise = bad (show err)

runCLI :: IO ()
runCLI = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  case args of
    [] -> usage stderr >> exitFailure
    (a : _) | a `elem` ["--help", "-h", "help"] -> usage stdout >> exitSuccess
    (a : _) | a `elem` ["--version", "-V", "version"] -> putStrLn ("coscad " ++ showVersion version)
    ["doctor"] -> runDoctor
    ["validate", f] -> case takeExtension f of
      ".assemble" -> loadAssembly [] f >>= either failAsm (either (designBad . unlines . map ((f ++ ": design: ") ++) . lines) (const (putStrLn "Valid assembly")) . validateHardware)
      ".coscad" -> do
        exists <- doesFileExist f
        unless exists (bad (f ++ ": file not found"))
        readFileUtf8 f >>= either bad (const (putStrLn "Valid part")) . parseProgramNamed f
      _ -> bad "validate needs a .coscad or .assemble file"
    ("validate" : _) -> bad "validate needs one .coscad or .assemble file"
    ("stl" : rest) -> withInOut "stl" rest renderPart
    ("next" : rest) -> do
      let strict = "--strict" `elem` rest
      case filter (/= "--strict") rest of
        [f] -> needExt ".assemble" f >> processNextWith strict f
        _ -> bad "next needs one .assemble file. Usage: coscad next [--strict] model.assemble"
    ("site" : rest) -> case rest of
      (dir : specs) | not (null specs) -> mapM_ (needExt ".assemble") specs >> processSite dir specs
      _ -> bad "site takes an output directory and one or more .assemble files"
    ("plan" : rest) -> do
      let png = "--png" `elem` rest
      case filter (/= "--png") rest of
        [f] -> needExt ".assemble" f >> processPlan png f
        _ -> bad "plan takes one .assemble file (and optionally --png)"
    ("check" : rest) -> do
      let keep = "--keep-temp" `elem` rest
      case filter (/= "--keep-temp") rest of
        [f] -> needExt ".assemble" f >> processCheckWith keep f
        _ -> bad "check takes one .assemble file (and optionally --keep-temp)"
    [f] | takeExtension f == ".assemble" -> processAssemble f
    rest -> withInOut "compile" rest compilePart

-- | `<file> [-o out]` in either order.
withInOut :: String -> [String] -> (FilePath -> Maybe FilePath -> IO ()) -> IO ()
withInOut what rest run = case rest of
  [f] -> go f Nothing
  [f, "-o", o] -> go f (Just o)
  ["-o", o, f] -> go f (Just o)
  _ -> bad (what ++ " takes one .coscad file and an optional -o <output>")
  where
    go f o = needExt ".coscad" f >> run f o

needExt :: String -> FilePath -> IO ()
needExt ext f
  | takeExtension f == ext = return ()
  | otherwise = bad ("expected a " ++ ext ++ " file, got " ++ f ++ " (see coscad --help)")

-- | Exit 1: the command could not run (usage, missing file, syntax or
-- geometry error, OpenSCAD failure).
bad :: String -> IO ()
bad msg = hPutStrLn stderr ("Error: " ++ msg) >> exitFailure

-- | Exit 2: the input is well formed but the design has a problem
-- (`validate`/`plan` design errors, `check` overlaps), so a pipeline can
-- tell "fix the model" from "fix the setup".
designBad :: String -> IO ()
designBad msg = mapM_ (hPutStrLn stderr . ("Error: " ++)) (lines msg) >> exitWith (ExitFailure 2)

usage :: Handle -> IO ()
usage h =
  mapM_
    (hPutStrLn h)
    [ "coscad " ++ showVersion version ++ " - glyph/pipeline CAD language -> OpenSCAD -> STL"
    , ""
    , "Usage:"
    , "  coscad <part.coscad> [-o part.scad]     compile one part to OpenSCAD"
    , "  coscad validate <part.coscad|spec.assemble>  validate source, mating constraints and hardware without writing artifacts"
    , "  coscad stl <part.coscad> [-o part.stl]  compile and render to STL (prints volume + bounds)"
    , "  coscad <spec.assemble>                  design stage: assembled view, packed plate(s), spec_design.json"
    , "  coscad next [--strict] <spec.assemble>  manufacturing: orient, pack beds, bedN.stl + spec_manifest.json;"
    , "                                          --strict exits 2 when any part has a printability warning"
    , "  coscad plan <spec.assemble> [--png]     build instructions: ordered steps, preload sheet, BOM"
    , "  coscad site <dir> <spec.assemble>...    static phone-friendly site: step images, search across builds"
    , "  coscad check <spec.assemble> [--keep-temp]"
    , "                                          interference / clearance check on real meshes"
    , "  coscad doctor                           verify OpenSCAD + BOSL2 + an end-to-end render"
    , "  coscad --version | --help"
    , ""
    , "Environment:"
    , "  COSCAD_OPENSCAD          OpenSCAD binary or wrapper script (else PATH, then the app bundle)"
    , "  COSCAD_BOSL2             BOSL2 checkout directory (else the OpenSCAD library folders)"
    , "  COSCAD_OPENSCAD_TIMEOUT  seconds per OpenSCAD run before it is killed (default 600)"
    , ""
    , "Exit status: 0 ok; 1 could not run (usage, missing file, syntax/geometry error, OpenSCAD"
    , "failure, unknown instance or frame); 2 the design has a problem (plan/validate design errors,"
    , "check overlaps, next --strict printability warnings, and for every command that loads an assembly: constraint loops that do not"
    , "close, parts no fixed root reaches, an instance fixed twice)."
    , "Diagnostics go to stderr as `Error: file:line:col: reason`, optionally followed by an excerpt."
    , ""
    , "Language (one definition per line; `main` is rendered; // comments):"
    , "  numbers      w = 20      t = w / 5 - 1      box w (w / 2) t      χ -r part"
    , "  shapes       ■ s  ▬ x y z  ● r  ◎ r h  ▻ r h  ⎏ n r h            (OpenSCAD, corner/bottom anchored)"
    , "               ▣ x y z c  ◙ x y z r  ⌭ r h c  ⌽ r h r2  ⊚ or ir h  ⏢ x1 y1 x2 y2 h  ◉ R r  ⊿ x y z"
    , "               cube s  box x y z  sphere r  cyl r h  xcyl/ycyl/zcyl r l  tube or ir h  torus R r  wedge x y z"
    , "  2D           △ r  ⬠ r  ⭘ r  ✎ x y x y ... (bezier)     extrude: ⮕ h p  |> extrude h"
    , "  loft         loft z0 p0 z1 p1 ...   ⟰ ...   p0 |> loft z1 p1      (2D profiles -> solid)"
    , "  booleans     ⊕ union  ⊖ difference  ∩ intersection  ⇓ hull  ⊞ minkowski  ↯ offset(2D)"
    , "  transforms   χ ψ ζ d (translate)  θ ϕ ω deg (rotate)  ⬈ sx sy sz  ⇋ nx ny nz (mirror)  ⚓ anchor"
    , "  pipelines    a |> add b |> cut c |> x 5 |> rotz 90 |> at top [dx dy dz] b |> on rt b |> cutat top 0 0 -2 c"
    , "  anchors      top bot lft rt fwd bak ctr, combinable: top+rt  lft+fwd   (a ⌖ top b, a ⋈ rt b)"
    , "  modes        first line !glyph or !simple (ASCII words: Box, Translate.x, Hull, * union, - difference)"
    , ""
    , "Manual: man coscad   Docs: docs/LANGEXTENSION.md, docs/TOPOLOGICAL.md, docs/MANUFACTURING.md"
    ]
