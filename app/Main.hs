-- | CLI entry point. All real logic lives in the Coscad.* library
-- modules; this file parses arguments, dispatches, prints warnings and
-- failures, and is the only place that exits the process.
module Main (main) where

import Coscad.Assemble (processAssemble)
import Coscad.Backend (Backend (..), parseBackend)
import Coscad.Check (processCheckWithBackend)
import Coscad.Doctor (runDoctor)
import Coscad.Next (processNextWithBackend)
import Coscad.Part (compilePart, renderPartWith)
import Coscad.Plan (processPlan)
import Coscad.Diagnose (validateJson, validateSource)
import Coscad.Report (Failure (..), collecting, stderrEmit)
import Coscad.Site (processSite)
import Data.List (isInfixOf, isPrefixOf)
import Data.Version (showVersion)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Paths_coscad (version)
import System.Environment (getArgs)
import Control.Monad (unless)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.FilePath (takeExtension)
import System.IO (Handle, getContents', hPutStrLn, hSetEncoding, stderr, stdin, stdout)
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
  hSetEncoding stdin utf8
  args <- getArgs
  case args of
    [] -> usage stderr >> exitFailure
    (a : _) | a `elem` ["--help", "-h", "help"] -> usage stdout >> exitSuccess
    (a : _) | a `elem` ["--version", "-V", "version"] -> putStrLn ("coscad " ++ showVersion version)
    ["doctor"] -> runDoctor >>= finish
    ("validate" : rest) -> case partitionFlags ["--json", "--stdin"] rest of
      Right (flags, [f]) -> do
        let json = "--json" `elem` flags
        unless (takeExtension f `elem` [".coscad", ".assemble"]) (bad "validate needs a .coscad or .assemble file")
        -- --stdin: the file's text comes from stdin (an unsaved editor
        -- buffer); relative references still resolve from f's directory
        source <- if "--stdin" `elem` flags then Just <$> getContents' else return Nothing
        if json
          then do
            (emit, collected) <- collecting
            outcome <- validateSource emit f source
            diags <- collected
            putStrLn (validateJson f outcome diags)
            either (\(Failure code _) -> exitWith (ExitFailure (if code == 0 then 1 else code))) (const (return ())) outcome
          else validateSource stderrEmit f source >>= either (finish . Left) putStrLn
      Right _ -> bad "validate needs one .coscad or .assemble file. Usage: coscad validate [--json] [--stdin] file"
      Left flag -> bad ("validate: unknown option " ++ flag ++ " (use --json, --stdin)")
    ("stl" : rest) -> do
      (backend,remaining) <- backendArgs "stl" rest
      withInOut "stl" remaining
        (\f o -> renderPartWith backend stderrEmit f o >>= finish)
    ("next" : rest) -> do
      (backend,remaining) <- backendArgs "next" rest
      let strict = "--strict" `elem` remaining
      case filter (/= "--strict") remaining of
        [f] -> needExt ".assemble" f >> processNextWithBackend backend stderrEmit strict f >>= finish
        _ -> bad "next needs one .assemble file. Usage: coscad next [--strict] model.assemble"
    ("site" : rest) -> case rest of
      (dir : specs) | not (null specs) -> mapM_ (needExt ".assemble") specs >> processSite stderrEmit dir specs >>= finish
      _ -> bad "site takes an output directory and one or more .assemble files"
    ("plan" : rest) -> do
      let png = "--png" `elem` rest
      case filter (/= "--png") rest of
        [f] -> needExt ".assemble" f >> processPlan stderrEmit png f >>= finish
        _ -> bad "plan takes one .assemble file (and optionally --png)"
    ("check" : rest) -> do
      (backend,remaining) <- backendArgs "check" rest
      let keep = "--keep-temp" `elem` remaining
      case filter (/= "--keep-temp") remaining of
        [f] -> needExt ".assemble" f >> processCheckWithBackend backend stderrEmit keep f >>= finish
        _ -> bad "check takes one .assemble file (and optionally --keep-temp)"
    [f] | takeExtension f == ".assemble" -> processAssemble stderrEmit f >>= finish
    rest -> withInOut "compile" rest (\f o -> compilePart stderrEmit f o >>= finish)

-- | Backend flags are command-local; no silent global selection or fallback.
backendArgs :: String -> [String] -> IO (Backend,[String])
backendArgs command args = do
  let prefix="--backend="
      choices=[drop (length prefix) a | a<-args, prefix `isPrefixOf` a]
  backend <- case choices of
    [] -> return OpenScadBackend
    [name] -> either (\err -> bad err >> return OpenScadBackend) return (parseBackend name)
    _ -> bad (command ++ ": specify --backend only once") >> return OpenScadBackend
  return (backend,filter (not . isPrefixOf prefix) args)

-- | A command's outcome: nothing to do on success; on failure print its
-- lines to stderr and exit with its status (1 could not run, 2 design).
finish :: Either Failure () -> IO ()
finish r = case r of
  Right () -> return ()
  Left (Failure code ls) -> mapM_ (hPutStrLn stderr) ls >> exitWith (if code == 0 then ExitFailure 1 else ExitFailure code)

-- | Split known @--flags@ from the other arguments; an unknown @--flag@
-- is reported.
partitionFlags :: [String] -> [String] -> Either String ([String], [String])
partitionFlags known args = case [a | a <- args, "--" `isPrefixOf` a, a `notElem` known] of
  bad' : _ -> Left bad'
  [] -> Right ([a | a <- args, a `elem` known], [a | a <- args, a `notElem` known])

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

usage :: Handle -> IO ()
usage h =
  mapM_
    (hPutStrLn h)
    [ "coscad " ++ showVersion version ++ " - glyph/pipeline CAD language -> OpenSCAD -> STL"
    , ""
    , "Usage:"
    , "  coscad <part.coscad> [-o part.scad]     compile one part to OpenSCAD"
    , "  coscad validate [--json] [--stdin] <part.coscad|spec.assemble>"
    , "                                          validate source, mating constraints and hardware without writing artifacts;"
    , "                                          --json prints diagnostics as JSON, --stdin reads the file's text from stdin"
    , "  coscad stl <part.coscad> [-o part.stl]  compile and render to STL (prints volume + bounds)"
    , "    [--backend=openscad|native-experimental]  opt-in native geometry experiment; default openscad"
    , "  coscad <spec.assemble>                  design stage: assembled view, packed plate(s), spec_design.json"
    , "  coscad next [--strict] <spec.assemble>  manufacturing: orient, pack beds, bedN.stl + spec_manifest.json;"
    , "                                          --strict exits 2 when any part has a printability warning"
    , "  coscad plan <spec.assemble> [--png]     build instructions: ordered steps, preload sheet, BOM"
    , "  coscad site <dir> <spec.assemble>...    static phone-friendly site: step images, search across builds"
    , "  coscad check <spec.assemble> [--keep-temp]"
    , "                                          interference / clearance check on real meshes"
    , "    --backend=openscad|native-experimental also applies to next and check"
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
    , "check overlaps, next --strict printability warnings, a part that does not fit the plate (design stage, next),"
    , "and for every command that loads an assembly: constraint loops that do not"
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
    , "Manual: man coscad   Docs: docs/2025-07-14-LANGEXTENSION.md, docs/2026-07-24-TOPOLOGICAL.md, docs/2026-07-24-MANUFACTURING.md"
    ]
