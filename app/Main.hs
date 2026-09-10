-- | CLI entry point. All real logic lives in the Coscad.* library
-- modules; this file only parses arguments and dispatches.
module Main (main) where

import Coscad.Assemble (processAssemble)
import Coscad.Check (processCheckWith)
import Coscad.Doctor (runDoctor)
import Coscad.Next (processNext)
import Coscad.Part (compilePart, renderPart)
import Data.Version (showVersion)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Paths_coscad (version)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeExtension)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout)

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  case args of
    [] -> usage >> exitFailure
    (a : _) | a `elem` ["--help", "-h", "help"] -> usage >> exitSuccess
    (a : _) | a `elem` ["--version", "-V", "version"] -> putStrLn ("coscad " ++ showVersion version)
    ["doctor"] -> runDoctor
    ("stl" : rest) -> withInOut "stl" rest renderPart
    ["next", f] -> needExt ".assemble" f >> processNext f
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

bad :: String -> IO ()
bad msg = hPutStrLn stderr ("Error: " ++ msg) >> exitFailure

usage :: IO ()
usage =
  mapM_
    putStrLn
    [ "coscad " ++ showVersion version ++ " - glyph/pipeline CAD language -> OpenSCAD -> STL"
    , ""
    , "Usage:"
    , "  coscad <part.coscad> [-o part.scad]     compile one part to OpenSCAD"
    , "  coscad stl <part.coscad> [-o part.stl]  compile and render to STL (prints volume + bounds)"
    , "  coscad <spec.assemble>                  design stage: assembled view, packed plate(s), manifest"
    , "  coscad next <spec.assemble>             manufacturing: orient, pack beds, bedN.stl + manifest"
    , "  coscad check <spec.assemble> [--keep-temp]"
    , "                                          interference / clearance check on real meshes"
    , "  coscad doctor                           verify OpenSCAD + BOSL2 + an end-to-end render"
    , "  coscad --version | --help"
    , ""
    , "Environment:"
    , "  COSCAD_OPENSCAD  OpenSCAD binary or wrapper script (else PATH, then the app bundle)"
    , "  COSCAD_BOSL2     BOSL2 checkout directory (else the OpenSCAD library folders)"
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
    , "Docs: docs/LANGEXTENSION.md, docs/TOPOLOGICAL.md, docs/MANUFACTURING.md"
    ]
