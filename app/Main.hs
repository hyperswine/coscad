-- | CLI entry point. All real logic lives in the Coscad.* library
-- modules; this file only dispatches and prints usage.
module Main (main) where

import Control.Exception (SomeException, catch)
import Coscad.Assemble (processAssemble)
import Coscad.Codegen (writeScad)
import Coscad.Next (processNext)
import Coscad.Parser (parseProgram)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (dropExtension, takeExtension)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  setLocaleEncoding utf8
  args <- getArgs
  case args of
    ["next", inputFile] -> processNext inputFile
    [inputFile] -> processFile inputFile
    _ -> do
      hPutStrLn stderr "Usage: coscad <input.coscad>"
      hPutStrLn stderr "Converts a .coscad file to .scad format"
      hPutStrLn stderr ""
      hPutStrLn stderr "Variable Syntax:"
      hPutStrLn stderr "  c₁ = ■ 10           -- cube variable"
      hPutStrLn stderr "  s₁ = ● 15           -- sphere variable"
      hPutStrLn stderr "  main = c₁ ⊕ s₁      -- main variable (gets rendered)"
      hPutStrLn stderr ""
      hPutStrLn stderr "Basic Shapes:"
      hPutStrLn stderr "  ■ 10           -- cube (10x10x10)"
      hPutStrLn stderr "  ● 15           -- sphere (radius 15)"
      hPutStrLn stderr "  ◎ 5 10         -- cylinder (radius 5, height 10)"
      hPutStrLn stderr "  ▻ 8 12         -- cone (radius 8, height 12)"
      hPutStrLn stderr "  ▬ 5 10 15      -- rectangle (5x10x15)"
      hPutStrLn stderr "  ⎏ 6 8 12       -- prism (6-sides, radius 8, height 12)"
      hPutStrLn stderr ""
      hPutStrLn stderr "BOSL2 Shapes (centered; emit include <BOSL2/std.scad>):"
      hPutStrLn stderr "  ▣ 20 15 10 2   -- cuboid 20x15x10, chamfer 2 (0 = plain)"
      hPutStrLn stderr "  ◙ 20 15 10 3   -- cuboid 20x15x10, rounding 3"
      hPutStrLn stderr "  ⌭ 5 20 1       -- cylinder r5 h20, chamfered ends 1"
      hPutStrLn stderr "  ⌽ 5 20 2       -- cylinder r5 h20, rounded ends 2"
      hPutStrLn stderr "  xcyl 5 20      -- cylinder r5 l20 along X (also ycyl, zcyl)"
      hPutStrLn stderr "  ⊚ 10 6 25      -- tube outer-r 10, inner-r 6, h 25"
      hPutStrLn stderr "  ⏢ 24 24 10 10 18 -- prismoid base 24x24 top 10x10 h 18"
      hPutStrLn stderr "  ◉ 12 4         -- torus major-r 12, minor-r 4"
      hPutStrLn stderr "  ⊿ 20 20 15     -- wedge (vertical face at -X)"
      hPutStrLn stderr ""
      hPutStrLn stderr "2D Profiles:"
      hPutStrLn stderr "  △ 10           -- triangle profile (radius 10)"
      hPutStrLn stderr "  ⬠ 8            -- pentagon profile (radius 8)"
      hPutStrLn stderr "  ⭘ 6            -- circle profile (radius 6)"
      hPutStrLn stderr ""
      hPutStrLn stderr "Boolean Operations:"
      hPutStrLn stderr "  ● 15 ⊖ ◎ 5 10   -- difference (sphere minus cylinder)"
      hPutStrLn stderr "  ● 15 ⊝ ◎ 5 10   -- difference (alternative glyph)"
      hPutStrLn stderr "  ■ 10 ⊕ ● 5     -- union (cube plus sphere)"
      hPutStrLn stderr "  ■ 10 ⊛ ● 5     -- union (alternative glyph)"
      hPutStrLn stderr ""
      hPutStrLn stderr "Advanced Operations:"
      hPutStrLn stderr "  ■ 10 ⇓ ● 5     -- hull (convex hull of cube and sphere)"
      hPutStrLn stderr "  ■ 10 ⊞ ● 5     -- minkowski sum (cube and sphere)"
      hPutStrLn stderr "  △ 8 ↯ ● 2      -- offset (offset triangle by sphere radius)"
      hPutStrLn stderr ""
      hPutStrLn stderr "Transformations:"
      hPutStrLn stderr "  χ 5 (● 3)       -- translate X by 5"
      hPutStrLn stderr "  ψ 10 (■ 4)      -- translate Y by 10"
      hPutStrLn stderr "  ζ 8 (● 2)       -- translate Z by 8"
      hPutStrLn stderr "  θ 45 (▬ 10 5 2) -- rotate X by 45 degrees"
      hPutStrLn stderr "  ϕ 90 (● 5)      -- rotate Y by 90 degrees"
      hPutStrLn stderr "  ω 30 (■ 6)      -- rotate Z by 30 degrees"
      hPutStrLn stderr "  ⬈ 2 1.5 0.5 (● 5) -- scale by (2, 1.5, 0.5)"
      hPutStrLn stderr "  ⇋ 1 0 0 (● 5)   -- mirror across plane with normal (1,0,0)"
      hPutStrLn stderr "  ⮕ 15 (△ 8)      -- extrude triangle by height 15"
      exitFailure

processFile :: FilePath -> IO ()
processFile inputFile
  | takeExtension inputFile == ".assemble" = processAssemble inputFile
processFile inputFile = do
  -- Check if input file exists
  exists <- doesFileExist inputFile
  if not exists
    then do
      hPutStrLn stderr $ "Error: File " ++ inputFile ++ " does not exist"
      exitFailure
    else do
      -- Check if input file has .coscad extension
      if takeExtension inputFile /= ".coscad"
        then do
          hPutStrLn stderr "Error: Input file must have .coscad extension"
          exitFailure
        else do
          -- Generate output filename
          let outputFile = dropExtension inputFile ++ ".scad"

          -- Try to process the file
          result <- catch (processFileContents inputFile outputFile) handleError
          case result of
            Right _ -> do
              putStrLn $ "Successfully converted " ++ inputFile ++ " to " ++ outputFile
              exitSuccess
            Left errMsg -> do
              hPutStrLn stderr $ "Error: " ++ errMsg
              exitFailure

processFileContents :: FilePath -> FilePath -> IO (Either String ())
processFileContents inputFile outputFile = do
  -- Read the .coscad file
  contents <- readFile inputFile

  -- Parse the program with variables
  case parseProgram contents of
    Right (_, mainShape) -> do
      writeScad mainShape outputFile
      return $ Right ()
    Left err -> return $ Left err

handleError :: SomeException -> IO (Either String ())
handleError e = return $ Left $ "IO Error: " ++ show e
