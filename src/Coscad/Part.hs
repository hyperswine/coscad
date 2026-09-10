-- | Single-part commands: `coscad part.coscad` (to .scad) and
-- `coscad stl part.coscad` (to .stl, with the numbers a human would
-- otherwise check by hand).
module Coscad.Part (compilePart, renderPart, scadPathFor) where

import Coscad.Codegen (renderScad)
import Coscad.IO (readFileUtf8, writeFileUtf8)
import Coscad.Mesh (meshBounds, meshVolume)
import Coscad.OpenScad (renderStlFile)
import Coscad.Parser (parseProgramNamed)
import Coscad.Shape (Shape)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath (replaceExtension, takeDirectory, takeExtension)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

scadPathFor :: FilePath -> Maybe FilePath -> FilePath
scadPathFor input out = maybe (replaceExtension input ".scad") id out

die' :: String -> IO a
die' msg = hPutStrLn stderr ("Error: " ++ msg) >> exitFailure

loadPart :: FilePath -> IO Shape
loadPart input = do
  exists <- doesFileExist input
  if not exists then die' ("file not found: " ++ input) else return ()
  if takeExtension input /= ".coscad" then die' ("expected a .coscad file, got " ++ input) else return ()
  src <- readFileUtf8 input
  case parseProgramNamed input src of
    Left err -> die' err
    Right (_, shape) -> return shape

-- | .coscad -> .scad
compilePart :: FilePath -> Maybe FilePath -> IO ()
compilePart input out = do
  shape <- loadPart input
  let scadF = scadPathFor input out
  createDirectoryIfMissing True (takeDirectory scadF)
  writeFileUtf8 scadF (renderScad shape)
  putStrLn ("Wrote " ++ scadF)

-- | .coscad -> .scad -> .stl, printing triangle count, volume, and bounds.
renderPart :: FilePath -> Maybe FilePath -> IO ()
renderPart input out = do
  shape <- loadPart input
  let stlF = maybe (replaceExtension input ".stl") id out
      scadF = replaceExtension stlF ".scad"
  createDirectoryIfMissing True (takeDirectory stlF)
  writeFileUtf8 scadF (renderScad shape)
  r <- renderStlFile scadF stlF
  case r of
    Left err -> die' err
    Right tris -> do
      let ((x0, y0, z0), (x1, y1, z1)) = meshBounds tris
      putStrLn ("Wrote " ++ scadF)
      printf "Wrote %s: %d triangles, volume %.3f mm^3, bounds [%.2f %.2f %.2f] .. [%.2f %.2f %.2f]\n"
        stlF (length tris) (meshVolume tris) x0 y0 z0 x1 y1 z1
