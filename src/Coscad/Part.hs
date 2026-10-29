-- | Single-part commands: `coscad part.coscad` (to .scad) and
-- `coscad stl part.coscad` (to .stl, with the numbers a human would
-- otherwise check by hand).
module Coscad.Part (compilePart, renderPart, renderPartWith, scadPathFor) where

import Coscad.Backend (Backend (..), renderShape)
import Coscad.Codegen (renderScad)
import Coscad.Geometry (anchorWarnings, formatAnchorWarning)
import Coscad.IO (readFileUtf8, writeFileUtf8)
import Coscad.Mesh (meshBounds, meshVolume)
import Coscad.Parser (parseProgramNamed)
import Coscad.Report (Emit, Diag (..), Failure, abort, inputFailure, runCommand)
import Coscad.Shape (Shape)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (replaceExtension, takeDirectory, takeExtension)
import Text.Printf (printf)

scadPathFor :: FilePath -> Maybe FilePath -> FilePath
scadPathFor input out = maybe (replaceExtension input ".scad") id out

die' :: String -> IO a
die' = abort . inputFailure

loadPart :: Emit -> FilePath -> IO Shape
loadPart emit input = do
  exists <- doesFileExist input
  if not exists then die' ("file not found: " ++ input) else return ()
  if takeExtension input /= ".coscad" then die' ("expected a .coscad file, got " ++ input) else return ()
  src <- readFileUtf8 input
  case parseProgramNamed input src of
    Left err -> die' err
    Right (_, shape) -> do
      mapM_ (emit . Warning . formatAnchorWarning input) (anchorWarnings shape)
      return shape

-- | .coscad -> .scad
compilePart :: Emit -> FilePath -> Maybe FilePath -> IO (Either Failure ())
compilePart emit input out = runCommand $ do
  shape <- loadPart emit input
  let scadF = scadPathFor input out
  createDirectoryIfMissing True (takeDirectory scadF)
  writeFileUtf8 scadF (renderScad shape)
  putStrLn ("Wrote " ++ scadF)

-- | .coscad -> .scad -> .stl, printing triangle count, volume, and bounds.
renderPart :: Emit -> FilePath -> Maybe FilePath -> IO (Either Failure ())
renderPart = renderPartWith Native

renderPartWith :: Backend -> Emit -> FilePath -> Maybe FilePath -> IO (Either Failure ())
renderPartWith backend emit input out = runCommand $ do
  shape <- loadPart emit input
  let stlF = maybe (replaceExtension input ".stl") id out
      scadF = replaceExtension stlF ".scad"
  createDirectoryIfMissing True (takeDirectory stlF)
  r <- renderShape backend scadF stlF shape
  case r of
    Left err -> die' err
    Right tris -> do
      let ((x0, y0, z0), (x1, y1, z1)) = meshBounds tris
      putStrLn ("Wrote " ++ scadF)
      printf "Wrote %s: %d triangles, volume %.3f mm^3, bounds [%.2f %.2f %.2f] .. [%.2f %.2f %.2f]\n"
        stlF (length tris) (meshVolume tris) x0 y0 z0 x1 y1 z1
