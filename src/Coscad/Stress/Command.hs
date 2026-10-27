-- | `coscad stress part.coscad [-o part_stress.json] [--full] [--backend=...]`:
-- render the part with the selected geometry backend, analyse it with the
-- directives in the file, write the report and exit 2 when the requested
-- safety factor is not met.
module Coscad.Stress.Command (processStress) where

import Coscad.Backend (Backend, backendName, renderShape)
import Coscad.Geometry (anchorWarnings, formatAnchorWarning)
import Coscad.IO (readFileUtf8, writeFileUtf8)
import Coscad.Json (encodePretty)
import Coscad.Parser (parseProgramFull)
import Coscad.Report (Emit, Diag (..), Failure, abort, designFailures, inputFailure, runCommand)
import Coscad.Stress.Part
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (dropExtension, takeDirectory, takeExtension)

processStress :: Backend -> Emit -> Bool -> FilePath -> Maybe FilePath -> IO (Either Failure ())
processStress backend emit full input out = runCommand $ do
  exists <- doesFileExist input
  if not exists then die' ("file not found: " ++ input) else return ()
  if takeExtension input /= ".coscad" then die' ("expected a .coscad file, got " ++ input) else return ()
  src <- readFileUtf8 input
  (shape, analysis) <- case parseProgramFull input src of
    Left err -> die' err
    Right (_, shape, analysis) -> return (shape, analysis)
  mapM_ (emit . Warning . formatAnchorWarning input) (anchorWarnings shape)
  setup <- either die' return (setupFrom analysis)
  let jsonF = maybe (dropExtension input ++ "_stress.json") id out
      base = dropExtension jsonF
      stlF = base ++ ".stl"; scadF = base ++ ".scad"
  createDirectoryIfMissing True (takeDirectory jsonF)
  tris <- renderShape backend scadF stlF shape >>= either die' return
  report <- either die' return (analysePart analysis setup shape tris)
  writeFileUtf8 jsonF (encodePretty (reportJson full input (backendName backend) report) ++ "\n")
  mapM_ putStrLn (reportSummary report)
  putStrLn ("Wrote " ++ jsonF ++ " (mesh analysed: " ++ stlF ++ ")")
  if srSafetyInPlane report >= suSafety (srSetup report) && srSafetyAcross report >= suSafety (srSetup report)
    then return ()
    else abort (designFailures [input ++ ": safety factor " ++ show (minimum [srSafetyInPlane report, srSafetyAcross report]) ++ " is below the requested " ++ show (suSafety (srSetup report))])
  where
    die' :: String -> IO a
    die' = abort . inputFailure
