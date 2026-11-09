-- | Analyse before publishing. A stable JSON report references immutable
-- generation files; replacing that one JSON file is the publication point.
module Coscad.Stress.Command (processStress) where

import Control.Exception (mask, onException, try, throwIO)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Numeric (showHex)
import Coscad.Backend (Backend, backendName, renderShape)
import Coscad.Geometry (anchorWarnings, formatAnchorWarning)
import Coscad.IO (writeFileUtf8)
import Coscad.Json (JValue (..), encodePretty)
import Coscad.Parser (parseProgramFull)
import Coscad.Report (Emit, Diag (..), Failure, abort, designFailures, inputFailure, runCommand)
import Coscad.Stress.Part
import System.Directory (createDirectory, createDirectoryIfMissing, doesFileExist,
                         canonicalizePath, removeDirectoryRecursive, renameFile)
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeFileName, (</>))
import System.IO.Error (isAlreadyExistsError)

processStress :: Backend -> Emit -> Bool -> FilePath -> Maybe FilePath -> IO (Either Failure ())
processStress backend emit full input out = runCommand $ do
  exists <- doesFileExist input
  if exists then return () else die ("file not found: " ++ input)
  if takeExtension input == ".coscad" then return () else die ("expected a .coscad file, got " ++ input)
  -- One byte snapshot supplies both the parser and provenance; a concurrent
  -- edit of the input cannot make the saved source/hash describe another run.
  sourceBytes <- BS.readFile input
  src <- either (die . ("invalid UTF-8: " ++) . show) (return . Text.unpack) (Text.decodeUtf8' sourceBytes)
  (shape, analysis) <- case parseProgramFull input src of
    Left err -> die err
    Right (_, shape, analysis, _) -> return (shape, analysis)
  mapM_ (emit . Warning . formatAnchorWarning input) (anchorWarnings shape)
  setup <- either die return (setupFrom analysis)
  let jsonF = maybe (dropExtension input ++ "_stress.json") id out
  sourcePath <- canonicalizePath input
  outputPath <- canonicalizePath jsonF
  if sourcePath == outputPath then die "stress: report path must not overwrite the source" else return ()
  createDirectoryIfMissing True (takeDirectory jsonF)
  -- Deferred async exceptions cannot remove a generation after the report
  -- starts referencing it. Before publication, failures clean up only this run.
  (report, stlF) <- mask $ \restore -> do
    dir <- generationDir jsonF
    let cleanup = removeDirectoryRecursive dir
        stlF = dir </> "mesh.stl"
        scadF = dir </> "model.scad"
        sourceF = dir </> "source.coscad"
        tempReport = dir </> "report.tmp"
        relative path = takeFileName dir </> takeFileName path
        artifact path bytes = JObject [("path",JString (relative path)),("sha256",JString (digest bytes))]
    report <- restore (do
      tris <- renderShape backend scadF stlF shape >>= either die return
      report <- either die return (analysePart analysis setup shape tris)
      BS.writeFile sourceF sourceBytes
      meshBytes <- BS.readFile stlF
      scadBytes <- BS.readFile scadF
      let provenance = JObject [("source",artifact sourceF sourceBytes),
                                ("mesh",artifact stlF meshBytes),("scad",artifact scadF scadBytes)]
          result = case reportJson full input (backendName backend) report of
            JObject fields -> JObject (fields ++ [("artifacts",provenance)])
            other -> other
      writeFileUtf8 tempReport (encodePretty result)
      return report) `onException` cleanup
    renameFile tempReport jsonF `onException` cleanup
    return (report, stlF)
  mapM_ putStrLn (reportSummary report)
  putStrLn ("Wrote " ++ jsonF ++ " (mesh analysed: " ++ stlF ++ ")")
  if srSafetyInPlane report >= suSafety (srSetup report) && srSafetyAcross report >= suSafety (srSetup report)
    then return ()
    else abort (designFailures [input ++ ": safety factor " ++ show (minimum [srSafetyInPlane report, srSafetyAcross report]) ++ " is below the requested " ++ show (suSafety (srSetup report))])
  where
    die :: String -> IO a
    die = abort . inputFailure

-- Atomic mkdir claims a generation even with concurrent runs. Existing
-- generations remain immutable for readers holding an older report.
generationDir :: FilePath -> IO FilePath
generationDir report = go (1 :: Int)
  where
    go n = do
      let path = report ++ ".assets-" ++ show n
      result <- try (createDirectory path)
      case result of
        Right () -> return path
        Left e | isAlreadyExistsError e && n < 100000 -> go (n+1)
        Left e -> throwIO e

digest :: BS.ByteString -> String
digest = concatMap hex . BS.unpack . SHA256.hash
  where hex x = let s = showHex x "" in replicate (2-length s) '0' ++ s
