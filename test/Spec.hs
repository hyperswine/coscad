-- | CoScad regression suite: the entry point. Each tier lives in its own
-- module under test/Tier/ (shared helpers in test/Harness.hs) and runs
-- in the order below. The main tiers:
--
--   1. diagnostics   pure: error messages carry file:line:col and say
--                    what is wrong (syntax, undefined, circular,
--                    duplicate, 2D/3D mismatch)
--   2. examples      pure: every example compiles, every .assemble
--                    loads, and the emitted .scad matches the snapshot
--                    under test/golden/scad/
--   3. geometry      every example renders natively and its mesh volume
--                    + bounds match test/golden/geometry.txt; plus pipeline
--                    cross-checks (tbracket == bracket, `coscad next`
--                    conserves volume, `coscad check` passes on bow3).
--                    With OpenSCAD 2025.05+ installed it also runs the
--                    independent OpenSCAD oracle checks; without it they
--                    are skipped and say so. COSCAD_RENDER=0 skips the tier.
--
-- COSCAD_UPDATE_GOLDEN=1 rewrites the snapshots from the current output
-- (review the diff before committing). Geometry entries within tolerance
-- keep their recorded values; only drifted or missing ones are rewritten.
-- Missing .scad snapshots are created on first run.
module Main (main) where

import Control.Monad (unless, when)
import Oracle.OpenScad (findOpenscad)
import Data.IORef
import GHC.IO.Encoding (setLocaleEncoding)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO
import Text.Printf (printf)
import Harness
import Tier.Diagnostics
import Tier.Examples
import Tier.Experiments
import Tier.Geometry
import Tier.Assembly
import Tier.Plan
import Tier.Image
import Tier.Manufacturing
import Tier.Cli
import Tier.Linalg
import Tier.Resolution
import Tier.Silence
import Tier.Stress
import Tier.Validate

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  t <- T <$> newIORef 0 <*> newIORef []
  update <- (== Just "1") <$> lookupEnv "COSCAD_UPDATE_GOLDEN"
  args <- getArgs
  unless (null args || args == ["--experiments-only"] || args == ["--images-only"]) $ do
    hPutStrLn stderr "usage: coscad-test [--experiments-only|--images-only]"
    exitFailure
  let focused = args == ["--experiments-only"]
  when (args == ["--images-only"]) (imageTier t)
  unless (args == ["--images-only"]) $ do
    unless focused (coreTiers t update)
    experimentsTier t
    stressTier t
    render <- lookupEnv "COSCAD_RENDER"
    -- OpenSCAD is only the test suite's independent oracle; the compiler
    -- never uses it. Its checks are skipped, loudly, when it is absent.
    bin <- findOpenscad
    case render of
      Just "0" -> putStrLn "\n(geometry tier skipped: COSCAD_RENDER=0)"
      _ -> do
        unless focused (geometry t update bin)
        case bin of
          Nothing -> putStrLn "\nSKIP native backend compared with OpenSCAD: OpenSCAD not found (it is only the test oracle; set COSCAD_OPENSCAD to run these)"
          Just _ -> backendOracleTier t
  n <- readIORef (tPassed t)
  fails <- readIORef (tFailed t)
  putStrLn ""
  printf "%d passed, %d failed\n" n (length fails)
  unless (null fails) $ do
    mapM_ (putStrLn . ("  - " ++)) fails
    exitFailure

coreTiers :: T -> Bool -> IO ()
coreTiers t update = do
  diagnostics t
  remainingDiagnostics t
  resolutionTier t
  printabilityTier t
  brimTier t
  packTier t
  variantNameTier t
  goldenMergeTier t
  linalgTier t
  topologyTier t
  frameTier t
  cubeTier t
  migrationTier t
  tesseractTier t
  examples t update
  planTier t
  imageTier t
  framedFastenerTier t
  cliTier t
  silenceTier t
  validateTier t
  exportListTier t
