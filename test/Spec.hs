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
--   3. geometry      needs OpenSCAD: every example renders, and its
--                    mesh volume + bounds match test/golden/geometry.txt;
--                    plus a few cross-checks (tbracket == bracket,
--                    `coscad next` conserves volume, `coscad check`
--                    passes on bow3). Skipped when OpenSCAD is not
--                    found or COSCAD_RENDER=0.
--
-- COSCAD_UPDATE_GOLDEN=1 rewrites the snapshots from the current output
-- (review the diff before committing). Geometry entries within tolerance
-- keep their recorded values; only drifted or missing ones are rewritten.
-- Missing .scad snapshots are created on first run.
module Main (main) where

import Control.Monad (unless)
import Coscad.OpenScad (findOpenscad)
import Data.IORef
import GHC.IO.Encoding (setLocaleEncoding)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO
import Text.Printf (printf)
import Harness
import Tier.Diagnostics
import Tier.Examples
import Tier.Geometry
import Tier.Assembly
import Tier.Plan
import Tier.Manufacturing
import Tier.Cli
import Tier.Linalg
import Tier.Resolution

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  t <- T <$> newIORef 0 <*> newIORef []
  update <- (== Just "1") <$> lookupEnv "COSCAD_UPDATE_GOLDEN"
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
  framedFastenerTier t
  cliTier t
  exportListTier t
  render <- lookupEnv "COSCAD_RENDER"
  bin <- findOpenscad
  case (render, bin) of
    (Just "0", _) -> putStrLn "\n(geometry tier skipped: COSCAD_RENDER=0)"
    (_, Nothing) -> putStrLn "\n(geometry tier skipped: OpenSCAD not found; set COSCAD_OPENSCAD)"
    (_, Just b) -> geometry t update b
  n <- readIORef (tPassed t)
  fails <- readIORef (tFailed t)
  putStrLn ""
  printf "%d passed, %d failed\n" n (length fails)
  unless (null fails) $ do
    mapM_ (putStrLn . ("  - " ++)) fails
    exitFailure
