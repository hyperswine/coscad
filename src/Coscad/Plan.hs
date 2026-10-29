-- | `coscad plan foo.assemble`: turn an assembly into a numbered build
-- sequence a person can follow without backtracking.
--
-- Model (docs/2026-09-25-PLAN.md): nodes are part instances, per-rail T-nut
-- batches, and fasteners; edges are derived requires-before relations
-- (host and clamped part before a screw, nuts before the screw and
-- before the part that covers the slot, slide-in nuts before anything
-- that seals a slot end). A state is the set of placed nodes plus the
-- face resting on the bench. Hard predicates prune (stable, supported,
-- driver accessible, nut can enter); a cost table picks among the
-- remaining linear extensions via beam search; consecutive moves on
-- one rest face merge into numbered steps.
module Coscad.Plan (processPlan, processPlanSummary, processPlanWithBackend, processPlanSummaryWithBackend, planSummary, validateHardware, PlanSummary (..), StepSummary (..), FastenerSummary (..)) where

import Coscad.Plan.Model
import Coscad.Plan.Geometry (prepareGeometry)
import Coscad.Plan.Render
import Coscad.Plan.Search

import Control.Monad (forM_, unless, when)
import Coscad.Assemble
import Coscad.Backend (Backend (..), defaultBackend)
import Coscad.Image (renderScenes)
import Coscad.Codegen (showD)
import Coscad.Geometry
import Coscad.IO (writeFileUtf8)
import Data.List (nub)
import qualified Data.Map as Map
import Coscad.Report
import System.FilePath (dropExtension, takeFileName)
import GHC.Clock (getMonotonicTime)
import Text.Printf (printf)


data StepSummary = StepSummary
  { ssRest :: String
  , ssFlip :: Bool
  , ssParts :: [String]
  , ssPreload :: [String]
  , ssFasteners :: [(String, String, String, String)] -- (spec, clamped, host, nut kind)
  , ssWarnings :: [String]
  , ssTerms :: [(String, Double)] -- the step's cost terms, before weighting
  , ssEntry :: [(String, V3)] -- each placed part and the direction it would leave by
  }
  deriving (Show)

data PlanSummary = PlanSummary
  { psParts :: [String]
  , psFastenerCount :: Int
  , psDesignErrors :: [String]
  , psSteps :: [StepSummary]
  , psCost :: Double
  , psFasteners :: [FastenerSummary]
  }
  deriving (Show)

-- Resolved hardware geometry for tools and independent placement tests.
data FastenerSummary = FastenerSummary
  { fsPosition :: V3
  , fsAxis :: V3
  , fsHeadPosition :: V3
  , fsNutSeat :: Maybe V3
  , fsNutEntry :: Maybe V3
  , fsThreadReach :: Double
  } deriving (Show)

-- | Plan an assembly file without writing anything (for tests and tools).
validateHardware :: AsmResult -> Either String ()
validateHardware ar
  | null (arFasteners ar) = Right ()
  | otherwise = do
      (_, errs) <- buildModel ar
      unless (null errs) (Left (unlines errs))

-- | Plan an assembly file without writing anything (for tests and tools).
planSummary :: FilePath -> IO (Either String PlanSummary)
planSummary path = do
  res <- loadAssembleFile [] path
  case res >>= buildModel of
    Left e -> return (Left e)
    Right (model0, errs) -> do
      geo <- prepareGeometry model0
      return $ do
        (ar, model) <- (,) <$> res <*> geo
        st <- either (Left . unlines) Right (runSearch ar model)
        Right (summarise model errs st)

summarise :: Model -> [String] -> St -> PlanSummary
summarise model errs st =
  let steps = toSteps (setStepParts (mSettings model)) (reverse (sMoves st))
      summ s = StepSummary (dirName (stRest s)) (stFlip s) (stParts s) (stNuts s)
        [(fSpec f, fClamped f, fHost f, nutName (fNut f)) | i <- stFasts s, let f = mFastById model Map.! i] (stWarn s)
        (Map.toList (stTerms s)) (stEntry s)
      hardware f = FastenerSummary (fPos f) (fFace f) (fHeadPos f) (fPocketSeat f) (fPocketEntry f) (fReach f)
   in PlanSummary (map iName (mInsts model)) (length (mFasts model)) errs (map summ steps) (sCost st) (map hardware (mFasts model))

-- ------------------------------------------------------------------
processPlan :: Emit -> Bool -> FilePath -> IO (Either Failure ())
processPlan emit png path = fmap (const ()) <$> processPlanSummary emit png path

processPlanWithBackend :: Backend -> Emit -> Bool -> FilePath -> IO (Either Failure ())
processPlanWithBackend backend emit png path = fmap (const ()) <$> processPlanSummaryWithBackend backend emit png path

-- Also return the plan metadata so site counts use physical instances and
-- actual steps, rather than declarations or stale image files.
-- Design errors (colliding screws, too little reach, an unsolvable
-- order) still write what can be written, then fail with exit 2.
processPlanSummary :: Emit -> Bool -> FilePath -> IO (Either Failure PlanSummary)
processPlanSummary = processPlanSummaryWithBackend defaultBackend

processPlanSummaryWithBackend :: Backend -> Emit -> Bool -> FilePath -> IO (Either Failure PlanSummary)
processPlanSummaryWithBackend _backend emit png path = runCommand $ do
  res <- loadAssembly emit [] path
  ar <- either (abort . asmFailure) return res
  (model0, errs) <- either (abort . inputFailure) return (buildModel ar)
  t0 <- getMonotonicTime
  model <- prepareGeometry model0 >>= either (abort . inputFailure) return
  t1 <- getMonotonicTime
  printf "plan: measured the parts' meshes, contacts and approach paths in %.1f s\n" (t1 - t0)
  let insts = mInsts model
      fasts = mFasts model
      base = dropExtension path
      title = takeFileName base
  printf "plan: %d part instances, %d fasteners on %d hosts\n" (length insts) (length fasts) (length (nub (map fHost fasts)))
  let designLines = map ((path ++ ": design: ") ++) errs
  case runSearch ar model of
    Left why -> do
      writeFileUtf8 (base ++ "_plan.md") (planMarkdown title model [] (errs ++ why))
      abort (designFailures (designLines ++ map ((path ++ ": ") ++) why))
    Right st -> do
      let steps = toSteps (setStepParts (mSettings model)) (reverse (sMoves st))
      writeFileUtf8 (base ++ "_plan.md") (planMarkdown title model steps errs)
      writeFileUtf8 (base ++ "_plan.json") (planJson path model steps errs)
      printf "Wrote %s_plan.md (%d steps, %d flips, cost %s)\n" base (length steps) (length (filter stFlip steps)) (showD (sCost st))
      forM_ (zip [1 :: Int ..] steps) $ \(k, _) -> do
        let f = base ++ "_step" ++ show k ++ ".scad"
        writeFileUtf8 f (stepScad model steps k)
      when png $ do
        rendered <- renderScenes [(base ++ "_step" ++ show k ++ ".png", stepScene model steps k) | k <- [1..length steps]]
        either (abort . inputFailure) return rendered
      putStrLn ("Wrote " ++ base ++ "_step1.." ++ show (length steps) ++ ".scad" ++ (if png then " and .png" else ""))
      unless (null errs) (abort (designFailures designLines))
      return (summarise model errs st)
