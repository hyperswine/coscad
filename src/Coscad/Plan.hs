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
module Coscad.Plan (processPlan, processPlanSummary, planSummary, validateHardware, PlanSummary (..), StepSummary (..), FastenerSummary (..)) where

import Coscad.Plan.Model
import Coscad.Plan.Render
import Coscad.Plan.Search

import Control.Monad (forM_, unless, when)
import Coscad.Assemble
import Coscad.Codegen (showD)
import Coscad.Geometry
import Coscad.IO (writeFileUtf8)
import Coscad.OpenScad (openscadFullRender, openscadPngArgs, runOpenscadRaw)
import Data.List (nub)
import qualified Data.Map as Map
import System.Exit (ExitCode (..))
import Coscad.Report
import System.FilePath (dropExtension, takeFileName)
import Text.Printf (printf)


data StepSummary = StepSummary
  { ssRest :: String
  , ssFlip :: Bool
  , ssParts :: [String]
  , ssPreload :: [String]
  , ssFasteners :: [(String, String, String, String)] -- (spec, clamped, host, nut kind)
  , ssWarnings :: [String]
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
  return $ do
    ar <- res
    (model, errs) <- buildModel ar
    st <- either (Left . unlines) Right (runSearch ar model)
    Right (summarise model errs st)

summarise :: Model -> [String] -> St -> PlanSummary
summarise model errs st =
  let steps = toSteps (reverse (sMoves st))
      summ s = StepSummary (dirName (stRest s)) (stFlip s) (stParts s) (stNuts s)
        [(fSpec f, fClamped f, fHost f, nutName (fNut f)) | i <- stFasts s, let f = mFastById model Map.! i] (stWarn s)
      hardware f = FastenerSummary (fPos f) (fFace f) (fHeadPos f) (fPocketSeat f) (fPocketEntry f) (fReach f)
   in PlanSummary (map iName (mInsts model)) (length (mFasts model)) errs (map summ steps) (sCost st) (map hardware (mFasts model))

-- ------------------------------------------------------------------
processPlan :: Emit -> Bool -> FilePath -> IO (Either Failure ())
processPlan emit png path = fmap (const ()) <$> processPlanSummary emit png path

-- Also return the plan metadata so site counts use physical instances and
-- actual steps, rather than declarations or stale image files.
-- Design errors (colliding screws, too little reach, an unsolvable
-- order) still write what can be written, then fail with exit 2.
processPlanSummary :: Emit -> Bool -> FilePath -> IO (Either Failure PlanSummary)
processPlanSummary emit png path = runCommand $ do
  res <- loadAssembly emit [] path
  ar <- either (abort . asmFailure) return res
  (model, errs) <- either (abort . inputFailure) return (buildModel ar)
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
      let steps = toSteps (reverse (sMoves st))
      writeFileUtf8 (base ++ "_plan.md") (planMarkdown title model steps errs)
      writeFileUtf8 (base ++ "_plan.json") (planJson path model steps errs)
      printf "Wrote %s_plan.md (%d steps, %d flips, cost %s)\n" base (length steps) (length (filter stFlip steps)) (showD (sCost st))
      full <- if png then openscadFullRender else return False
      when (png && not full) $
        emit $ Note "step images use the OpenSCAD preview (coincident faces may flicker); OpenSCAD 2025.05 or newer renders them fully at the same speed"
      forM_ (zip [1 :: Int ..] steps) $ \(k, _) -> do
        let f = base ++ "_step" ++ show k ++ ".scad"
        writeFileUtf8 f (stepScad model steps k)
        when png $ do
          r <- runOpenscadRaw (openscadPngArgs full (base ++ "_step" ++ show k ++ ".png") f)
          case r of
            Right (ExitSuccess, _, _) -> return ()
            Right (_, _, e) -> emit (Warning ("png for step " ++ show k ++ ": " ++ take 200 e))
            Left e -> emit (Warning e)
      putStrLn ("Wrote " ++ base ++ "_step1.." ++ show (length steps) ++ ".scad" ++ (if png then " and .png" else ""))
      unless (null errs) (abort (designFailures designLines))
      return (summarise model errs st)
