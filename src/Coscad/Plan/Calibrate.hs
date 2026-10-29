-- | `coscad calibrate TIMES.json`: fit the plan cost weights to timed
-- builds, so that a plan's cost becomes an estimate in seconds.
--
-- Every step in a plan JSON lists its cost terms (how many flips, bits of
-- alignment, turns, extra hands, ...). Given the seconds each step really
-- took, the weights that best explain those times are the non-negative
-- least-squares solution of  terms × weights ≈ seconds  (Lawson and
-- Hanson's active-set method). Terms no timed step used keep their
-- default and are reported as not calibrated.
--
-- TIMES.json:
--
-- > { "builds": [ { "plan": "tesseract_plan.json", "step_seconds": [40, 35, null, ...] } ] }
--
-- Plan paths are relative to TIMES.json; null marks a step that was not
-- timed (it is left out of the fit).
module Coscad.Plan.Calibrate (runCalibrate, fitWeights, nnls) where

import Coscad.Json
import Coscad.Plan.Model (defaultWeights)
import Coscad.Report (Failure (..))
import Data.List (foldl', nub, sort, transpose)
import qualified Data.Map as Map
import System.FilePath (takeDirectory, (</>))
import Text.Printf (printf)

-- | One timed step: its terms and the seconds it took.
type Row = (Map.Map String Double, Double)

runCalibrate :: FilePath -> IO (Either Failure ())
runCalibrate path = do
  raw <- readFile path
  case decode raw >>= builds of
    Left e -> failWith (path ++ ": " ++ e)
    Right bs -> do
      rowss <- mapM (loadBuild (takeDirectory path)) bs
      case sequence rowss of
        Left e -> failWith e
        Right rss -> report (concat rss)
  where
    failWith e = putStrLn ("Error: " ++ e) >> return (Left (Failure 1 []))
    builds v = do
      arr <- maybe (Left "expected {\"builds\": [...]}") Right (lookupKey "builds" v >>= asArray)
      mapM (\b -> do
              p <- maybe (Left "each build needs \"plan\": path to a plan JSON") Right (lookupKey "plan" b >>= asString)
              ts <- maybe (Left "each build needs \"step_seconds\": [...]") Right (lookupKey "step_seconds" b >>= asArray)
              Right (p, map asNumber ts)) arr
    report rows
      | null rows = failWith "no timed steps"
      | otherwise = do
          let (fitted, unused, r2, rms) = fitWeights rows
          printf "calibrate: %d timed steps, %d terms fitted\n" (length rows) (Map.size fitted)
          mapM_ (\(k, v) -> printf "  %-9s %8.2f s each\n" k v) (Map.toList fitted)
          printf "  fit: R^2 %.3f, typical step error %.1f s\n" r2 rms
          if null unused then return () else
            putStrLn ("  not calibrated (no timed step used them; they keep their defaults): " ++ unwords unused)
          if length rows < 2 * Map.size fitted
            then putStrLn "  warning: few timed steps for this many terms; time more steps before trusting the weights"
            else return ()
          putStrLn ("plan " ++ unwords [k ++ "=" ++ showW v | (k, v) <- Map.toList fitted])
          return (Right ())
    showW v = let r = fromIntegral (round (v * 100) :: Integer) / 100 :: Double in show r

loadBuild :: FilePath -> (String, [Maybe Double]) -> IO (Either String [Row])
loadBuild dir (plan, seconds) = do
  raw <- readFile (dir </> plan)
  return $ do
    v <- either (Left . ((plan ++ ": ") ++)) Right (decode raw)
    steps <- maybe (Left (plan ++ ": no \"steps\"")) Right (lookupKey "steps" v >>= asArray)
    if length steps /= length seconds
      then Left (plan ++ ": " ++ show (length steps) ++ " steps but " ++ show (length seconds) ++ " times; give null for an untimed step")
      else Right [ (terms s, t) | (s, Just t) <- zip steps seconds ]
  where
    terms s = Map.fromList [(k, n) | Just (JObject kvs) <- [lookupKey "terms" s], (k, JNumber n) <- kvs]

-- | Fitted weights (seconds), terms no step used, R², and the RMS error.
fitWeights :: [Row] -> (Map.Map String Double, [String], Double, Double)
fitWeights rows =
  let used = sort (nub [k | (ts, _) <- rows, (k, v) <- Map.toList ts, v /= 0])
      a = [[Map.findWithDefault 0 k ts | k <- used] | (ts, _) <- rows]
      b = map snd rows
      x = nnls a b
      predicted = [sum (zipWith (*) r x) | r <- a]
      mean = sum b / fromIntegral (length b)
      ssRes = sum [(p - t) ^ (2 :: Int) | (p, t) <- zip predicted b]
      ssTot = sum [(t - mean) ^ (2 :: Int) | t <- b]
      r2 = if ssTot <= 0 then 1 else 1 - ssRes / ssTot
   in (Map.fromList (zip used x), [k | k <- Map.keys defaultWeights, k `notElem` used], r2, sqrt (ssRes / fromIntegral (length b)))

-- | Non-negative least squares, min |A x - b| with x >= 0 (Lawson-Hanson).
nnls :: [[Double]] -> [Double] -> [Double]
nnls a b = outer (replicate n 0) [] (0 :: Int)
  where
    n = case a of r : _ -> length r; [] -> 0
    cols = transpose a
    residual x = zipWith (-) b [sum (zipWith (*) r x) | r <- a]
    gradient x = [sum (zipWith (*) c (residual x)) | c <- cols]
    tol = 1e-10
    outer x passive it
      | it > 3 * n + 10 = x
      | otherwise =
          let w = gradient x
              candidates = [(wj, j) | (j, wj) <- zip [0 ..] w, j `notElem` passive, wj > tol]
           in case candidates of
                [] -> x
                _ -> let j = snd (maximum candidates) in inner x (j : passive) it
    inner x passive it =
      let z = solvePassive passive
       in if all (\j -> z !! j > tol) passive
            then outer z passive (it + 1)
            else
              let alphas = [x !! j / (x !! j - z !! j) | j <- passive, z !! j <= tol, x !! j - z !! j > 0]
                  alpha = if null alphas then 0 else minimum alphas
                  x' = [xi + alpha * (zi - xi) | (xi, zi) <- zip x z]
                  passive' = [j | j <- passive, x' !! j > tol]
               in inner [if j `elem` passive' then v else 0 | (j, v) <- zip [0 ..] x'] passive' (it + 1)
    -- least squares on the passive columns only, by the normal equations
    solvePassive passive =
      let sub = [[r !! j | j <- passive] | r <- a]
          ata = [[sum (zipWith (*) ci cj) | cj <- transpose sub] | ci <- transpose sub]
          atb = [sum (zipWith (*) ci b) | ci <- transpose sub]
          sol = gauss ata atb
       in [maybe 0 id (lookup j (zip passive sol)) | j <- [0 .. n - 1]]

-- | Gaussian elimination with partial pivoting; a singular system gets
-- zeros for the unresolved unknowns.
gauss :: [[Double]] -> [Double] -> [Double]
gauss m0 v0 = back (foldl' elim (zipWith (\r x -> r ++ [x]) m0 v0) [0 .. k - 1])
  where
    k = length v0
    elim rows c =
      let (done, rest) = splitAt c rows
          pivotRow = snd (maximum [(abs (r !! c), i) | (i, r) <- zip [0 :: Int ..] rest])
          p = rest !! pivotRow
          others = [r | (i, r) <- zip [0 ..] rest, i /= pivotRow]
          reduce r = if abs (p !! c) < 1e-12 then r else let f = r !! c / p !! c in zipWith (\x y -> x - f * y) r p
       in if null rest then rows else done ++ [p] ++ map reduce others
    back rows = foldr (\(i, r) acc ->
                         let a' = r !! i
                             s = sum [r !! j * xj | (j, xj) <- zip [i + 1 ..] acc]
                          in (if abs a' < 1e-12 then 0 else (last r - s) / a') : acc) [] (zip [0 ..] rows)
