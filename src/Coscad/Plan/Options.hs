-- | The option vocabulary shared by the @.assemble@ front end and the
-- planner: part, fastener and joint hint validators, the closed set of
-- @plan@ options, and the cost-weight and setting names. This lives
-- outside 'Coscad.Assemble' so both sides can import it -- the planner
-- consumes the same validators the grammar applies, so a weight or
-- setting is named, defaulted and ranged in one place.
module Coscad.Plan.Options
  ( validatePartHint
  , validateFastenerHint
  , validateJointHint
  , validatePlanOpt
  , validFrameOption
  , planWeightNames
  , planSettingRanges
  )
where

import Control.Monad (unless)
import Data.Char (isAlphaNum, isLetter, toLower)
import Data.List (intercalate)
import Coscad.Format (showD)
import Coscad.Parser (anchorVec)
import Coscad.Validate (readFinite, suggestion)
import Text.Megaparsec (eof, parse)

validatePartHint :: (String, String) -> Either String ()
validatePartHint (key, raw) = case key of
  "material" -> enum "material" ["printed", "pla", "petg", "abs", "asa", "tpu", "nylon", "pc", "aluminium", "aluminum", "steel"] raw
  "profile" -> enum "profile" ["2020"] raw
  "preload" -> enum "preload mode" ["hand", "placed"] raw
  "ends" -> if length (splitComma raw) == 2
    then mapM_ (enum "rail end" ["open", "blocked"]) (splitComma raw)
    else Left "ends= expects two values: open,blocked (min end, max end)"
  "mass" -> numberHint key "mass in grams" (> 0) "greater than zero" raw
  "torque" -> numberHint key "torque in Nm" (> 0) "greater than zero" raw
  "brim" -> numberHint key "brim width in mm" (>= 0) "non-negative (brim=0 opts out of a plate brim)" raw
  _ -> Right ()

validateFastenerHint :: (String, String) -> Either String ()
validateFastenerHint (key, raw) = case key of
  "nut" -> enum "nut kind" ["dropin", "drop-in", "slidein", "slide-in", "hex", "captive", "none"] raw
  "head" -> enum "head" ["button", "socket"] raw
  "seat" -> validFrameOption raw
  "at" -> mapM_ (numberHint key "distance in mm" (>= 0) "non-negative") (splitComma raw)
  "through" -> numberHint key "thickness in mm" (>= 0) "non-negative" raw
  "torque" -> numberHint key "torque in Nm" (> 0) "greater than zero" raw
  "inset" -> case readFinite raw of
    Nothing -> Left "inset= needs a distance in mm (finite and non-negative)"
    Just n | n < 0 -> Left "inset= must lie within the host (a non-negative distance in mm)"
    Just _ -> Right ()
  "pocket" -> let ref = case break (== '.') raw of ("host", '.' : f) -> f; ("clamped", '.' : f) -> f; _ -> raw
              in if take 1 ref == "@" then validFrameOption ref else validFace ref
  _ -> Left ("unknown fastener option '" ++ key ++ "'" ++ suggestion ["at", "nut", "head", "seat", "through", "torque", "pocket", "inset"] key ++ "; choose at, nut, head, seat, through, torque, pocket, or inset")

validateJointHint :: (String, String) -> Either String ()
validateJointHint (key, raw) = case key of
  "pitch" -> numberHint key "thread pitch in mm" (> 0) "greater than zero" raw
  "axis" -> validFace raw
  _ -> Left ("unknown joint option '" ++ key ++ "'" ++ suggestion ["pitch", "axis"] key ++ "; choose pitch or axis")

validFrameOption :: String -> Either String ()
validFrameOption ('@':c:n) | isLetter c, all (\x -> isAlphaNum x || x == '_') n = Right ()
validFrameOption raw = Left ("expected a named frame @name, got '" ++ raw ++ "'")

numberHint :: String -> String -> (Double -> Bool) -> String -> String -> Either String ()
numberHint key units valid range raw = case readFinite raw of
  Just n | valid n -> Right ()
  _ -> Left (key ++ "=" ++ raw ++ ": expected a finite " ++ units ++ ", " ++ range)

enum :: String -> [String] -> String -> Either String ()
enum what values raw = unless (map toLower raw `elem` values) $
  Left ("unknown " ++ what ++ " '" ++ raw ++ "'" ++ suggestion values raw ++ "; choose " ++ intercalate ", " values)

splitComma :: String -> [String]
splitComma s = case break (== ',') s of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitComma rest

validFace :: String -> Either String ()
validFace s = case parse (anchorVec <* eof) "" s of
  Right (x, y, z) | abs x + abs y + abs z == 1 -> Right ()
  _ -> Left ("unknown face '" ++ s ++ "'; choose top, bot, lft, rt, fwd, or bak (aliases allowed)")

-- Search controls must not silently fall back on defaults after a typo.
-- Arbitrary manufacturing hints on parts remain an extension mechanism.
validatePlanOpt :: (String, String) -> Either String ()
validatePlanOpt (key, raw)
  | key `elem` ["beam", "astar", "step_parts"] = case number of
      Just n | n >= 1, n == fromInteger (truncate n) ->
        if (truncate n :: Integer) <= toInteger (maxBound :: Int)
          then Right ()
          else Left (at ++ "positive whole number exceeds the maximum " ++ show (maxBound :: Int))
      _ -> Left (at ++ "expected a positive whole number (e.g. " ++ key ++ "=30)")
  | key `elem` planWeightNames = case number of
      Just n | n >= 0 -> Right ()
      _ -> Left (at ++ "expected a finite, non-negative cost weight (e.g. " ++ key ++ "=1)")
  | Just (lo, hi) <- lookup key planSettingRanges = case number of
      Just n | n >= lo, n <= hi -> Right ()
      _ -> Left (at ++ "expected a number from " ++ showD lo ++ " to " ++ showD hi)
  | key == "rest" = mapM_ face (splitFaces raw)
  | otherwise = Left ("unknown plan option '" ++ key ++ "'" ++ suggestion known key ++ "; expected " ++ intercalate ", " known)
  where
    known = ["beam", "astar", "rest", "step_parts"] ++ planWeightNames ++ map fst planSettingRanges
    at = "plan " ++ key ++ "=" ++ raw ++ ": "
    number = case reads (if take 1 raw == "." then '0' : raw else raw) of
      [(n, "")] | not (isNaN n || isInfinite n) -> Just (n :: Double)
      _ -> Nothing
    splitFaces s = case break (== ',') s of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitFaces rest
    face s = case parse (anchorVec <* eof) "" s of
      Right (x, y, z) | abs x + abs y + abs z == 1 -> Right ()
      _ -> Left (at ++ "unknown rest face '" ++ s ++ "'; choose top, bot, lft, rt, fwd, or bak (aliases allowed)")

-- | Cost weights `coscad plan` reads from a `plan` line (defaults and
-- parsers in Coscad.Plan.Model).
planWeightNames :: [String]
planWeightNames = ["flip", "vertical", "driver", "shrink", "sibling", "loose", "insert", "pass", "turn", "hand", "finger", "part", "fastener", "nut"]

-- | Planner settings with their accepted ranges (defaults in Coscad.Plan.Model).
planSettingRanges :: [(String, (Double, Double))]
planSettingRanges =
  [ ("driver_angle", (0, 90)), ("standing_angle", (0, 90)), ("slender", (1, 1000))
  , ("driver_length", (1, 1000)), ("driver_margin", (0, 100)), ("phase_tau", (0, 1000))
  , ("finger_size", (1, 100)), ("contact", (0.001, 5)), ("settle", (0, 20)), ("sweep_step", (0.01, 20))
  , ("shell", (0, 20)), ("infill", (0, 1)) ]
