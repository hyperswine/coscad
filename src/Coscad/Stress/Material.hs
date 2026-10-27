-- | Default material table for `coscad stress`. Values are typical datasheet
-- figures for printed polymers and common metals, in MPa and g/cm3; they are
-- starting points, not measurements of a particular spool or print.
module Coscad.Stress.Material
  ( Material (..), materialNamed, materialTable, materialNames ) where

import Data.Char (toLower)

data Material = Material
  { matName :: String
  , matYoungMPa :: Double
  , matPoisson :: Double
  , matStrengthMPa :: Double        -- ^ tensile strength in the layer plane (or the metal's yield strength)
  , matAcrossLayersMPa :: Double    -- ^ tensile strength across printed layers (same as in-plane for metals)
  , matDensity :: Double            -- ^ g/cm3
  , matPrinted :: Bool
  } deriving (Eq, Show)

materialTable :: [Material]
materialTable =
  [ Material "pla"       3500 0.36 50 35 1.24 True
  , Material "petg"      2100 0.38 45 32 1.27 True
  , Material "abs"       2200 0.35 35 25 1.04 True
  , Material "asa"       2300 0.35 40 28 1.07 True
  , Material "pa"        1800 0.40 50 38 1.14 True   -- nylon, dry
  , Material "tpu"         30 0.45 30 25 1.21 True   -- 95A, elastic range only
  , Material "aluminium" 69000 0.33 240 240 2.70 False -- 6061-T6 yield
  , Material "steel"    200000 0.30 250 250 7.85 False -- mild steel yield
  ]

materialNames :: [String]
materialNames = map matName materialTable

materialNamed :: String -> Either String Material
materialNamed raw = case [m | m <- materialTable, matName m == name] of
  (m:_) -> Right m
  [] -> Left ("unknown material '" ++ raw ++ "'; choose one of " ++ unwords materialNames)
  where name = map toLower raw
