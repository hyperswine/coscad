-- | Number formatting shared by code generation and diagnostics, kept
-- apart so mesh-level modules need not depend on the OpenSCAD emitter.
module Coscad.Format (showD, roundTo3) where

-- | A value rounded to three decimals (a thousandth of a millimetre),
-- the resolution the manifests, messages and plan JSON report at.
roundTo3 :: Double -> Double
roundTo3 x = fromIntegral (round (x * 1000) :: Integer) / 1000

-- | Numbers as OpenSCAD reads them: rounded to 1e-9 (rotation noise
-- like 3.06e-16 becomes 0), integers without a trailing ".0", no "-0".
showD :: Double -> String
showD x
  | isNaN x || isInfinite x = show x
  | abs x > 1e299 = show x -- avoid overflow in the decimal-rounding step
  | otherwise =
      let r = fromIntegral (round (x * 1e9) :: Integer) / 1e9 :: Double
          r' = if r == 0 then 0 else r
          i = round r' :: Integer
       in if fromIntegral i == r' then show i else show r'
