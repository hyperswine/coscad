-- | Coscad.Linalg unit tests.
module Tier.Linalg
  ( linalgTier
  )
where

import Coscad.Geometry (mApply, vnormed, dot3, vlen, rodMat)
import Coscad.Linalg (M3, V3, cross, downMat, downRot, matIdentity, matMul, matTranspose, rotFromUp)
import Harness

linalgTier :: T -> IO ()
linalgTier t = do
  let near a b = abs (a - b) < 1e-12
      nearV (a, b, c) (x, y, z) = near a x && near b y && near c z
      nearM (r1, r2, r3) (s1, s2, s3) = nearV r1 s1 && nearV r2 s2 && nearV r3 s3
      det ((a, b, c), (d, e, f), (g, h, i)) = a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)
      isRotation m = nearM (matMul m (matTranspose m)) matIdentity && near (det m) 1
      dirs = [(0, 0, -1), (0, 0, 1), (1, 0, 0), (-1, 0, 0), (0, -1, 0), (0, 1, 0), (1, 2, 3), (-2, 0.5, -7), (0, 0, 1e-12)] :: [V3]
      unit v = mApply matIdentity (vnormed v)
      ra = rodMat 37 (1, 2, 3) :: M3
      rb = rodMat (-110) (0.3, -1, 0.2) :: M3
      p = (1.5, -2, 4) :: V3
  assertT t "linalg: downMat sends every direction straight down"
    (and [nearV (mApply (downMat v) (unit v)) (0, 0, -1) | v <- dirs]) (show [mApply (downMat v) (unit v) | v <- dirs])
  assertT t "linalg: downMat is a proper rotation" (all (isRotation . downMat) dirs) ""
  assertT t "linalg: a downward direction needs no turn" (downRot (0, 0, -3) == Nothing && downMat (0, 0, -1) == matIdentity) ""
  assertT t "linalg: straight up turns 180 degrees about x" (downRot (0, 0, 2) == Just (180, (1, 0, 0))) (show (downRot (0, 0, 2)))
  assertT t "linalg: downMat is rodMat of downRot (the design stage and next agree)"
    (and [downMat v == maybe matIdentity (uncurry rodMat) (downRot v) | v <- dirs]) ""
  assertT t "linalg: rotFromUp turns +z onto the direction"
    (and [nearV (mApply (uncurry rodMat (rotFromUp v)) (0, 0, 1)) (unit v) | v <- dirs]) ""
  assertT t "linalg: matMul composes like function application"
    (nearV (mApply (matMul ra rb) p) (mApply ra (mApply rb p))) ""
  assertT t "linalg: matMul is associative and has an identity"
    (nearM (matMul (matMul ra rb) ra) (matMul ra (matMul rb ra)) && matMul matIdentity ra == ra && matMul ra matIdentity == ra) ""
  assertT t "linalg: cross is right-handed and orthogonal"
    (cross (1, 0, 0) (0, 1, 0) == (0, 0, 1) && near (dot3 (cross (1, 2, 3) (-4, 5, 0.5)) (1, 2, 3)) 0) ""
  assertT t "linalg: vnormed scales safely and maps zero to +z"
    (nearV (vnormed (0, 3e-200, 4e-200)) (0, 0.6, 0.8) && vnormed (0, 0, 0) == (0, 0, 1) && near (vlen (vnormed (1e200, 1e200, 0))) 1) ""
