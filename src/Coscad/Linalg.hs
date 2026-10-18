-- | Small 3-D linear algebra shared by every stage: vectors as
-- triples, 3×3 matrices as rows, rotations, and the bed-orientation
-- helpers ('downRot', 'downMat') used by the design stage, @coscad next@
-- and the planner.
--
-- Depends only on @base@. The arithmetic is written out in a fixed
-- order (e.g. @a*x + b*y + c*z@) so that results, and so the emitted
-- @.scad@, STL and manifests, are reproducible to the last bit.
module Coscad.Linalg
  ( -- * Types
    V3,
    M3,

    -- * Vectors
    vadd,
    vsub,
    vmul,
    vneg,
    vlen,
    vnormed,
    vclamp,
    cross,
    dot3,
    scale3,

    -- * Matrices
    matIdentity,
    matTranspose,
    matMul,
    mApply,
    rodMat,
    mirrorMat,

    -- * Orientation
    rotFromUp,
    downRot,
    downMat,
  )
where

-- | A point or direction.
type V3 = (Double, Double, Double)

-- | A 3×3 matrix stored as rows.
type M3 = (V3, V3, V3)

vadd, vsub, vmul :: V3 -> V3 -> V3
vadd (a, b, c) (x, y, z) = (a + x, b + y, c + z)
vsub (a, b, c) (x, y, z) = (a - x, b - y, c - z)
vmul (a, b, c) (x, y, z) = (a * x, b * y, c * z)

vneg :: V3 -> V3
vneg (a, b, c) = (-a, -b, -c)

vlen :: V3 -> Double
vlen (a, b, c) = sqrt (a * a + b * b + c * c)

-- | Unit vector in the direction of @v@ (scaled by its largest component
-- first, so tiny and huge vectors normalise cleanly). The zero vector
-- maps to +Z.
vnormed :: V3 -> V3
vnormed (a, b, c) =
  let m = maximum [abs a, abs b, abs c]
      (x, y, z) = (a / m, b / m, c / m)
      l = sqrt (x * x + y * y + z * z)
   in if m == 0 then (0, 0, 1) else (x / l, y / l, z / l)

-- | Clamp each component to [-1, 1].
vclamp :: V3 -> V3
vclamp (a, b, c) = (cl a, cl b, cl c)
  where
    cl = max (-1) . min 1

cross :: V3 -> V3 -> V3
cross (a1, a2, a3) (b1, b2, b3) = (a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1)

dot3 :: V3 -> V3 -> Double
dot3 (a, b, c) (x, y, z) = a * x + b * y + c * z

scale3 :: Double -> V3 -> V3
scale3 s = vmul (s, s, s)

matIdentity :: M3
matIdentity = ((1, 0, 0), (0, 1, 0), (0, 0, 1))

matTranspose :: M3 -> M3
matTranspose ((a, b, c), (d, e, f), (g, h, i)) = ((a, d, g), (b, e, h), (c, f, i))

-- | Matrix product @a · b@.
matMul :: M3 -> M3 -> M3
matMul a b = matTranspose (let (x, y, z) = matTranspose b in (mApply a x, mApply a y, mApply a z))

-- | Matrix times column vector.
mApply :: M3 -> V3 -> V3
mApply (r1, r2, r3) v = (dot3 r1 v, dot3 r2 v, dot3 r3 v)

-- | Rodrigues rotation matrix: angle in degrees about an axis.
rodMat :: Double -> V3 -> M3
rodMat aDeg axis =
  let a = aDeg * pi / 180
      (x, y, z) = vnormed axis
      c = cos a
      s = sin a
      t = 1 - c
   in ( (t * x * x + c, t * x * y - s * z, t * x * z + s * y),
        (t * x * y + s * z, t * y * y + c, t * y * z - s * x),
        (t * x * z - s * y, t * y * z + s * x, t * z * z + c)
      )

-- | Reflection matrix across the plane with normal @n@.
mirrorMat :: V3 -> M3
mirrorMat n =
  let (x, y, z) = vnormed n
   in ( (1 - 2 * x * x, -2 * x * y, -2 * x * z),
        (-2 * x * y, 1 - 2 * y * y, -2 * y * z),
        (-2 * x * z, -2 * y * z, 1 - 2 * z * z)
      )

-- | Rotation (degrees, axis) taking +Z to the given direction.
rotFromUp :: V3 -> (Double, V3)
rotFromUp v =
  let d@(_, _, dz) = vnormed v
   in if vlen (vsub d (0, 0, 1)) < 1e-9
        then (0, (1, 0, 0))
        else
          if vlen (vsub d (0, 0, -1)) < 1e-9
            then (180, (1, 0, 0))
            else (acos (max (-1) (min 1 dz)) * 180 / pi, cross (0, 0, 1) d)

-- | Rotation (degrees, axis) taking direction @v@ to straight down (-Z),
-- or Nothing when @v@ already points down. Opposite +Z turns 180° about X.
downRot :: V3 -> Maybe (Double, V3)
downRot v =
  let d@(_, _, dz) = vnormed v
   in if vlen (vsub d (0, 0, -1)) < 1e-9
        then Nothing
        else
          if vlen (vsub d (0, 0, 1)) < 1e-9
            then Just (180, (1, 0, 0))
            else Just (acos (max (-1) (min 1 (-dz))) * 180 / pi, cross d (0, 0, -1))

-- | Rotation matrix taking direction @v@ to straight down; the matrix
-- form of 'downRot'.
downMat :: V3 -> M3
downMat v = maybe matIdentity (uncurry rodMat) (downRot v)

