-- | Shared affine composition and tagged instance traversal.
module Coscad.Instance (Aff, affApply, affCompose, instancesOf, physicalInstances, numberedInstances, rigid) where
import Coscad.Geometry
import Coscad.Shape
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Coscad.Validate (finite)

type Aff = (M3, V3)

affApply :: Aff -> V3 -> V3
affApply (m, t) p = vadd (mApply m p) t

affCompose :: Aff -> Aff -> Aff
affCompose (m1, t1) (m2, t2) = (matMul m1 m2, vadd (mApply m1 t2) t1)

diag :: V3 -> M3
diag (x, y, z) = ((x, 0, 0), (0, y, 0), (0, 0, z))

-- | Every Tag'd subtree with its accumulated world transform.
instancesOf :: Shape -> [(String, Aff, Shape)]
instancesOf = go (matIdentity, (0, 0, 0))
  where
    go aff s = case s of
      Tag n x -> [(n, aff, x)]
      Datum _ _ x -> go aff x
      Frame _ _ _ x -> go aff x
      Tx d x -> go (affCompose aff (matIdentity, (d, 0, 0))) x
      Ty d x -> go (affCompose aff (matIdentity, (0, d, 0))) x
      Tz d x -> go (affCompose aff (matIdentity, (0, 0, d))) x
      Translate v x -> go (affCompose aff (matIdentity, v)) x
      Rx a x -> go (affCompose aff (rodMat a (1, 0, 0), (0, 0, 0))) x
      Ry a x -> go (affCompose aff (rodMat a (0, 1, 0), (0, 0, 0))) x
      Rz a x -> go (affCompose aff (rodMat a (0, 0, 1), (0, 0, 0))) x
      RotAxis a v x -> go (affCompose aff (rodMat a v, (0, 0, 0))) x
      Transform3 m t x -> go (affCompose aff (m,t)) x
      Scale v x -> go (affCompose aff (diag v, (0, 0, 0))) x
      Mirror n x -> go (affCompose aff (mirrorMat n, (0, 0, 0))) x
      -- Union keeps physical instances on both sides; a difference's
      -- cutter is negative space and is never a placed part (it is
      -- consumed by the cut). What hull, intersection and minkowski do
      -- to tagged subtrees is still an open question (see
      -- docs/2026-11-08-ARCHITECTURE-REVIEW.md).
      Diff a _ -> go aff a
      Union xs -> concatMap (go aff) xs
      Intersection xs -> concatMap (go aff) xs
      Hull xs -> concatMap (go aff) xs
      Minkowski xs -> concatMap (go aff) xs
      Extrude _ x -> go aff x
      Revolve _ x -> go aff x
      RevolveSeg _ _ x -> go aff x
      Offset _ x -> go aff x
      _ -> []


-- | Analysis cannot infer separate bodies after an assembly-level Boolean,
-- hull or extrusion. Such operations belong inside a referenced .coscad part.
physicalInstances :: Shape -> Either String [(String, Aff, Shape)]
physicalInstances s = do
  check s
  pure (instancesOf s)
  where
    check (Tag _ _) = Right ()
    check Empty = Right ()
    check x = case x of
      Union xs -> mapM_ check xs
      Datum _ _ a -> check a
      Frame _ _ _ a -> check a
      Tx _ a -> check a
      Ty _ a -> check a
      Tz _ a -> check a
      Translate _ a -> check a
      Rx _ a -> check a
      Ry _ a -> check a
      Rz _ a -> check a
      RotAxis _ _ a -> check a
      Transform3 _ _ a -> check a
      Scale _ a -> check a
      Mirror _ a -> check a
      _ -> Left "instances: assembly-level geometry operation or unreferenced solid has no unambiguous physical identity; put geometry inside a referenced .coscad part"

numberedInstances :: [(String, Aff, Shape)] -> [(String, String, Aff, Shape)]
numberedInstances occ = reverse (snd (foldl' step (Map.empty, []) occ))
  where
    counts = Map.fromListWith (+) [(n,1::Int) | (n,_,_) <- occ]
    step (seen,acc) (n,aff,sh) =
      let k = Map.findWithDefault (0::Int) n seen + 1
          name = if Map.findWithDefault 1 n counts > 1 then n ++ "#" ++ show k else n
      in (Map.insert n k seen, (name,n,aff,sh):acc)

rigid :: Aff -> Bool
rigid (m,t) = all finite (coords t ++ concatMap coords [a,b,c]) &&
  all (\v -> abs (vlen v - 1) < 1e-8) [a,b,c] &&
  all (\v -> abs v < 1e-8) [dot3 a b,dot3 a c,dot3 b c]
  where
    (a,b,c) = m
    coords (x,y,z) = [x,y,z]
