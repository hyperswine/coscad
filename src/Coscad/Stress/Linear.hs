{-# LANGUAGE BangPatterns #-}
-- | Internal sparse vector solver, shared by validated Hex8 and Tet4 frontends.
module Coscad.Stress.Linear (Vector, Rows, vector, rowDot, dot, pcg) where
import qualified Data.Array as A
import qualified Data.Array.Unboxed as U
import Data.List (foldl')
import Coscad.Validate (finite)
type Vector = U.UArray Int Double
type Rows = A.Array Int [(Int,Double)]

vector :: [Double] -> Vector
vector xs=U.listArray (0,length xs-1) xs

rowDot :: [(Int,Double)] -> Vector -> Double
rowDot row x=foldl' (\s (j,v) -> s+v*(x U.! j)) 0 row

dot :: Vector -> Vector -> Double
dot a b=foldl' (+) 0 (zipWith (*) (U.elems a) (U.elems b))

combine :: (Double -> Double -> Double) -> Vector -> Vector -> Vector
combine f a b=vector (zipWith f (U.elems a) (U.elems b))

-- Jacobi-preconditioned CG. Check the actual residual before accepting a
-- solution; an iteration cap or loss of positive curvature is an error.
pcg :: Int -> Double -> (Vector -> Vector) -> Vector -> Vector -> Either String (Vector,Int,Double)
pcg limit tol apply diag rhs = go 0 zero rhs z0 (dot rhs z0)
  where
    zero=vector (replicate (U.rangeSize (U.bounds rhs)) 0)
    normB=sqrt (dot rhs rhs)
    pre r=combine (/) r diag
    z0=pre rhs
    go !it !x !r !p !rz
      | not (finite normB) || normB<=0 = Left "FEA: invalid load norm"
      | sqrt (dot r r)/normB <= tol =
          let actual=combine (-) rhs (apply x); rel=sqrt (dot actual actual)/normB
          in if finite rel && rel<=tol then Right (x,it,rel)
             else if it>=limit then exhausted
             else let z=pre actual in go (it+1) x actual z (dot actual z)
      | it>=limit = exhausted
      | not (finite curvature) || curvature<=0 || not (finite rz) || rz<=0 =
          Left "FEA: solver lost positive curvature or numeric range"
      | otherwise =
          let alpha=rz/curvature
              x'=combine (\a b -> a+alpha*b) x p
              r'=combine (\a b -> a-alpha*b) r ap
              z'=pre r'; rz'=dot r' z'; beta=rz'/rz
              p'=combine (\a b -> a+beta*b) z' p
          in go (it+1) x' r' p' rz'
      where
        ap=apply p
        curvature=dot p ap
    exhausted=Left ("FEA: solver did not converge within " ++ show limit ++ " iterations; no result accepted")

