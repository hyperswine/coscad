{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- | Unboxed sparse linear algebra for the part stress solver: CSR matrices,
-- flat vectors and a Jacobi-preconditioned conjugate-gradient solve whose
-- result is only accepted after the true residual has been recomputed.
module Coscad.Stress.Sparse
  ( Vec, CSR (..)
  , vecFromList, vecToList, vecSize, vecAt, vecDot, vecNorm, vecLin, vecZero, vecMask
  , csrFromRows, csrApply, csrDiagonal, csrNonzeros
  , pcgSolve
  ) where

import Control.Monad (forM_, when)
import Data.Array.ST (newArray, readArray, writeArray, runSTUArray)
import Data.Array.Unboxed (UArray, (!), bounds, elems, listArray)
import Data.Array.Base (unsafeAt)
import Coscad.Validate (finite)

type Vec = UArray Int Double

data CSR = CSR { csrN :: !Int, csrPtr :: !(UArray Int Int), csrCol :: !(UArray Int Int), csrVal :: !(UArray Int Double) }

vecFromList :: [Double] -> Vec
vecFromList xs = listArray (0, length xs-1) xs

vecToList :: Vec -> [Double]
vecToList = elems

vecSize :: Vec -> Int
vecSize v = let (lo,hi)=bounds v in hi-lo+1

vecAt :: Vec -> Int -> Double
vecAt = (!)

vecZero :: Int -> Vec
vecZero n = listArray (0,n-1) (replicate n 0)

vecDot :: Vec -> Vec -> Double
vecDot a b = go 0 0
  where
    n = vecSize a
    go !i !acc | i >= n = acc
               | otherwise = go (i+1) (acc + unsafeAt a i * unsafeAt b i)

vecNorm :: Vec -> Double
vecNorm v = sqrt (vecDot v v)

-- | alpha*x + beta*y
vecLin :: Double -> Vec -> Double -> Vec -> Vec
vecLin alpha x beta y = runSTUArray $ do
  let n = vecSize x
  out <- newArray (0, n-1) 0
  forM_ [0 .. n-1] $ \i -> writeArray out i (alpha * unsafeAt x i + beta * unsafeAt y i)
  return out

-- | Zero the entries whose mask is False.
vecMask :: UArray Int Bool -> Vec -> Vec
vecMask free x = runSTUArray $ do
  let n = vecSize x
  out <- newArray (0, n-1) 0
  forM_ [0 .. n-1] $ \i -> when (free!i) (writeArray out i (x!i))
  return out

-- | Rows given as column/value pairs; duplicates are summed and columns sorted.
csrFromRows :: Int -> [[(Int,Double)]] -> CSR
csrFromRows n rows = CSR n ptr col val
  where
    merged = map (mergeSorted . sortPairs) rows
    counts = map length merged
    ptr = listArray (0,n) (scanl (+) 0 counts)
    nnz = sum counts
    col = listArray (0, nnz-1) (concatMap (map fst) merged)
    val = listArray (0, nnz-1) (concatMap (map snd) merged)
    sortPairs = foldr insertPair []
    insertPair p [] = [p]
    insertPair p@(j,_) (q@(k,_):rest) | j <= k = p:q:rest
                                      | otherwise = q : insertPair p rest
    mergeSorted ((j,a):(k,b):rest) | j == k = mergeSorted ((j,a+b):rest)
    mergeSorted (p:rest) = p : mergeSorted rest
    mergeSorted [] = []

csrNonzeros :: CSR -> Int
csrNonzeros m = csrPtr m ! csrN m

csrApply :: CSR -> Vec -> Vec
csrApply (CSR n ptr col val) x = runSTUArray $ do
  out <- newArray (0, n-1) 0
  forM_ [0 .. n-1] $ \i -> do
    let end = unsafeAt ptr (i+1)
        go !k !acc | k >= end = acc
                   | otherwise = go (k+1) (acc + unsafeAt val k * unsafeAt x (unsafeAt col k))
    writeArray out i (go (unsafeAt ptr i) 0)
  return out

csrDiagonal :: CSR -> Vec
csrDiagonal (CSR n ptr col val) = runSTUArray $ do
  out <- newArray (0, n-1) 0
  forM_ [0 .. n-1] $ \i -> do
    let end = ptr!(i+1)
        go !k !acc | k >= end = acc
                   | col!k == i = go (k+1) (acc + val!k)
                   | otherwise = go (k+1) acc
    writeArray out i (go (ptr!i) 0)
  return out

-- | Solve K u = f on the free degrees of freedom (constrained ones stay zero).
-- Returns the solution, iterations used and the recomputed relative residual.
-- Nonconvergence within the limit or loss of positive curvature is an error.
pcgSolve :: Int -> Double -> CSR -> UArray Int Bool -> Vec -> Either String (Vec, Int, Double)
pcgSolve limit tol matrix free rhs
  | not (finite normB) || normB <= 0 = Left "FEA: invalid load vector"
  | otherwise = loop 0 x0 r0 z0 z0 (vecDot r0 z0)
  where
    n = csrN matrix
    apply v = vecMask free (csrApply matrix (vecMask free v))
    diag = csrDiagonal matrix
    precondition :: Vec -> Vec
    precondition v = ssor matrix free diag v
    b = vecMask free rhs
    normB = vecNorm b
    x0 = vecZero n
    r0 = b
    z0 = precondition r0
    trueResidual x = vecNorm (vecLin 1 b (-1) (apply x)) / normB
    loop :: Int -> Vec -> Vec -> Vec -> Vec -> Double -> Either String (Vec, Int, Double)
    loop !it !x !r !_z !p !rz
      | vecNorm r / normB <= tol =
          let rel = trueResidual x
          in if finite rel && rel <= tol then Right (x, it, rel)
             else if it >= limit then exhausted
             else let r' = vecLin 1 b (-1) (apply x); z' = precondition r' in loop (it+1) x r' z' z' (vecDot r' z')
      | it >= limit = exhausted
      | not (finite curvature) || curvature <= 0 || not (finite rz) || rz <= 0 =
          Left "FEA: solver lost positive curvature or numeric range (singular or badly scaled system)"
      | otherwise =
          let alpha = rz / curvature
              x' = vecLin 1 x alpha p
              r' = vecLin 1 r (-alpha) q
              z' = precondition r'
              rz' = vecDot r' z'
              p' = vecLin 1 z' (rz'/rz) p
          in loop (it+1) x' r' z' p' rz'
      where
        q = apply p
        curvature = vecDot p q
    exhausted = Left ("FEA: solver did not converge within " ++ show limit ++ " iterations; no result accepted (try a coarser cell or check the supports)")


-- | Symmetric Gauss-Seidel (SSOR with omega = 1) applied to a residual: a
-- forward substitution with the lower triangle, a diagonal scaling and a
-- backward substitution with the upper triangle. Constrained rows act as the
-- identity, so the preconditioner stays symmetric positive definite on the
-- free subspace.
ssor :: CSR -> UArray Int Bool -> Vec -> Vec -> Vec
ssor (CSR n ptr col val) free diag r = runSTUArray $ do
  z <- newArray (0, n-1) 0
  -- forward: y_i = (r_i - sum_{j<i} a_ij y_j) / a_ii
  forM_ [0 .. n-1] $ \i -> when (unsafeAt free i) $ do
    let end = unsafeAt ptr (i+1)
        go !k !acc | k >= end = return acc
                   | otherwise = do
                       let j = unsafeAt col k
                       if j < i && unsafeAt free j then do yj <- readArray z j; go (k+1) (acc - unsafeAt val k * yj)
                       else go (k+1) acc
    acc <- go (unsafeAt ptr i) (unsafeAt r i)
    writeArray z i (acc / unsafeAt diag i)
  -- scale: w = D y
  forM_ [0 .. n-1] $ \i -> when (unsafeAt free i) $ do
    yi <- readArray z i
    writeArray z i (yi * unsafeAt diag i)
  -- backward: z_i = (w_i - sum_{j>i} a_ij z_j) / a_ii
  forM_ [n-1, n-2 .. 0] $ \i -> when (unsafeAt free i) $ do
    let end = unsafeAt ptr (i+1)
        go !k !acc | k >= end = return acc
                   | otherwise = do
                       let j = unsafeAt col k
                       if j > i && unsafeAt free j then do zj <- readArray z j; go (k+1) (acc - unsafeAt val k * zj)
                       else go (k+1) acc
    wi <- readArray z i
    acc <- go (unsafeAt ptr i) wi
    writeArray z i (acc / unsafeAt diag i)
  return z
