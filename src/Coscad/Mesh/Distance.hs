-- | Triangle-surface distance with a bounding-volume tree. Includes edge/edge
-- minima and crossings; it does not assume the closest point is a vertex.
module Coscad.Mesh.Distance (meshDistance, triangleDistance, segmentDistance) where

import Coscad.Linalg (V3, cross, dot3, scale3, vadd, vlen, vsub)
import Coscad.Mesh (Tri, meshBounds, triNormal)
import Coscad.Validate (finite)
import Data.List (foldl',sortOn)

pointTriDist :: V3 -> Tri -> Double
pointTriDist p (a, b, c) =
  let ab = vsub b a
      ac = vsub c a
      ap = vsub p a
      d1 = dot3 ab ap
      d2 = dot3 ac ap
   in if d1 <= 0 && d2 <= 0
        then vlen (vsub p a)
        else
          let bp = vsub p b
              d3 = dot3 ab bp
              d4 = dot3 ac bp
           in if d3 >= 0 && d4 <= d3
                then vlen (vsub p b)
                else
                  let vc = d1 * d4 - d3 * d2
                   in if vc <= 0 && d1 >= 0 && d3 <= 0
                        then vlen (vsub p (vadd a (scale3 (d1 / (d1 - d3)) ab)))
                        else
                          let cp = vsub p c
                              d5 = dot3 ab cp
                              d6 = dot3 ac cp
                           in if d6 >= 0 && d5 <= d6
                                then vlen (vsub p c)
                                else
                                  let vb = d5 * d2 - d1 * d6
                                   in if vb <= 0 && d2 >= 0 && d6 <= 0
                                        then vlen (vsub p (vadd a (scale3 (d2 / (d2 - d6)) ac)))
                                        else
                                          let va = d3 * d6 - d5 * d4
                                           in if va <= 0 && (d4 - d3) >= 0 && (d5 - d6) >= 0
                                                then
                                                  let w = (d4 - d3) / ((d4 - d3) + (d5 - d6))
                                                   in vlen (vsub p (vadd b (scale3 w (vsub c b))))
                                                else
                                                  let denom = 1 / (va + vb + vc)
                                                      v = vb * denom
                                                      w = vc * denom
                                                   in vlen (vsub p (vadd a (vadd (scale3 v ab) (scale3 w ac))))


-- The constrained minimum of two segment parameters occurs either at the
-- unconstrained stationary point or on one of the four parameter boundaries.
segmentDistance :: (V3,V3) -> (V3,V3) -> Double
segmentDistance (a,b) (c,d) = minimum (boundaries++interior)
  where
    u=vsub b a; v=vsub d c; w=vsub c a
    n=cross u v; denom=dot3 n n
    s=dot3 (cross w v) n/denom
    t=dot3 (cross w u) n/denom
    interior=[vlen (vsub (vadd a (scale3 s u)) (vadd c (scale3 t v)))
             | denom>0,finite s,finite t,s>=0,s<=1,t>=0,t<=1]
    boundaries=[pointSegment a c d,pointSegment b c d,pointSegment c a b,pointSegment d a b]
    pointSegment p x y =
      let direction=vsub y x; norm2=dot3 direction direction
          fraction=if norm2==0 then 0 else max 0 (min 1 (dot3 (vsub p x) direction/norm2))
      in vlen (vsub p (vadd x (scale3 fraction direction)))

triangleDistance :: Tri -> Tri -> Double
triangleDistance a b
  | any (`crosses` b) (edges a) || any (`crosses` a) (edges b) = 0
  | otherwise = minimum ([pointTriDist p b | p<-vertices a]++[pointTriDist p a | p<-vertices b]
                       ++[segmentDistance x y | x<-edges a,y<-edges b])
  where
    vertices (x,y,z)=[x,y,z]
    edges (x,y,z)=[(x,y),(y,z),(z,x)]
    crosses (x,y) tri@(p,_,_) =
      let normal=triNormal tri; direction=vsub y x
          denom=dot3 normal direction
          t=dot3 normal (vsub p x)/denom
      in denom/=0 && finite t && t>=0 && t<=1
         && pointTriDist (vadd x (scale3 t direction)) tri<=1e-9

type Bounds = (V3,V3)
data Tree = Leaf Bounds [Tri] | Branch Bounds Int Tree Tree
bounds :: Tree -> Bounds
bounds (Leaf b _)=b
bounds (Branch b _ _ _)=b
size :: Tree -> Int
size (Leaf _ ts)=length ts
size (Branch _ n _ _)=n

build :: [Tri] -> Tree
build ts
  | n<=8 = Leaf box ts
  | otherwise = Branch box n (build a) (build b)
  where
    n=length ts; box@(lo,hi)=meshBounds ts
    (dx,dy,dz)=vsub hi lo
    axis :: Int
    axis | dx>=dy && dx>=dz=0 | dy>=dz=1 | otherwise=2
    coordinate (x,y,z) = case axis of 0 -> x; 1 -> y; _ -> z
    centre (p,q,r)=(coordinate p+coordinate q+coordinate r)/3
    (a,b)=splitAt (n `div` 2) (sortOn centre ts)

gap :: Bounds -> Bounds -> Double
gap ((ax,ay,az),(aX,aY,aZ)) ((bx,by,bz),(bX,bY,bZ)) =
  vlen (distance ax aX bx bX,distance ay aY by bY,distance az aZ bz bZ)
  where distance lo hi low high=max 0 (max (low-hi) (lo-high))

-- | Empty or numerically unresolved inputs produce Nothing, never an infinity
-- that a diagnostic formatter could accidentally try to round to an integer.
meshDistance :: [Tri] -> [Tri] -> Maybe Double
meshDistance [] _=Nothing
meshDistance _ []=Nothing
meshDistance as@(a:_) bs@(b:_)
  | not (all validPoint [p | (x,y,z)<-as++bs,p<-[x,y,z]]) = Nothing
  | otherwise = let distance=visit (triangleDistance a b) (build as) (build bs)
                in if finite distance then Just distance else Nothing
  where
    validPoint (x,y,z)=all finite [x,y,z]
    visit best x y
      | best<=0 || gap (bounds x) (bounds y)>=best = best
      | otherwise = case (x,y) of
          (Leaf _ aa,Leaf _ bb) -> foldl' min best [triangleDistance p q | p<-aa,q<-bb]
          (Branch _ _ l r,_) | size x>=size y -> children best [(l,y),(r,y)]
          (_,Branch _ _ l r) -> children best [(x,l),(x,r)]
          (Branch _ _ l r,_) -> children best [(l,y),(r,y)]
    children best pairs = foldl' (\current (x,y) -> visit current x y) best
      (sortOn (\(x,y) -> gap (bounds x) (bounds y)) pairs)
