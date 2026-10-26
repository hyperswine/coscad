-- | Connectivity is distinct from position: coincident vertices need not be
-- connected. The constructor is hidden so foreign indices cannot reach array
-- indexing without validation. Triangle-soup welding is an explicit import.
module Coscad.Mesh.Indexed
  ( IndexedMesh, indexedMesh, indexedVertices, indexedFaces, indexedTriangles
  , emptyIndexed, weldTriangles, mapIndexed, reverseIndexed
  , indexedComponents, concatenateIndexed
  ) where

import Control.Monad (unless)
import qualified Data.Array as A
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Coscad.Linalg (V3)
import Coscad.Mesh (Tri, triArea)
import Coscad.Validate (finite)

data IndexedMesh = IndexedMesh [V3] [(Int,Int,Int)] deriving (Eq, Show)

indexedVertices :: IndexedMesh -> [V3]
indexedVertices (IndexedMesh vs _) = vs
indexedFaces :: IndexedMesh -> [(Int,Int,Int)]
indexedFaces (IndexedMesh _ fs) = fs

emptyIndexed :: IndexedMesh
emptyIndexed = IndexedMesh [] []

indexedMesh :: [V3] -> [(Int,Int,Int)] -> Either String IndexedMesh
indexedMesh vs fs = do
  let n=length vs
  unless (n<=1500000 && length fs<=500000) (Left "indexed mesh exceeds 1500000 vertices or 500000 triangles")
  unless (all (all finite . components) vs) (Left "indexed mesh has a nonfinite vertex")
  unless (all (\(a,b,c) -> a/=b && b/=c && c/=a && all (\i -> i>=0 && i<n) [a,b,c]) fs)
    (Left "indexed mesh has invalid triangle indices")
  let mesh=IndexedMesh vs fs
  unless (all (\t -> finite (triArea t) && triArea t>0) (indexedTriangles mesh))
    (Left "indexed mesh has a degenerate or unresolved triangle")
  return mesh
  where components (x,y,z)=[x,y,z]

indexedTriangles :: IndexedMesh -> [Tri]
indexedTriangles (IndexedMesh vs fs) = [(points A.! a,points A.! b,points A.! c) | (a,b,c)<-fs]
  where points=A.listArray (0,length vs-1) vs

-- | Only for primitive tessellation and legacy STL/soup input. Never apply this
-- to an indexed CSG result: it would identify unrelated coincident vertices.
weldTriangles :: [Tri] -> Either String IndexedMesh
weldTriangles tris = do
  unless (length tris<=500000) (Left "indexed mesh exceeds 500000 triangles")
  let allPoints=[p | (a,b,c)<-tris,p<-[a,b,c]]
  unless (all (\(x,y,z) -> all finite [x,y,z]) allPoints) (Left "indexed mesh has a nonfinite vertex")
  let vertices=Map.keys (Map.fromList [(p,()) | p<-allPoints])
      indices=Map.fromList (zip vertices [0..])
  indexedMesh vertices [(indices Map.! a,indices Map.! b,indices Map.! c) | (a,b,c)<-tris]

mapIndexed :: (V3 -> V3) -> IndexedMesh -> Either String IndexedMesh
mapIndexed f (IndexedMesh vs fs) = indexedMesh (map f vs) fs

reverseIndexed :: IndexedMesh -> IndexedMesh
reverseIndexed (IndexedMesh vs fs) = IndexedMesh vs [(a,c,b) | (a,b,c)<-fs]


-- | Split by vertex identity, not coordinate equality. Compact each result
-- without welding it; separate solids may touch geometrically.
indexedComponents :: IndexedMesh -> [IndexedMesh]
indexedComponents (IndexedMesh vs fs) = map compact (components (IS.fromList [0..length fs-1]))
  where
    vertices=A.listArray (0,length vs-1) vs
    faces=A.listArray (0,length fs-1) fs
    ids (a,b,c)=[a,b,c]
    adjacent=IM.fromListWith (++) [(v,[i]) | (i,f)<-zip [0..] fs,v<-ids f]
    walk found []=found
    walk found (x:xs)
      | IS.member x found=walk found xs
      | otherwise=walk (IS.insert x found) (concatMap (\v -> IM.findWithDefault [] v adjacent) (ids (faces A.! x))++xs)
    components remaining=case IS.minView remaining of
      Nothing -> []
      Just (first,_) -> let component=walk IS.empty [first]
                        in component:components (remaining `IS.difference` component)
    compact component =
      let triangles=[faces A.! i | i<-IS.toAscList component]
          used=IS.toAscList (IS.fromList (concatMap ids triangles))
          indices=IM.fromList (zip used [0..])
      in IndexedMesh [vertices A.! i | i<-used]
           [(indices IM.! a,indices IM.! b,indices IM.! c) | (a,b,c)<-triangles]

-- | Concatenation preserves each input's connectivity; this is for disjoint
-- shells, never a substitute for the boolean union of overlapping solids.
concatenateIndexed :: [IndexedMesh] -> Either String IndexedMesh
concatenateIndexed meshes = indexedMesh (concatMap indexedVertices meshes)
  [(a+offset,b+offset,c+offset) | (offset,mesh)<-zip offsets meshes,(a,b,c)<-indexedFaces mesh]
  where offsets=scanl (+) 0 (map (length . indexedVertices) meshes)
