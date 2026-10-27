{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | Optional direct Manifold bridge. No subprocess, implicit fallback or
-- unsafePerformIO. Empty results are valid intermediate CSG operands.
module Coscad.Manifold (MeshOp (..), manifoldAvailable, manifoldKernelVersion, meshOperation, indexedOperation, indexedOperationWithLimit, extrudePolygon, extrudeOffsetPolygon) where

import Coscad.Mesh (Tri)
import Coscad.Mesh.Indexed
#ifdef COSCAD_MANIFOLD
import Control.Exception (finally, mask)
import Foreign
import Foreign.C
#endif

data MeshOp = MeshUnion | MeshDifference | MeshIntersection | MeshHull | MeshMinkowski
  deriving (Eq, Show, Enum)

manifoldAvailable :: Bool
-- | The linked kernel's version. It is a constant, not a probe: the Cabal
-- pkg-config constraint and the static_assert in cbits/coscad_manifold.cpp
-- both refuse any other version than the one native-kernel.lock.json pins.
manifoldKernelVersion :: Maybe String
#ifdef COSCAD_MANIFOLD
manifoldAvailable = True
manifoldKernelVersion = Just "3.5.2"

foreign import ccall safe "coscad_mesh_op_indexed_timed"
  c_indexed :: Word64 -> CInt -> Ptr CDouble -> CSize -> Ptr Word64 -> CSize
            -> Ptr CDouble -> CSize -> Ptr Word64 -> CSize
            -> Ptr (Ptr CDouble) -> Ptr CSize -> Ptr (Ptr Word64) -> Ptr CSize
            -> Ptr CChar -> CSize -> IO CInt
foreign import ccall unsafe "coscad_mesh_free_indices"
  c_free_indices :: Ptr Word64 -> IO ()
foreign import ccall safe "coscad_mesh_extrude"
  c_extrude :: Ptr CDouble -> CSize -> CDouble -> Ptr (Ptr CDouble) -> Ptr CSize -> Ptr CChar -> CSize -> IO CInt
foreign import ccall unsafe "coscad_mesh_free"
  c_free :: Ptr CDouble -> IO ()
foreign import ccall safe "coscad_mesh_extrude_offset"
  c_extrude_offset :: Ptr CDouble -> CSize -> CDouble -> CInt -> CDouble -> Ptr (Ptr CDouble) -> Ptr CSize -> Ptr CChar -> CSize -> IO CInt

-- | The limit is cooperative: a large Boolean phase may overrun it. A timed
-- out operation is never accepted, and every call gets a fresh kernel context.
indexedOperationWithLimit :: Int -> MeshOp -> IndexedMesh -> IndexedMesh -> IO (Either String IndexedMesh)
indexedOperationWithLimit milliseconds _ _ _ | milliseconds<1 || milliseconds>600000 =
  return (Left "Manifold: native timeout must be 1..600000 milliseconds")
indexedOperationWithLimit milliseconds op a b =
  withArray (coordinates a) $ \av -> withArray (indices a) $ \af ->
  withArray (coordinates b) $ \bv -> withArray (indices b) $ \bf ->
  alloca $ \outV -> alloca $ \nv -> alloca $ \outF -> alloca $ \nt -> allocaBytes 256 $ \err -> mask $ \restore -> do
    poke outV nullPtr; poke outF nullPtr
    (do status<-c_indexed (fromIntegral milliseconds) (fromIntegral (fromEnum op)) av (vertexCount a) af (faceCount a)
                   bv (vertexCount b) bf (faceCount b) outV nv outF nt err 256
        if status/=0 then Left . ("Manifold: "++) <$> peekCString err else do
          vertexN<-peek nv; faceN<-peek nt; vp<-peek outV; fp<-peek outF
          if vertexN>1500000 || faceN>500000 || (vertexN>0 && vp==nullPtr) || (faceN>0 && fp==nullPtr)
            then return (Left "Manifold: invalid indexed output buffer")
            else restore $ do
              vs<-peekArray (3*fromIntegral vertexN) vp
              fs<-peekArray (3*fromIntegral faceN) fp
              if any (>=fromIntegral vertexN) fs then return (Left "Manifold: invalid output index")
              else return (indexedMesh (triples (map realToFrac vs)) (triples (map fromIntegral fs))))
      `finally` ((peek outV >>= c_free) `finally` (peek outF >>= c_free_indices))
  where
    coordinates=concatMap (\(x,y,z) -> map realToFrac [x,y,z]) . indexedVertices
    indices=concatMap (\(a',b',c') -> map fromIntegral [a',b',c']) . indexedFaces
    vertexCount=fromIntegral . length . indexedVertices
    faceCount=fromIntegral . length . indexedFaces
    triples (a':b':c':rest)=(a',b',c'):triples rest
    triples _=[] -- validated ABI returns exactly three components per item

extrudePolygon :: Double -> [(Double,Double)] -> IO (Either String [Tri])
extrudePolygon height points
  | length points>4096 = return (Left "Manifold: extrusion exceeds 4096 profile vertices")
  | otherwise = withArray (concatMap (\(x,y) -> map realToFrac [x,y]) points) $ \ptr ->
      withMeshResult (c_extrude ptr (fromIntegral (length points)) (realToFrac height))

-- | OpenSCAD's @offset(r = delta)@ of one simple outline (round joins with
-- @segments@ arc steps, as OpenSCAD derives them from @$fn@) followed by a
-- straight extrusion, through the kernel's Clipper2 CrossSection.
extrudeOffsetPolygon :: Double -> Int -> Double -> [(Double,Double)] -> IO (Either String [Tri])
extrudeOffsetPolygon height segments delta points
  | length points>4096 = return (Left "Manifold: offset profile exceeds 4096 vertices")
  | otherwise = withArray (concatMap (\(x,y) -> map realToFrac [x,y]) points) $ \ptr ->
      withMeshResult (c_extrude_offset ptr (fromIntegral (length points)) (realToFrac delta) (fromIntegral segments) (realToFrac height))

withMeshResult :: (Ptr (Ptr CDouble) -> Ptr CSize -> Ptr CChar -> CSize -> IO CInt) -> IO (Either String [Tri])
withMeshResult action = alloca $ \out -> alloca $ \count -> allocaBytes 256 $ \err -> mask $ \restore -> do
  poke out nullPtr
  -- Cleanup precedes the safe C call, including asynchronous exceptions.
  (do status<-action out count err 256
      if status/=0 then Left . ("Manifold: "++) <$> peekCString err
      else do
        n<-peek count
        p<-peek out
        if n>500000 || (n>0 && p==nullPtr) then return (Left "Manifold: invalid output buffer")
        else restore (Right . triangles <$> peekArray (9*fromIntegral n) p))
    `finally` (peek out >>= c_free)
  where
    triangles (a:b:c:d:e:f:g:h:i:xs)=((v a,v b,v c),(v d,v e,v f),(v g,v h,v i)):triangles xs
    triangles _=[] -- exactly nine doubles per triangle from the checked ABI
    v=realToFrac
#else
manifoldAvailable = False
manifoldKernelVersion = Nothing

indexedOperationWithLimit :: Int -> MeshOp -> IndexedMesh -> IndexedMesh -> IO (Either String IndexedMesh)
indexedOperationWithLimit _ _ _ _ = return (Left "native CSG needs a build with the manifold flag (-fmanifold, the default; this binary was built with -f-manifold or without the pinned kernel); see scripts/build-manifold.py")
#endif

#ifndef COSCAD_MANIFOLD
extrudePolygon :: Double -> [(Double,Double)] -> IO (Either String [Tri])
extrudePolygon _ _ = return (Left "native extrusion needs a build with the manifold flag (-fmanifold, the default; this binary was built with -f-manifold or without the pinned kernel); see scripts/build-manifold.py")

extrudeOffsetPolygon :: Double -> Int -> Double -> [(Double,Double)] -> IO (Either String [Tri])
extrudeOffsetPolygon _ _ _ _ = return (Left "native 2D offset needs a build with the manifold flag (-fmanifold, the default; this binary was built with -f-manifold or without the pinned kernel); see scripts/build-manifold.py")
#endif

-- | Legacy triangle-soup boundary: import by exact-position welding. Native CSG
-- uses 'indexedOperation' throughout, without this lossy round trip.
meshOperation :: MeshOp -> [Tri] -> [Tri] -> IO (Either String [Tri])
meshOperation op a b = case (weldTriangles a,weldTriangles b) of
  (Right x,Right y) -> fmap indexedTriangles <$> indexedOperation op x y
  (Left err,_) -> return (Left ("Manifold: "++err))
  (_,Left err) -> return (Left ("Manifold: "++err))

indexedOperation :: MeshOp -> IndexedMesh -> IndexedMesh -> IO (Either String IndexedMesh)
indexedOperation = indexedOperationWithLimit 60000
