{-# LANGUAGE ForeignFunctionInterface #-}
-- | Kernel solids that stay alive across many queries. The build planner
-- imports each part once and then asks, under rigid poses, how far apart
-- two parts are and how much they overlap (an insertion sweep makes
-- hundreds of such queries per part).
module Coscad.Solid
  ( Solid
  , Pose
  , solidFromMesh
  , solidPosed
  , solidPrepare
  , solidGap
  , solidOverlap
  )
where

import Coscad.Linalg (M3, V3)
import Coscad.Mesh.Indexed
import Control.Exception (finally)
import Foreign
import Foreign.C

-- | A rotation and a translation: world = R p + t.
type Pose = (M3, V3)

newtype Solid = Solid (ForeignPtr ())

foreign import ccall safe "coscad_solid_new"
  c_new :: Ptr CDouble -> CSize -> Ptr Word64 -> CSize -> Ptr (Ptr ()) -> Ptr CChar -> CSize -> IO CInt
foreign import ccall safe "coscad_solid_posed"
  c_posed :: Ptr () -> Ptr CDouble -> Ptr (Ptr ()) -> Ptr CChar -> CSize -> IO CInt
foreign import ccall safe "coscad_solid_prepare"
  c_prepare :: Ptr () -> CDouble -> IO CInt
foreign import ccall unsafe "&coscad_solid_free"
  c_free_solid :: FunPtr (Ptr () -> IO ())
foreign import ccall safe "coscad_solid_gap"
  c_gap :: Ptr () -> Ptr CDouble -> Ptr () -> Ptr CDouble -> CDouble -> CDouble -> CDouble -> Ptr CDouble -> Ptr CChar -> CSize -> IO CInt
foreign import ccall safe "coscad_solid_overlap"
  c_overlap :: Ptr () -> Ptr CDouble -> Ptr () -> Ptr CDouble -> CInt -> Ptr CDouble -> Ptr (Ptr CDouble) -> Ptr CSize -> Ptr CChar -> CSize -> IO CInt
foreign import ccall unsafe "coscad_mesh_free"
  c_free_points :: Ptr CDouble -> IO ()

solidFromMesh :: IndexedMesh -> IO (Either String Solid)
solidFromMesh mesh =
  withArray [realToFrac c | (x, y, z) <- indexedVertices mesh, c <- [x, y, z]] $ \vs ->
  withArray [fromIntegral i | (a, b, c) <- indexedFaces mesh, i <- [a, b, c]] $ \fs ->
  alloca $ \out -> allocaBytes 256 $ \err -> do
    status <- c_new vs (fromIntegral (length (indexedVertices mesh))) fs (fromIntegral (length (indexedFaces mesh))) out err 256
    if status /= 0
      then Left . ("solid: " ++) <$> peekCString err
      else do
        p <- peek out
        Right . Solid <$> newForeignPtr c_free_solid p

-- | The solid under a pose, as a solid of its own: queries that pose it by
-- the identity reuse the kernel's posed mesh and collision tree.
solidPosed :: Solid -> Pose -> IO (Either String Solid)
solidPosed (Solid a) pa =
  withForeignPtr a $ \ha -> withPose pa $ \ta -> alloca $ \out -> allocaBytes 256 $ \err -> do
    status <- c_posed ha ta out err 256
    if status /= 0
      then Left . ("solid: " ++) <$> peekCString err
      else do
        p <- peek out
        Right . Solid <$> newForeignPtr c_free_solid p

-- | Build the solid's distance trees now (and the shrunken copy's, for a
-- positive inset), so concurrent queries afterwards only read them.
solidPrepare :: Solid -> Double -> IO ()
solidPrepare (Solid a) inset = withForeignPtr a $ \ha -> () <$ c_prepare ha (realToFrac inset)

withPose :: Pose -> (Ptr CDouble -> IO a) -> IO a
withPose ((r1, r2, r3), (tx, ty, tz)) = withArray (map realToFrac (row r1 tx ++ row r2 ty ++ row r3 tz))
  where row (a, b, c) t = [a, b, c, t]

-- | The smallest distance from the first solid, moved by its pose, to the
-- second (which must be posed already: its pose is the identity), capped at
-- the search length. Once the two are known to be within `enough` the
-- answer is some distance no larger than that. 0 when they touch.
--
-- With a positive inset the first solid is shrunk by that much along its
-- vertex normals first, so 0 means a real overlap rather than a touch.
solidGap :: Solid -> Pose -> Solid -> Pose -> Double -> Double -> Double -> IO (Either String Double)
solidGap (Solid a) pa (Solid b) pb search enough inset =
  withForeignPtr a $ \ha -> withForeignPtr b $ \hb -> withPose pa $ \ta -> withPose pb $ \tb ->
  alloca $ \out -> allocaBytes 256 $ \err -> do
    status <- c_gap ha ta hb tb (realToFrac search) (realToFrac enough) (realToFrac inset) out err 256
    if status /= 0 then Left . ("gap: " ++) <$> peekCString err else Right . realToFrac <$> peek out

-- | The overlap volume of the posed solids and, when asked, the vertices of
-- the overlap (a contact patch).
solidOverlap :: Solid -> Pose -> Solid -> Pose -> Bool -> IO (Either String (Double, [V3]))
solidOverlap (Solid a) pa (Solid b) pb wantPoints =
  withForeignPtr a $ \ha -> withForeignPtr b $ \hb -> withPose pa $ \ta -> withPose pb $ \tb ->
  alloca $ \vol -> alloca $ \pts -> alloca $ \np -> allocaBytes 256 $ \err -> do
    poke pts nullPtr
    (do status <- c_overlap ha ta hb tb (if wantPoints then 1 else 0) vol pts np err 256
        if status /= 0 then Left . ("overlap: " ++) <$> peekCString err else do
          v <- realToFrac <$> peek vol
          n <- fromIntegral <$> peek np
          p <- peek pts
          cs <- if n == 0 || p == nullPtr then return [] else peekArray (3 * n) p
          return (Right (v, triples (map realToFrac cs))))
      `finally` (peek pts >>= \p -> if p == nullPtr then return () else c_free_points p)
  where
    triples (x : y : z : rest) = (x, y, z) : triples rest
    triples _ = []
