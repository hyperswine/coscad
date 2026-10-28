{-# LANGUAGE FlexibleContexts #-}
-- | Headless, deterministic triangle rendering. Geometry is meshed once per
-- distinct local shape; poses, colours and the view belong to the scene.
-- Orthographic framing fits every vertex, including hardware and the bench.
module Coscad.Image
  ( RGB, Scene (..), SceneObject (..), MeshObject (..)
  , defaultView, renderImage, renderScenes
  ) where

import Codec.Picture (Image, PixelRGB8 (..), generateImage, writePng)
import Control.Exception (IOException, try)
import Control.Monad (forM, forM_)
import Control.Monad.ST (ST)
import Data.Array.ST (STUArray, newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, (!))
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.IORef (newIORef, readIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import Data.Word (Word8, Word32)
import Coscad.Backend (nativeCSGMeshAllowEmpty)
import Coscad.Geometry (fromCorners)
import Coscad.Linalg
import Coscad.Mesh (Tri, meshMap)
import Coscad.Shape (Shape)

type RGB = (Word8, Word8, Word8)

data SceneObject = SceneObject
  { soName :: String
  , soShape :: Shape
  , soPose :: (M3, V3)
  , soColor :: RGB
  } deriving (Show)

data Scene = Scene
  { sceneObjects :: [SceneObject]
  , sceneView :: V3 -- target-to-eye direction; +Z is world up
  } deriving (Show)

data MeshObject = MeshObject
  { moTriangles :: [Tri]
  , moColor :: RGB
  } deriving (Show)

defaultView :: V3
defaultView = (1, -1.5, 1.2)

-- | Render all steps with a shared local-mesh cache. No shell, OpenSCAD, GPU,
-- display server or browser is involved. Native errors are never hidden by a
-- renderer fallback. The caller reports failures through its normal contract.
renderScenes :: [(FilePath, Scene)] -> IO (Either String ())
renderScenes scenes = do
  cache <- newIORef Map.empty
  let meshObject obj = do
        let key = show (soShape obj)
        old <- Map.lookup key <$> readIORef cache
        result <- case old of
          Just mesh -> return (Right mesh)
          Nothing -> do
            r <- nativeCSGMeshAllowEmpty (soShape obj)
            case r of
              Right mesh -> modifyIORef' cache (Map.insert key mesh)
              Left _ -> return ()
            return r
        return $ case result of
          Left e -> Left (soName obj ++ ": " ++ e)
          Right mesh -> let (rot, pos) = soPose obj
                         in Right (MeshObject (meshMap (vadd pos . mApply rot) mesh) (soColor obj))
      go [] = return (Right ())
      go ((path, scene):rest) = do
        objects <- sequence <$> forM (sceneObjects scene) meshObject
        case objects >>= renderImage 1200 900 (sceneView scene) of
          Left e -> return (Left (path ++ ": " ++ e))
          Right image -> do
            written <- try (writePng path image) :: IO (Either IOException ())
            case written of
              Left e -> return (Left (path ++ ": cannot write PNG: " ++ show e))
              Right () -> go rest
  go scenes

-- | Two samples per axis, flat Lambert lighting and a per-sample depth buffer.
-- Draw order cannot reveal hidden triangles. Equal-depth ties keep the first
-- surface, deterministically; physically coincident surfaces remain ambiguous.
renderImage :: Int -> Int -> V3 -> [MeshObject] -> Either String (Image PixelRGB8)
renderImage width height view objects
  | width < 1 || height < 1 || width > 2048 || height > 2048 = Left "image size must be 1..2048 pixels on each axis"
  | null triangles = Left "image scene has no triangles"
  | not (all finitePoint points) = Left "image scene contains non-finite coordinates"
  | not (finitePoint view) || vlen view < 1e-12 = Left "image view direction must be finite and nonzero"
  | not (finitePoint centre) || not (all finite [extentX, extentY]) = Left "image scene bounds overflow"
  | not (all finitePoint projected) = Left "image scene projection overflow"
  | otherwise = Right (generateImage sample width height)
  where
    triangles = [(tri, moColor obj) | obj <- objects, tri <- moTriangles obj]
    points = concatMap (\((a,b,c),_) -> [a,b,c]) triangles
    finite x = not (isNaN x || isInfinite x)
    finitePoint (x,y,z) = all finite [x,y,z]
    bounds = fromCorners points
    centre = mid bounds
    mid ((ax,ay,az),(bx,by,bz)) = (ax/2+bx/2, ay/2+by/2, az/2+bz/2)
    forward = vneg (vnormed view)
    worldUp = if abs (dot3 forward (0,0,1)) > 0.99 then (0,1,0) else (0,0,1)
    right = vnormed (cross forward worldUp)
    up = cross right forward
    camera p = let q = vsub p centre in (dot3 q right, dot3 q up, dot3 q forward)
    projected = map camera points
    ((x0,y0,_),(x1,y1,_)) = fromCorners projected
    extentX = x1-x0
    extentY = y1-y0
    sw = width*2
    sh = height*2
    zoom = min (fromIntegral sw*0.84 / max 1e-9 extentX) (fromIntegral sh*0.84 / max 1e-9 extentY)
    project p = let (x,y,z) = camera p in
      ((x-(x0/2+x1/2))*zoom + fromIntegral sw/2,
       fromIntegral sh/2-(y-(y0/2+y1/2))*zoom,z)
    painted = [( (project a,project b,project c), lit color a b c ) | ((a,b,c),color) <- triangles]
    pixels = raster sw sh painted
    sample x y =
      let cs = [pixels ! ((y*2+dy)*sw+x*2+dx) | dy <- [0,1], dx <- [0,1]]
          channel shift = fromIntegral (sum [fromIntegral ((c `shiftR` shift) .&. 255) :: Int | c <- cs] `div` 4)
       in PixelRGB8 (channel 16) (channel 8) (channel 0)
    lit (r,g,blue) a b c =
      let n0 = vnormed (cross (vnormed (vsub b a)) (vnormed (vsub c a)))
          n = if dot3 n0 (vnormed view) < 0 then vneg n0 else n0
          shade = 0.42 + 0.58*max 0 (dot3 n (vnormed (-0.4,-0.7,1)))
          scale v = fromIntegral (max 0 (min 255 (round (fromIntegral v * shade) :: Int)))
       in pack (scale r,scale g,scale blue)

pack :: RGB -> Word32
pack (r,g,b) = fromIntegral r `shiftL` 16 .|. fromIntegral g `shiftL` 8 .|. fromIntegral b

raster :: Int -> Int -> [(Tri,Word32)] -> UArray Int Word32
raster width height triangles = runSTUArray $ do
  colors <- newArray (0,width*height-1) (pack (250,249,246))
  depth <- newArray (0,width*height-1) (1/0) :: ST s (STUArray s Int Double)
  forM_ triangles $ \((a@(ax,ay,az),b@(bx,by,bz),c@(cx,cy,cz)),color) -> do
    let area = edge a b c
        xmin = max 0 (floor (minimum [ax,bx,cx]))
        xmax = min (width-1) (ceiling (maximum [ax,bx,cx]))
        ymin = max 0 (floor (minimum [ay,by,cy]))
        ymax = min (height-1) (ceiling (maximum [ay,by,cy]))
    if abs area < 1e-12 then return () else
      forM_ [ymin..ymax] $ \y -> forM_ [xmin..xmax] $ \x -> do
        let p = (fromIntegral x+0.5,fromIntegral y+0.5,0)
            wa = edge b c p / area
            wb = edge c a p / area
            wc = 1-wa-wb
        if minimum [wa,wb,wc] < -1e-10 then return () else do
          let z = wa*az+wb*bz+wc*cz
              index = y*width+x
          previous <- readArray depth index
          if z >= previous then return () else do
            writeArray depth index z
            writeArray colors index color
  return colors
  where
    edge :: V3 -> V3 -> V3 -> Double
    edge (ax,ay,_) (bx,by,_) (cx,cy,_) = (bx-ax)*(cy-ay)-(by-ay)*(cx-ax)
