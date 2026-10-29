-- | Image correctness and planner/site integration without an OpenSCAD process.
module Tier.Image (imageTier) where

import Codec.Picture
import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Coscad.Backend (Backend (..))
import Coscad.Image
import Coscad.Linalg (matIdentity)
import Coscad.Shape (Shape (..))
import Coscad.Plan (PlanSummary (..), processPlanSummaryWithBackend)
import Coscad.Report (silent)
import Coscad.Site (processSiteWithBackend)
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath
import Harness

imageTier :: T -> IO ()
imageTier t = do
  section "native images"
  let front = MeshObject [((-1,-1,1),(1,-1,1),(0,1,1))] (255,0,0)
      back = MeshObject [((-1,-1,0),(1,-1,0),(0,1,0))] (0,0,255)
      image objects = renderImage 80 60 (0,0,1) objects
  case (image [front,back], image [back,front]) of
    (Right a,Right b) -> do
      let PixelRGB8 red _ blue = pixelAt a 40 30
      assertT t "image: depth hides rear triangle independent of submission order"
        (red > blue && imageData a == imageData b) "rear geometry leaked through the foreground"
    _ -> failT t "image: overlap scene" "could not render valid triangles"
  let mirrored = MeshObject [((1,-1,0),(-1,-1,0),(0,1,0))] (255,0,0)
  case image [mirrored] of
    Left e -> failT t "image: mirrored winding" e
    Right a -> let PixelRGB8 r _ b = pixelAt a 40 30 in
      assertT t "image: reversed winding stays visible" (r>b) "back-face culling removed mirrored surface"
  forM_ [(0,60,(0,0,1),[front]),(80,0,(0,0,1),[front]),(2049,60,(0,0,1),[front]),
         (80,60,(0,0,0),[front]),(80,60,(0,0,1),[]),
         (80,60,(0,0,1),[MeshObject [((0/0,0,0),(1,0,0),(0,1,0))] (0,0,0)])] $ \(w,h,v,os) ->
    assertT t "image: invalid input refused before rasterisation"
      (case renderImage w h v os of Left _ -> True; Right _ -> False) "invalid scene accepted"
  -- All views, including a pole-on view, fit extreme aspect ratios.
  forM_ [defaultView,(0,0,1),(0,0,-1)] $ \view -> do
    let tall = MeshObject [((0,0,0),(40,0,1000),(0,40,1000))] (255,165,0)
    case renderImage 80 60 view [tall] of
      Left e -> failT t "image: tall scene" e
      Right a -> do
        let colored = [(x,y) | x <- [0..79], y <- [0..59], pixelAt a x y /= PixelRGB8 250 249 246]
        assertT t "image: auto-fit keeps tall scene away from image edges"
          (not (null colored) && all (\(x,y)->x>0 && y>0 && x<79 && y<59) colored) "scene clipped or empty"
  do
    tmp <- tempDir "native-images"
    let marker = tmp </> "openscad-invocations"
        forbidden = tmp </> "forbidden-openscad"
    writeFile forbidden ("#!/bin/sh\necho invoked >> \"" ++ marker ++ "\"\nexit 99\n")
    permissions <- getPermissions forbidden
    setPermissions forbidden permissions {executable=True}
    bracket (lookupEnv "COSCAD_OPENSCAD") (maybe (unsetEnv "COSCAD_OPENSCAD") (setEnv "COSCAD_OPENSCAD")) $ \_ -> do
      setEnv "COSCAD_OPENSCAD" forbidden
      unsupported <- renderScenes [(tmp </> "unsupported.png", Scene
        [SceneObject "unsupported torus" (Torus 1 2) (matIdentity,(0,0,0)) (255,0,0)] defaultView)]
      assertT t "native image geometry errors do not fall back to OpenSCAD"
        (either (const True) (const False) unsupported) (show unsupported)
      hidden <- renderScenes [(tmp </> "hidden.png", Scene
        [SceneObject "hidden checker" (Hidden (Rectangle 10 10 10)) (matIdentity,(0,0,0)) (255,0,0),
         SceneObject "visible box" (Rectangle 1 2 3) (matIdentity,(0,0,0)) (200,200,200)] defaultView)]
      assertT t "native images skip hidden checker geometry"
        (either (const False) (const True) hidden) (show hidden)
      unwritable <- renderScenes [(tmp, Scene
        [SceneObject "box" (Rectangle 1 2 3) (matIdentity,(0,0,0)) (255,0,0)] defaultView)]
      assertT t "native PNG write failure is returned as an error"
        (either (const True) (const False) unwritable) (show unwritable)
      -- Rounded/hollow CSG, long rails, hardware, flips/preloads and the shelf.
      forM_ [("corner","examples/assemble/plan","corner_pair"),
             ("ball","examples/assemble/ball","ball"),
             ("bow","examples/assemble/bow3","bow3"),
             ("bracket","examples/stress","shelf-bracket")] $ \(tag,folder,name) -> do
        let dest = tmp </> tag
        createDirectoryIfMissing True dest
        files <- listDirectory folder
        forM_ [f | f <- files, takeExtension f `elem` [".coscad",".assemble"]] $ \f ->
          copyFile (folder </> f) (dest </> f)
        result <- processPlanSummaryWithBackend Native silent True (dest </> name <.> "assemble")
        case result of
          Left e -> failT t ("native plan PNG " ++ tag) (show e)
          Right summary -> forM_ [1..length (psSteps summary)] $ \k -> do
            decoded <- readImage (dest </> name ++ "_step" ++ show k ++ ".png")
            case decoded of
              Left e -> failT t ("PNG decode " ++ tag) e
              Right dynamic -> do
                let a = convertRGB8 dynamic
                    ink = length [() | x <- [0,20..1199], y <- [0,20..899], pixelAt a x y /= PixelRGB8 250 249 246]
                assertT t ("PNG dimensions and content " ++ tag)
                  (imageWidth a==1200 && imageHeight a==900 && ink>20) "invalid or blank PNG"
        -- Existing planner output is also consumed end to end by the site.
        when (tag=="corner") $ do
          site <- processSiteWithBackend Native silent (tmp </> "site") [dest </> name <.> "assemble"]
          page <- doesFileExist (tmp </> "site" </> name </> "index.html")
          png <- doesFileExist (tmp </> "site" </> name </> name ++ "_step1.png")
          assertT t "native site publishes HTML and PNG with OpenSCAD forbidden"
            (either (const False) (const True) site && page && png) (show site)
      invoked <- doesFileExist marker
      assertT t "native planner and site never invoked OpenSCAD" (not invoked) marker
