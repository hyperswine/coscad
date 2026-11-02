module Tier.Stdlib (stdlibTier) where

import Control.Monad (forM_)
import qualified Data.Map as Map
import Coscad.Backend (nativeCSGMesh, nativeCSGMeshAllowEmpty)
import Coscad.IO (readFileUtf8)
import Coscad.Mesh (meshVolume, meshBounds, Tri)
import Coscad.Mesh.Distance (triangleDistance)
import Coscad.Parser (parseProgram)
import Coscad.Shape
import Coscad.Stdlib
import Harness

stdlibTier :: T -> IO ()
stdlibTier t = do
  section "standard printable connections"
  let male = "thread_male 4.05 5 3 8 2.2 0.2 24"
      female = "thread_female 4.05 5 3 8 2.2 0.2 24 0.35 0.18"
  forM_ ["", "!simple\n", "!glyph\n"] $ \pragma -> do
    assertT t "stdlib parses in every syntax mode" (either (const False) (const True) (parseProgram (pragma++"main = "++male))) ""
  forM_ [("thread_male 4 5 3 8 2 0.2 24.5","whole number"),
         ("thread_female 4 5 3 8 2 0.2 24 (-0.1) 0.1","nonnegative"),
         ("thread_female 4 5 3 8 2.8 0.2 24 0.3 0.2","pitch"),
         ("thread_male 4 5 3 99999 2 0.2 24","budget"),
         ("captive_bearing 2 20 3 2 0.5 0.3 1","stops"),
         ("clearance_bore 2 10 (-1)","nonnegative"),
         ("thread_male 4 5", "pitch"),
         ("globe_stud 0.5 0.6 3 12", "root"),
         ("thread_male = box 1 2 3\nmain = thread_male", "reserved") ] $ \(src,msg) ->
    expectErr t ("stdlib rejects "++src) (parseProgram (if take 11 src == "thread_male" && '=' `elem` src then src else "main = "++src)) [msg] []
  assertT t "Haskell API rejects nonfinite dimensions" (either (const True) (const False) (clearanceBore (0/0) 10 0.3)) ""
  assertT t "Haskell API rejects excessive expansion before allocation" (either (const True) (const False) (threadMale tesseractThread {threadLength=100000})) ""
  -- Real source baselines, not snapshots generated from the new implementation.
  tess <- readFileUtf8 "models/tesseract-v3-threaded/design.coscad"
  globe <- readFileUtf8 "models/print-in-place-globe/v2-threaded/assembly.coscad"
  forM_ [(tess,"male",male),(tess,"female",female),
         (globe,"male","globe_stud 10 12 6 12"),
         (globe,"socket","globe_socket 10 12 6 12 0.4 0.25")] $ \(old,name,expr) ->
    case (parseProgram old,parseProgram ("main = "++expr)) of
      (Right (vars,_),Right (_,shape)) -> case Map.lookup name vars of
        Nothing -> failT t ("stdlib reference "++name) "reference missing"
        Just reference -> do
          oldMesh <- nativeCSGMesh reference
          newMesh <- nativeCSGMesh shape
          case (oldMesh,newMesh) of
            (Right a,Right b) -> do
              assertT t ("stdlib baseline volume "++name) (abs (meshVolume a-meshVolume b)<0.02) (show (meshVolume a,meshVolume b))
              let flatten ((x,y,z),(u,v,w)) = [x,y,z,u,v,w]
              assertT t ("stdlib baseline bounds "++name) (and (zipWith (\x y -> abs(x-y)<0.001) (flatten (meshBounds a)) (flatten (meshBounds b)))) ""
              assertT t ("stdlib sampled bidirectional surface "++name) (surfaceGap a b<0.001 && surfaceGap b a<0.001) (show (surfaceGap a b,surfaceGap b a))
            (Left e,_) -> failT t "stdlib baseline mesh" e
            (_,Left e) -> failT t "stdlib replacement mesh" e
      (Left e,_) -> failT t "stdlib baseline parses" e
      (_,Left e) -> failT t "stdlib recipe parses" e
  case (parseProgram ("main = "++male),parseProgram ("main = "++female)) of
    (Right (_,a),Right (_,cutter)) -> do
      let socket = Diff (Tz (-5) (ZCyl 8 10)) cutter
      forM_ [0,30..360] $ \angle -> do
        collision <- nativeCSGMeshAllowEmpty (Intersection [socket,Tz (3*angle/360) (Rz angle a)])
        assertT t ("thread screw motion clearance "++show angle)
          (either (const False) ((<0.01) . abs . meshVolume) collision) (either id (show . meshVolume) collision)
      collision <- nativeCSGMeshAllowEmpty (Intersection [socket,Rz 180 a])
      assertT t "wrong thread phase really collides (negative control)"
        (either (const False) ((>0.1) . abs . meshVolume) collision) ""
    _ -> failT t "stdlib thread pair" "parse failed"
  forM_ ["thread_male 6 8 5 16 3 0.4 36", "thread_female 6 8 5 16 3 0.4 36 0.4 0.2",
         "globe_stud 10 12 8 15", "globe_socket 10 12 8 15 1.2 0.3",
         "clearance_pin 2.5 12 4 2", "clearance_bore 2.5 12 0.6",
         "captive_bearing 2.5 20 6 2 0.6 0.4 1.2"] $ \expr ->
    case parseProgram ("main = "++expr) of
      Left e -> failT t "stdlib parameterized recipe parses" e
      Right (_,s) -> do
        result <- nativeCSGMesh s
        assertT t ("stdlib native render "++expr) (either (const False) ((>0) . meshVolume) result) (either id (const "") result)

-- Volume/bounds plus distributed vertices AND face centroids; a sampled check,
-- not an exact symmetric-difference proof (coincident CSG operands are refused
-- by the native kernel's indexed-import guard).
surfaceGap :: [Tri] -> [Tri] -> Double
surfaceGap a b = maximum [minimum [triangleDistance (p,p,p) tri | tri <- b] | p <- points]
  where
    step = max 1 (length a `div` 32)
    samples = [tri | (i,tri) <- zip [0::Int ..] a, i `mod` step == 0]
    points = concat [[x,y,z,center x y z] | (x,y,z) <- samples]
    center (x,y,z) (u,v,w) (i,j,k) = ((x+u+i)/3,(y+v+j)/3,(z+w+k)/3)
