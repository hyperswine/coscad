module Tier.Experiments (experimentsTier, backendOracleTier) where

import Control.Monad (forM_)
import Coscad.Backend
import Coscad.Check (splitSolidBodies, splitIndexedSolids)
import Coscad.Json (JValue (..), lookupKey)
import Coscad.Geometry (V3, cross, dot3, vadd, vlen)
import Coscad.Mesh
import Coscad.Mesh.Distance
import Coscad.Linalg (mApply, rodMat)
import Coscad.Mesh.Indexed
import Coscad.Loft (loftTriangles, profilePath, triangulatePolygon)
import Coscad.Codegen (loftMethod)
import Coscad.Manifold
import Coscad.Parser (parseProgramNamed)
import Coscad.Shape
import Coscad.Stress.Experiment
import Coscad.Stress.Cad
import Coscad.Stress.Tetra
import qualified Data.Map.Strict as Map
import qualified Data.Array as A
import Harness
import Coscad.OpenScad (openscadVersion)
import Data.Char (isDigit)
import System.FilePath ((</>))
import System.Directory (findExecutable, doesFileExist)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode, readCreateProcessWithExitCode, proc, CreateProcess (..))
import System.Environment (getEnvironment)
import Data.List (isInfixOf, nub)

meshCases :: [(String,Shape)]
meshCases =
  [ ("box",Rectangle 2 3 4)
  , ("centered box",Cuboid (2,3,4) 0 0)
  , ("rounded cuboid primitive",Cuboid (4,6,8) 0 1)
  , ("rounded panel primitive",Cuboid (210,200,2.5) 0 1)
  , ("sphere",Sphere 3)
  , ("cylinder",Cylinder 3 7)
  , ("cone",Cone 3 7)
  , ("frustum",Frustum 7 3 1)
  , ("upside-down cone",Frustum 7 0 3)
  , ("prism",Prism 7 3 7)
  , ("chamfer cylinder",Cyl 3 7 1 0)
  , ("max chamfer cylinder",Cyl 3 6 3 0)
  , ("rounded cylinder",Cyl 3 7 0 1)
  , ("max rounded cylinder",Cyl 3 6 0 3)
  , ("torus",Torus 5 1)
  , ("centered cylinder",Cyl 3 7 0 0)
  , ("x cylinder",XCyl 3 7)
  , ("y cylinder",YCyl 3 7)
  , ("reflected",Mirror (1,1,0) (Cylinder 3 7))
  , ("negative scale",Scale (-2,1,3) (Rectangle 2 3 4))
  , ("wedge",Wedge (2,3,4))
  , ("prismoid",Prismoid (4,6) (2,3) 7)
  , ("pyramid",Prismoid (4,6) (0,0) 7)
  , ("ridge prismoid",Prismoid (4,6) (2,0) 7)
  , ("tube",Tube 4 2 7)
  , ("solid tube",Tube 4 0 7)
  , ("transformed",Translate (2,3,4) (Rz 37 (Rx 21 (Rectangle 2 3 4))))
  ]

experimentsTier :: T -> IO ()
experimentsTier t = do
  section "experimental native meshing and FEA"
  distanceTier t
  indexedContractTier t
  elementTier t
  cadTier t
  tetraTier t
  quadraticTetraTier t
  forM_ meshCases $ \(name,shape) -> case nativeMesh shape of
    Left e -> failT t name e
    Right tris -> do
      assertT t (name ++ " closed oriented surface") (closed tris) "non-manifold edge or reversed face"
      assertT t (name ++ " positive winding") (signedVolume tris > 0) "inside-out mesh"
  forM_ [("union",Union [Rectangle 2 3 4,Rectangle 1 2 3]),("difference",Diff (Cylinder 3 7) (Cylinder 1 7))] $ \(name,s) ->
    expectErr t ("native refuses "++name) (nativeMesh s) ["unsupported"] []
  expectErr t "native invalid radius" (nativeMesh (Cylinder (-1) 2)) ["greater than zero"] []
  expectErr t "native collapsed transform" (nativeMesh (Scale (0,1,1) (Rectangle 1 2 3))) [] []
  expectErr t "native empty CSG refused without evaluating bounds" (nativeMesh (Union [])) ["unsupported"] []
  expectErr t "native empty profile refused without evaluating bounds" (nativeMesh (Poly (PD [] []))) ["unsupported"] []
  expectErr t "native zero rotation axis refused" (nativeMesh (RotAxis 30 (0,0,0) (Rectangle 1 2 3))) ["nonzero axis"] []
  case nativeMesh (Scale (-1e-120,1e-120,1e-120) (Rectangle 1e120 1e120 1e120)) of
    Right tris -> assertT t "reflection parity survives determinant underflow" (abs (signedVolume tris-1)<1e-9) (show (signedVolume tris))
    Left err -> failT t "extreme reflected scale" err
  expectErr t "unknown backend refused" (parseBackend "magic") ["unknown backend"] []
  assertT t "default backend follows the build: native with the kernel, openscad without"
    (defaultBackend == (if manifoldAvailable then NativeExperimental else OpenScadBackend)) (show (defaultBackend, manifoldAvailable))
  assertT t "--backend=native is an alias of native-experimental"
    (parseBackend "native" == Right NativeExperimental && parseBackend "native-experimental" == Right NativeExperimental && parseBackend "openscad" == Right OpenScadBackend) ""
  loftChecks t
  let axial = defaultBeam {beamCells=(4,2,2),poisson=0,tipForceN=(1,0,0)}
  case solveBeam axial of
    Left e -> failT t "axial bar" e
    Right r -> do
      let (ux,uy,uz)=tipDisplacementMM r
      assertT t "axial extension PL/AE" (abs (ux/(20/(2000*16))-1)<1e-7 && abs uy+abs uz<1e-10) (show r)
      assertT t "axial stress P/A" (abs (maxCellCentreVonMisesMPa r/(1/16)-1)<1e-7) (show r)
      assertT t "axial strain energy" (abs (strainEnergyNMM r-ux/2)<1e-10) (show r)
      balance t axial r
  let coarse=defaultBeam {beamCells=(10,2,2)}
      fine=defaultBeam {beamCells=(20,4,4)}
      theoretical=20^(3::Int)/(3*2000*(4*4^(3::Int)/12))
      bendError r=let (_,_,z)=tipDisplacementMM r in abs (negate z/theoretical-1)
  case (solveBeam coarse,solveBeam fine) of
    (Right a,Right b) -> do
      assertT t "cantilever refinement improves bending error" (bendError b < bendError a) (show (a,b))
      assertT t "cantilever displacement within 10% of beam theory" (bendError b < 0.10) (show b)
      balance t fine b
      let (_,_,z)=tipDisplacementMM b
      assertT t "bending work equals twice strain energy" (abs (strainEnergyNMM b+z/2)<1e-9) (show b)
    results -> failT t "cantilever solve" (show results)
  case (solveBeam axial,solveBeam axial {tipForceN=(-2,0,0)}) of
    (Right a,Right b) -> assertT t "load reversal and linearity" (vlen (vadd (tipDisplacementMM b) (mul 2 (tipDisplacementMM a))) < 1e-9) (show (a,b))
    results -> failT t "linearity solve" (show results)
  forM_
    [ ("zero cells",defaultBeam {beamCells=(0,2,2)})
    , ("grid overflow",defaultBeam {beamCells=(maxBound,maxBound,2)})
    , ("zero stiffness",defaultBeam {youngMPa=0})
    , ("NaN stiffness",defaultBeam {youngMPa=0/0})
    , ("incompressible",defaultBeam {poisson=0.5})
    , ("infinite force",defaultBeam {tipForceN=(1/0,0,0)})
    , ("zero force",defaultBeam {tipForceN=(0,0,0)})
    , ("invalid tolerance",defaultBeam {relativeTolerance=0})
    , ("invalid iteration cap",defaultBeam {iterationLimit=0})
    ] $ \(name,p) -> expectErr t name (solveBeam p) ["FEA:"] []
  expectErr t "nonconvergence is not success" (solveBeam coarse {iterationLimit=1}) ["did not converge"] []
  -- Exercise the real CLI/parser and ensure an unsupported native operation
  -- cannot quietly produce a different mesh or replace a previous output.
  bin <- findExecutable "coscad"
  case bin of
    Nothing -> failT t "native CLI available" "coscad not on PATH"
    Just exe -> do
      dir <- tempDir "native-cli"
      let src=dir </> "box.coscad"; dest=dir </> "native.stl"
      writeFile src "main = box 2 3 4\n"
      (code,_,err) <- readProcessWithExitCode exe ["stl",src,"--backend=native-experimental","-o",dest] ""
      exists <- doesFileExist dest
      assertT t "native STL CLI" (code==ExitSuccess && exists) err
      if exists then do
        content <- readFile dest
        assertT t "native CLI volume" (either (const False) (\ts -> abs (meshVolume ts-24)<1e-9) (parseStlAscii content)) content
      else return ()
      (aliasCode,_,aliasErr) <- readProcessWithExitCode exe ["stl",src,"--backend=native","-o",dest] ""
      assertT t "native STL CLI accepts --backend=native" (aliasCode==ExitSuccess) aliasErr
      writeFile src "main = ◉ 2 5\n"
      writeFile dest "previous valid output"
      (failed,_,why) <- readProcessWithExitCode exe ["stl",src,"--backend=native-experimental","-o",dest] ""
      kept <- readFile dest
      assertT t "unsupported CLI keeps prior output" (failed==ExitFailure 1 && "spindle/horn torus is unsupported" `isInfixOf` why && kept=="previous valid output") why
      -- the build's default: a kernel build never needs OpenSCAD for stl; a
      -- kernel-less build must name OpenSCAD as the missing piece
      writeFile src "main = box 2 3 4\n"
      environment <- getEnvironment
      (defaultCode,_,defaultErr) <- readCreateProcessWithExitCode ((proc exe ["stl",src,"-o",dest]) {env=Just (("COSCAD_OPENSCAD","/nonexistent/native-must-not-fallback"):filter ((/="COSCAD_OPENSCAD") . fst) environment)}) ""
      if manifoldAvailable
        then assertT t "default backend renders without OpenSCAD in a kernel build" (defaultCode==ExitSuccess) defaultErr
        else assertT t "default backend is OpenSCAD in a kernel-less build" (defaultCode==ExitFailure 1 && "could not run /nonexistent/native-must-not-fallback" `isInfixOf` defaultErr) defaultErr
      (badBackend,_,message) <- readProcessWithExitCode exe ["stl",src,"--backend=magic"] ""
      assertT t "CLI rejects unknown backend" (badBackend==ExitFailure 1 && "unknown backend" `isInfixOf` message) message
  if manifoldAvailable then manifoldTier t >> indexedKernelTier t >> extrusionTier t >> nativeCorpusTier t >> nativePipelineTier t else do
    r <- meshOperation MeshUnion [] []
    expectErr t "unlinked kernel is explicit" r ["-fmanifold"] []

manifoldTier :: T -> IO ()
manifoldTier t = do
  let box=Rectangle 2 3 4
  forM_
    [("union overlap",Union [box,Tx 1 box],36)
    ,("difference",Diff box (Tx 1 box),12)
    ,("intersection",Intersection [box,Tx 1 box],12)
    ,("disconnected union",Union [box,Tx 4 box],48)
    ,("hull",Hull [box,Tx 4 box],72)
    ,("Minkowski",Minkowski [box,Rectangle 1 1 1],60)
    ,("hidden checker shape",Union [box,Hidden (Tx 4 box)],24)
    ,("empty intermediate",Union [Diff box box,box],24)
    ,("transform of boolean",Scale (-1,2,1) (Diff box (Tx 1 box)),24)
    ] $ \(name,s,volume) -> do
      r <- nativeCSGMesh s
      case r of
        Left e -> failT t ("Manifold "++name) e
        Right tris -> assertT t ("Manifold "++name) (abs (meshVolume tris-volume)<1e-8 && closed tris) (show (meshVolume tris,length tris))
  removed <- nativeCSGMesh (Diff box box)
  expectErr t "final empty CSG is an error" removed ["empty mesh"] []
  invalid <- meshOperation MeshUnion [((0,0,0),(1,0,0),(0,1,0))] []
  expectErr t "open mesh refused at FFI boundary" invalid ["Manifold:"] []

nativeCorpusTier :: T -> IO ()
nativeCorpusTier t = do
  section "accepted native example corpus"
  baseline <- readFile "test/golden/geometry.txt"
  accepted <- readFile "test/golden/native-supported.txt"
  let goldens=Map.fromList [(f,g) | line <- lines baseline, f:values <- [words line], Just g <- [readGeo values]]
      paths=[f | line <- lines accepted, [f] <- [words line], take 1 f /= "#"]
  forM_ paths $ \file -> do
    source <- readFile file
    case (parseProgramNamed file source,Map.lookup file goldens) of
      (Right (_,shape),Just expected) -> do
        r <- nativeCSGMesh shape
        case r of
          Left err -> failT t ("native corpus "++file) err
          Right tris -> assertT t ("native corpus "++file) (geoClose expected (geoOf tris)) (show (expected,geoOf tris))
      _ -> failT t ("native corpus "++file) "source failed to parse or geometry baseline missing"

elementTier :: T -> IO ()
elementTier t = case elementMatrix (2,3,4) 2000 0.3 of
  Left err -> failT t "Hex8 element" err
  Right k -> do
    let nodes=[(x,y,z) | x <- [0,2], y <- [0,3], z <- [0,4]]
        vector f=concatMap (\p -> let (x,y,z)=f p in [x,y,z]) nodes
        apply u=[sum [k A.! (i,j)*v | (j,v) <- zip [0..] u] | i <- [0..23]]
        energy u=sum (zipWith (*) u (apply u))/2
        e=2000; nu=0.3; mu=e/(2*(1+nu)); lambda=e*nu/((1+nu)*(1-2*nu))
        rigid=[const (1,0,0),const (0,1,0),const (0,0,1)
              ,\(_,y,z) -> (0,-z,y),\(x,_,z) -> (z,0,-x),\(x,y,_) -> (-y,x,0)]
    assertT t "element stiffness symmetric" (maximum [abs (k A.! (i,j)-k A.! (j,i)) | i <- [0..23],j <- [0..23]]<1e-10) "asymmetric stiffness"
    forM_ (zip [1::Int ..] rigid) $ \(i,f) ->
      assertT t ("rigid-body mode "++show i++" has no internal force") (maximum (map abs (apply (vector f)))<1e-9) (show (apply (vector f)))
    let normal=vector (\(x,y,z) -> (0.01*x,0.02*y,0.03*z))
        referenceNormal=24*(lambda*0.06*0.06/2+mu*(0.01*0.01+0.02*0.02+0.03*0.03))
        shear=vector (\(_,y,_) -> (0.01*y,0,0))
        referenceShear=24*mu*0.01*0.01/2
    assertT t "constant normal strain patch energy" (abs (energy normal/referenceNormal-1)<1e-12) (show (energy normal,referenceNormal))
    assertT t "constant shear strain patch energy" (abs (energy shear/referenceShear-1)<1e-12) (show (energy shear,referenceShear))

balance :: T -> BeamProblem -> BeamResult -> IO ()
balance t p r = do
  assertT t "reaction balances total load" (vlen (vadd (supportReactionN r) (tipForceN p)) < 1e-6) (show r)
  let (x,y,z)=beamSize p
      moment=cross (x,y/2,z/2) (tipForceN p)
  assertT t "reaction moment balances applied moment" (vlen (vadd (supportMomentNMM r) moment)<1e-5) (show r)
  assertT t "fresh solver residual meets tolerance" (freeResidualRelative r <= relativeTolerance p) (show r)

mul :: Double -> V3 -> V3
mul k (x,y,z)=(k*x,k*y,k*z)

signedVolume :: [Tri] -> Double
signedVolume = sum . map (\(a,b,c) -> dot3 a (cross b c)/6)

closed :: [Tri] -> Bool
closed tris = all (== (2,0)) (Map.elems edges)
  where
    edges = Map.fromListWith (\(a,b) (c,d) -> (a+c,b+d))
      [ (if a<b then (a,b) else (b,a), (1::Int,if a<b then 1::Int else -1))
      | (p,q,r) <- tris, (a,b) <- [(p,q),(q,r),(r,p)]]

backendOracleTier :: T -> IO ()
backendOracleTier t = do
  section "native backend compared with OpenSCAD"
  -- The oracle values were measured against OpenSCAD 2025.05 (Manifold
  -- backend). Older releases (e.g. Ubuntu's apt 2021.01) produce meshes that
  -- differ, so skip loudly rather than fail or pass silently.
  v <- openscadVersion
  case v >>= versionParts of
    Just parts | parts < [2025,5] ->
      putStrLn ("SKIP native backend compared with OpenSCAD: needs OpenSCAD >= 2025.05, found "
        ++ maybe "?" id v ++ " (set COSCAD_OPENSCAD to a newer build to run these)")
    _ -> backendOracleChecks t

versionParts :: String -> Maybe [Int]
versionParts s = case [read d | d <- splitDots (takeWhile (\c -> isDigit c || c == '.') s), not (null d)] of
  ps@(_:_:_) -> Just (take 2 ps)
  _ -> Nothing
  where splitDots str = case break (== '.') str of
          (a, []) -> [a]
          (a, _:rest) -> a : splitDots rest

backendOracleChecks :: T -> IO ()
backendOracleChecks t = do
  dir <- tempDir "backend-oracle"
  forM_ meshCases $ \(name,shape) -> do
    oracle <- renderShape OpenScadBackend (dir </> "part.scad") (dir </> "part.stl") shape
    case (nativeMesh shape,oracle) of
      (Right actual,Right expected) -> do
        assertT t ("native/OpenSCAD "++name) (geoClose (geoOf actual) (geoOf expected)) (show (geoOf actual,geoOf expected))
        if manifoldAvailable then symmetricDifference t name actual expected else return ()
      results -> failT t name (show (fmap length (fst results),fmap length (snd results)))
  if manifoldAvailable then do
    chamferActual<-nativeCSGMesh (Cuboid (4,6,8) 1 0)
    chamferExpected<-renderShape OpenScadBackend (dir </> "chamfer.scad") (dir </> "chamfer.stl") (Cuboid (4,6,8) 1 0)
    case (chamferActual,chamferExpected) of
      (Right a,Right b) -> symmetricDifference t "chamfered cuboid" a b
      r -> failT t "chamfered cuboid" (show (fmap length (fst r),fmap length (snd r)))
    forM_ [("rounded box",Cuboid (4,6,8) 0 1),("max rounded box",Cuboid (2,3,4) 0 1),("thin rounded panel",Cuboid (200,200,2.5) 0 1),("wide rounded panel",Cuboid (210,200,2.5) 0 1)] $ \(name,shape) -> do
      actual<-nativeCSGMesh shape
      expected<-renderShape OpenScadBackend (dir </> "rounded.scad") (dir </> "rounded.stl") shape
      case (actual,expected) of
        (Right a,Right b) -> do
          assertT t (name++" bounds and volume") (geoClose (geoOf a) (geoOf b)) (show (geoOf a,geoOf b))
          symmetricDifference t name a b
        r -> failT t name (show (fmap length (fst r),fmap length (snd r)))
    forM_ ["examples/bracket.coscad","examples/assemble/tesseract/bolted/corner.coscad","examples/assemble/plan/rail2020.coscad","examples/extrusion.coscad","examples/gear_test.coscad"] $ \path -> do
      source<-readFile path
      case parseProgramNamed path source of
        Left err -> failT t path err
        Right (_,shape) -> do
          actual<-nativeCSGMesh shape
          expected<-renderShape OpenScadBackend (dir </> "corpus.scad") (dir </> "corpus.stl") shape
          case (actual,expected) of
            (Right a,Right b) -> symmetricDifference t path a b
            r -> failT t path (show (fmap length (fst r),fmap length (snd r)))
    -- Lofts (BOSL2 skin: reindex and distance) and round 2D offsets against
    -- the OpenSCAD render, on volume and bounds. A reindex loft is the same
    -- triangulation on both sides, so its volume agrees far below tolerance.
    forM_ [ ("reindex loft frustum",Loft [(0,Shape2D 50 10),(30,Shape2D 50 5)],1e-9)
          , ("reindex loft, offset circles",Loft [(0,Shape2D 40 6),(10,Tx 3 (Shape2D 40 4)),(14,Ty (-2) (Rz 20 (Shape2D 40 5)))],1e-9)
          , ("reindex loft, bezier to bezier",Loft [(0,bezPoly 8 [(0,0),(10,0),(10,10),(0,10),(-5,5),(-2,2),(0,0)]),(8,Rz 90 (Scale (0.5,0.5,1) (bezPoly 8 [(0,0),(10,0),(10,10),(0,10),(-5,5),(-2,2),(0,0)])))],1e-9)
          , ("distance loft, circle to triangle",Loft [(0,Shape2D 50 3),(6,Rz 30 (Shape2D 3 4))],1e-9)
          , ("distance loft, pentagon to square",Loft [(0,Shape2D 5 8),(12,Shape2D 4 5)],1e-9)
          , ("distance loft, three profiles",Loft [(0,Shape2D 6 4),(5,Shape2D 50 6),(9,Tx 1 (Shape2D 3 3))],1e-9)
          , ("offset triangle",Extrude 10 (Offset 3 (Shape2D 3 15)),1e-3)
          , ("offset circle",Extrude 70 (Offset 2 (Shape2D 50 22)),1e-3)
          , ("negative offset circle",Extrude 5 (Offset (-3) (Shape2D 50 24)),1e-3)
          , ("offset moved pentagon",Extrude 4 (Offset 1.5 (Tx 10 (Rz 15 (Shape2D 5 6)))),1e-3)
          , ("offset bezier",Extrude 6 (Offset 2 (bezPoly 8 [(0,0),(10,0),(10,10),(0,10),(-5,5),(-2,2),(0,0)])),1e-3)
          ] $ \(name,shape,relative) -> do
      actual<-nativeCSGMesh shape
      expected<-renderShape OpenScadBackend (dir </> "loft.scad") (dir </> "loft.stl") shape
      case (actual,expected) of
        (Right a,Right b) -> do
          let (va,loA,hiA)=geoOf a; (vb,_,_)=geoOf b
          assertT t ("native/OpenSCAD "++name++" bounds and volume") (geoClose (geoOf a) (geoOf b)) (show (geoOf a,geoOf b))
          assertT t ("native/OpenSCAD "++name++" volume within "++show relative) (abs (va-vb)<=relative*abs vb) (show (va,vb,loA,hiA))
          assertT t ("native "++name++" closed oriented surface") (closed a && signedVolume a>0) "non-manifold edge or reversed face"
        r -> failT t name (show (fmap length (fst r),fmap length (snd r)))
    forM_ ["examples/loft.coscad","examples/offset_demo.coscad","examples/waterbottle_advanced.coscad","examples/aerofold_phone_stand.coscad"] $ \path -> do
      source<-readFile path
      case parseProgramNamed path source of
        Left err -> failT t path err
        Right (_,shape) -> do
          actual<-nativeCSGMesh shape
          expected<-renderShape OpenScadBackend (dir </> "corpus.scad") (dir </> "corpus.stl") shape
          case (actual,expected) of
            (Right a,Right b) -> assertT t ("native/OpenSCAD "++path++" bounds and volume") (geoClose (geoOf a) (geoOf b)) (show (geoOf a,geoOf b))
            r -> failT t path (show (fmap length (fst r),fmap length (snd r)))
    -- OpenSCAD stays available as the explicit opt-out, and next records it
    bin<-findExecutable "coscad"
    case bin of
      Nothing -> failT t "opt-out CLI available" "coscad not on PATH"
      Just exe -> do
        optDir<-tempDir "backend-opt-out"
        writeFile (optDir </> "a.coscad") "main = box 20 20 20\n"
        writeFile (optDir </> "b.coscad") "main = box 4 4 4\n"
        writeFile (optDir </> "pair.assemble") "a ← a.coscad ×1\nb ← b.coscad ×1\nasm = a |> at top b\n"
        (stlCode,stlOut,stlErr)<-readProcessWithExitCode exe ["stl",optDir </> "a.coscad","--backend=openscad","-o",optDir </> "a.stl"] ""
        assertT t "--backend=openscad still renders stl" (stlCode==ExitSuccess && "volume 8000.000" `isInfixOf` stlOut) (stlOut++stlErr)
        (nextCode,nextOut,nextErr)<-readProcessWithExitCode exe ["next",optDir </> "pair.assemble","--backend=openscad"] ""
        manifest<-readJson (optDir </> "pair_manifest.json")
        assertT t "next --backend=openscad records the openscad backend" (nextCode==ExitSuccess && either (const False) (\m -> lookupKey "geometry_backend" m==Just (JString "openscad")) manifest) (nextOut++nextErr++show manifest)
        (checkCode,checkOut,checkErr)<-readProcessWithExitCode exe ["check",optDir </> "pair.assemble","--backend=openscad"] ""
        assertT t "check --backend=openscad still runs" (checkCode==ExitSuccess && "geometry backend: openscad" `isInfixOf` checkOut) (checkOut++checkErr)
  else return ()

cadTier :: T -> IO ()
cadTier t = do
  json<-readFile "experiments/cad/existing-post.json"
  source<-readFile "examples/assemble/constraints/post.coscad"
  case (parseCadProblem json,parseProgramNamed "post" source) of
    (Right p,Right (_,shape)) -> case solveCad shape p of
      Right r -> assertT t "existing post CAD analysis" (elementCount (gridSummary r)==160) (show (gridSummary r))
      Left err -> failT t "existing post CAD analysis" err
    r -> failT t "existing post inputs" (show r)
  expectErr t "CAD config rejects unknown keys" (parseCadProblem "{\"extra\":0}") ["required fields exactly"] []
  expectErr t "CAD config rejects duplicate keys" (parseCadProblem "{\"schema\":0,\"schema\":0}") ["required fields exactly"] []
  let shape=Rectangle 20 4 4
      spec=CadProblem (4,2,2) 2000 0 (1,0,0)
        (FaceRegion (-1,0,0) ((0,0,0),(0,4,4))) (FaceRegion (1,0,0) ((20,0,0),(20,4,4)))
      axial=defaultBeam {beamCells=(4,2,2),poisson=0,tipForceN=(1,0,0)}
  case (solveCad shape spec,solveBeam axial) of
    (Right a,Right b) -> do
      assertT t "CAD beam matches analytical fixture" (vlen (vadd (tipDisplacementMM (gridSummary a)) (mul (-1) (tipDisplacementMM b)))<1e-12) (show (gridSummary a,b))
      assertT t "CAD beam exports nodes and cell stresses" (length (nodeDisplacements a)==45 && length (cellCentreStresses a)==16) (show (gridSummary a))
      balance t axial (gridSummary a)
    r -> failT t "CAD beam solve" (show r)
  forM_
    [("unresolved thin feature",Diff shape (Translate (10,0,0) (Rectangle 0.1 4 4)),spec,"boundary falls inside")
    ,("unsupported curved CAD",Cylinder 2 20,spec,"unsupported Cylinder")
    ,("arbitrary rotation",Rz 15 shape,spec,"not axis aligned")
    ,("empty CAD",Diff shape shape,spec,"selects no exterior faces")
    ,("missing support face",shape,spec {cadSupport=FaceRegion (-1,0,0) ((5,0,0),(5,4,4))},"support selects no")
    ,("missing load face",shape,spec {cadLoad=FaceRegion (1,0,0) ((15,0,0),(15,4,4))},"load selects no")
    ,("partial load face",shape,spec {cadLoad=FaceRegion (1,0,0) ((20,0,0),(20,3,4))},"grid-aligned")
    ,("load on support",shape,spec {cadLoad=cadSupport spec},"unclamped nodes")
    ,("disconnected CAD",Diff shape (Translate (5,0,0) (Rectangle 5 4 4)),spec,"disconnected")
    ,("grid overflow CAD",shape,spec {cadCells=(maxBound,2,2)},"grid exceeds")
    ,("zero modulus CAD",shape,spec {cadYoungMPa=0},"Young's modulus")
    ] $ \(name,s,p,message) -> expectErr t name (solveCad s p) [message] []
  case prepareCad shape spec of
    Left e -> failT t "prepare grid" e
    Right grid -> do
      expectErr t "grid missing support node" (solveGrid grid {clampedNodes=[(100,0,0)]}) ["missing node"] []
      expectErr t "grid line support is unstable" (solveGrid grid {clampedNodes=[(0,0,0),(0,0,1),(0,0,2)]}) ["noncollinear"] []
      expectErr t "grid duplicate element" (solveGrid grid {activeCells=(0,0,0):activeCells grid}) ["unique"] []
      expectErr t "grid duplicate force is not normalized" (solveGrid grid {loadedNodes=loadedNodes grid++loadedNodes grid}) ["sum to one"] []
      expectErr t "grid empty elements" (solveGrid grid {activeCells=[]}) ["nonempty"] []
  let bracket=Diff (Rectangle 20 20 4) (Translate (10,10,0) (Rectangle 10 10 4))
      bracketSpec=CadProblem (8,8,2) 2000 0.3 (0,0,-1)
        (FaceRegion (1,0,0) ((20,0,0),(20,10,4))) (FaceRegion (0,1,0) ((0,20,0),(10,20,4)))
  case solveCad bracket bracketSpec of
    Left e -> failT t "CAD bracket" e
    Right r -> do
      let s=gridSummary r
      assertT t "CAD bracket removes cutout cells" (elementCount s==96) (show s)
      assertT t "CAD bracket force balance" (vlen (vadd (supportReactionN s) (0,0,-1))<1e-6) (show s)
      assertT t "CAD bracket moment balance" (vlen (vadd (supportMomentNMM s) (cross (5,20,2) (0,0,-1)))<1e-5) (show s)
      let (_,_,uz)=tipDisplacementMM s
      assertT t "CAD bracket work/energy" (abs (strainEnergyNMM s+uz/2)<1e-9) (show s)

symmetricDifference :: T -> String -> [Tri] -> [Tri] -> IO ()
symmetricDifference t name a b = do
  ab<-meshOperation MeshDifference a b
  ba<-meshOperation MeshDifference b a
  case (ab,ba) of
    (Right x,Right y) -> assertT t ("native/oracle symmetric difference "++name)
      (meshVolume x+meshVolume y < max 1e-5 (1e-5*meshVolume b)) (show (meshVolume x,meshVolume y))
    r -> failT t ("native/oracle symmetric difference "++name) (show (fmap length (fst r),fmap length (snd r)))

extrusionTier :: T -> IO ()
extrusionTier t = do
  let l=[(0,0),(4,0),(4,1),(1,1),(1,4),(0,4)]
      polygon pts=Poly (PD pts [])
      outer=polygon [(0,0),(4,0),(4,4),(0,4)]
      hole=polygon [(1,1),(3,1),(3,3),(1,3)]
  forM_ [("concave",Extrude 2 (polygon l),14)
        ,("clockwise",Extrude 2 (polygon (reverse l)),14)
        ,("closing vertex",Extrude 2 (polygon (l++[(0,0)])),14)
        ,("2D hole via difference",Extrude 2 (Diff outer hole),24)
        ,("2D reflected profile",Extrude 2 (Mirror (1,0,0) (polygon l)),14)] $ \(name,s,volume) -> do
    result<-nativeCSGMesh s
    case result of
      Left err -> failT t name err
      Right mesh -> assertT t name (closed mesh && abs (signedVolume mesh-volume)<1e-9) (show (meshVolume mesh))
  forM_ [("crossing polygon",[(0,0),(4,0),(0,3),(4,4)])
        ,("non-neighbour touch",[(0,0),(4,0),(4,4),(2,0),(0,4)])
        ,("zero area",[(0,0),(1,0),(2,0)])] $ \(name,pts) -> do
    result<-extrudePolygon 2 pts
    expectErr t name result ["Manifold:"] []

tetraTier :: T -> IO ()
tetraTier t = do
  let nodes=[(2,3,4),(4,3,4),(2,6,4),(2,3,8)]
  case tetraElementMatrix 2000 0.3 nodes of
    Left err -> failT t "Tet4 element" err
    Right k -> do
      let values f=concatMap (\p -> let (x,y,z)=f p in [x,y,z]) nodes
          apply u=[sum [k A.! (i,j)*v | (j,v)<-zip [0..] u] | i<-[0..11]]
          energy u=sum (zipWith (*) u (apply u))/2
          e=2000; nu=0.3; mu=e/(2*(1+nu)); lambda=e*nu/((1+nu)*(1-2*nu))
          rigid=[const (1,0,0),const (0,1,0),const (0,0,1),\(_,y,z)->(0,-z,y),\(x,_,z)->(z,0,-x),\(x,y,_)->(-y,x,0)]
      assertT t "Tet4 stiffness symmetric" (maximum [abs (k A.! (i,j)-k A.! (j,i)) | i<-[0..11],j<-[0..11]]<1e-10) "asymmetric"
      forM_ (zip [1::Int ..] rigid) $ \(i,f) -> assertT t ("Tet4 rigid mode "++show i) (maximum (map abs (apply (values f)))<1e-9) "nonzero rigid force"
      let normal=values (\(x,y,z)->(0.01*x,0.02*y,0.03*z))
          expected=4*(lambda*0.06*0.06/2+mu*(0.01*0.01+0.02*0.02+0.03*0.03))
          shear=values (\(_,y,_)->(0.01*y,0,0))
      assertT t "Tet4 normal strain energy patch" (abs (energy normal/expected-1)<1e-12) (show (energy normal,expected))
      assertT t "Tet4 shear strain energy patch" (abs (energy shear/(4*mu*0.01*0.01/2)-1)<1e-12) (show (energy shear))
  expectErr t "Tet4 rejects inverted tetrahedron" (tetraElementMatrix 2000 0.3 [(0,0,0),(0,1,0),(1,0,0),(0,0,1)]) ["inverted"] []
  expectErr t "Tet4 rejects collapsed tetrahedron" (tetraElementMatrix 2000 0.3 [(0,0,0),(1,0,0),(0,1,0),(1,1,0)]) ["degenerate"] []
  let points=[(5*fromIntegral i,4*fromIntegral j,4*fromIntegral k) | i<-[0::Int ..4],j<-[0::Int ..1],k<-[0::Int ..1]]
      node i j k=4*i+2*j+k
      cells=concat [let a=node i 0 0; b=node (i+1) 0 0; c=node i 1 0; d=node (i+1) 1 0; e=node i 0 1; f=node (i+1) 0 1; g=node i 1 1; h=node (i+1) 1 1
                   in [[a,b,d,h],[a,d,c,h],[a,c,g,h],[a,g,e,h],[a,e,f,h],[a,f,b,h]] | i<-[0..3]]
      spec=CadProblem (1,1,1) 2000 0 (1,0,0) (FaceRegion (-1,0,0) ((0,0,0),(0,4,4))) (FaceRegion (1,0,0) ((20,0,0),(20,4,4)))
      problem=TetraProblem points cells spec
  forM_ [("Tet4",solveTetra),("Tet10",solveTetraQuadratic)] $ \(label,solve) -> case solve problem of
    Left err -> failT t (label++" axial beam") err
    Right r -> do
      let s=gridSummary r; (ux,uy,uz)=tipDisplacementMM s
      assertT t (label++" analytical axial extension") (abs (ux/0.000625-1)<1e-8 && abs uy+abs uz<1e-10) (show s)
      assertT t (label++" analytical axial stress") (abs (maxCellCentreVonMisesMPa s/(1/16)-1)<1e-8) (show s)
      balance t defaultBeam {tipForceN=(1,0,0),poisson=0} s
  expectErr t "Tet4 duplicate element" (solveTetra problem {tetraElements=cells++take 1 cells}) ["duplicate element"] []
  expectErr t "Tet4 invalid index" (solveTetra problem {tetraElements=[[0,1,2,999]]}) ["element indices"] []
  expectErr t "Tet4 support missing" (solveTetra problem {tetraConditions=spec {cadSupport=FaceRegion (-1,0,0) ((5,0,0),(5,4,4))}}) ["support selects no"] []
  expectErr t "Tet4 load overlaps clamp" (solveTetra problem {tetraConditions=spec {cadLoad=cadSupport spec}}) ["overlaps"] []
  expectErr t "Tet4 disconnected bodies" (solveTetra problem {tetraNodes=points++map (vadd (100,0,0)) points,tetraElements=cells++map (map (+length points)) cells}) ["disconnected"] []

quadraticTetraTier :: T -> IO ()
quadraticTetraTier t = do
  let corners=[(0,0,0),(1,0,0),(0,1,0),(0,0,1)]
      nodes=corners++[mul 0.5 (vadd (corners!!i) (corners!!j)) | (i,j)<-[(0,1),(0,2),(0,3),(1,2),(1,3),(2,3)]]
  case quadraticTetraElementMatrix 2000 0.3 corners of
    Left e -> failT t "Tet10 matrix" e
    Right k -> do
      let values f=concatMap (\p -> let (x,y,z)=f p in [x,y,z]) nodes
          apply u=[sum [k A.! (i,j)*v | (j,v)<-zip [0..] u] | i<-[0..29]]
          energy u=sum (zipWith (*) u (apply u))/2
          e=2000; nu=0.3; mu=e/(2*(1+nu)); lambda=e*nu/((1+nu)*(1-2*nu))
          rigid=[const (1,0,0),const (0,1,0),const (0,0,1),\(_,y,z)->(0,-z,y),\(x,_,z)->(z,0,-x),\(x,y,_)->(-y,x,0)]
      assertT t "Tet10 symmetric stiffness" (maximum [abs (k A.! (i,j)-k A.! (j,i)) | i<-[0..29],j<-[0..29]]<1e-9) "asymmetric"
      forM_ (zip [1::Int ..] rigid) $ \(i,f) -> assertT t ("Tet10 rigid mode "++show i) (maximum (map abs (apply (values f)))<1e-9) "rigid body force"
      let normal=values (\(x,y,z)->(0.01*x,0.02*y,0.03*z))
          expected=(lambda*0.06*0.06/2+mu*(0.01*0.01+0.02*0.02+0.03*0.03))/6
      assertT t "Tet10 normal strain patch" (abs (energy normal/expected-1)<1e-12) (show (energy normal,expected))
      assertT t "Tet10 quadratic displacement energy" (abs (energy (values (\(x,_,_)->(x*x,0,0)))/((lambda+2*mu)/30)-1)<1e-12) "quadrature or shape gradient error"


-- Run native pipeline commands with an intentionally unusable OpenSCAD binary:
-- success cannot accidentally be supplied by a fallback subprocess.
nativePipelineTier :: T -> IO ()
nativePipelineTier t = do
  bin<-findExecutable "coscad"
  case bin of
    Nothing -> failT t "native pipeline CLI" "coscad missing"
    Just exe -> do
      dir<-tempDir "native-pipeline"
      environment<-getEnvironment
      let run args=readCreateProcessWithExitCode ((proc exe args) {env=Just (("COSCAD_OPENSCAD","/nonexistent/native-must-not-fallback"):filter ((/="COSCAD_OPENSCAD") . fst) environment)}) ""
          source=dir </> "pair.assemble"
          refs="a ← a.coscad ×1\nb ← b.coscad ×1\n"
      writeFile (dir </> "a.coscad") "main = box 20 20 20\n"
      writeFile (dir </> "b.coscad") "main = box 4 4 4\n"
      forM_ [("overlap","a |> at top 0 0 -1 b",ExitFailure 2,"OVERLAP")
            ,("contact","a |> at top b",ExitSuccess,"contact")
            ,("clearance","a |> at top 0 0 0.1 b",ExitSuccess,"WARN")
            ,("unused alias","a",ExitSuccess,"1 placed instances")
            ,("transformed","(a |> at top b) |> z 30",ExitSuccess,"contact")] $ \(label,expr,expected,fragment) -> do
        writeFile source (refs++"clearance 0.3\nasm = "++expr++"\n")
        (code,out,err)<-run ["check",source,"--backend=native-experimental"]
        assertT t ("native check "++label) (code==expected && fragment `isInfixOf` out) (out++err)
      -- A completely enclosed cavity must not count as a second positive solid.
      writeFile (dir </> "a.coscad") "main = box 20 20 20 ⊖ box 12 12 12\n"
      writeFile source (refs++"asm = a ⊕ b\n")
      (cavityCode,cavityOut,cavityErr)<-run ["check",source,"--backend=native-experimental"]
      assertT t "native check enclosed cavity" (cavityCode==ExitSuccess && "0 overlaps" `isInfixOf` cavityOut && "2 placed instances" `isInfixOf` cavityOut) (cavityOut++cavityErr)
      writeFile (dir </> "a.coscad") "main = box 20 20 20\n"
      writeFile source (refs++"asm = a |> at top b\n")
      (nextCode,nextOut,nextErr)<-run ["next",source,"--backend=native-experimental"]
      assertT t "native next without OpenSCAD" (nextCode==ExitSuccess) (nextOut++nextErr)
      if nextCode==ExitSuccess then do
        manifest<-readJson (dir </> "pair_manifest.json")
        assertT t "next records backend provenance" (either (const False) (\m -> lookupKey "geometry_backend" m==Just (JString "native-experimental")) manifest) (show manifest)
        mesh<-readStl (dir </> "pair_bed1.stl")
        assertT t "native next conserves packed part volume" (abs (meshVolume mesh-8064)<1e-6) (show (meshVolume mesh))
      else return ()
      writeFile (dir </> "a.coscad") "main = box 20 2 2\n"
      writeFile (dir </> "b.coscad") "main = box 2 20 2\n"
      writeFile source (refs++"clearance 0.3\nasm = a ⊕ (b |> move 0 0 2.1)\n")
      (edgeCode,edgeOut,edgeErr)<-run ["check",source,"--backend=native-experimental"]
      assertT t "native check crossed-bar edge clearance" (edgeCode==ExitSuccess && "WARN" `isInfixOf` edgeOut && "gap 0.1 mm" `isInfixOf` edgeOut) (edgeOut++edgeErr)
      writeFile (dir </> "a.coscad") "main = box 2 2 2\n"
      writeFile source "a ← a.coscad ×2\nb ← b.coscad ×1\nasm = a ⊕ (a |> move 2 2 0) ⊕ (b |> move 50 0 0)\n"
      (touchCode,touchOut,touchErr)<-run ["check",source,"--backend=native-experimental"]
      assertT t "native check separates edge-touching instances" (touchCode==ExitSuccess && "3 placed instances" `isInfixOf` touchOut && "contact" `isInfixOf` touchOut) (touchOut++touchErr)
      -- the default backend of a kernel build is native: no flag, no OpenSCAD
      writeFile (dir </> "a.coscad") "main = box 20 20 20\n"
      writeFile (dir </> "b.coscad") "main = box 4 4 4\n"
      writeFile source (refs++"asm = a |> at top b\n")
      (defaultCode,defaultOut,defaultErr)<-run ["next",source]
      assertT t "default next renders natively without OpenSCAD" (defaultCode==ExitSuccess) (defaultOut++defaultErr)
      if defaultCode==ExitSuccess then do
        manifest<-readJson (dir </> "pair_manifest.json")
        assertT t "default next records the native backend in its manifest" (either (const False) (\m -> lookupKey "geometry_backend" m==Just (JString (backendName defaultBackend))) manifest && defaultBackend==NativeExperimental) (show manifest)
      else return ()
      (defaultCheck,defaultCheckOut,_)<-run ["check",source]
      assertT t "default check runs natively without OpenSCAD" (defaultCheck==ExitSuccess && "geometry backend: native-experimental" `isInfixOf` defaultCheckOut) defaultCheckOut
      writeFile (dir </> "a.coscad") "main = ◉ 2 5\n"
      forM_ ["next","check"] $ \command -> do
        (code,_,err)<-run [command,source,"--backend=native-experimental"]
        assertT t ("native "++command++" refuses unsupported part") (code==ExitFailure 1 && "spindle/horn torus is unsupported" `isInfixOf` err) err
      -- lofts and offsets are native now; the whole example must render with OpenSCAD unreachable
      forM_ ["examples/loft.coscad","examples/offset_demo.coscad","examples/waterbottle_advanced.coscad"] $ \path -> do
        (code,out,err)<-run ["stl",path,"-o",dir </> "example.stl"]
        assertT t ("native stl renders "++path++" without OpenSCAD") (code==ExitSuccess && "volume" `isInfixOf` out) (out++err)
  shell<-nativeCSGMesh (Diff (Rectangle 20 20 20) (Rectangle 12 12 12))
  case shell >>= splitSolidBodies of
    Right [solid] -> assertT t "cavity grouping preserves net volume" (abs (meshVolume solid-6272)<1e-6) (show (meshVolume solid))
    r -> failT t "cavity grouping" (show r)


-- | The Haskell loft tessellation on its own (no kernel): BOSL2's frustum
-- of two equal-count circles, a distance-matched pair, the profile order
-- conventions, and the cap triangulator on a concave outline.
loftChecks :: T -> IO ()
loftChecks t = do
  let k = (50 / (2 * pi)) * sin (2 * pi / 50)
      a1 = k * pi * 100; a2 = k * pi * 25
      frustum = 30 / 3 * (a1 + a2 + sqrt (a1 * a2))
      layers shapes = mapM (\(z,p) -> (,) z <$> profilePath p) shapes
  case layers [(0,Shape2D 50 10),(30,Shape2D 50 5)] >>= loftTriangles "reindex" of
    Left e -> failT t "pure loft frustum" e
    Right tris -> do
      assertT t "pure loft frustum volume" (abs (meshVolume tris - frustum) < 1e-9 * frustum) (show (meshVolume tris, frustum))
      assertT t "pure loft frustum closed oriented surface" (closed tris && signedVolume tris > 0) "non-manifold edge or reversed face"
  case layers [(0,Shape2D 50 3),(6,Rz 30 (Shape2D 3 4))] >>= loftTriangles "distance" of
    Left e -> failT t "pure distance loft" e
    Right tris -> assertT t "pure distance loft closed oriented surface" (closed tris && signedVolume tris > 0 && length tris > 100) (show (length tris))
  assertT t "loftMethod picks reindex for equal counts and distance otherwise"
    (loftMethod [Shape2D 50 1,Shape2D 50 2] == "reindex" && loftMethod [Shape2D 50 1,Shape2D 3 2] == "distance") ""
  assertT t "BOSL2 circle paths are clockwise from angle 0"
    (case profilePath (Shape2D 4 1) of Right [(1,0),(0,-1),(-1,0),(0,1)] -> True; _ -> False) (show (profilePath (Shape2D 4 1)))
  assertT t "BOSL2 polygon paths are made counter-clockwise keeping the first point"
    (profilePath (Poly (PD [(0,0),(0,1),(1,1),(1,0)] [])) == Right [(0,0),(1,0),(1,1),(0,1)]) (show (profilePath (Poly (PD [(0,0),(0,1),(1,1),(1,0)] []))))
  expectErr t "offset inside a loft profile is refused" (profilePath (Offset 1 (Shape2D 8 2))) ["offset inside a loft profile"] []
  expectErr t "loft profile must be 2D" (profilePath (Rectangle 1 2 3)) ["2D outline"] []
  expectErr t "mismatched counts refused under reindex" (layers [(0,Shape2D 4 1),(1,Shape2D 5 1)] >>= loftTriangles "reindex") ["equal vertex counts"] []
  let lShape = [(0,0),(4,0),(4,1),(1,1),(1,3),(0,3)]
      area tris = sum [abs ((bx-ax)*(cy-ay)-(by-ay)*(cx-ax))/2 | ((ax,ay),(bx,by),(cx,cy)) <- tris]
  case triangulatePolygon lShape of
    Left e -> failT t "ear clipping of a concave outline" e
    Right tris -> assertT t "ear clipping of a concave outline" (length tris == 4 && abs (area tris - 6) < 1e-12) (show tris)
  case triangulatePolygon (reverse lShape ++ [(0,0),(0,0)]) of
    Left e -> failT t "ear clipping tolerates repeated points and clockwise input" e
    Right tris -> assertT t "ear clipping tolerates repeated points and clockwise input" (length tris == 4 && abs (area tris - 6) < 1e-12) (show tris)
  expectErr t "bow-tie cap (zero signed area) is an error" (triangulatePolygon [(0,0),(2,2),(2,0),(0,2)]) ["zero area"] []
  expectErr t "degenerate cap is an error" (triangulatePolygon [(0,0),(1,0),(2,0)]) ["zero area"] []

indexedContractTier :: T -> IO ()
indexedContractTier t = do
  expectErr t "indexed out-of-range face" (indexedMesh [(0,0,0),(1,0,0),(0,1,0)] [(0,1,3)]) ["invalid triangle indices"] []
  expectErr t "indexed negative face" (indexedMesh [(0,0,0),(1,0,0),(0,1,0)] [(0,1,-1)]) ["invalid triangle indices"] []
  expectErr t "indexed repeated face vertex" (indexedMesh [(0,0,0),(1,0,0),(0,1,0)] [(0,1,1)]) ["invalid triangle indices"] []
  expectErr t "indexed nonfinite position" (indexedMesh [(0/0,0,0)] []) ["nonfinite"] []
  expectErr t "indexed degenerate triangle" (indexedMesh [(0,0,0),(1,0,0),(2,0,0)] [(0,1,2)]) ["degenerate"] []
  expectErr t "soup refuses NaN before map ordering" (weldTriangles [((0/0,0,0),(1,0,0),(0,1,0))]) ["nonfinite"] []
  assertT t "indexed empty solid" (null (indexedTriangles emptyIndexed)) "empty mesh changed"

indexedKernelTier :: T -> IO ()
indexedKernelTier t = do
  let box=Rectangle 2 3 4
  forM_ [("point",(2,3,4)),("edge",(2,3,0))] $ \(label,offset) -> do
    let touching=Union [box,Translate offset box]
    result<-nativeIndexedCSGMesh touching
    case result of
      Left err -> failT t ("indexed "++label++" contact") err
      Right mesh -> do
        let vertices=indexedVertices mesh
        assertT t ("indexed "++label++" contact retains distinct coincident vertices")
          (length (nub vertices)<length vertices && closedIndexed mesh && abs (meshVolume (indexedTriangles mesh)-48)<1e-8)
          (show (length vertices,length (nub vertices),indexedFaces mesh))
        assertT t ("indexed "++label++" contact components")
          (length (indexedComponents mesh)==2 && either (const False) ((==2) . length) (splitIndexedSolids mesh)) "touching solids were joined by position"
        roundTrip<-indexedOperation MeshUnion mesh emptyIndexed
        assertT t ("indexed "++label++" contact round trip")
          (either (const False) (\m -> closedIndexed m && abs (meshVolume (indexedTriangles m)-48)<1e-8) roundTrip) (show roundTrip)
    let cutter=Rectangle 1 1 1
    nested<-nativeIndexedCSGMesh (Diff (Mirror (1,0,0) touching) (Mirror (1,0,0) cutter))
    assertT t ("indexed nested CSG after "++label++" contact and reflection")
      (either (const False) (\m -> closedIndexed m && abs (meshVolume (indexedTriangles m)-47)<1e-8) nested) (show nested)
  invalidLimit<-indexedOperationWithLimit 0 MeshUnion emptyIndexed emptyIndexed
  expectErr t "native deadline range" invalidLimit ["timeout must be"] []
  where
    closedIndexed mesh = all (\ss -> length ss==2 && sum ss==0) (Map.elems edges)
      where edges=Map.fromListWith (++) [(if u<v then (u,v) else (v,u),[if u<v then 1::Int else -1])
                                      | (a,b,c)<-indexedFaces mesh,(u,v)<-[(a,b),(b,c),(c,a)]]


distanceTier :: T -> IO ()
distanceTier t = do
  let a=((-10,0,0),(10,0,0),(-10,-1,0))
      b=((0,-10,0.1),(0,10,0.1),(1,10,0.1))
      near expected got=finiteD got && abs (got-expected)<1e-8
      finiteD x=not (isNaN x || isInfinite x)
      moved f (p,q,r)=(f p,f q,f r)
      rigid=vadd (12,34,-56) . mApply (rodMat 37 (1,2,3))
  assertT t "skew segment interior distance" (near 0.1 (segmentDistance ((-10,0,0),(10,0,0)) ((0,-10,0.1),(0,10,0.1)))) "missed interior minimum"
  assertT t "parallel segment distance" (near 2 (segmentDistance ((0,0,0),(10,0,0)) ((2,2,0),(8,2,0)))) "parallel segment error"
  assertT t "degenerate segment distance" (near 1 (segmentDistance ((0,0,0),(0,0,0)) ((-1,1,0),(1,1,0)))) "point/segment error"
  assertT t "nearly parallel segment distance" (near 0.1 (segmentDistance ((0,0,0),(10,0,0)) ((0,0,0.1),(10,1e-11,0.1)))) "near parallel error"
  assertT t "triangle edge/edge distance" (near 0.1 (triangleDistance a b)) "vertices alone do not find this gap"
  assertT t "triangle distance rigid-motion invariance" (near 0.1 (triangleDistance (moved rigid a) (moved rigid b))) "rotation changed clearance"
  let crossed=triangleDistance ((-2,-2,0),(2,-2,0),(0,2,0)) ((-0.1,0,-1),(0.1,0,-1),(0,0,1))
  assertT t "triangle interior crossing" (near 0 crossed) "missed edge/face intersection"
  assertT t "empty surface distance is explicit" (meshDistance [] [a]==Nothing) "empty input accepted"
  assertT t "nonfinite surface distance is explicit" (meshDistance [((0/0,0,0),(1,0,0),(0,1,0))] [b]==Nothing) "NaN accepted"
  let aa=[moved (vadd (fromIntegral i*40,0,0)) a | i<-[0::Int ..15]]
      bb=[moved (vadd (0,fromIntegral i*40,0)) b | i<-[0::Int ..15]]
  forM_ [(aa,bb),(reverse aa,reverse bb),(bb,aa)] $ \(left,right) ->
    assertT t "surface BVH finds unsampled interior gap" (maybe False (near 0.1) (meshDistance left right)) "hierarchy pruned nearest triangles"
