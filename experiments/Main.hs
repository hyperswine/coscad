-- A separate executable so a numerical experiment cannot be mistaken for
-- a qualified `coscad stress` result on an arbitrary printable part.
module Main (main) where

import Coscad.Json (JValue (..), encodePretty)
import Coscad.Stress.Experiment
import Coscad.Stress.Cad
import Coscad.Stress.Tetra
import Coscad.Stress.Hex20
import Coscad.Stress.Voxel
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Coscad.Backend (nativeMesh, nativeCSGMesh)
import Coscad.Manifold (manifoldAvailable)
import Coscad.Mesh (meshVolume, meshBounds)
import Coscad.Geometry (resolve)
import Coscad.IO (readFileUtf8, writeFileUtf8)
import Coscad.Parser (parseProgramNamed)
import Coscad.Shape (Shape, shapeChildren)
import qualified Coscad.Shape
import Data.List (nub, sort)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)

main :: IO ()
main = catchIOError runMain (bad . show)

runMain :: IO ()
runMain = do
  args <- getArgs
  case args of
    ["inventory"] -> inventory
    ["cad",source,conditions] -> runCad source conditions
    ["tetra",mesh,conditions] -> runTetra False mesh conditions
    ["tetra2",mesh,conditions] -> runTetra True mesh conditions
    ["voxel-mesh",source,cell,output] -> case readMaybe cell of
        Just h | h > 0 -> voxelMesh source h output
        _ -> bad "voxel-mesh needs a positive cell size in mm"
    ["hex20",mode,ny] | mode `elem` ["axial","bend"] -> case readMaybe ny of
        Just n | n>=1 && n<=64 -> runHex20 mode n
        _ -> bad "hex20 needs a cell count per 4 mm side in [1,64]"
    ["beam",mode,x,y,z] | mode `elem` ["axial","bend"] ->
      case (readMaybe x,readMaybe y,readMaybe z) :: (Maybe Integer,Maybe Integer,Maybe Integer) of
        (Just nx,Just ny,Just nz) | all (\n -> n>=1 && n<=128) [nx,ny,nz] -> run mode (fromInteger nx,fromInteger ny,fromInteger nz)
        _ -> bad "cell counts must be integers in [1,128]"
    _ -> bad "usage: coscad-experiments beam axial|bend NX NY NZ | hex20 axial|bend NY | voxel-mesh MODEL.coscad CELL MESH.json | inventory | cad MODEL.coscad CONDITIONS.json | tetra|tetra2 MESH.json LOADS.json"

-- The checked-in geometry corpus is the acceptance population; unrelated
-- untracked designs are not silently turned into regression baselines.
inventory :: IO ()
inventory = do
  golden <- readFileUtf8 "test/golden/geometry.txt"
  rows <- mapM inspect [f | line <- lines golden, (f:_) <- [words line], take 1 f /= "#"]
  putStrLn (encodePretty (JObject
    [("schema",JString "coscad.native-coverage.v1"),("manifold_linked",JBool manifoldAvailable),("examples",JArray rows)]))
  where
    inspect path = do
      source <- readFileUtf8 path
      case parseProgramNamed path source of
        Left err -> return (JObject [("file",JString path),("parse_error",JString err)])
        Right (_,shape) -> do
          let resolved=resolve shape
          actual <- nativeCSGMesh resolved
          return (JObject
            [("file",JString path)
            ,("features",JArray (map JString (sort (nub (features resolved)))))
            ,("primitive_native",summary (nativeMesh resolved))
            ,("native",summary actual)])
    summary (Left err) = JObject [("supported",JBool False),("reason",JString err)]
    summary (Right tris) = let (lo,hi)=meshBounds tris in JObject
      [("supported",JBool True),("triangles",JNumber (fromIntegral (length tris)))
      ,("volume_mm3",JNumber (meshVolume tris)),("bounds_mm",JArray [v3 lo,v3 hi])]
    features :: Shape -> [String]
    features s = takeWhile (/=' ') (show s) : concatMap features (shapeChildren s)

run :: String -> (Int,Int,Int) -> IO ()
run mode cells = do
  let p = defaultBeam { beamCells=cells
                      , tipForceN=if mode=="axial" then (1,0,0) else (0,0,-1)
                      , poisson=if mode=="axial" then 0 else 0.3 }
  case solveBeam p of
    Left err -> bad err
    Right r -> do
      let (ux,_,uz)=tipDisplacementMM r
          observed=if mode=="axial" then ux else negate uz
          -- E=2000 MPa, L=20 mm, b=h=4 mm, P=1 N.
          theory=if mode=="axial" then 20/(2000*16) else 20^(3::Int)/(3*2000*(4*4^(3::Int)/12))
      putStrLn (encodePretty (JObject
        [ ("schema",JString "coscad.fea-experiment.v1")
        , ("experimental",JBool True)
        , ("case",JString mode)
        , ("size_mm",v3 (beamSize p))
        , ("cells",let (a,b,c)=cells in JArray (map (JNumber . fromIntegral) [a,b,c]))
        , ("young_mpa",JNumber (youngMPa p)), ("poisson",JNumber (poisson p))
        , ("total_tip_force_n",v3 (tipForceN p))
        , ("tip_displacement_mm",v3 (tipDisplacementMM r))
        , ("support_reaction_n",v3 (supportReactionN r))
        , ("support_moment_n_mm",v3 (supportMomentNMM r))
        , ("strain_energy_n_mm",JNumber (strainEnergyNMM r))
        , ("max_cell_centre_von_mises_mpa",JNumber (maxCellCentreVonMisesMPa r))
        , ("analytic_displacement_mm",JNumber theory)
        , ("analytic_relative_error",JNumber (abs (observed/theory-1)))
        , ("free_residual_relative",JNumber (freeResidualRelative r))
        , ("iterations",JNumber (fromIntegral (solverIterations r)))
        , ("elements",JNumber (fromIntegral (elementCount r)))
        , ("degrees_of_freedom",JNumber (fromIntegral (degreeCount r)))
        , ("limitations",JArray (map JString
          [ "Homogeneous isotropic full-integration Hex8 baseline, not qualified for printed parts."
          , "Clamped x=0 face; uniform end traction. No arbitrary CAD, contact, infill or layer anisotropy."
          , "Bending and volumetric locking require refinement; centre stresses omit boundary peaks."
          , "Euler-Bernoulli bending reference omits transverse shear and local clamp effects."
          , "No material strength or safety-factor conclusion is made."
          ]))
        ]))

v3 :: (Double,Double,Double) -> JValue
v3 (x,y,z)=JArray (map JNumber [x,y,z])

bad :: String -> IO ()
bad msg=hPutStrLn stderr ("Error: "++msg) >> exitFailure

runCad :: FilePath -> FilePath -> IO ()
runCad path conditions = do
  source<-readFileUtf8 path
  config<-readFileUtf8 conditions
  case do
    (_,shape)<-parseProgramNamed path source
    spec<-parseCadProblem config
    grid<-prepareCad shape spec
    result<-solveGrid grid
    return (grid,result) of
    Left err -> bad err
    Right (grid,result) -> putStrLn (encodePretty (JObject
      [("schema",JString "coscad.fea-cad-result.v1"),("experimental",JBool True)
      ,("source",JString path),("conditions",JString conditions),("grid",gridJson grid),("result",resultJson result)
      ,("limitations",JArray (map JString
        ["Exact grid-aligned rectangular CSG only. One face-connected solid, one fully clamped region, one uniform total force."
        ,"Homogeneous isotropic small-strain Hex8; no joints, contact, infill, layer anisotropy or strength verdict."
        ,"Cell-centre stress omits surface peaks; clamp/reentrant singularities and bending require refinement."
        ,"Support moment is about the world origin; all lengths mm, forces N, stresses MPa."]))]))

runTetra :: Bool -> FilePath -> FilePath -> IO ()
runTetra quadratic mesh conditions = do
  meshText<-readFileUtf8 mesh
  conditionText<-readFileUtf8 conditions
  case parseTetraProblem meshText conditionText >>= (if quadratic then solveTetraQuadratic else solveTetra) of
    Left err -> bad err
    Right result -> putStrLn (encodePretty (JObject
      [("schema",JString "coscad.fea-tet-result.v1"),("experimental",JBool True),("element_order",JNumber (if quadratic then 2 else 1))
      ,("mesh",JString mesh),("conditions",JString conditions),("result",resultJson result)
      ,("limitations",JArray (map JString ["Homogeneous isotropic small-strain tetrahedra. Refinement is mandatory for bending and stresses; quadratic mode adds midpoint nodes on the same faceted geometry."
        ,"Mesh geometry acceptance is separate from solver accuracy. No contact, joints, infill, layer anisotropy or material strength verdict."
        ,"Support and uniform total force select complete exterior triangles on planar regions; moments are about world origin."]))]))

-- | The same 20 x 4 x 4 cantilever on the voxel/Hex20 path: the box is
-- tessellated natively, voxelized, clamped at x=0 and loaded at x=20.
runHex20 :: String -> Int -> IO ()
runHex20 mode ny = do
  started <- getCurrentTime
  let h = 4 / fromIntegral ny
      tris = either error id (nativeMesh (resolve (Coscad.Shape.Rectangle 20 4 4)))
  grid <- either (\e -> bad e >> fail e) return (voxelize h tris)
  let force = if mode=="axial" then (1,0,0) else (0,0,-1)
      nu = if mode=="axial" then 0 else 0.3
      faces = exteriorFaces grid
      atX0 = [f | f@(Face (i,_,_) 0 (-1)) <- faces, i==0]
      (nx,_,_) = vgCounts grid
      atX1 = [f | f@(Face (i,_,_) 0 1) <- faces, i==nx-1]
      clamped = Set.fromList [k | f <- atX0, (k,_) <- faceNodes f]
      perFace = 1 / fromIntegral (length atX1)
      loads = Map.fromListWith vadd3 [ (k, scale (w*perFace) force) | f <- atX1, (k,w) <- faceNodes f ]
      problem = Hex20Problem grid 2000 nu clamped loads 1e-8 20000
  case solveHex20 problem of
    Left err -> bad err
    Right r -> do
      finished <- getCurrentTime
      let tipNodes = [ (k,d) | (k,_,d) <- hrNodes r, Map.member k loads ]
          weights = Map.fromListWith (+) [ (k, w*perFace) | f <- atX1, (k,w) <- faceNodes f ]
          tip = foldl vadd3 (0,0,0) [ scale (weights Map.! k) d | (k,d) <- tipNodes ]
          (ux,_,uz) = tip
          observed = if mode=="axial" then ux else negate uz
          theory = if mode=="axial" then 20/(2000*16) else 20^(3::Int)/(3*2000*(4*4^(3::Int)/12))
          peak = maximum [ csPeakVonMises s | (_,_,s) <- hrCells r ]
      putStrLn (encodePretty (JObject
        [ ("schema",JString "coscad.fea-hex20-experiment.v1"), ("case",JString mode)
        , ("cell_mm",JNumber h), ("cells",JNumber (fromIntegral (voxelCount grid))), ("degrees_of_freedom",JNumber (fromIntegral (hrDofs r)))
        , ("voxel_volume_mm3",JNumber (voxelVolume grid))
        , ("tip_displacement_mm",v3 tip), ("analytic_displacement_mm",JNumber theory), ("analytic_relative_error",JNumber (abs (observed/theory-1)))
        , ("support_reaction_n",v3 (hrReaction r)), ("support_moment_n_mm",v3 (hrMoment r)), ("strain_energy_n_mm",JNumber (hrEnergy r))
        , ("max_gauss_von_mises_mpa",JNumber peak), ("iterations",JNumber (fromIntegral (hrIterations r))), ("free_residual_relative",JNumber (hrResidual r))
        , ("seconds",JNumber (realToFrac (diffUTCTime finished started))) ]))
  where
    vadd3 (a,b,c) (d,e,f) = (a+d,b+e,c+f)
    scale s (a,b,c) = (s*a,s*b,s*c)

-- | A conforming tetrahedral mesh for the tetra/tetra2 experiments without
-- Gmsh: the part is voxelized and every cube is split into six tetrahedra
-- along its main diagonal (the Kuhn subdivision, which is conforming across
-- neighbouring cubes). Output follows the coscad.fea-tet-mesh.v1 schema.
voxelMesh :: FilePath -> Double -> FilePath -> IO ()
voxelMesh source h output = do
  text <- readFileUtf8 source
  shape <- either (\e -> bad e >> fail e) (return . snd) (parseProgramNamed source text)
  tris <- nativeCSGMesh (resolve shape) >>= either (\e -> bad e >> fail e) return
  grid <- either (\e -> bad e >> fail e) return (voxelize h tris)
  let cells = Set.toList (vgCells grid)
      corner (i,j,k) (a,b,c) = (i+a, j+b, k+c)
      vertexKeys = Set.toList (Set.fromList [ corner c d | c <- cells, d <- [(a,b,e) | a <- [0,1], b <- [0,1], e <- [0,1]] ])
      index = Map.fromList (zip vertexKeys [0 :: Int ..])
      at c d = index Map.! corner c d
      -- six tetrahedra sharing the diagonal from (0,0,0) to (1,1,1), positively oriented
      kuhn c = [ [at c (0,0,0), at c p, at c q, at c (1,1,1)] | (p,q) <- [((1,0,0),(1,1,0)), ((1,1,0),(0,1,0)), ((0,1,0),(0,1,1)), ((0,1,1),(0,0,1)), ((0,0,1),(1,0,1)), ((1,0,1),(1,0,0))] ]
      point key = let (x,y,z) = cellCorner grid key in JArray (map JNumber [x,y,z])
      doc = JObject
        [ ("schema", JString "coscad.fea-tet-mesh.v1")
        , ("nodes_mm", JArray (map point vertexKeys))
        , ("tetrahedra", JArray [ JArray (map (JNumber . fromIntegral) tet) | c <- cells, tet <- kuhn c ])
        , ("provenance", JObject [ ("mesher", JString "coscad voxel Kuhn split"), ("source", JString source), ("cell_mm", JNumber h)
                                 , ("solid_cells", JNumber (fromIntegral (voxelCount grid))), ("voxel_volume_mm3", JNumber (voxelVolume grid)), ("mesh_volume_mm3", JNumber (meshVolume tris)) ]) ]
  writeFileUtf8 output (encodePretty doc ++ "\n")
  putStrLn ("Wrote " ++ output ++ ": " ++ show (length vertexKeys) ++ " nodes, " ++ show (6 * length cells) ++ " tetrahedra from " ++ show (length cells) ++ " cells")
