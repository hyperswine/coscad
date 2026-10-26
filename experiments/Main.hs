-- A separate executable so a numerical experiment cannot be mistaken for
-- a qualified `coscad stress` result on an arbitrary printable part.
module Main (main) where

import Coscad.Json (JValue (..), encodePretty)
import Coscad.Stress.Experiment
import Coscad.Stress.Cad
import Coscad.Stress.Tetra
import Coscad.Backend (nativeMesh, nativeCSGMesh)
import Coscad.Manifold (manifoldAvailable)
import Coscad.Mesh (meshVolume, meshBounds)
import Coscad.Geometry (resolve)
import Coscad.IO (readFileUtf8)
import Coscad.Parser (parseProgramNamed)
import Coscad.Shape (Shape, shapeChildren)
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
    ["beam",mode,x,y,z] | mode `elem` ["axial","bend"] ->
      case (readMaybe x,readMaybe y,readMaybe z) :: (Maybe Integer,Maybe Integer,Maybe Integer) of
        (Just nx,Just ny,Just nz) | all (\n -> n>=1 && n<=128) [nx,ny,nz] -> run mode (fromInteger nx,fromInteger ny,fromInteger nz)
        _ -> bad "cell counts must be integers in [1,128]"
    _ -> bad "usage: coscad-experiments beam axial|bend NX NY NZ | inventory | cad MODEL.coscad CONDITIONS.json | tetra|tetra2 MESH.json LOADS.json"

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
