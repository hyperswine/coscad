-- | `coscad stress`: directive parsing, the Hex20 element, voxel meshing,
-- region selection and the command end to end on small fixtures.
module Tier.Stress (stressTier) where

import Coscad.Backend (Backend (..), nativeMesh, nativeCSGMesh)
import Coscad.Geometry (resolve)
import Coscad.Json (JValue (..), lookupKey, asNumber, asString)
import Coscad.Parser (parseProgramFull)
import Coscad.Report (Failure (..), silent)
import Coscad.Shape (Shape (..))
import Coscad.Stress.Command (processStress)
import Coscad.Stress.Hex20
import Coscad.Stress.Material (materialNamed, materialTable, matName)
import Coscad.Stress.Part
import Coscad.Directive
import Coscad.Stress.Voxel
import Data.Array.Unboxed ((!))
import Control.Exception (try, IOException)
import Control.Monad (forM_)
import Data.List (isInfixOf, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Harness
import System.Directory (doesFileExist, createDirectory, listDirectory)
import System.FilePath ((</>))

stressTier :: T -> IO ()
stressTier t = do
  section "stress: directives"
  let prog body = parseProgramFull "<stress>" ("main = box 10 10 10 |> datum tip top 0 0 0\n" ++ body)
  case prog "support bot\nload @tip 0 0 -5\nload top 1 0 0\nsupport @tip lft\nstress material=petg safety=3 cell=0.5\n" of
    Left err -> failT t "directives parse" err
    Right (_, _, a, _) -> do
      assertT t "two supports" (aSupports a == [AtBox (0,0,-1), AtDatumFace "tip" (-1,0,0)]) (show (aSupports a))
      assertT t "two loads" (aLoads a == [(AtDatum "tip", (0,0,-5)), (AtBox (0,0,1), (1,0,0))]) (show (aLoads a))
      assertT t "settings in order" (aSettings a == [("material","petg"),("safety","3"),("cell","0.5")]) (show (aSettings a))
      case setupFrom a of
        Left err -> failT t "setup" err
        Right s -> assertT t "setup values" (matName (suMaterial s) == "petg" && suSafety s == 3 && suCell s == Just 0.5) (show s)
  expectErr t "corner is not a face" (prog "support top+rt\n") ["not a face anchor"] []
  expectErr t "zero load" (prog "load top 0 0 0\n") ["nonzero force"] []
  expectErr t "unknown setting" (prog "stress colour=red\n") ["unknown stress setting"] []
  case prog "support = box 1 1 1\nload = box 2 2 2\n" of
    Left err -> failT t "definitions named support/load still parse" err
    Right (_, _, a, _) -> assertT t "no directives from definitions" (a == emptyAnalysis) (show a)
  expectErr t "unknown material" (setupFrom emptyAnalysis { aSettings = [("material","unobtainium")] }) ["unknown material"] []
  expectErr t "safety range" (setupFrom emptyAnalysis { aSettings = [("safety","0.5")] }) ["safety="] []
  assertT t "material table names are unique" (length (Set.toList (Set.fromList (map matName materialTable))) == length materialTable) ""
  assertT t "pla lookup is case-insensitive" (either (const False) ((== "pla") . matName) (materialNamed "PLA")) ""

  section "stress: Hex20 element"
  let ke = elementStiffness 2000 0.3 1.5
      rigid d = [ if i == d then 1 else 0 | _ <- [0..19 :: Int], i <- [0..2 :: Int] ]
      row r = [ ke ! (r*60 + c) | c <- [0..59] ]
      zeroForce u = maximum [ abs (sum (zipWith (*) (row r) u)) | r <- [0..59] ]
  assertT t "translations carry no force" (maximum [ zeroForce (rigid d) | d <- [0..2] ] < 1e-9) ""
  let sym = maximum [ abs (ke ! (r*60+c) - ke ! (c*60+r)) | r <- [0..59], c <- [0..59] ]
  assertT t "element matrix is symmetric" (sym < 1e-9) (show sym)
  -- patch test: a 2x2x2 block under uniform axial traction shows uniform stress
  let block = either error id (nativeMesh (resolve (Rectangle 2 2 2)))
  case voxelize 1 block of
    Left err -> failT t "voxelize block" err
    Right grid -> do
      assertT t "block has eight cells" (voxelCount grid == 8) (show (voxelCount grid))
      let faces = exteriorFaces grid
          atX0 = [f | f@(Face (i,_,_) 0 (-1)) <- faces, i == 0]
          atX1 = [f | f@(Face (i,_,_) 0 1) <- faces, i == 1]
          clamped = Set.fromList [k | f <- atX0, (k,_) <- faceNodes f]
          loads = Map.fromListWith (\(a,b,c) (d,e,g) -> (a+d,b+e,c+g)) [ (k, (w/4, 0, 0)) | f <- atX1, (k,w) <- faceNodes f ]
      case solveHex20 (Hex20Problem grid 1000 0 clamped loads 1e-10 1000) of
        Left err -> failT t "patch solve" err
        Right r -> do
          let sxx = [ x | (_,_,s) <- hrCells r, (x:_) <- [csCentre s] ]
              peaks = [ csPeakVonMises s | (_,_,s) <- hrCells r ]
          assertT t "uniform stress 0.25 MPa in every cell" (all (\v -> abs (v - 0.25) < 1e-9) sxx) (show sxx)
          assertT t "Gauss peaks equal the centre value" (all (\v -> abs (v - 0.25) < 1e-9) peaks) (show peaks)
          assertT t "reaction balances the load" (let (x,_,_) = hrReaction r in abs (x + 1) < 1e-9) (show (hrReaction r))
  -- cantilever against the beam formula, within the 3D elasticity margin
  let beam = either error id (nativeMesh (resolve (Rectangle 20 4 4)))
  case voxelize 2 beam of
    Left err -> failT t "voxelize beam" err
    Right grid -> do
      let faces = exteriorFaces grid
          atX0 = [f | f@(Face (i,_,_) 0 (-1)) <- faces, i == 0]
          atX1 = [f | f@(Face (i,_,_) 0 1) <- faces, i == 9]
          clamped = Set.fromList [k | f <- atX0, (k,_) <- faceNodes f]
          loads = Map.fromListWith (\(a,b,c) (d,e,g) -> (a+d,b+e,c+g)) [ (k, (0, 0, -w/4)) | f <- atX1, (k,w) <- faceNodes f ]
      case solveHex20 (Hex20Problem grid 2000 0.3 clamped loads 1e-9 5000) of
        Left err -> failT t "cantilever solve" err
        Right r -> do
          let tip = maximum [ negate z | (_, (x,_,_), (_,_,z)) <- hrNodes r, x > 19.9 ]
              theory = 20^(3::Int) / (3*2000*(4*4^(3::Int)/12))
          assertT t "cantilever tip within 3% of PL^3/3EI" (abs (tip/theory - 1) < 0.03) (show (tip, theory))
          assertT t "residual accepted below tolerance" (hrResidual r <= 1e-9) (show (hrResidual r))

  section "stress: voxelizer"
  case voxelize 0.7 (either error id (nativeMesh (resolve (Cylinder 3 5)))) of
    Left err -> failT t "voxelize cylinder" err
    Right grid -> do
      let ratio = voxelVolume grid / (pi*9*5)
      assertT t "cylinder volume within 15% at 0.7 mm cells" (abs (ratio - 1) < 0.15) (show ratio)
      assertT t "one body" (length (connectedCells grid) == 1) ""
  expectErr t "open mesh is refused" (voxelize 1 (drop 2 (either error id (nativeMesh (resolve (Rectangle 2 2 2)))))) ["not closed"] []

  planeSelectionTests t

  section "stress: command"
  dir <- tempDir "stress"
  let part = dir </> "bar.coscad"
  writeFile part (unlines
    [ "main = ▬ 20 4 4 |> datum tip ctr 10 0 2"
    , "support lft"
    , "load @tip top 0 0 -1"
    , "stress material=pla safety=2 cell=1" ])
  r' <- processStress Native silent False part Nothing
  case r' of
    Left (Failure code ls) -> failT t "stress command on a bar" (show code ++ " " ++ unlines ls)
    Right () -> do
      let out = dir </> "bar_stress.json"
      exists <- doesFileExist out
      assertT t "report written" exists out
      parsed <- readJson out
      case parsed of
        Left err -> failT t "report parses" err
        Right j -> do
          let num path = foldl (\acc k -> acc >>= lookupKey k) (Just j) path >>= asNumber
          assertT t "schema" (lookupKey "schema" j == Just (JString "coscad.stress/1")) ""
          -- 1 N spread along the whole 20 mm top face: a uniformly loaded cantilever, tip = qL^4/8EI = PL^3/8EI
          let uniform = 20^(3::Int) / (8 * 3500 * (4 * 4^(3::Int) / 12))
          assertT t "deflection near the uniform-load beam formula" (maybe False (\d -> abs (d / uniform - 1) < 0.05) (num ["result","max_displacement_mm"])) (show (num ["result","max_displacement_mm"], uniform))
          assertT t "safety reported" (maybe False (> 1) (num ["safety","in_plane"])) ""
  writeFile part (unlines [ "main = ▬ 20 4 4", "support lft", "load rt 0 0 -500", "stress material=pla safety=2 cell=1" ])
  r2 <- processStress Native silent False part Nothing
  case r2 of
    Left (Failure 2 ls) -> assertT t "exit 2 names the safety factor" (any ("safety factor" `isInfixOf`) ls) (unlines ls)
    other -> failT t "overloaded bar exits 2" (show (either failExit (const 0) other))
  writeFile part (unlines [ "main = ▬ 20 4 4", "load rt 0 0 -1" ])
  r3 <- processStress Native silent False part Nothing
  case r3 of
    Left (Failure 1 ls) -> assertT t "missing support is an input error" (any ("no support" `isInfixOf`) ls) (unlines ls)
    other -> failT t "missing support exits 1" (show (either failExit (const 0) other))

  publicationTests t

planeSelectionTests :: T -> IO ()
planeSelectionTests t = do
  section "stress: exact and ambiguous stepped planes; tension-only recovery"
  let stepped = "main = block 2 2 2 |> add (block 2 2 2 |> x 2 |> z 1)"
      solve text = case parseProgramFull "<plane-test>" text of
        Left err -> return (Left err)
        Right (_,shape,a,_) -> case setupFrom a of
          Left err -> return (Left err)
          Right setup -> do
            mesh <- nativeCSGMesh shape
            return (mesh >>= analysePart a setup shape)
      program geometry support load = unlines [geometry,support,load,"stress material=pla cell=1"]
  exact <- solve (program stepped "support bot" "load top 0 0 -1")
  case exact of
    Left err -> failT t "stepped part solves" err
    Right r -> do
      assertT t "only lowest step is clamped" (map rrFaces (srSupports r)==[4]) (show (srSupports r))
      assertT t "only highest step is loaded" (map rrFaces (srLoads r)==[4]) (show (srLoads r))
  ambiguous <- solve (program (stepped ++ " |> datum mid bot 0 0 0.5") "support @mid bot" "load top 0 0 -1")
  expectErr t "midway datum refuses two equally near planes" ambiguous ["ambiguous face plane"] []
  snapped <- solve (program (stepped ++ " |> datum face top 0 0 0.2") "support bot" "load @face top 0 0 -1")
  case snapped of
    Left err -> failT t "nearby datum selects unique voxel plane" err
    Right r -> assertT t "nearby datum does not broaden patch" (map rrFaces (srLoads r)==[4]) ""
  missing <- solve (program (stepped ++ " |> datum far top 0 0 10") "support bot" "load @far top 0 0 -1")
  expectErr t "far datum refuses missing plane" missing ["selects no exterior face"] []
  forM_ [("compression",-1),("tension",1)] $ \(label,force) -> do
    result <- solve (program "main = block 2 2 2" "support bot" ("load top 0 0 " ++ show (force :: Int)))
    case result of
      Left err -> failT t label err
      Right r -> do
        assertT t (label ++ " has nonnegative tensile demand") (all (\(_,_,c) -> csPeakTensionZ c >= 0) (hrCells (srResult r))) ""
        assertT t (label ++ " tensile demand is correctly recovered")
          (if force<0 then fst (srAcrossLayers r)==0 else fst (srAcrossLayers r)>0.2) (show (srAcrossLayers r))

publicationTests :: T -> IO ()
publicationTests t = do
  section "stress: immutable artifacts and atomic report publication"
  dir <- tempDir "stress-publication"
  let input = dir </> "part.coscad"
      report = dir </> "result.json"
      source = "main = block 2 2 2\nsupport bot\nload top 0 0 -1\nstress cell=1\n"
  writeFile input source
  first <- processStress Native silent False input (Just report)
  case first of
    Left err -> failT t "initial publication" (show err)
    Right () -> do
      oldReport <- readFile report
      parsed <- readJson report
      case parsed of
        Left err -> failT t "artifact report parses" err
        Right j -> do
          let get role field = lookupKey "artifacts" j >>= lookupKey role >>= lookupKey field >>= asString
              files = [dir </> path | role <- ["source","mesh","scad"], Just path <- [get role "path"]]
          assertT t "three referenced generation artifacts" (length files==3) ""
          forM_ ["source","mesh","scad"] $ \role ->
            assertT t (role ++ " SHA-256 provenance") (maybe False ((==64) . length) (get role "sha256")) ""
          oldContents <- mapM readFile files
          assertT t "source snapshot matches parser input" (take 1 oldContents==[source]) ""
          before <- sort <$> listDirectory dir
          writeFile input "main = block 3 2 2\nsupport bot\nstress cell=1\n"
          failure <- processStress Native silent False input (Just report)
          assertT t "analysis failure exits 1" (either ((==1) . failExit) (const False) failure) ""
          stillReport <- readFile report
          stillContents <- mapM readFile files
          after <- sort <$> listDirectory dir
          assertT t "failed analysis preserves report and every artifact" (stillReport==oldReport && stillContents==oldContents) ""
          assertT t "failed analysis removes unpublished generation" (before==after) (show (before,after))
          writeFile input ("// next generation\n" ++ source)
          second <- processStress Native silent False input (Just report)
          assertT t "replacement publishes successfully" (either (const False) (const True) second) ""
          newReport <- readFile report
          retained <- mapM readFile files
          assertT t "replacement leaves old generation immutable" (newReport/=oldReport && retained==oldContents) ""
          -- A publication error must also clean its generation, without
          -- touching the already published report or any older artifacts.
          let blocked = dir </> "blocked.json"
          createDirectory blocked
          beforeFailure <- sort <$> listDirectory dir
          ioFailure <- try (processStress Native silent False input (Just blocked)) :: IO (Either IOException (Either Failure ()))
          afterFailure <- sort <$> listDirectory dir
          assertT t "rename error cleans unpublished artifacts" (either (const True) (const False) ioFailure && beforeFailure==afterFailure) ""
          saved <- readFile input
          overwrite <- processStress Native silent False input (Just input)
          untouched <- readFile input
          assertT t "source cannot be report destination" (either ((==1) . failExit) (const False) overwrite && saved==untouched) ""
