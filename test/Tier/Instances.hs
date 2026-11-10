module Tier.Instances (instancesTier) where

import Harness
import Coscad.Assembly.Instances
import Coscad.Instance
import Coscad.Geometry (matIdentity, vlen, vsub, bbox)
import Coscad.Shape (Shape(..))
import Coscad.Json
import Coscad.Report (silent, Failure(..))
import Coscad.Stress.Assembly
import Coscad.Stress.Bonded
import Coscad.Stress.Voxel
import qualified Data.Map.Strict as Map
import Data.Either (isLeft)
import Data.List (sortOn, isInfixOf)
import Control.Monad (forM_)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

set :: String -> JValue -> JValue -> JValue
set k v (JObject xs) = JObject ((k,v):filter ((/=k).fst) xs)
set _ _ x = x

instancesTier :: T -> IO ()
instancesTier t = do
  section "assembly instances: recursive identity, transforms and bonded bridge"
  dir <- tempDir "instances"
  let part=dir </> "part.coscad"; child=dir </> "child.assemble"; root=dir </> "root.assemble"
      load text = writeFile root text >> loadInstances silent root
      near a b = vlen (vsub a b)<1e-8
  writeFile part "main = block 2 4 6\n"
  writeFile child "p ← part.coscad ×7 ▽bot\nasm = p |> move 2 0 0\n"
  nested <- load "sub ← child.assemble ×0\nunused ← part.coscad ×1\nasm = (sub |> rotz 90 |> move 10 20 30) ⊕ (sub |> move 40 0 0)\n"
  case nested of
    Left e -> failT t "nested export loads" (show e)
    Right snap -> do
      let rs=isInstances snap
      assertT t "qualified repeated IDs and no count expansion" (map riId rs == ["sub#1/p","sub#2/p"]) (show (map riId rs))
      assertT t "compose rotation before parent translation" (and (zipWith near (map (\r -> affApply (riTransform r) (0,0,0)) rs) [(10,22,30),(42,0,0)])) "poses"
      assertT t "count zero ancestor remains physical assembly-only" (all riAssemblyOnly rs && all ((==7).riCount) rs) "counts"
      assertT t "one hash per dependency including repeated source" (length (isSources snap)==3 && all ((==64).length.snd) (isSources snap)) "hashes"
      assertT t "explicit print transforms supplied" (all (maybe False (\a -> near (affApply a (0,0,0)) (0,0,0)).riPrintTransform) rs) "print poses"
      assertT t "export JSON round trip" (decode (encode (instancesJson snap)) == Right (instancesJson snap)) "JSON"
  oriented <- load "p ← part.coscad ×1 ▽rt\nasm = p |> rotz 90\n"
  case oriented of
    Right snap | [r] <- isInstances snap -> do
      let bounds = fmap (\(m,v) -> bbox (Transform3 m v (riShape r))) (riPrintTransform r)
      assertT t "side-down print frame normalized" (maybe False (\(lo,hi) -> near lo (0,0,0) && near hi (6,4,2)) bounds) (show bounds)
      let normal = do
            JArray [row] <- lookupKey "instances" (instancesJson snap)
            lookupKey "print_layer_normal_assembly" row
      assertT t "layer normal includes print and assembly rotations" (case normal of
        Just (JArray [JNumber x,JNumber y,JNumber z]) -> near (x,y,z) (0,-1,0)
        _ -> False) (show normal)
    _ -> failT t "side-down export loads" "missing leaf"
  forM_ ["asm = p |> cut (box 1 1 1)\n","asm = p ⊕ box 1 1 1\n"] $ \body -> do
    bad <- load ("p ← part.coscad ×1\n"++body)
    assertT t "ambiguous assembly geometry rejected" (either (any (isInfixOf "physical identity") . failLines) (const False) bad) body
  writeFile part "main = box 2 4 6 |> frame foot bot bot bak |> frame head top top bak\n"
  mated <- load "a ← part.coscad ×1\nb ← part.coscad ×1\nfixed a 10 20 30 0 90 0\nmate b @foot a @head\n"
  case mated of
    Left e -> failT t "mated export loads" (show e)
    Right snap -> do
      let poses=Map.fromList [(riId r,affApply (riTransform r) (0,0,0)) | r<-isInstances snap]
      assertT t "constraint solver poses preserved" (maybe False (`near` (10,20,30)) (Map.lookup "a" poses) && maybe False (`near` (16,20,30)) (Map.lookup "b" poses)) (show poses)
      assertT t "unknown print orientation stays unknown" (all ((==Nothing).riPrintTransform) (isInstances snap)) "orientation"
  let fixture="experiments/assembly-fea/shelf.assemble"
  loaded <- loadInstances silent fixture
  case loaded of
    Left e -> failT t "shelf assembly loads" (show e)
    Right snap -> do
      forM_ [("shelf-analysis.json","shelf-symmetric.json"),("shelf-soft-right-analysis.json","shelf-soft-right.json")] $ \(cfg,ref) -> do
        text <- readFile ("experiments/assembly-fea/"++cfg)
        reference <- readFile ("experiments/bonded/"++ref)
        case (decode text,parseBondedProblem reference) of
          (Right config,Right expected) -> do
            built <- assemblyProblem snap config
            case built of
              Left e -> failT t "assembly FEA builds" e
              Right actual -> do
                let bodies = sortOn bodyName . bpBodies
                    normalize b = b {bodyGrid=(bodyGrid b) {vgFill=Map.empty}}
                    patches = sortOn patchBody . map (\p -> p {patchFaces=sortOn show (patchFaces p)})
                assertT t (cfg++": exact reference mesh/materials") (map normalize (bodies actual)==map normalize (bodies expected)) "body grids"
                assertT t "reference supports and loads" (patches (bpSupports actual)==patches (bpSupports expected) && bpLoads actual==bpLoads expected) "regions"
                assertT t "reference bond regions" (map (\b -> (bondA b,bondB b)) (bpBonds actual)==map (\b -> (bondA b,bondB b)) (bpBonds expected)) "ties"
                case (solveBonded actual, solveBonded expected) of
                  (Right a,Right b) -> do
                    assertT t "reference energy agrees" (abs (baEnergy a/baEnergy b-1)<1e-7) (show (baEnergy a,baEnergy b))
                    let nodes r=Map.fromList [((brName body,k),u) | body<-baBodies r,(k,_,u)<-brNodes body]
                    assertT t "all reference nodal displacements agree" (Map.keysSet(nodes a)==Map.keysSet(nodes b) && all (<1e-8) (Map.elems (Map.intersectionWith (\u v -> vlen(vsub u v)) (nodes a) (nodes b)))) "displacements"
                  (a,b) -> failT t "reference solves" (show (fmap baEnergy a,fmap baEnergy b))
            forM_ [("no inferred support",set "supports" (JArray []) config),("no inferred bonds",set "bonds" (JArray []) config)] $ \(label,cfgBad) -> do
              p <- assemblyProblem snap cfgBad
              assertT t label (isLeft (p >>= solveBonded)) "fixed is placement only"
            forM_ [("missing materials",set "bodies" (JArray []) config),("unaligned mesh",set "cell_mm" (JNumber 3) config),("grid budget",set "cell_mm" (JNumber 0.1) config)
              ,("unknown field",set "typo" JNull config)
              ,("empty rectangle",set "supports" (JArray [JObject [("instance",JString "left"),("axis",JString "y"),("side",JNumber (-1)),("plane_mm",JNumber (-2)),("bounds_mm",JArray [JArray (map JNumber [0,-2,0]),JArray (map JNumber [4,-2,2])])]]) config)] $ \(label,cfgBad) -> do
              bad <- assemblyProblem snap cfgBad
              assertT t label (isLeft bad) "rejected"
            let scaled=snap {isInstances=map (\r -> r {riTransform=(((2,0,0),(0,1,0),(0,0,1)),(0,0,0))}) (isInstances snap)}
            badScale <- assemblyProblem scaled config
            assertT t "scaled placements rejected by FEA" (isLeft badScale && not (rigid (((2,0,0),(0,1,0),(0,0,1)),(0,0,0)))) "scale"
          _ -> failT t "benchmark JSON parses" "parse error"
  assertT t "identity transform rigid" (rigid (matIdentity,(0,0,0))) "identity"
  let output=dir </> "report.json"
      config="experiments/assembly-fea/shelf-analysis.json"
  (ok,_,err) <- readProcessWithExitCode "coscad" ["stress",fixture,"--bonded",config,"-o",output] ""
  assertT t "assembly stress CLI writes report" (ok==ExitSuccess) err
  before <- readFile output
  length before `seq` pure ()
  writeFile (dir </> "bad.json") "{}"
  (bad,_,_) <- readProcessWithExitCode "coscad" ["stress",fixture,"--bonded",dir </> "bad.json","-o",output] ""
  after <- readFile output
  assertT t "failed run preserves previous report" (bad/=ExitSuccess && before==after) "atomic output"
  original <- readFile part
  length original `seq` pure ()
  (blocked,_,_) <- readProcessWithExitCode "coscad" ["instances",root,"-o",part] ""
  preserved <- readFile part
  assertT t "export cannot overwrite referenced input" (blocked/=ExitSuccess && preserved==original) "input protection"
