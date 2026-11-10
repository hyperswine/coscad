module Tier.Mass (massTier) where
import Harness
import Coscad.Backend (nativeMesh)
import Coscad.Parser (parseProgram)
import Coscad.Mesh
import Coscad.Mass
import Coscad.Mass.Command
import Coscad.Json
import Coscad.Linalg (vadd, vsub, vlen)
import Data.Either (isLeft)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))

massTier :: T -> IO ()
massTier t = do
  section "mass: mesh moments and explicit components"
  case parseProgram "main = box 2 4 6" >>= nativeMesh . snd of
    Left e -> failT t "box mesh" e
    Right tris -> do
      let check label mesh expectedV expectedC = do
            let (v,c) = meshVolumeCentre mesh
            assertT t label (abs (v-expectedV)<1e-8 && maybe False (\q -> vlen (vsub q expectedC)<1e-8) c) (show (v,c))
      check "box volume and centre" tris 48 (0,0,0)
      check "translated moments near 1e9" (meshMap (vadd (1e9,-1e9,1e9)) tris) 48 (1e9,-1e9,1e9)
      check "reversed winding" [(c,b,a)|(a,b,c)<-tris] 48 (0,0,0)
      check "disconnected solids weighted by volume" (tris ++ meshMap (vadd (10,0,0)) tris) 96 (5,0,0)
  assertT t "empty mesh has no centre" (meshVolumeCentre [] == (0,Nothing)) "empty"
  let body = Component "body" "sliced" 100 (0,0,0)
      parts = [body, Component "bolt" "hardware" 10 (110,0,0)]
  assertT t "hardware contributes moment" (combineMass parts == Right (110,(10,0,0))) (show (combineMass parts))
  mapM_ (\(name,cs) -> assertT t name (isLeft (combineMass cs)) (show cs))
    [("no components",[]),("duplicate name",[body,body]),("negative mass",[Component "bad" "hardware" (-1) (0,0,0)])
    ,("unknown kind",[Component "bad" "infill" 1 (0,0,0)]),("nonfinite position",[Component "bad" "hardware" 1 (0/0,0,0)])]
  assertT t "density unit/argument parsing" (massArgs ["a.coscad","--density","1.24","-o","out.json"] == Right ("a.coscad",Just 1.24,Just "out.json")) "args"
  mapM_ (\args -> assertT t ("reject mass args "++show args) (isLeft (massArgs args)) "args")
    [["a.coscad","--density","nan"],["a.coscad","--density","0"],["a.coscad","--density","1","--density","2"]]
  dir <- tempDir "mass"
  let source = dir </> "box.coscad"
  writeFile source "main = box 10 10 10\n"
  report <- processMass source (Just 1.24)
  case report of
    Left e -> failT t "solid report" (show e)
    Right value -> do
      assertT t "density in g per cm3" (maybe False (\v -> abs (v-1.24)<1e-10) (lookupKey "mass_g" value >>= asNumber)) (show value)
      assertT t "source SHA256 present" (maybe False ((==64).length) (lookupKey "source_sha256" value >>= asString)) (show value)
  noDensity <- processMass source Nothing
  assertT t "no implicit density" (either (const False) ((==Just JNull).lookupKey "mass_g") noDensity) (show noDensity)
  let manifest = JObject [("schema",JString "coscad.mass-components/1"),("components",JArray
        [JObject [("name",JString "bolt"),("kind",JString "hardware"),("mass_g",JNumber 2),("center_mm",JArray(map JNumber [4,5,6]))]])]
  assertT t "manifest round trip" (componentManifest manifest == Right [Component "bolt" "hardware" 2 (4,5,6)]) (show manifest)

  let output = dir </> "report.json"
  (ok,_,err) <- readProcessWithExitCode "coscad" ["mass",source,"--density","1.24","-o",output] ""
  assertT t "mass CLI writes JSON" (ok == ExitSuccess) err
  before <- readFile output
  length before `seq` pure ()
  (badDensity,_,_) <- readProcessWithExitCode "coscad" ["mass",source,"--density","-1","-o",output] ""
  after <- readFile output
  assertT t "invalid run preserves previous report" (badDensity /= ExitSuccess && before == after) after
  (same,_,_) <- readProcessWithExitCode "coscad" ["mass",source,"-o",source] ""
  preserved <- readFile source
  assertT t "input cannot be overwritten" (same /= ExitSuccess && preserved == "main = box 10 10 10\n") preserved
