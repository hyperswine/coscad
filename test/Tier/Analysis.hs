module Tier.Analysis (analysisTier) where
import Harness
import Coscad.Flight
import Coscad.Gear
import Coscad.Hydro
import Coscad.Json
import System.Directory (makeAbsolute)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

analysisTier :: T -> IO ()
analysisTier t = do
  section "float: hydrostatics against closed-form boxes"
  let cube = [((0,0,0),(0,1,0),(1,1,0)),((0,0,0),(1,1,0),(1,0,0)),((0,0,1),(1,0,1),(1,1,1)),((0,0,1),(1,1,1),(0,1,1))
             ,((0,0,0),(1,0,0),(1,0,1)),((0,0,0),(1,0,1),(0,0,1)),((0,1,0),(0,1,1),(1,1,1)),((0,1,0),(1,1,1),(1,1,0))
             ,((0,0,0),(0,0,1),(0,1,1)),((0,0,0),(0,1,1),(0,1,0)),((1,0,0),(1,1,0),(1,1,1)),((1,0,0),(1,1,1),(1,0,1))]
      close a b = abs (a-b) < 1e-9
  case submerged 0.25 cube of
    (v, Just (x,y,z)) -> assertT t "clipped cube volume and centre" (close v 0.25 && close x 0.5 && close y 0.5 && close z 0.125) (show (v,x,y,z))
    other -> failT t "clipped cube volume and centre" (show other)
  assertT t "nothing below the bottom" (fst (submerged (-1) cube) == 0) "volume"
  dir <- tempDir "analysis"
  let boxFile = dir </> "box.coscad"
      voidFile = dir </> "void.coscad"
      spec name extra = do
        let f = dir </> (name ++ ".json")
        writeFile f (encode (JObject (("schema",JString "coscad.float/1") : extra)))
        pure f
  writeFile boxFile "main = box 40 20 10 |> move 0 0 5\n"
  writeFile voidFile "main = (box 40 20 10 |> move 0 0 5) ⊖ (box 30 10 6 |> move 0 0 5)\n"
  half <- spec "half" [("body",JString boxFile),("density_g_cm3",JNumber 0.5),("heel_deg",JArray (map JNumber [0,10]))]
  result <- processFloat half
  let num path v = foldl (\acc k -> acc >>= lookupKey k) (Just v) path >>= asNumber
      heel10 v = case lookupKey "righting_curve" v >>= asArray of
        Just [_, r] -> lookupKey "righting_arm_mm" r >>= asNumber
        _ -> Nothing
      -- wall-sided box: T = 5, KB = 2.5, KG = 5, BM = W^2/(12 T)
      bm = 400 / 60; gm = 2.5 + bm - 5; phi = 10 * pi / 180
      gz = sin phi * (gm + bm / 2 * tan phi ^ (2::Int))
  case result of
    Left e -> failT t "half-density box" (show e)
    Right v -> do
      assertT t "half-density box drafts half its height" (maybe False (\d -> abs (d-5) < 1e-6) (num ["upright","draft_mm"] v)) (show v)
      assertT t "wall-sided righting arm at 10 degrees" (maybe False (\g -> abs (g-gz) < 1e-6) (heel10 v)) (show (heel10 v, gz))
      assertT t "symmetric box needs no trim" (maybe False (\a -> abs a < 1e-6) (num ["upright","static_trim_deg_bow_up"] v)) (show v)
  sealed <- processFloat voidFile
  case sealed of
    Left e -> failT t "sealed void floats the box" (show e)
    Right v -> assertT t "sealed void floats the box"
      (maybe False (\d -> abs (d - 6200*1.24/800) < 1e-6) (num ["upright","draft_mm"] v)) (show (num ["upright","draft_mm"] v))
  flooded <- spec "flooded" [("body",JString voidFile),("displacer",JString voidFile),("heel_deg",JArray [JNumber 0])]
  sinks <- processFloat flooded
  assertT t "plastic alone sinks" (either (const False) (\v -> (lookupKey "upright" v >>= lookupKey "floats") == Just (JBool False)) sinks) (show sinks)
  badKey <- spec "bad" [("body",JString boxFile),("densty",JNumber 1)]
  rejected <- processFloat badKey
  assertT t "unknown spec key rejected" (either (const True) (const False) rejected) (show rejected)

  section "gears: SG90 rack and pinion sweep"
  pinion <- makeAbsolute ("test" </> "fixtures" </> "gears" </> "pinion.coscad")
  rack <- makeAbsolute ("test" </> "fixtures" </> "gears" </> "rack.coscad")
  let gearSpecFile name sign = do
        let f = dir </> (name ++ ".json")
        writeFile f (encode (JObject
          [ ("schema",JString "coscad.gear-mesh/1")
          , ("driver",JObject [("part",JString pinion),("rotate",JObject [("axis",JArray (map JNumber [0,0,1]))])])
          , ("driven",JObject [("part",JString rack),("translate",JArray (map JNumber [sign,0,0]))])
          , ("teeth",JNumber 22),("driven_teeth",JNumber 0),("module_mm",JNumber 1.5),("face_mm",JNumber 4)
          , ("torque_n_mm",JNumber 176),("steps",JNumber 6) ]))
        pure f
  good <- gearSpecFile "mesh" (-1)
  report <- processGears good
  case report of
    Left e -> failT t "rack and pinion sweep" (show e)
    Right v -> do
      let at path = foldl (\acc k -> acc >>= lookupKey k) (Just v) path
          lewisDriver = at ["lewis","teeth"] >>= asArray >>= \rows -> case rows of
            r:_ -> lookupKey "root_stress_mpa" r >>= asNumber
            [] -> Nothing
      assertT t "meshes without interference" (at ["verdict"] == Just (JString "ok")) (show (at ["verdict"]))
      assertT t "backlash matches the designed 0.37 mm"
        (maybe False (\b -> b > 0.35 && b < 0.39) (at ["backlash","driver_pitch_mm","min"] >>= asNumber)) (show (at ["backlash"]))
      assertT t "at least one tooth pair always in contact"
        (maybe False (>= 1) (at ["contact_pairs","forward","min"] >>= asNumber)) (show (at ["contact_pairs"]))
      assertT t "Lewis root stress for 22 teeth"
        (maybe False (\s -> abs (s - (176/16.5) / (4*1.5*0.331)) < 1e-9) lewisDriver) (show lewisDriver)
  wrong <- gearSpecFile "reversed" 1
  (code,_,_) <- readProcessWithExitCode "coscad" ["gears",wrong,"-o",dir </> "reversed-report.json"] ""
  assertT t "reversed rack interferes and exits 2" (code == ExitFailure 2) (show code)
  assertT t "Lewis factor for a rack" (lewisFormFactor 0 == 0.485) "rack"

  section "flight: vortex lattice against wing theory"
  assertT t "LU solve with pivoting" (luSolve 2 [0,1,2,3] [[1,8]] == Right [[2.5,1]]) (show (luSolve 2 [0,1,2,3] [[1,8]]))
  let rectWing = JObject [("name",JString "wing"),("mirror",JBool True),("cl_max",JNumber 2),("sections",JArray
        [ JObject [("le",JArray (map JNumber [0,0,0])),("chord",JNumber 100)]
        , JObject [("le",JArray (map JNumber [0,300,0])),("chord",JNumber 100)] ])]
      tail' inc = JObject [("name",JString "stab"),("mirror",JBool True),("sections",JArray
        [ JObject [("le",JArray (map JNumber [300,0,0])),("chord",JNumber 60),("incidence_deg",JNumber inc)]
        , JObject [("le",JArray (map JNumber [300,100,0])),("chord",JNumber 60),("incidence_deg",JNumber inc)] ])]
      flightFile name cgX surfaces = do
        let f = dir </> (name ++ ".json")
        writeFile f (encode (JObject [("schema",JString "coscad.flight/1"),("mass_g",JNumber 150)
          ,("cg_mm",JArray (map JNumber [cgX,0,0])),("surfaces",JArray surfaces)]))
        pure f
      field path v = foldl (\acc k -> acc >>= lookupKey k) (Just v) path >>= asNumber
  rectFile <- flightFile "rect" 20 [rectWing]
  rectReport <- processFlight rectFile
  case rectReport of
    Left e -> failT t "rectangular AR 6 wing" (show e)
    Right v -> do
      -- Helmbold 4.53, lifting line about 4.3, vortex-lattice references about 4.2-4.3
      assertT t "AR 6 rectangular lift slope" (maybe False (\a -> a > 4.15 && a < 4.4) (field ["stability","cl_alpha_per_rad"] v)) (show (field ["stability","cl_alpha_per_rad"] v))
      assertT t "rectangular wing neutral point near quarter chord"
        (maybe False (\x -> x > 0.22 && x < 0.26) (field ["stability","neutral_point_fraction_mac"] v)) (show (field ["stability","neutral_point_fraction_mac"] v))
      assertT t "MAC of a rectangle is its chord" (maybe False (\c -> abs (c - 100) < 1e-9) (field ["reference","mac_mm"] v)) "mac"
  stable <- flightFile "stable" 30 [rectWing, tail' (-3)]
  stableReport <- processFlight stable
  case stableReport of
    Left e -> failT t "wing and tail trim" (show e)
    Right v -> do
      assertT t "wing and tail trim with positive lift" ((lookupKey "verdict" v >>= asString) == Just "ok") (show (lookupKey "verdict" v, lookupKey "trim" v))
      -- tail volume 0.55: NP about 25 + 0.55 x 100 x (a_t/a_w)(1 - de/da), near 48 mm
      assertT t "tail moves the neutral point aft of the wing's"
        (maybe False (\x -> x > 40 && x < 58) (field ["stability","neutral_point_x_mm"] v)) (show (field ["stability","neutral_point_x_mm"] v))
  tailHeavy <- flightFile "tail-heavy" 200 [rectWing, tail' (-3)]
  (code,_,_) <- readProcessWithExitCode "coscad" ["flight",tailHeavy,"-o",dir </> "tail-heavy-report.json"] ""
  assertT t "CG behind the neutral point is unstable and exits 2" (code == ExitFailure 2) (show code)
  heavyText <- readFile (dir </> "tail-heavy-report.json")
  assertT t "non-finite results are written as null" (either (const False) (const True) (decode heavyText)) (take 200 heavyText)
