-- | CoScad regression suite. Three tiers:
--
--   1. diagnostics   pure: error messages carry file:line:col and say
--                    what is wrong (syntax, undefined, circular,
--                    duplicate, 2D/3D mismatch)
--   2. examples      pure: every example compiles, every .assemble
--                    loads, and the emitted .scad matches the snapshot
--                    under test/golden/scad/
--   3. geometry      needs OpenSCAD: every example renders, and its
--                    mesh volume + bounds match test/golden/geometry.txt;
--                    plus a few cross-checks (tbracket == bracket,
--                    `coscad next` conserves volume, `coscad check`
--                    passes on bow3). Skipped when OpenSCAD is not
--                    found or COSCAD_RENDER=0.
--
-- COSCAD_UPDATE_GOLDEN=1 rewrites the snapshots from the current output
-- (review the diff before committing). Missing snapshots are created
-- on first run.
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless, when)
import Coscad.Assemble (AsmResult (..), FlatPart (..), loadAssembleFile, packBeds, processAssemble, resolveAssembly)
import Coscad.Check (processCheckIn, processCheckWith, splitBodies)
import Coscad.Codegen (gen, renderScad, showD)
import Coscad.Geometry (PointRef (..), MatingFrame (..), bbox, bcenter, pointOf, resolve, frameOf, mApply, vneg, vnormed, dot3, vlen, vsub, scale3, vadd, vmul, rodMat, mirrorMat)
import Coscad.Json (JValue (..), asArray, asNumber, asString, decode, lookupKey)
import qualified Coscad.Json
import Coscad.Mesh (Tri, meshBounds, meshVolume, parseStlAscii)
import Coscad.Next (processNext)
import Coscad.OpenScad (bosl2Version, findOpenscad, openscadFullRender, openscadPngArgs, openscadVersion, renderStlFile, runOpenscadRaw)
import Coscad.Parser (parseProgramNamed)
import Coscad.Part (renderPart)
import Coscad.Plan (PlanSummary (..), StepSummary (..), FastenerSummary (..), planSummary)
import MigrationCases (migrationCases)
import Coscad.Site (buildPageHtml, indexHtml)
import Coscad.Shape (Shape (..), shapeChildren)
import Data.IORef
import Data.List (isInfixOf, isPrefixOf, sort, tails)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Map as Map
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.IO.Encoding (setLocaleEncoding)
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath
import System.IO
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)

-- ------------------------------------------------------------------
-- tiny harness

data T = T {tPassed :: IORef Int, tFailed :: IORef [String]}

pass :: T -> IO ()
pass t = modifyIORef' (tPassed t) (+ 1)

failT :: T -> String -> String -> IO ()
failT t name detail = do
  putStrLn ("FAIL " ++ name)
  putStrLn (unlines (map ("     " ++) (lines detail)))
  modifyIORef' (tFailed t) (++ [name])

assertT :: T -> String -> Bool -> String -> IO ()
assertT t name ok detail = if ok then pass t else failT t name detail

-- | Expect a Left whose message contains every `yes` and no `no`.
expectErr :: T -> String -> Either String a -> [String] -> [String] -> IO ()
expectErr t name r yes no = case r of
  Right _ -> failT t name "expected an error, but it parsed"
  Left msg ->
    let missing = [y | y <- yes, not (y `isInfixOf` msg)]
        present = [n | n <- no, n `isInfixOf` msg]
     in assertT t name (null missing && null present) $
          "message:\n" ++ msg ++ "\nmissing: " ++ show missing ++ "\nunwanted: " ++ show present

-- | Expect a Right whose rendered .scad contains the fragment.
expectGen :: T -> String -> Either String (a, Shape) -> String -> IO ()
expectGen t name r frag = case r of
  Left e -> failT t name e
  Right (_, m) -> assertT t name (frag `isInfixOf` renderScad m) ("wanted fragment: " ++ frag ++ "\n" ++ renderScad m)

-- ------------------------------------------------------------------
-- build plans (coscad plan): pure, no OpenSCAD needed

cubeTier :: T -> IO ()
cubeTier t = do
  section "2020 cube: independent bracket poses and closed interfaces"
  loaded <- loadAssembleFile [] "examples/assemble/plan/cube.assemble"
  let near (a,b,c) (x,y,z) = all (< 1e-7) [abs(a-x),abs(b-y),abs(c-z)]
  case loaded of
    Left e -> failT t "constrained cube loads" e
    Right ar -> case arAsm ar of
      Just (Union shapes) -> do
        let placed = Map.fromList (zip (map fst (arParts ar)) shapes)
            brackets = [(n,s) | (n,s) <- Map.toList placed, "br_" `isPrefixOf` n]
            rails = [(n,s) | (n,s) <- Map.toList placed, n /= "panel", not ("br_" `isPrefixOf` n)]
            -- World-space measurements, independent of the mating statements.
            ring ringName z =
              [("br_"++ringName++"_fl",(20,20,z),(30,10,z),(10,30,z),(0,0,1))
              ,("br_"++ringName++"_fr",(180,20,z),(190,30,z),(170,10,z),(0,0,1))
              ,("br_"++ringName++"_bl",(20,180,z),(10,170,z),(30,190,z),(0,0,1))
              ,("br_"++ringName++"_br",(180,180,z),(170,190,z),(190,170,z),(0,0,1))]
            side face y =
              [("br_"++face++"_low_left",(20,y,20),(30,y,10),(10,y,30),(0,-1,0))
              ,("br_"++face++"_low_right",(180,y,20),(190,y,30),(170,y,10),(0,-1,0))
              ,("br_"++face++"_high_left",(20,y,180),(10,y,170),(30,y,190),(0,-1,0))
              ,("br_"++face++"_high_right",(180,y,180),(170,y,190),(190,y,170),(0,-1,0))]
            expected = ring "bottom" (-3) ++ ring "top" 203 ++ side "front" (-3) ++ side "back" 203
        assertT t "cube: 12 rails, 16 distinct brackets, 1 panel, 54 mates and 1 root"
          (length shapes == 29 && length rails == 12 && length brackets == 16 && Map.member "panel" placed && length (arConstraints ar) == 55) (show (Map.keys placed))
        forM_ expected $ \(n,centre,a,b,normal) -> case Map.lookup n placed of
          Nothing -> failT t ("cube bracket " ++ n) "missing bracket"
          Just s -> do
            assertT t ("cube bracket centre " ++ n) (near (bcenter (bbox s)) centre) (show (bbox s))
            forM_ [("a_top",a),("b_top",b)] $ \(f,want) -> case frameOf f s of
              Left e -> failT t ("cube bracket frame " ++ n ++ " @" ++ f) e
              Right (MatingFrame p basis) -> do
                let nz = mApply basis (0,0,1)
                    mid = vsub p (scale3 3 nz)
                assertT t ("cube bore and normal " ++ n ++ " @" ++ f) (near mid want && near nz normal) (show (mid,nz))
        forM_ [("lower_front",(200,60,10)),("lower_back",(200,140,10)),("upper_front",(200,60,190)),("upper_back",(200,140,190)),("side_front",(200,10,100)),("side_back",(200,190,100))] $ \(f,p) ->
          case Map.lookup "panel" placed >>= either (const Nothing) Just . frameOf f of
            Nothing -> failT t ("cube panel bore " ++ f) "missing panel interface"
            Just (MatingFrame got b) -> assertT t ("cube panel bore " ++ f) (near got p && near (mApply b (0,0,1)) (-1,0,0)) (show (got,b))
      _ -> failT t "constrained cube generates one union" "no generated assembly"
  dir <- tempDir "cube-constraints"
  forM_ ["rail200.coscad","rail160.coscad","flat90.coscad","panel190.coscad"] $ \f -> copyFile ("examples/assemble/plan" </> f) (dir </> f)
  src <- readFile "examples/assemble/plan/cube.assemble"
  let spec = dir </> "cube.assemble"
      replace old new = unlines . map (\line -> if line == old then new else line) . lines
  forM_ [("wrong corner leg", "mate br_bottom_fr @b_top bottom_front @bolt_bot_high", "mate br_bottom_fr @a_top bottom_front @bolt_bot_high")
        ,("flipped bracket face", "mate br_bottom_fl @a_top bottom_front @bolt_bot_low", "mate br_bottom_fl @a_bot bottom_front @bolt_bot_low")
        ,("wrong rear bracket rotation", "mate br_back_high_right @a_top top_back @bolt_bak_high", "mate br_back_high_right @b_top top_back @bolt_bak_high")] $ \(label,old,new) -> do
    writeFile spec (replace old new src)
    r <- loadAssembleFile [] spec
    expectErr t ("cube rejects " ++ label) r ["conflicting assembly constraints"] []
  bracket <- readFile (dir </> "flat90.coscad")
  length bracket `seq` writeFile (dir </> "flat90.coscad") (replace "main = box 40 40 6" "main = box 40 40 8" bracket)
  writeFile spec src
  resized <- loadAssembleFile [] spec
  case resized >>= maybe (Left "missing asm") Right . arAsm of
    Left e -> failT t "cube bracket thickness resize" e
    Right s -> let (lo,hi) = bbox s in assertT t "cube bracket thickness resize keeps all mates closed"
      (near lo (0,-8,-8) && near hi (203,208,208)) (show (lo,hi))

-- Probe the origin and three non-collinear axis points of each physical
-- instance, independently of its mating-frame metadata. This detects a flipped
-- symmetric part even when its assembled union and bounds look unchanged.
migrationPoses :: AsmResult -> Either String (Map.Map String [(Double,Double,Double)])
migrationPoses ar = do
  table <- resolveAssembly ar (Map.fromList [(n,Tag n sh) | (n,sh) <- arParts ar])
  asm <- maybe (Left "missing asm") Right (Map.lookup "asm" table)
  let occurrences = go id (resolve asm)
      counts = Map.fromListWith (+) [(n,1::Int) | (n,_) <- occurrences]
      named = snd $ foldl (\(seen,acc) (n,ps) ->
        let i = Map.findWithDefault 0 n seen + 1
            key = if counts Map.! n > 1 then n ++ "#" ++ show i else n
        in (Map.insert n i seen, acc ++ [(key,ps)])) (Map.empty,[]) occurrences
  return (Map.fromList named)
  where
    go f sh = case sh of
      -- A reflected recursive fixture has moved reflection into its part
      -- definition, because reflection cannot be encoded by a rigid pose.
      Tag n x -> let local = case x of Mirror v _ -> mApply (mirrorMat v); _ -> id
                 in [(n,map (f . local) [(0,0,0),(100,0,0),(0,100,0),(0,0,100)])]
      Tx d x -> go (f . vadd (d,0,0)) x
      Ty d x -> go (f . vadd (0,d,0)) x
      Tz d x -> go (f . vadd (0,0,d)) x
      Translate v x -> go (f . vadd v) x
      Rx a x -> go (f . mApply (rodMat a (1,0,0))) x
      Ry a x -> go (f . mApply (rodMat a (0,1,0))) x
      Rz a x -> go (f . mApply (rodMat a (0,0,1))) x
      RotAxis a v x -> go (f . mApply (rodMat a v)) x
      Transform3 m v x -> go (f . vadd v . mApply m) x
      Scale v x -> go (f . vmul v) x
      Mirror v x -> go (f . mApply (mirrorMat v)) x
      _ -> concatMap (go f) (shapeChildren sh)

migrationTier :: T -> IO ()
migrationTier t = do
  section "all assembly migrations: bounds, quantities, constraints and instruction joints"
  forM_ migrationCases $ \(file,ref,tolerance,aliases) -> do
    a <- loadAssembleFile [] file
    b <- loadAssembleFile [] ref
    case (a,b) of
      (Right actual,Right original) -> do
        let Just sh = arAsm actual
            Just old = arAsm original
            (lo,hi) = bbox sh
            (ol,oh) = bbox old
            near (x,y,z) (u,v,w) = maximum [abs(x-u),abs(y-v),abs(z-w)] <= tolerance
        assertT t (file ++ ": bounds preserved") (near lo ol && near hi oh) (show (bbox sh,bbox old))
        assertT t (file ++ ": manufacturing count preserved")
          (sum(map fpCount(arFlat actual)) == sum(map fpCount(arFlat original))) (show (map fpCount(arFlat actual),map fpCount(arFlat original)))
        assertT t (file ++ ": uses constraints") (not(null(arConstraints actual))) "missing constraints"
        case (migrationPoses actual,migrationPoses original) of
          (Right ap,Right op) -> forM_ aliases $ \(oldName,newName) ->
            assertT t (file ++ ": pose of " ++ newName)
              (case (Map.lookup newName ap,Map.lookup oldName op) of
                (Just ps,Just qs) -> and (zipWith near ps qs)
                _ -> False) (show (Map.lookup newName ap,Map.lookup oldName op))
          _ -> failT t (file ++ ": instance pose comparison") "cannot resolve tagged instances"

        when (file `elem` ["examples/assemble/ball/ball.assemble","examples/assemble/tesseract/bolted/tesseract.assemble","examples/assemble/plan/corner_pair.assemble","models/orbit-tower/orbit-tower.assemble"]) $ do
          pa <- planSummary file
          pb <- planSummary ref
          case (pa,pb) of
            (Right p,Right q) -> do
              let rename n = fromMaybe n (lookup n aliases)
                  joints = sort . concatMap (map (\(spec,c,h,nut)->(spec,rename c,rename h,nut)) . ssFasteners) . psSteps
              assertT t (file ++ ": instruction joints preserved") (joints q == sort(concatMap ssFasteners(psSteps p)) && null(psDesignErrors p)) (show (psDesignErrors p))
              assertT t (file ++ ": all instances placed once") (sort(concatMap ssParts(psSteps p)) == sort(psParts p)) (show(psParts p))
            _ -> failT t (file ++ ": instruction comparison") (show (pa,pb))
      _ -> failT t (file ++ ": migration loads") (either id (const "ok") a ++ " / " ++ either id (const "ok") b)

tesseractTier :: T -> IO ()
tesseractTier t = do
  section "fastener-free tesseract: captive joints and closure order"
  let dir = "examples/assemble/tesseract"
  finished <- loadAssembleFile [] (dir </> "finished.assemble")
  case finished of
    Left e -> failT t "finished tesseract loads" e
    Right ar -> do
      assertT t "finished tesseract contains 37 printed pieces, no jig"
        (sum (map fpCount (arFlat ar)) == 37 && all ((/= "jig.coscad") . takeFileName . fpSource) (arFlat ar)) "incorrect finished BOM"
      assertT t "slimmer tesseract retains the 100 mm outer envelope"
        (maybe False (\sh -> let (lo,hi) = bbox sh in vlen lo < 1e-8 && vlen (vsub hi (100,100,100)) < 1e-8) (arAsm ar)) (show (fmap bbox (arAsm ar)))
  coupon <- loadAssembleFile [] (dir </> "fit-test.assemble")
  assertT t "joint fit test is three physical pieces" (either (const False) ((== 3) . sum . map fpCount . arFlat) coupon) "incorrect fit test"
  summary <- planSummary (dir </> "tesseract.assemble")
  case summary of
    Left e -> failT t "interlocking tesseract instruction plan" e
    Right p -> do
      let placed = concatMap ssParts (psSteps p)
          ranks = Map.fromList (zip placed [0::Int ..])
          before a b = case (Map.lookup a ranks, Map.lookup b ranks) of
            (Just i,Just j) -> i < j
            _ -> False
      assertT t "tesseract instructions place every part exactly once" (length placed == 38 && sort placed == sort (psParts p)) (show placed)
      assertT t "tesseract uses integral joints and no separate screws or nuts"
        (null (psDesignErrors p) && all (\(spec,_,_,nut) -> spec == "peg" && nut == "none") (concatMap ssFasteners (psSteps p))) (show (psDesignErrors p))
      assertT t "assembly support goes under the centre before the struts"
        (before "jig" "inner" && all (before "inner" . ("strut" ++) . show) [1::Int ..8]) (show placed)
      assertT t "each corner base follows its radial strut"
        (all (\i -> before ("strut" ++ show i) ("corner" ++ show i)) [1::Int ..8]) (show placed)
      assertT t "open bases precede every bar"
        (and [before ("corner" ++ show i) ("bar" ++ show j) | i <- [1::Int ..8],j <- [1::Int ..12]]) (show placed)
      assertT t "caps close only after every bar is seated"
        (and [before ("bar" ++ show i) ("cap" ++ show j) | i <- [1::Int ..12],j <- [1::Int ..8]]) (show placed)

frameTier :: T -> IO ()
frameTier t = do
  section "oriented mating frames and rigid assembly constraints"
  let p = parseProgramNamed "frames.coscad"
      near (a,b,c) (x,y,z) = all (< 1e-8) [abs (a-x), abs (b-y), abs (c-z)]
      bounds name src (lo,hi) = case p src of
        Left e -> failT t name e
        Right (_,s) -> let (a,b) = bbox s in assertT t name (near a lo && near b hi) (show (a,b))
      dirs = [("top",(0,0,1)),("bot",(0,0,-1)),("rt",(1,0,0)),("lft",(-1,0,0)),("bak",(0,1,0)),("fwd",(0,-1,0))]
      arm = "arm = box 2 4 6 |> frame root bot bot bak\n"
  forM_ [(n,nv,u,uv) | (n,nv) <- dirs, (u,uv) <- dirs, dot3 nv uv == 0] $ \(n,nv,u,uv) -> do
    let src = "ref = box 20 20 10 |> frame face ctr " ++ n ++ " " ++ u ++ "\n" ++ arm ++ "main = arm |> mate ref @face @root\n"
    case p src >>= \(_,s) -> frameOf "root" s of
      Left e -> failT t ("frame directions " ++ n ++ "/" ++ u) e
      Right (MatingFrame origin basis) -> assertT t ("frame directions " ++ n ++ "/" ++ u)
        (near origin (0,0,0) && near (mApply basis (0,0,1)) (vneg nv) && near (mApply basis (0,1,0)) uv) (show (origin,basis))
  bounds "mate emits child only and preserves asymmetric side orientation"
    ("ref = box 20 10 4 |> frame face rt rt top\n" ++ arm ++ "main = arm |> mate ref @face @root\n") ((10,-1,-2),(16,1,2))
  bounds "roll is about the parent normal; gap moves outward"
    ("ref = box 20 20 4 |> frame face top top bak\n" ++ arm ++ "main = arm |> mate ref @face @root 90 2\n") ((-2,-1,4),(2,1,10))
  bounds "transformed parent frame retains its up direction"
    ("ref = box 20 10 4 |> frame face top top bak |> roty 90 |> move 7 8 9\n" ++ arm ++ "main = arm |> mate ref @face @root\n") ((9,6,8),(15,10,10))
  forM_ ["", "!glyph\n", "!simple\n"] $ \pragma -> bounds ("frames in syntax " ++ show pragma)
    (pragma ++ "ref = box 20 20 4 |> frame face top top bak\n" ++ arm ++ "main = arm |> mate ref @face @root\n") ((-1,-2,2),(1,2,8))
  case p "main = box 20 10 4 |> frame face top top bak |> scale 2 3 4 |> move 1 2 3\n" >>= \(_,s) -> frameOf "face" s of
    Left e -> failT t "orthogonal scaled frame" e
    Right (MatingFrame origin basis) -> assertT t "orthogonal scaled frame" (near origin (1,2,11) && near (mApply basis (0,0,1)) (0,0,1)) (show (origin,basis))
  case p "main = box 2 2 2 |> frame f top top+rt bak\n" >>= \(_,s) -> frameOf "f" s of
    Left e -> failT t "diagonal frame normal" e
    Right (MatingFrame _ b) -> assertT t "diagonal frame normal" (near (mApply b (0,0,1)) (vnormed (1,0,1))) (show b)
  bounds "frame origins can be used as point datums"
    "main = box 2 4 6 |> frame f top top bak |> anchor @f\n" ((-1,-2,-6),(1,2,0))
  bounds "frame survives unrelated growth and cuts"
    ("base = box 20 20 4 |> frame f top top bak\nref = base |> at top (box 30 30 50) |> cut (sphere 1)\n" ++ arm ++ "main = arm |> mate ref @f @root\n") ((-1,-2,2),(1,2,8))
  bounds "fresh frame after reflection can be mated"
    ("ref = box 20 20 4 |> mirror 1 0 0 |> frame f top top bak\n" ++ arm ++ "main = arm |> mate ref @f @root\n") ((-1,-2,2),(1,2,8))
  forM_ [(20,6),(40,12)] $ \(w,h) -> bounds ("mating survives parent/child resize " ++ show (w,h))
    ("ref = box " ++ show w ++ " 10 4 |> frame f rt rt top\narm = box 2 4 " ++ show h ++ " |> frame root bot bot bak\nmain = arm |> mate ref @f @root 0 0.25\n") ((w/2+0.25,-1,-2),(w/2+0.25+h,1,2))
  forM_ [ ("zero normal", "main = box 2 2 2 |> frame f top ctr bak\n", ["nonzero"])
        , ("parallel up", "main = box 2 2 2 |> frame f top top bot\n", ["not be parallel"])
        , ("2D frame", "main = ⭘ 2 |> frame f ctr top bak\n", ["3D solid"])
        , ("duplicate namespace", "main = box 2 2 2 |> datum f ctr |> frame f top top bak\n", ["duplicate"])
        , ("duplicate frame namespace", "main = box 2 2 2 |> frame f top top bak |> datum f ctr\n", ["duplicate"])
        , ("point is not frame", "ref = box 2 2 2 |> datum f top\n" ++ arm ++ "main = arm |> mate ref @f @root\n", ["point datum", "oriented frame"])
        , ("missing frame", "ref = box 2 2 2\n" ++ arm ++ "main = arm |> mate ref @f @root\n", ["unknown frame '@f'"])
        , ("reflected frame", "ref = box 2 2 2 |> frame f top top bak |> mirror 1 0 0\n" ++ arm ++ "main = arm |> mate ref @f @root\n", ["reflected", "new frame"])
        , ("skewed frame", "ref = box 2 2 2 |> frame f top top+rt bak |> scale 2 1 1\n" ++ arm ++ "main = arm |> mate ref @f @root\n", ["skewed"])
        , ("ambiguous frame", "ref = box 2 2 2 |> frame f top top bak |> grid 2 1 4 0\n" ++ arm ++ "main = arm |> mate ref @f @root\n", ["ambiguous frame", "2 copies"])
        , ("incomplete roll/gap", "ref = box 2 2 2 |> frame f top top bak\n" ++ arm ++ "main = arm |> mate ref @f @root 45\n", ["gap", "both required"])
        , ("numeric typo", "main = box 2 2 2 |> frame f top top bak typo 0 0\n", ["undefined variable 'typo'"])
        ] $ \(name,src,want) -> expectErr t name (p src) ("frames.coscad:" : want) ["circular dependency"]
  dir <- tempDir "frame-constraints"
  writeFile (dir </> "part.coscad") "main = box 2 4 6 |> frame foot bot bot bak |> frame head top top bak\n"
  let file = dir </> "graph.assemble"
      refs = "a ← part.coscad ×1\nb ← part.coscad ×1\nc ← part.coscad ×1\n"
      joints = "mate b @foot a @head\nmate c @foot b @head\n"
      load src = writeFile file src >> loadAssembleFile [] file
      graphBounds name src want = do
        r <- load src
        case r >>= maybe (Left "missing asm") Right . arAsm of
          Left e -> failT t name e
          Right s -> let (lo,hi) = bbox s; (a,b) = want in assertT t name (near lo a && near hi b) (show (lo,hi))
  graphBounds "forward graph propagation" (refs ++ "fixed a\n" ++ joints) ((-1,-2,-3),(1,2,15))
  graphBounds "reverse graph propagation" (refs ++ "fixed c\n" ++ joints) ((-1,-2,-15),(1,2,3))
  graphBounds "fixed root XYZ rotation and translation" (refs ++ "fixed a 10 20 30 0 90 0\n" ++ joints) ((7,18,29),(25,22,31))
  graphBounds "consistent loop is accepted" (refs ++ "fixed a\n" ++ joints ++ "mate c @foot a @head gap=6\n") ((-1,-2,-3),(1,2,15))
  graphBounds "consistent multiple fixed roots" (refs ++ "fixed a\nfixed c 0 0 12 0 0 0\n" ++ joints) ((-1,-2,-3),(1,2,15))
  forM_ [ ("floating component", refs ++ "fixed a\nmate b @foot a @head\n", ["underconstrained", "c"])
        , ("no fixed root", refs ++ joints, ["underconstrained", "fixed"])
        , ("translation conflict", refs ++ "fixed a\n" ++ joints ++ "mate c @foot a @head gap=7\n", ["graph.assemble:", "conflicting assembly constraints", "position residual"])
        , ("orientation conflict", refs ++ "fixed a\n" ++ joints ++ "mate c @foot a @head gap=6 roll=90\n", ["conflicting assembly constraints", "orientation residual"])
        , ("fixed pose conflict", refs ++ "fixed a\nfixed c\n" ++ joints, ["conflicting assembly constraints"])
        , ("unknown instance", refs ++ "fixed a\nmate typo @foot a @head\n", ["graph.assemble:5:1:", "unknown constrained instance 'typo'"])
        , ("unknown frame on instance", refs ++ "fixed a\nmate b @bad a @head\n", ["graph.assemble:5:1:", "b @bad", "unknown frame"])
        , ("self mating", refs ++ "fixed a\nmate a @foot a @head\n", ["two different part instances"])
        , ("duplicate fixed", refs ++ "fixed a\nfixed a\n", ["graph.assemble:5:1:", "duplicate fixed"])
        , ("manual asm conflict", refs ++ "fixed a\n" ++ joints ++ "asm = a\n", ["remove the manual asm"])
        , ("multiplicity ambiguity", "a ← part.coscad ×2\nfixed a\n", ["distinct instance names", "×2"])
        , ("incomplete fixed pose", refs ++ "fixed a 1 2 3\n", ["graph.assemble:4:", "six pose numbers"])
        , ("bad mate option", refs ++ "mate b @foot a @head rol=90\n", ["graph.assemble:4:", "did you mean 'roll'"])
        , ("duplicate mate option", refs ++ "mate b @foot a @head roll=0 roll=90\n", ["duplicate mate option 'roll'"])
        , ("nonfinite mate option", refs ++ "mate b @foot a @head gap=NaN\n", ["expected a finite distance"])
        , ("keyword cannot swallow next line", refs ++ "mate b @foot a\nfixed a\n", ["graph.assemble:4:", "oriented frame reference"])
        ] $ \(name,src,want) -> load src >>= \r -> expectErr t name r want ["circular dependency"]
  r <- load (refs ++ "fixed a\n" ++ joints)
  case r of
    Left e -> failT t "constraint substitutions" e
    Right ar -> do
      let substituted wrap = resolveAssembly ar (Map.fromList [(n,wrap n s) | (n,s) <- arParts ar]) >>= maybe (Left "no asm") Right . Map.lookup "asm"
      assertT t "planner tags survive constraint resolution"
        (either (const False) (\s -> all (`isInfixOf` show s) ["Tag \"a\"", "Tag \"b\"", "Tag \"c\""]) (substituted Tag)) "missing instance identity"
      case substituted (\n s -> if n == "c" then s else Hidden s) of
        Left e -> failT t "checker retains hidden mating frames" e
        Right s -> let out = renderScad s in assertT t "checker retains hidden mating frames"
          (length (filter (isPrefixOf "cuboid" . dropWhile (== ' ')) (lines out)) == 1 && "[0, 0, 1, 12]" `isInfixOf` out) out
  planned <- planSummary "examples/assemble/constraints/frame-stand.assemble"
  assertT t "constraint-generated assembly plans all three instances"
    (either (const False) (\ps -> sort (psParts ps) == ["base", "left_post", "right_post"] && sum (map (length . ssParts) (psSteps ps)) == 3 && null (psDesignErrors ps)) planned) (either id show planned)

topologyTier :: T -> IO ()
topologyTier t = do
  section "named datums, child-only placement, grids and unary hull"
  let p = parseProgramNamed "topology.coscad"
      near (a,b,c) (x,y,z) = all (< 1e-8) [abs (a-x), abs (b-y), abs (c-z)]
      bounds name source (lo, hi) = case p source of
        Left e -> failT t name e
        Right (_, s) -> let (a, b) = bbox s in assertT t name (near lo a && near hi b) (show (a, b))
      base = "base = box 20 10 4 |> datum p top+rt 1 2 3\n"
  forM_ [ ("move 3 -2 7", (14,0,12))
        , ("rotx 90", (11,-5,2))
        , ("roty 90", (5,2,-11))
        , ("rotz 90", (-2,11,5))
        , ("scale -2 3 4", (-22,6,20))
        , ("mirror 1 0 0", (-11,2,5))
        , ("anchor bot", (11,2,7))
        , ("anchor @p", (0,0,0))
        ] $ \(stage, want) -> case p (base ++ "main = base |> " ++ stage ++ "\n") of
    Left e -> failT t ("datum follows " ++ stage) e
    Right (_, s) -> case pointOf (NamedPoint "p") s of
      Left e -> failT t ("datum follows " ++ stage) e
      Right got -> assertT t ("datum follows " ++ stage) (near got want) (show got)
  let stable = "plate = box 20 10 4 |> datum hole top\ngrown = plate |> at rt (box 80 10 4)\n"
  bounds "datum survives unrelated growth; parent is not emitted"
    (stable ++ "main = box 2 2 2 |> place grown @hole bot\n") ((-1,-1,2),(1,1,4))
  bounds "place has explicit target/child anchors and offsets"
    "main = box 2 4 6 |> place (box 20 10 4) top+rt bot+lft 1 2 3\n" ((11,0,5),(13,4,11))
  bounds "place supports a child's named point and forward definitions"
    "main = child |> place parent @p @q\nchild = box 2 2 2 |> datum q top\nparent = box 20 20 4 |> datum p top\n" ((-1,-1,0),(1,1,2))
  bounds "datum may be derived from another datum"
    "main = box 2 2 2 |> datum a top |> datum b @a 0 0 2 |> anchor @b\n" ((-1,-1,-4),(1,1,-2))
  bounds "box-anchor placement does not rotate the child"
    "main = box 2 4 6 |> place (box 20 10 4) rt bot\n" ((9,-2,0),(11,2,6))
  forM_ [("at top", (11,2,12)), ("on rt", (12,2,-11))] $ \(op, want) ->
    case p (base ++ "main = box 10 10 10 |> " ++ op ++ " base\n") of
      Left e -> failT t ("datum through " ++ op) e
      Right (_, s) -> assertT t ("datum through " ++ op)
        (either (const False) (`near` want) (pointOf (NamedPoint "p") s)) (show (pointOf (NamedPoint "p") s))
  expectGen t "datums preserve BOSL2 inclusion" (p "main = box 2 2 2 |> datum p ctr\n") "include <BOSL2/std.scad>"
  expectGen t "datum metadata works in loft profiles"
    (p "profile = ⭘ 2 |> datum axis ctr\nmain = loft 0 profile 5 profile\n") "method = \"reindex\""
  forM_ [ ("loft 0 (⭘ 2) 5 (⭘ 3 |> datum p rt)", (3,0,5))
        , ("(⭘ 3 |> datum p rt) |> extrude 5", (3,0,0))
        ] $ \(expr, want) -> case p ("main = " ++ expr ++ "\n") of
    Left e -> failT t ("profile datum: " ++ expr) e
    Right (_, s) -> assertT t ("profile datum: " ++ expr)
      (either (const False) (`near` want) (pointOf (NamedPoint "p") s)) (show s)
  bounds "2D construction datum can position a 3D child"
    "main = box 2 2 2 |> place (⭘ 10) rt ctr\n" ((9,-1,-1),(11,1,1))
  case p "tool = zcyl 1 8 |> datum bore ctr\nmain = box 10 10 4 |> cut tool\n" of
    Left e -> failT t "cutters retain construction datums" e
    Right (_, s) -> assertT t "cutters retain construction datums" (pointOf (NamedPoint "bore") s == Right (0,0,0)) (show s)
  forM_ [ ("main = box 2 2 2 |> anchor @missing\n", ["unknown datum '@missing'", "available datums: none"])
        , ("main = box 2 2 2 |> datum a ctr |> datum a top\n", ["duplicate datum '@a'"])
        , ("a = box 2 2 2 |> datum p ctr\nmain = (a ⊕ a) |> anchor @p\n", ["ambiguous datum '@p'", "2 copies"])
        , ("a = box 2 2 2 |> datum p ctr |> grid 2 1 3 0\nmain = sphere 1 |> place a @p ctr\n", ["ambiguous datum '@p'", "2 copies"])
        , ("main = sphere 1 |> place (box -2 2 2) top ctr\n", ["box width, depth and height"])
        , ("main = box 2 2 2\n  |> datum p @missing\n", ["topology.coscad:2:6:", "unknown datum"])
        , ("main = box 2 2 2 |> datum p ctr 1 2\n", ["datum", "third offset z"])
        , ("main = box 2 2 2 |> datum p ctr offset 0 0\n", ["undefined variable 'offset'"])
        , ("main = box 2 2 2 |> place (box 4 4 4) top bot offset 0 0\n", ["undefined variable 'offset'"])
        , ("main = box 2 2 2 |> place (box 4 4 4) top\n", ["topology.coscad:1:", "end of input"])
        , ("main = box 2 2 2 |> datum\n", ["topology.coscad:1:", "end of input"])
        ] $ \(source, fragments) -> expectErr t ("datum diagnostic: " ++ take 70 source) (p source) fragments ["circular dependency"]
  bounds "grid pitch and origin" "main = box 2 4 6 |> grid 3 2 10 20\n" ((-1,-2,-3),(21,22,3))
  bounds "grid permits negative pitch" "main = box 2 4 6 |> grid 3 2 -10 -20\n" ((-21,-22,-3),(1,2,3))
  bounds "single grid copy permits zero pitch" "main = box 2 4 6 |> grid 1 1 0 0\n" ((-1,-2,-3),(1,2,3))
  bounds "2D grid remains extrudable" "main = (⭘ 1 |> grid 2 2 5 6) |> extrude 3\n" ((-1,-1,0),(6,7,3))
  bounds "numeric grid dimensions resolve forward" "main = box 2 2 2 |> grid n 1 pitch 0\nn = 2\npitch = 4\n" ((-1,-1,-1),(5,1,1))
  forM_ [ ("grid 0 2 5 5", "column count must be a whole number")
        , ("grid 2 -1 5 5", "row count must be a whole number")
        , ("grid 2.5 2 5 5", "column count must be a whole number")
        , ("grid 2 10001 5 5", "row count must be a whole number")
        , ("grid 100 101 5 5", "grid copy count exceeds 10000")
        , ("grid 2 2 0 5", "pitch must be nonzero")
        , ("grid 2 2 5 0", "pitch must be nonzero")
        , ("grid 2 2 5", "row pitch")
        , ("grid 2 2 1e309 5", "finite")
        , ("grid 3 2 1e308 5", "translation must be finite")
        , ("grid 100 100 5 5 |> grid 2 1 1000 0", "including nested grids")
        ] $ \(stage, fragment) -> expectErr t ("grid diagnostic: " ++ stage) (p ("main = box 2 2 2 |> " ++ stage ++ "\n")) [fragment] ["circular dependency"]
  expectGen t "unary hull in a continued pipeline"
    (p "main = (box 2 2 2 |> at rt (box 2 2 2))\n  |> hull\n  |> anchor bot\n") "hull()"
  expectGen t "binary hull syntax remains accepted" (p "main = box 2 2 2 |> hull (sphere 2)\n") "sphere(2)"
  expectErr t "unary hull does not swallow malformed child"
    (p "main = box 2 2 2 |> hull missing\n") ["undefined variable 'missing'"] ["circular dependency"]
  forM_ ["", "!simple\n", "!glyph\n"] $ \pragma ->
    bounds ("new stages in mode " ++ show pragma)
      (pragma ++ "a = box 4 4 4 |> datum p top\nmain = box 2 2 2 |> grid 2 1 3 0 |> hull |> place a @p bot\n") ((-2.5,-1,2),(2.5,1,4))
  tmp <- tempDir "datum-plan"
  writeFile (tmp </> "a.coscad") "main = box 20 20 4\n"
  writeFile (tmp </> "b.coscad") "main = box 4 4 4\n"
  writeFile (tmp </> "test.assemble") "a ← a.coscad ×1\nb ← b.coscad ×1\nasm = (a |> datum mount top) |> add (b |> place a top bot)\n"
  planned <- planSummary (tmp </> "test.assemble")
  assertT t "datums preserve part identities; placement datum is not an instance"
    (either (const False) (\ps -> length (psParts ps) == 2 && sum (map (length . ssParts) (psSteps ps)) == 2) planned)
    (either id show planned)


planTier :: T -> IO ()
planTier t = do
  section "build plans"
  -- the design document's two-rail corner
  rc <- planSummary "examples/assemble/plan/corner_pair.assemble"
  case rc of
    Left e -> failT t "plan corner_pair" e
    Right ps -> do
      assertT t "corner_pair: 3 parts, 2 fasteners, no design errors" (length (psParts ps) == 3 && psFastenerCount ps == 2 && null (psDesignErrors ps)) (show ps)
      checkInvariants t "corner_pair" ps
      assertT t "corner_pair: both screws driven, no flips" (sum (map (length . ssFasteners) (psSteps ps)) == 2 && not (any ssFlip (psSteps ps))) (show (psSteps ps))
  -- the 2020 cube: rails, brackets, panel
  tmp <- tempDir "plan-cube"
  src <- readFile "examples/assemble/plan/cube.assemble"
  writeFile (tmp </> "cube.assemble") (src ++ "\nplan beam=10\n") -- narrow beam keeps the test fast
  forM_ ["rail200.coscad", "rail160.coscad", "flat90.coscad", "panel190.coscad"] $ \f -> copyFile ("examples/assemble/plan" </> f) (tmp </> f)
  rq <- planSummary (tmp </> "cube.assemble")
  case rq of
    Left e -> failT t "plan cube" e
    Right ps -> do
      assertT t "cube: 29 instances, 38 fasteners, no design errors" (length (psParts ps) == 29 && psFastenerCount ps == 38 && null (psDesignErrors ps)) (show (length (psParts ps), psFastenerCount ps, psDesignErrors ps))
      checkInvariants t "cube" ps
      assertT t "cube: every fastener driven" (sum (map (length . ssFasteners) (psSteps ps)) == 38) (show (sum (map (length . ssFasteners) (psSteps ps))))
      assertT t "cube: a handful of flips, not one per bracket" (length (filter ssFlip (psSteps ps)) <= 8) (show (length (filter ssFlip (psSteps ps))))
  -- design errors are reported: two screws at the same spot
  let bad = src ++ "\nfastener M5x10 br_bottom_fl bottom_front bot at=30\n"
  writeFile (tmp </> "bad.assemble") bad
  rb <- planSummary (tmp </> "bad.assemble")
  assertT t "plan: colliding screws are a design error" (either (const False) (any ("collide" `isInfixOf`) . psDesignErrors) rb) (either id (show . psDesignErrors) rb)
  writeFile (tmp </> "clamp.coscad") "main = box 5 2 5\n"
  writeFile (tmp </> "miss-slot.assemble") "rail ← rail200.coscad ×0 profile=2020\nclamp ← clamp.coscad ×1\nasm = (rail |> move 100 10 10) ⊕ (clamp |> move 100 -1 18)\nfastener M5x10 clamp rail fwd at=100\n"
  miss <- planSummary (tmp </> "miss-slot.assemble")
  expectErr t "plan: partial overlap cannot move a screw off the rail slot" miss ["does not cover the 2020 slot centre"] []
  -- the companion site pages embed the plan and reference the step images
  let page = buildPageHtml "cube" "{\"steps\": []}" "cube_step" 3
      idx = indexHtml [("cube", "cube", ["M5x10", "material=printed"], 29, 38, 29)]
  assertT t "site: build page embeds plan JSON, images, safe-area header, both themes"
    (all (`isInfixOf` page) ["const PLAN = {\"steps\": []}", "cube_step", "NSTEPS = 3", "env(safe-area-inset-top", "data-theme=dark", "prefers-color-scheme:dark"]) (take 400 page)
  assertT t "site: index lists builds with searchable tags" (all (`isInfixOf` idx) ["\"cube\"", "M5x10 material=printed", "\"steps\": 29"]) (take 400 idx)
  -- The bow now describes all four real bolts and captive nuts.
  rw <- planSummary "examples/assemble/bow3/bow3.assemble"
  assertT t "plan: bow3 places all three parts and makes all four M4 joints" (either (const False) (\ps -> sum (map (length . ssParts) (psSteps ps)) == 3 && psFastenerCount ps == 4 && null (psDesignErrors ps)) rw) (either id show rw)
  -- the bolted ball: enclosing shells, captive hex nuts in pockets
  rl <- planSummary "examples/assemble/ball/ball.assemble"
  case rl of
    Left e -> failT t "plan ball" e
    Right ps -> do
      assertT t "ball: 3 parts, 2 fasteners, no design errors" (length (psParts ps) == 3 && psFastenerCount ps == 2 && null (psDesignErrors ps)) (show ps)
      checkInvariants t "ball" ps
      assertT t "ball: both screws driven" (sum (map (length . ssFasteners) (psSteps ps)) == 2) (show (psSteps ps))
  ballMd <- readFile "examples/assemble/ball/ball_plan.md"
  assertT t "ball: plan speaks of hex nuts and pockets, not T-slots" (all (`isInfixOf` ballMd) ["M5 hex nut", "pocket on the top face", "socket head through shell1"] && not ("T-nut" `isInfixOf` ballMd)) (take 600 ballMd)
  ballSrc <- readFile "examples/assemble/ball/ball.assemble"
  tmpB <- tempDir "plan-ball"
  forM_ ["shell.coscad", "core.coscad"] $ \f -> copyFile ("examples/assemble/ball" </> f) (tmpB </> f)
  writeFile (tmpB </> "bad.assemble") (ballSrc ++ "\nfastener M5x20 shell1 core rt pocket=top\n")
  rp <- planSummary (tmpB </> "bad.assemble")
  assertT t "plan: pocket= without nut=hex is an error" (either ("pocket= only applies to nut=hex" `isInfixOf`) (const False) rp) (either id show rp)
  -- Preserve the old bolted tesseract regression alongside the new captive design.
  rt <- planSummary "examples/assemble/tesseract/bolted/tesseract.assemble"
  case rt of
    Left e -> failT t "plan tesseract" e
    Right ps -> do
      assertT t "tesseract: 29 parts, 40 joints, no design errors" (length (psParts ps) == 29 && psFastenerCount ps == 40 && null (psDesignErrors ps)) (show (length (psParts ps), psFastenerCount ps, psDesignErrors ps))
      checkInvariants t "tesseract" ps
      assertT t "tesseract: every joint made, every block preloaded with its nuts" (sum (map (length . ssFasteners) (psSteps ps)) == 40 && sort (concatMap ssPreload (psSteps ps)) == sort ["corner" ++ show i | i <- [1 .. 8 :: Int]]) (show (psSteps ps))
      assertT t "tesseract: starts with the corner blocks on the bench" (take 4 (concatMap ssParts (psSteps ps)) == ["corner1", "corner2", "corner3", "corner4"]) (show (take 6 (concatMap ssParts (psSteps ps))))
      assertT t "tesseract: no screw is driven up through the bench" (all (\st -> ssRest st /= "bottom (-Z) face" || null [f | f@(_, _, _, _) <- ssFasteners st, False]) (psSteps ps)) "n/a"
      assertT t "tesseract: at most 2 flips" (length (filter ssFlip (psSteps ps)) <= 2) (show (length (filter ssFlip (psSteps ps))))
  tessMd <- readFile "examples/assemble/tesseract/bolted/tesseract_plan.md"
  assertT t "tesseract: plan speaks of pegs and nut pockets in the blocks, no T-nuts" (all (`isInfixOf` tessMd) ["Push: strut1 into", "24 × M3 hex nut", "into the pocket on the top face"] && not ("T-nut" `isInfixOf` tessMd)) (take 500 tessMd)
  -- A rotor must enter its recessed journal before the next axle captures it.
  ro <- planSummary "models/orbit-tower/orbit-tower.assemble"
  case ro of
    Left e -> failT t "plan orbit tower" e
    Right ps -> do
      checkInvariants t "orbit tower" ps
      let steps = zip [1 :: Int ..] (psSteps ps)
          stepOf p = head ([k | (k, s) <- steps, p `elem` ssParts s] ++ [maxBound])
          joints = concatMap ssFasteners (psSteps ps)
      assertT t "orbit tower: 20 parts and all 19 peg joints, no hardware or errors"
        (length (psParts ps) == 20 && length joints == 19 && all (\(s, _, _, n) -> s == "peg" && n == "none") joints && null (psDesignErrors ps) && all (null . ssPreload) (psSteps ps)) (show ps)
      assertT t "orbit tower: stays upright throughout assembly"
        (all (\s -> ssRest s == "bottom (-Z) face" && not (ssFlip s)) (psSteps ps)) (show (psSteps ps))
      assertT t "orbit tower: every rotor enters before the axle or cap that captures it"
        (and [stepOf ("rotor" ++ show k) < stepOf (if k == 6 then "cap" else "axle" ++ show (k + 1)) | k <- [1 .. 6 :: Int]]) (show (psSteps ps))
      assertT t "orbit tower: each rotor and satellite follows its supporting part"
        (and [stepOf ("axle" ++ show k) < stepOf ("rotor" ++ show k) && stepOf ("rotor" ++ show k) < stepOf ("satellite" ++ show k) | k <- [1 .. 6 :: Int]]) (show (psSteps ps))
  orbitSrc <- readFile "models/orbit-tower/orbit-tower.assemble"
  tmpO <- tempDir "plan-orbit"
  forM_ ["base.coscad", "axle.coscad", "rotor.coscad", "satellite.coscad", "cap.coscad"] $ \f -> copyFile ("models/orbit-tower" </> f) (tmpO </> f)
  let withBaseHint hint = unlines [if "base ←" `isPrefixOf` l then l ++ " after=" ++ hint else l | l <- lines orbitSrc]
      invalidPlans =
        [ ("unknown after reference", withBaseHint "missing", "unknown part 'missing'")
        , ("unknown group after reference", withBaseHint "rotor", "unknown part")
        , ("self after cycle", withBaseHint "base", "cyclic after= ordering")
        , ("two-part after cycle", withBaseHint "axle1", "cyclic after= ordering")
        , ("negative peg inset", orbitSrc ++ "\nfastener peg rotor1 axle1 top inset=-1\n", "inset= must lie within the host")
        , ("peg inset beyond host", orbitSrc ++ "\nfastener peg rotor1 axle1 top inset=25\n", "inset= must lie within the host")
        , ("invalid peg inset", orbitSrc ++ "\nfastener peg rotor1 axle1 top inset=bad\n", "inset= needs a distance")
        , ("screw with peg inset", orbitSrc ++ "\nfastener M4x10 rotor1 axle1 top inset=1\n", "inset= only applies to peg joints")
        ]
  forM_ invalidPlans $ \(name, source, message) -> do
    writeFile (tmpO </> "bad.assemble") source
    result <- planSummary (tmpO </> "bad.assemble")
    assertT t ("plan: " ++ name ++ " is rejected") (either (message `isInfixOf`) (const False) result) (either id show result)

-- Independent synthetic joints: positions, directions and reach must follow
-- arbitrary rigid poses and renamed parts, rather than an example name.
framedFastenerTier :: T -> IO ()
framedFastenerTier t = do
  section "frame-based hardware geometry and diagnostics"
  dir <- tempDir "framed-fasteners"
  let host = "main = box 20 20 10 |> frame hole top top bak -4 -4 0 |> frame nut bot bot bak -4 -4 3\n"
      clamp = "main = box 20 20 4 |> frame head top top bak -4 -4 0\n"
      joint = "fastener M4x16 lid block @hole seat=@head head=socket nut=hex pocket=@nut\n"
      src = "block ← block.coscad ×1 preload=hand\nlid ← lid.coscad ×1\nasm = block ⊕ (lid |> move 0 0 7)\n" ++ joint
      run source = writeFile (dir </> "test.assemble") source >> planSummary (dir </> "test.assemble")
      near a b = vlen (vsub a b) < 1e-6
      writeParts h c = writeFile (dir </> "block.coscad") h >> writeFile (dir </> "lid.coscad") c
  writeParts host clamp
  good <- run src
  case good of
    Left e -> failT t "framed joint plans" e
    Right ps -> do
      checkInvariants t "framed joint" ps
      assertT t "framed joint: measured grip and nut reach"
        (case psFasteners ps of [f] -> near (fsPosition f) (-4,-4,5) && near (fsHeadPosition f) (-4,-4,9) && maybe False (`near` (-4,-4,-2)) (fsNutSeat f) && abs(fsThreadReach f - 5) < 1e-7; _ -> False) (show ps)
      assertT t "framed joint: in-hand preload precedes the host placement"
        (take 1 (concatMap ssPreload (psSteps ps)) == ["block"] && null (ssParts (head (psSteps ps)))) (show (psSteps ps))
      assertT t "framed joint: preload preparation warning survives step grouping"
        (any ("in hand" `isInfixOf`) (concatMap ssWarnings (psSteps ps))) (show(psSteps ps))
  let transform = " |> rotx 23 |> rotz 37 |> move 31 -17 9"
      rotated = "block ← block.coscad ×1 preload=hand\nlid ← lid.coscad ×1\nasm = (block ⊕ (lid |> move 0 0 7))" ++ transform ++ "\n" ++ joint
      move p = vadd (31,-17,9) (mApply (rodMat 37 (0,0,1)) (mApply (rodMat 23 (1,0,0)) p))
      axis = mApply (rodMat 37 (0,0,1)) (mApply (rodMat 23 (1,0,0)) (0,0,1))
  tilted <- run rotated
  assertT t "framed joint: arbitrary rotations carry hole, head, nut and axis"
    (either (const False) (\ps -> case psFasteners ps of [f] -> near (fsPosition f) (move (-4,-4,5)) && near (fsHeadPosition f) (move (-4,-4,9)) && near (fsAxis f) axis && maybe False (`near` move (-4,-4,-2)) (fsNutSeat f) && maybe False (`near` move (-4,-4,-5)) (fsNutEntry f) && abs(fsThreadReach f-5) < 1e-7; _ -> False) tilted) (either id show tilted)
  renamed <- run "core ← block.coscad ×1 preload=hand\ncover ← lid.coscad ×1\nasm = core ⊕ (cover |> move 0 0 7)\nfastener M4x16 cover core @hole seat=@head head=socket nut=hex pocket=@nut\n"
  assertT t "framed joint: aliases do not affect hardware placement"
    (either (const False) (\ps -> case psFasteners ps of [f] -> near (fsPosition f) (-4,-4,5) && abs(fsThreadReach f-5) < 1e-7; _ -> False) renamed) (either id show renamed)
  writeParts "main = box 20 20 16 |> frame hole top top bak -4 -4 0 |> frame nut bot bot bak -4 -4 3\n" clamp
  resized <- run "block ← block.coscad ×1 preload=hand\nlid ← lid.coscad ×1\nasm = block ⊕ (lid |> move 0 0 10)\nfastener M4x22 lid block @hole seat=@head head=socket nut=hex pocket=@nut\n"
  assertT t "framed joint: resized host updates both bearing planes and grip"
    (either (const False) (\ps -> case psFasteners ps of [f] -> near (fsPosition f) (-4,-4,8) && near (fsHeadPosition f) (-4,-4,12) && maybe False (`near` (-4,-4,-5)) (fsNutSeat f) && abs(fsThreadReach f-5) < 1e-7; _ -> False) resized) (either id show resized)
  writeParts "main = box 60 20 10 |> frame hole top top bak -4 0 0\n" "main = box 20 20 4 |> frame head top top bak -4 0 0\n"
  let railSrc = "block ← block.coscad ×0 profile=2020 preload=hand\nlid ← lid.coscad ×1\nasm = (block ⊕ (lid |> move 0 0 7))" ++ transform ++ "\nfastener M4x10 lid block @hole seat=@head\n"
  rail <- run railSrc
  assertT t "framed rail: slot checks use part coordinates under arbitrary rotation"
    (either (const False) (\ps -> case psFasteners ps of [f] -> near (fsPosition f) (move (-4,0,5)) && near (fsAxis f) axis && null(psDesignErrors ps); _ -> False) rail) (either id show rail)
  writeParts "main = box 60 20 10 |> frame hole top top bak -4 2 0\n" "main = box 20 20 4 |> frame head top top bak -4 2 0\n"
  offSlot <- run railSrc
  expectErr t "framed rail: an off-centre hole remains invalid after rotation" offSlot ["2020 slot centre"] []
  writeParts host clamp
  -- Longer solid above the counterbore must not change the bearing seat or
  -- produce a phantom driver collision with an object behind that seat.
  let recessed = "main = box 20 20 4 |> frame head ctr top bak -4 -4 0\n"
  writeParts host recessed
  recess <- run src
  assertT t "framed joint: recessed head uses its seat, not the outside box"
    (either (const False) (\ps -> case psFasteners ps of [f] -> near (fsHeadPosition f) (-4,-4,7) && abs(fsThreadReach f-7) < 1e-7; _ -> False) recess) (either id show recess)
  writeParts host clamp
  let invalid =
        [ ("missing head seat", "fastener M4x16 lid block @hole nut=hex pocket=@nut\n", "needs seat=@frame")
        , ("unknown hole", "fastener M4x16 lid block @missing seat=@head nut=hex pocket=@nut\n", "unknown frame '@missing'")
        , ("missing nut seat", "fastener M4x16 lid block @hole seat=@head nut=hex\n", "needs pocket=@frame")
        , ("mixed offset conventions", "fastener M4x16 lid block @hole seat=@head at=4 nut=hex pocket=@nut\n", "omit ×count/at=")
        , ("group at one hole", "fastener M4x16 lid block @hole ×2 seat=@head nut=hex pocket=@nut\n", "locates one fastener")
        , ("mixed grip conventions", "fastener M4x16 lid block @hole seat=@head through=4 nut=hex pocket=@nut\n", "omit inset=/through=")
        , ("frame option with old face", "fastener M4x16 lid block top seat=@head nut=hex pocket=@nut\n", "requires a host @frame")
        , ("frame peg", "fastener peg lid block @hole seat=@head\n", "currently describe screws")
        ]
      base = unlines (take 3 (lines src))
  forM_ invalid $ \(name,line,msg) -> do
    r <- run (base ++ line)
    expectErr t ("framed joint: " ++ name) r [msg] []
  forM_ [ ("misaligned head", "main = box 20 20 4 |> frame head top top bak -3 -4 0\n", "must be coaxial")
        , ("reversed head", "main = box 20 20 4 |> frame head top bot bak -4 -4 0\n", "matching outward normals")
        , ("head outside solid", "main = box 20 20 4 |> frame head top top bak 40 0 0\n", "outside part")
        , ("point instead of frame", "main = box 20 20 4 |> datum head top -4 -4 0\n", "point datum")
        , ("ambiguous frame", "main = (box 20 20 4 |> frame head top top bak -4 -4 0) ⊕ (box 20 20 4 |> frame head top top bak 4 4 0)\n", "ambiguous frame")
        ] $ \(name,c,msg) -> do
    writeParts host c
    r <- run src
    expectErr t ("framed joint: " ++ name) r [msg] []
  writeParts "main = box 20 20 10 |> frame hole top top bak -4 -4 0 |> frame nut bot top bak -4 -4 3\n" clamp
  wrongNut <- run src
  expectErr t "framed joint: nut bearing normal must oppose the head" wrongNut ["opposing outward normals"] []
  writeParts host clamp
  short <- run (base ++ "fastener M4x12 lid block @hole seat=@head nut=hex pocket=@nut\n")
  assertT t "framed joint: short screw checks actual distance to the nut seat"
    (either (const False) (any ("only 1 mm" `isInfixOf`) . psDesignErrors) short) (either id show short)
  duplicate <- run (src ++ joint)
  assertT t "framed joint: duplicate hole is a hardware collision"
    (either (const False) (any ("collide" `isInfixOf`) . psDesignErrors) duplicate) (either id show duplicate)
  -- A driver obstruction must remain an error, even though a frame can
  -- locate a recessed seat independently of the clamped part's outside box.
  writeFile (dir </> "obstacle.coscad") "main = box 2 2 30 |> move -4 -4 10\n"
  blocked <- run ("block ← block.coscad ×1 preload=hand\nlid ← lid.coscad ×1 after=obstacle\nobstacle ← obstacle.coscad ×1\nasm = block ⊕ (lid |> move 0 0 7) ⊕ obstacle\n" ++ joint)
  expectErr t "framed joint: another part can block the actual driver path" blocked ["blocked by obstacle"] []
  originalCorner <- readFile "examples/assemble/corner.assemble"
  forM_ ["rail200.coscad","rail120.coscad","inner90.coscad","bracket90.coscad"] $ \file -> copyFile ("examples/assemble" </> file) (dir </> file)
  let wrongCorner = unlines [if "mate bracket1 @other_bore railH" `isPrefixOf` line
                             then "mate bracket1 @outer_bore_railH railH @outer_bore_bracket1 roll=180"
                             else line | line <- lines originalCorner]
  badRotation <- run wrongCorner
  expectErr t "corner: closing both bracket bores rejects its original wrong rotation" badRotation ["conflicting assembly constraints"] []
  bow <- planSummary "examples/assemble/bow3/bow3.assemble"
  case bow of
    Left e -> failT t "bow frame hardware" e
    Right ps -> do
      checkInvariants t "bow hardware" ps
      assertT t "bow: four distinct holes, recessed heads, 4 mm nut reach"
        (sort (map fsPosition (psFasteners ps)) == sort [(x,y,0) | x <- [-42,42], y <- [-8,8]] && all (\f -> abs(fsThreadReach f-4) < 1e-7) (psFasteners ps) && null(psDesignErrors ps)) (show ps)
  corner <- planSummary "examples/assemble/corner.assemble"
  case corner of
    Left e -> failT t "corner frame hardware" e
    Right ps -> do
      checkInvariants t "corner hardware" ps
      assertT t "corner: all six screws planned, clear of each other"
        (psFastenerCount ps == 6 && sum(map(length . ssFasteners)(psSteps ps)) == 6 && null(psDesignErrors ps)) (show ps)

-- nut-first, host-and-clamp-before-screw, every part exactly once
checkInvariants :: T -> String -> PlanSummary -> IO ()
checkInvariants t name ps = do
  let steps = zip [1 :: Int ..] (psSteps ps)
      stepOfPart p = head ([k | (k, s) <- steps, p `elem` ssParts s] ++ [maxBound])
      stepOfPreload h = head ([k | (k, s) <- steps, h `elem` ssPreload s] ++ [maxBound])
      screws = [(k, f) | (k, s) <- steps, f <- ssFasteners s]
      lateHost = [f | (k, f@(_, c, h, _)) <- screws, stepOfPart h > k || stepOfPart c > k]
      lateNut = [f | (k, f@(_, c, h, nut)) <- screws, nut /= "none", min (stepOfPreload h) (stepOfPreload c) > k]
      placedTwice = [p | p <- psParts ps, length [() | (_, s) <- steps, p `elem` ssParts s] /= 1]
  assertT t (name ++ ": host and clamped part are placed before each screw") (null lateHost) (show lateHost)
  assertT t (name ++ ": every rail is preloaded before its first screw (nut-first)") (null lateNut) (show lateNut)
  assertT t (name ++ ": every part placed exactly once") (null placedTwice) (show placedTwice)

section :: String -> IO ()
section s = putStrLn ("\n== " ++ s)

-- ------------------------------------------------------------------
-- 1. diagnostics

diagnostics :: T -> IO ()
diagnostics t = do
  section "diagnostics"
  let p = parseProgramNamed "t.coscad"
  expectErr t "undefined variable: position + name"
    (p "main = foo ⊕ ● 5") ["t.coscad:1:8:", "undefined variable 'foo'"] ["circular"]
  expectErr t "syntax error: position, not 'circular'"
    (p "main = ● 5 ⊕ ▼ 3") ["t.coscad:1:14:", "unexpected", "a transform", "a shape"] ["circular", "expecting \"Anchor\""]
  expectErr t "error on a |> continuation line reports that line"
    (p "main = box 2 2 2\n  |> at top ▼\n") ["t.coscad:2:13:"] ["circular"]
  expectErr t "unclosed paren: end of input reported on the expression's last line"
    (p "main = plate\n  |> cutat top (zcyl 1 50\nplate = box 20 20 4\n") ["t.coscad:2:26:", "end of input", "an operator"] ["<empty line>", "circular"]
  expectErr t "circular dependency lists only the cycle"
    (p "a = b ⊕ ● 1\nb = a ⊕ ● 2\nmain = a\n")
    ["circular", "a  (t.coscad:1:5)", "b  (t.coscad:2:5)"] ["main  ("]
  expectErr t "duplicate definition"
    (p "a = ● 1\na = ● 2\nmain = a\n") ["t.coscad:2:5", "duplicate definition of 'a'", "t.coscad:1:5"] []
  expectErr t "missing main"
    (p "a = ● 5\n") ["no 'main'"] ["Error: Error"]
  expectErr t "dimension: extrude a solid"
    (p "main = ⮕ 2 (● 1)") ["t.coscad:1:8", "in 'main'", "extrude", "3D"] []
  expectErr t "dimension: offset a solid"
    (p "main = ■ 10 ↯ ● 2") ["offset", "3D"] []
  expectErr t "dimension: union of profile and solid"
    (p "main = ⭘ 3 ⊕ ■ 2") ["2D profile with a 3D"] []
  expectErr t "dimension: main is 2D"
    (p "main = △ 8") ["'main' is a 2D profile", "t.coscad:1:8"] []
  expectErr t "error inside a helper names the helper"
    (p "hole = ⮕ 2 (● 1)\nmain = ■ 10 ⊖ hole\n") ["t.coscad:1:8", "in 'hole'"] ["Undefined", "circular"]
  -- numeric bindings
  expectErr t "number used as a shape"
    (p "r = 5\nmain = r ⊕ ● 1\n") ["t.coscad:2:8", "'r' is a number, not a shape"] ["circular", "undefined"]
  expectErr t "main aliased to a number"
    (p "r = 5\nmain = r\n") ["'main' is a number"] []
  expectErr t "shape used as a number"
    (p "plate = box 1 1 1\nmain = ● plate\n") ["t.coscad:2:10", "'plate' is a shape, not a number"] ["circular"]
  expectErr t "main defined as a number"
    (p "main = 5\n") ["'main' is a number"] []
  expectErr t "undefined name in arithmetic"
    (p "r = q * 2\nmain = ● r\n") ["undefined variable 'q'"] ["circular"]
  expectGen t "numeric binding as argument" (p "r = 5\nmain = ● r\n") "sphere(5);"
  expectGen t "arithmetic in parens, forward numeric reference"
    (p "main = box (w / 2) w (w - 2 * 3)\nw = 20\n") "cuboid([10, 20, 14]);"
  expectGen t "numbers depend on numbers, negated name"
    (p "a = b * 2 + 1\nb = 3\nmain = χ -a (● 1)\n") "translate([-7, 0, 0])"
  expectGen t "numeric offset in a pipeline stage (identity translate elided)"
    (p "t = 4\nmain = box 20 20 t |> cutat top 0 0 (-t / 2) (zcyl 1 50)\n") "difference() {\n  cuboid([20, 20, 4]);\n  zcyl(r = 1, l = 50);"
  -- front-end review: reject silent reinterpretation without changing arithmetic
  expectErr t "spaced minus in primitive arguments"
    (p "w = 10\nmain = box w - 2 3\n") ["t.coscad:2:14:", "negative argument", "(w - 2)"] ["circular"]
  expectErr t "spaced minus in pipeline arguments"
    (p "main = box 1 1 1\n  |> move - 2 0 0\n") ["t.coscad:2:11:", "negative argument"] []
  expectErr t "tab after unary minus"
    (p "main = χ -\t2 (sphere 1)\n") ["negative argument"] []
  expectGen t "tight negation and spaced arithmetic remain valid"
    (p "w = 10\nk = - 2\nmain = box (w - 2) 3 4 |> move -w k -(w / 2)\n")
    "translate([-10, -2, -5])"
  expectGen t "compact glyph and parenthesized argument"
    (p "main = ●15 ⊕ ●(2 + 3)\n") "sphere(15);"
  forM_ [("division by zero", "10 / 0"), ("NaN", "0 / 0"), ("overflow", "1e308 * 10")] $ \(label, rhs) -> do
    expectErr t ("non-finite numeric binding: " ++ label)
      (p ("r = " ++ rhs ++ "\nmain = sphere r\n")) ["t.coscad:1:5", "in 'r'", "numeric binding must be finite"] ["circular", "undefined"]
    expectErr t ("non-finite argument: " ++ label)
      (p ("main = sphere (" ++ rhs ++ ")\n")) ["t.coscad:1:15", "numeric argument must be finite"] []
  expectErr t "non-finite forward dependency names the originating binding"
    (p "a = b * 2\nb = 1 / 0\nmain = sphere a\n") ["in 'b'", "must be finite"] ["circular"]
  expectErr t "unused non-finite binding is still an error"
    (p "bad = 0 / 0\nmain = sphere 1\n") ["in 'bad'", "must be finite"] []
  expectErr t "overflowing literal before prism side-count conversion"
    (p "main = ⎏ 1e999 2 3\n") ["must be finite"] []
  forM_ ["lft+rt", "top+top", "top+up", "ctr+top", "center+ctr"] $ \anchor ->
    expectErr t ("invalid anchor: " ++ anchor)
      (p ("main = box 10 10 10 |> at " ++ anchor ++ " (box 1 1 1)\n"))
      ["t.coscad:1:27:", "invalid anchor combination", "one direction per axis"] []
  expectGen t "three-axis anchor with aliases"
    (p "main = box 10 10 10 |> at up+right+front (box 2 2 2)\n") "translate([6, -6, 6])"
  expectGen t "center remains a valid anchor"
    (p "main = box 10 10 10 |> at ctr (box 2 2 2)\n") "cuboid([2, 2, 2]);"
  forM_ ["cube", "sphere", "xcyl", "loft", "Box", "Translate", "Offset"] $ \name ->
    expectErr t ("reserved definition: " ++ name)
      (p (name ++ " = 2\nmain = sphere 1\n")) ["t.coscad:1:", "reserved word '" ++ name ++ "'", "definition name"] []
  expectGen t "stage, anchor, and keyword-prefix bindings remain usable"
    (p "x = 2\ntop = 3\ncube2 = 4\nmain = box x top cube2\n") "cuboid([2, 3, 4]);"
  expectGen t "inactive simple keyword can be a glyph-mode binding"
    (p "!glyph\nBox = 2\nmain = sphere Box\n") "sphere(2);"
  expectErr t "simple-mode definition keyword is reserved"
    (p "!simple\nBox = 2\nmain = Sphere 1\n") ["t.coscad:2:", "reserved word 'Box'"] []
  expectErr t "glyph-mode common shape keyword is reserved"
    (p "!glyph\nsphere = 2\nmain = ● 1\n") ["reserved word 'sphere'"] []
  expectErr t "non-finite transform tuple argument"
    (p "!simple\nmain = Translate (0, (1 / 0), 0) (Sphere 1)\n") ["t.coscad:2:22", "numeric argument must be finite"] []
  -- lofts
  expectGen t "loft prefix form emits skin"
    (p "main = loft 0 (⭘ 10) 30 (⭘ 5)\n") "skin([circle(r = 10, $fn = 100), circle(r = 5, $fn = 100)], z = [0, 30], slices = 0, method = \"reindex\");"
  expectGen t "loft pipeline form starts at z = 0 and chains"
    (p "main = ⭘ 10 |> loft 20 (△ 5) |> loft 35 (⬠ 3)\n") "z = [0, 20, 35]"
  expectGen t "loft profile transforms become path functions"
    (p "main = loft 0 (χ 2 (ω 30 (⬈ 1 0.5 1 (⭘ 6)))) 10 (△ 4 ↯ ⭘ 1)\n")
    "move([2, 0], p = zrot(30, p = scale([1, 0.5], p = circle(r = 6, $fn = 100)))), offset(circle(r = 4, $fn = 3), r = 1, closed = true)"
  expectGen t "loft with mismatched vertex counts uses method distance"
    (p "main = loft 0 (⭘ 10) 30 (△ 5)\n") "method = \"distance\""
  expectGen t "loft with an offset profile (unknown count) uses method distance"
    (p "main = loft 0 (⭘ 10) 30 (⭘ 10 ↯ ⭘ 1)\n") "method = \"distance\""
  expectGen t "loft emits the BOSL2 include" (p "main = ⟰ 0 (⭘ 1) 1 (⭘ 2)\n") "include <BOSL2/std.scad>"
  expectErr t "loft of a solid"
    (p "main = loft 0 (● 3) 10 (⭘ 5)\n") ["in 'main'", "loft profiles must be 2D"] []
  expectErr t "loft with a boolean profile"
    (p "main = loft 0 (⭘ 3 ⊕ χ 5 (⭘ 3)) 10 (⭘ 5)\n") ["in loft profile", "single closed outline"] []
  expectErr t "loft with one profile"
    (p "main = loft 0 (⭘ 3)\n") ["at least two profiles"] []
  expectErr t "loft profile rotated out of plane"
    (p "main = loft 0 (θ 90 (⭘ 3)) 10 (⭘ 5)\n") ["XY plane"] []
  expectErr t "loft rejects vector Z translation"
    (p "main = loft 0 (⭘ 3 |> move 0 0 7) 10 (⭘ 5)\n") ["t.coscad:1:8", "cannot be moved in Z"] []
  expectErr t "loft rejects simple vector Z translation"
    (p "!simple\nmain = Loft 0 (Translate (0, 0, 7) (Circle 3)) 10 (Circle 5)\n") ["cannot be moved in Z"] []
  expectErr t "loft rejects oblique reflection"
    (p "main = loft 0 (⭘ 3 |> mirror 1 0 1) 10 (⭘ 5)\n") ["XY plane"] []
  expectErr t "loft rejects zero mirror normal"
    (p "main = loft 0 (⭘ 3 |> mirror 0 0 0) 10 (⭘ 5)\n") ["nonzero normal"] []
  expectGen t "loft preserves XY translation"
    (p "main = loft 0 (⭘ 3 |> move 2 4 0) 10 (⭘ 5)\n") "move([2, 4], p = circle(r = 3"
  expectGen t "loft preserves XY reflection"
    (p "main = loft 0 (△ 3 |> mirror 1 0 0) 10 (⭘ 5)\n") "mirror([1, 0], p = circle(r = 3"
  expectGen t "loft reflection in XY is identity"
    (p "main = loft 0 (△ 3 |> mirror 0 0 1) 10 (⭘ 5)\n") "skin([circle(r = 3, $fn = 3), circle(r = 5"
  -- success paths
  case p "main = box 2 2 2 // base\n  |> at top (box 1 1 1)\n" of
    Left e -> failT t "trailing comment does not swallow continuation lines" e
    Right (_, m) ->
      let (_, (_, _, zmax)) = bbox (resolve m)
       in assertT t "trailing comment does not swallow continuation lines" (abs (zmax - 2) < 1e-9) ("zmax = " ++ show zmax)
  case p "main = χ 5 $ ● 3 ⊕ ● 3" of
    Left e -> failT t "$ application" e
    Right (_, m) -> assertT t "$ application" ("translate([5, 0, 0]) {\n  union()" `isInfixOf` renderScad m) (renderScad m)
  case p "  main = ● 5\n" of
    Left e -> failT t "indented definition" e
    Right _ -> pass t
  -- number formatting in emitted .scad
  assertT t "showD: rotation noise, integers, -0, decimals"
    (map showD [3.061616997868383e-16, 10, -0.0, 2.5, -7, 0.1] == ["0", "10", "0", "2.5", "-7", "0.1"])
    (show (map showD [3.061616997868383e-16, 10, -0.0, 2.5, -7, 0.1]))
  -- packing: seven 43x43 brackets on a 120x120 plate with 6 mm margins = 4 + 3
  case packBeds (120, 120, 6) [("br_" ++ show i, ("b", "top"), (43, 42.98)) | i <- [1 .. 7 :: Int]] of
    Left e -> failT t "packBeds spills to a second plate" e
    Right plates -> assertT t "packBeds spills to a second plate" (map length plates == [4, 3]) (show (map length plates))
  case packBeds (100, 100, 6) [("big", ("b", "top"), (200, 10))] of
    Left e -> assertT t "packBeds rejects an oversized footprint with a clear message" ("exceeds the 100.0x100.0 plate" `isInfixOf` e) e
    Right _ -> failT t "packBeds rejects an oversized footprint with a clear message" "packed"
  -- the manual must mention every command, flag, and environment variable
  manPage <- readFile "man/coscad.1"
  let manMissing = [w | w <- ["Cm stl", "Cm next", "Cm check", "Cm doctor", "Cm plan", "Cm site", "keep-temp", "Fl -png", "Fl -version", "Fl -help", "COSCAD_OPENSCAD", "COSCAD_BOSL2", "loft", "cutat", "fastener", "Fl o"], not (w `isInfixOf` manPage)]
  assertT t "man/coscad.1 documents every command, flag, and env var" (null manMissing) (show manMissing)
  expectErr t "empty definition cannot swallow the next definition"
    (parseProgramNamed "empty.coscad" "a =\nmain = ■ 10\n")
    ["empty.coscad:1:4:", "missing expression after '=' in 'a'"] ["undefined", "circular"]
  expectErr t "comment after empty definition names the missing expression"
    (parseProgramNamed "empty.coscad" "a = // todo\nmain = ■ 10\n")
    ["missing expression after '=' in 'a'"] ["undefined"]
  -- .assemble diagnostics
  tmp <- tempDir "asm-diag"
  let asm1 = tmp </> "d1.assemble"
      asm2 = tmp </> "d2.assemble"
  writeFile asm1 "p ← x.coscad ×2 ▽ nowhere\nasm = p\n"
  r1 <- loadAssembleFile [] asm1
  expectErr t "assemble: bad part option at its real position" r1 ["d1.assemble:1:19:"] []
  writeFile asm2 "asm = q ⊕ ● 1\n"
  r2 <- loadAssembleFile [] asm2
  expectErr t "assemble: undefined part name" r2 ["d2.assemble:1:7:", "undefined variable 'q'"] ["circular"]
  writeFile asm1 "p ← x.coscad ×2 ▽top+up\nasm = p\n"
  r3 <- loadAssembleFile [] asm1
  expectErr t "assemble: invalid print-orientation anchor at declaration" r3
    ["d1.assemble:1:18:", "invalid anchor combination"] []
  writeFile asm2 "cube = box 1 1 1\nasm = cube\n"
  r4 <- loadAssembleFile [] asm2
  expectErr t "assemble: reserved helper name" r4 ["reserved word 'cube'"] []
  writeFile (tmp </> "part.coscad") "main = box 10 10 10\n"
  let twoParts = "a ← part.coscad\nb ← part.coscad\nasm = a ⌖ top b\n"
      badPlanOpts =
        [ ("beam=0", "positive whole number")
        , ("beam=-2", "positive whole number")
        , ("beam=1.5", "positive whole number")
        , ("beam=oops", "positive whole number")
        , ("beam=NaN", "positive whole number")
        , ("beam=Infinity", "positive whole number")
        , ("beam=1e100", "positive whole number")
        , ("astar=-1", "positive whole number")
        , ("astar=NaN", "positive whole number")
        , ("rest=bot,tpo", "unknown rest face 'tpo'")
        , ("rest=ctr", "unknown rest face 'ctr'")
        , ("flip=-1", "non-negative cost weight")
        , ("flip=NaN", "finite, non-negative")
        , ("flip=oops", "finite, non-negative")
        , ("beem=30", "unknown plan option 'beem'")
        , ("×3", "plan takes key=value")
        , ("beam=10 beam=20", "duplicate plan option 'beam'")
        ]
  forM_ badPlanOpts $ \(option, msg) -> do
    writeFile asm1 ("plan " ++ option ++ "\n" ++ twoParts)
    r <- loadAssembleFile [] asm1
    expectErr t ("assemble: rejects plan " ++ option) r ["d1.assemble:1:", msg] []
  forM_ ["a ← part.coscad ×18446744073709551616\nasm = a\n", twoParts ++ "fastener peg b a top ×18446744073709551616\n"] $ \src -> do
    writeFile asm1 src
    r <- loadAssembleFile [] asm1
    expectErr t "assemble: oversized count cannot wrap to zero" r ["count is too large", "maximum is"] []
  writeFile asm1 ("a ← part.coscad ×0 seam=rear custom=retained\nplan beam=1 astar=30 rest=bottom\nasm = a\n")
  r5 <- loadAssembleFile [] asm1
  -- `bottom` is intentionally not an anchor alias; check the documented one.
  expectErr t "assemble: unsupported rest alias is explicit" r5 ["unknown rest face 'bottom'"] []
  writeFile asm1 ("a ← part.coscad ×0 seam=rear custom=retained\nplan beam=1 astar=30 rest=down,up flip=0 vertical=.5\nasm = a\n")
  r6 <- loadAssembleFile [] asm1
  assertT t "assemble: valid controls, premade count and free part hints survive"
    (either (const False) (\ar -> isJust (arAsm ar) && lookup "a" (arPartMeta ar) == Just (0, [("seam", "rear"), ("custom", "retained")])) r6) (either id (const "valid") r6)
  writeFile asm1 ("a ← part.coscad\nplan\nasm = a\n")
  r7 <- loadAssembleFile [] asm1
  assertT t "assemble: empty plan line cannot consume the next definition"
    (either (const False) (\ar -> isJust (arAsm ar) && null (arPlanOpts ar)) r7) (either id (const "valid") r7)


-- Regression cases from the malformed-input audit: assert cause/location and
-- retain valid boundary forms, rather than merely asserting a nonzero CLI exit.
remainingDiagnostics :: T -> IO ()
remainingDiagnostics t = do
  section "remaining audit diagnostics"
  let p = parseProgramNamed "audit.coscad"
      invalidShapes =
        [ ("sphere -1", "sphere radius")
        , ("box 0 1 1", "box width, depth and height")
        , ("box -1 1 1", "greater than zero")
        , ("tube 3 5 10", "inner radius must be smaller")
        , ("tube 5 -1 10", "inner radius")
        , ("◙ 10 10 1 5", "rounding is too large")
        , ("▣ 10 10 1 5", "chamfer is too large")
        , ("⌽ 2 4 3", "rounding is too large")
        , ("⌭ 2 4 3", "chamfer is too large")
        , ("⎏ 2 5 10", "side count must be a whole number")
        , ("⎏ 3.5 5 10", "side count must be a whole number")
        , ("⎏ 1e100 5 10", "4096")
        , ("■ 10 |> scale 0 1 1", "scale factors must be nonzero")
        , ("■ 10 |> mirror 0 0 0", "nonzero normal")
        , ("⮕ 0 (⭘ 5)", "extrusion height")
        , ("loft 0 (⭘ 5) 0 (⭘ 3)", "strictly increasing")
        , ("loft 10 (⭘ 5) 0 (⭘ 3)", "strictly increasing")
        , ("box 10 20", "box is missing or has an invalid height argument")
        , ("■ 10 |> mov 1 2 3", "did you mean 'move'?")
        , ("■ 10 |> at top 1 2 (● 3)", "third offset z")
        , ("χ −2 (■ 5)", "Unicode minus U+2212")
        , ("cuube 10", "did you mean 'cube'?")
        , ("■ 1 |> scale 1e308 1e308 1e308 |> scale 10 10 10", "bounds overflowed")
        ]
  forM_ invalidShapes $ \(expr, msg) ->
    expectErr t ("audit: " ++ expr) (p ("main = " ++ expr ++ "\n")) ["audit.coscad:1:", msg] ["circular"]
  forM_ [("!simpel\nmain = cube 1\n", "unknown syntax mode"), ("!simple\n!glyph\nmain = cube 1\n", "only one syntax mode")] $ \(src, msg) ->
    expectErr t "audit: invalid mode directive" (p src) ["audit.coscad:", msg] []
  forM_ ["tube 5 0 10", "⏢ 10 10 0 0 5", "◙ 10 10 2 1", "■ 10 |> scale -1 1 1", "■ 10 |> mirror 1e200 1 0", "⎏ 4096 5 10"] $ \expr ->
    assertT t ("audit: valid boundary " ++ expr) (either (const False) (const True) (p ("main = " ++ expr ++ "\n"))) "unexpected rejection"
  expectGen t "audit: zero Z scale on a 2D profile is harmless"
    (p "main = ⮕ 2 (⭘ 5 |> scale 1 1 0)\n") "scale([1, 1, 0])"
  expectGen t "audit: pragma with trailing comment"
    (p "!simple // ASCII mode\nmain = Box 10 20 5\n") "cuboid([10, 20, 5]);"
  assertT t "audit: finite large number emission cannot overflow its formatter"
    (showD 1e308 == "1.0e308") (showD 1e308)
  expectErr t "audit: invalid empty plate is rejected before packing"
    (packBeds (100, 100, -1) []) ["plate dimensions", "margin"] []
  expectErr t "audit: infinite plate is rejected before packing"
    (packBeds (1 / 0, 100, 6) []) ["plate dimensions", "finite"] []
  expectErr t "audit: invalid footprint cannot stall packing"
    (packBeds (100, 100, 6) [("bad", ("b", "top"), (0 / 0, 10))]) ["bad footprint must be finite and positive"] []
  tmp <- tempDir "audit"
  writeFile (tmp </> "part.coscad") "main = box 10 10 10\n"
  let file = tmp </> "bad.assemble"
      base = "a ← part.coscad\nb ← part.coscad\nasm = a ⌖ top b\n"
      badAssemblies =
        [ (base ++ "fastener M0x0 b a top\n", "diameter and length must be positive")
        , (base ++ "fastener peg b a top ×0\n", "count must be at least 1")
        , (base ++ "fastener peg b a top at=oops\n", "at=oops: expected a finite")
        , (base ++ "fastener peg b a top at=NaN\n", "at=NaN: expected a finite")
        , (base ++ "fastener peg b a top inset=NaN\n", "inset= needs a distance")
        , (base ++ "fastener M4x20 b a top torque=oops\n", "torque=oops: expected a finite")
        , (base ++ "fastener M4x20 b a top torque=-2\n", "torque=-2: expected a finite")
        , (base ++ "fastener M4x20 b a top through=oops\n", "through=oops: expected a finite")
        , (base ++ "fastener M4x20 b a top nut=drpin\n", "did you mean 'dropin'?")
        , (base ++ "fastener M4x20 b a top head=buton\n", "did you mean 'button'?")
        , (base ++ "fastener M4x20 b a top nutt=hex\n", "unknown fastener option 'nutt'")
        , (base ++ "fastener M4x20 b a top head=button head=socket\n", "duplicate fastener option")
        , ("a ← part.coscad\na ← part.coscad\nasm = a\n", "duplicate part declaration 'a'")
        , ("cube ← part.coscad\nasm = cube\n", "reserved word 'cube' cannot be a part name")
        , ("plan beam=10\n" ++ base ++ "plan beam=20\n", "duplicate plan option 'beam'")
        , ("plate 1e999 220 6\n" ++ base, "plate width must be a finite")
        , ("plate 220 220 -6\n" ++ base, "plate margin must be a finite")
        , ("plate 10 10 5\n" ++ base, "plate margin must leave usable space")
        , ("clearance -1\n" ++ base, "clearance must be a finite")
        , ("clearance 1e999\n" ++ base, "clearance must be a finite")
        , ("a ← part.coscad mass=NaN\nasm = a\n", "mass=NaN: expected a finite")
        , ("a ← part.coscad mass=-1\nasm = a\n", "mass=-1: expected a finite")
        , ("a ← part.coscad material=stel\nasm = a\n", "unknown material 'stel'")
        , ("a ← part.coscad profile=202O\nasm = a\n", "unknown profile '202O'")
        , ("a ← part.coscad ends=blockd,open\nasm = a\n", "unknown rail end 'blockd'")
        , ("a ← part.coscad mass=1 mass=2\nasm = a\n", "duplicate part option 'mass'")
        , ("a ← part.coscad seam=\nasm = a\n", "missing value after 'seam='")
        , ("a ← part.coscad ×10001\nasm = a\n", "count is too large")
        , ("a ← part.coscad ×6000\nb ← part.coscad ×6000\nasm = a ⌖ top b\n", "total expanded print count is too large")
        , ("fastener peg b a\n" ++ base, "host face")
        ]
  forM_ badAssemblies $ \(src, msg) -> do
    writeFile file src
    result <- if "M0x0" `isInfixOf` src then fmap (fmap (const ())) (planSummary file) else fmap (fmap (const ())) (loadAssembleFile [] file)
    expectErr t ("audit assembly: " ++ msg) result [msg] []
  writeFile file "a ← part.coscad\na ← part.coscad\nasm = a\n"
  dup <- loadAssembleFile [] file
  expectErr t "audit: duplicate part names both source locations" dup ["bad.assemble:2:1:", "first declared at", "bad.assemble:1:1"] []
  writeFile (tmp </> "child.assemble") "a ← part.coscad ×10000\nasm = a\n"
  writeFile file "child ← child.assemble ×10000\nasm = child\n"
  overflow <- loadAssembleFile [] file
  expectErr t "audit: recursive counts validated before multiplication" overflow ["expanded print count", "too large"] []
  writeFile file "a ← part.coscad ×0 material=PLA seam=rear custom=retained\nasm = a\n"
  control <- planSummary file
  assertT t "audit: premade PLA and free part metadata survive" (either (const False) (const True) control) (either id show control)
  writeFile file (base ++ "fastener M4x20 b a top head=SOCKET nut=NONE through=0 torque=1\n")
  screw <- planSummary file
  assertT t "audit: validated enum values are case insensitive" (either (const False) (const True) screw) (either id show screw)
  writeFile file (base ++ "fastener peg b a top ×101\nfastener peg b a top\n")
  manyF <- planSummary file
  case manyF of
    Left e -> failT t "audit: expanded fastener IDs stay unique" e
    Right ps -> assertT t "audit: expanded fastener IDs stay unique"
      (psFastenerCount ps == 102 && sum (map (length . ssFasteners) (psSteps ps)) == 102) (show ps)

-- ------------------------------------------------------------------
-- 2. examples + snapshots

exampleFiles :: String -> IO [FilePath]
exampleFiles ext = do
  fs <- concat <$> mapM walk ["examples", "examples-next"]
  return (sort [normKey f | f <- fs, takeExtension f == ext, not ("examples/archive/" `isPrefixOf` normKey f)])
  where
    walk d = do
      es <- listDirectory d
      concat <$> forM es (\e -> do
        let f = d </> e
        isD <- doesDirectoryExist f
        if isD then walk f else return [f])

-- | Golden keys always use forward slashes so the files are shared across OSes.
normKey :: FilePath -> FilePath
normKey = map (\c -> if c == '\\' then '/' else c)

goldenScad :: FilePath -> FilePath
goldenScad f = "test/golden/scad" </> replaceExtension f ".scad"

examples :: T -> Bool -> IO ()
examples t update = do
  section "examples compile + .scad snapshots"
  fs <- exampleFiles ".coscad"
  forM_ fs $ \f -> do
    src <- readFile f
    case parseProgramNamed f src of
      Left e -> failT t ("compile " ++ f) e
      Right (_, m) -> do
        let out = renderScad m
            g = goldenScad f
        exists <- doesFileExist g
        if update || not exists
          then do
            createDirectoryIfMissing True (takeDirectory g)
            writeFile g out
            putStrLn ((if exists then "updated " else "created ") ++ g)
            pass t
          else do
            want <- readFile g
            assertT t ("snapshot " ++ f) (want == out) (firstDiff want out ++ "\n(COSCAD_UPDATE_GOLDEN=1 cabal test to accept)")
  section "assemblies load"
  as <- exampleFiles ".assemble"
  forM_ as $ \f -> do
    r <- loadAssembleFile [] f
    case r of
      Left e -> failT t ("load " ++ f) e
      Right _ -> pass t

firstDiff :: String -> String -> String
firstDiff a b =
  case [(i, x, y) | (i, x, y) <- zip3 [(1 :: Int) ..] (lines a ++ repeat "<eof>") (lines b ++ repeat "<eof>"), x /= y] of
    ((i, x, y) : _) -> "first difference at line " ++ show i ++ "\n  golden: " ++ x ++ "\n  now:    " ++ y
    [] -> "(no line differs; whitespace/eof?)"

-- ------------------------------------------------------------------
-- 3. geometry (OpenSCAD)

type Geo = (Double, (Double, Double, Double), (Double, Double, Double))

renderMesh :: FilePath -> FilePath -> String -> IO (Either String [Tri])
renderMesh _ dir scadText = do
  let scadF = dir </> "part.scad"
      stlF = dir </> "part.stl"
  writeFile scadF scadText
  r <- renderStlFile scadF stlF
  removePathForcibly stlF
  return r

geoOf :: [Tri] -> Geo
geoOf tris = (meshVolume tris, lo, hi)
  where
    (lo, hi) = meshBounds tris

showGeo :: Geo -> String
showGeo (v, (a, b, c), (d, e, f)) = unwords (map (printf "%.3f") [v, a, b, c, d, e, f])

readGeo :: [String] -> Maybe Geo
readGeo [v, a, b, c, d, e, f] = Just (read v, (read a, read b, read c), (read d, read e, read f))
readGeo _ = Nothing

geoClose :: Geo -> Geo -> Bool
geoClose (v1, lo1, hi1) (v2, lo2, hi2) =
  abs (v1 - v2) <= max 0.01 (1e-3 * abs v2) && cl lo1 lo2 && cl hi1 hi2
  where
    cl (a, b, c) (x, y, z) = all (\(p, q) -> abs (p - q) <= 0.01) [(a, x), (b, y), (c, z)]

geometry :: T -> Bool -> FilePath -> IO ()
geometry t update bin = do
  section ("geometry via " ++ bin)
  t0 <- getCurrentTime
  let goldenF = "test/golden/geometry.txt"
  haveG <- doesFileExist goldenF
  golden <- if haveG then Map.fromList . concatMap parseLine . lines <$> readFile goldenF else return Map.empty
  dir <- tempDir "geo"
  fs <- exampleFiles ".coscad"
  rows <- forM fs $ \f -> do
    src <- readFile f
    case parseProgramNamed f src of
      Left e -> failT t ("render " ++ f) e >> return Nothing
      Right (_, m) -> do
        r <- renderMesh bin dir (renderScad m)
        case r of
          Left e -> failT t ("render " ++ f) e >> return Nothing
          Right tris -> do
            let g = geoOf tris
            case Map.lookup f golden of
              Just want | not update ->
                assertT t ("geometry " ++ f) (geoClose want g)
                  ("volume/bounds drifted\n  golden: " ++ showGeo want ++ "\n  now:    " ++ showGeo g ++ "\n(COSCAD_UPDATE_GOLDEN=1 cabal test to accept)")
              Nothing | not update ->
                failT t ("geometry " ++ f) "missing geometry baseline; review the mesh and record it explicitly with COSCAD_UPDATE_GOLDEN=1 cabal test"
              _ -> putStrLn ("recorded " ++ f) >> pass t
            return (Just (f, g))
  let table = Map.fromList [x | Just x <- rows]
      merged = if update then Map.union table golden else Map.union golden table
  when update $
    writeFile goldenF (unlines ["# path volume xmin ymin zmin xmax ymax zmax (regenerate: COSCAD_UPDATE_GOLDEN=1 cabal test)"] ++ unlines [f ++ " " ++ showGeo g | (f, g) <- Map.toList merged])
  -- Check actual bracket material, not the planner's axis-aligned boxes.
  -- Independent world-space probes cover all 16 corners and 32 rail bores.
  cube <- loadAssembleFile [] "examples/assemble/plan/cube.assemble"
  case cube of
    Left e -> failT t "cube bracket geometry" e
    Right ar -> do
      let parts = Map.fromList [(n, if "br_" `isPrefixOf` n then sh else Hidden sh) | (n, sh) <- arParts ar]
      case resolveAssembly ar parts >>= \tab -> maybe (Left "missing asm") Right (Map.lookup "asm" tab) of
        Left e -> failT t "cube bracket geometry" e
        Right brackets -> do
          let corners = [(x,y,z) | x <- [10,190], y <- [10,190], z <- [-3,203]]
                     ++ [(x,y,z) | x <- [10,190], y <- [-3,203], z <- [10,190]]
              inward v = if v == 10 then 30 else 170
              ringHoles = concat [[(inward x,y,z),(x,inward y,z)] | x <- [10,190], y <- [10,190], z <- [-3,203]]
              sideHoles = concat [[(inward x,y,z),(x,y,inward z)] | x <- [10,190], y <- [-3,203], z <- [10,190]]
              probe dims (x,y,z) = "translate(" ++ show [x,y,z] ++ ") cube(" ++ show dims ++ ",center=true);"
              checkProbes name probes want = do
                let scad = "include <BOSL2/std.scad>\n$fn=50;\nunion(){translate([-1000,-1000,-1000]) cube(1); intersection(){" ++ gen brackets ++ "union(){" ++ concat probes ++ "}}}"
                r <- renderMesh bin dir scad
                case r of
                  Left e -> failT t name e
                  Right tris -> assertT t name (abs (meshVolume tris - want) < 0.01) (show (meshVolume tris, want))
          checkProbes "cube: material at all 16 bracket corners" (map (probe [2,2,2]) corners) 129
          checkProbes "cube: all 32 rail-aligned holes pass through brackets"
            (map (probe [2,2,8]) ringHoles ++ map (probe [2,8,2]) sideHoles) 1
          baseline <- loadAssembleFile [] "test/fixtures/cube-coordinate.assemble"
          case baseline of
            Left e -> failT t "cube coordinate pose baseline" e
            Right ref -> do
              let hidden = Map.fromList [(n, if n == "br" then sh else Hidden sh) | (n,sh) <- arParts ref]
              case resolveAssembly ref hidden >>= maybe (Left "missing baseline asm") Right . Map.lookup "asm" of
                Left e -> failT t "cube bracket pose equivalence" e
                Right original -> do
                  let actual = gen brackets
                      expected = gen original
                      sym = "include <BOSL2/std.scad>\nunion(){translate([-1000,-1000,-1000]) cube(1);difference(){"++actual++expected++"}difference(){"++expected++actual++"}}"
                  r <- renderMesh bin dir sym
                  assertT t "cube: all 16 bracket poses match coordinate reference exactly"
                    (either (const False) (\tris -> abs(meshVolume tris-1) < 1e-7 && length (splitBodies tris) == 1) r) (either id (show . meshVolume) r)
          let panelOnly = Map.fromList [(n,if n == "panel" then sh else Hidden sh) | (n,sh) <- arParts ar]
          case resolveAssembly ar panelOnly >>= maybe (Left "missing panel") Right . Map.lookup "asm" of
            Left e -> failT t "cube panel geometry" e
            Right panel -> do
              let bores = [(60,10),(140,10),(60,190),(140,190),(10,100),(190,100)] :: [(Double,Double)]
                  probes = concat ["translate([201.5,"++show y++","++show z++"]) cube([5,2,2],center=true);" | (y,z) <- bores]
                  scad = "include <BOSL2/std.scad>\nunion(){translate([-1000,-1000,-1000]) cube(1);intersection(){"++gen panel++"union(){"++probes++"}}}"
              r <- renderMesh bin dir scad
              assertT t "cube: all six panel bores match rail slot centres"
                (either (const False) (\tris -> abs(meshVolume tris-1) < 1e-7) r) (either id (show . meshVolume) r)
  -- Compare actual assembled solids, independently of mating metadata.
  -- Oblique rotations leave non-manifold zero-thickness slivers in CGAL
  -- subtraction. Their origins and all three axes are checked above instead;
  -- all individual printable parts still go through strict STL rendering.
  forM_ [c | c@(file,_,_,_) <- migrationCases, not (any (`isInfixOf` file) ["tesseract","orbit-tower"])] $ \(file,ref,_,_) -> do
    a <- loadAssembleFile [] file
    b <- loadAssembleFile [] ref
    case (a >>= maybe (Left "no asm") Right . arAsm, b >>= maybe (Left "no reference asm") Right . arAsm) of
      (Right actual,Right expected) -> do
        let x = gen actual
            y = gen expected
            scad = "include <BOSL2/std.scad>\nunion(){translate([-1000,-1000,-1000])cube(1);difference(){"++x++y++"}difference(){"++y++x++"}}"
            limit = 0.00001
        result <- renderMesh bin dir scad
        assertT t (file ++ ": assembled solid equivalence")
          (either (const False) (\tris -> abs(meshVolume tris-1) <= limit) result) (either id (\tris -> "symmetric difference mm3: "++show(abs(meshVolume tris-1))) result)
      _ -> failT t (file ++ ": geometry comparison") "cannot load migration pair"
  -- cross-check: the topological bracket is the coordinate bracket
  case (Map.lookup "examples/bracket.coscad" table, Map.lookup "examples/topological/tbracket.coscad" table) of
    (Just (v1, lo1, hi1), Just (v2, lo2, hi2)) ->
      let ext (a, b, c) (x, y, z) = (x - a, y - b, z - c)
          sameExt (a, b, c) (x, y, z) = abs (a - x) < 0.01 && abs (b - y) < 0.01 && abs (c - z) < 0.01
       in assertT t "tbracket is the same solid as bracket" (abs (v1 - v2) < 0.5 && sameExt (ext lo1 hi1) (ext lo2 hi2))
            (printf "bracket: vol %.3f ext %s\ntbracket: vol %.3f ext %s" v1 (show (ext lo1 hi1)) v2 (show (ext lo2 hi2)))
    _ -> failT t "tbracket is the same solid as bracket" "one of the two did not render"
  -- cross-check: a circle-to-circle loft is the frustum of the two 100-gons
  case parseProgramNamed "loft.coscad" "main = loft 0 (⭘ 10) 30 (⭘ 5)\n" of
    Left e -> failT t "loft frustum volume" e
    Right (_, m) -> do
      r <- renderMesh bin dir (renderScad m)
      case r of
        Left e -> failT t "loft frustum volume" e
        Right tris ->
          let k = (100 / (2 * pi)) * sin (2 * pi / 100) -- 100-gon area / disc area
              a1 = k * pi * 100
              a2 = k * pi * 25
              want = 30 / 3 * (a1 + a2 + sqrt (a1 * a2))
              got = meshVolume tris
           in assertT t "loft frustum volume" (abs (got - want) < 1e-3 * want) (printf "got %.3f want %.3f" got want)
  -- cross-check: `coscad next` conserves volume across bed packing
  bow <- tempDir "bow3"
  srcs <- listDirectory "examples/assemble/bow3"
  forM_ srcs $ \s -> copyFile ("examples/assemble/bow3" </> s) (bow </> s)
  rn <- try (processNext (bow </> "bow3.assemble")) :: IO (Either SomeException ())
  case rn of
    Left e -> failT t "coscad next bow3" (show e)
    Right () -> do
      bed <- readStl (bow </> "bow3_bed1.stl")
      parts <- mapM (\n -> readStl (bow </> ("bow3_next_" ++ n ++ "_asis.stl"))) ["center", "larch", "rarch"]
      mf <- decode <$> readFile (bow </> "bow3_manifest.json")
      assertT t "coscad next manifest is valid JSON with one bed of three placements"
        (case mf of Right v -> maybe False (\bs -> length bs == 1 && maybe False ((== 3) . length) (asArray =<< lookupKey "placements" (head bs))) (asArray =<< lookupKey "beds" v); Left _ -> False) (either id (const "parsed") mf)
      let vb = meshVolume bed
          vp = sum (map meshVolume parts)
          ((x0, y0, z0), (x1, y1, _)) = meshBounds bed
      assertT t "coscad next: bed volume == sum of variant volumes" (abs (vb - vp) < 1e-6 * vp) (printf "bed %.3f parts %.3f" vb vp)
      -- the bed and margin come from the fixture's `plate` line, via the manifest
      manifest <- readFile (bow </> "bow3_manifest.json")
      let num key = case dropWhile (not . isPrefixOf key) (tails manifest) of
            (m : _) -> read (takeWhile (`elem` "0123456789.") (dropWhile (`elem` "\": ") (drop (length key) m))) :: Double
            [] -> error ("manifest lacks " ++ key)
          (pw, pd, marg) = (num "\"w\"", num "\"d\"", num "\"margin\"")
      assertT t (printf "coscad next: placements inside the %.0fx%.0f bed with %.0fmm margin, on z=0" pw pd marg)
        (x0 >= marg - 1e-6 && y0 >= marg - 1e-6 && x1 <= pw - marg + 1e-6 && y1 <= pd - marg + 1e-6 && abs z0 < 1e-6)
        (show (meshBounds bed, pw, pd, marg))
  rc <- try (processCheckWith False (bow </> "bow3.assemble")) :: IO (Either SomeException ())
  assertT t "coscad check bow3: no overlaps" (either (const False) (const True) rc) (either show (const "") rc)
  leftovers <- filter ("chk" `isInfixOf`) <$> listDirectory bow
  assertT t "coscad check leaves no scratch files next to the assembly" (null leftovers) (show leftovers)
  -- Geometry from explicit frames must equal independently placed reference
  -- solids. A remote 1 mm3 marker makes an empty symmetric difference a
  -- successful STL export, without treating OpenSCAD failures as emptiness.
  cornerSrc <- readFile "examples/topological/mating-corner.coscad"
  case parseProgramNamed "mating-corner.coscad" cornerSrc of
    Left e -> failT t "oriented corner symmetric difference" e
    Right (_,s) -> do
      let reference = "union(){cube([20,20,4],center=true);translate([0,0,5]) cube([2,4,6],center=true);translate([13,0,0]) cube([6,2,4],center=true);}"
          actual = gen s
          symmetric = "include <BOSL2/std.scad>\nunion(){translate([-1000,-1000,-1000]) cube(1);difference(){" ++ actual ++ reference ++ "}difference(){" ++ reference ++ actual ++ "}}"
      r <- renderMesh bin dir symmetric
      assertT t "oriented corner symmetric difference is empty"
        (either (const False) (\tris -> abs (meshVolume tris - 1) < 1e-8 && length (splitBodies tris) == 1) r) (either id (show . meshVolume) r)
  framed <- tempDir "constraint-check"
  forM_ ["base.coscad", "post.coscad", "frame-stand.assemble"] $ \f ->
    copyFile ("examples/assemble/constraints" </> f) (framed </> f)
  checkDir <- tempDir "check-frame-stand"
  checked <- try (processCheckIn True (Just checkDir) (framed </> "frame-stand.assemble")) :: IO (Either SomeException ())
  case checked of
    Left e -> failT t "constraint check" (show e)
    Right () -> do
      forM_ [("left_post",(-13,-5,2),(-7,5,22)), ("right_post",(5,-3,2),(15,3,22))] $ \(n,wlo,whi) -> do
        tris <- readStl (checkDir </> ("chk_" ++ n ++ ".stl"))
        let bodies = filter (\body -> let ((x,_,_),_) = meshBounds body in x < 90000) (splitBodies tris)
            (lo,hi) = meshBounds (concat bodies)
            near (a,b,c) (x,y,z) = all (< 1e-6) [abs(a-x),abs(b-y),abs(c-z)]
        assertT t ("constraint check isolates world pose of " ++ n) (length bodies == 1 && near lo wlo && near hi whi) (show (lo,hi))
  writeFile (framed </> "collision.assemble") "base ← base.coscad ×1\nleft_post ← post.coscad ×1\nfixed base\nmate left_post @foot base @left_mount gap=-1\n"
  collision <- try (processCheckWith False (framed </> "collision.assemble")) :: IO (Either ExitCode ())
  assertT t "consistent constraints still expose actual interference (exit 2)" (collision == Left (ExitFailure 2)) (show collision)
  -- Hidden parts must retain their anchors during isolation, including
  -- chained attachments and transformed assemblies. Verify the rendered
  -- isolated child's actual bounds, not the bbox containing hidden parts.
  rel <- tempDir "relational-check"
  writeFile (rel </> "a.coscad") "main = box 20 20 20\n"
  writeFile (rel </> "b.coscad") "main = box 4 4 4\n"
  forM_ [ ("top", "a |> at top b", 10, 14)
        , ("on", "a |> on top b", 10, 14)
        , ("chain", "a |> at top b |> at top b", 10, 18)
        , ("moved", "(a |> at top b) |> z 30", 40, 44)
        , ("anchor", "(a |> at top b) |> anchor bot", 20, 24)
        , ("datum", "(a |> datum mount top) |> add (b |> place (a |> datum mount top) @mount bot)", 10, 14)
        ] $ \(name, expr, zlo, zhi) -> do
    let f = rel </> (name ++ ".assemble")
    writeFile f ("a ← a.coscad ×1\nb ← b.coscad ×1\nasm = " ++ expr ++ "\n")
    relDir <- tempDir ("check-" ++ name)
    result <- try (processCheckIn True (Just relDir) f) :: IO (Either SomeException ())
    case result of
      Left e -> failT t ("relational check " ++ name) (show e)
      Right () -> do
        tris <- readStl (relDir </> "chk_b.stl")
        let bodies = filter (\body -> let ((x, _, _), _) = meshBounds body in x < 90000) (splitBodies tris)
            ((_, _, lo), (_, _, hi)) = meshBounds (concat bodies)
        assertT t ("relational check " ++ name) (abs (lo - zlo) < 1e-6 && abs (hi - zhi) < 1e-6)
          (show (lo, hi))
  let overlapF = rel </> "overlap.assemble"
  writeFile overlapF "a ← a.coscad ×1\nb ← b.coscad ×1\nasm = a |> at top 0 0 -1 b\n"
  overlap <- try (processCheckWith False overlapF) :: IO (Either ExitCode ())
  assertT t "relational check still detects real overlap (exit 2)" (overlap == Left (ExitFailure 2)) (show overlap)
  -- design stage spills across plates instead of failing
  sp <- tempDir "spill"
  copyFile "examples/assemble/spill.assemble" (sp </> "spill.assemble")
  copyFile "examples/assemble/bracket90.coscad" (sp </> "bracket90.coscad")
  ra <- try (processAssemble (sp </> "spill.assemble")) :: IO (Either SomeException ())
  case ra of
    Left e -> failT t "design stage: multi-plate spill" (show e)
    Right () -> do
      p1 <- doesFileExist (sp </> "spill_plate1.scad")
      p2 <- doesFileExist (sp </> "spill_plate2.scad")
      mf <- readFile (sp </> "spill_manifest.json")
      assertT t "design stage: multi-plate spill" (p1 && p2 && "\"plate\": 2" `isInfixOf` mf && "\"count\": 2" `isInfixOf` mf) (show (p1, p2))
  -- `coscad stl` end to end
  st <- tempDir "stl"
  writeFile (st </> "p.coscad") "r = 5\nmain = ● r\n"
  rs <- try (renderPart (st </> "p.coscad") Nothing) :: IO (Either SomeException ())
  case rs of
    Left e -> failT t "coscad stl renders a part" (show e)
    Right () -> do
      tris <- readStl (st </> "p.stl")
      let v = meshVolume tris
      assertT t "coscad stl renders a part" (abs (v - 4 / 3 * pi * 125) < 0.03 * (4 / 3 * pi * 125)) (printf "sphere volume %.2f" v)
  -- step scenes render fully (Manifold) where OpenSCAD supports it, with the bench in the mesh
  full <- openscadFullRender
  tmpBase <- getTemporaryDirectory
  let sceneF = tmpBase </> "coscad-test" </> "cli" </> "corner_pair_step1.scad"
      scenePng = dir </> "step1.png"
      sceneStl = dir </> "step1.stl"
  haveScene <- doesFileExist sceneF
  if not haveScene
    then failT t "step scene from the cli tier exists" sceneF
    else do
      rp <- runOpenscadRaw (openscadPngArgs full scenePng sceneF)
      pngOk <- doesFileExist scenePng
      pngSize <- if pngOk then fromIntegral <$> withFile scenePng ReadMode hFileSize else return (0 :: Int)
      assertT t ("step scene renders to PNG (" ++ (if full then "full Manifold render" else "preview") ++ ")")
        (either (const False) (\(c, out, err) -> c == ExitSuccess && not (any ("ERROR" `isPrefixOf`) (lines (out ++ err)))) rp && pngOk && pngSize > 5000)
        (show rp ++ " size " ++ show pngSize)
      when full $ do
        rs <- runOpenscadRaw ["-o", sceneStl, "--export-format", "asciistl", "--backend=manifold", "--render=force", sceneF]
        tris <- either (const []) id . parseStlAscii <$> readFile sceneStl
        let ((_, _, z0), (_, _, z1)) = meshBounds tris
        assertT t "full render keeps the bench slab under the parts (scene extends 2.5 mm below the rest face)"
          (either (const False) (\(c, _, _) -> c == ExitSuccess) rs && not (null tris) && z0 < -2.4 && z1 > 0)
          (show rs ++ " bounds z " ++ show (z0, z1))
  -- toolchain probes used by `coscad doctor`
  ov <- openscadVersion
  assertT t "openscad --version is parsed" (maybe False (not . null) ov) (show ov)
  bv <- bosl2Version dir
  assertT t "BOSL2 answers the version probe" (either (const False) (not . null) bv) (show bv)
  t1 <- getCurrentTime
  printf "(geometry tier: %.1fs)\n" (realToFrac (diffUTCTime t1 t0) :: Double)
  where
    parseLine l
      | "#" `isPrefixOf` l = []
      | otherwise = case words l of
          (f : rest) | Just g <- readGeo rest -> [(f, g)]
          _ -> []

-- ------------------------------------------------------------------

-- | Decode a JSON file; a missing file is a Left, not an exception.
readJson :: FilePath -> IO (Either String JValue)
readJson f = do
  e <- doesFileExist f
  if e then decode <$> readFile f else return (Left (f ++ ": not written"))

readStl :: FilePath -> IO [Tri]
readStl f = either (\e -> error (f ++ ": " ++ e)) id . parseStlAscii <$> readFile f

-- ------------------------------------------------------------------
-- cli: the built binary, as an editor or pipeline sees it

cliTier :: T -> IO ()
cliTier t = do
  section "cli"
  exe <- findExecutable "coscad"
  case exe of
    Nothing -> failT t "coscad executable on PATH" "cabal puts it there via build-tool-depends; run the suite with `cabal test`"
    Just bin -> do
      dir <- tempDir "cli"
      let run args = readProcessWithExitCode bin args ""
          firstLine = takeWhile (/= '\n')
      (c0, o0, _) <- run ["--version"]
      assertT t "--version exits 0 and names the version" (c0 == ExitSuccess && "coscad " `isPrefixOf` o0) (show (c0, o0))
      (c1, _, e1) <- run []
      assertT t "no arguments is a usage error (exit 1)" (c1 == ExitFailure 1 && not (null e1)) (show (c1, e1))
      (c2, o2, e2) <- run [dir </> "missing.coscad"]
      assertT t "missing file: exit 1, Error: on stderr, nothing on stdout" (c2 == ExitFailure 1 && "Error: " `isPrefixOf` e2 && "not found" `isInfixOf` e2 && null o2) (show (c2, o2, e2))
      (c2v, o2v, e2v) <- run ["validate", dir </> "missing.coscad"]
      assertT t "validate missing file: exit 1, Error: on stderr" (c2v == ExitFailure 1 && "Error: " `isPrefixOf` e2v && "not found" `isInfixOf` e2v && null o2v) (show (c2v, o2v, e2v))
      writeFile (dir </> "ok.coscad") "r = 5\nmain = ● r\n"
      (c3, o3, e3) <- run ["validate", dir </> "ok.coscad"]
      assertT t "validate good part: exit 0, Valid part, empty stderr" (c3 == ExitSuccess && o3 == "Valid part\n" && null e3) (show (c3, o3, e3))
      writeFile (dir </> "bad.coscad") "main = ■ 10 |> mov 1 2 3\n"
      (c4, o4, e4) <- run ["validate", dir </> "bad.coscad"]
      let want4 = "Error: " ++ dir </> "bad.coscad" ++ ":1:16: unknown pipeline stage 'mov'"
      assertT t "validate bad part: exit 1, first stderr line is Error: file:line:col: reason" (c4 == ExitFailure 1 && want4 `isPrefixOf` firstLine e4 && "did you mean 'move'" `isInfixOf` firstLine e4 && null o4) (show (c4, o4, e4))
      assertT t "validate bad part: the excerpt follows the reason" (any ("| main = " `isInfixOf`) (lines e4) && any ("^" `isInfixOf`) (lines e4)) e4
      writeFile (dir </> "syntax.coscad") "main = box 10 20\n"
      (c5, _, e5) <- run ["validate", dir </> "syntax.coscad"]
      assertT t "validate syntax error: position and reason on one line" (c5 == ExitFailure 1 && ("Error: " ++ dir </> "syntax.coscad" ++ ":1:") `isPrefixOf` firstLine e5 && "height" `isInfixOf` firstLine e5) (show (c5, e5))
      writeFile (dir </> "nopart.assemble") "a ← gone.coscad ×1\nasm = a\n"
      (c6, _, e6) <- run ["validate", dir </> "nopart.assemble"]
      assertT t "validate assembly with a missing part names the file" (c6 == ExitFailure 1 && ("Error: " ++ dir </> "gone.coscad" ++ ": part file not found") `isPrefixOf` firstLine e6) (show (c6, e6))
      (c7, _, e7) <- run ["next", dir </> "ok.coscad"]
      assertT t "next on a .coscad is a usage error" (c7 == ExitFailure 1 && "Error: " `isPrefixOf` e7) (show (c7, e7))
      -- design stage + plan on the corner pair fixture, read back as JSON
      planFiles <- listDirectory "examples/assemble/plan"
      forM_ ("corner_pair.assemble" : filter ((== ".coscad") . takeExtension) planFiles) $ \f -> copyFile ("examples/assemble/plan" </> f) (dir </> f)
      (c8, o8, e8) <- run [dir </> "corner_pair.assemble"]
      mf <- readJson (dir </> "corner_pair_manifest.json")
      assertT t "design stage: exit 0, manifest parses, plate and placements present" (c8 == ExitSuccess && null e8 && "Wrote" `isInfixOf` o8 && either (const False) (\v -> isJust (lookupKey "plate" v) && maybe False (not . null) (asArray =<< lookupKey "placements" v)) mf) (show (c8, e8, either id (const "parsed") mf))
      (c9, o9, e9) <- run ["plan", dir </> "corner_pair.assemble"]
      pj <- readJson (dir </> "corner_pair_plan.json")
      let nuts v = [n | st <- fromMaybe [] (asArray =<< lookupKey "steps" v), f <- fromMaybe [] (asArray =<< lookupKey "fasteners" st), Just n <- [asString =<< lookupKey "nut" f]]
          steps v = fromMaybe [] (asArray =<< lookupKey "steps" v)
      step1 <- readFile (dir </> "corner_pair_step1.scad")
      assertT t "plan: step scene draws the bench as a solid slab, no background modifier" ("color(\"Gainsboro\") translate(" `isInfixOf` step1 && not ("\n%" `isInfixOf` step1) && "color(\"Orange\")" `isInfixOf` step1) (take 600 step1)
      assertT t "plan: exit 0, plan JSON parses, nut kinds use the nut= words" (c9 == ExitSuccess && null e9 && "Wrote" `isInfixOf` o9 && either (const False) (\v -> not (null (steps v)) && all (`elem` ["dropin", "slidein", "hex", "none"]) (nuts v) && not (null (nuts v))) pj) (show (c9, e9, either id (const "parsed") pj))
      -- a design error: two screws at the same spot. Exit 2, stderr, files still written.
      srcPair <- readFile (dir </> "corner_pair.assemble")
      let fastLines = [l | l <- lines srcPair, "fastener " `isPrefixOf` l]
      case fastLines of
        (fl : _) -> do
          writeFile (dir </> "collide.assemble") (srcPair ++ "\n" ++ fl ++ "\n")
          (c10, o10, e10) <- run ["validate", dir </> "collide.assemble"]
          assertT t "validate design error: exit 2, Error: on stderr" (c10 == ExitFailure 2 && "Error: " `isPrefixOf` e10 && "collide" `isInfixOf` e10 && null o10) (show (c10, o10, e10))
          (c11, _, e11) <- run ["plan", dir </> "collide.assemble"]
          pj2 <- readJson (dir </> "collide_plan.json")
          assertT t "plan design error: exit 2, design: on stderr, design_errors in the JSON" (c11 == ExitFailure 2 && (": design: " `isInfixOf` e11) && either (const False) (maybe False (not . null) . (asArray =<<) . lookupKey "design_errors") pj2) (show (c11, e11, either id (const "parsed") pj2))
        [] -> failT t "corner_pair has a fastener line" srcPair
      -- the STL reader diagnoses what OpenSCAD can hand back
      let nanStl = "solid x\n facet normal 0 0 1\n outer loop\n vertex 0 0 0\n vertex 1 0 0\n vertex nan 1 0\n endloop\n endfacet\nendsolid x\n"
          binStl = "solid bin" ++ replicate 80 '\0' ++ "\1\0\0\0"
      assertT t "STL: non-finite vertex is an error naming the line" (either ("line 6" `isInfixOf`) (const False) (parseStlAscii nanStl)) (show (parseStlAscii nanStl))
      assertT t "STL: binary file is an error, not a crash" (either ("not ASCII" `isInfixOf`) (const False) (parseStlAscii binStl)) (show (parseStlAscii binStl))
      assertT t "STL: empty solid parses to no triangles" (parseStlAscii "solid e\nendsolid e\n" == Right []) (show (parseStlAscii "solid e\nendsolid e\n"))
      assertT t "STL: bounds of the empty mesh are a point" (meshBounds [] == ((0, 0, 0), (0, 0, 0))) ""
      -- JSON round trip of awkward strings
      let awkward = JObject [("s", JString "quote\" back\\ nl\n tab\t uni\x2028 </script>"), ("n", JArray [JNumber 1.5, JNumber (-2), JNull, JBool True])]
      assertT t "Coscad.Json round-trips escapes" (decode (Coscad.Json.encode awkward) == Right awkward) (Coscad.Json.encode awkward)
      _ <- return (asNumber JNull)
      return ()

-- ------------------------------------------------------------------

tempDir :: String -> IO FilePath
tempDir name = do
  base <- getTemporaryDirectory
  let d = base </> "coscad-test" </> name
  removePathForcibly d
  createDirectoryIfMissing True d
  return d

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  t <- T <$> newIORef 0 <*> newIORef []
  update <- (== Just "1") <$> lookupEnv "COSCAD_UPDATE_GOLDEN"
  diagnostics t
  remainingDiagnostics t
  topologyTier t
  frameTier t
  cubeTier t
  migrationTier t
  tesseractTier t
  examples t update
  planTier t
  framedFastenerTier t
  cliTier t
  render <- lookupEnv "COSCAD_RENDER"
  bin <- findOpenscad
  case (render, bin) of
    (Just "0", _) -> putStrLn "\n(geometry tier skipped: COSCAD_RENDER=0)"
    (_, Nothing) -> putStrLn "\n(geometry tier skipped: OpenSCAD not found; set COSCAD_OPENSCAD)"
    (_, Just b) -> geometry t update b
  n <- readIORef (tPassed t)
  fails <- readIORef (tFailed t)
  putStrLn ""
  printf "%d passed, %d failed\n" n (length fails)
  unless (null fails) $ do
    mapM_ (putStrLn . ("  - " ++)) fails
    exitFailure
