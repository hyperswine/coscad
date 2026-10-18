-- | Assembly tiers: constraint solving, frames, topology, migrations
-- and the cube/tesseract fixtures.
module Tier.Assembly
  ( cubeTier
  , migrationPoses
  , migrationTier
  , tesseractTier
  , frameTier
  , topologyTier
  , ringAssembly
  , writeLink
  , constraintLoopTier
  , tagNames
  )
where

import Control.Monad (forM_, when)
import Coscad.Assemble (AsmResult (..), loadAssembleFile, loadAssembly, resolveAssembly)
import Coscad.Report (silent)
import Coscad.Variants (FlatPart (..))
import Coscad.Constraint (AsmError (..), Constraint (..), Tolerance (..), defaultTolerance, errorText)
import Coscad.Codegen (renderScad)
import Coscad.Geometry (PointRef (..), MatingFrame (..), bbox, bcenter, pointOf, resolve, frameOf, mApply, vneg, vnormed, dot3, vlen, vsub, scale3, vadd, vmul, rodMat, mirrorMat)
import Coscad.Linalg (matTranspose)
import Coscad.Parser (parseProgramNamed)
import Coscad.Plan (PlanSummary (..), StepSummary (..), planSummary)
import MigrationCases (migrationCases)
import Coscad.Shape (Shape (..), shapeChildren)
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Map as Map
import System.Directory
import System.FilePath
import Harness

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
        let i = Map.findWithDefault (0 :: Int) n seen + 1
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
        let sh = maybe Empty id (arAsm actual)
            old = maybe Empty id (arAsm original)
            (lo,hi) = bbox sh
            (ol,oh) = bbox old
            near (x,y,z) (u,v,w) = maximum [abs(x-u),abs(y-v),abs(z-w)] <= tolerance
        assertT t (file ++ ": both versions define asm") (isJust (arAsm actual) && isJust (arAsm original)) "missing asm"
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
        , ("multiplicity ambiguity", "a ← part.coscad ×2\nfixed a\n", ["graph.assemble:1:1:", "distinct instance names", "×2"])
        , ("incomplete fixed pose", refs ++ "fixed a 1 2 3\n", ["graph.assemble:4:", "six pose numbers"])
        , ("bad mate option", refs ++ "mate b @foot a @head rol=90\n", ["graph.assemble:4:", "did you mean 'roll'"])
        , ("duplicate mate option", refs ++ "mate b @foot a @head roll=0 roll=90\n", ["duplicate mate option 'roll'"])
        , ("nonfinite mate option", refs ++ "mate b @foot a @head gap=NaN\n", ["expected a finite distance"])
        , ("keyword cannot swallow next line", refs ++ "mate b @foot a\nfixed a\n", ["graph.assemble:4:", "oriented frame reference"])
        ] $ \(name,src,want) -> load src >>= \r -> expectErr t name r want ["circular dependency"]
  constraintLoopTier t dir
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

-- | A ring of eight links from /workspace/edge/loop: l1 fixed, l(k+1)
-- mated to l(k) on lines 10-16, and the closing mate l1 → l8 on line 17.
ringAssembly :: String -> String
ringAssembly closing =
  concat ["l" ++ show k ++ " ← link.coscad ×1\n" | k <- [1 .. 8 :: Int]]
    ++ "fixed l1\n"
    ++ concat ["mate l" ++ show (k + 1) ++ " @a l" ++ show k ++ " @b\n" | k <- [1 .. 7 :: Int]]
    ++ "mate l1 @a l8 @b" ++ closing ++ "\n"

writeLink :: FilePath -> IO ()
writeLink dir = writeFile (dir </> "link.coscad") "main = box 10 10 10\n  |> frame a lft lft top\n  |> frame b rt+fwd rt+fwd top\n"

constraintLoopTier :: T -> FilePath -> IO ()
constraintLoopTier t dir = do
  writeLink dir
  let file = dir </> "loop.assemble"
      load src = writeFile file src >> loadAssembly silent [] file
      firstLine = takeWhile (/= '\n')
      at n = file ++ ":" ++ show (n :: Int) ++ ":1:"
      design name src check = do
        r <- load src
        case r of
          Left (DesignError m) -> assertT t name (check m) m
          Left (InputError m) -> failT t name ("expected a design error, got an input error:\n" ++ m)
          Right _ -> failT t name "expected a design error, but it loaded"
      inputErr name src want = do
        r <- load src
        case r of
          Left (InputError m) -> assertT t name (all (`isInfixOf` m) want) m
          Left (DesignError m) -> failT t name ("expected an input error, got a design error:\n" ++ m)
          Right _ -> failT t name "expected an input error, but it loaded"
      wholeRing m =
        (at 17 `isPrefixOf` firstLine m)
          && and [("  note: " ++ at n ++ " mate ") `isInfixOf` m | n <- [10 .. 17]]
          && length (lines m) == 9
          && not (at 13 `isInfixOf` firstLine m)
  design "loop: ring with a 2 µm closing gap is blamed on the closing mate (line 17) and lists all 8 mates"
    (ringAssembly " gap=0.000002") (\m -> wholeRing m && "position residual" `isInfixOf` m && "Any of its 8 constraints" `isInfixOf` m)
  design "loop: ring with a 1 µdeg closing roll lists the whole loop with its orientation residual"
    (ringAssembly " roll=0.000001") (\m -> wholeRing m && "orientation residual" `isInfixOf` m)
  rs <- load (ringAssembly " gap=0.0000005")
  assertT t "loop: a closing gap inside the 1e-6 mm tolerance still loads" (either (const False) (const True) rs) (either show (const "") rs)
  rc <- load (ringAssembly "")
  assertT t "loop: a consistent ring loads" (either (const False) (const True) rc) (either show (const "") rc)
  let links n = concat ["l" ++ show k ++ " ← link.coscad ×1\n" | k <- [1 .. n :: Int]]
  design "floating part is reported at its own declaration, not the first constraint"
    (links 3 ++ "fixed l1\nmate l2 @a l1 @b\n") (\m -> (at 3 ++ " underconstrained assembly") `isPrefixOf` firstLine m && "reaches l3" `isInfixOf` m)
  design "several floating parts: the first declaration is the error, each other one a note"
    (links 4 ++ "fixed l1\n") (\m -> at 2 `isPrefixOf` firstLine m && "reaches l2 l3 l4" `isInfixOf` m && ("  note: " ++ at 3) `isInfixOf` m && ("  note: " ++ at 4) `isInfixOf` m)
  design "two fixed roots that disagree: the chain between them with both fixed lines"
    (links 2 ++ "fixed l1\nfixed l2\nmate l2 @a l1 @b\n")
    (\m -> at 5 `isPrefixOf` firstLine m && "chain between fixed instances" `isInfixOf` m && all (`isInfixOf` m) ["  note: " ++ at 3 ++ " fixed l1", "  note: " ++ at 4 ++ " fixed l2", "  note: " ++ at 5 ++ " mate l2 @a l1 @b"])
  design "duplicate fixed is a design error that points back at the first fixed"
    (links 2 ++ "fixed l1\nfixed l1\nmate l2 @a l1 @b\n") (\m -> at 4 `isPrefixOf` firstLine m && "duplicate fixed" `isInfixOf` m && ("  note: " ++ at 3 ++ " fixed l1") `isInfixOf` m)
  rd <- load (links 2 ++ "fixed l1\nmate l2 @a l1 @b\nmate l2 @a l1 @b\n")
  assertT t "an exactly repeated mate is consistent and loads" (either (const False) (const True) rd) (either show (const "") rd)
  inputErr "unknown instance in a mate stays an input error" (links 2 ++ "fixed l1\nmate l9 @a l1 @b\n") [at 4, "unknown constrained instance 'l9'"]
  inputErr "unknown frame stays an input error" (links 2 ++ "fixed l1\nmate l2 @zz l1 @b\n") [at 4, "unknown frame"]
  inputErr "syntax error stays an input error" (links 2 ++ "fixed l1 1 2\n") [file ++ ":3:13:", "six pose numbers"]
  -- a conflict inside a subassembly is still a design error when loaded through its parent
  writeFile (dir </> "ring.assemble") (ringAssembly " gap=0.000002")
  writeFile (dir </> "outer.assemble") "r ← ring.assemble ×1\nasm = r\n"
  ro <- loadAssembly silent [] (dir </> "outer.assemble")
  assertT t "a loop conflict in a subassembly is a design error of the parent too, located in the subassembly"
    (case ro of Left (DesignError m) -> ((dir </> "ring.assemble") ++ ":17:1:") `isPrefixOf` m; _ -> False) (either show (const "loaded") ro)
  -- numeric bindings and arithmetic in fixed / mate, and the tolerance statement (step 10)
  let posAt n c = file ++ ":" ++ show (n :: Int) ++ ":" ++ show (c :: Int) ++ ":"
      poses r = case r of
        Right ar -> Right [(n, tr, rot) | Fixed _ n tr rot <- arConstraints ar]
        Left e -> Left (errorText e)
      mates r = case r of
        Right ar -> Right [(roll, gap) | Mate _ _ _ _ _ roll gap <- arConstraints ar]
        Left e -> Left (errorText e)
  rb <- load ("w = 8.535533906\n" ++ links 2 ++ "fixed l1\nfixed l2 w 0 0 0 0 0\n")
  assertT t "fixed: a numeric binding is a pose number" (poses rb == Right [("l1", (0, 0, 0), (0, 0, 0)), ("l2", (8.535533906, 0, 0), (0, 0, 0))]) (show (poses rb))
  re <- load (links 2 ++ "fixed l1\nfixed l2 (w / 2) -w (-(w - 4)) 0 0 (a * 3)\nw = 10\na = w + 5\n")
  assertT t "fixed: atoms, negation, parenthesized arithmetic and forward bindings"
    (poses re == Right [("l1", (0, 0, 0), (0, 0, 0)), ("l2", (5, -10, -6), (0, 0, 45))]) (show (poses re))
  rm1 <- load ("a = 90\n" ++ links 2 ++ "fixed l1\nmate l2 @a l1 @b roll=(a / 2) gap=-g\ng = 0.5\n")
  assertT t "mate: roll and gap take bindings and arithmetic" (mates rm1 == Right [(45, -0.5)]) (show (mates rm1))
  rm2 <- load (links 2 ++ "fixed l1\nmate l2 @a l1 @b roll=45 gap=-0.5\n")
  assertT t "mate: an expression gives the same assembly as the literal"
    (case (rm1, rm2) of (Right a1, Right a2) -> fmap show (arAsm a1) == fmap show (arAsm a2); _ -> False) ""
  let two5 = links 2 ++ "fixed l1\nfixed l2 8.53553 -8.53553 0 0 0 -45\nmate l2 @a l1 @b\n"
  design "tolerance: a 5-decimal irrational pose misses the default tolerance, and the error says so"
    two5 (\m -> "position residual 3.9" `isInfixOf` m && "tolerance 1.0e-6 mm, 1.0e-8" `isInfixOf` m)
  rt <- load ("tolerance 0.00001\n" ++ two5)
  assertT t "tolerance: tolerance 0.00001 closes the same loop" (either (const False) (const True) rt) (either errorText (const "") rt)
  assertT t "tolerance: recorded, with the default rotation tolerance"
    (either (const False) ((== Tolerance 0.00001 1e-8) . arTolerance) rt) ""
  rt2 <- load ("tolerance 0.000001 0.001\n" ++ links 2 ++ "fixed l1\nmate l2 @a l1 @b roll=0.01\nmate l2 @a l1 @b\n")
  assertT t "tolerance: a rotation tolerance admits a small closing roll" (either (const False) (const True) rt2) (either errorText (const "") rt2)
  rd0 <- load (links 1 ++ "fixed l1\n")
  assertT t "tolerance: default when not declared" (either (const False) ((== defaultTolerance) . arTolerance) rd0) ""
  inputErr "tolerance must be positive" (links 2 ++ "tolerance 0\nfixed l1\nmate l2 @a l1 @b\n") [posAt 3 11, "position tolerance must be a finite distance in mm, greater than zero"]
  inputErr "fixed: an unknown binding is an undefined variable at its token" (links 2 ++ "fixed l1\nfixed l2 0 q 0 0 0 0\n") [posAt 4 12, "undefined variable 'q'"]
  inputErr "fixed: a non-finite expression is an error at its token" (links 2 ++ "fixed l1\nfixed l2 0 0 (1 / 0) 0 0 0\n") [posAt 4 14, "must be finite"]
  inputErr "fixed: a part name is not a number" (links 2 ++ "fixed l1\nfixed l2 l1 0 0 0 0 0\n") [posAt 4 10, "'l1' is a shape, not a number"]
  inputErr "fixed: arithmetic outside parentheses is refused with a fix" ("w = 1\n" ++ links 2 ++ "fixed l1\nfixed l2 w+1 0 0 0 0 0\n") [posAt 5 10, "write arithmetic in parentheses"]
  inputErr "mate: an unknown binding in roll= is an undefined variable at its token" (links 2 ++ "fixed l1\nmate l2 @a l1 @b roll=(r / 2)\n") [posAt 4 24, "undefined variable 'r'"]
  inputErr "mate: a value that is not a number" (links 2 ++ "fixed l1\nmate l2 @a l1 @b roll=90deg\n") [posAt 4 23, "'90deg' is not a number"]
  inputErr "mate: unknown options are still refused" (links 2 ++ "fixed l1\nmate l2 @a l1 @b spin=2\n") [posAt 4 18, "unknown mate option 'spin'"]
  inputErr "mate: duplicate options are still refused" (links 2 ++ "fixed l1\nmate l2 @a l1 @b gap=1 gap=2\n") [posAt 4 24, "duplicate mate option 'gap'"]
  -- frames of nested constraint assemblies, addressed through instance aliases (step 11)
  writeFile (dir </> "sub.assemble") "p ← link.coscad ×1\nq ← link.coscad ×1\nfixed p\nmate q @a p @b\n"
  let nested ref = "s ← sub.assemble ×1\nl ← link.coscad ×1\nfixed s\nmate l @a s " ++ ref ++ "\n"
      near3 (a, b, c) (x, y, z) = all (< 1e-9) [abs (a - x), abs (b - y), abs (c - z)]
      zAxis (MatingFrame _ m) = let (_, _, z) = matTranspose m in z
  inputErr "nested: an unqualified frame present in two instances is ambiguous and names the qualified choices"
    (nested "@b") [at 4, "ambiguous frame '@b': 2 copies", "qualify it with its instance: @p.b, @q.b"]
  rq <- load (nested "@q.b")
  case rq >>= maybe (Left (InputError "no asm")) Right . arAsm of
    Left e -> failT t "nested: @q.b selects one copy" (errorText e)
    Right asm -> case (frameOf "l.a" asm, frameOf "s.q.b" asm, frameOf "s.p.b" asm) of
      (Right la, Right qb, Right pb) -> do
        assertT t "nested: @q.b mates l to q's frame (origins meet, normals oppose)"
          (near3 (frameOrigin la) (frameOrigin qb) && near3 (zAxis la) (vneg (zAxis qb))) (show (la, qb))
        assertT t "nested: and not to p's" (not (near3 (frameOrigin la) (frameOrigin pb))) (show (la, pb))
      other -> failT t "nested: qualified frames of the solved assembly" (show other)
  inputErr "nested: an unknown qualified frame lists the instance aliases" (nested "@p.zz") [at 4, "unknown frame '@p.zz'", "qualify with an instance alias: p q"]
  writeFile (dir </> "nested.assemble") (nested "@q.b")
  let outer ref = "n ← nested.assemble ×1\nm ← link.coscad ×1\nfixed n\nmate m @a n " ++ ref ++ "\n"
  r2 <- load (outer "@s.q.b")
  assertT t "nested: a path through two levels (@s.q.b)" (either (const False) (const True) r2) (either errorText (const "") r2)
  r3 <- load (outer "@q.b")
  assertT t "nested: a unique partial path also resolves (@q.b from the grandparent)" (either (const False) (const True) r3) (either errorText (const "") r3)
  rpl <- load (nested "@q.b")
  case rpl of
    Left e -> failT t "nested: planner tags" (errorText e)
    Right ar -> assertT t "nested: planner substitution keeps one Tag per instance"
      (either (const False) (\tbl -> maybe False (\a -> length (filter (== "Tag \"l\"") (tagNames a)) == 1) (Map.lookup "asm" tbl))
        (resolveAssembly ar (Map.fromList [(n, Tag n sh) | (n, sh) <- arParts ar]))) ""

-- ------------------------------------------------------------------
-- variant names: one id and one artefact per (source, orientation)

-- | Every Tag in a shape, outermost first, rendered as `Tag "name"`.
tagNames :: Shape -> [String]
tagNames sh = case sh of
  Tag n x -> ("Tag " ++ show n) : tagNames x
  _ -> concatMap tagNames (shapeChildren sh)
