module Tier.Bonded (bondedTier) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Coscad.Json (encode, decode)
import Coscad.Linalg (V3, vadd, vsub, vlen, cross)
import Coscad.Stress.Bonded
import Coscad.Stress.Hex20
import Coscad.Stress.Voxel
import Harness

bondedTier :: T -> IO ()
bondedTier t = do
  section "bonded assembly: split beam, interface equilibrium and distinct materials"
  source <- readFile "experiments/bonded/split-cantilever.json"
  case parseBondedProblem source of
    Left err -> failT t "bonded fixture parses" err
    Right p -> do
      case solveBonded p of
        Left err -> failT t "split cantilever solves" err
        Right r -> do
          let a = head (bpBodies p); g = bodyGrid a
              cells = Set.fromList [(i,j,k) | i <- [0..9], j <- [0..1], k <- [0..1]]
              whole = g {vgCounts=(10,2,2),vgCells=cells}
              fixed = Set.fromList [k | f <- exteriorFaces whole, faceAxis f==0, faceSign f == -1, (k,_) <- faceNodes f]
              end = [f | f <- exteriorFaces whole, faceAxis f==0, faceSign f==1]
              loads = Map.fromListWith vadd [(k,(0,0,-w/4)) | f <- end, (k,w) <- faceNodes f]
              single = Hex20Problem whole 2000 0.3 fixed loads (bpTolerance p) (bpIterations p)
          case solveHex20 single of
            Left err -> failT t "single beam reference solves" err
            Right reference -> do
              let refNodes = Map.fromList [(x,u) | (_,x,u) <- hrNodes reference]
                  errors = [vlen (vsub u (refNodes Map.! x)) | b <- baBodies r, (_,x,u) <- brNodes b]
                  refCells = Map.fromList [(x,csCentre s) | (_,x,s) <- hrCells reference]
                  stressError = maximum [maximum (zipWith (\x y -> abs (x-y)) (csCentre s) (refCells Map.! x)) | b <- baBodies r, (_,x,s) <- brCells b]
              assertT t "split displacement matches all monolithic nodes" (maximum errors < 1e-9) (show (maximum errors))
              assertT t "split stress matches monolithic cells" (stressError < 1e-7) (show stressError)
              assertT t "split strain energy matches monolithic" (abs (baEnergy r / hrEnergy reference - 1) < 1e-8) (show (baEnergy r,hrEnergy reference))
              assertT t "same reduced degrees of freedom" (baDofs r == hrDofs reference) (show (baDofs r,hrDofs reference))
              closeV t "split support force matches" 1e-8 (wrenchForce (baSupport r)) (hrReaction reference)
              closeV t "split support moment matches" 1e-7 (wrenchMoment (baSupport r)) (hrMoment reference)
          balanced t "global" 1e-7 (baSupport r) (baApplied r)
          let joint = head (baInterfaces r)
          balanced t "interface" 1e-7 (irOnA joint) (irOnB joint)
          closeV t "force transferred to tip" 1e-7 (wrenchForce (irOnB joint)) (0,0,1)
          closeV t "moment transferred to tip about world origin" 1e-7 (wrenchMoment (irOnB joint)) (2,-20,0)
          assertT t "interface patch area and nodes" (irArea joint==16 && irNodes joint==21) (show joint)
          assertT t "body energy sums to assembly" (abs (sum (map brEnergy (baBodies r)) - baEnergy r) < 1e-12) ""
          assertT t "accepted true residual" (baResidual r <= bpTolerance p) (show (baResidual r))
          assertT t "report JSON round trips" (decode (encode (bondedJson p r)) == Right (bondedJson p r)) ""
          -- Translation tests catch frame/origin mistakes in interface moments.
          let shift=(3,-5,7)
              moved b = b {bodyGrid=(bodyGrid b) {vgOrigin=vadd shift (vgOrigin (bodyGrid b))}}
          case solveBonded p {bpBodies=map moved (bpBodies p)} of
            Left err -> failT t "translated assembly" err
            Right movedResult -> do
              closeV t "translation preserves interface force" 1e-8 (wrenchForce (irOnB (head (baInterfaces movedResult)))) (wrenchForce (irOnB joint))
              closeV t "moment translates by r cross F" 1e-7 (wrenchMoment (irOnB (head (baInterfaces movedResult)))) (vadd (wrenchMoment (irOnB joint)) (cross shift (wrenchForce (irOnB joint))))
          case solveBonded p {bpBodies=reverse (bpBodies p)} of
            Left err -> failT t "body order independent" err
            Right reversed -> assertT t "body order preserves energy" (abs (baEnergy reversed / baEnergy r - 1) < 1e-8) ""
      -- External traction at the tie must be subtracted from body residuals,
      -- otherwise the two recovered interface forces fail action/reaction.
      let atTie = bondB (head (bpBonds p))
      case solveBonded p {bpLoads=bpLoads p ++ [(atTie,(0,0,-1))]} of
        Left err -> failT t "load on tied patch" err
        Right r -> do
          let joint = head (baInterfaces r)
          balanced t "loaded interface" 1e-7 (irOnA joint) (irOnB joint)
          balanced t "loaded interface global" 1e-7 (baSupport r) (baApplied r)
          closeV t "loaded tip receives two newtons" 1e-7 (wrenchForce (irOnB joint)) (0,0,2)
      failureCases t p
  barText <- readFile "experiments/bonded/two-material-bar.json"
  case parseBondedProblem barText >>= solveBonded of
    Left err -> failT t "two-material axial bar" err
    Right r -> do
      let tip = last (baBodies r)
          tipU = [x | (_, (px,_,_), (x,_,_)) <- brNodes tip, px==20]
          expected = 10/(2000*16) + 10/(1000*16)
      assertT t "two-material bar follows series compliance" (all (\u -> abs (u/expected-1)<1e-8) tipU && not (null tipU)) (show (tipU,expected))
      assertT t "bar energy equals half force times displacement" (abs (baEnergy r/(expected/2)-1)<1e-8) (show (baEnergy r))
      balanced t "two-material interface" 1e-8 (irOnA (head (baInterfaces r))) (irOnB (head (baInterfaces r)))
      forM_ (baBodies r) $ \b -> assertT t (brName b ++ " axial stress is force/area")
        (all (\(_,_,s) -> abs (head (csCentre s)-1/16) < 1e-8) (brCells b)) ""

  shelfTier t

failureCases :: T -> BondedProblem -> IO ()
failureCases t p = do
  section "bonded assembly: unsupported inputs fail before solving"
  let joint = head (bpBonds p); a=bondA joint; b=bondB joint
      modified j = p {bpBonds=[j]}
      err name problem parts = expectErr t name (solveBonded problem) parts []
      changeTip f = p {bpBodies=[head (bpBodies p), f (last (bpBodies p))]}
  err "untied tip is floating, even when coincident" p {bpBonds=[]} ["floating","tip"]
  err "no support" p {bpSupports=[]} ["floating"]
  err "empty interface" (modified joint {bondA=a {patchFaces=[]}}) ["empty"]
  err "partial mismatch" (modified joint {bondB=b {patchFaces=take 1 (patchFaces b)}}) ["mismatched"]
  err "gap is not silently tied" (changeTip (\x -> x {bodyGrid=(bodyGrid x) {vgOrigin=(12,0,0)}})) ["mismatched"]
  err "nonmatching grid" (changeTip (\x -> x {bodyGrid=(bodyGrid x) {vgCell=1}})) ["same cell size"]
  err "misaligned origin" (changeTip (\x -> x {bodyGrid=(bodyGrid x) {vgOrigin=(10.1,0,0)}})) ["aligned"]
  err "overlap" (changeTip (\x -> x {bodyGrid=(bodyGrid x) {vgOrigin=(8,0,0)}})) ["overlap"]
  err "invalid face" (modified joint {bondA=a {patchFaces=[Face (0,0,0) 3 1]}}) ["invalid face"]
  err "interior face" (modified joint {bondA=a {patchFaces=[Face (1,0,0) 0 1]}}) ["non-exterior"]
  err "duplicate face" (modified joint {bondA=a {patchFaces=head (patchFaces a):patchFaces a}}) ["duplicate faces"]
  err "unknown body" (modified joint {bondA=a {patchBody="missing"}}) ["unknown body"]
  err "duplicate body identity" p {bpBodies=replicate 2 (head (bpBodies p))} ["unique"]
  err "duplicate joint" p {bpBonds=[joint,joint]} ["unique"]
  err "shared joint nodes" p {bpBonds=[joint,joint {bondName="other"}]} ["shared interface"]
  err "support on joint" p {bpSupports=[a]} ["attribution"]
  err "partial cells" (changeTip (\x -> x {bodyGrid=(bodyGrid x) {vgFill=Map.singleton (0,0,0) 0.5}})) ["fully filled"]
  err "invalid material" (changeTip (\x -> x {bodyYoung=0})) ["modulus"]
  err "nonfinite load" p {bpLoads=[(fst (head (bpLoads p)),(0/0,0,0))]} ["finite"]
  err "no load" p {bpLoads=[]} ["free load"]
  err "loads cancel" p {bpLoads=[(fst (head (bpLoads p)),(0,0,1)),head (bpLoads p)]} ["free load"]
  err "iteration failure" p {bpIterations=1} ["did not converge"]
  err "invalid tolerance" p {bpTolerance=0/0} ["tolerance"]
  expectErr t "JSON rejects unknown fields" (parseBondedProblem "{\"schema\":\"coscad.bonded-problem/1\",\"typo\":1}") ["unknown"] []
  expectErr t "JSON rejects duplicate fields" (parseBondedProblem "{\"schema\":\"a\",\"schema\":\"b\"}") ["duplicate"] []
  -- Unconnected, separately supported bodies remain independent; touching
  -- geometry is not bonded by proximity.
  let root= head (bpBodies p); tip=last (bpBodies p)
      supportTip = Patch "tip" [f | f <- exteriorFaces (bodyGrid tip),faceAxis f==0,faceSign f == -1]
      loadRoot = (Patch "root" (patchFaces a),(0,0,-1))
      separate=p {bpBonds=[],bpSupports=bpSupports p ++ [supportTip],bpLoads=[loadRoot,head (bpLoads p)]}
  case solveBonded separate of
    Left e -> failT t "separately supported touching bodies" e
    Right r -> do
      assertT t "no implicit ties" (null (baInterfaces r) && baDofs r==3*(Set.size (allNodes (bodyGrid root))+Set.size (allNodes (bodyGrid tip)))) ""
      forM_ (baBodies r) $ \body -> closeV t (brName body ++ " carries only its own load") 1e-7 (wrenchForce (brSupport body)) (0,0,1)

closeV :: T -> String -> Double -> V3 -> V3 -> IO ()
closeV t name tol a b = assertT t name (vlen (vsub a b)<tol) (show (a,b))
balanced :: T -> String -> Double -> Wrench -> Wrench -> IO ()
balanced t name tol a b = do
  closeV t (name ++ " force equilibrium") tol (vadd (wrenchForce a) (wrenchForce b)) (0,0,0)
  closeV t (name ++ " moment equilibrium") tol (vadd (wrenchMoment a) (wrenchMoment b)) (0,0,0)

-- Two separate supports make load sharing depend on component stiffness.
-- These coarse fixtures run in the ordinary suite without Python; the optional
-- reference script checks three refinements against independent elements.
shelfTier :: T -> IO ()
shelfTier t = do
  section "bonded shelf: symmetry and stiffness-dependent load sharing"
  symmetricText <- readFile "experiments/bonded/shelf-symmetric.json"
  softText <- readFile "experiments/bonded/shelf-soft-right.json"
  case do
    p <- parseBondedProblem symmetricText
    q <- parseBondedProblem softText
    a <- solveBonded p
    b <- solveBonded q
    return (p,q,a,b) of
    Left err -> failT t "shelf fixtures solve" err
    Right (p,q,a,b) -> do
      let reaction name r = [wrenchForce (brSupport x) | x <- baBodies r, brName x==name]
          vertical name r = sum [z | (_,_,z) <- reaction name r]
          probe r = [u | body <- baBodies r, brName body=="shelf", (_,x,u) <- brNodes body, x==(6,6,4)]
          magnitude r = maximum [vlen u | u <- probe r]
      forM_ [("symmetric",a),("soft-right",b)] $ \(label,r) -> do
        assertT t (label ++ " has three bodies and two interfaces") (length (baBodies r)==3 && length (baInterfaces r)==2) ""
        balanced t (label ++ " global") 1e-7 (baSupport r) (baApplied r)
        forM_ (baInterfaces r) $ \i -> balanced t (label ++ " " ++ irName i) 1e-7 (irOnA i) (irOnB i)
        assertT t (label ++ " has centre-front probe") (length (probe r)==1) ""
      assertT t "symmetric arms each carry half the vertical force"
        (abs (vertical "left" a-0.5)<1e-8 && abs (vertical "right" a-0.5)<1e-8) (show (reaction "left" a,reaction "right" a))
      assertT t "softening right shifts load to left"
        (vertical "left" b > vertical "left" a + 1e-3 && vertical "right" b < vertical "right" a - 1e-3) (show (reaction "left" b,reaction "right" b))
      assertT t "softening increases displacement and energy"
        (magnitude b > magnitude a && baEnergy b > baEnergy a) ""
      -- Mirror the material assignment while keeping symmetric geometry/load.
      let mirrored = q {bpBodies=[body {bodyYoung=if bodyName body=="left" then 500 else 2000} | body <- bpBodies q]}
      case solveBonded mirrored of
        Left err -> failT t "mirrored stiffness shelf" err
        Right r -> do
          assertT t "mirroring stiffness swaps vertical reactions"
            (abs (vertical "left" r - vertical "right" b)<1e-8 && abs (vertical "right" r - vertical "left" b)<1e-8) ""
          assertT t "mirroring preserves energy" (abs (baEnergy r/baEnergy b-1)<1e-8) ""
      case solveBonded p {bpLoads=[(patch,(2*x,2*y,2*z)) | (patch,(x,y,z)) <- bpLoads p]} of
        Left err -> failT t "shelf doubled load" err
        Right r -> do
          assertT t "linear displacement and quadratic energy scaling"
            (abs (magnitude r/magnitude a-2)<1e-8 && abs (baEnergy r/baEnergy a-4)<1e-8) ""
          assertT t "doubled load doubles reaction" (abs (vertical "left" r-1)<1e-8) ""
