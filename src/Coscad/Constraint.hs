-- | Deterministic rigid mating graph. Each connected component must have a
-- fixed root. Edges constrain all six degrees of freedom; loops are checked,
-- never silently relaxed. This is not a flexible-joint or collision solver.
module Coscad.Constraint (Constraint (..), solveConstraints) where

import Control.Monad (foldM, unless, when)
import Coscad.Dim (Dim (D3), dimOf)
import Coscad.Geometry
import Coscad.Shape
import Coscad.Validate (finite, maxQuantity, validateShape)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

data Constraint
  = Fixed String String V3 V3 -- location, instance, translation, XYZ degrees
  | Mate String String String String String D D -- location, child, frame, parent, frame, roll, gap
  deriving (Show)

type Pose = (M3, V3)

compose :: Pose -> Pose -> Pose
compose (a,t) (b,u) = (matMul a b, vadd t (mApply a u))

inverse :: Pose -> Pose
inverse (r,t) = let rt = matTranspose r in (rt, vneg (mApply rt t))

-- | Return each declared instance once, in declaration order. Supplied
-- shapes may be tagged or hidden: planner/checker substitutions retain the
-- exact same poses and construction metadata as the normal assembled view.
solveConstraints :: [(String, Shape)] -> [Constraint] -> Either String Shape
solveConstraints parts constraints = do
  when (null parts) (problem "assembly constraints need at least one part declaration")
  when (length parts > maxQuantity || length constraints > maxQuantity) (problem ("assembly constraints exceed the limit of " ++ show maxQuantity ++ " parts or constraints"))
  mapM_ (\(n,s) -> do
    d <- dimOf s
    unless (d == D3) (problem ("constrained instance '" ++ n ++ "' must be a 3D solid"))) parts
  edges <- mapM edge [c | c@Mate {} <- constraints]
  roots <- foldM fixed Map.empty [c | c@Fixed {} <- constraints]
  let adjacency = Map.fromListWith (++) (concat
        [[(parent, [(child, pose, loc)]), (child, [(parent, inverse pose, loc)])]
        | (parent, child, pose, loc) <- edges])
  poses <- walk adjacency roots (Seq.fromList (Map.keys roots))
  let floating = [n | (n,_) <- parts, Map.notMember n poses]
  unless (null floating) (problem ("underconstrained assembly: no fixed root reaches " ++ unwords floating ++ "; add fixed <instance> to each disconnected component or connect it with mate"))
  placed <- mapM (\(n,s) -> case Map.lookup n poses of
    Just (r,t) -> Right (Transform3 r t s)
    Nothing -> Left ("missing pose for '" ++ n ++ "'")) parts
  let assembled = Union placed
  at location (validateShape assembled)
  return assembled
  where
    table = Map.fromList parts
    location = case constraints of Fixed loc _ _ _ : _ -> loc; Mate loc _ _ _ _ _ _ : _ -> loc; [] -> "assembly"
    problem msg = Left (location ++ ": " ++ msg)
    at loc = either (Left . ((loc ++ ": ") ++)) Right
    part loc n = maybe (Left (loc ++ ": unknown constrained instance '" ++ n ++ "'; declare it with name ← part.coscad")) Right (Map.lookup n table)
    edge (Mate loc child cf parent pf roll gap) = at loc $ do
      when (child == parent) (Left "a mate needs two different part instances")
      unless (all finite [roll,gap]) (Left "mate roll and gap must be finite")
      cs <- part "mate" child
      ps <- part "mate" parent
      cframe <- at (child ++ " @" ++ cf) (frameOf cf cs)
      pframe <- at (parent ++ " @" ++ pf) (frameOf pf ps)
      return (parent, child, matingPose pframe cframe roll gap, loc ++ ": mate " ++ child ++ " @" ++ cf ++ " " ++ parent ++ " @" ++ pf)
    edge _ = Left "internal error: expected mate constraint"
    fixed poses (Fixed loc n t angles@(rx,ry,rz)) = at loc $ do
      _ <- part "fixed" n
      when (Map.member n poses) (Left ("duplicate fixed constraint for '" ++ n ++ "'; fix each instance once"))
      unless (all finite (vec t ++ vec angles)) (Left "fixed pose must be finite")
      let r = rodMat rz (0,0,1) `matMul` rodMat ry (0,1,0) `matMul` rodMat rx (1,0,0)
      return (Map.insert n (r,t) poses)
    fixed _ _ = Left "internal error: expected fixed constraint"
    walk adjacency poses queue = case Seq.viewl queue of
      Seq.EmptyL -> Right poses
      n Seq.:< rest -> case Map.lookup n poses of
        Nothing -> Left ("missing pose for '" ++ n ++ "'")
        Just pose -> do
          (next, added) <- foldM (propagate pose) (poses, Seq.empty) (Map.findWithDefault [] n adjacency)
          walk adjacency next (rest Seq.>< added)
    propagate parent (poses, queue) (child, relative, loc) = do
      let expected@(r,t) = compose parent relative
      case Map.lookup child poses of
        Nothing -> return (Map.insert child expected poses, queue Seq.|> child)
        Just (actualR,actualT) -> do
          let distance = maximum (map abs (vec (vsub actualT t)))
              rotation = maximum (zipWith (\a b -> abs (a-b)) (entries actualR) (entries r))
          unless (finite distance && finite rotation && distance <= 1e-6 && rotation <= 1e-8) $
            Left (loc ++ ": conflicting assembly constraints at '" ++ child ++ "' (position residual " ++ show distance ++ " mm, orientation residual " ++ show rotation ++ "); check fixed poses, mating frames, roll and gap")
          return (poses,queue)
    vec (a,b,c) = [a,b,c]
    entries (a,b,c) = concatMap vec [a,b,c]
