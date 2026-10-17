-- | Deterministic rigid mating graph. Each connected component must have a
-- fixed root. Edges constrain all six degrees of freedom; loops are checked,
-- never silently relaxed. This is not a flexible-joint or collision solver.
module Coscad.Constraint
  ( Constraint (..)
  , AsmError (..)
  , errorText
  , solveConstraints
  ) where

import Control.Monad (foldM, unless, when)
import Coscad.Dim (Dim (D3), dimOf)
import Coscad.Geometry
import Coscad.Shape
import Coscad.Validate (finite, maxQuantity, validateShape)
import Data.List (intercalate, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

data Constraint
  = Fixed String String V3 V3 -- location, instance, translation, XYZ degrees
  | Mate String String String String String D D -- location, child, frame, parent, frame, roll, gap
  deriving (Show)

-- | Why an assembly could not be loaded. 'InputError' means the file could
-- not be read as written (syntax, unknown names or frames, limits) and
-- maps to exit 1; 'DesignError' means it was read but describes something
-- that cannot be built (a constraint loop that does not close, a part no
-- fixed root reaches, an instance fixed twice) and maps to exit 2.
data AsmError = InputError String | DesignError String
  deriving (Eq, Show)

errorText :: AsmError -> String
errorText (InputError e) = e
errorText (DesignError e) = e

type Pose = (M3, V3)

compose :: Pose -> Pose -> Pose
compose (a,t) (b,u) = (matMul a b, vadd t (mApply a u))

inverse :: Pose -> Pose
inverse (r,t) = let rt = matTranspose r in (rt, vneg (mApply rt t))

-- | A constraint as the solver reports it: its index in declaration order,
-- its source location, and how it reads in the file.
data Joint = Joint {jIndex :: Int, jLoc :: String, jText :: String}

-- | Return each declared instance once, in declaration order. Parts are
-- (name, declaration location, shape); the location is where floating
-- parts are reported. Supplied shapes may be tagged or hidden:
-- planner/checker substitutions retain the exact same poses and
-- construction metadata as the normal assembled view.
solveConstraints :: [(String, String, Shape)] -> [Constraint] -> Either AsmError Shape
solveConstraints parts constraints = do
  when (null parts) (input firstLoc "assembly constraints need at least one part declaration")
  when (length parts > maxQuantity || length constraints > maxQuantity) (input firstLoc ("assembly constraints exceed the limit of " ++ show maxQuantity ++ " parts or constraints"))
  mapM_ (\(n,loc,s) -> do
    d <- either (input loc) Right (dimOf s)
    unless (d == D3) (input loc ("constrained instance '" ++ n ++ "' must be a 3D solid"))) parts
  edges <- mapM edge [(i, c) | (i, c@Mate {}) <- indexed]
  roots <- foldM fixed Map.empty [(i, c) | (i, c@Fixed {}) <- indexed]
  let adjacency = Map.fromListWith (flip (++)) (concat
        [[(parent, [(child, pose, j)]), (child, [(parent, inverse pose, j)])]
        | (parent, child, pose, j) <- edges])
      rootJoint = Map.map snd roots
  poses <- walk adjacency rootJoint (Map.map fst roots) Map.empty (Seq.fromList (Map.keys roots))
  case [(n, loc) | (n,loc,_) <- parts, Map.notMember n poses] of
    [] -> return ()
    floating@((_, loc) : _) -> Left (DesignError (intercalate "\n" (
      (loc ++ ": underconstrained assembly: no fixed root reaches " ++ unwords (map fst floating) ++ "; add fixed <instance> to each disconnected component or connect it with mate")
      : ["  note: " ++ l ++ ": '" ++ n ++ "' is declared here and is not placed" | (n, l) <- drop 1 floating])))
  placed <- mapM (\(n,_,s) -> case Map.lookup n poses of
    Just (r,t) -> Right (Transform3 r t s)
    Nothing -> input firstLoc ("missing pose for '" ++ n ++ "'")) parts
  let assembled = Union placed
  either (input firstLoc) Right (validateShape assembled)
  return assembled
  where
    indexed = zip [0 :: Int ..] constraints
    table = Map.fromList [(n, s) | (n,_,s) <- parts]
    firstLoc = case (constraints, parts) of
      (Fixed loc _ _ _ : _, _) -> loc
      (Mate loc _ _ _ _ _ _ : _, _) -> loc
      ([], (_, loc, _) : _) -> loc
      _ -> "assembly"
    input loc msg = Left (InputError (loc ++ ": " ++ msg))
    at loc = either (input loc) Right
    part loc n = maybe (input loc ("unknown constrained instance '" ++ n ++ "'; declare it with name ← part.coscad")) Right (Map.lookup n table)
    edge (i, Mate loc child cf parent pf roll gap) = do
      when (child == parent) (input loc "a mate needs two different part instances")
      unless (all finite [roll,gap]) (input loc "mate roll and gap must be finite")
      cs <- part loc child
      ps <- part loc parent
      cframe <- at loc (either (Left . ((child ++ " @" ++ cf ++ ": ") ++)) Right (frameOf cf cs))
      pframe <- at loc (either (Left . ((parent ++ " @" ++ pf ++ ": ") ++)) Right (frameOf pf ps))
      return (parent, child, matingPose pframe cframe roll gap, Joint i loc ("mate " ++ child ++ " @" ++ cf ++ " " ++ parent ++ " @" ++ pf))
    edge (_, c) = Left (InputError ("internal error: expected mate constraint, got " ++ show c))
    fixed roots (i, Fixed loc n t angles@(rx,ry,rz)) = do
      _ <- part loc n
      unless (all finite (vec t ++ vec angles)) (input loc "fixed pose must be finite")
      case Map.lookup n roots of
        Just (_, first) -> Left (DesignError (intercalate "\n"
          [ loc ++ ": duplicate fixed constraint for '" ++ n ++ "'; fix each instance once"
          , "  note: " ++ jLoc first ++ ": " ++ jText first ++ " (first fixed here)" ]))
        Nothing -> do
          let r = rodMat rz (0,0,1) `matMul` rodMat ry (0,1,0) `matMul` rodMat rx (1,0,0)
          return (Map.insert n ((r,t), Joint i loc ("fixed " ++ n)) roots)
    fixed _ (_, c) = Left (InputError ("internal error: expected fixed constraint, got " ++ show c))
    -- Breadth-first from every fixed root. `via` records the joint each
    -- instance was reached through, so a loop that fails to close can be
    -- reported as the whole loop rather than as the edge being checked.
    walk adjacency rootJoint poses via queue = case Seq.viewl queue of
      Seq.EmptyL -> Right poses
      n Seq.:< rest -> case Map.lookup n poses of
        Nothing -> input firstLoc ("missing pose for '" ++ n ++ "'")
        Just pose -> do
          (poses', via', added) <- foldM (propagate rootJoint n pose) (poses, via, Seq.empty) (Map.findWithDefault [] n adjacency)
          walk adjacency rootJoint poses' via' (rest Seq.>< added)
    propagate rootJoint from parent (poses, via, queue) (child, relative, j) = do
      let expected@(r,t) = compose parent relative
      case Map.lookup child poses of
        Nothing -> return (Map.insert child expected poses, Map.insert child (from, j) via, queue Seq.|> child)
        Just (actualR,actualT)
          | Just (back, bj) <- Map.lookup from via, back == child, jIndex bj == jIndex j -> return (poses, via, queue)
          | otherwise -> do
              let distance = maximum (map abs (vec (vsub actualT t)))
                  rotation = maximum (zipWith (\a b -> abs (a-b)) (entries actualR) (entries r))
              unless (finite distance && finite rotation && distance <= 1e-6 && rotation <= 1e-8) $
                Left (DesignError (conflict rootJoint via from child j distance rotation))
              return (poses, via, queue)
    -- The loop is the closing joint plus both tree paths up to their
    -- common ancestor; when the paths end at different fixed roots, the two
    -- `fixed` lines are part of it. It is blamed on its last-declared
    -- member, the line that closed it, and every member is listed.
    conflict rootJoint via from child j distance rotation =
      let chain n = case Map.lookup n via of
            Just (p, pj) -> (n, Just pj) : chain p
            Nothing -> [(n, Nothing)]
          up = chain from
          down = chain child
          common = [n | (n, _) <- up, n `elem` map fst down]
          joints = case common of
            lca : _ ->
              [pj | (_, Just pj) <- takeWhile ((/= lca) . fst) up] ++ [pj | (_, Just pj) <- takeWhile ((/= lca) . fst) down]
            [] ->
              [pj | (_, Just pj) <- up] ++ [pj | (_, Just pj) <- down]
                ++ [rj | (n, Nothing) <- [last up, last down], Just rj <- [Map.lookup n rootJoint]]
          members = sortOn jIndex (j : joints)
          blamed = last members
          shape = if null common then "the chain between fixed instances" else "the loop"
          names = case common of
            lca : _ -> lca : reverse (map fst (takeWhile ((/= lca) . fst) up)) ++ map fst (takeWhile ((/= lca) . fst) down)
            [] -> map fst (reverse up) ++ map fst down
       in intercalate "\n"
            ( (jLoc blamed ++ ": conflicting assembly constraints: " ++ shape ++ " " ++ unwords names ++ " does not close at '" ++ child
                ++ "' (position residual " ++ show distance ++ " mm, orientation residual " ++ show rotation
                ++ "); check fixed poses, mating frames, roll and gap. Any of its " ++ show (length members) ++ " constraints may be the wrong one:")
              : ["  note: " ++ jLoc m ++ ": " ++ jText m | m <- members] )
    vec (a,b,c) = [a,b,c]
    entries (a,b,c) = concatMap vec [a,b,c]
