-- | Analysis directives a part file may carry next to its geometry:
--
-- > support bot                 all exterior faces on the part's bottom plane
-- > support @foot bot           the faces with that normal in the plane through datum foot
-- > load top 0 0 -20            20 N total, spread over the faces on the top plane
-- > load @tip 0 0 -20           20 N at the surface node nearest datum tip
-- > load @tip top 0 0 -20       spread over the +z faces coplanar with datum tip
-- > stress material=pla safety=2 cell=1
--
-- They are data only; the OpenSCAD and native geometry paths ignore them.
module Coscad.Stress.Spec
  ( Where (..), Analysis (..), emptyAnalysis, faceAnchor, faceAnchorName, describeWhere ) where

import Coscad.Linalg (V3)

-- | Where a support holds or a load acts.
data Where
  = AtBox V3                -- ^ the part's bounding-box face with this outward normal
  | AtDatumFace String V3   -- ^ exterior faces with this normal in the plane through a datum
  | AtDatum String          -- ^ the surface node nearest a datum
  deriving (Eq, Show)

data Analysis = Analysis
  { aSupports :: [Where]
  , aLoads :: [(Where, V3)]          -- ^ total force in N
  , aSettings :: [(String, String)]  -- ^ key=value pairs from `stress` lines, in order
  } deriving (Eq, Show)

emptyAnalysis :: Analysis
emptyAnalysis = Analysis [] [] []

-- | A single bounding-box face anchor; edges and corners are not faces.
faceAnchor :: String -> Maybe V3
faceAnchor name = lookup name
  [ ("top",(0,0,1)), ("bot",(0,0,-1)), ("rt",(1,0,0)), ("lft",(-1,0,0)), ("bak",(0,1,0)), ("fwd",(0,-1,0)) ]

faceAnchorName :: V3 -> String
faceAnchorName v = case [n | (n,u) <- [("top",(0,0,1)), ("bot",(0,0,-1)), ("rt",(1,0,0)), ("lft",(-1,0,0)), ("bak",(0,1,0)), ("fwd",(0,-1,0))], u == v] of
  (n:_) -> n
  [] -> show v

describeWhere :: Where -> String
describeWhere w = case w of
  AtBox v -> faceAnchorName v
  AtDatumFace d v -> "@" ++ d ++ " " ++ faceAnchorName v
  AtDatum d -> "@" ++ d
