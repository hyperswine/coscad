-- | Mass moments in mm and grams. Mesh centres assume uniform density;
-- they do not describe the distribution of infill or shell toolpaths.
module Coscad.Mass
  ( Component(..), combineMass, componentManifest, massReport ) where

import Control.Monad (unless)
import Data.List (nub)
import Coscad.Json
import Coscad.Linalg (V3, vadd, scale3)
import Coscad.Validate (finite)

data Component = Component
  { componentName :: String, componentKind :: String
  , componentMass :: Double, componentCentre :: V3
  } deriving (Eq, Show)

combineMass :: [Component] -> Either String (Double, V3)
combineMass cs = do
  unless (not (null cs)) (Left "mass: no components")
  let names = map componentName cs
  unless (all (not . null) names && length names == length (nub names))
    (Left "mass: component names must be nonempty and unique")
  mapM_ valid cs
  let total = sum (map componentMass cs)
      ref = case cs of c:_ -> componentCentre c; [] -> (0,0,0)
      sub (a,b,c) (x,y,z) = (a-x,b-y,c-z)
      moment = foldr vadd (0,0,0) [scale3 (componentMass c) (sub (componentCentre c) ref) | c <- cs]
      centre = vadd ref (scale3 (1/total) moment)
  unless (finite total && all finite (xyz centre)) (Left "mass: moments overflow")
  pure (total, centre)
  where
    valid c = do
      unless (componentKind c `elem` ["solid", "sliced", "measured", "hardware"])
        (Left "mass: kind must be solid, sliced, measured or hardware")
      unless (finite (componentMass c) && componentMass c > 0 && all finite (xyz (componentCentre c)))
        (Left "mass: masses must be positive finite grams and centres finite mm")

xyz :: V3 -> [Double]
xyz (x,y,z) = [x,y,z]

componentManifest :: JValue -> Either String [Component]
componentManifest root = do
  schema <- str =<< field "schema" root
  unless (schema == "coscad.mass-components/1") (Left "mass: expected coscad.mass-components/1")
  rows <- case lookupKey "components" root of
    Just (JArray xs) -> Right xs
    _ -> Left "mass: expected components array"
  cs <- mapM row rows
  _ <- combineMass cs
  pure cs
  where
    row v = do
      name <- str =<< field "name" v
      kind <- str =<< field "kind" v
      mass <- num =<< field "mass_g" v
      centre <- field "center_mm" v >>= \q -> case q of
        JArray [x,y,z] -> (,,) <$> num x <*> num y <*> num z
        _ -> Left "mass: center_mm must contain three numbers"
      pure (Component name kind mass centre)
    field k v = maybe (Left ("mass: missing " ++ k)) Right (lookupKey k v)
    str (JString x) = Right x
    str _ = Left "mass: expected string"
    num (JNumber x) | finite x = Right x
    num _ = Left "mass: expected finite number"

massReport :: [Component] -> Either String JValue
massReport cs = do
  (total, centre) <- combineMass cs
  pure (JObject
    [("schema",JString "coscad.mass-report/1"), ("basis",JString "explicit-components"), ("mass_g",JNumber total)
    ,("center_mm",JArray (map JNumber (xyz centre)))
    ,("components",JArray [JObject [("name",JString (componentName c)),("kind",JString (componentKind c))
        ,("mass_g",JNumber (componentMass c)),("center_mm",JArray (map JNumber (xyz (componentCentre c))))] | c <- cs])
    ,("limitations",JArray [JString "All centres must use the same assembly frame; no implicit fasteners or density assumptions.",
        JString "Measured mass alone does not establish a measured centre of mass; no inertia or strength verdict."])])
