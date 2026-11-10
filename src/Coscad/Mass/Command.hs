-- | Read-only native geometry/component mass reports. No manufacturing or dispatch.
module Coscad.Mass.Command (massArgs, processMass) where

import Coscad.Backend (nativeCSGMesh)
import Coscad.Json
import Coscad.Mass
import Coscad.Mesh (meshVolumeCentre, triArea)
import Coscad.Parser (parseProgramFull)
import Coscad.Report
import Coscad.Validate (readFinite, finite)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Crypto.Hash.SHA256 as SHA
import Numeric (showHex)
import Control.Monad (unless)
import System.FilePath (takeExtension)

massArgs :: [String] -> Either String (FilePath, Maybe Double, Maybe FilePath)
massArgs = go Nothing Nothing Nothing
  where
    go (Just f) d o [] = Right (f,d,o)
    go Nothing _ _ [] = Left "mass needs a .coscad or component .json file"
    go f Nothing o ("--density":v:rest) = case readFinite v of
      Just d | d > 0 -> go f (Just d) o rest
      _ -> Left "mass: density must be positive finite g/cm3"
    go f d Nothing ("-o":v:rest) | not (null v) && take 1 v /= "-" = go f d (Just v) rest
    go Nothing d o (v:rest) | not (null v) && take 1 v /= "-" = go (Just v) d o rest
    go _ _ _ _ = Left "usage: coscad mass <part.coscad|components.json> [--density g/cm3] [-o report.json]"

processMass :: FilePath -> Maybe Double -> IO (Either Failure JValue)
processMass input density = runCommand $ do
  bytes <- BS.readFile input
  source <- either (const (abort (inputFailure "mass: input is not UTF-8"))) (pure . Text.unpack) (Text.decodeUtf8' bytes)
  let get = either (abort . inputFailure) pure
      hash = concatMap (\n -> let h = showHex n "" in if length h == 1 then '0':h else h) (BS.unpack (SHA.hash bytes))
  report <- case takeExtension input of
    ".json" -> do
      unless (density == Nothing) (abort (inputFailure "mass: --density applies only to solid .coscad geometry"))
      cs <- get (decode source >>= componentManifest)
      get (massReport cs)
    ".coscad" -> do
      (_,shape,_,_) <- get (parseProgramFull input source)
      tris <- nativeCSGMesh shape >>= get
      let (volume, centre) = meshVolumeCentre tris
          area = sum (map triArea tris)
      c <- maybe (abort (inputFailure "mass: geometry has no finite volume centre")) pure centre
      unless (finite volume && volume > 0 && finite area) (abort (inputFailure "mass: invalid solid volume or area"))
      case density of
        Just rho -> unless (finite rho && rho > 0 && finite (volume*rho/1000)) (abort (inputFailure "mass: invalid density or overflowing mass"))
        Nothing -> pure ()
      let (x,y,z) = c
      pure (JObject [("schema",JString "coscad.mass-report/1"),("basis",JString "uniform-solid-geometry")
        ,("volume_mm3",JNumber volume),("area_mm2",JNumber area),("center_mm",JArray (map JNumber [x,y,z]))
        ,("density_g_cm3",maybe JNull JNumber density),("mass_g",maybe JNull (JNumber . (volume/1000 *)) density)
        ,("limitations",JArray [JString "Uniform solid only: printed shells, infill and hardware are not inferred. Mass is null without explicit density."])])
    _ -> abort (inputFailure "mass: expected .coscad or component .json input")
  case report of
    JObject fields -> pure (JObject (fields ++ [("source",JString input),("source_sha256",JString hash)]))
    _ -> abort (inputFailure "mass: internal report error")
