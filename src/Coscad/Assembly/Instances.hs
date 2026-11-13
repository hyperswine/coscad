-- | Immutable dependency snapshot and qualified physical leaf placements.
module Coscad.Assembly.Instances
  ( ResolvedInstance(..), InstanceSnapshot(..), loadInstances, instancesJson, printToAssembly, protectInputs ) where
import Coscad.Assemble
import Coscad.Instance
import Coscad.Geometry
import Coscad.Shape
import Coscad.Json
import Coscad.Report
import Coscad.Validate (maxQuantity, finite)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Crypto.Hash.SHA256 as SHA
import Data.IORef
import Data.List (intercalate)
import Control.Monad (unless)
import System.Directory (canonicalizePath, doesFileExist)
import Numeric (showHex)

data ResolvedInstance = ResolvedInstance
  { riId :: String, riSource :: FilePath, riShape :: Shape, riTransform :: Aff
  , riHints :: [(String,String)], riCount :: Int, riAssemblyOnly :: Bool
  , riPrintTransform :: Maybe Aff
  }
data InstanceSnapshot = InstanceSnapshot
  { isSource :: FilePath, isInstances :: [ResolvedInstance], isSources :: [(FilePath,String)] }

loadInstances :: Emit -> FilePath -> IO (Either Failure InstanceSnapshot)
loadInstances emit input = runCommand $ do
  cache <- newIORef Map.empty
  let reader file = do
        key <- canonicalizePath file
        known <- readIORef cache
        case Map.lookup key known of
          Just (src,_) -> pure (Just src)
          Nothing -> do
            exists <- doesFileExist key
            if not exists then pure Nothing else do
              bytes <- BS.readFile key
              src <- either (const (abort (inputFailure (key ++ ": invalid UTF-8")))) (pure . Text.unpack) (Text.decodeUtf8' bytes)
              let hash = concatMap (\n -> let h = showHex n "" in if length h == 1 then '0':h else h) (BS.unpack (SHA.hash bytes))
              modifyIORef' cache (Map.insert key (src,hash))
              pure (Just src)
  ar <- loadAssemblyWith reader emit [] input >>= either (abort . asmFailure) pure
  leaves <- either (abort . inputFailure) pure (flatten [] (matIdentity,(0,0,0)) [] False ar)
  unless (not (null leaves) && length leaves <= maxQuantity) (abort (inputFailure "instances: empty assembly or too many physical leaves"))
  canonical <- mapM (\r -> do path <- canonicalizePath (riSource r); pure r {riSource=path}) leaves
  sources <- map (\(p,(_,hash)) -> (p,hash)) . Map.toAscList <$> readIORef cache
  pure (InstanceSnapshot input canonical sources)

flatten :: [String] -> Aff -> [(String,String)] -> Bool -> AsmResult -> Either String [ResolvedInstance]
flatten trail parent inherited parentOnly ar = do
  table <- resolveAssembly ar (Map.fromList [(n,Tag n sh) | (n,sh) <- arParts ar])
  assembly <- maybe (Left "instances: assembly has no placed asm or constraints") Right (Map.lookup "asm" table)
  placements <- physicalInstances (resolve assembly)
  batches <- mapM leaf (numberedInstances placements)
  let result = concat batches
  unless (length result <= maxQuantity) (Left "instances: too many expanded physical leaves")
  pure result
  where
    leaf (name,alias,pose,shape) = do
      let path = trail ++ [name]; world = affCompose parent pose
          (count,hints) = maybe (1,[]) id (lookup alias (arPartMeta ar))
          combined = Map.toAscList (Map.fromList (hints ++ inherited))
          only = parentOnly || count == 0
      let (matrix,offset) = world
          (a,b,c) = matrix
          coords (x,y,z) = [x,y,z]
      unless (all finite (concatMap coords [a,b,c,offset])) (Left "instances: nonfinite composed transform")
      case lookup alias (arChildren ar) of
        Just child -> flatten path world combined only child
        Nothing -> do
          (source,down) <- case [(p,d) | (n,p,d) <- arReferences ar, n==alias] of
            [x] -> Right x
            _ -> Left ("instances: missing reference metadata for " ++ alias)
          let printPose = case down of
                Nothing -> Nothing
                Just (normal,_) ->
                  let m = downMat normal
                      (lo,_) = bbox (Transform3 m (0,0,0) shape)
                  in Just (m,vneg lo)
          pure [ResolvedInstance (intercalate "/" path) source shape world combined count only printPose]

protectInputs :: [FilePath] -> FilePath -> IO ()
protectInputs inputs output = do
  target <- canonicalizePath output
  paths <- mapM canonicalizePath inputs
  unless (target `notElem` paths) (abort (inputFailure "output must not overwrite any input or assembly dependency"))

printToAssembly :: ResolvedInstance -> Maybe Aff
printToAssembly r = fmap (affCompose (riTransform r) . inverse) (riPrintTransform r)
  where inverse (m,t) = let rot=matTranspose m in (rot,vneg (mApply rot t))

instancesJson :: InstanceSnapshot -> JValue
instancesJson snap = JObject
  [("schema",JString "coscad.instances/1"),("source",JString (isSource snap))
  ,("units",JString "mm"),("matrix_convention",JString "row-major 4x4 multiplying column [x,y,z,1]")
  ,("sources",JArray [JObject [("path",JString p),("sha256",JString h)] | (p,h) <- isSources snap])
  ,("instances",JArray (map row (isInstances snap)))
  ,("limitations",JArray (map JString ["IDs follow alias paths and occurrence order; editing placement order can renumber repeated aliases.",
      "Print count is a manufacturing hint, not physical multiplicity. No implicit hardware, supports or mechanical bonds.",
      "Print transforms exist only for explicitly declared leaf print orientation; automatic packing/slicer placement is not inferred."]))]
  where
    vec (x,y,z) = JArray (map JNumber [x,y,z])
    matrix (m,t) = let ((a,b,c),(d,e,f),(g,h,i))=m; (x,y,z)=t
                  in JArray [JArray (map JNumber r) | r <- [[a,b,c,x],[d,e,f,y],[g,h,i,z],[0,0,0,1]]]
    row r = let world=riTransform r
                printWorld = printToAssembly r
                layer = if rigid world then fmap (\(m,_) -> vec (mApply m (0,0,1))) printWorld else Nothing
            in JObject [("id",JString (riId r)),("source",JString (riSource r))
              ,("part_to_assembly",matrix world),("rigid",JBool (rigid world))
              ,("part_to_print",maybe JNull matrix (riPrintTransform r))
              ,("print_to_assembly",maybe JNull matrix printWorld)
              ,("print_layer_normal_assembly",maybe JNull id layer)
              ,("declared_print_count",JNumber (fromIntegral (riCount r))),("assembly_only",JBool (riAssemblyOnly r))
              ,("hints",JObject [(k,JString v) | (k,v) <- riHints r])]
