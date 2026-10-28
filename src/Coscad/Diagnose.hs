-- | `coscad validate`: check a part or an assembly without writing
-- artifacts, and describe the outcome as structured diagnostics. The text
-- the commands print (`Error: file:line:col: reason`, an excerpt,
-- `Warning: ...`) is turned into records here, once, so tools such as the
-- VS Code extension read JSON instead of re-parsing compiler output.
module Coscad.Diagnose
  ( Severity (..)
  , Diagnostic (..)
  , validateSource
  , validateProject
  , projectSources
  , validateProjectJson
  , failureDiagnostics
  , diagDiagnostic
  , validateJson
  )
where

import Coscad.Assemble (asmFailure, loadAssemblyWith)
import Coscad.Geometry (anchorWarnings, formatAnchorWarning, datumPoints, resolve)
import Coscad.IO (readFileUtf8)
import Coscad.Json
import Coscad.Parser (parseProgramFull)
import Coscad.Stress.Part (setupFrom)
import Coscad.Stress.Spec (Analysis (..), Where (..))
import Coscad.Plan (validateHardware)
import Coscad.Report
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory (doesFileExist, canonicalizePath)
import Data.IORef
import qualified Data.Map.Strict as Map
import Control.Monad (unless)
import System.FilePath (takeExtension, isAbsolute)

data Severity = SevError | SevWarning | SevNote
  deriving (Eq, Show)

-- | One diagnostic: where (when known) and what. 'dgExcerpt' holds the
-- source excerpt lines that followed an error (`  |`, `3 | ...`, `  | ^`).
data Diagnostic = Diagnostic
  { dgSeverity :: Severity
  , dgFile :: Maybe FilePath
  , dgLine :: Maybe Int
  , dgColumn :: Maybe Int
  , dgMessage :: String
  , dgExcerpt :: [String]
  }
  deriving (Eq, Show)

-- | Legacy validation keeps disk dependencies; project validation overlays all
-- supplied buffers without creating files or modifying the working tree.
validateSource :: Emit -> FilePath -> Maybe String -> IO (Either Failure String)
validateSource emit file source = fst <$> validateProject emit file (maybe [] (\t -> [(file,t)]) source)

-- | Decode a bounded, versioned editor snapshot. Paths are absolute; aliases
-- are canonicalized and duplicate identities rejected by 'validateProject'.
projectSources :: String -> Either String [(FilePath,String)]
projectSources text = do
  unless (length text <= 8*1024*1024) (Left "project snapshot exceeds 8 Mi characters")
  value <- decode text
  unless (lookupKey "schema" value == Just (JString "coscad.project/1")) (Left "expected coscad.project/1 snapshot")
  files <- maybe (Left "snapshot needs a files array") Right (lookupKey "files" value >>= asArray)
  unless (length files <= 256) (Left "snapshot exceeds 256 buffers")
  mapM entry files
  where
    entry v = do
      path <- maybe (Left "buffer needs path") Right (lookupKey "path" v >>= asString)
      body <- maybe (Left "buffer needs text") Right (lookupKey "text" v >>= asString)
      unless (isAbsolute path && takeExtension path `elem` [".coscad", ".assemble"] && '\0' `notElem` path)
        (Left "buffer path must be absolute and end in .coscad or .assemble")
      return (path,body)

validateProject :: Emit -> FilePath -> [(FilePath,String)] -> IO (Either Failure String,[FilePath])
validateProject emit file sources = do
  entries <- mapM (\(p,t) -> (\key -> (key,t)) <$> canonicalizePath p) sources
  deps <- newIORef []
  contents <- newIORef Map.empty
  let buffers = Map.fromList entries
      reader path = do
        key <- canonicalizePath path
        modifyIORef' deps (key :)
        cached <- readIORef contents
        case Map.lookup key cached of
          Just text -> return text
          Nothing -> do
            text <- case Map.lookup key buffers of
              Just text -> return (Just text)
              Nothing -> do
                exists <- doesFileExist path
                if exists then Just <$> readFileUtf8 path else return Nothing
            modifyIORef' contents (Map.insert key text)
            return text
  outcome <- runCommand $ do
    unless (Map.size buffers == length entries) (abort (inputFailure "duplicate buffer path in project snapshot"))
    result <- case takeExtension file of
      ".assemble" -> do
        r <- loadAssemblyWith reader emit [] file
        case r of
          Left e -> abort (asmFailure e)
          Right ar -> case validateHardware ar of
            Left errs -> abort (designFailures (map ((file ++ ": design: ") ++) (lines errs)))
            Right () -> return "Valid assembly"
      ".coscad" -> do
        source <- reader file
        text <- maybe (abort (inputFailure (file ++ ": file not found"))) return source
        case parseProgramFull file text of
          Left err -> abort (inputFailure err)
          Right (_, shape, _) -> do
            mapM_ (emit . Warning . formatAnchorWarning file) (anchorWarnings shape)
            return "Valid part"
      _ -> abort (inputFailure "validate needs a .coscad or .assemble file")
    -- Check analysis settings and datum identities on the same source snapshot,
    -- including referenced parts. This does not mesh or solve the model.
    readIORef contents >>= mapM_ checkAnalysis . Map.toList
    return result
  paths <- Map.keys . Map.fromList . map (\p -> (p,())) <$> readIORef deps
  return (outcome,paths)
  where
    checkAnalysis (path, Just text) | takeExtension path == ".coscad" =
      case parseProgramFull path text of
        Left err -> abort (inputFailure err)
        Right (_, shape, analysis) -> do
          let failAt message = abort (inputFailure (path ++ ": " ++ message))
              datums = datumPoints (resolve shape)
              check name = case Map.lookup name datums of
                Nothing -> failAt ("stress: unknown datum '@" ++ name ++ "'")
                Just [_] -> return ()
                Just ps -> failAt ("stress: ambiguous datum '@" ++ name ++ "': " ++ show (length ps) ++ " copies")
              checkWhere (AtDatum name) = check name
              checkWhere (AtDatumFace name _) = check name
              checkWhere (AtBox _) = return ()
          either failAt (const (return ())) (setupFrom analysis)
          mapM_ checkWhere (aSupports analysis ++ map fst (aLoads analysis))
    checkAnalysis _ = return ()

validateProjectJson :: FilePath -> Either Failure String -> [Diag] -> [FilePath] -> String
validateProjectJson file outcome diags deps = case decode (validateJson file outcome diags) of
  Right (JObject fields) -> encode (JObject (fields ++ [("dependencies",JArray (map JString deps))]))
  _ -> validateJson file outcome diags

-- | A failure's printed lines as diagnostics: each `Error...` line starts
-- one, excerpt lines attach to it, other continuation lines (`  note: ...`)
-- extend its message.
failureDiagnostics :: Failure -> [Diagnostic]
failureDiagnostics (Failure _ ls) = reverse (foldl step [] ls)
  where
    step acc l
      | "Error: " `isPrefixOf` l = locate SevError (drop 7 l) : acc
      | "Error " `isPrefixOf` l = locate SevError (drop 6 l) : acc
      | otherwise = case acc of
          d : rest
            | isExcerpt l -> d {dgExcerpt = dgExcerpt d ++ [l]} : rest
            | all isSpace l -> acc
            | otherwise -> d {dgMessage = dgMessage d ++ "\n" ++ dropWhile isSpace l} : rest
          [] -> if all isSpace l then acc else [locate SevError l]
    isExcerpt l = case dropWhile isSpace l of
      '|' : _ -> True
      rest -> let (ds, more) = span isDigit rest in not (null ds) && take 1 (dropWhile isSpace more) == "|"

diagDiagnostic :: Diag -> Diagnostic
diagDiagnostic d = case d of
  Warning m -> locate SevWarning m
  Note m -> locate SevNote m

-- | Split @file:line:col: message@ or @file.coscad: message@ off a message.
locate :: Severity -> String -> Diagnostic
locate sev m = case positioned of
  (f, l, c, rest) : _ -> Diagnostic sev (Just f) (Just l) (Just c) rest []
  [] -> case fileLevel of
    (f, rest) : _ -> Diagnostic sev (Just f) Nothing Nothing rest []
    [] -> Diagnostic sev Nothing Nothing Nothing m []
  where
    splits = [(take i m, drop (i + 1) m) | (i, ch) <- zip [0 ..] m, ch == ':']
    positioned =
      [ (f, read ls, read cs, dropWhile (== ' ') rest)
      | (f, after) <- splits, not (null f)
      , let (ls, a1) = span isDigit after, not (null ls), take 1 a1 == ":"
      , let (cs, a2) = span isDigit (drop 1 a1), not (null cs), take 1 a2 == ":"
      , let rest = drop 1 a2 ]
    fileLevel =
      [ (f, dropWhile (== ' ') rest)
      | (f, rest) <- splits, any (`isSuffixOf` f) [".coscad", ".assemble"], take 1 rest == " " ]

-- | The JSON document `coscad validate --json` prints on stdout.
validateJson :: FilePath -> Either Failure String -> [Diag] -> String
validateJson file outcome diags =
  encode $
    jobj
      [ ("schema", JString "coscad.validate/1")
      , ("file", JString file)
      , ("valid", JBool (either (const False) (const True) outcome))
      , ("exit", JNumber (fromIntegral (either failExit (const 0) outcome)))
      , ("diagnostics", jlist (map json (map diagDiagnostic diags ++ either failureDiagnostics (const []) outcome)))
      ]
  where
    json d =
      jobj
        [ ("severity", JString (case dgSeverity d of SevError -> "error"; SevWarning -> "warning"; SevNote -> "note"))
        , ("file", maybe JNull JString (dgFile d))
        , ("line", maybe JNull (JNumber . fromIntegral) (dgLine d))
        , ("column", maybe JNull (JNumber . fromIntegral) (dgColumn d))
        , ("message", JString (dgMessage d))
        , ("excerpt", jlist (map JString (dgExcerpt d)))
        ]
