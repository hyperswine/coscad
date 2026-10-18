-- | `coscad validate`: check a part or an assembly without writing
-- artifacts, and describe the outcome as structured diagnostics. The text
-- the commands print (`Error: file:line:col: reason`, an excerpt,
-- `Warning: ...`) is turned into records here, once, so tools such as the
-- VS Code extension read JSON instead of re-parsing compiler output.
module Coscad.Diagnose
  ( Severity (..)
  , Diagnostic (..)
  , validateSource
  , failureDiagnostics
  , diagDiagnostic
  , validateJson
  )
where

import Coscad.Assemble (asmFailure, loadAssembly, loadAssemblyText)
import Coscad.Geometry (anchorWarnings, formatAnchorWarning)
import Coscad.IO (readFileUtf8)
import Coscad.Json
import Coscad.Parser (parseProgramNamed)
import Coscad.Plan (validateHardware)
import Coscad.Report
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

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

-- | Validate @file@ (a .coscad part or a .assemble assembly). @source@,
-- when given, is used instead of the file's contents on disk (an unsaved
-- editor buffer); referenced files are always read from disk. Warnings go
-- to @emit@. Right is the success line ("Valid part" / "Valid assembly").
validateSource :: Emit -> FilePath -> Maybe String -> IO (Either Failure String)
validateSource emit file source = runCommand $ case takeExtension file of
  ".assemble" -> do
    r <- maybe (loadAssembly emit [] file) (loadAssemblyText emit file) source
    case r of
      Left e -> abort (asmFailure e)
      Right ar -> case validateHardware ar of
        Left errs -> abort (designFailures (map ((file ++ ": design: ") ++) (lines errs)))
        Right () -> return "Valid assembly"
  ".coscad" -> do
    text <- case source of
      Just t -> return t
      Nothing -> do
        exists <- doesFileExist file
        if exists then readFileUtf8 file else abort (inputFailure (file ++ ": file not found"))
    case parseProgramNamed file text of
      Left err -> abort (inputFailure err)
      Right (_, shape) -> do
        mapM_ (emit . Warning . formatAnchorWarning file) (anchorWarnings shape)
        return "Valid part"
  _ -> abort (inputFailure "validate needs a .coscad or .assemble file")

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
