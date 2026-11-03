-- | Definitions, variable resolution and program entry points
-- over the expression grammar: pragmas, per-definition parsing with
-- retry on missing names, the numeric phase and analysis directives.
module Coscad.Parser.Program
  ( Def (..)
  , checkedPragma
  , expressionString
  , numericPhase
  , parseExpression
  , parseProgram
  , parseProgramNamed
  , parseProgramFull
  , prettyBundle
  , resolveVariables
  , variableDefinition
  ) where

import Coscad.Codegen (loftErrors)
import Coscad.Dim (Dim (D2), dimOf)
import Coscad.Parser.Core
import Coscad.Parser.Grammar (expression)
import Coscad.Shape
import Coscad.Stress.Spec (Analysis (..), Where (..), emptyAnalysis, faceAnchor)
import Coscad.Validate (validateShape)
import Control.Monad (void, when)
import Data.Char (isAlphaNum, isLetter, isSpace)
import Data.Either (lefts, rights)
import Data.List (dropWhileEnd, foldl', intercalate, isPrefixOf, tails)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

-- | One `name = expr` definition, with enough source information to
-- report errors at their real file position later (expressions are
-- parsed lazily, once their dependencies are known).
data Def = Def
  { defName :: VarName
  , defExpr :: String -- raw source text of the expression, newlines preserved
  , defPos :: SourcePos -- position of the expression's first character
  , defPrefix :: String -- text preceding the expression on its first line (error display)
  }
  deriving (Show)

-- | Why a definition could not be resolved in this round.
data Failure
  = FSyntax String -- a real parse (or dimension) error, fully rendered
  | FMissing VarName String -- refers to a variable not (yet) in the table; rendered message
  deriving (Show)

-- | If a parse failed only because an identifier was not in the table,
-- return that identifier.
undefinedVarIn :: ParseErrorBundle String Void -> Maybe VarName
undefinedVarIn b =
  listToMaybe
    [ takeWhile (/= '\'') (drop (length undefPfx + 1) m)
    | FancyError _ fs <- NE.toList (bundleErrors b)
    , ErrorFail m <- Set.toList fs
    , undefPfx `isPrefixOf` m
    ]

-- | Run a parser on a fragment of source text as if it sat at `pos` in
-- the original file, so error positions and the quoted line are right.
runAt :: SourcePos -> String -> String -> Parser a -> Either (ParseErrorBundle String Void) a
runAt pos prefix input p = snd (runParser' p st)
  where
    st =
      State
        { stateInput = input
        , stateOffset = 0
        , statePosState =
            PosState
              { pstateInput = input
              , pstateOffset = 0
              , pstateSourcePos = pos
              , pstateTabWidth = defaultTabWidth
              , pstateLinePrefix = prefix
              }
        , stateParseErrors = []
        }

-- Try to resolve a single definition against the current table
tryResolveVar :: SynMode -> VarTable -> NumTable -> Def -> Either (Def, Failure) (VarName, Shape)
tryResolveVar mode table nums d =
  case runAt (defPos d) (defPrefix d) (defExpr d) (sc *> expression (Ctx mode table nums) <* eof) of
    Right shape -> case dimOf shape >> loftErrors shape >> validateShape shape of
      Right _ -> Right (defName d, shape)
      Left msg -> Left (d, FSyntax (sourcePosPretty (defPos d) ++ ": in '" ++ defName d ++ "': " ++ msg))
    Left err -> case undefinedVarIn err of
      Just v -> Left (d, FMissing v (prettyBundle err))
      Nothing -> Left (d, FSyntax (prettyBundle err))

-- | Render a parse error as `file:line:col: message` on the first line,
-- followed by the source excerpt and caret. Every CoScad diagnostic
-- therefore starts with a position and the reason on one line, which is
-- what editors and pipelines match on; the excerpt is for humans.
prettyBundle :: ParseErrorBundle String Void -> String
prettyBundle bundle =
  case lines (errorBundlePretty bundle) of
    (header : rest) ->
      let (excerpt, msg) = span isExcerpt rest
          reason = intercalate "; " (filter (not . null) (map (dropWhileEnd isSpace) msg))
       in dropWhileEnd isSpace (unlines ((header ++ " " ++ reason) : excerpt))
    [] -> ""
  where
    isExcerpt l = case dropWhile isSpace l of
      '|' : _ -> True
      cs -> let (digits, after) = span (`elem` "0123456789") cs in not (null digits) && take 1 (dropWhile isSpace after) == "|"

hasUnsupportedOp :: ParseErrorBundle String Void -> Bool
hasUnsupportedOp b =
  or [unsupportedOpPfx `isPrefixOf` m | FancyError _ fs <- NE.toList (bundleErrors b), ErrorFail m <- Set.toList fs]

-- Parse a complete program (file name is used in error messages)
parseProgramNamed :: FilePath -> String -> Either String (VarTable, Shape)
parseProgramNamed file input = (\(table, shape, _) -> (table, shape)) <$> parseProgramFull file input

-- | Like 'parseProgramNamed', also returning the part's analysis directives
-- (`support`, `load`, `stress` lines) for `coscad stress`.
parseProgramFull :: FilePath -> String -> Either String (VarTable, Shape, Analysis)
parseProgramFull file input =
  case checkedPragma file input of
    Left err -> Left err
    Right (mode, body) -> case parse (sc *> program file mode <* eof) file body of
        Left err -> Left (prettyBundle err)
        Right result -> result

checkedPragma :: FilePath -> String -> Either String (SynMode, String)
checkedPragma file input = do
  check False 0 (lines input)
  Right (splitPragma input)
  where
    check _ _ [] = Right ()
    check seen off (line : rest)
      | null code = check seen (off + length line + 1) rest
      | take 1 code == "!" =
          if code `notElem` ["!simple", "!glyph"]
            then err ("unknown syntax mode '" ++ code ++ "'; choose !simple or !glyph as the first directive")
            else if seen
              then err "only one syntax mode directive is allowed, before all definitions"
              else check True (off + length line + 1) rest
      | otherwise = check True (off + length line + 1) rest
      where
        code = trim (takeComment line)
        at = off + length (takeWhile isSpace line)
        err msg = case parse (takeP Nothing at *> fail msg :: Parser ()) file input of
          Left e -> Left (prettyBundle e)
          Right _ -> Right ()
    takeComment ('/' : '/' : _) = []
    takeComment (x : xs) = x : takeComment xs
    takeComment [] = []

parseProgram :: String -> Either String (VarTable, Shape)
parseProgram = parseProgramNamed "<input>"

-- | Detect a !simple / !glyph pragma on the first non-comment, non-blank
-- line. The pragma line is blanked (not removed) so error line numbers
-- in the body stay correct.
splitPragma :: String -> (SynMode, String)
splitPragma input = go [] (lines input)
  where
    go _ [] = (ModeLegacy, input)
    go acc (l : rest)
      | blank l = go (acc ++ [l]) rest
      | trim (takeWhile (/= '/') l) == "!simple" = (ModeSimple, unlines (acc ++ ("" : rest)))
      | trim (takeWhile (/= '/') l) == "!glyph" = (ModeGlyph, unlines (acc ++ ("" : rest)))
      | otherwise = (ModeLegacy, input)
    blank l = let t = trim l in null t || take 2 t == "//"

-- Parse the entire program
program :: FilePath -> SynMode -> Parser (Either String (VarTable, Shape, Analysis))
program file mode = do
  items <- many ((Left <$> directive) <|> (Right <$> variableDefinition))
  let varDefs = rights items
      analysis = foldl' addDirective emptyAnalysis (lefts items)
  return $ do
    varTable <- resolveVariables mode varDefs Map.empty
    case Map.lookup "main" varTable of
      Nothing
        | any ((== "main") . defName) varDefs -> Left (file ++ ": 'main' is a number, not a shape; main must be the shape that gets rendered")
        | otherwise -> Left (file ++ ": no 'main' definition found; add a line `main = <shape>` (it is the shape that gets rendered)")
      Just mainShape -> case dimOf mainShape of
        Right D2 ->
          let at = maybe file (sourcePosPretty . defPos) (listToMaybe [d | d <- varDefs, defName d == "main"])
           in Left (at ++ ": 'main' is a 2D profile; extrude it to get a solid (⮕ h main, or |> extrude h)")
        _ -> Right (varTable, mainShape, analysis)

-- ANALYSIS DIRECTIVES -------------------------------------------
-- `support WHERE`, `load WHERE fx fy fz`, `stress key=value ...` lines; see
-- Coscad.Stress.Spec. A definition named support/load/stress still parses
-- because a directive is never followed by '='.
data Directive = DSupport Where | DLoad Where (Double, Double, Double) | DSettings [(String, String)]

addDirective :: Analysis -> Directive -> Analysis
addDirective a d = case d of
  DSupport w -> a { aSupports = aSupports a ++ [w] }
  DLoad w f -> a { aLoads = aLoads a ++ [(w, f)] }
  DSettings kvs -> a { aSettings = aSettings a ++ kvs }

directive :: Parser Directive
directive = choice
  [ dir "support" (DSupport <$> whereP)
  , dir "load" loadP
  , dir "stress" (DSettings <$> some setting)
  ]
  where
    dir word p = try (keyword word <* notFollowedBy (char '=')) *> p
    whereP = datumWhere <|> (AtBox <$> faceWord)
    datumWhere = do
      _ <- char '@'
      name <- identifier
      face <- optional (try faceWord)
      return (maybe (AtDatum name) (AtDatumFace name) face)
    faceWord = do
      o <- getOffset
      w <- lexeme (some (alphaNumChar <|> char '_' <|> char '+'))
      case faceAnchor w of
        Just v -> return v
        Nothing -> setOffset o *> fail ("'" ++ w ++ "' is not a face anchor; use top, bot, lft, rt, fwd or bak")
    loadP = do
      w <- whereP
      o <- getOffset
      fx <- double <?> "force x in N"
      fy <- double <?> "force y in N"
      fz <- double <?> "force z in N"
      when (fx == 0 && fy == 0 && fz == 0) (setOffset o *> fail "a load needs a nonzero force (N)")
      return (DLoad w (fx, fy, fz))
    setting = do
      o <- getOffset
      key <- identifier
      _ <- char '='
      value <- lexeme (some (satisfy (\c -> not (isSpace c))))
      when (key `notElem` ["material", "safety", "cell"]) (setOffset o *> fail ("unknown stress setting '" ++ key ++ "'; use material=, safety= or cell="))
      return (key, value)

-- | The text of the current line up to (not including) the current
-- position: what precedes a lazily-parsed expression, for error quoting.
-- Leading indentation was consumed by the previous lexeme, so it is
-- approximated with spaces (column-accurate for space-indented files).
linePrefix :: SourcePos -> String -> String
linePrefix start matched = replicate (unPos (sourceColumn start) - 1) ' ' ++ matched

-- Parse a variable definition
variableDefinition :: Parser Def
variableDefinition = do
  start <- getSourcePos
  -- '=' must not consume the next definition as its missing expression.
  (pre, name) <- match (identifier <* char '=' <* hspace)
  pos <- getSourcePos
  emptyExpr <- optional (lookAhead (void newline <|> eof <|> void (string "//")))
  case emptyExpr of
    Just _ -> fail ("missing expression after '=' in '" ++ name ++ "'; write " ++ name ++ " = <shape or number>")
    Nothing -> return ()
  expr <- expressionString
  return (Def name expr pos (linePrefix start pre))

-- | The raw source text of an expression, for later parsing with
-- context: the rest of the line, plus any following lines that begin
-- with |> (multi-line pipelines) or a boolean glyph (⊕ ⊖ ∩ ...). Newlines are preserved so error
-- positions inside the expression map back to real file lines, and
-- a trailing // comment cannot swallow the continuation lines.
expressionString :: Parser String
expressionString = lexeme (dropWhileEnd isSpace . fst <$> match (firstLine *> many contLine))
  where
    lineEnd = try (void newline) <|> eof
    firstLine = manyTill anySingle lineEnd
    -- comment-only lines may sit between continuation lines
    commentLine = try $ do
      _ <- many (char ' ' <|> char '\t')
      _ <- string "//"
      manyTill anySingle lineEnd
    contLine = try $ do
      _ <- many commentLine
      _ <- many (char ' ' <|> char '\t')
      _ <- lookAhead (string "|>" <|> (pure <$> oneOf "⊕⊛⊖⊝∩⇓⊞↯"))
      manyTill anySingle lineEnd

-- Parse an expression with variable context
parseExpression :: Ctx -> String -> Either String Shape
parseExpression ctx input =
  case parse (sc *> expression ctx <* eof) "" input of
    Left err -> Left (prettyBundle err)
    Right shape -> Right shape

-- | Resolve the numeric bindings among @defs@ (arithmetic over literals
-- and other numeric bindings), returning them and the remaining
-- (shape) definitions. Numbers can only depend on numbers, so they
-- resolve first, in dependency order. A non-finite value is an error at
-- its definition.
numericPhase :: SynMode -> [Def] -> VarTable -> Either String (NumTable, [Def])
numericPhase mode defs table0 = go Map.empty Nothing (map withTokens defs)
  where
    -- `fresh` is what the previous round resolved (Nothing: the first
    -- round). A definition is parsed again only when its text mentions a
    -- freshly resolved name; otherwise it would fail exactly as before.
    go nums fresh remaining =
      let step (d, toks)
            | not (retry fresh toks) = (Left (d, toks), Nothing)
            | otherwise = case runAt (defPos d) (defPrefix d) (defExpr d) (sc *> numExpr (Ctx mode table0 nums) <* eof) of
                Right v -> (Right (defName d, v), Nothing)
                -- a numeric definition using ^ or ** is reported as such, not
                -- retried as a shape (which would say "expecting a shape")
                Left e -> (Left (d, toks), if hasUnsupportedOp e then Just e else Nothing)
          attempts = map step remaining
          results = map fst attempts
          found = rights results
          misusedOp = [e | (_, Just e) <- attempts]
       in case misusedOp of
          (e : _) -> Left (prettyBundle e)
          [] -> if null found
            then Right (nums, map fst remaining)
            else case [d | (d, _) <- remaining, Just v <- [lookup (defName d) found], isNaN v || isInfinite v] of
              (d : _) -> Left (sourcePosPretty (defPos d) ++ ": in '" ++ defName d ++ "': numeric binding must be finite (check division by zero or overflow)")
              [] -> go (foldl' (\acc (n, v) -> Map.insert n v acc) nums found) (Just (Set.fromList (map fst found))) (lefts results)

-- | A definition with every name its expression could look up.
withTokens :: Def -> (Def, Set.Set VarName)
withTokens d = (d, identTokens (defExpr d))

-- | Every identifier the parser could read anywhere in @src@: each
-- letter-initial suffix of each run of identifier characters (an
-- over-approximation, which is what scheduling needs).
identTokens :: String -> Set.Set VarName
identTokens src = Set.fromList [t | run <- runs src, t@(c : _) <- tails run, isLetter c]
  where
    identChar c = isAlphaNum c || c == '_' || c `elem` ("₀₁₂₃₄₅₆₇₈₉" :: String)
    runs str = case dropWhile (not . identChar) str of
      [] -> []
      rest -> let (r, more) = span identChar rest in r : runs more

-- | Whether a pending definition must be parsed again this round. A
-- parse reads the tables only for identifiers in its own text, and the
-- tables only grow, so a definition none of whose names changed would
-- fail again exactly as it did: skipping it gives the same rounds, the
-- same results and the same errors as retrying everything.
retry :: Maybe (Set.Set VarName) -> Set.Set VarName -> Bool
retry Nothing _ = True
retry (Just fresh) toks = not (Set.disjoint fresh toks)

-- | Resolve definitions by dependency: each round parses every
-- still-unresolved definition against the table so far; definitions
-- that only fail because they reference not-yet-resolved names are
-- retried next round. When a round makes no progress the remaining
-- failures are classified: real syntax/dimension errors are reported
-- first (all of them, with file:line:col), then references to names
-- that are defined nowhere, and only if neither applies is it a
-- genuine circular dependency.
resolveVariables :: SynMode -> [Def] -> VarTable -> Either String VarTable
resolveVariables mode defs table0 = do
  checkDuplicates
  checkReserved
  (nums, shapeDefs) <- numericPhase mode defs table0
  go [(d, toks, Nothing) | (d, toks) <- map withTokens shapeDefs] table0 nums Nothing
  where
    known = Set.fromList (map defName defs ++ Map.keys table0)

    -- Only names dispatched as prefix expressions are reserved. Pipeline
    -- stages and anchor words remain usable as bindings (e.g. x, top).
    checkReserved = case [d | d <- defs, defName d `elem` reservedNames mode] of
      (d : _) -> Left (sourcePosPretty (defPos d) ++ ": reserved word '" ++ defName d ++ "' cannot be a definition name; pick another name")
      [] -> Right ()

    checkDuplicates =
      case [(a, b) | (i, a) <- zip [(0 :: Int) ..] defs, b <- drop (i + 1) defs, defName a == defName b] of
        ((a, b) : _) ->
          Left (sourcePosPretty (defPos b) ++ ": duplicate definition of '" ++ defName a ++ "' (first defined at " ++ sourcePosPretty (defPos a) ++ ")")
        [] -> case [d | d <- defs, defName d `Map.member` table0] of
          (d : _) -> Left (sourcePosPretty (defPos d) ++ ": '" ++ defName d ++ "' is already the name of a part; pick another name")
          [] -> Right ()

    -- Each round parses the pending definitions against the table so
    -- far; one whose names did not change keeps its last failure (see
    -- 'retry'), so a chain of n definitions costs O(n) parses, not O(n²).
    go [] table _ _ = Right table
    go remaining table nums fresh =
      let attempt (d, toks, lastFail) = case lastFail of
            Just f | not (retry fresh toks) -> Left (d, toks, f)
            _ -> case tryResolveVar mode table nums d of
              Right r -> Right r
              Left (_, f) -> Left (d, toks, f)
          results = map attempt remaining
          resolved = rights results
          failed = lefts results
       in if null resolved
            then Left (report [(d, f) | (d, _, f) <- failed])
            else go [(d, toks, Just f) | (d, toks, f) <- failed] (foldl' (\acc (n, sh) -> Map.insert n sh acc) table resolved) nums (Just (Set.fromList (map fst resolved)))

    report failed
      | not (null syntax) = intercalate "\n" syntax
      | not (null undefinedRefs) = intercalate "\n" undefinedRefs
      | otherwise = circularMsg
      where
        syntax = [m | (_, FSyntax m) <- failed]
        undefinedRefs = [m | (_, FMissing v m) <- failed, not (v `Set.member` known)]
        missingOf = Map.fromList [(defName d, v) | (d, FMissing v _) <- failed]
        onCycle start = walk (Map.size missingOf) (Map.lookup start missingOf)
          where
            walk :: Int -> Maybe VarName -> Bool
            walk 0 _ = False
            walk _ Nothing = False
            walk n (Just v)
              | v == start = True
              | otherwise = walk (n - 1) (Map.lookup v missingOf)
        cyc = [d | (d, FMissing _ _) <- failed, onCycle (defName d)]
        members = if null cyc then map fst failed else cyc
        circularMsg =
          "circular dependency: these definitions refer to each other and can never be resolved:\n"
            ++ intercalate "\n" ["  " ++ defName d ++ "  (" ++ sourcePosPretty (defPos d) ++ ")" | d <- members]
