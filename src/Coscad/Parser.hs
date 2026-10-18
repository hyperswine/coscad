-- | Parser for .coscad files: glyph shapes, word shapes, boolean
-- operators, attachment ops, and |> pipelines with multi-line
-- continuation. Also the variable-resolution pass.
module Coscad.Parser
  ( Ctx (..)
  , Def (..)
  , Parser
  , SynMode (..)
  , VarTable
  , anchorVec
  , checkedPragma
  , double
  , expressionString
  , frameRef
  , identifier
  , lexeme
  , num
  , numericPhase
  , parseExpression
  , parseProgram
  , parseProgramNamed
  , prettyBundle
  , reservedNames
  , resolveVariables
  , sc
  , symbol
  , trim
  , variableDefinition
  )
where

import Control.Monad (foldM, void, when)
import Coscad.Codegen (loftErrors)
import Coscad.Dim (Dim (..), dimOf)
import Coscad.Geometry (PointRef (..), MatingFrame (..), datumPoints, pointOf, makeFrame, frameOf, matingPose)
import Coscad.Linalg (vadd, vneg, vsub)
import Coscad.Shape
import Coscad.Validate (maxQuantity, maxSides, suggestion, validateShape)
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
import qualified Text.Megaparsec.Char.Lexer as L

getOffsetValue :: Shape -> Double
getOffsetValue (Sphere r) = r
getOffsetValue (Cylinder r _) = r
getOffsetValue (Shape2D _ r) = r
getOffsetValue _ = 1.0 -- Default offset value

-- | Syntax mode, chosen by a pragma on the first code line of a file:
--   !glyph  — glyph operators/shapes (plus legacy lowercase words)
--   !simple — ASCII word set: Translate/Rotate/Scale/Mirror (with .x/.y/.z
--             and (x, y, z) tuple forms), Hull/Intersect/Minkowski/Offset,
--             capitalized shape words, and '*' / '-' for union / difference.
-- No pragma = legacy mode: both sets accepted (backwards compatible).
-- '$' (Haskell-style loose application) and |> pipelines work in ALL modes.
data SynMode = ModeLegacy | ModeGlyph | ModeSimple deriving (Eq, Show)

data Ctx = Ctx {cMode :: SynMode, cVars :: VarTable, cNums :: NumTable}

glyphOK, simpleOK :: Ctx -> Bool
glyphOK c = cMode c /= ModeSimple
simpleOK c = cMode c /= ModeGlyph

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

undefPfx :: String
undefPfx = "undefined variable "

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

type VarName = String

type VarTable = Map.Map VarName Shape

-- | Numeric bindings: `w = 20`, `r = w / 2 - 1`
type NumTable = Map.Map VarName Double

type Parser = Parsec Void String

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

-- Lexer helpers
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- | Identifier-like keyword (e.g. "xcyl") that must not be a prefix of a
-- longer identifier, so user variables like "xcyl2" still work.
keyword :: String -> Parser String
keyword s = lexeme $ try (string s <* notFollowedBy (alphaNumChar <|> char '_'))

identifier :: Parser String
identifier = lexeme $ do
  first <- letterChar
  rest <- many (alphaNumChar <|> char '_' <|> subscriptChar)
  return (first : rest)
  where
    subscriptChar = oneOf "₀₁₂₃₄₅₆₇₈₉"

double :: Parser Double
double = lexeme $ L.signed sc (try L.float <|> fromIntegral <$> (L.decimal :: Parser Integer))

-- NUMBERS -------------------------------------------------------
-- A numeric argument is a single atom: a literal, a numeric binding's
-- name, a parenthesized arithmetic expression, or a negated atom.
--   box w (h / 2) t      χ (w / 2 + 1) part      ● -r
-- Arithmetic (+ - * /, parens, unary minus) lives inside parentheses
-- or on the right-hand side of a numeric definition, so juxtaposed
-- arguments stay unambiguous.
num :: Ctx -> Parser Double
num ctx = do
  o <- getOffset
  unicodeMinus <- optional (lookAhead (char '−'))
  case unicodeMinus of
    Just _ -> fail "Unicode minus U+2212 is not a numeric sign; replace it with '-'"
    Nothing -> return ()
  v <- choice
    [ do
        _ <- char '-'
        spaced <- optional (lookAhead spaceChar)
        case spaced of
          Just _ -> setOffset o *> fail "a negative argument must touch its operand: use -2 or -w; write (w - 2) for subtraction"
          Nothing -> negate <$> num ctx
    , try double
    , try (between (symbol "(") (symbol ")") (numExpr ctx))
    , try (numName ctx)
    ] <?> "a number (literal, numeric binding, or parenthesized arithmetic)"
  if isNaN v || isInfinite v
    then setOffset o *> fail "numeric argument must be finite (check division by zero or overflow)"
    else return v

numExpr :: Ctx -> Parser Double
numExpr ctx = do
  t <- term
  rest <- many ((,) <$> (symbol "+" <|> symbol "-") <*> term)
  unsupportedOperator
  return (foldl (\a (o, b) -> if o == "+" then a + b else a - b) t rest)
  where
    -- `^` and `**` look like arithmetic but are not operators here; say so
    -- instead of letting the shape parser report "expecting a shape".
    unsupportedOperator = do
      o <- getOffset
      op <- optional (lookAhead (string "**" <|> string "^"))
      case op of
        Just w -> setOffset o *> fail (unsupportedOpPfx ++ "'" ++ w ++ "' is not an arithmetic operator; numbers use + - * / and parentheses (write w * w for w squared)")
        Nothing -> return ()
    term = do
      f <- factor
      -- `**` is not multiplication; leave it for unsupportedOperator
      rest <- many ((,) <$> (lexeme (try (string "*" <* notFollowedBy (char '*'))) <|> symbol "/") <*> factor)
      return (foldl (\a (o, b) -> if o == "*" then a * b else a / b) f rest)
    factor = (symbol "-" *> (negate <$> factor)) <|> atom
    atom =
      choice
        [ lexeme (try L.float <|> fromIntegral <$> (L.decimal :: Parser Integer))
        , between (symbol "(") (symbol ")") (numExpr ctx)
        , numName ctx
        ]
        <?> "a number, a numeric binding, or '('"

-- | Where the next token starts, for attachment nodes (see 'SrcPos').
-- Definitions are parsed at their own position in the file ('runAt'),
-- so this is the real line and column.
srcPos :: Parser SrcPos
srcPos = (\p -> Just (unPos (sourceLine p), unPos (sourceColumn p))) <$> getSourcePos

-- | Prefix of the error for `^` / `**`; the numeric phase reports it
-- directly rather than retrying the definition as a shape.
unsupportedOpPfx :: String
unsupportedOpPfx = "unsupported operator: "

hasUnsupportedOp :: ParseErrorBundle String Void -> Bool
hasUnsupportedOp b =
  or [unsupportedOpPfx `isPrefixOf` m | FancyError _ fs <- NE.toList (bundleErrors b), ErrorFail m <- Set.toList fs]

-- | A numeric binding by name; a shape's name here is its own error.
numName :: Ctx -> Parser Double
numName ctx = do
  o <- getOffset
  n <- identifier
  case Map.lookup n (cNums ctx) of
    Just v -> return v
    Nothing
      | Map.member n (cVars ctx) -> setOffset o *> fail ("'" ++ n ++ "' is a shape, not a number")
      | otherwise -> setOffset o *> fail (undefPfx ++ "'" ++ n ++ "'")

-- Parse a complete program (file name is used in error messages)
parseProgramNamed :: FilePath -> String -> Either String (VarTable, Shape)
parseProgramNamed file input =
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
program :: FilePath -> SynMode -> Parser (Either String (VarTable, Shape))
program file mode = do
  varDefs <- many variableDefinition
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
        _ -> Right (varTable, mainShape)

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

-- Name a missing parameter, while preserving undefined-name and numeric errors.
arg :: Ctx -> String -> String -> Parser Double
arg ctx form param = do
  off <- getOffset
  result <- observing (num ctx)
  case result of
    Right n -> return n
    Left e@FancyError {} -> parseError e
    Left _ -> setOffset off *> fail (form ++ " is missing or has an invalid " ++ param ++ " argument")

-- Parse an expression (pipelines bind loosest)
expression :: Ctx -> Parser Shape
expression = pipeExpression

-- | Shape argument for prefix forms: either a primary, or '$' followed
-- by the whole rest of the expression (Haskell-style loose application):
--   Translate.x 5 $ a * b        χ 5 $ a ⊕ b
shapeArg :: Ctx -> Parser Shape
shapeArg ctx = (symbol "$" *> expression ctx) <|> primaryExpression ctx

-- |> pipelines: each stage is a postfix operation on the shape so far.
--   plate |> at top 5.5 0 -5 flange |> cutat lft 3.5 0 0 (xcyl 2.7 12)
pipeExpression :: Ctx -> Parser Shape
pipeExpression ctx = do
  left <- booleanExpression ctx
  rest <- many $ do
    _ <- symbol "|>"
    off <- getOffset
    stage <- pipeStage ctx
    return (off, stage)
  -- `|>` binds loosest, so a boolean operator cannot follow a pipeline;
  -- point at the fix instead of "expecting |> or end of input"
  when (not (null rest)) $ do
    o <- getOffset
    op <- optional (lookAhead (oneOf ("\x2295\x229B\x2296\x229D\x2229\x21D3\x229E\x21AF*" :: String)))
    case op of
      Just c -> setOffset o *> fail ("'" ++ [c] ++ "' cannot follow a pipeline: |> binds loosest, so wrap the pipeline in parentheses, e.g. (a |> move 1 2 3) " ++ [c] ++ " b")
      Nothing -> return ()
  foldM apply left rest
  where
    apply s (off, stage) = either (\e -> setOffset off *> fail e) return (stage s)

pipeStage :: Ctx -> Parser (Shape -> Either String Shape)
pipeStage ctx = choice
  [ datumStage
  , frameStage
  , mateStage
  , placeStage
  , gridStage
  , anchorStage
  , fmap (\f s -> Right (f s)) plainStage
  ]
  where
    plainStage = choice
      [ num1 "x" Tx
      , num1 "y" Ty
      , num1 "z" Tz
      , num1 "rotx" Rx
      , num1 "roty" Ry
      , num1 "rotz" Rz
      , num1 "extrude" Extrude
      , num3 "move" Translate
      , num3 "scale" Scale
      , num3 "mirror" Mirror
      , keyword "loft" *> (loftStage <$> num ctx <*> primaryExpression ctx)
      , rel "at" Position
      , rel "on" AttachTo
      , rel "cutat" CutAt
      , bin "add" (\p s -> Union [p, s])
      , bin "cut" Diff
      , bin "isect" (\p s -> Intersection [p, s])
      , do
          _ <- keyword "hull"
          child <- optional (shapeArg ctx)
          return (\p -> Hull (p : maybe [] (: []) child))
      , bin "mink" (\p s -> Minkowski [p, s])
      , do
          off <- getOffset
          name <- identifier
          setOffset off *> fail ("unknown pipeline stage '" ++ name ++ "'" ++ suggestion stages name ++ "; use e.g. |> move x y z")
      ]
    stages = ["x", "y", "z", "rotx", "roty", "rotz", "extrude", "move", "scale", "mirror", "anchor", "datum", "frame", "mate", "place", "grid", "loft", "at", "on", "cutat", "add", "cut", "isect", "hull", "mink"]
    anchorStage = do
      at <- srcPos
      _ <- keyword "anchor"
      ref <- pointRef
      return $ \p -> case ref of
        BoxPoint v -> Right (Anchor at v p)
        NamedPoint _ -> (\v -> Translate (vneg v) p) <$> pointOf ref p
    datumStage = do
      _ <- keyword "datum"
      name <- identifier
      ref <- pointRef
      off <- pointOffsets "datum"
      return $ \p -> do
        when (Map.member name (datumPoints p)) (Left ("duplicate datum '@" ++ name ++ "'; choose a new name"))
        pt <- pointOf ref p
        return (Datum name (vadd pt off) p)
    frameStage = do
      _ <- keyword "frame"
      name <- identifier
      ref <- pointRef
      normal <- anchorVec <?> "frame outward normal (top bot rt lft fwd bak)"
      up <- anchorVec <?> "frame in-plane up direction"
      off <- pointOffsets "frame"
      return $ \p -> do
        requireSolid "frame" p
        when (Map.member name (datumPoints p)) (Left ("duplicate datum or frame '@" ++ name ++ "'; choose a new name"))
        pt <- pointOf ref p
        MatingFrame origin basis <- makeFrame (vadd pt off) normal up
        return (Frame name origin basis p)
    mateStage = do
      _ <- keyword "mate"
      parent <- primaryExpression ctx
      target <- frameRef
      own <- frameRef
      (roll, gap) <- ((0,0) <$ stageEnd) <|> ((,) <$> arg ctx "mate" "roll in degrees" <*> arg ctx "mate" "gap in mm (roll and gap are both required)")
      return $ \child -> do
        requireSolid "mate parent" parent
        requireSolid "mate child" child
        loftErrors parent
        validateShape parent
        pf <- frameOf target parent
        cf <- frameOf own child
        let (r,t) = matingPose pf cf roll gap
        return (Transform3 r t child)
    requireSolid context s = do
      d <- dimOf s
      when (d /= D3) (Left (context ++ " needs a 3D solid; extrude a 2D profile first"))
    -- Only the piped child is returned; the separate datum solid never enters
    -- the emitted CSG or assembly instance graph. Both points are explicit.
    placeStage = do
      _ <- keyword "place"
      parent <- primaryExpression ctx
      target <- pointRef
      own <- pointRef
      off <- pointOffsets "place"
      return $ \child -> do
        _ <- dimOf parent
        loftErrors parent
        validateShape parent
        pa <- pointOf target parent
        ca <- pointOf own child
        return (Translate (vsub (vadd pa off) ca) child)
    gridStage = do
      _ <- keyword "grid"
      nx <- gridCount "column count"
      ny <- gridCount "row count"
      when (nx * ny > maxQuantity) (fail ("grid copy count exceeds " ++ show maxQuantity ++ "; reduce columns or rows"))
      px <- arg ctx "grid" "column pitch"
      py <- arg ctx "grid" "row pitch"
      when ((nx > 1 && px == 0) || (ny > 1 && py == 0)) (fail "grid pitch must be nonzero on each repeated axis; negative pitch is allowed")
      return $ \child -> do
        -- Stop counting at the budget before allocating repeated subtrees.
        -- This bounds nested grids, not just one expansion at a time.
        let leaves = length (take (maxQuantity + 1) (leafShapes child))
        when (nx * ny * leaves > maxQuantity) (Left ("grid expanded geometry exceeds " ++ show maxQuantity ++ " primitive copies (including nested grids); reduce repetition"))
        return (Union
          [ if i == 0 && j == 0 then child
            else Translate (fromIntegral i * px, fromIntegral j * py, 0) child
          | j <- [0 .. ny - 1], i <- [0 .. nx - 1]
          ])
    leafShapes s = case shapeChildren s of [] -> [s]; xs -> concatMap leafShapes xs
    gridCount param = do
      off <- getOffset
      n <- arg ctx "grid" param
      if n >= 1 && n <= fromIntegral maxQuantity && n == fromInteger (truncate n)
        then return (round n :: Int)
        else setOffset off *> fail ("grid " ++ param ++ " must be a whole number from 1 to " ++ show maxQuantity)
    -- `keyword` backtracks by itself when the word does not match; the
    -- stage body is deliberately NOT under `try`, so an error inside a
    -- stage is reported where it happened (megaparsec >= 9.7 reports a
    -- failed `try` at its start).
    num1 w f = keyword w *> (f <$> num ctx)
    num3 w f = keyword w *> (f <$> ((,,) <$> num ctx <*> num ctx <*> num ctx))
    rel w f = do
      at <- srcPos
      _ <- keyword w
      v <- anchorVec
      off <- offsets w
      child <- shapeArg ctx
      return (\p -> f at v off p child)
    offsets w = option (0, 0, 0) $ do
      _ <- lookAhead (try (num ctx))
      vectorArgs w "third offset z (three offsets followed by a child shape are required)"
    -- These stages have no trailing child argument. Only a stage/expression
    -- boundary omits offsets; a misspelled numeric binding must stay an error.
    stageEnd = lookAhead (void (symbol "|>") <|> void (symbol ")") <|> eof)
    pointOffsets w = ((0, 0, 0) <$ stageEnd)
      <|> vectorArgs w "third offset z (three offsets are required)"
    vectorArgs w third = (,,) <$> arg ctx w "first offset x" <*> arg ctx w "second offset y" <*> arg ctx w third
    bin w f = do
      _ <- keyword w
      s <- shapeArg ctx
      return (`f` s)
    -- `p |> loft z q`: start a loft from p (at z = 0), or extend one
    loftStage z q (Loft ps) = Loft (ps ++ [(z, q)])
    loftStage z q p = Loft [(0, p), (z, q)]

-- Parse boolean expressions (union, difference, hull, minkowski, offset)
-- Glyph operators in !glyph/legacy; '*' (union) and '-' (difference)
-- in !simple/legacy.
booleanExpression :: Ctx -> Parser Shape
booleanExpression ctx = do
  left <- attachExpression ctx
  rest <- many $ do
    op <- choice (gOps ++ sOps) <?> opLabel
    right <- attachExpression ctx
    return (op, right)
  return $ foldl applyBooleanOp left rest
  where
    gOps = if glyphOK ctx then map symbol ["⊖", "⊝", "⊕", "⊛", "∩", "⇓", "⊞", "↯"] else []
    sOps = if simpleOK ctx then map symbol ["*", "-"] else []
    opLabel = case cMode ctx of
      ModeGlyph -> "an operator (⊕ ⊖ ∩ ⇓ ⊞ ↯ ⌖ ⋈ |>)"
      ModeSimple -> "an operator (* - |>)"
      ModeLegacy -> "an operator (⊕ ⊖ ∩ ⇓ ⊞ ↯ ⌖ ⋈ * - |>)"
    applyBooleanOp left ("⊖", right) = Diff left right
    applyBooleanOp left ("⊝", right) = Diff left right
    applyBooleanOp left ("-", right) = Diff left right
    applyBooleanOp left ("⊕", right) = Union [left, right]
    applyBooleanOp left ("⊛", right) = Union [left, right]
    applyBooleanOp left ("*", right) = Union [left, right]
    applyBooleanOp left ("∩", right) = Intersection [left, right]
    applyBooleanOp left ("⇓", right) = Hull [left, right]
    applyBooleanOp left ("⊞", right) = Minkowski [left, right]
    applyBooleanOp left ("↯", right) = Offset (getOffsetValue right) left
    applyBooleanOp _ (op, _) = error $ "Unknown boolean operator: " ++ op

-- | Anchor vector: words like top/bot/lft/rt/fwd/bak/ctr, combinable
-- with '+', e.g. "top+rt" for the top-right edge.
anchorVec :: Parser (Double, Double, Double)
anchorVec = do
  o <- getOffset
  ws <- sepBy1 anchorWord (symbol "+")
  let (xs, ys, zs) = unzip3 ws
      repeatedAxis vs = length (filter (/= 0) vs) > 1
      invalid = any repeatedAxis [xs, ys, zs] || (length ws > 1 && (0, 0, 0) `elem` ws)
  if invalid
    then setOffset o *> fail "invalid anchor combination: choose at most one direction per axis; use ctr alone"
    else return (sum xs, sum ys, sum zs)
  where
    anchorWord =
      choice
        [ (0, 0, 1) <$ (keyword "top" <|> keyword "up"),
          (0, 0, -1) <$ (keyword "bot" <|> keyword "dn" <|> keyword "down"),
          (1, 0, 0) <$ (keyword "rt" <|> keyword "right"),
          (-1, 0, 0) <$ (keyword "lft" <|> keyword "left"),
          (0, -1, 0) <$ (keyword "fwd" <|> keyword "front"),
          (0, 1, 0) <$ (keyword "bak" <|> keyword "back"),
          (0, 0, 0) <$ (keyword "ctr" <|> keyword "center")
        ]

-- Named references are explicit, so bindings and anchor words keep their
-- existing meanings in all syntax modes.
pointRef :: Parser PointRef
pointRef = (symbol "@" *> (NamedPoint <$> identifier)) <|> (BoxPoint <$> anchorVec)

-- | @name, or a dotted path through assembly instance aliases (@q.b).
frameRef :: Parser String
frameRef = symbol "@" *> lexeme (intercalate "." <$> ((:) <$> rawIdent <*> many (try (char '.' *> rawIdent)))) <?> "oriented frame reference (@name or @instance.name)"
  where
    rawIdent = (:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> oneOf "₀₁₂₃₄₅₆₇₈₉")

-- Parse attachment expressions: bind tighter than boolean ops.
--   a ⌖ top b   -- position: b's bottom snapped to a's top (translate only)
--   a ⋈ rt b    -- attach: b rotated so +Z points right, bottom mated to face
attachExpression :: Ctx -> Parser Shape
attachExpression ctx = do
  left <- transformExpression ctx
  rest <- many (choice steps)
  return $ foldl (flip ($)) left rest
  where
    steps
      | glyphOK ctx =
          [ do
              at <- srcPos
              _ <- symbol "⌖"
              v <- anchorVec
              right <- transformExpression ctx
              return (\l -> Position at v (0, 0, 0) l right),
            do
              at <- srcPos
              _ <- symbol "⋈"
              v <- anchorVec
              right <- transformExpression ctx
              return (\l -> AttachTo at v (0, 0, 0) l right)
          ]
      | otherwise = []

-- Parse transformation expressions
transformExpression :: Ctx -> Parser Shape
transformExpression ctx = (transformation ctx <?> tLabel) <|> primaryExpression ctx
  where
    tLabel = case cMode ctx of
      ModeGlyph -> "a transform (χ ψ ζ θ ϕ ω ⬈ ⇋ ⮕ ⟰ ⚓ loft)"
      ModeSimple -> "a transform (Translate Rotate Scale Mirror Extrude Loft Anchor Hull Union Intersect Minkowski Offset)"
      ModeLegacy -> "a transform (χ ψ ζ θ ϕ ω ⬈ ⇋ ⮕ ⟰ ⚓ loft, or Translate/Rotate/Scale/...)"

-- | A comma tuple: (x, y, z)
tuple3 :: Ctx -> Parser (Double, Double, Double)
tuple3 ctx = between (symbol "(") (symbol ")") $ do
  a <- num ctx
  _ <- symbol ","
  b <- num ctx
  _ <- symbol ","
  c <- num ctx
  return (a, b, c)

-- Parse transformations
transformation :: Ctx -> Parser Shape
transformation ctx =
  choice (glyphTs ++ wordTs ++ simpleTs)
  where
    -- loft z0 p0 z1 p1 [z2 p2 ...] : skin 2D profiles at ascending z
    loftPairs = do
      ps <- some ((,) <$> try (num ctx) <*> primaryExpression ctx)
      if length ps < 2
        then fail "loft needs at least two profiles: loft z0 profile0 z1 profile1 ..."
        else return (Loft ps)
    wordTs = [keyword "loft" *> loftPairs]
    glyphTs
      | glyphOK ctx =
          [ g1 "χ" Tx, g1 "ψ" Ty, g1 "ζ" Tz
          , g1 "θ" Rx, g1 "ϕ" Ry, g1 "ω" Rz
          , g3 "⬈" Scale, g3 "⇋" Mirror
          , g1 "⮕" Extrude
          , symbol "⟰" *> loftPairs
          , do
              at <- srcPos
              _ <- symbol "⚓"
              v <- anchorVec
              Anchor at v <$> shapeArg ctx
          ]
      | otherwise = []
    g1 s f = do
      _ <- symbol s
      n <- num ctx
      f n <$> shapeArg ctx
    g3 s f = do
      _ <- symbol s
      a <- num ctx
      b <- num ctx
      c <- num ctx
      f (a, b, c) <$> shapeArg ctx

    -- !simple word set: namespaced transforms + prefix combinators.
    --   Translate (x, y, z) obj      Translate.x n obj
    --   Rotate (x, y, z) obj         Rotate.z n obj    (rotate order: x, y, z)
    --   Scale (x, y, z) / Scale.x n  Mirror (x, y, z) / Mirror.x
    --   Extrude h obj   Anchor top obj
    --   Hull a b   Union a b   Intersect a b   Minkowski a b   Offset n a
    simpleTs
      | simpleOK ctx =
          [ ns "Translate" Translate [("x", Tx), ("y", Ty), ("z", Tz)]
          , ns "Rotate" rotXYZ [("x", Rx), ("y", Ry), ("z", Rz)]
          , ns "Scale" Scale [("x", \n -> Scale (n, 1, 1)), ("y", \n -> Scale (1, n, 1)), ("z", \n -> Scale (1, 1, n))]
          , nsMirror
          , keyword "Extrude" *> (Extrude <$> num ctx <*> shapeArg ctx)
          , keyword "Loft" *> loftPairs
          , srcPos >>= \at -> keyword "Anchor" *> (Anchor at <$> anchorVec <*> shapeArg ctx)
          , bin2 "Hull" (\a b -> Hull [a, b])
          , bin2 "Union" (\a b -> Union [a, b])
          , bin2 "Intersect" (\a b -> Intersection [a, b])
          , bin2 "Minkowski" (\a b -> Minkowski [a, b])
          , keyword "Offset" *> ((\n s -> Offset n s) <$> num ctx <*> shapeArg ctx)
          ]
      | otherwise = []
    rotXYZ (a, b, c) s = Rz c (Ry b (Rx a s))
    ns w tupleF axes = do
      _ <- keyword w
      choice
        ( [ try (symbol ("." ++ ax)) *> (axF <$> num ctx <*> shapeArg ctx)
          | (ax, axF) <- axes
          ]
            ++ [tupleF <$> tuple3 ctx <*> shapeArg ctx]
        )
    nsMirror = do
      _ <- keyword "Mirror"
      choice
        [ try (symbol ".x") *> (Mirror (1, 0, 0) <$> shapeArg ctx)
        , try (symbol ".y") *> (Mirror (0, 1, 0) <$> shapeArg ctx)
        , try (symbol ".z") *> (Mirror (0, 0, 1) <$> shapeArg ctx)
        , Mirror <$> tuple3 ctx <*> shapeArg ctx
        ]
    bin2 w f = do
      _ <- keyword w
      a <- primaryExpression ctx
      f a <$> shapeArg ctx

-- Parse primary expressions (shapes, variables, parentheses)
-- NOTE: shapes come before variables so keyword primitives (xcyl etc.)
-- are not swallowed by the variable parser.
primaryExpression :: Ctx -> Parser Shape
primaryExpression ctx =
  choice
    ( [parenthesized]
        ++ (if glyphOK ctx then [basicShape, bosl2Shape] else [])
        ++ [wordShape]
        ++ (if simpleOK ctx then [simpleShape] else [])
        ++ (if glyphOK ctx then [shape2D] else [])
        ++ [variable]
    )
    <?> pLabel
  where
    pLabel = case cMode ctx of
      ModeGlyph -> "a shape (■ ● ◎ ▻ ▬ ⎏ ▣ ◙ ⌭ ⌽ ⊚ ⏢ ◉ ⊿ △ ⬠ ⭘ ✎ or a word shape), a variable, or '('"
      ModeSimple -> "a shape (Sphere Cube Box Cylinder Cone Tube Torus Wedge Prismoid Circle Triangle Pentagon Bezier or a word shape), a variable, or '('"
      ModeLegacy -> "a shape (glyph or word), a variable, or '('"
    varTable = cVars ctx
    parenthesized = between (symbol "(") (symbol ")") (expression ctx)

    -- an unknown name is reported at the identifier itself, not after it
    variable = do
      o <- getOffset
      name <- identifier
      case Map.lookup name varTable of
        Just shape -> return shape
        Nothing
          | Map.member name (cNums ctx) -> setOffset o *> fail ("'" ++ name ++ "' is a number, not a shape (numbers are used as arguments: ● " ++ name ++ ", box " ++ name ++ " ...)")
          | otherwise -> setOffset o *> fail (undefPfx ++ "'" ++ name ++ "'" ++ suggestion (Map.keys varTable ++ reservedNames (cMode ctx)) name)

    -- !simple capitalized shape words
    simpleShape =
      choice
        [ keyword "Sphere" *> (Sphere <$> num ctx)
        , keyword "Cube" *> ((\s -> Cuboid (s, s, s) 0 0) <$> num ctx)
        , keyword "Box" *> ((\a b c -> Cuboid (a, b, c) 0 0) <$> arg ctx "Box" "width" <*> arg ctx "Box" "depth" <*> arg ctx "Box" "height")
        , keyword "Cylinder" *> ((\r h -> Cyl r h 0 0) <$> num ctx <*> num ctx)
        , keyword "Cone" *> (Cone <$> num ctx <*> num ctx)
        , keyword "Tube" *> (Tube <$> num ctx <*> num ctx <*> num ctx)
        , keyword "Torus" *> (Torus <$> num ctx <*> num ctx)
        , keyword "Wedge" *> ((\a b c -> Wedge (a, b, c)) <$> num ctx <*> num ctx <*> num ctx)
        , keyword "Prismoid" *> ((\a b c d h -> Prismoid (a, b) (c, d) h) <$> num ctx <*> num ctx <*> num ctx <*> num ctx <*> num ctx)
        , keyword "Circle" *> (Shape2D 100 <$> num ctx)
        , keyword "Triangle" *> (Shape2D 3 <$> num ctx)
        , keyword "Pentagon" *> (Shape2D 5 <$> num ctx)
        , keyword "Bezier" *> bezierBody ctx
        ]

    basicShape =
      choice
        [cube, sphere, cylinder, cone, rectangle, prism]

    cube = do
      _ <- symbol "■"
      size <- num ctx
      return $ Rectangle size size size

    sphere = do
      _ <- symbol "●"
      Sphere <$> num ctx

    cylinder = do
      _ <- symbol "◎"
      radius <- num ctx
      Cylinder radius <$> num ctx

    cone = do
      _ <- symbol "▻"
      radius <- num ctx
      Cone radius <$> num ctx

    rectangle = do
      _ <- symbol "▬"
      x <- arg ctx "box / ▬" "width"
      y <- arg ctx "box / ▬" "depth"
      Rectangle x y <$> arg ctx "box / ▬" "height"

    prism = do
      _ <- symbol "⎏"
      off <- getOffset
      n <- num ctx
      if n >= 3 && n <= fromIntegral maxSides && n == fromInteger (truncate n)
        then return ()
        else setOffset off *> fail ("prism side count must be a whole number from 3 to " ++ show maxSides)
      radius <- num ctx
      Prism (round n) radius <$> num ctx

    -- BOSL2 primitives (all centered, matching BOSL2 defaults) ----
    bosl2Shape =
      choice
        [ cuboidChamfer,
          cuboidRound,
          cylChamfer,
          cylRound,
          tubeShape,
          prismoidShape,
          torusShape,
          wedgeShape
        ]

    cuboidChamfer = do
      _ <- symbol "▣"
      x <- num ctx
      y <- num ctx
      z <- num ctx
      c <- num ctx
      return $ Cuboid (x, y, z) c 0

    cuboidRound = do
      _ <- symbol "◙"
      x <- num ctx
      y <- num ctx
      z <- num ctx
      r <- num ctx
      return $ Cuboid (x, y, z) 0 r

    cylChamfer = do
      _ <- symbol "⌭"
      r <- num ctx
      h <- num ctx
      c <- num ctx
      return $ Cyl r h c 0

    cylRound = do
      _ <- symbol "⌽"
      r <- num ctx
      h <- num ctx
      ro <- num ctx
      return $ Cyl r h 0 ro

    tubeShape = do
      _ <- symbol "⊚"
      ro <- num ctx
      ri <- num ctx
      Tube ro ri <$> num ctx

    prismoidShape = do
      _ <- symbol "⏢"
      x1 <- num ctx
      y1 <- num ctx
      x2 <- num ctx
      y2 <- num ctx
      Prismoid (x1, y1) (x2, y2) <$> num ctx

    torusShape = do
      _ <- symbol "◉"
      rj <- num ctx
      Torus rj <$> num ctx

    wedgeShape = do
      _ <- symbol "⊿"
      x <- num ctx
      y <- num ctx
      z <- num ctx
      return $ Wedge (x, y, z)

    xcylShape = do
      _ <- keyword "xcyl"
      r <- num ctx
      XCyl r <$> num ctx

    ycylShape = do
      _ <- keyword "ycyl"
      r <- num ctx
      YCyl r <$> num ctx

    zcylShape = do
      _ <- keyword "zcyl"
      r <- num ctx
      ZCyl r <$> num ctx

    -- word-named shapes (all BOSL2-centered family) — available in every mode
    wordShape =
      choice
        [ xcylShape
        , ycylShape
        , zcylShape
        , keyword "cube" *> ((\s -> Cuboid (s, s, s) 0 0) <$> num ctx)
        , keyword "box" *> ((\a b c -> Cuboid (a, b, c) 0 0) <$> arg ctx "box" "width" <*> arg ctx "box" "depth" <*> arg ctx "box" "height")
        , keyword "sphere" *> (Sphere <$> num ctx)
        , keyword "cyl" *> ((\r h -> Cyl r h 0 0) <$> num ctx <*> num ctx)
        , keyword "tube" *> (Tube <$> num ctx <*> num ctx <*> num ctx)
        , keyword "torus" *> (Torus <$> num ctx <*> num ctx)
        , keyword "wedge" *> ((\a b c -> Wedge (a, b, c)) <$> num ctx <*> num ctx <*> num ctx)
        ]

    shape2D =
      choice
        [triangle, pentagon, circle, bezier]

    bezier = symbol "✎" *> bezierBody ctx

    triangle = do
      _ <- symbol "△"
      Shape2D 3 <$> num ctx

    pentagon = do
      _ <- symbol "⬠"
      Shape2D 5 <$> num ctx

    circle = do
      _ <- symbol "⭘"
      Shape2D 100 <$> num ctx

-- | Shared body for ✎ / Bezier: 3k+1 control points as x y pairs
bezierBody :: Ctx -> Parser Shape
bezierBody ctx = do
  ns <- some (num ctx)
  let n = length ns
  if odd n || n < 8 || (n `div` 2) `mod` 3 /= 1
    then fail ("bezier needs 3k+1 control points as x y pairs (got " ++ show n ++ " numbers)")
    else return (bezPoly 24 (pairUp ns))
  where
    pairUp (a : b : r) = (a, b) : pairUp r
    pairUp _ = []

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

-- | Keywords that take precedence over variable lookup in this mode.
reservedNames :: SynMode -> [String]
reservedNames mode =
  ["cube", "box", "sphere", "cyl", "tube", "torus", "wedge", "xcyl", "ycyl", "zcyl", "loft"]
    ++ if mode == ModeGlyph then [] else
      [ "Sphere", "Cube", "Box", "Cylinder", "Cone", "Tube", "Torus", "Wedge"
      , "Prismoid", "Circle", "Triangle", "Pentagon", "Bezier"
      , "Translate", "Rotate", "Scale", "Mirror", "Extrude", "Loft", "Anchor"
      , "Hull", "Union", "Intersect", "Minkowski", "Offset"
      ]

-- Utility functions
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
