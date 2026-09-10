-- | Parser for .coscad files: glyph shapes, word shapes, boolean
-- operators, attachment ops, and |> pipelines with multi-line
-- continuation. Also the variable-resolution pass.
module Coscad.Parser (module Coscad.Parser) where

import Control.Monad (void)
import Coscad.Codegen (loftErrors)
import Coscad.Dim (Dim (..), dimOf)
import Coscad.Shape
import Data.Char (isSpace)
import Data.Either (lefts, rights)
import Data.List (dropWhileEnd, foldl', intercalate, isPrefixOf)
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
    Right shape -> case dimOf shape >> loftErrors shape of
      Right _ -> Right (defName d, shape)
      Left msg -> Left (d, FSyntax (sourcePosPretty (defPos d) ++ ": in '" ++ defName d ++ "': " ++ msg))
    Left err -> case undefinedVarIn err of
      Just v -> Left (d, FMissing v (errorBundlePretty err))
      Nothing -> Left (d, FSyntax (errorBundlePretty err))

type VarName = String

type VarTable = Map.Map VarName Shape

-- | Numeric bindings: `w = 20`, `r = w / 2 - 1`
type NumTable = Map.Map VarName Double

type Parser = Parsec Void String

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
double = lexeme $ L.signed sc (try L.float <|> fromIntegral <$> L.decimal)

-- NUMBERS -------------------------------------------------------
-- A numeric argument is a single atom: a literal, a numeric binding's
-- name, a parenthesized arithmetic expression, or a negated atom.
--   box w (h / 2) t      χ (w / 2 + 1) part      ● -r
-- Arithmetic (+ - * /, parens, unary minus) lives inside parentheses
-- or on the right-hand side of a numeric definition, so juxtaposed
-- arguments stay unambiguous.
num :: Ctx -> Parser Double
num ctx =
  choice
    [ try double
    , try (between (symbol "(") (symbol ")") (numExpr ctx))
    , try (symbol "-" *> (negate <$> num ctx))
    , try (numName ctx)
    ]
    <?> "a number (literal, numeric binding, or parenthesized arithmetic)"

numExpr :: Ctx -> Parser Double
numExpr ctx = do
  t <- term
  rest <- many ((,) <$> (symbol "+" <|> symbol "-") <*> term)
  return (foldl (\a (o, b) -> if o == "+" then a + b else a - b) t rest)
  where
    term = do
      f <- factor
      rest <- many ((,) <$> (symbol "*" <|> symbol "/") <*> factor)
      return (foldl (\a (o, b) -> if o == "*" then a * b else a / b) f rest)
    factor = (symbol "-" *> (negate <$> factor)) <|> atom
    atom =
      choice
        [ lexeme (try L.float <|> fromIntegral <$> L.decimal)
        , between (symbol "(") (symbol ")") (numExpr ctx)
        , numName ctx
        ]
        <?> "a number, a numeric binding, or '('"

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
  let (mode, body) = splitPragma input
   in case parse (sc *> program file mode <* eof) file body of
        Left err -> Left (errorBundlePretty err)
        Right result -> result

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
      | trim l == "!simple" = (ModeSimple, unlines (acc ++ ("" : rest)))
      | trim l == "!glyph" = (ModeGlyph, unlines (acc ++ ("" : rest)))
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
  (pre, name) <- match (identifier <* symbol "=")
  pos <- getSourcePos
  expr <- expressionString
  return (Def name expr pos (linePrefix start pre))

-- | The raw source text of an expression, for later parsing with
-- context: the rest of the line, plus any following lines that begin
-- with |> (multi-line pipelines). Newlines are preserved so error
-- positions inside the expression map back to real file lines, and
-- a trailing // comment cannot swallow the continuation lines.
expressionString :: Parser String
expressionString = lexeme (dropWhileEnd isSpace . fst <$> match (firstLine *> many contLine))
  where
    eol = try (void newline) <|> eof
    firstLine = manyTill anySingle eol
    contLine = try $ do
      _ <- many (char ' ' <|> char '\t')
      _ <- lookAhead (string "|>")
      manyTill anySingle eol

-- Parse an expression with variable context
parseExpression :: Ctx -> String -> Either String Shape
parseExpression ctx input =
  case parse (sc *> expression ctx <* eof) "" input of
    Left err -> Left (errorBundlePretty err)
    Right shape -> Right shape

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
  rest <- many (symbol "|>" *> pipeStage ctx)
  return (foldl (flip ($)) left rest)

pipeStage :: Ctx -> Parser (Shape -> Shape)
pipeStage ctx =
  choice
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
    , keyword "anchor" *> (Anchor <$> anchorVec)
    , keyword "loft" *> (loftStage <$> num ctx <*> primaryExpression ctx)
    , rel "at" Position
    , rel "on" AttachTo
    , rel "cutat" CutAt
    , bin "add" (\p s -> Union [p, s])
    , bin "cut" Diff
    , bin "isect" (\p s -> Intersection [p, s])
    , bin "hull" (\p s -> Hull [p, s])
    , bin "mink" (\p s -> Minkowski [p, s])
    ]
  where
    -- `keyword` backtracks by itself when the word does not match; the
    -- stage body is deliberately NOT under `try`, so an error inside a
    -- stage is reported where it happened (megaparsec >= 9.7 reports a
    -- failed `try` at its start).
    num1 w f = keyword w *> (f <$> num ctx)
    num3 w f = keyword w *> (f <$> ((,,) <$> num ctx <*> num ctx <*> num ctx))
    rel w f = do
      keyword w
      v <- anchorVec
      off <- option (0, 0, 0) (try ((,,) <$> num ctx <*> num ctx <*> num ctx))
      child <- shapeArg ctx
      return (\p -> f v off p child)
    bin w f = do
      keyword w
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
  ws <- sepBy1 anchorWord (symbol "+")
  let (xs, ys, zs) = unzip3 ws
  return (cl (sum xs), cl (sum ys), cl (sum zs))
  where
    cl = max (-1) . min 1
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
              symbol "⌖"
              v <- anchorVec
              right <- transformExpression ctx
              return (\l -> Position v (0, 0, 0) l right),
            do
              symbol "⋈"
              v <- anchorVec
              right <- transformExpression ctx
              return (\l -> AttachTo v (0, 0, 0) l right)
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
              symbol "⚓"
              v <- anchorVec
              Anchor v <$> shapeArg ctx
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
          , keyword "Anchor" *> (Anchor <$> anchorVec <*> shapeArg ctx)
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
          | otherwise -> setOffset o *> fail (undefPfx ++ "'" ++ name ++ "'")

    -- !simple capitalized shape words
    simpleShape =
      choice
        [ keyword "Sphere" *> (Sphere <$> num ctx)
        , keyword "Cube" *> ((\s -> Cuboid (s, s, s) 0 0) <$> num ctx)
        , keyword "Box" *> ((\a b c -> Cuboid (a, b, c) 0 0) <$> num ctx <*> num ctx <*> num ctx)
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
      symbol "■"
      size <- num ctx
      return $ Rectangle size size size

    sphere = do
      symbol "●"
      Sphere <$> num ctx

    cylinder = do
      symbol "◎"
      radius <- num ctx
      Cylinder radius <$> num ctx

    cone = do
      symbol "▻"
      radius <- num ctx
      Cone radius <$> num ctx

    rectangle = do
      symbol "▬"
      x <- num ctx
      y <- num ctx
      Rectangle x y <$> num ctx

    prism = do
      symbol "⎏"
      n <- num ctx
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
      symbol "▣"
      x <- num ctx
      y <- num ctx
      z <- num ctx
      c <- num ctx
      return $ Cuboid (x, y, z) c 0

    cuboidRound = do
      symbol "◙"
      x <- num ctx
      y <- num ctx
      z <- num ctx
      r <- num ctx
      return $ Cuboid (x, y, z) 0 r

    cylChamfer = do
      symbol "⌭"
      r <- num ctx
      h <- num ctx
      c <- num ctx
      return $ Cyl r h c 0

    cylRound = do
      symbol "⌽"
      r <- num ctx
      h <- num ctx
      ro <- num ctx
      return $ Cyl r h 0 ro

    tubeShape = do
      symbol "⊚"
      ro <- num ctx
      ri <- num ctx
      Tube ro ri <$> num ctx

    prismoidShape = do
      symbol "⏢"
      x1 <- num ctx
      y1 <- num ctx
      x2 <- num ctx
      y2 <- num ctx
      Prismoid (x1, y1) (x2, y2) <$> num ctx

    torusShape = do
      symbol "◉"
      rj <- num ctx
      Torus rj <$> num ctx

    wedgeShape = do
      symbol "⊿"
      x <- num ctx
      y <- num ctx
      z <- num ctx
      return $ Wedge (x, y, z)

    xcylShape = do
      keyword "xcyl"
      r <- num ctx
      XCyl r <$> num ctx

    ycylShape = do
      keyword "ycyl"
      r <- num ctx
      YCyl r <$> num ctx

    zcylShape = do
      keyword "zcyl"
      r <- num ctx
      ZCyl r <$> num ctx

    -- word-named shapes (all BOSL2-centered family) — available in every mode
    wordShape =
      choice
        [ xcylShape
        , ycylShape
        , zcylShape
        , keyword "cube" *> ((\s -> Cuboid (s, s, s) 0 0) <$> num ctx)
        , keyword "box" *> ((\a b c -> Cuboid (a, b, c) 0 0) <$> num ctx <*> num ctx <*> num ctx)
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
      symbol "△"
      Shape2D 3 <$> num ctx

    pentagon = do
      symbol "⬠"
      Shape2D 5 <$> num ctx

    circle = do
      symbol "⭘"
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
  let (nums, shapeDefs) = numPhase Map.empty defs
  go shapeDefs table0 nums
  where
    known = Set.fromList (map defName defs ++ Map.keys table0)

    -- A definition whose right-hand side is arithmetic over literals
    -- and already-known numeric bindings is a number, not a shape.
    -- Numbers can only depend on numbers, so they resolve first.
    numPhase nums remaining =
      let step d = case runAt (defPos d) (defPrefix d) (defExpr d) (sc *> numExpr (Ctx mode table0 nums) <* eof) of
            Right v -> Right (defName d, v)
            Left _ -> Left d
          results = map step remaining
          found = rights results
       in if null found
            then (nums, remaining)
            else numPhase (foldl' (\acc (n, v) -> Map.insert n v acc) nums found) (lefts results)

    checkDuplicates =
      case [(a, b) | (i, a) <- zip [(0 :: Int) ..] defs, b <- drop (i + 1) defs, defName a == defName b] of
        ((a, b) : _) ->
          Left (sourcePosPretty (defPos b) ++ ": duplicate definition of '" ++ defName a ++ "' (first defined at " ++ sourcePosPretty (defPos a) ++ ")")
        [] -> case [d | d <- defs, defName d `Map.member` table0] of
          (d : _) -> Left (sourcePosPretty (defPos d) ++ ": '" ++ defName d ++ "' is already the name of a part; pick another name")
          [] -> Right ()

    go [] table _ = Right table
    go remaining table nums =
      let results = map (tryResolveVar mode table nums) remaining
          resolved = rights results
          failed = lefts results
       in if null resolved
            then Left (report failed)
            else go (map fst failed) (foldl' (\acc (n, sh) -> Map.insert n sh acc) table resolved) nums

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

-- Utility functions
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
