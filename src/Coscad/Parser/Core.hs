-- | Parser core: contexts and syntax modes, the lexer, numeric
-- parsers, anchor/frame references and reserved words. Shared by the
-- expression grammar and the program layer; it imports no parser code.
module Coscad.Parser.Core
  ( Parser
  , SynMode (..)
  , Ctx (..)
  , glyphOK
  , simpleOK
  , VarName
  , VarTable
  , NumTable
  , sc
  , lexeme
  , symbol
  , keyword
  , identifier
  , double
  , trim
  , num
  , numExpr
  , numName
  , srcPos
  , unsupportedOpPfx
  , arg
  , revolveSegs
  , anchorVec
  , pointRef
  , frameRef
  , tuple3
  , undefPfx
  , reservedNames
  ) where

import Coscad.Geometry (PointRef (..))
import Coscad.Shape
import Coscad.Stdlib (standardNames)
import Coscad.Validate (maxSides)
import Data.Char (isSpace)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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

undefPfx :: String
undefPfx = "undefined variable "

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

-- Name a missing parameter, while preserving undefined-name and numeric errors.
arg :: Ctx -> String -> String -> Parser Double
arg ctx form param = do
  off <- getOffset
  result <- observing (num ctx)
  case result of
    Right n -> return n
    Left e@FancyError {} -> parseError e
    Left _ -> setOffset off *> fail (form ++ " is missing or has an invalid " ++ param ++ " argument")

-- | A revolve segment count: a whole number from 3 to maxSides.
-- Positioned here so the error points at the count, not the profile.
revolveSegs :: Ctx -> Parser Int
revolveSegs ctx = do
  off <- getOffset
  n <- num ctx
  if n >= 3 && n <= fromIntegral maxSides && n == fromInteger (truncate n)
    then return (round n)
    else setOffset off *> fail ("revolve segments must be a whole number from 3 to " ++ show maxSides)

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

-- | A comma tuple: (x, y, z)
tuple3 :: Ctx -> Parser (Double, Double, Double)
tuple3 ctx = between (symbol "(") (symbol ")") $ do
  a <- num ctx
  _ <- symbol ","
  b <- num ctx
  _ <- symbol ","
  c <- num ctx
  return (a, b, c)

-- | Keywords that take precedence over variable lookup in this mode.
reservedNames :: SynMode -> [String]
reservedNames mode =
  standardNames ++ ["cube", "box", "sphere", "cyl", "tube", "torus", "wedge", "xcyl", "ycyl", "zcyl", "frustum", "pillar", "spike", "prism", "block", "poly", "loft"]
    ++ if mode == ModeGlyph then [] else
      [ "Sphere", "Cube", "Box", "Cylinder", "Cone", "Tube", "Torus", "Wedge"
      , "Prismoid", "Circle", "Triangle", "Pentagon", "Bezier"
      , "Translate", "Rotate", "Scale", "Mirror", "Extrude", "Revolve", "Loft", "Anchor"
      , "Hull", "Union", "Intersect", "Minkowski", "Offset"
      ]

-- Utility functions
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
