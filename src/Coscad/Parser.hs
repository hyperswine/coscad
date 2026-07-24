-- | Parser for .coscad files: glyph shapes, word shapes, boolean
-- operators, attachment ops, and |> pipelines with multi-line
-- continuation. Also the variable-resolution pass.
module Coscad.Parser (module Coscad.Parser) where

import Control.Monad (void)
import Coscad.Shape
import Data.Char (isSpace)
import Data.Either (lefts, rights)
import qualified Data.Map as Map
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

getOffsetValue :: Shape -> Double
getOffsetValue (Sphere r) = r
getOffsetValue (Cylinder r _) = r
getOffsetValue (Shape2D _ r) = r
getOffsetValue _ = 1.0 -- Default offset value

-- Try to resolve a single variable
tryResolveVar :: VarTable -> (VarName, String) -> Either (VarName, String) (VarName, Shape)
tryResolveVar table (name, expr) =
  let cleanExpr = trim expr
   in case parseExpression table cleanExpr of
        Right shape -> Right (name, shape)
        Left err -> Left (name, cleanExpr)

type VarName = String

type VarTable = Map.Map VarName Shape

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

-- Parse a complete program
parseProgram :: String -> Either String (VarTable, Shape)
parseProgram input =
  case parse (sc *> program <* eof) "" input of
    Left err -> Left (errorBundlePretty err)
    Right result -> result

-- Parse the entire program
program :: Parser (Either String (VarTable, Shape))
program = do
  varDefs <- many variableDefinition
  return $ do
    varTable <- resolveVariables varDefs Map.empty
    case Map.lookup "main" varTable of
      Just mainShape -> Right (varTable, mainShape)
      Nothing -> Left "Error: No 'main' variable found. Please define a 'main' variable."

-- Parse a variable definition
variableDefinition :: Parser (VarName, String)
variableDefinition = do
  name <- identifier
  symbol "="
  expr <- expressionString
  return (name, expr)

-- Get the remaining expression as a string (for later parsing with context)
expressionString :: Parser String
expressionString = lexeme $ do
  -- Parse until end of line; a following line beginning with |>
  -- continues the same expression (multi-line pipelines)
  first <- manyTill anySingle (try (void newline) <|> eof)
  rest <- many contLine
  return (trim (unwords (first : rest)))
  where
    contLine = try $ do
      _ <- many (char ' ' <|> char '\t')
      _ <- lookAhead (string "|>")
      manyTill anySingle (try (void newline) <|> eof)

-- Parse an expression with variable context
parseExpression :: VarTable -> String -> Either String Shape
parseExpression varTable input =
  case parse (sc *> expression varTable <* eof) "" input of
    Left err -> Left (errorBundlePretty err)
    Right shape -> Right shape

-- Parse an expression (pipelines bind loosest)
expression :: VarTable -> Parser Shape
expression = pipeExpression

-- |> pipelines: each stage is a postfix operation on the shape so far.
--   plate |> at top 5.5 0 -5 flange |> cutat lft 3.5 0 0 (xcyl 2.7 12)
pipeExpression :: VarTable -> Parser Shape
pipeExpression varTable = do
  left <- booleanExpression varTable
  rest <- many (symbol "|>" *> pipeStage varTable)
  return (foldl (flip ($)) left rest)

pipeStage :: VarTable -> Parser (Shape -> Shape)
pipeStage vt =
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
    , try (keyword "anchor" *> (Anchor <$> anchorVec))
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
    num1 w f = try (keyword w *> (f <$> double))
    num3 w f = try (keyword w *> (f <$> ((,,) <$> double <*> double <*> double)))
    rel w f = try $ do
      keyword w
      v <- anchorVec
      off <- option (0, 0, 0) (try ((,,) <$> double <*> double <*> double))
      child <- primaryExpression vt
      return (\p -> f v off p child)
    bin w f = try $ do
      keyword w
      s <- primaryExpression vt
      return (`f` s)

-- Parse boolean expressions (union, difference, hull, minkowski, offset)
booleanExpression :: VarTable -> Parser Shape
booleanExpression varTable = do
  left <- attachExpression varTable
  rest <- many $ do
    op <- symbol "⊖" <|> symbol "⊝" <|> symbol "⊕" <|> symbol "⊛" <|> symbol "∩" <|> symbol "⇓" <|> symbol "⊞" <|> symbol "↯"
    right <- attachExpression varTable
    return (op, right)
  return $ foldl applyBooleanOp left rest
  where
    applyBooleanOp left ("⊖", right) = Diff left right
    applyBooleanOp left ("⊝", right) = Diff left right
    applyBooleanOp left ("⊕", right) = Union [left, right]
    applyBooleanOp left ("⊛", right) = Union [left, right]
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
attachExpression :: VarTable -> Parser Shape
attachExpression varTable = do
  left <- transformExpression varTable
  rest <- many step
  return $ foldl (flip ($)) left rest
  where
    step =
      choice
        [ do
            symbol "⌖"
            v <- anchorVec
            right <- transformExpression varTable
            return (\l -> Position v (0, 0, 0) l right),
          do
            symbol "⋈"
            v <- anchorVec
            right <- transformExpression varTable
            return (\l -> AttachTo v (0, 0, 0) l right)
        ]

-- Parse transformation expressions
transformExpression :: VarTable -> Parser Shape
transformExpression varTable = transformation varTable <|> primaryExpression varTable

-- Parse transformations
transformation :: VarTable -> Parser Shape
transformation varTable =
  choice
    [ translateX,
      translateY,
      translateZ,
      rotateX,
      rotateY,
      rotateZ,
      scaleTransform,
      mirrorTransform,
      anchorTransform,
      extrudeTransform
    ]
  where
    anchorTransform = do
      symbol "⚓"
      v <- anchorVec
      shape <- primaryExpression varTable
      return $ Anchor v shape

    translateX = do
      symbol "χ"
      dx <- double
      shape <- primaryExpression varTable
      return $ Tx dx shape

    translateY = do
      symbol "ψ"
      dy <- double
      shape <- primaryExpression varTable
      return $ Ty dy shape

    translateZ = do
      symbol "ζ"
      dz <- double
      shape <- primaryExpression varTable
      return $ Tz dz shape

    rotateX = do
      symbol "θ"
      ax <- double
      shape <- primaryExpression varTable
      return $ Rx ax shape

    rotateY = do
      symbol "ϕ"
      ay <- double
      shape <- primaryExpression varTable
      return $ Ry ay shape

    rotateZ = do
      symbol "ω"
      az <- double
      shape <- primaryExpression varTable
      return $ Rz az shape

    scaleTransform = do
      symbol "⬈"
      sx <- double
      sy <- double
      sz <- double
      shape <- primaryExpression varTable
      return $ Scale (sx, sy, sz) shape

    mirrorTransform = do
      symbol "⇋"
      mx <- double
      my <- double
      mz <- double
      shape <- primaryExpression varTable
      return $ Mirror (mx, my, mz) shape

    extrudeTransform = do
      symbol "⮕"
      h <- double
      shape <- primaryExpression varTable
      return $ Extrude h shape

-- Parse primary expressions (shapes, variables, parentheses)
-- NOTE: shapes come before variables so keyword primitives (xcyl etc.)
-- are not swallowed by the variable parser.
primaryExpression :: VarTable -> Parser Shape
primaryExpression varTable =
  choice
    [ parenthesized,
      basicShape,
      bosl2Shape,
      wordShape,
      shape2D,
      variable
    ]
  where
    parenthesized = between (symbol "(") (symbol ")") (expression varTable)

    variable = do
      name <- identifier
      case Map.lookup name varTable of
        Just shape -> return shape
        Nothing -> fail $ "Undefined variable: " ++ name

    basicShape =
      choice
        [cube, sphere, cylinder, cone, rectangle, prism]

    cube = do
      symbol "■"
      size <- double
      return $ Rectangle size size size

    sphere = do
      symbol "●"
      Sphere <$> double

    cylinder = do
      symbol "◎"
      radius <- double
      Cylinder radius <$> double

    cone = do
      symbol "▻"
      radius <- double
      Cone radius <$> double

    rectangle = do
      symbol "▬"
      x <- double
      y <- double
      Rectangle x y <$> double

    prism = do
      symbol "⎏"
      n <- double
      radius <- double
      Prism (round n) radius <$> double

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
          wedgeShape,
          xcylShape,
          ycylShape,
          zcylShape
        ]

    cuboidChamfer = do
      symbol "▣"
      x <- double
      y <- double
      z <- double
      c <- double
      return $ Cuboid (x, y, z) c 0

    cuboidRound = do
      symbol "◙"
      x <- double
      y <- double
      z <- double
      r <- double
      return $ Cuboid (x, y, z) 0 r

    cylChamfer = do
      symbol "⌭"
      r <- double
      h <- double
      c <- double
      return $ Cyl r h c 0

    cylRound = do
      symbol "⌽"
      r <- double
      h <- double
      ro <- double
      return $ Cyl r h 0 ro

    tubeShape = do
      symbol "⊚"
      ro <- double
      ri <- double
      Tube ro ri <$> double

    prismoidShape = do
      symbol "⏢"
      x1 <- double
      y1 <- double
      x2 <- double
      y2 <- double
      Prismoid (x1, y1) (x2, y2) <$> double

    torusShape = do
      symbol "◉"
      rj <- double
      Torus rj <$> double

    wedgeShape = do
      symbol "⊿"
      x <- double
      y <- double
      z <- double
      return $ Wedge (x, y, z)

    xcylShape = do
      keyword "xcyl"
      r <- double
      XCyl r <$> double

    ycylShape = do
      keyword "ycyl"
      r <- double
      YCyl r <$> double

    zcylShape = do
      keyword "zcyl"
      r <- double
      ZCyl r <$> double

    -- word-named shapes (all BOSL2-centered family)
    wordShape =
      choice
        [ try (keyword "cube" *> ((\s -> Cuboid (s, s, s) 0 0) <$> double))
        , try (keyword "box" *> ((\a b c -> Cuboid (a, b, c) 0 0) <$> double <*> double <*> double))
        , try (keyword "sphere" *> (Sphere <$> double))
        , try (keyword "cyl" *> ((\r h -> Cyl r h 0 0) <$> double <*> double))
        , try (keyword "tube" *> (Tube <$> double <*> double <*> double))
        , try (keyword "torus" *> (Torus <$> double <*> double))
        , try (keyword "wedge" *> ((\a b c -> Wedge (a, b, c)) <$> double <*> double <*> double))
        ]

    shape2D =
      choice
        [triangle, pentagon, circle, bezier]

    bezier = do
      symbol "✎"
      ns <- some double
      let n = length ns
      if odd n || n < 8 || (n `div` 2) `mod` 3 /= 1
        then fail ("✎ needs 3k+1 control points as x y pairs (got " ++ show n ++ " numbers)")
        else return (bezPoly 24 (pairUp ns))
      where
        pairUp (a : b : r) = (a, b) : pairUp r
        pairUp _ = []

    triangle = do
      symbol "△"
      Shape2D 3 <$> double

    pentagon = do
      symbol "⬠"
      Shape2D 5 <$> double

    circle = do
      symbol "⭘"
      Shape2D 100 <$> double

-- Resolve variables with dependency resolution
resolveVariables :: [(VarName, String)] -> VarTable -> Either String VarTable
resolveVariables [] table = Right table
resolveVariables remaining table = do
  let results = map (tryResolveVar table) remaining
  let resolved = rights results
  let unresolved = lefts results

  if null resolved
    then Left $ "Cannot resolve variables (possible circular dependency): " ++ show (map fst unresolved)
    else do
      let newTable = foldl (\acc (name, shape) -> Map.insert name shape acc) table resolved
      resolveVariables unresolved newTable

-- Utility functions
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
