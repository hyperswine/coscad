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

-- | Syntax mode, chosen by a pragma on the first code line of a file:
--   !glyph  — glyph operators/shapes (plus legacy lowercase words)
--   !simple — ASCII word set: Translate/Rotate/Scale/Mirror (with .x/.y/.z
--             and (x, y, z) tuple forms), Hull/Intersect/Minkowski/Offset,
--             capitalized shape words, and '*' / '-' for union / difference.
-- No pragma = legacy mode: both sets accepted (backwards compatible).
-- '$' (Haskell-style loose application) and |> pipelines work in ALL modes.
data SynMode = ModeLegacy | ModeGlyph | ModeSimple deriving (Eq, Show)

data Ctx = Ctx {cMode :: SynMode, cVars :: VarTable}

glyphOK, simpleOK :: Ctx -> Bool
glyphOK c = cMode c /= ModeSimple
simpleOK c = cMode c /= ModeGlyph

-- Try to resolve a single variable
tryResolveVar :: SynMode -> VarTable -> (VarName, String) -> Either (VarName, String) (VarName, Shape)
tryResolveVar mode table (name, expr) =
  let cleanExpr = trim expr
   in case parseExpression (Ctx mode table) cleanExpr of
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
  let (mode, body) = splitPragma input
   in case parse (sc *> program mode <* eof) "" body of
        Left err -> Left (errorBundlePretty err)
        Right result -> result

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
program :: SynMode -> Parser (Either String (VarTable, Shape))
program mode = do
  varDefs <- many variableDefinition
  return $ do
    varTable <- resolveVariables mode varDefs Map.empty
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
pipeExpression varTable = do
  left <- booleanExpression varTable
  rest <- many (symbol "|>" *> pipeStage varTable)
  return (foldl (flip ($)) left rest)

pipeStage :: Ctx -> Parser (Shape -> Shape)
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
      child <- shapeArg vt
      return (\p -> f v off p child)
    bin w f = try $ do
      keyword w
      s <- shapeArg vt
      return (`f` s)

-- Parse boolean expressions (union, difference, hull, minkowski, offset)
-- Glyph operators in !glyph/legacy; '*' (union) and '-' (difference)
-- in !simple/legacy.
booleanExpression :: Ctx -> Parser Shape
booleanExpression ctx = do
  left <- attachExpression ctx
  rest <- many $ do
    op <- choice (gOps ++ sOps)
    right <- attachExpression ctx
    return (op, right)
  return $ foldl applyBooleanOp left rest
  where
    gOps = if glyphOK ctx then map symbol ["⊖", "⊝", "⊕", "⊛", "∩", "⇓", "⊞", "↯"] else []
    sOps = if simpleOK ctx then map symbol ["*", "-"] else []
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
transformExpression ctx = transformation ctx <|> primaryExpression ctx

-- | A comma tuple: (x, y, z)
tuple3 :: Parser (Double, Double, Double)
tuple3 = try $ between (symbol "(") (symbol ")") $ do
  a <- double
  _ <- symbol ","
  b <- double
  _ <- symbol ","
  c <- double
  return (a, b, c)

-- Parse transformations
transformation :: Ctx -> Parser Shape
transformation ctx =
  choice (glyphTs ++ simpleTs)
  where
    glyphTs
      | glyphOK ctx =
          [ g1 "χ" Tx, g1 "ψ" Ty, g1 "ζ" Tz
          , g1 "θ" Rx, g1 "ϕ" Ry, g1 "ω" Rz
          , g3 "⬈" Scale, g3 "⇋" Mirror
          , g1 "⮕" Extrude
          , do
              symbol "⚓"
              v <- anchorVec
              Anchor v <$> shapeArg ctx
          ]
      | otherwise = []
    g1 s f = do
      _ <- symbol s
      n <- double
      f n <$> shapeArg ctx
    g3 s f = do
      _ <- symbol s
      a <- double
      b <- double
      c <- double
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
          , try (keyword "Extrude" *> (Extrude <$> double <*> shapeArg ctx))
          , try (keyword "Anchor" *> (Anchor <$> anchorVec <*> shapeArg ctx))
          , bin2 "Hull" (\a b -> Hull [a, b])
          , bin2 "Union" (\a b -> Union [a, b])
          , bin2 "Intersect" (\a b -> Intersection [a, b])
          , bin2 "Minkowski" (\a b -> Minkowski [a, b])
          , try (keyword "Offset" *> ((\n s -> Offset n s) <$> double <*> shapeArg ctx))
          ]
      | otherwise = []
    rotXYZ (a, b, c) s = Rz c (Ry b (Rx a s))
    ns w tupleF axes = try $ do
      _ <- keyword w
      choice
        ( [ try (symbol ("." ++ ax) *> (axF <$> double <*> shapeArg ctx))
          | (ax, axF) <- axes
          ]
            ++ [tupleF <$> tuple3 <*> shapeArg ctx]
        )
    nsMirror = try $ do
      _ <- keyword "Mirror"
      choice
        [ try (symbol ".x" *> (Mirror (1, 0, 0) <$> shapeArg ctx))
        , try (symbol ".y" *> (Mirror (0, 1, 0) <$> shapeArg ctx))
        , try (symbol ".z" *> (Mirror (0, 0, 1) <$> shapeArg ctx))
        , Mirror <$> tuple3 <*> shapeArg ctx
        ]
    bin2 w f = try $ do
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
  where
    varTable = cVars ctx
    parenthesized = between (symbol "(") (symbol ")") (expression ctx)

    variable = do
      name <- identifier
      case Map.lookup name varTable of
        Just shape -> return shape
        Nothing -> fail $ "Undefined variable: " ++ name

    -- !simple capitalized shape words
    simpleShape =
      choice
        [ try (keyword "Sphere" *> (Sphere <$> double))
        , try (keyword "Cube" *> ((\s -> Cuboid (s, s, s) 0 0) <$> double))
        , try (keyword "Box" *> ((\a b c -> Cuboid (a, b, c) 0 0) <$> double <*> double <*> double))
        , try (keyword "Cylinder" *> ((\r h -> Cyl r h 0 0) <$> double <*> double))
        , try (keyword "Cone" *> (Cone <$> double <*> double))
        , try (keyword "Tube" *> (Tube <$> double <*> double <*> double))
        , try (keyword "Torus" *> (Torus <$> double <*> double))
        , try (keyword "Wedge" *> ((\a b c -> Wedge (a, b, c)) <$> double <*> double <*> double))
        , try (keyword "Prismoid" *> ((\a b c d h -> Prismoid (a, b) (c, d) h) <$> double <*> double <*> double <*> double <*> double))
        , try (keyword "Circle" *> (Shape2D 100 <$> double))
        , try (keyword "Triangle" *> (Shape2D 3 <$> double))
        , try (keyword "Pentagon" *> (Shape2D 5 <$> double))
        , try (keyword "Bezier" *> bezierBody)
        ]

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
          wedgeShape
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

    -- word-named shapes (all BOSL2-centered family) — available in every mode
    wordShape =
      choice
        [ xcylShape
        , ycylShape
        , zcylShape
        , try (keyword "cube" *> ((\s -> Cuboid (s, s, s) 0 0) <$> double))
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

    bezier = symbol "✎" *> bezierBody

    triangle = do
      symbol "△"
      Shape2D 3 <$> double

    pentagon = do
      symbol "⬠"
      Shape2D 5 <$> double

    circle = do
      symbol "⭘"
      Shape2D 100 <$> double

-- | Shared body for ✎ / Bezier: 3k+1 control points as x y pairs
bezierBody :: Parser Shape
bezierBody = do
  ns <- some double
  let n = length ns
  if odd n || n < 8 || (n `div` 2) `mod` 3 /= 1
    then fail ("bezier needs 3k+1 control points as x y pairs (got " ++ show n ++ " numbers)")
    else return (bezPoly 24 (pairUp ns))
  where
    pairUp (a : b : r) = (a, b) : pairUp r
    pairUp _ = []

-- Resolve variables with dependency resolution
resolveVariables :: SynMode -> [(VarName, String)] -> VarTable -> Either String VarTable
resolveVariables _ [] table = Right table
resolveVariables mode remaining table = do
  let results = map (tryResolveVar mode table) remaining
  let resolved = rights results
  let unresolved = lefts results

  if null resolved
    then Left $ "Cannot resolve variables (possible circular dependency): " ++ show (map fst unresolved)
    else do
      let newTable = foldl (\acc (name, shape) -> Map.insert name shape acc) table resolved
      resolveVariables mode unresolved newTable

-- Utility functions
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
