module Main (main) where

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Either (lefts, rights)
import qualified Data.Map as Map
import Data.Void (Void)
import Lib
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (dropExtension, takeExtension)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Helper function to extract offset value from a shape (for Offset operation)
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
  -- Parse until end of line or end of input
  expr <- manyTill anySingle (try (void newline) <|> eof)
  return (trim expr)

-- Parse an expression with variable context
parseExpression :: VarTable -> String -> Either String Shape
parseExpression varTable input =
  case parse (sc *> expression varTable <* eof) "" input of
    Left err -> Left (errorBundlePretty err)
    Right shape -> Right shape

-- Parse an expression
expression :: VarTable -> Parser Shape
expression = booleanExpression

-- Parse boolean expressions (union, difference, hull, minkowski, offset)
booleanExpression :: VarTable -> Parser Shape
booleanExpression varTable = do
  left <- transformExpression varTable
  rest <- many $ do
    op <- symbol "⊖" <|> symbol "⊝" <|> symbol "⊕" <|> symbol "⊛" <|> symbol "⇓" <|> symbol "⊞" <|> symbol "↯"
    right <- transformExpression varTable
    return (op, right)
  return $ foldl applyBooleanOp left rest
  where
    applyBooleanOp left ("⊖", right) = Diff left right
    applyBooleanOp left ("⊝", right) = Diff left right
    applyBooleanOp left ("⊕", right) = Union [left, right]
    applyBooleanOp left ("⊛", right) = Union [left, right]
    applyBooleanOp left ("⇓", right) = Hull [left, right]
    applyBooleanOp left ("⊞", right) = Minkowski [left, right]
    applyBooleanOp left ("↯", right) = Offset (getOffsetValue right) left
    applyBooleanOp left (op, _) = error $ "Unknown boolean operator: " ++ op

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
      extrudeTransform
    ]
  where
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

    extrudeTransform = do
      symbol "⮕"
      h <- double
      shape <- primaryExpression varTable
      return $ Extrude h shape

-- Parse primary expressions (shapes, variables, parentheses)
primaryExpression :: VarTable -> Parser Shape
primaryExpression varTable =
  choice
    [ parenthesized,
      variable,
      basicShape,
      shape2D
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

    shape2D =
      choice
        [triangle, pentagon, circle]

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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> processFile inputFile
    _ -> do
      hPutStrLn stderr "Usage: coscad <input.coscad>"
      hPutStrLn stderr "Converts a .coscad file to .scad format"
      hPutStrLn stderr ""
      hPutStrLn stderr "Variable Syntax:"
      hPutStrLn stderr "  c₁ = ■ 10           -- cube variable"
      hPutStrLn stderr "  s₁ = ● 15           -- sphere variable"
      hPutStrLn stderr "  main = c₁ ⊕ s₁      -- main variable (gets rendered)"
      hPutStrLn stderr ""
      hPutStrLn stderr "Basic Shapes:"
      hPutStrLn stderr "  ■ 10           -- cube (10x10x10)"
      hPutStrLn stderr "  ● 15           -- sphere (radius 15)"
      hPutStrLn stderr "  ◎ 5 10         -- cylinder (radius 5, height 10)"
      hPutStrLn stderr "  ▻ 8 12         -- cone (radius 8, height 12)"
      hPutStrLn stderr "  ▬ 5 10 15      -- rectangle (5x10x15)"
      hPutStrLn stderr "  ⎏ 6 8 12       -- prism (6-sides, radius 8, height 12)"
      hPutStrLn stderr ""
      hPutStrLn stderr "2D Profiles:"
      hPutStrLn stderr "  △ 10           -- triangle profile (radius 10)"
      hPutStrLn stderr "  ⬠ 8            -- pentagon profile (radius 8)"
      hPutStrLn stderr "  ⭘ 6            -- circle profile (radius 6)"
      hPutStrLn stderr ""
      hPutStrLn stderr "Boolean Operations:"
      hPutStrLn stderr "  ● 15 ⊖ ◎ 5 10   -- difference (sphere minus cylinder)"
      hPutStrLn stderr "  ● 15 ⊝ ◎ 5 10   -- difference (alternative glyph)"
      hPutStrLn stderr "  ■ 10 ⊕ ● 5     -- union (cube plus sphere)"
      hPutStrLn stderr "  ■ 10 ⊛ ● 5     -- union (alternative glyph)"
      hPutStrLn stderr ""
      hPutStrLn stderr "Advanced Operations:"
      hPutStrLn stderr "  ■ 10 ⇓ ● 5     -- hull (convex hull of cube and sphere)"
      hPutStrLn stderr "  ■ 10 ⊞ ● 5     -- minkowski sum (cube and sphere)"
      hPutStrLn stderr "  △ 8 ↯ ● 2      -- offset (offset triangle by sphere radius)"
      hPutStrLn stderr ""
      hPutStrLn stderr "Transformations:"
      hPutStrLn stderr "  χ 5 (● 3)       -- translate X by 5"
      hPutStrLn stderr "  ψ 10 (■ 4)      -- translate Y by 10"
      hPutStrLn stderr "  ζ 8 (● 2)       -- translate Z by 8"
      hPutStrLn stderr "  θ 45 (▬ 10 5 2) -- rotate X by 45 degrees"
      hPutStrLn stderr "  ϕ 90 (● 5)      -- rotate Y by 90 degrees"
      hPutStrLn stderr "  ω 30 (■ 6)      -- rotate Z by 30 degrees"
      hPutStrLn stderr "  ⬈ 2 1.5 0.5 (● 5) -- scale by (2, 1.5, 0.5)"
      hPutStrLn stderr "  ⮕ 15 (△ 8)      -- extrude triangle by height 15"
      exitFailure

processFile :: FilePath -> IO ()
processFile inputFile = do
  -- Check if input file exists
  exists <- doesFileExist inputFile
  if not exists
    then do
      hPutStrLn stderr $ "Error: File " ++ inputFile ++ " does not exist"
      exitFailure
    else do
      -- Check if input file has .coscad extension
      if takeExtension inputFile /= ".coscad"
        then do
          hPutStrLn stderr "Error: Input file must have .coscad extension"
          exitFailure
        else do
          -- Generate output filename
          let outputFile = dropExtension inputFile ++ ".scad"

          -- Try to process the file
          result <- catch (processFileContents inputFile outputFile) handleError
          case result of
            Right _ -> do
              putStrLn $ "Successfully converted " ++ inputFile ++ " to " ++ outputFile
              exitSuccess
            Left errMsg -> do
              hPutStrLn stderr $ "Error: " ++ errMsg
              exitFailure

processFileContents :: FilePath -> FilePath -> IO (Either String ())
processFileContents inputFile outputFile = do
  -- Read the .coscad file
  contents <- readFile inputFile

  -- Parse the program with variables
  case parseProgram contents of
    Right (varTable, mainShape) -> do
      writeScad mainShape outputFile
      return $ Right ()
    Left err -> return $ Left err

handleError :: SomeException -> IO (Either String ())
handleError e = return $ Left $ "IO Error: " ++ show e
