module Main (main) where

import Control.Exception (SomeException, catch)
import Lib
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (dropExtension, takeExtension)
import System.IO (hPutStrLn, stderr)
import qualified Data.Map as Map
import Data.Char (isAlpha, isAlphaNum, isDigit)

type VarName = String
type VarTable = Map.Map VarName Shape

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

processFileContents inputFile outputFile = do
  -- Read the .coscad file
  contents <- readFile inputFile

  -- Parse the program with variables
  case parseProgram contents of
    Right (varTable, mainShape) -> do
      writeScad mainShape outputFile
      return $ Right ()
    Left err -> return $ Left err

-- Parse a complete program with variable definitions
parseProgram :: String -> Either String (VarTable, Shape)
parseProgram contents = do
  let contentLines = lines contents
  let nonEmptyLines = filter (not . null . trim) contentLines
  let nonCommentLines = filter (not . isCommentLine) nonEmptyLines

  -- First pass: collect variable names and their raw expressions
  rawVarDefs <- mapM parseVarDefinitionRaw nonCommentLines

  -- Second pass: recursively resolve variables
  varTable <- resolveVariables rawVarDefs Map.empty

  -- Look for main variable
  case Map.lookup "main" varTable of
    Just mainShape -> Right (varTable, mainShape)
    Nothing -> Left "Error: No 'main' variable found. Please define a 'main' variable."

-- Parse a variable definition to extract name and raw expression
parseVarDefinitionRaw :: String -> Either String (VarName, String)
parseVarDefinitionRaw line =
  case break (== '=') line of
    (varPart, '=' : exprPart) -> do
      let varName = trim varPart
      let expr = trim exprPart
      if isValidVarName varName
        then Right (varName, expr)
        else Left $ "Invalid variable name: " ++ varName
    _ -> Left $ "Invalid syntax: expected variable = expression, got: " ++ line

-- Resolve all variables by building the table incrementally
resolveVariables :: [(VarName, String)] -> VarTable -> Either String VarTable
resolveVariables [] table = Right table
resolveVariables remaining table = do
  -- Try to resolve any variable that we can with the current table
  let results = map (tryResolveVar table) remaining
  let resolved = [x | Right x <- results]
  let unresolved = [x | Left x <- results]

  if null resolved
    then Left $ "Cannot resolve variables (possible circular dependency): " ++ show (map fst unresolved)
    else do
      -- Add resolved variables to the table
      let newTable = foldl (\acc (name, shape) -> Map.insert name shape acc) table resolved
      -- Continue with unresolved variables
      resolveVariables unresolved newTable

-- Try to resolve a single variable with the current table
tryResolveVar :: VarTable -> (VarName, String) -> Either (VarName, String) (VarName, Shape)
tryResolveVar table (name, expr) =
  case parseExpressionWithVars expr table of
    Right shape -> Right (name, shape)
    Left _ -> Left (name, expr)

-- Helper function to partition Either values
partitionEithers :: [Either a b] -> ([b], [a])
partitionEithers [] = ([], [])
partitionEithers (Left x : xs) = let (rs, ls) = partitionEithers xs in (rs, x : ls)
partitionEithers (Right x : xs) = let (rs, ls) = partitionEithers xs in (x : rs, ls)

-- Parse a single variable definition line
parseVarDefinition :: String -> Either String (VarName, Shape)
parseVarDefinition line =
  case break (== '=') line of
    (varPart, '=' : exprPart) -> do
      let varName = trim varPart
      let expr = trim exprPart
      if isValidVarName varName
        then do
          -- Parse with an empty variable table first, then we'll need to fix this
          -- to support forward references properly
          shape <- parseExpressionWithVars expr Map.empty
          Right (varName, shape)
        else Left $ "Invalid variable name: " ++ varName
    _ -> Left $ "Invalid syntax: expected variable = expression, got: " ++ line

-- Check if a variable name is valid
isValidVarName :: String -> Bool
isValidVarName [] = False
isValidVarName (c:cs) = isAlpha c && all isValidVarChar cs
  where
    isValidVarChar ch = isAlphaNum ch || ch == '_' || ch == '₀' || ch == '₁' || ch == '₂' || ch == '₃' || ch == '₄' || ch == '₅' || ch == '₆' || ch == '₇' || ch == '₈' || ch == '₉'

-- Check if a line is a comment
isCommentLine :: String -> Bool
isCommentLine line = take 2 (trim line) == "//"

-- Parse expression with variable context
parseExpressionWithVars :: String -> VarTable -> Either String Shape
parseExpressionWithVars expr varTable =
  let trimmed = trim expr
  in -- First check for boolean operations (higher precedence)
     if "⊖" `isInfixOf` trimmed
       then parseBooleanOpWithVars "⊖" trimmed Diff varTable
       else
         if "⊝" `isInfixOf` trimmed
           then parseBooleanOpWithVars "⊝" trimmed Diff varTable
           else
             if "⊕" `isInfixOf` trimmed
               then parseBooleanOpWithVars "⊕" trimmed (\a b -> Union [a, b]) varTable
               else
                 if "⊛" `isInfixOf` trimmed
                   then parseBooleanOpWithVars "⊛" trimmed (\a b -> Union [a, b]) varTable
                   else -- Check for transformations or shapes
                     parseTransformOrShapeWithVars trimmed varTable

-- Parse boolean operations with variable context
parseBooleanOpWithVars :: String -> String -> (Shape -> Shape -> Shape) -> VarTable -> Either String Shape
parseBooleanOpWithVars op input constructor varTable =
  case splitOn op input of
    [left, right] ->
      case (parseExpressionWithVars (trim left) varTable, parseExpressionWithVars (trim right) varTable) of
        (Right s1, Right s2) -> Right $ constructor s1 s2
        (Left err, _) -> Left err
        (_, Left err) -> Left err
    _ -> Left $ "Invalid " ++ op ++ " syntax. Expected: shape1 " ++ op ++ " shape2"

-- Parse transformations and basic shapes with variable context
parseTransformOrShapeWithVars :: String -> VarTable -> Either String Shape
parseTransformOrShapeWithVars input varTable =
  let ws = words input
  in case ws of
       -- Variable reference
       [var] -> case Map.lookup var varTable of
         Just shape -> Right shape
         Nothing -> Left $ "Undefined variable: " ++ var
       -- Basic shapes
       ["■", size] -> parseWithSize size (\s -> Rectangle s s s)
       ["●", size] -> parseWithSize size Sphere
       ["◎", r, h] -> parseWithTwoSizes r h Cylinder
       ["▻", r, h] -> parseWithTwoSizes r h Cone
       ["▬", x, y, z] -> parseWithThreeSizes x y z Rectangle
       ["⎏", n, r, h] -> parseWithThreeSizes n r h (Prism . round)
       ["△", r] -> parseWithSize r (Shape2D 3)
       ["⬠", r] -> parseWithSize r (Shape2D 5)
       ["⭘", r] -> parseWithSize r (Shape2D 100)
       -- Transformations - handle by parsing the rest of the expression
       ("χ" : dx : rest) -> parseTransformWithVars dx (unwords rest) Tx varTable
       ("ψ" : dy : rest) -> parseTransformWithVars dy (unwords rest) Ty varTable
       ("ζ" : dz : rest) -> parseTransformWithVars dz (unwords rest) Tz varTable
       ("θ" : ax : rest) -> parseTransformWithVars ax (unwords rest) Rx varTable
       ("ϕ" : ay : rest) -> parseTransformWithVars ay (unwords rest) Ry varTable
       ("ω" : az : rest) -> parseTransformWithVars az (unwords rest) Rz varTable
       ("⮕" : h : rest) -> parseTransformWithVars h (unwords rest) Extrude varTable
       -- Scale transformation (expects 3 values then shape)
       ("⬈" : sx : sy : sz : rest) -> parseScaleTransformWithVars sx sy sz (unwords rest) varTable
       -- Try to parse as a transformation with more complex syntax
       _ -> parseComplexTransformWithVars input varTable

-- Parse transformations with variable context
parseTransformWithVars :: String -> String -> (Double -> Shape -> Shape) -> VarTable -> Either String Shape
parseTransformWithVars valueStr shapeStr constructor varTable =
  case readDouble valueStr of
    Just value -> case parseExpressionWithVars shapeStr varTable of
      Right shape -> Right $ constructor value shape
      Left err -> Left err
    Nothing -> Left $ "Could not parse transform value: " ++ valueStr

-- Parse scale transformation with variable context
parseScaleTransformWithVars :: String -> String -> String -> String -> VarTable -> Either String Shape
parseScaleTransformWithVars sx sy sz shapeStr varTable =
  case (readDouble sx, readDouble sy, readDouble sz) of
    (Just x, Just y, Just z) -> case parseExpressionWithVars shapeStr varTable of
      Right shape -> Right $ Scale (x, y, z) shape
      Left err -> Left err
    _ -> Left $ "Could not parse scale values: " ++ sx ++ " " ++ sy ++ " " ++ sz

-- Handle complex transformations with variable context
parseComplexTransformWithVars :: String -> VarTable -> Either String Shape
parseComplexTransformWithVars input varTable =
  -- If we can't parse it as a known pattern, try to extract parentheses
  case parseParentheses input of
    Just (beforeParen, insideParen, afterParen) ->
      -- Try to parse what's inside parentheses first
      case parseExpressionWithVars insideParen varTable of
        Right innerShape ->
          -- Now try to parse the transformation prefix
          case parseTransformPrefix (beforeParen ++ afterParen) of
            Right transformer -> Right $ transformer innerShape
            Left _ -> Left $ "Could not parse transformation: " ++ beforeParen ++ afterParen
        Left err -> Left err
    Nothing -> Left $ "Could not parse expression: " ++ input

-- Helper parsers (unchanged from original)
parseWithSize sizeStr constructor =
  case readDouble sizeStr of
    Just size -> Right $ constructor size
    Nothing -> Left $ "Could not parse size: " ++ sizeStr

parseWithTwoSizes s1 s2 constructor =
  case (readDouble s1, readDouble s2) of
    (Just v1, Just v2) -> Right $ constructor v1 v2
    _ -> Left $ "Could not parse parameters: " ++ s1 ++ " " ++ s2

parseWithThreeSizes s1 s2 s3 constructor =
  case (readDouble s1, readDouble s2, readDouble s3) of
    (Just v1, Just v2, Just v3) -> Right $ constructor v1 v2 v3
    _ -> Left $ "Could not parse parameters: " ++ s1 ++ " " ++ s2 ++ " " ++ s3

parseTransform valueStr shapeStr constructor =
  case readDouble valueStr of
    Just value -> case parseExpressionWithVars shapeStr Map.empty of
      Right shape -> Right $ constructor value shape
      Left err -> Left err
    Nothing -> Left $ "Could not parse transform value: " ++ valueStr

parseScaleTransform sx sy sz shapeStr =
  case (readDouble sx, readDouble sy, readDouble sz) of
    (Just x, Just y, Just z) -> case parseExpressionWithVars shapeStr Map.empty of
      Right shape -> Right $ Scale (x, y, z) shape
      Left err -> Left err
    _ -> Left $ "Could not parse scale values: " ++ sx ++ " " ++ sy ++ " " ++ sz

-- Handle more complex transformation syntax
parseComplexTransform input =
  -- If we can't parse it as a known pattern, try to extract parentheses
  case parseParentheses input of
    Just (beforeParen, insideParen, afterParen) ->
      -- Try to parse what's inside parentheses first
      case parseExpressionWithVars insideParen Map.empty of
        Right innerShape ->
          -- Now try to parse the transformation prefix
          case parseTransformPrefix (beforeParen ++ afterParen) of
            Right transformer -> Right $ transformer innerShape
            Left _ -> Left $ "Could not parse transformation: " ++ beforeParen ++ afterParen
        Left err -> Left err
    Nothing -> Left $ "Could not parse expression: " ++ input

-- Extract content within parentheses
parseParentheses input =
  case span (/= '(') input of
    (before, '(' : rest) ->
      case span (/= ')') rest of
        (inside, ')' : after) -> Just (before, inside, after)
        _ -> Nothing
    _ -> Nothing

-- Parse transformation prefix (like "χ 5" to create Tx 5)
parseTransformPrefix input =
  case words input of
    ["χ", dx] -> case readDouble dx of
      Just d -> Right $ Tx d
      Nothing -> Left $ "Could not parse Tx value: " ++ dx
    ["ψ", dy] -> case readDouble dy of
      Just d -> Right $ Ty d
      Nothing -> Left $ "Could not parse Ty value: " ++ dy
    ["ζ", dz] -> case readDouble dz of
      Just d -> Right $ Tz d
      Nothing -> Left $ "Could not parse Tz value: " ++ dz
    ["θ", ax] -> case readDouble ax of
      Just d -> Right $ Rx d
      Nothing -> Left $ "Could not parse Rx value: " ++ ax
    ["ϕ", ay] -> case readDouble ay of
      Just d -> Right $ Ry d
      Nothing -> Left $ "Could not parse Ry value: " ++ ay
    ["ω", az] -> case readDouble az of
      Just d -> Right $ Rz d
      Nothing -> Left $ "Could not parse Rz value: " ++ az
    ["⮕", h] -> case readDouble h of
      Just d -> Right $ Extrude d
      Nothing -> Left $ "Could not parse Extrude value: " ++ h
    ["⬈", sx, sy, sz] -> case (readDouble sx, readDouble sy, readDouble sz) of
      (Just x, Just y, Just z) -> Right $ Scale (x, y, z)
      _ -> Left $ "Could not parse Scale values: " ++ sx ++ " " ++ sy ++ " " ++ sz
    _ -> Left $ "Unknown transformation: " ++ input

-- Utility functions
readDouble s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

trim = unwords . words

-- Simple string splitting function
splitOn sep str = case breakOn sep str of
  (before, after)
    | null after -> [before]
    | otherwise -> before : splitOn sep (drop (length sep) after)

-- Helper function to break string on separator
breakOn sep str = case findIndex (isPrefixOf sep) (tails str) of
  Just i -> splitAt i str
  Nothing -> (str, "")

-- Check if one list is prefix of another
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

-- Get all tails of a list
tails [] = [[]]
tails xs@(_ : ys) = xs : tails ys

-- Find index of first element satisfying predicate
findIndex p xs = findIndex' p xs 0
  where
    findIndex' _ [] _ = Nothing
    findIndex' predicate (y : ys) i
      | predicate y = Just i
      | otherwise = findIndex' predicate ys (i + 1)

isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)

handleError :: SomeException -> IO (Either String ())
handleError e = return $ Left $ "IO Error: " ++ show e
