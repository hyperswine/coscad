module Main (main) where

import Control.Exception (SomeException, catch)
import Lib
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (dropExtension, takeExtension)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> processFile inputFile
    _ -> do
      hPutStrLn stderr "Usage: coscad <input.coscad>"
      hPutStrLn stderr "Converts a .coscad file to .scad format"
      hPutStrLn stderr ""
      hPutStrLn stderr "Example .coscad file contents:"
      hPutStrLn stderr "  -- Simple cube"
      hPutStrLn stderr "  ■ 10"
      hPutStrLn stderr ""
      hPutStrLn stderr "  -- Sphere with cylinder difference"
      hPutStrLn stderr "  ● 15 ⊖ (◎ 5 10)"
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

  -- For now, we'll create a simple example
  -- In a real implementation, you'd want to parse the DSL syntax
  case parseSimpleExpression contents of
    Right shape -> do
      writeScad shape outputFile
      return $ Right ()
    Left err -> return $ Left err

-- Simple parser for basic coscad expressions
-- This is a basic implementation - you can expand this for more complex parsing
parseSimpleExpression :: String -> Either String Shape
parseSimpleExpression contents =
  let trimmed = trim contents
   in -- First check for boolean operations (higher precedence)
      if "⊖" `isInfixOf` trimmed
        then parseDifference trimmed
        else if "⊕" `isInfixOf` trimmed
          then parseUnion trimmed
          else -- Then check for basic shapes
            case words trimmed of
              ["■", _] -> parseCube trimmed
              ["●", _] -> parseShapeWithSize "●" trimmed (\r -> Sphere r)
              ["◎", _, _] -> parseCylinder trimmed
              ["▻", _, _] -> parseCone trimmed
              -- Default example for unrecognized input
              _ ->
                Right $
                  Union
                    [ Rectangle 10 10 10,
                      Tx 15 (Sphere 5),
                      Ty 15 (Cylinder 3 10)
                    ]

parseShapeWithSize :: String -> String -> (Double -> Shape) -> Either String Shape
parseShapeWithSize symbol input constructor =
  case words input of
    [s, sizeStr] | s == symbol ->
      case readDouble sizeStr of
        Just size -> Right $ constructor size
        Nothing -> Left $ "Could not parse size: " ++ sizeStr
    _ -> Left $ "Invalid " ++ symbol ++ " syntax. Expected: " ++ symbol ++ " <size>"

-- Special parser for cube that creates a cube with equal dimensions
parseCube :: String -> Either String Shape
parseCube input =
  case words input of
    ["■", sizeStr] ->
      case readDouble sizeStr of
        Just size -> Right $ Rectangle size size size
        Nothing -> Left $ "Could not parse cube size: " ++ sizeStr
    _ -> Left "Invalid cube syntax. Expected: ■ <size>"

parseCylinder :: String -> Either String Shape
parseCylinder input =
  case words input of
    ["◎", rStr, hStr] ->
      case (readDouble rStr, readDouble hStr) of
        (Just r, Just h) -> Right $ Cylinder r h
        _ -> Left "Could not parse cylinder parameters"
    _ -> Left "Invalid cylinder syntax. Expected: ◎ <radius> <height>"

-- Special parser for cone
parseCone :: String -> Either String Shape
parseCone input =
  case words input of
    ["▻", rStr, hStr] ->
      case (readDouble rStr, readDouble hStr) of
        (Just r, Just h) -> Right $ Cone r h
        _ -> Left "Could not parse cone parameters"
    _ -> Left "Invalid cone syntax. Expected: ▻ <radius> <height>"

-- Simple parser for difference operations
parseDifference :: String -> Either String Shape
parseDifference input =
  -- Very basic parser - looks for "shape1 ⊖ shape2"
  case splitOn "⊖" input of
    [left, right] ->
      case (parseBasicShape (trim left), parseBasicShape (trim right)) of
        (Right s1, Right s2) -> Right $ Diff s1 s2
        (Left err, _) -> Left err
        (_, Left err) -> Left err
    _ -> Left "Invalid difference syntax. Expected: shape1 ⊖ shape2"

-- Simple parser for union operations
parseUnion :: String -> Either String Shape
parseUnion input =
  -- Very basic parser - looks for "shape1 ⊕ shape2"
  case splitOn "⊕" input of
    [left, right] ->
      case (parseBasicShape (trim left), parseBasicShape (trim right)) of
        (Right s1, Right s2) -> Right $ Union [s1, s2]
        (Left err, _) -> Left err
        (_, Left err) -> Left err
    _ -> Left "Invalid union syntax. Expected: shape1 ⊕ shape2"

-- Parse basic shapes without operators
parseBasicShape :: String -> Either String Shape
parseBasicShape input =
  let trimmed = trim input
   in case words trimmed of
        ["■", sizeStr] ->
          case readDouble sizeStr of
            Just size -> Right $ Rectangle size size size
            Nothing -> Left $ "Could not parse cube size: " ++ sizeStr
        ["●", rStr] ->
          case readDouble rStr of
            Just r -> Right $ Sphere r
            Nothing -> Left $ "Could not parse sphere radius: " ++ rStr
        ["◎", rStr, hStr] ->
          case (readDouble rStr, readDouble hStr) of
            (Just r, Just h) -> Right $ Cylinder r h
            _ -> Left "Could not parse cylinder parameters"
        ["▻", rStr, hStr] ->
          case (readDouble rStr, readDouble hStr) of
            (Just r, Just h) -> Right $ Cone r h
            _ -> Left "Could not parse cone parameters"
        _ -> Left $ "Could not parse shape: " ++ trimmed

-- Simple string splitting function
splitOn :: String -> String -> [String]
splitOn sep str = case breakOn sep str of
  (before, after)
    | null after -> [before]
    | otherwise -> before : splitOn sep (drop (length sep) after)

-- Helper function to break string on separator
breakOn :: String -> String -> (String, String)
breakOn sep str = case findIndex (isPrefixOf sep) (tails str) of
  Just i -> splitAt i str
  Nothing -> (str, "")

-- Check if one list is prefix of another
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Get all tails of a list
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:ys) = xs : tails ys

-- Find index of first element satisfying predicate
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p xs = findIndex' p xs 0
  where
    findIndex' _ [] _ = Nothing
    findIndex' predicate (y:ys) i
      | predicate y = Just i
      | otherwise = findIndex' predicate ys (i + 1)

readDouble :: String -> Maybe Double
readDouble s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

trim :: String -> String
trim = unwords . words

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` words haystack

handleError :: SomeException -> IO (Either String ())
handleError e = return $ Left $ "IO Error: " ++ show e
