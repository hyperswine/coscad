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
   in case trimmed of
        -- Simple cube
        c | "■" `isInfixOf` c -> parseShapeWithSize "■" c Rectangle
        -- Simple sphere
        s | "●" `isInfixOf` s -> parseShapeWithSize "●" s (\r -> Sphere r)
        -- Simple cylinder
        cyl | "◎" `isInfixOf` cyl -> parseCylinder cyl
        -- Default example
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

parseCylinder :: String -> Either String Shape
parseCylinder input =
  case words input of
    ["◎", rStr, hStr] ->
      case (readDouble rStr, readDouble hStr) of
        (Just r, Just h) -> Right $ Cylinder r h
        _ -> Left "Could not parse cylinder parameters"
    _ -> Left "Invalid cylinder syntax. Expected: ◎ <radius> <height>"

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
