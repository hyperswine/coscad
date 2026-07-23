{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.Char (isAlphaNum, isSpace)
import Data.Either (lefts, rights)
import Data.List (foldl', intercalate, sortBy)
import qualified Data.Map as Map
import Data.Void (Void)
import Lib
import System.Directory (doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.Process (readProcessWithExitCode)
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
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
            return (\l -> Position v l right),
          do
            symbol "⋈"
            v <- anchorVec
            right <- transformExpression varTable
            return (\l -> AttachTo v l right)
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

main :: IO ()
main = do
  setLocaleEncoding utf8
  args <- getArgs
  case args of
    ["next", inputFile] -> processNext inputFile
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
      hPutStrLn stderr "BOSL2 Shapes (centered; emit include <BOSL2/std.scad>):"
      hPutStrLn stderr "  ▣ 20 15 10 2   -- cuboid 20x15x10, chamfer 2 (0 = plain)"
      hPutStrLn stderr "  ◙ 20 15 10 3   -- cuboid 20x15x10, rounding 3"
      hPutStrLn stderr "  ⌭ 5 20 1       -- cylinder r5 h20, chamfered ends 1"
      hPutStrLn stderr "  ⌽ 5 20 2       -- cylinder r5 h20, rounded ends 2"
      hPutStrLn stderr "  xcyl 5 20      -- cylinder r5 l20 along X (also ycyl, zcyl)"
      hPutStrLn stderr "  ⊚ 10 6 25      -- tube outer-r 10, inner-r 6, h 25"
      hPutStrLn stderr "  ⏢ 24 24 10 10 18 -- prismoid base 24x24 top 10x10 h 18"
      hPutStrLn stderr "  ◉ 12 4         -- torus major-r 12, minor-r 4"
      hPutStrLn stderr "  ⊿ 20 20 15     -- wedge (vertical face at -X)"
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
      hPutStrLn stderr "  ⇋ 1 0 0 (● 5)   -- mirror across plane with normal (1,0,0)"
      hPutStrLn stderr "  ⮕ 15 (△ 8)      -- extrude triangle by height 15"
      exitFailure

processFile :: FilePath -> IO ()
processFile inputFile
  | takeExtension inputFile == ".assemble" = processAssemble inputFile
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
    Right (_, mainShape) -> do
      writeScad mainShape outputFile
      return $ Right ()
    Left err -> return $ Left err

handleError :: SomeException -> IO (Either String ())
handleError e = return $ Left $ "IO Error: " ++ show e

-- ============================================================
-- ASSEMBLY SUBSYSTEM (.assemble files)
-- ============================================================
-- Union inside a .coscad = one printed solid. Separate references
-- in a .assemble = declared-separate physical objects. Both recurse:
-- a reference may be a .coscad leaf or another .assemble, and a
-- subassembly looks like any other shape from the parent's view.
--
-- Syntax (line oriented, like .coscad):
--   plate 220 220 6                       -- bed W D margin (optional)
--   bracket ← bracket90.coscad ×4 ▽top seam=rear
--   rail    ← rail200.coscad ×0           -- ×0: assembly-only (premade)
--   corner  ← corner.assemble ×2          -- recursive subassembly
--   asm = rail ⊕ χ 10 bracket             -- assembled view (CoScad expr)
--
-- ▽anchor declares which face goes on the bed; orientation is
-- resolved HERE (upstream), so the slicer never searches for it.
-- Outputs for foo.assemble:
--   foo_asm.scad         assembled view (if asm defined)
--   foo_plate.scad       all printable instances packed on the bed
--   foo_part_<v>.scad    one per variant (part+orientation), print-ready
--   foo_manifest.json    variants (slice once) + placements (stamp many)

data POpt = OCount Int | ODown (Double, Double, Double) String | OHint (String, String)

data AsmStmt
  = APart String FilePath Int (Maybe ((Double, Double, Double), String)) [(String, String)]
  | APlate Double Double Double
  | ADef (VarName, String)

data FlatPart = FlatPart
  { fpTrail :: [String]
  , fpName :: String
  , fpSource :: FilePath
  , fpCount :: Int
  , fpDown :: Maybe ((Double, Double, Double), String)
  , fpHints :: [(String, String)]
  , fpShape :: Shape
  }

data AsmResult = AsmResult
  { arPlate :: (Double, Double, Double)
  , arAsm :: Maybe Shape
  , arFlat :: [FlatPart]
  }

asmProgram :: Parser [AsmStmt]
asmProgram = many asmStmt

asmStmt :: Parser AsmStmt
asmStmt = try plateStmt <|> try partStmt <|> (ADef <$> variableDefinition)

plateStmt :: Parser AsmStmt
plateStmt = do
  keyword "plate"
  w <- double
  d <- double
  m <- double <|> pure 6
  return (APlate w d m)

partStmt :: Parser AsmStmt
partStmt = do
  name <- identifier
  symbol "←"
  rest <- expressionString
  case parse (sc *> partOpts name <* eof) "" rest of
    Left e -> fail (errorBundlePretty e)
    Right st -> return st

partOpts :: String -> Parser AsmStmt
partOpts name = do
  path <- lexeme (some (satisfy (not . isSpace)))
  opts <- many partOpt
  let cnt = last (1 : [n | OCount n <- opts])
      down = case [(v, raw) | ODown v raw <- opts] of
        (d : _) -> Just d
        [] -> Nothing
      hints = [h | OHint h <- opts]
  return (APart name path cnt down hints)

partOpt :: Parser POpt
partOpt =
  choice
    [ OCount <$> (symbol "×" *> lexeme L.decimal)
    , do
        symbol "▽"
        (raw, v) <- match anchorVec
        return (ODown v (trim raw))
    , try $ do
        k <- identifier
        symbol "="
        v <- lexeme (some (satisfy (not . isSpace)))
        return (OHint (k, v))
    ]

loadAssembleFile :: [FilePath] -> FilePath -> IO (Either String AsmResult)
loadAssembleFile visited path
  | path `elem` visited = return (Left ("Cycle in .assemble references involving: " ++ path))
  | otherwise = do
      exists <- doesFileExist path
      if not exists
        then return (Left ("File not found: " ++ path))
        else do
          contents <- readFile path
          case parse (sc *> asmProgram <* eof) path contents of
            Left err -> return (Left (errorBundlePretty err))
            Right stmts -> do
              let dir = takeDirectory path
                  partsS = [p | p@APart {} <- stmts]
                  defs = [d | ADef d <- stmts]
                  plate = last ((220, 220, 6) : [(w, d, m) | APlate w d m <- stmts])
              loaded <- mapM (loadRef (path : visited) dir) partsS
              case sequence loaded of
                Left err -> return (Left err)
                Right entries -> do
                  let table0 = Map.fromList [(n, s) | (n, s, _) <- entries]
                  case resolveVariables defs table0 of
                    Left err -> return (Left err)
                    Right table ->
                      return $
                        Right
                          AsmResult
                            { arPlate = plate
                            , arAsm = Map.lookup "asm" table
                            , arFlat = concat [f | (_, _, f) <- entries]
                            }

loadRef :: [FilePath] -> FilePath -> AsmStmt -> IO (Either String (String, Shape, [FlatPart]))
loadRef visited dir (APart name rel cnt down hints) = do
  let p = dir </> rel
  case takeExtension p of
    ".coscad" -> do
      exists <- doesFileExist p
      if not exists
        then return (Left ("Part file not found: " ++ p))
        else do
          c <- readFile p
          case parseProgram c of
            Left err -> return (Left ("In " ++ p ++ ":\n" ++ err))
            Right (_, shape) ->
              return (Right (name, shape, [FlatPart [] name p cnt down hints shape]))
    ".assemble" -> do
      r <- loadAssembleFile visited p
      case r of
        Left err -> return (Left err)
        Right ar -> case arAsm ar of
          Nothing -> return (Left ("Subassembly " ++ p ++ " has no 'asm' definition"))
          Just s ->
            return $
              Right
                ( name
                , s
                , [ fp
                      { fpTrail = name : fpTrail fp
                      , fpCount = fpCount fp * cnt
                      , fpHints = fpHints fp ++ hints
                      }
                    | fp <- arFlat ar
                  ]
                )
    ext -> return (Left ("Unsupported part reference (" ++ ext ++ "): " ++ p))
loadRef _ _ _ = return (Left "internal error: loadRef on non-part statement")

-- | Rotate a shape so the declared anchor faces the bed (-Z),
-- rotating about its own bbox center. Nothing = already print-ready.
orientForBed :: Maybe ((Double, Double, Double), String) -> Shape -> Shape
orientForBed Nothing s = s
orientForBed (Just (v, _)) s =
  let d@(_, _, dz) = vnormed v
   in if vlen (vsub d (0, 0, -1)) < 1e-9
        then s
        else
          let (ang, ax) =
                if vlen (vsub d (0, 0, 1)) < 1e-9
                  then (180, (1, 0, 0))
                  else (acos (max (-1) (min 1 (-dz))) * 180 / pi, cross d (0, 0, -1))
              c = bcenter (bbox s)
           in Translate c (RotAxis ang ax (Translate (vneg c) s))

type VKey = (FilePath, String)

vkey :: FlatPart -> VKey
vkey fp = (fpSource fp, maybe "asis" snd (fpDown fp))

sanitize :: String -> String
sanitize = map (\c -> if isAlphaNum c then c else '_')

instId :: FlatPart -> Int -> String
instId fp i = intercalate "/" (fpTrail fp ++ [fpName fp]) ++ "_" ++ show i

-- | Shelf packing in declaration order; margin around and between.
packShelf ::
  (Double, Double, Double) ->
  [(String, VKey, (Double, Double))] ->
  Either String [(String, VKey, (Double, Double), (Double, Double))]
packShelf (pw, pd, m) = go m m 0
  where
    go _ _ _ [] = Right []
    go x y rowD (item@(iid, k, (w, d)) : rest)
      | w > pw - 2 * m || d > pd - 2 * m =
          Left (iid ++ " footprint " ++ show w ++ "x" ++ show d ++ " exceeds the plate")
      | x + w > pw - m = go m (y + rowD + m) 0 (item : rest)
      | y + d > pd - m =
          Left ("Plate overflow at " ++ iid ++ " — parts do not fit on one plate")
      | otherwise =
          ((iid, k, (w, d), (x, y)) :) <$> go (x + w + m) y (max rowD d) rest

jstr :: String -> String
jstr s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc c = [c]

jnum :: Double -> String
jnum x =
  let r = fromIntegral (round (x * 1000) :: Integer) / 1000 :: Double
   in if r == fromIntegral (round r :: Integer)
        then show (round r :: Integer)
        else show r

manifestJson ::
  FilePath ->
  String ->
  (Double, Double, Double) ->
  [(VKey, FlatPart, Shape, (Double, Double, Double), String)] ->
  [(String, VKey, (Double, Double), (Double, Double))] ->
  [FlatPart] ->
  String
manifestJson src base (pw, pd, m) variants placed asmOnly =
  "{\n"
    ++ ("  \"source\": " ++ jstr src ++ ",\n")
    ++ ("  \"plate\": {\"w\": " ++ jnum pw ++ ", \"d\": " ++ jnum pd ++ ", \"margin\": " ++ jnum m ++ "},\n")
    ++ ("  \"variants\": [\n" ++ intercalate ",\n" (map vj variants) ++ "\n  ],\n")
    ++ ("  \"placements\": [\n" ++ intercalate ",\n" (map pj placed) ++ "\n  ],\n")
    ++ ("  \"assembly_only\": [" ++ intercalate ", " (map aj asmOnly) ++ "]\n")
    ++ "}\n"
  where
    vj (k@(srcf, dk), fp, _, (w, d, h), vname) =
      "    {\"id\": " ++ jstr vname ++ ", \"part\": " ++ jstr (fpName fp)
        ++ ", \"source\": " ++ jstr srcf
        ++ ", \"down\": " ++ jstr dk
        ++ ", \"count\": " ++ show (length [() | (_, k', _, _) <- placed, k' == k])
        ++ ", \"footprint\": [" ++ jnum w ++ ", " ++ jnum d ++ ", " ++ jnum h ++ "]"
        ++ ", \"scad\": " ++ jstr (base ++ "_part_" ++ vname ++ ".scad")
        ++ ", \"hints\": {" ++ intercalate ", " [jstr hk ++ ": " ++ jstr hv | (hk, hv) <- fpHints fp] ++ "}}"
    pj (iid, k, (w, d), (x, y)) =
      "    {\"instance\": " ++ jstr iid ++ ", \"variant\": " ++ jstr (vnameOfK k)
        ++ ", \"x\": " ++ jnum x ++ ", \"y\": " ++ jnum y
        ++ ", \"w\": " ++ jnum w ++ ", \"d\": " ++ jnum d ++ "}"
    vnameOfK k = head [vn | (k', _, _, _, vn) <- variants, k' == k]
    aj fp = "{\"part\": " ++ jstr (fpName fp) ++ ", \"source\": " ++ jstr (fpSource fp) ++ "}"

processAssemble :: FilePath -> IO ()
processAssemble inputFile = do
  r <- loadAssembleFile [] inputFile
  case r of
    Left err -> do
      hPutStrLn stderr ("Error: " ++ err)
      exitFailure
    Right ar -> do
      let base = dropExtension inputFile
          baseName = takeFileName base
          plate@(pw, pd, _) = arPlate ar
          printable = [fp | fp <- arFlat ar, fpCount fp > 0]
          asmOnly = [fp | fp <- arFlat ar, fpCount fp <= 0]
      case arAsm ar of
        Just s -> do
          writeScad s (base ++ "_asm.scad")
          putStrLn ("Wrote " ++ base ++ "_asm.scad")
        Nothing -> putStrLn "(no 'asm' defined; skipping assembled view)"
      let vkeys = foldl' (\acc fp -> if vkey fp `elem` acc then acc else acc ++ [vkey fp]) [] printable
          mkVariant k =
            let fp = head [f | f <- printable, vkey f == k]
                oriented = resolve (orientForBed (fpDown fp) (fpShape fp))
                (lo, hi) = bbox oriented
                norm = Translate (vneg lo) oriented
                dims = vsub hi lo
                vname = sanitize (fpName fp ++ "_" ++ maybe "asis" snd (fpDown fp))
             in (k, fp, norm, dims, vname)
          variants = map mkVariant vkeys
      mapM_
        ( \(_, _, norm, _, vname) -> do
            let f = base ++ "_part_" ++ vname ++ ".scad"
            writeScad norm f
            putStrLn ("Wrote " ++ f)
        )
        variants
      let footOf k = head [(w, d) | (k', _, _, (w, d, _), _) <- variants, k' == k]
          normOf k = head [n | (k', _, n, _, _) <- variants, k' == k]
          instances = [(instId fp i, vkey fp) | fp <- printable, i <- [1 .. fpCount fp]]
      case packShelf plate [(iid, k, footOf k) | (iid, k) <- instances] of
        Left err -> do
          hPutStrLn stderr ("Error: " ++ err)
          exitFailure
        Right placed -> do
          let slab = Translate (0, 0, -0.8) (Rectangle pw pd 0.6)
              plateShape = Union (slab : [Translate (x, y, 0) (normOf k) | (_, k, _, (x, y)) <- placed])
          writeScad plateShape (base ++ "_plate.scad")
          putStrLn ("Wrote " ++ base ++ "_plate.scad")
          writeFile (base ++ "_manifest.json") (manifestJson inputFile baseName plate variants placed asmOnly)
          putStrLn ("Wrote " ++ base ++ "_manifest.json")

-- ============================================================
-- COSCAD NEXT — manufacturing stage (`coscad next foo.assemble`)
-- ============================================================
-- Consumes an assembly spec and produces per-bed print artifacts:
--   1. Each variant is compiled to a mesh once (via OpenSCAD;
--      override the binary with COSCAD_OPENSCAD, e.g. an xvfb
--      wrapper on headless machines).
--   2. Print orientation: a declared ▽ always wins; otherwise the
--      six axis faces are scored on the REAL mesh with FFF
--      heuristics — reward bed-contact area, punish overhang area
--      (faces steeper than 45° pointing down, off the bed) and
--      height — and the best legal face (within bed Z) is chosen.
--   3. Instances are packed largest-first onto beds, spilling to
--      as many beds as needed.
-- Outputs: foo_bed<N>.stl (one solid per bed, all instances,
-- normals recomputed), and foo_manifest.json recording per-variant
-- chosen orientation + per-bed placements — the slicer slices each
-- variant once and stamps it at the recorded offsets.

type Tri = ((Double, Double, Double), (Double, Double, Double), (Double, Double, Double))

parseStlAscii :: String -> [Tri]
parseStlAscii s = group3 verts
  where
    verts =
      [ toV (map read (take 3 (drop 1 ws)))
        | l <- lines s
        , let ws = words l
        , take 1 ws == ["vertex"]
      ]
    toV [x, y, z] = (x, y, z)
    toV _ = (0, 0, 0)
    group3 (a : b : c : r) = (a, b, c) : group3 r
    group3 _ = []

triNormal :: Tri -> (Double, Double, Double)
triNormal (a, b, c) = vnormed (cross (vsub b a) (vsub c a))

triArea :: Tri -> Double
triArea (a, b, c) = vlen (cross (vsub b a) (vsub c a)) / 2

meshMap :: ((Double, Double, Double) -> (Double, Double, Double)) -> [Tri] -> [Tri]
meshMap f = map (\(a, b, c) -> (f a, f b, f c))

meshBounds :: [Tri] -> BBox
meshBounds tris = fromCorners [v | (a, b, c) <- tris, v <- [a, b, c]]

stlAscii :: String -> [Tri] -> String
stlAscii name tris =
  "solid " ++ name ++ "\n"
    ++ concatMap facet tris
    ++ ("endsolid " ++ name ++ "\n")
  where
    facet t@(a, b, c) =
      let (nx, ny, nz) = triNormal t
       in "  facet normal " ++ unwords (map show [nx, ny, nz]) ++ "\n"
            ++ "    outer loop\n"
            ++ concatMap vtx [a, b, c]
            ++ "    endloop\n  endfacet\n"
    vtx (x, y, z) = "      vertex " ++ unwords (map show [x, y, z]) ++ "\n"

-- | Rotation matrix taking direction v to straight down.
downMat :: (Double, Double, Double) -> M3
downMat v =
  let d = vnormed v
   in if vlen (vsub d (0, 0, -1)) < 1e-9
        then ((1, 0, 0), (0, 1, 0), (0, 0, 1))
        else
          if vlen (vsub d (0, 0, 1)) < 1e-9
            then rodMat 180 (1, 0, 0)
            else
              let (_, _, dz) = d
               in rodMat (acos (max (-1) (min 1 (-dz))) * 180 / pi) (cross d (0, 0, -1))

faceCandidates :: [((Double, Double, Double), String)]
faceCandidates =
  [ ((0, 0, -1), "bot")
  , ((0, 0, 1), "top")
  , ((1, 0, 0), "rt")
  , ((-1, 0, 0), "lft")
  , ((0, -1, 0), "fwd")
  , ((0, 1, 0), "bak")
  ]

-- | FFF orientation score for a mesh already rotated into a
-- candidate orientation. Higher is better; Nothing = exceeds bed Z.
fffScore :: Double -> [Tri] -> Maybe Double
fffScore maxZ tris =
  let ((x0, y0, z0), (x1, y1, z1)) = meshBounds tris
      h = z1 - z0
      maxdim = maximum [x1 - x0, y1 - y0, h, 1]
      eps = 0.3
      stats = [(triNormal t, triArea t, minimum [z | (_, _, z) <- verts t], maximum [z | (_, _, z) <- verts t]) | t <- tris]
      verts (a, b, c) = [a, b, c]
      total = max 1e-9 (sum [a | (_, a, _, _) <- stats])
      contact = sum [a | ((_, _, nz), a, _, mxz) <- stats, nz < -0.9, mxz < z0 + eps]
      -- only near-horizontal downward faces need support; curved
      -- flanks of lying cylinders (45-65 deg) self-support in FFF
      overhang = sum [a | ((_, _, nz), a, mnz, _) <- stats, nz < -0.9, mnz > z0 + eps]
      -- stability: punish tall skinny prints (wobble, layer-shear)
      slender = h / max 1 (min (x1 - x0) (y1 - y0))
      wobble = 0.1 * max 0 (slender - 2)
   in if h > maxZ
        then Nothing
        else Just (contact / total - 3 * overhang / total - 0.3 * h / maxdim - wobble)

-- | Choose orientation: declared ▽ wins; otherwise best FFF score.
-- Returns (rotation matrix, chosen face label, mode, score).
chooseOrientation :: Double -> Maybe ((Double, Double, Double), String) -> [Tri] -> Either String (M3, String, String, Double)
chooseOrientation maxZ (Just (v, lbl)) tris =
  let m = downMat v
      rot = meshMap (mApply m) tris
   in case fffScore maxZ rot of
        Nothing -> Left ("declared ▽" ++ lbl ++ " exceeds bed height")
        Just sc -> Right (m, lbl, "declared", sc)
chooseOrientation maxZ Nothing tris =
  let scored =
        [ (sc, m, lbl)
          | (v, lbl) <- faceCandidates
          , let m = downMat v
          , Just sc <- [fffScore maxZ (meshMap (mApply m) tris)]
        ]
   in case scored of
        [] -> Left "no orientation fits within bed height"
        _ ->
          let (sc, m, lbl) = foldl1 (\a@(s1, _, _) b@(s2, _, _) -> if s2 > s1 then b else a) scored
           in Right (m, lbl, "auto", sc)

-- | Pack onto as many beds as needed, largest footprint first.
packBeds ::
  (Double, Double, Double) ->
  [(String, VKey, (Double, Double))] ->
  Either String [[(String, VKey, (Double, Double), (Double, Double))]]
packBeds plate@(pw, pd, m) items0 = go (sortBy bigger items0)
  where
    bigger (_, _, (w1, d1)) (_, _, (w2, d2)) = compare (w2 * d2) (w1 * d1)
    go [] = Right []
    go xs = do
      (placed, rest) <- fitOne xs
      (placed :) <$> go rest
    fitOne = fit m m 0 []
    fit _ _ _ acc [] = Right (reverse acc, [])
    fit x y rowD acc (item@(iid, k, (w, d)) : rest)
      | w > pw - 2 * m || d > pd - 2 * m =
          Left (iid ++ " footprint " ++ show w ++ "x" ++ show d ++ " exceeds the bed")
      | x + w > pw - m = fit m (y + rowD + m) 0 acc (item : rest)
      | y + d > pd - m = Right (reverse acc, item : rest)
      | otherwise = fit (x + w + m) y (max rowD d) ((iid, k, (w, d), (x, y)) : acc) rest

runOpenscad :: FilePath -> FilePath -> IO (Either String ())
runOpenscad scadF stlF = do
  bin <- maybe "openscad" id <$> lookupEnv "COSCAD_OPENSCAD"
  (code, _, err) <- readProcessWithExitCode bin ["-o", stlF, scadF] ""
  case code of
    ExitSuccess -> return (Right ())
    ExitFailure n -> return (Left ("openscad failed (" ++ show n ++ ") on " ++ scadF ++ ":\n" ++ err))

processNext :: FilePath -> IO ()
processNext inputFile = do
  r <- loadAssembleFile [] inputFile
  case r of
    Left err -> hPutStrLn stderr ("Error: " ++ err) >> exitFailure
    Right ar -> do
      let base = dropExtension inputFile
          (pw, pd, marg) = arPlate ar
          bedZ = 250 -- default max print height; TODO: plate stmt 4th arg
          printable = [fp | fp <- arFlat ar, fpCount fp > 0]
          asmOnly = [fp | fp <- arFlat ar, fpCount fp <= 0]
          vkeys = foldl' (\acc fp -> if vkey fp `elem` acc then acc else acc ++ [vkey fp]) [] printable
      -- compile each variant to a mesh once
      variantsE <- mapM
        ( \k -> do
            let fp = head [f | f <- printable, vkey f == k]
                vname = sanitize (fpName fp ++ "_" ++ maybe "asis" snd (fpDown fp))
                scadF = base ++ "_next_" ++ vname ++ ".scad"
                stlF = base ++ "_next_" ++ vname ++ ".stl"
            writeScad (resolve (fpShape fp)) scadF
            ro <- runOpenscad scadF stlF
            case ro of
              Left err -> return (Left err)
              Right () -> do
                mesh <- parseStlAscii <$> readFile stlF
                case chooseOrientation bedZ (fpDown fp) mesh of
                  Left err -> return (Left (vname ++ ": " ++ err))
                  Right (m, lbl, mode, sc) -> do
                    let rot = meshMap (mApply m) mesh
                        (lo, hi) = meshBounds rot
                        norm = meshMap (\v -> vsub v lo) rot
                        dims = vsub hi lo
                    return (Right (k, fp, norm, dims, vname, lbl, mode, sc))
        )
        vkeys
      case sequence variantsE of
        Left err -> hPutStrLn stderr ("Error: " ++ err) >> exitFailure
        Right variants -> do
          let footOf k = head [(w, d) | (k', _, _, (w, d, _), _, _, _, _) <- variants, k' == k]
              meshOf k = head [n | (k', _, n, _, _, _, _, _) <- variants, k' == k]
              vnameOf k = head [vn | (k', _, _, _, vn, _, _, _) <- variants, k' == k]
              instances = [(instId fp i, vkey fp) | fp <- printable, i <- [1 .. fpCount fp]]
          case packBeds (pw, pd, marg) [(iid, k, footOf k) | (iid, k) <- instances] of
            Left err -> hPutStrLn stderr ("Error: " ++ err) >> exitFailure
            Right beds -> do
              mapM_
                ( \(i, placed) -> do
                    let tris = concat [meshMap (vadd (x, y, 0)) (meshOf k) | (_, k, _, (x, y)) <- placed]
                        f = base ++ "_bed" ++ show i ++ ".stl"
                    writeFile f (stlAscii ("bed" ++ show i) tris)
                    putStrLn ("Wrote " ++ f ++ " (" ++ show (length placed) ++ " parts)")
                )
                (zip [(1 :: Int) ..] beds)
              writeFile (base ++ "_manifest.json") (nextManifest inputFile base (pw, pd, marg) bedZ variants beds asmOnly)
              putStrLn ("Wrote " ++ base ++ "_manifest.json")

nextManifest ::
  FilePath ->
  String ->
  (Double, Double, Double) ->
  Double ->
  [(VKey, FlatPart, [Tri], (Double, Double, Double), String, String, String, Double)] ->
  [[(String, VKey, (Double, Double), (Double, Double))]] ->
  [FlatPart] ->
  String
nextManifest src base (pw, pd, m) bedZ variants beds asmOnly =
  "{\n"
    ++ ("  \"source\": " ++ jstr src ++ ",\n")
    ++ ("  \"bed\": {\"w\": " ++ jnum pw ++ ", \"d\": " ++ jnum pd ++ ", \"margin\": " ++ jnum m ++ ", \"max_z\": " ++ jnum bedZ ++ "},\n")
    ++ ("  \"variants\": [\n" ++ intercalate ",\n" (map vj variants) ++ "\n  ],\n")
    ++ ("  \"beds\": [\n" ++ intercalate ",\n" (map bj (zip [(1 :: Int) ..] beds)) ++ "\n  ],\n")
    ++ ("  \"assembly_only\": [" ++ intercalate ", " (map aj asmOnly) ++ "]\n")
    ++ "}\n"
  where
    total = length . concat $ beds
    vj ((srcf, _), fp, _, (w, d, h), vname, lbl, mode, sc) =
      "    {\"id\": " ++ jstr vname ++ ", \"part\": " ++ jstr (fpName fp)
        ++ ", \"source\": " ++ jstr srcf
        ++ ", \"down\": " ++ jstr lbl ++ ", \"orientation_mode\": " ++ jstr mode
        ++ ", \"fff_score\": " ++ jnum sc
        ++ ", \"footprint\": [" ++ jnum w ++ ", " ++ jnum d ++ ", " ++ jnum h ++ "]"
        ++ ", \"mesh\": " ++ jstr (base ++ "_next_" ++ vname ++ ".stl")
        ++ ", \"hints\": {" ++ intercalate ", " [jstr hk ++ ": " ++ jstr hv | (hk, hv) <- fpHints fp] ++ "}}"
    bj (i, placed) =
      "    {\"index\": " ++ show i ++ ", \"stl\": " ++ jstr (base ++ "_bed" ++ show i ++ ".stl")
        ++ ", \"placements\": [\n" ++ intercalate ",\n" (map pj placed) ++ "\n    ]}"
    pj (iid, k, (w, d), (x, y)) =
      "      {\"instance\": " ++ jstr iid ++ ", \"variant\": " ++ jstr (vn k)
        ++ ", \"x\": " ++ jnum x ++ ", \"y\": " ++ jnum y ++ ", \"w\": " ++ jnum w ++ ", \"d\": " ++ jnum d ++ "}"
    vn k = head [vname | (k', _, _, _, vname, _, _, _) <- variants, k' == k]
    aj fp = "{\"part\": " ++ jstr (fpName fp) ++ ", \"source\": " ++ jstr (fpSource fp) ++ "}"
