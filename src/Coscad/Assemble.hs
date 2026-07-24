-- | The .assemble subsystem: recursive part references with print
-- counts, orientation declarations, and hints; design-stage outputs
-- (assembled view, packed plate, per-variant scads, manifest).
module Coscad.Assemble (module Coscad.Assemble) where

import Coscad.Codegen
import Coscad.Geometry
import Coscad.Parser
import Coscad.Shape
import Data.Char (isAlphaNum, isSpace)
import Data.List (foldl', intercalate)
import qualified Data.Map as Map
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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

