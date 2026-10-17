{-# LANGUAGE PatternSynonyms #-}

-- | The .assemble subsystem: recursive part references with print
-- counts, orientation declarations, and hints; design-stage outputs
-- (assembled view, packed plate, per-variant scads, manifest).
module Coscad.Assemble (module Coscad.Assemble) where

import Control.Monad (unless, void, when)
import Coscad.Codegen
import Coscad.Constraint (Constraint (..), solveConstraints)
import Coscad.Geometry
import Coscad.IO (readFileUtf8, writeFileUtf8)
import Coscad.Json
import Coscad.Parser
import Coscad.Shape
import Coscad.Validate (finite, readFinite, suggestion, maxQuantity)
import Data.Char (isAlphaNum, isLetter, isSpace, toLower)
import Data.List (foldl', intercalate, sortBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
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
--   foo_design.json      design manifest: variants + plate placements
--                        (schema coscad.design/1). foo_manifest.json is
--                        written only by `coscad next` (schema coscad.next/1).

data POpt = OCount Int | ODown (Double, Double, Double) String | OHint (String, String)

data AsmStmt
  = APart String FilePath Int (Maybe ((Double, Double, Double), String)) [(String, String)]
  | APlate Double Double Double
  | AClearance Double
  | AFastener FastDecl
  | APlan [(String, String)]
  | AConstraint Constraint
  | ADef Def

-- | `fastener M5x8 bracket railA top ×2 nut=dropin at=350,370`: a screw
-- (spec) through the clamped instance into a slot/hole on the host
-- instance's named face (world-frame anchor word). Consumed by `coscad plan`.
data FastDecl = FastDecl
  { fdSpec :: String
  , fdClamped :: String
  , fdHost :: String
  , fdFace :: String
  , fdCount :: Int
  , fdOpts :: [(String, String)]
  , fdLine :: Int
  }
  deriving (Show)

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
  { arSource :: FilePath          -- the .assemble file this came from (for diagnostics)
  , arPlate :: (Double, Double, Double)
  , arAsm :: Maybe Shape
  , arFlat :: [FlatPart]
  , arMode :: SynMode           -- pragma of this .assemble file
  , arClearance :: Double       -- required clearance for `coscad check` (0 = contacts only)
  , arDefs :: [Def]             -- raw asm-side defs (re-resolvable with substitutions)
  , arParts :: [(String, Shape)]  -- top-level part name -> resolved shape
  , arPartMeta :: [(String, (Int, [(String, String)]))] -- top-level part name -> (count, hints)
  , arFasteners :: [FastDecl]   -- `fastener` lines, for `coscad plan`
  , arPlanOpts :: [(String, String)] -- `plan key=value ...` line
  , arConstraints :: [Constraint] -- rigid frames, re-resolved for check/plan
  }

asmProgram :: SynMode -> Parser [AsmStmt]
asmProgram mode = do
  located <- many ((,,) <$> getOffset <*> getSourcePos <*> asmStmt)
  distinct "part declaration" [(n, o, p) | (o, p, APart n _ _ _ _) <- located]
  distinct "plan option" [(k, o, p) | (o, p, APlan opts) <- located, (k, _) <- opts]
  mapM_ (\(o, _, st) -> case st of
    APart n _ _ _ _ | n `elem` reservedNames mode -> setOffset o *> fail ("reserved word '" ++ n ++ "' cannot be a part name; pick another name")
    _ -> return ()) located
  return [st | (_, _, st) <- located]
  where
    distinct what = go Map.empty
      where
        go _ [] = return ()
        go seen ((n, o, p) : rest) = case Map.lookup n seen of
          Just first -> setOffset o *> fail ("duplicate " ++ what ++ " '" ++ n ++ "' (first declared at " ++ sourcePosPretty first ++ ")")
          Nothing -> go (Map.insert n p seen) rest

-- Each alternative decides with lookahead only, then commits, so an
-- error inside a statement is reported where it happened.
asmStmt :: Parser AsmStmt
asmStmt =
  plateStmt
    <|> clearanceStmt
    <|> fastenerStmt
    <|> planStmt
    <|> fixedStmt
    <|> mateStmt
    <|> (lookAhead (try (identifier *> symbol "←")) *> partStmt)
    <|> (ADef <$> variableDefinition)

-- a bare word (instance names may carry `#2`, specs like M5x8)
word :: Parser String
word = lexeme (some (satisfy (\c -> not (isSpace c) && c /= '=' && c /= '×')))

-- A statement keyword commits only after dispatch, and cannot eat a new line.
statementKeyword :: String -> Parser ()
statementKeyword name = void $ try (string name <* notFollowedBy (alphaNumChar <|> char '_') <* hspace <* notFollowedBy (char '=' <|> char '←'))

fastenerStmt :: Parser AsmStmt
fastenerStmt = do
  pos <- getSourcePos
  statementKeyword "fastener"
  onStatementLine $ do
    spec <- word <?> "screw specification (M5x8 or peg)"
    clamped <- word <?> "clamped part instance"
    host <- word <?> "host part instance"
    face <- word <?> "host face (top bot lft rt fwd bak) or @frame"
    when (take 1 face == "@") $ either fail return (validFrameOption face)
    opts <- options "fastener" $ \opt -> case opt of
      OHint h -> validateFastenerHint h
      OCount n | n < 1 -> Left "fastener count must be at least 1; remove the declaration to omit the joint"
      OCount _ -> Right ()
      _ -> Left "fasteners take ×count and key=value options, not a print orientation"
    let cnt = last (1 : [n | OCount n <- opts])
    return (AFastener (FastDecl spec clamped host face cnt [h | OHint h <- opts] (unPos (sourceLine pos))))

planStmt :: Parser AsmStmt
planStmt = statementKeyword "plan" *> onStatementLine planOptions

fixedStmt :: Parser AsmStmt
fixedStmt = do
  pos <- getSourcePos
  statementKeyword "fixed"
  onStatementLine $ do
    name <- identifier <?> "fixed part instance"
    (t,r) <- ((,) (0,0,0) (0,0,0) <$ lookAhead eof) <|> ((,) <$> vector <*> vector)
    return (AConstraint (Fixed (sourcePosPretty pos) name t r))
  where
    vector = (,,) <$> literal <*> literal <*> literal
    literal = do
      n <- double <?> "six pose numbers: x y z rx ry rz (mm and degrees)"
      unless (finite n) (fail "fixed pose must be finite")
      return n

mateStmt :: Parser AsmStmt
mateStmt = do
  pos <- getSourcePos
  statementKeyword "mate"
  onStatementLine $ do
    child <- identifier <?> "child part instance"
    cf <- frameRef
    parent <- identifier <?> "parent part instance"
    pf <- frameRef
    opts <- options "mate" $ \opt -> case opt of
      OHint (k,v) | k `elem` ["roll", "gap"] -> numberHint k (if k == "roll" then "angle in degrees" else "distance in mm") (const True) "positive, zero or negative" v
      OHint (k,_) -> Left ("unknown mate option '" ++ k ++ "'" ++ suggestion ["roll", "gap"] k ++ "; choose roll or gap")
      _ -> Left "mate takes roll=degrees and gap=mm options"
    let get k = maybe 0 id (lookup k [h | OHint h <- opts] >>= readFinite)
    return (AConstraint (Mate (sourcePosPretty pos) child cf parent pf (get "roll") (get "gap")))

-- One shared resolution path for assembled views, tagged planner instances
-- and checker-hidden references. Counts are manufacturing multiplicities;
-- a constrained pose needs a distinct alias for every physical instance.
resolveAssembly :: AsmResult -> VarTable -> Either String VarTable
resolveAssembly ar = resolveWithConstraints (arMode ar) (arDefs ar) (arPartMeta ar) (arConstraints ar)

resolveWithConstraints :: SynMode -> [Def] -> [(String, (Int, [(String,String)]))] -> [Constraint] -> VarTable -> Either String VarTable
resolveWithConstraints mode defs meta constraints table0 = do
  case [d | d <- defs, defName d == "asm", not (null constraints)] of
    d : _ -> Left (sourcePosPretty (defPos d) ++ ": assembly constraints generate asm; remove the manual asm = definition or remove fixed/mate statements")
    [] -> return ()
  case [n | (n,(printCount,_)) <- meta, printCount > 1, not (null constraints)] of
    n : _ -> Left (location ++ ": constrained part '" ++ n ++ "' needs distinct instance names: use separate declarations for each copy (each with ×0 or ×1), not ×2 or larger")
    [] -> return ()
  table <- resolveVariables mode defs table0
  if null constraints then return table else do
    parts <- mapM (\(n,_) -> maybe (Left ("missing constrained instance '" ++ n ++ "'")) (Right . (,) n) (Map.lookup n table0)) meta
    asm <- solveConstraints parts constraints
    return (Map.insert "asm" asm table)
  where
    location = case constraints of Fixed loc _ _ _ : _ -> loc; Mate loc _ _ _ _ _ _ : _ -> loc; [] -> "assembly"

-- Parse line-local options without consuming a following `asm = ...` as a hint.
onStatementLine :: Parser a -> Parser a
onStatementLine p = do
  off <- getOffset
  rest <- expressionString
  case parse (sc *> p <* eof) "" rest of
    Left e -> parseError (shiftErr off (NE.head (bundleErrors e)))
    Right a -> return a
  where
    shiftErr k (TrivialError o u e) = TrivialError (o + k) u e
    shiftErr k (FancyError o e) = FancyError (o + k) e

-- Check each option at its token, including duplicates, before losing positions.
options :: String -> (POpt -> Either String ()) -> Parser [POpt]
options context validate = go []
  where
    go seen = (do
      off <- getOffset
      opt <- partOpt
      let key = case opt of OHint (k, _) -> k; OCount _ -> "×count"; ODown _ _ -> "▽orientation"
      when (key `elem` seen) (setOffset off *> fail ("duplicate " ++ context ++ " option '" ++ key ++ "'; specify it once"))
      either (\msg -> setOffset off *> fail msg) return (validate opt)
      (opt :) <$> go (key : seen)) <|> pure []

planOptions :: Parser AsmStmt
planOptions = do
  opts <- options "plan" $ \opt -> case opt of
    OHint h -> validatePlanOpt h
    _ -> Left "plan takes key=value options, e.g. plan beam=30 rest=bot,top"
  return (APlan [h | OHint h <- opts])

-- Planner-consumed part hints are checked; other hints remain free metadata.
validatePartHint :: (String, String) -> Either String ()
validatePartHint (key, raw) = case key of
  "material" -> enum "material" ["printed", "pla", "petg", "abs", "asa", "tpu", "nylon", "pc", "aluminium", "aluminum", "steel"] raw
  "profile" -> enum "profile" ["2020"] raw
  "preload" -> enum "preload mode" ["hand", "placed"] raw
  "ends" -> if length (splitComma raw) == 2
    then mapM_ (enum "rail end" ["open", "blocked"]) (splitComma raw)
    else Left "ends= expects two values: open,blocked (min end, max end)"
  "mass" -> numberHint key "mass in grams" (> 0) "greater than zero" raw
  "torque" -> numberHint key "torque in Nm" (> 0) "greater than zero" raw
  _ -> Right ()

validateFastenerHint :: (String, String) -> Either String ()
validateFastenerHint (key, raw) = case key of
  "nut" -> enum "nut kind" ["dropin", "drop-in", "slidein", "slide-in", "hex", "captive", "none"] raw
  "head" -> enum "head" ["button", "socket"] raw
  "seat" -> validFrameOption raw
  "at" -> mapM_ (numberHint key "distance in mm" (>= 0) "non-negative") (splitComma raw)
  "through" -> numberHint key "thickness in mm" (>= 0) "non-negative" raw
  "torque" -> numberHint key "torque in Nm" (> 0) "greater than zero" raw
  "inset" -> case readFinite raw of
    Nothing -> Left "inset= needs a distance in mm (finite and non-negative)"
    Just n | n < 0 -> Left "inset= must lie within the host (a non-negative distance in mm)"
    Just _ -> Right ()
  "pocket" -> let ref = case break (== '.') raw of ("host", '.' : f) -> f; ("clamped", '.' : f) -> f; _ -> raw
              in if take 1 ref == "@" then validFrameOption ref else validFace ref
  _ -> Left ("unknown fastener option '" ++ key ++ "'" ++ suggestion ["at", "nut", "head", "seat", "through", "torque", "pocket", "inset"] key ++ "; choose at, nut, head, seat, through, torque, pocket, or inset")

validFrameOption :: String -> Either String ()
validFrameOption ('@':c:n) | isLetter c, all (\x -> isAlphaNum x || x == '_') n = Right ()
validFrameOption raw = Left ("expected a named frame @name, got '" ++ raw ++ "'")

numberHint :: String -> String -> (Double -> Bool) -> String -> String -> Either String ()
numberHint key units valid range raw = case readFinite raw of
  Just n | valid n -> Right ()
  _ -> Left (key ++ "=" ++ raw ++ ": expected a finite " ++ units ++ ", " ++ range)

enum :: String -> [String] -> String -> Either String ()
enum label values raw = unless (map toLower raw `elem` values) $
  Left ("unknown " ++ label ++ " '" ++ raw ++ "'" ++ suggestion values raw ++ "; choose " ++ intercalate ", " values)

splitComma :: String -> [String]
splitComma s = case break (== ',') s of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitComma rest

validFace :: String -> Either String ()
validFace s = case parse (anchorVec <* eof) "" s of
  Right (x, y, z) | abs x + abs y + abs z == 1 -> Right ()
  _ -> Left ("unknown face '" ++ s ++ "'; choose top, bot, lft, rt, fwd, or bak (aliases allowed)")

-- Search controls must not silently fall back on defaults after a typo.
-- Arbitrary manufacturing hints on parts remain an extension mechanism.
validatePlanOpt :: (String, String) -> Either String ()
validatePlanOpt (key, raw)
  | key `elem` ["beam", "astar"] = case number of
      Just n | n >= 1, n == fromInteger (truncate n) ->
        if (truncate n :: Integer) <= toInteger (maxBound :: Int)
          then Right ()
          else Left (at ++ "positive whole number exceeds the maximum " ++ show (maxBound :: Int))
      _ -> Left (at ++ "expected a positive whole number (e.g. " ++ key ++ "=30)")
  | key `elem` ["flip", "vertical", "driver", "shrink", "sibling", "loose"] = case number of
      Just n | n >= 0 -> Right ()
      _ -> Left (at ++ "expected a finite, non-negative cost weight (e.g. " ++ key ++ "=1)")
  | key == "rest" = mapM_ face (splitFaces raw)
  | otherwise = Left ("unknown plan option '" ++ key ++ "'; expected beam, astar, rest, flip, vertical, driver, shrink, sibling, or loose")
  where
    at = "plan " ++ key ++ "=" ++ raw ++ ": "
    number = case reads (if take 1 raw == "." then '0' : raw else raw) of
      [(n, "")] | not (isNaN n || isInfinite n) -> Just (n :: Double)
      _ -> Nothing
    splitFaces s = case break (== ',') s of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitFaces rest
    face s = case parse (anchorVec <* eof) "" s of
      Right (x, y, z) | abs x + abs y + abs z == 1 -> Right ()
      _ -> Left (at ++ "unknown rest face '" ++ s ++ "'; choose top, bot, lft, rt, fwd, or bak (aliases allowed)")

clearanceStmt :: Parser AsmStmt
clearanceStmt = statementKeyword "clearance" *> onStatementLine (AClearance <$> distance "clearance" (>= 0) "non-negative")

plateStmt :: Parser AsmStmt
plateStmt = statementKeyword "plate" *> onStatementLine (do
  w <- distance "plate width" (> 0) "greater than zero"
  d <- distance "plate depth" (> 0) "greater than zero"
  off <- getOffset
  m <- distance "plate margin" (>= 0) "non-negative" <|> pure 6
  when (m >= w / 2 || m >= d / 2) (setOffset off *> fail "plate margin must leave usable space: twice the margin must be smaller than width and depth")
  return (APlate w d m))

distance :: String -> (Double -> Bool) -> String -> Parser Double
distance name valid range = do
  off <- getOffset
  n <- double <?> (name ++ " in mm")
  unless (finite n && valid n) (setOffset off *> fail (name ++ " must be a finite distance in mm, " ++ range))
  return n

partStmt :: Parser AsmStmt
partStmt = do
  name <- identifier
  _ <- char '←' <* hspace
  onStatementLine (partOpts name)

partOpts :: String -> Parser AsmStmt
partOpts name = do
  path <- lexeme (some (satisfy (not . isSpace))) <?> "a part path (.coscad or .assemble)"
  opts <- options "part" $ \opt -> case opt of
    OHint h -> validatePartHint h
    _ -> Right ()
  let cnt = last (1 : [n | OCount n <- opts])
      down = case [(v, raw) | ODown v raw <- opts] of
        (d : _) -> Just d
        [] -> Nothing
      hints = [h | OHint h <- opts]
  return (APart name path cnt down hints)

partOpt :: Parser POpt
partOpt =
  choice
    [ do
        symbol "×"
        off <- getOffset
        n <- lexeme L.decimal :: Parser Integer
        if n > toInteger maxQuantity
          then setOffset off *> fail ("part/fastener count is too large; maximum is " ++ show maxQuantity)
          else return (OCount (fromInteger n))
    , do
        symbol "▽"
        (raw, v) <- match anchorVec
        return (ODown v (trim raw))
    , do
        _ <- lookAhead (try (identifier *> symbol "="))
        k <- identifier
        _ <- char '=' <* hspace
        v <- (notFollowedBy (string "//") *> lexeme (some (satisfy (not . isSpace)))) <|> fail ("missing value after '" ++ k ++ "='")
        return (OHint (k, v))
    ]

loadAssembleFile :: [FilePath] -> FilePath -> IO (Either String AsmResult)
loadAssembleFile visited path
  | path `elem` visited = return (Left (path ++ ": cycle in .assemble references involving this file" ++ concatMap (" <- " ++) (reverse visited)))
  | otherwise = do
      exists <- doesFileExist path
      if not exists
        then return (Left (path ++ ": file not found"))
        else do
          contents <- readFileUtf8 path
          case checkedPragma path contents of
            Left err -> return (Left err)
            Right (fmode, fbody) -> case parse (sc *> asmProgram fmode <* eof) path fbody of
              Left err -> return (Left (prettyBundle err))
              Right stmts -> do
                let dir = takeDirectory path
                    partsS = [p | p@APart {} <- stmts]
                    defs = [d | ADef d <- stmts]
                    plate = last ((220, 220, 6) : [(w, d, m) | APlate w d m <- stmts])
                    clr = last (0 : [c | AClearance c <- stmts])
                loaded <- mapM (loadRef (path : visited) dir) partsS
                case sequence loaded of
                  Left err -> return (Left err)
                  Right entries | sum [toInteger (fpCount f) | (_, _, fs) <- entries, f <- fs] > toInteger maxQuantity ->
                    return (Left (path ++ ": total expanded print count is too large; maximum is " ++ show maxQuantity))
                  Right entries -> do
                    let table0 = Map.fromList [(n, s) | (n, s, _) <- entries]
                        meta = [(n, (c, h)) | APart n _ c _ h <- partsS]
                        constraints = [c | AConstraint c <- stmts]
                    case resolveWithConstraints fmode defs meta constraints table0 of
                      Left err -> return (Left err)
                      Right table ->
                        return $
                          Right
                            AsmResult
                              { arSource = path
                              , arPlate = plate
                              , arAsm = Map.lookup "asm" table
                              , arMode = fmode
                              , arClearance = clr
                              , arDefs = defs
                              , arParts = [(n, s) | (n, s, _) <- entries]
                              , arPartMeta = meta
                              , arFasteners = [f | AFastener f <- stmts]
                              , arPlanOpts = concat [o | APlan o <- stmts]
                              , arConstraints = constraints
                              , arFlat = concat [f | (_, _, f) <- entries]
                              }

loadRef :: [FilePath] -> FilePath -> AsmStmt -> IO (Either String (String, Shape, [FlatPart]))
loadRef visited dir (APart name rel cnt down hints) = do
  let p = dir </> rel
      referrer = case visited of (v : _) -> takeFileName v; [] -> "the assembly"
  case takeExtension p of
    ".coscad" -> do
      exists <- doesFileExist p
      if not exists
        then return (Left (p ++ ": part file not found (referenced from " ++ referrer ++ ")"))
        else do
          c <- readFileUtf8 p
          case parseProgramNamed p c of
            Left err -> return (Left err)
            Right (_, shape) ->
              return (Right (name, shape, [FlatPart [] name p cnt down hints shape]))
    ".assemble" -> do
      r <- loadAssembleFile visited p
      case r of
        Left err -> return (Left err)
        Right ar -> case arAsm ar of
          Nothing -> return (Left (p ++ ": subassembly has no 'asm' definition"))
          Just _ | any (\f -> toInteger (fpCount f) * toInteger cnt > toInteger maxQuantity) (arFlat ar) ->
            return (Left (p ++ ": expanded print count for '" ++ name ++ "' is too large; maximum is " ++ show maxQuantity))
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
    ext -> return (Left (p ++ ": unsupported part reference (" ++ ext ++ "); use a .coscad or .assemble file"))
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

-- | One file-safe name per print variant (distinct source and orientation),
-- in first-seen order. Both stages name their per-variant artefacts and
-- manifest ids with these. The historical `part_down` name is kept when it
-- is unique. Colliding names (the same part name from two sources, e.g. a
-- `base` in two subassemblies, or names equal after sanitizing) are
-- qualified by the assembly trail, then numbered; the result is unique.
variantNames :: [FlatPart] -> [(VKey, String)]
variantNames printable = zip keys (unique (zipWith pick plain qualified))
  where
    keys = foldl' (\acc fp -> if vkey fp `elem` acc then acc else acc ++ [vkey fp]) [] printable
    firsts = [fp | k <- keys, fp <- take 1 [f | f <- printable, vkey f == k]]
    downOf = maybe "asis" snd . fpDown
    plain = [sanitize (fpName fp ++ "_" ++ downOf fp) | fp <- firsts]
    qualified = [sanitize (intercalate "_" (fpTrail fp ++ [fpName fp, downOf fp])) | fp <- firsts]
    occurrences xs x = length (filter (== x) xs)
    pick p q = if occurrences plain p > 1 then q else p
    -- Number whatever still collides, skipping numbered names that are taken.
    unique names = reverse (foldl' step [] names)
      where
        step used n
          | occurrences names n == 1 = n : used
          | otherwise = free (1 :: Int) : used
          where
            free i =
              let c = n ++ "_" ++ show i
               in if c `elem` used || c `elem` names then free (i + 1) else c

instId :: FlatPart -> Int -> String
instId fp i = intercalate "/" (fpTrail fp ++ [fpName fp]) ++ "_" ++ show i

-- | Shelf-pack footprints onto as many plates as needed, largest
-- first; margin around and between. Shared by the design stage
-- (`coscad foo.assemble`) and the manufacturing stage (`coscad next`).
packBeds ::
  (Double, Double, Double) ->
  [(String, VKey, (Double, Double))] ->
  Either String [[(String, VKey, (Double, Double), (Double, Double))]]
packBeds (pw, pd, m) items0 = do
  unless (all finite [pw, pd, m] && pw > 0 && pd > 0 && m >= 0 && m < pw / 2 && m < pd / 2) $
    Left "plate dimensions must be finite and positive; margin must be non-negative and leave usable space"
  mapM_ (\(iid, _, (w, d)) -> unless (finite w && finite d && w > 0 && d > 0) (Left (iid ++ " footprint must be finite and positive"))) items0
  go (sortBy bigger items0)
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
          Left (iid ++ " footprint " ++ show w ++ "x" ++ show d ++ " exceeds the " ++ show pw ++ "x" ++ show pd ++ " plate with " ++ show m ++ " margin")
      | x + w > pw - m = fit m (y + rowD + m) 0 acc (item : rest)
      | y + d > pd - m = Right (reverse acc, item : rest)
      | otherwise = fit (x + w + m) y (max rowD d) ((iid, k, (w, d), (x, y)) : acc) rest

-- | Manifest schema identifiers. The design stage and `coscad next` write
-- different files with different shapes; consumers check `schema` first.
designSchema, nextSchema :: String
designSchema = "coscad.design/1"
nextSchema = "coscad.next/1"

-- | Design-stage manifest path for an assembly base path.
designManifestPath :: FilePath -> FilePath
designManifestPath base = base ++ "_design.json"

-- | Manufacturing manifest path (`coscad next`) for an assembly base path.
nextManifestPath :: FilePath -> FilePath
nextManifestPath base = base ++ "_manifest.json"

-- | A `<base>_manifest.json` left by a design stage from coscad 1.2 or
-- earlier: it has a `plate` but neither `beds` nor `schema`.
legacyDesignManifest :: String -> Bool
legacyDesignManifest txt = case decode txt of
  Right v -> isJust (lookupKey "plate" v) && isNothing (lookupKey "beds" v) && isNothing (lookupKey "schema" v)
  Left _ -> False

manifestJson ::
  FilePath ->
  String ->
  (Double, Double, Double) ->
  [(VKey, FlatPart, Shape, (Double, Double, Double), String)] ->
  [[(String, VKey, (Double, Double), (Double, Double))]] ->
  [FlatPart] ->
  String
manifestJson src base (pw, pd, m) variants plates asmOnly =
  encodePretty $
    jobj
      [ ("schema", JString designSchema)
      , ("source", JString src)
      , ("plate", jobj [("w", jnum3 pw), ("d", jnum3 pd), ("margin", jnum3 m), ("count", JNumber (fromIntegral (length plates)))])
      , ("variants", jlist (map vj variants))
      , ("placements", jlist [pj i p | (i, ps) <- zip [(1 :: Int) ..] plates, p <- ps])
      , ("assembly_only", jlist (map aj asmOnly))
      ]
  where
    placed = concat plates
    vj (k@(srcf, dk), fp, _, (w, d, h), vname) =
      jobj
        [ ("id", JString vname)
        , ("part", JString (fpName fp))
        , ("source", JString srcf)
        , ("down", JString dk)
        , ("count", JNumber (fromIntegral (length [() | (_, k', _, _) <- placed, k' == k])))
        , ("footprint", jlist (map jnum3 [w, d, h]))
        , ("scad", JString (base ++ "_part_" ++ vname ++ ".scad"))
        , ("hints", jobj [(hk, JString hv) | (hk, hv) <- fpHints fp])
        ]
    pj i (iid, k, (w, d), (x, y)) =
      jobj
        [ ("instance", JString iid)
        , ("variant", JString (vnameOfK k))
        , ("plate", JNumber (fromIntegral i))
        , ("x", jnum3 x), ("y", jnum3 y), ("w", jnum3 w), ("d", jnum3 d)
        ]
    vnameOfK k = head [vn | (k', _, _, _, vn) <- variants, k' == k]
    aj fp = jobj [("part", JString (fpName fp)), ("source", JString (fpSource fp))]
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
      let names = variantNames printable
          vkeys = map fst names
          mkVariant k =
            let fp = head [f | f <- printable, vkey f == k]
                oriented = resolve (orientForBed (fpDown fp) (fpShape fp))
                (lo, hi) = bbox oriented
                norm = Translate (vneg lo) oriented
                dims = vsub hi lo
                vname = maybe (sanitize (fpName fp)) id (lookup k names)
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
      case packBeds plate [(iid, k, footOf k) | (iid, k) <- instances] of
        Left err -> do
          hPutStrLn stderr ("Error: " ++ err)
          exitFailure
        Right plates -> do
          let nPlates = length plates
              plateFile i = base ++ (if nPlates == 1 then "_plate" else "_plate" ++ show i) ++ ".scad"
          mapM_
            ( \(i, placed) -> do
                let slab = Translate (0, 0, -0.8) (Rectangle pw pd 0.6)
                    plateShape = Union (slab : [Translate (x, y, 0) (normOf k) | (_, k, _, (x, y)) <- placed])
                writeScad plateShape (plateFile i)
                putStrLn ("Wrote " ++ plateFile i ++ " (" ++ show (length placed) ++ " parts)")
            )
            (zip [(1 :: Int) ..] plates)
          when (nPlates > 1) $
            putStrLn ("(" ++ show (length instances) ++ " parts need " ++ show nPlates ++ " plates of " ++ showD pw ++ "x" ++ showD pd ++ ")")
          writeFileUtf8 (designManifestPath base) (manifestJson inputFile baseName plate variants plates asmOnly)
          putStrLn ("Wrote " ++ designManifestPath base)
          noteLegacyManifest base

-- | Explain a stale design manifest under the manufacturing name instead of
-- leaving a file that `bambu-slice.py` will refuse without context. The
-- file is left in place: it may be referenced by the user's own scripts.
noteLegacyManifest :: FilePath -> IO ()
noteLegacyManifest base = do
  let old = nextManifestPath base
  exists <- doesFileExist old
  when exists $ do
    txt <- readFileUtf8 old
    when (legacyDesignManifest txt) $
      hPutStrLn stderr ("Note: " ++ old ++ " is a design manifest from coscad 1.2 or earlier. The design stage now writes "
        ++ designManifestPath base ++ "; " ++ takeFileName old ++ " is reserved for `coscad next`. Delete it or run `coscad next` to replace it.")
