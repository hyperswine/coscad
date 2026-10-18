{-# LANGUAGE PatternSynonyms #-}

-- | The .assemble subsystem: recursive part references with print
-- counts, orientation declarations, and hints; design-stage outputs
-- (assembled view, packed plate, per-variant scads, manifest).
module Coscad.Assemble (module Coscad.Assemble) where

import Control.Monad (unless, void, when)
import Coscad.Codegen
import Coscad.Constraint (AsmError (..), Constraint (..), Tolerance (..), defaultTolerance, errorText, solveConstraints)
import Coscad.Geometry
import Coscad.IO (readFileUtf8, writeFileUtf8)
import Coscad.Json
import Coscad.Parser
import Coscad.Shape
import Coscad.Validate (finite, readFinite, suggestion, maxQuantity)
import Data.Char (isAlphaNum, isDigit, isLetter, isSpace, toLower)
import Data.List (foldl', intercalate, sortBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeFileName, (</>))
import Data.Void (Void)
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
--   plate 220 220 6 180                   -- bed W D [margin [max height]]
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
  | APlate Double Double Double Double (Maybe Double) -- ^ width, depth, margin, max print height, brim=N for parts that recommend one
  | AClearance Double
  | AFastener FastDecl
  | APlan [(String, String)]
  | AConstraint (NumEnv -> Either (ParseErrorBundle String Void) Constraint) -- ^ evaluated once the file's numeric bindings are known
  | ATolerance Tolerance
  | ADef Def

-- | `fastener M5x8 bracket railA top ×2 nut=dropin at=350,370`: a screw
-- (spec) through the clamped instance into a slot/hole on the host
-- instance's named face (world-frame anchor word). Consumed by `coscad plan`.
-- | A numeric argument of `fixed` or a `mate` option: a literal, or an
-- atom (numeric binding, -atom, parenthesized arithmetic) kept with its
-- offset in the file so it can be evaluated after the bindings resolve
-- and still report errors at its own token.
data NumArg = NumLit Double | NumSrc Int String

-- | What evaluating a 'NumArg' needs: the file (for error positions and
-- excerpts) and the numeric bindings.
data NumEnv = NumEnv FilePath String Ctx

evalNumArg :: NumEnv -> NumArg -> Either (ParseErrorBundle String Void) Double
evalNumArg _ (NumLit v) = Right v
evalNumArg (NumEnv path body ctx) (NumSrc off txt) = snd (runParser' (num ctx <* eof) st)
  where
    st =
      State
        { stateInput = txt
        , stateOffset = off
        , statePosState = PosState {pstateInput = body, pstateOffset = 0, pstateSourcePos = initialPos path, pstateTabWidth = defaultTabWidth, pstateLinePrefix = ""}
        , stateParseErrors = []
        }

-- | Parse one numeric argument without evaluating it. A plain literal is
-- kept as its value; anything else must be one atom, as in `.coscad`
-- argument position: a name, @-atom@, or @( arithmetic )@.
numArg :: String -> String -> (String -> String) -> Parser NumArg
numArg what missing nonFinite = do
  off <- getOffset
  tok <- lookAhead (many (satisfy (not . isSpace)))
  lit <- optional (try (L.signed (pure ()) (try L.float <|> fromIntegral <$> (L.decimal :: Parser Integer)) <* notFollowedBy (satisfy (not . isSpace))))
  case lit of
    Just v -> do
      sc
      unless (finite v) (setOffset off *> fail (nonFinite tok))
      return (NumLit v)
    Nothing -> do
      let numeric = case dropWhile (`elem` "+-") tok of
            c : _ -> isDigit c || c == '.'
            [] -> False
      when (null tok || take 2 tok == "//") (fail missing)
      when (dropWhile (`elem` "+-") tok `elem` ["NaN", "Infinity"]) (fail (nonFinite tok))
      when numeric (fail (what ++ ": '" ++ tok ++ "' is not a number"))
      (txt, _) <- match atomSyntax <?> what
      next <- optional (lookAhead (satisfy (not . isSpace)))
      when (isJust next) (setOffset off *> fail (what ++ ": '" ++ tok ++ "' is not one numeric atom; write arithmetic in parentheses, e.g. (w / 2)"))
      sc
      return (NumSrc off txt)
  where
    atomSyntax = (char '-' *> atomSyntax) <|> parens <|> void (identifier' :: Parser String)
    identifier' = (:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> oneOf "₀₁₂₃₄₅₆₇₈₉")
    parens = char '(' *> skipMany (parens <|> void (satisfy (\c -> c /= '(' && c /= ')'))) <* (void (char ')') <|> fail "missing ')' in a numeric expression")

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
  , arPlateBrim :: Maybe Double  -- `plate … brim=N`: brim reserved by `next` for parts whose printability recommends one
  , arPlateMaxZ :: Double        -- `plate W D margin maxZ`: tallest part the printer can build (default 'defaultMaxZ')
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
  , arTolerance :: Tolerance -- `tolerance` line: how far a mating loop may miss
  , arPartPos :: [(String, String)] -- top-level part name -> declaration file:line:col
  }

asmProgram :: SynMode -> Parser [(SourcePos, AsmStmt)]
asmProgram mode = do
  located <- many ((,,) <$> getOffset <*> getSourcePos <*> asmStmt)
  distinct "part declaration" [(n, o, p) | (o, p, APart n _ _ _ _) <- located]
  distinct "plan option" [(k, o, p) | (o, p, APlan opts) <- located, (k, _) <- opts]
  mapM_ (\(o, _, st) -> case st of
    APart n _ _ _ _ | n `elem` reservedNames mode -> setOffset o *> fail ("reserved word '" ++ n ++ "' cannot be a part name; pick another name")
    _ -> return ()) located
  return [(p, st) | (_, p, st) <- located]
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
    <|> toleranceStmt
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
    args <- ([] <$ lookAhead eof) <|> count 6 (numArg "fixed pose" "fixed needs six pose numbers: x y z rx ry rz (mm and degrees; numeric bindings and (arithmetic) allowed)" (const "fixed pose must be finite"))
    return $ AConstraint $ \env -> do
      vs <- mapM (evalNumArg env) args
      let (t, r) = case vs of
            [x, y, z, rx, ry, rz] -> ((x, y, z), (rx, ry, rz))
            _ -> ((0, 0, 0), (0, 0, 0))
      return (Fixed (sourcePosPretty pos) name t r)

mateStmt :: Parser AsmStmt
mateStmt = do
  pos <- getSourcePos
  statementKeyword "mate"
  onStatementLine $ do
    child <- identifier <?> "child part instance"
    cf <- frameRef
    parent <- identifier <?> "parent part instance"
    pf <- frameRef
    opts <- mateOptions []
    return $ AConstraint $ \env -> do
      let get k = maybe (Right 0) (evalNumArg env) (lookup k opts)
      roll <- get "roll"
      gap <- get "gap"
      return (Mate (sourcePosPretty pos) child cf parent pf roll gap)
  where
    mateOptions seen =
      ( do
          off <- getOffset
          _ <- lookAhead (try (identifier *> symbol "="))
          k <- identifier
          _ <- char '=' <* hspace
          when (k `elem` seen) (setOffset off *> fail ("duplicate mate option '" ++ k ++ "'; specify it once"))
          unless (k `elem` ["roll", "gap"]) (setOffset off *> fail ("unknown mate option '" ++ k ++ "'" ++ suggestion ["roll", "gap"] k ++ "; choose roll or gap"))
          let units = if k == "roll" then "angle in degrees" else "distance in mm"
              what = "mate " ++ k ++ " (" ++ units ++ ")"
          v <- numArg what ("missing value after '" ++ k ++ "='") (\raw -> k ++ "=" ++ raw ++ ": expected a finite " ++ units ++ ", positive, zero or negative")
          ((k, v) :) <$> mateOptions (k : seen)
      )
        <|> ( do
                bad <- optional (lookAhead (symbol "×" <|> symbol "▽"))
                when (isJust bad) (fail "mate takes roll=degrees and gap=mm options")
                pure []
            )

-- One shared resolution path for assembled views, tagged planner instances
-- and checker-hidden references. Counts are manufacturing multiplicities;
-- a constrained pose needs a distinct alias for every physical instance.
resolveAssembly :: AsmResult -> VarTable -> Either String VarTable
resolveAssembly ar = either (Left . errorText) Right . resolveAssemblyChecked ar

-- | 'resolveAssembly' keeping the input/design distinction for exit codes.
resolveAssemblyChecked :: AsmResult -> VarTable -> Either AsmError VarTable
resolveAssemblyChecked ar = resolveWithConstraints (arMode ar) (arTolerance ar) (arDefs ar) (arPartMeta ar) (arPartPos ar) (arConstraints ar)

resolveWithConstraints :: SynMode -> Tolerance -> [Def] -> [(String, (Int, [(String,String)]))] -> [(String, String)] -> [Constraint] -> VarTable -> Either AsmError VarTable
resolveWithConstraints mode tol defs meta positions constraints table0 = do
  case [d | d <- defs, defName d == "asm", not (null constraints)] of
    d : _ -> Left (InputError (sourcePosPretty (defPos d) ++ ": assembly constraints generate asm; remove the manual asm = definition or remove fixed/mate statements"))
    [] -> return ()
  case [n | (n,(printCount,_)) <- meta, printCount > 1, not (null constraints)] of
    n : _ -> Left (InputError (declared n ++ ": constrained part '" ++ n ++ "' needs distinct instance names: use separate declarations for each copy (each with ×0 or ×1), not ×2 or larger"))
    [] -> return ()
  table <- either (Left . InputError) Right (resolveVariables mode defs table0)
  if null constraints then return table else do
    parts <- mapM (\(n,_) -> maybe (Left (InputError (declared n ++ ": missing constrained instance '" ++ n ++ "'"))) (\s -> Right (n, declared n, s)) (Map.lookup n table0)) meta
    asm <- solveConstraints tol parts constraints
    return (Map.insert "asm" asm table)
  where
    location = case constraints of Fixed loc _ _ _ : _ -> loc; Mate loc _ _ _ _ _ _ : _ -> loc; [] -> "assembly"
    declared n = maybe location id (lookup n positions)

-- Parse line-local options without consuming a following `asm = ...` as a hint.
onStatementLine :: Parser a -> Parser a
onStatementLine p = do
  off <- getOffset
  rest <- expressionString
  -- the inner parse starts at the line's offset in the file, so offsets
  -- inside it (and in its errors) are file offsets
  let st =
        State
          { stateInput = rest
          , stateOffset = off
          , statePosState = PosState {pstateInput = rest, pstateOffset = off, pstateSourcePos = initialPos "", pstateTabWidth = defaultTabWidth, pstateLinePrefix = ""}
          , stateParseErrors = []
          }
  case snd (runParser' (sc *> p <* eof) st) of
    Left e -> parseError (NE.head (bundleErrors e))
    Right a -> return a

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
  "brim" -> numberHint key "brim width in mm" (>= 0) "non-negative (brim=0 opts out of a plate brim)" raw
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

-- | `tolerance <mm> [<rotation>]`: how far a mating loop may miss and
-- still close (defaults 1e-6 mm and 1e-8; see 'Tolerance').
toleranceStmt :: Parser AsmStmt
toleranceStmt = statementKeyword "tolerance" *> onStatementLine (do
  p <- distance "position tolerance" (> 0) "greater than zero"
  r <- distance "rotation tolerance" (> 0) "greater than zero" <|> pure (tolRotation defaultTolerance)
  return (ATolerance (Tolerance p r)))

clearanceStmt :: Parser AsmStmt
clearanceStmt = statementKeyword "clearance" *> onStatementLine (AClearance <$> distance "clearance" (>= 0) "non-negative")

plateStmt :: Parser AsmStmt
plateStmt = statementKeyword "plate" *> onStatementLine (do
  w <- distance "plate width" (> 0) "greater than zero"
  d <- distance "plate depth" (> 0) "greater than zero"
  off <- getOffset
  m <- distance "plate margin" (>= 0) "non-negative" <|> pure 6
  when (m >= w / 2 || m >= d / 2) (setOffset off *> fail "plate margin must leave usable space: twice the margin must be smaller than width and depth")
  z <- distance "plate max height" (> 0) "greater than zero" <|> pure defaultMaxZ
  brim <- optional (do
    _ <- symbol "brim"
    _ <- char '=' <* hspace
    (symbol "auto" *> pure defaultBrimMm) <|> distance "plate brim" (>= 0) "non-negative (or brim=auto)")
  return (APlate w d m z brim))

-- | Print height `plate W D margin` assumes without a 4th argument: the
-- 250 mm `coscad next` always used (A1, X1, P1). Set it for smaller
-- printers, e.g. @plate 180 180 6 180@ for an A1 mini.
defaultMaxZ :: Double
defaultMaxZ = 250

-- | Width `plate … brim=auto` reserves: Bambu Studio's default brim width.
defaultBrimMm :: Double
defaultBrimMm = 5

-- | The brim an instance asks for with its own @brim=N@ hint. The
-- outermost reference wins, as hints from an enclosing @.assemble@ come
-- after the subassembly's own.
brimHint :: FlatPart -> Maybe Double
brimHint fp = case [v | ("brim", v) <- reverse (fpHints fp)] of
  v : _ -> readFinite v
  [] -> Nothing

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

-- | Load an assembly, flattening any error to its text (tests and tools
-- that only need the message). Commands use 'loadAssembly' and 'failAsm'.
loadAssembleFile :: [FilePath] -> FilePath -> IO (Either String AsmResult)
loadAssembleFile visited path = either (Left . errorText) Right <$> loadAssembly visited path

-- | Report a load error the way every command does: the first line is
-- `Error: file:line:col: reason`, any further lines (`  note: ...`) follow
-- verbatim. Input errors exit 1, design errors exit 2.
failAsm :: AsmError -> IO a
failAsm e = do
  case lines (errorText e) of
    [] -> hPutStrLn stderr "Error: assembly could not be loaded"
    first : rest -> mapM_ (hPutStrLn stderr) (("Error: " ++ first) : rest)
  exitWith (ExitFailure (case e of DesignError _ -> 2; InputError _ -> 1))

loadAssembly :: [FilePath] -> FilePath -> IO (Either AsmError AsmResult)
loadAssembly visited path
  | path `elem` visited = return (Left (InputError (path ++ ": cycle in .assemble references involving this file" ++ concatMap (" <- " ++) (reverse visited))))
  | otherwise = do
      exists <- doesFileExist path
      if not exists
        then return (Left (InputError (path ++ ": file not found")))
        else do
          contents <- readFileUtf8 path
          case checkedPragma path contents of
            Left err -> return (Left (InputError err))
            Right (fmode, fbody) -> case parse (sc *> asmProgram fmode <* eof) path fbody of
              Left err -> return (Left (InputError (prettyBundle err)))
              Right located -> do
                let stmts = map snd located
                    positions = [(n, sourcePosPretty p) | (p, APart n _ _ _ _) <- located]
                    dir = takeDirectory path
                    partsS = [p | p@APart {} <- stmts]
                    defs = [d | ADef d <- stmts]
                    plate = last ((220, 220, 6) : [(w, d, m) | APlate w d m _ _ <- stmts])
                    plateMaxZ = last (defaultMaxZ : [z | APlate _ _ _ z _ <- stmts])
                    plateBrim = last (Nothing : [b | APlate _ _ _ _ b <- stmts])
                    clr = last (0 : [c | AClearance c <- stmts])
                loaded <- mapM (loadRef (path : visited) dir) partsS
                case sequence loaded of
                  Left err -> return (Left err)
                  Right entries | sum [toInteger (fpCount f) | (_, _, fs) <- entries, f <- fs] > toInteger maxQuantity ->
                    return (Left (InputError (path ++ ": total expanded print count is too large; maximum is " ++ show maxQuantity)))
                  Right entries -> do
                    let table0 = Map.fromList [(n, s) | (n, s, _) <- entries]
                        meta = [(n, (c, h)) | APart n _ c _ h <- partsS]
                        tol = last (defaultTolerance : [t | ATolerance t <- stmts])
                        evaluated = do
                          (nums, _) <- either (Left . InputError) Right (numericPhase fmode defs table0)
                          let env = NumEnv path fbody (Ctx fmode table0 nums)
                          mapM (\c -> either (Left . InputError . prettyBundle) Right (c env)) [c | AConstraint c <- stmts]
                    case evaluated >>= \constraints -> (,) constraints <$> resolveWithConstraints fmode tol defs meta positions constraints table0 of
                      Left err -> return (Left err)
                      Right (constraints, table) ->
                        return $
                          Right
                            AsmResult
                              { arSource = path
                              , arPlate = plate
                              , arPlateBrim = plateBrim
                              , arPlateMaxZ = plateMaxZ
                              , arAsm = Map.lookup "asm" table
                              , arMode = fmode
                              , arClearance = clr
                              , arDefs = defs
                              , arParts = [(n, s) | (n, s, _) <- entries]
                              , arPartMeta = meta
                              , arFasteners = [f | AFastener f <- stmts]
                              , arPlanOpts = concat [o | APlan o <- stmts]
                              , arConstraints = constraints
                              , arTolerance = tol
                              , arPartPos = positions
                              , arFlat = concat [f | (_, _, f) <- entries]
                              }

loadRef :: [FilePath] -> FilePath -> AsmStmt -> IO (Either AsmError (String, Shape, [FlatPart]))
loadRef visited dir (APart name rel cnt down hints) = do
  let p = dir </> rel
      referrer = case visited of (v : _) -> takeFileName v; [] -> "the assembly"
  case takeExtension p of
    ".coscad" -> do
      exists <- doesFileExist p
      if not exists
        then return (Left (InputError (p ++ ": part file not found (referenced from " ++ referrer ++ ")")))
        else do
          c <- readFileUtf8 p
          case parseProgramNamed p c of
            Left err -> return (Left (InputError err))
            Right (_, shape) ->
              return (Right (name, shape, [FlatPart [] name p cnt down hints shape]))
    ".assemble" -> do
      r <- loadAssembly visited p
      case r of
        Left err -> return (Left err)
        Right ar -> case arAsm ar of
          Nothing -> return (Left (InputError (p ++ ": subassembly has no 'asm' definition")))
          Just _ | any (\f -> toInteger (fpCount f) * toInteger cnt > toInteger maxQuantity) (arFlat ar) ->
            return (Left (InputError (p ++ ": expanded print count for '" ++ name ++ "' is too large; maximum is " ++ show maxQuantity)))
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
    ext -> return (Left (InputError (p ++ ": unsupported part reference (" ++ ext ++ "); use a .coscad or .assemble file")))
loadRef _ _ _ = return (Left (InputError "internal error: loadRef on non-part statement"))

-- | Rotate a shape so the declared anchor faces the bed (-Z),
-- rotating about its own bbox center. Nothing = already print-ready.
orientForBed :: Maybe ((Double, Double, Double), String) -> Shape -> Shape
orientForBed Nothing s = s
orientForBed (Just (v, _)) s = case downRot v of
  Nothing -> s
  Just (ang, ax) ->
    let c = bcenter (bbox s)
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

-- | One packed instance: its id and variant, its footprint as placed
-- (width and depth swapped when turned), the corner of the part itself
-- (inside any reserved brim), the brim reserved around it, and its turn
-- about Z: 0, or 90 degrees counter-clockwise about the variant's
-- origin followed by a shift of its original depth along +X, so the
-- turned part still starts at its local (0, 0) (see 'turnZ').
data Placement = Placement
  { plInstance :: String
  , plKey :: VKey
  , plSize :: (Double, Double)
  , plAt :: (Double, Double)
  , plBrim :: Double
  , plRotZ :: Int
  }
  deriving (Eq, Show)

-- | The point a variant-local point (at the origin, footprint @w × d@)
-- moves to when its placement is turned 90° about Z: @(d - y, x)@.
turnZ :: Double -> (Double, Double, Double) -> (Double, Double, Double)
turnZ d (x, y, z) = (d - y, x, z)

-- | Shelf-pack footprints onto as many plates as needed, with the margin
-- around and between them. Shared by the design stage
-- (`coscad foo.assemble`) and the manufacturing stage (`coscad next`).
packBeds ::
  (Double, Double, Double) ->
  [(String, VKey, (Double, Double))] ->
  Either String [[Placement]]
packBeds plate items = packBedsBrim plate [(iid, k, wd, 0) | (iid, k, wd) <- items]

-- | 'packBeds' with a brim width reserved around each footprint: the
-- packer places the footprint grown by the brim on every side, so the
-- part keeps the margin to its neighbours and the plate edge *outside*
-- its brim. Placements are the part's own corner and size (the brim is
-- returned alongside), so meshes are stamped exactly as before.
--
-- First-fit decreasing over shelves, deterministic: items go largest
-- reserved area first (ties in input order). A plate stays open until
-- every remaining item has been tried; an item that fits nowhere waits
-- for the next plate. Each item takes the first of
--
--   1. the open (last) shelf, as declared;
--   2. a gap at the end of an earlier shelf, as declared;
--   3. any shelf, turned 90° about Z;
--   4. a new shelf, as declared;
--   5. a new shelf, turned.
--
-- so a layout the old next-fit packer could finish is kept unless a
-- later, smaller item can fill a gap it left. Square cells never turn.
packBedsBrim ::
  (Double, Double, Double) ->
  [(String, VKey, (Double, Double), Double)] ->
  Either String [[Placement]]
packBedsBrim (pw, pd, m) items0 = do
  unless (all finite [pw, pd, m] && pw > 0 && pd > 0 && m >= 0 && m < pw / 2 && m < pd / 2) $
    Left "plate dimensions must be finite and positive; margin must be non-negative and leave usable space"
  mapM_ (\(iid, _, (w, d), b) -> do
    unless (finite w && finite d && w > 0 && d > 0) (Left (iid ++ " footprint must be finite and positive"))
    unless (finite b && b >= 0) (Left (iid ++ " brim must be a finite, non-negative width"))
    let (cw, cd) = cell (w, d) b
        fits (a, c) = m + a <= pw - m + eps && m + c <= pd - m + eps
    unless (fits (cw, cd) || fits (cd, cw)) $
      Left (iid ++ " footprint " ++ show w ++ "x" ++ show d ++ (if b > 0 then " with a " ++ show b ++ " mm brim" else "")
        ++ " exceeds the " ++ show pw ++ "x" ++ show pd ++ " plate with " ++ show m ++ " margin, even turned 90 degrees")) items0
  Right (go (sortBy bigger items0))
  where
    eps = 1e-9
    cell (w, d) b = (w + 2 * b, d + 2 * b)
    bigger (_, _, f1, b1) (_, _, f2, b2) =
      let (w1, d1) = cell f1 b1
          (w2, d2) = cell f2 b2
       in compare (w2 * d2) (w1 * d1)
    -- every item fits an empty plate (checked above), so each plate
    -- takes at least one and this terminates
    go [] = []
    go xs = let (placed, rest) = fillPlate xs in placed : go rest
    -- shelves are (y, depth, next free x), bottom first
    fillPlate = loop [] [] []
      where
        loop _ acc deferred [] = (reverse acc, reverse deferred)
        loop shelves acc deferred (item : rest) = case place shelves item of
          Just (shelves', p) -> loop shelves' (p : acc) deferred rest
          Nothing -> loop shelves acc (item : deferred) rest
    place shelves (iid, k, (w, d), b) =
      firstJust (map try (candidates shelves))
      where
        turnable = abs (w - d) > eps
        try (Left i, turned)
          | turned && not turnable = Nothing
          | otherwise =
              let (y, depth, x) = shelves !! i
                  isOpen = i == length shelves - 1
                  (cw, cd) = orient turned
               in if x + cw <= pw - m + eps && (if isOpen then y + cd <= pd - m + eps else cd <= depth + eps)
                    then Just (update i (y, max depth cd, x + cw + m), mk x y turned)
                    else Nothing
        try (Right (), turned)
          | turned && not turnable = Nothing
          | otherwise =
              let y = case reverse shelves of
                    (ly, ld, _) : _ -> ly + ld + m
                    [] -> m
                  (cw, cd) = orient turned
               in if m + cw <= pw - m + eps && y + cd <= pd - m + eps
                    then Just (shelves ++ [(y, cd, m + cw + m)], mk m y turned)
                    else Nothing
        orient turned = let (cw, cd) = cell (w, d) b in if turned then (cd, cw) else (cw, cd)
        mk x y turned =
          Placement iid k (if turned then (d, w) else (w, d)) (x + b, y + b) b (if turned then 90 else 0)
        update i sh = [if j == i then sh else s0 | (j, s0) <- zip [0 ..] shelves]
    -- preference order documented above
    candidates shelves =
      let n = length shelves
          open = [n - 1 | n > 0]
          earlier = [0 .. n - 2]
       in [(Left i, False) | i <- open]
            ++ [(Left i, False) | i <- earlier]
            ++ [(Left i, True) | i <- open ++ earlier]
            ++ [(Right (), False), (Right (), True)]
    firstJust xs = case [x | Just x <- xs] of
      x : _ -> Just x
      [] -> Nothing

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

-- | One design-stage print variant: a part in its declared orientation,
-- moved so its bounding box starts at the origin.
data DesignVariant = DesignVariant
  { dvKey :: VKey
  , dvPart :: FlatPart
  , dvShape :: Shape -- ^ oriented and normalised to the origin
  , dvDims :: (Double, Double, Double) -- ^ bounding-box size (w, d, h)
  , dvName :: String -- ^ file-safe id from 'variantNames'
  }

manifestJson ::
  FilePath ->
  String ->
  (Double, Double, Double) ->
  Double ->
  [DesignVariant] ->
  [[Placement]] ->
  [FlatPart] ->
  String
manifestJson src base (pw, pd, m) maxZ variants plates asmOnly =
  encodePretty $
    jobj
      [ ("schema", JString designSchema)
      , ("source", JString src)
      , ("plate", jobj [("w", jnum3 pw), ("d", jnum3 pd), ("margin", jnum3 m), ("max_z", jnum3 maxZ), ("count", JNumber (fromIntegral (length plates)))])
      , ("variants", jlist (map vj variants))
      , ("placements", jlist [pj i p | (i, ps) <- zip [(1 :: Int) ..] plates, p <- ps])
      , ("assembly_only", jlist (map aj asmOnly))
      ]
  where
    placed = concat plates
    vj (DesignVariant k@(srcf, dk) fp _ (w, d, h) vname) =
      jobj
        [ ("id", JString vname)
        , ("part", JString (fpName fp))
        , ("source", JString srcf)
        , ("down", JString dk)
        , ("count", JNumber (fromIntegral (length [() | p <- placed, plKey p == k])))
        , ("footprint", jlist (map jnum3 [w, d, h]))
        , ("scad", JString (base ++ "_part_" ++ vname ++ ".scad"))
        , ("hints", jobj [(hk, JString hv) | (hk, hv) <- fpHints fp])
        ]
    pj i (Placement iid k (w, d) (x, y) b r) =
      jobj
        ( [ ("instance", JString iid)
          , ("variant", JString (vnameOfK k))
          , ("plate", JNumber (fromIntegral i))
          , ("x", jnum3 x), ("y", jnum3 y), ("w", jnum3 w), ("d", jnum3 d)
          ]
            ++ [("brim_mm", jnum3 b) | b > 0]
            ++ [("rot_z", JNumber (fromIntegral r)) | r /= 0]
        )
    vnameOfK k = head [dvName v | v <- variants, dvKey v == k]
    aj fp = jobj [("part", JString (fpName fp)), ("source", JString (fpSource fp))]
processAssemble :: FilePath -> IO ()
processAssemble inputFile = do
  r <- loadAssembly [] inputFile
  case r of
    Left err -> failAsm err
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
             in DesignVariant {dvKey = k, dvPart = fp, dvShape = norm, dvDims = dims, dvName = vname}
          variants = map mkVariant vkeys
          maxZ = arPlateMaxZ ar
      -- The design stage measures the Shape's bounding box, an
      -- approximation of the mesh `coscad next` checks (and refuses).
      mapM_ (\(DesignVariant _ fp _ (_, _, h) vname) ->
        when (h > maxZ + 1e-9) $
          hPutStrLn stderr ("Warning: " ++ vname ++ ": about " ++ showD (roundTo3 h) ++ " mm tall " ++ orientNote fp
            ++ ", over the plate's max height of " ++ showD maxZ ++ " mm; `coscad next` will refuse it unless it is turned"
            ++ " (declare another ▽face) or the plate's 4th argument is raised")) variants
      mapM_
        ( \v -> do
            let f = base ++ "_part_" ++ dvName v ++ ".scad"
            writeScad (dvShape v) f
            putStrLn ("Wrote " ++ f)
        )
        variants
      let footOf k = head [(w, d) | v <- variants, dvKey v == k, let (w, d, _) = dvDims v]
          normOf k = head [dvShape v | v <- variants, dvKey v == k]
          instances = [(instId fp i, vkey fp, maybe 0 id (brimHint fp)) | fp <- printable, i <- [1 .. fpCount fp]]
      -- only explicit brim=N hints here: plate brim=N follows the mesh
      -- printability that `coscad next` measures
      case packBedsBrim plate [(iid, k, footOf k, b) | (iid, k, b) <- instances] of
        Left err -> do
          -- a part bigger than the plate: the input is fine, the design is not
          hPutStrLn stderr ("Error: " ++ inputFile ++ ": " ++ err)
          exitWith (ExitFailure 2)
        Right plates -> do
          let nPlates = length plates
              plateFile i = base ++ (if nPlates == 1 then "_plate" else "_plate" ++ show i) ++ ".scad"
          mapM_
            ( \(i, placed) -> do
                let slab = Translate (0, 0, -0.8) (Rectangle pw pd 0.6)
                    plateShape = Union (slab : [Translate (x, y, 0) (turned p (normOf (plKey p))) | p <- placed, let (x, y) = plAt p])
                    -- the same turn as 'turnZ': +90 about Z, then the original depth along +X
                    turned p s = if plRotZ p == 0 then s else Translate (fst (plSize p), 0, 0) (RotAxis 90 (0, 0, 1) s)
                writeScad plateShape (plateFile i)
                putStrLn ("Wrote " ++ plateFile i ++ " (" ++ show (length placed) ++ " parts)")
            )
            (zip [(1 :: Int) ..] plates)
          when (nPlates > 1) $
            putStrLn ("(" ++ show (length instances) ++ " parts need " ++ show nPlates ++ " plates of " ++ showD pw ++ "x" ++ showD pd ++ ")")
          writeFileUtf8 (designManifestPath base) (manifestJson inputFile baseName plate (arPlateMaxZ ar) variants plates asmOnly)
          putStrLn ("Wrote " ++ designManifestPath base)
          noteLegacyManifest base

-- | How a variant stands on the bed, for messages: its declared face, or
-- as modelled.
orientNote :: FlatPart -> String
orientNote fp = maybe "as modelled" (\(_, l) -> "standing on ▽" ++ l) (fpDown fp)

roundTo3 :: Double -> Double
roundTo3 x = fromIntegral (round (x * 1000) :: Integer) / 1000

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
