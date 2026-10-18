-- | Print variants: the parts of an assembly as the design stage and
-- @coscad next@ print them. A variant is one source in one orientation;
-- this module names variants and instances, orients a part for the bed
-- and packs instances onto plates. Both stages build their own variant
-- record (a Shape for the design stage, a mesh for @next@) and look it up
-- here by 'VKey'.
module Coscad.Variants
  ( -- * Parts
    FlatPart (..),
    brimHint,
    orientForBed,

    -- * Variants and instances
    VKey,
    vkey,
    sanitize,
    printVariants,
    variantNames,
    instId,
    printInstances,
    variantMap,
    lookupVariant,
    placedVariants,
    orInternal,

    -- * Packing
    Placement (..),
    turnZ,
    packBeds,
    packBedsBrim,
    packStrategies,
    packShelfLegacy,
    TurnRule (..),
    packFirstFit,
  )
where

import Control.Monad (unless)
import Coscad.Geometry (bbox, bcenter)
import Coscad.Linalg (downRot, vneg)
import Coscad.Shape
import Coscad.Validate (finite, readFinite)
import Data.Char (isAlphaNum)
import Data.List (foldl', intercalate, sortBy)
import qualified Data.Map as Map
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

-- | One part of a flattened assembly: where it sits in the assembly
-- (its trail of subassembly aliases), its print count and orientation,
-- hints, and shape.
data FlatPart = FlatPart
  { fpTrail :: [String]
  , fpName :: String
  , fpSource :: FilePath
  , fpCount :: Int
  , fpDown :: Maybe ((Double, Double, Double), String)
  , fpHints :: [(String, String)]
  , fpShape :: Shape
  }


-- | The brim an instance asks for with its own @brim=N@ hint. The
-- outermost reference wins, as hints from an enclosing @.assemble@ come
-- after the subassembly's own.
brimHint :: FlatPart -> Maybe Double
brimHint fp = case [v | ("brim", v) <- reverse (fpHints fp)] of
  v : _ -> readFinite v
  [] -> Nothing

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
-- in first-seen order, with the first part of that variant. Both stages
-- name their per-variant artefacts and manifest ids with these. The
-- historical `part_down` name is kept when it is unique. Colliding names
-- (the same part name from two sources, e.g. a `base` in two
-- subassemblies, or names equal after sanitizing) are qualified by the
-- assembly trail, then numbered; the result is unique.
printVariants :: [FlatPart] -> [(VKey, String, FlatPart)]
printVariants printable = zip3 (map vkey firsts) (unique (zipWith pick plain qualified)) firsts
  where
    firsts = reverse (foldl' (\acc fp -> if any ((== vkey fp) . vkey) acc then acc else fp : acc) [] printable)
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

-- | 'printVariants' without the parts: (key, name) in first-seen order.
variantNames :: [FlatPart] -> [(VKey, String)]
variantNames printable = [(k, n) | (k, n, _) <- printVariants printable]

-- | Every printable instance, counts expanded: its id, variant and part.
printInstances :: [FlatPart] -> [(String, VKey, FlatPart)]
printInstances printable = [(instId fp i, vkey fp, fp) | fp <- printable, i <- [1 .. fpCount fp]]

-- | Index variants by key.
variantMap :: (v -> VKey) -> [v] -> Map.Map VKey v
variantMap key vs = Map.fromList [(key v, v) | v <- vs]

-- | The variant a key names. Every key of a printable part has a variant,
-- so a miss is an internal error, reported rather than crashing.
lookupVariant :: Map.Map VKey v -> VKey -> Either String v
lookupVariant m k = maybe (Left ("internal error: no print variant for " ++ fst k ++ " ▽" ++ snd k)) Right (Map.lookup k m)

-- | Every placement on every bed with its variant.
placedVariants :: Map.Map VKey v -> [[Placement]] -> Either String [[(Placement, v)]]
placedVariants m = mapM (mapM (\p -> (,) p <$> lookupVariant m (plKey p)))

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
-- Deterministic portfolio: every strategy in 'packStrategies' packs the
-- same items, and the layout with the fewest plates wins; ties go to the
-- earlier strategy. The first is the 1.2 next-fit shelf packer, with
-- the brim reservation ('packShelfLegacy'), so a layout it could already
-- finish on as few plates is kept exactly, and the packer never needs more plates than
-- it did. The first-fit decreasing strategies ('packFirstFit') fill gaps
-- and turn parts 90° about Z, and win only when they save a plate.
packBedsBrim ::
  (Double, Double, Double) ->
  [(String, VKey, (Double, Double), Double)] ->
  Either String [[Placement]]
packBedsBrim plate items0 = do
  validatePack plate items0
  case [r | (_, s) <- packStrategies, Just r <- [s plate items0]] of
    [] -> Left "internal error: no packing strategy placed every item"
    r : rs -> Right (foldl' (\best c -> if length c < length best then c else best) r rs)

-- | The packing strategies 'packBedsBrim' compares, in tie-break order,
-- with a short name for tests and diagnostics. A strategy returns
-- 'Nothing' when it cannot place every item (the legacy packer never
-- turns, so a part that fits only turned is beyond it).
packStrategies ::
  [ ( String
    , (Double, Double, Double) -> [(String, VKey, (Double, Double), Double)] -> Maybe [[Placement]]
    )
  ]
packStrategies =
  [ ("shelf", packShelfLegacy)
  , ("first-fit", \p xs -> packFirstFit TurnAnyShelf p (sortBy byArea xs))
  , ("first-fit, no deepening turns", \p xs -> packFirstFit TurnWithinShelf p (sortBy byArea xs))
  , ("first-fit by longest side", \p xs -> packFirstFit TurnWithinShelf p (sortBy byLongest xs))
  ]
  where
    -- stable sorts: ties stay in declaration order
    byArea (_, _, f1, b1) (_, _, f2, b2) =
      let (w1, d1) = brimCell f1 b1
          (w2, d2) = brimCell f2 b2
       in compare (w2 * d2) (w1 * d1)
    byLongest (_, _, f1, b1) (_, _, f2, b2) =
      let (w1, d1) = brimCell f1 b1
          (w2, d2) = brimCell f2 b2
       in compare (max w2 d2, w2 * d2) (max w1 d1, w1 * d1)

-- | The checks every strategy relies on: a usable plate, finite positive
-- footprints and brims, and each reserved cell fitting an empty plate
-- one way round or the other.
validatePack :: (Double, Double, Double) -> [(String, VKey, (Double, Double), Double)] -> Either String ()
validatePack (pw, pd, m) items0 = do
  unless (all finite [pw, pd, m] && pw > 0 && pd > 0 && m >= 0 && m < pw / 2 && m < pd / 2) $
    Left "plate dimensions must be finite and positive; margin must be non-negative and leave usable space"
  mapM_ (\(iid, _, (w, d), b) -> do
    unless (finite w && finite d && w > 0 && d > 0) (Left (iid ++ " footprint must be finite and positive"))
    unless (finite b && b >= 0) (Left (iid ++ " brim must be a finite, non-negative width"))
    let (cw, cd) = brimCell (w, d) b
        fits (a, c) = m + a <= pw - m + packEps && m + c <= pd - m + packEps
    unless (fits (cw, cd) || fits (cd, cw)) $
      Left (iid ++ " footprint " ++ show w ++ "x" ++ show d ++ (if b > 0 then " with a " ++ show b ++ " mm brim" else "")
        ++ " exceeds the " ++ show pw ++ "x" ++ show pd ++ " plate with " ++ show m ++ " margin, even turned 90 degrees")) items0

packEps :: Double
packEps = 1e-9

-- | A footprint grown by its brim on every side.
brimCell :: (Double, Double) -> Double -> (Double, Double)
brimCell (w, d) b = (w + 2 * b, d + 2 * b)

-- | The coscad 1.2 packer, kept bit for bit: largest reserved area first
-- (ties in input order), next-fit over shelves, never turned. A part
-- that doesn't fit on the open shelf starts a new one; a shelf that
-- doesn't fit closes the plate, and everything left goes to the next.
-- 'Nothing' when an item fits an empty plate only turned.
packShelfLegacy ::
  (Double, Double, Double) ->
  [(String, VKey, (Double, Double), Double)] ->
  Maybe [[Placement]]
packShelfLegacy (pw, pd, m) items0 = go (sortBy bigger items0)
  where
    bigger (_, _, f1, b1) (_, _, f2, b2) =
      let (w1, d1) = brimCell f1 b1
          (w2, d2) = brimCell f2 b2
       in compare (w2 * d2) (w1 * d1)
    go [] = Just []
    go xs = do
      (placed, rest) <- fit m m 0 [] xs
      (placed :) <$> go rest
    fit _ _ _ acc [] = Just (reverse acc, [])
    fit x y rowD acc (item@(iid, k, (w, d), b) : rest)
      | cw > pw - 2 * m || cd > pd - 2 * m = Nothing
      | x + cw > pw - m = fit m (y + rowD + m) 0 acc (item : rest)
      | y + cd > pd - m = if null acc then Nothing else Just (reverse acc, item : rest)
      | otherwise = fit (x + cw + m) y (max rowD cd) (Placement iid k (w, d) (x + b, y + b) b 0 : acc) rest
      where
        (cw, cd) = brimCell (w, d) b

-- | Where 'packFirstFit' may turn a part about Z before opening a shelf.
data TurnRule
  = -- | onto any shelf, the open one growing as deep as the turned part
    -- (the original first-fit rule: one long part turned onto the open
    -- shelf deepens it for good)
    TurnAnyShelf
  | -- | onto a shelf only within its current depth; turning onto the open
    -- shelf and deepening it comes after opening a new shelf as declared
    TurnWithinShelf
  deriving (Eq, Show)

-- | First-fit decreasing over shelves, in the order given. A plate stays
-- open until every remaining item has been tried; an item that fits
-- nowhere waits for the next plate. Each item takes the first of
--
--   1. the open (last) shelf, as declared;
--   2. a gap at the end of an earlier shelf, as declared;
--   3. any shelf, turned 90° about Z ('TurnWithinShelf': without
--      deepening it);
--   4. a new shelf, as declared;
--   5. ('TurnWithinShelf' only) the open shelf turned, deepening it;
--   6. a new shelf, turned.
--
-- Square cells never turn. 'Nothing' when an item fits no empty plate
-- either way round ('validatePack' rules that out first).
packFirstFit ::
  TurnRule ->
  (Double, Double, Double) ->
  [(String, VKey, (Double, Double), Double)] ->
  Maybe [[Placement]]
packFirstFit rule (pw, pd, m) = go
  where
    eps = packEps
    go [] = Just []
    go xs = case fillPlate xs of
      ([], _) -> Nothing -- an item that fits no empty plate
      (placed, rest) -> (placed :) <$> go rest
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
        try (Left (i, deepen), turned)
          | turned && not turnable = Nothing
          | otherwise =
              let (y, depth, x) = shelves !! i
                  (cw, cd) = orient turned
               in if x + cw <= pw - m + eps && (if deepen then y + cd <= pd - m + eps else cd <= depth + eps)
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
        orient turned = let (cw, cd) = brimCell (w, d) b in if turned then (cd, cw) else (cw, cd)
        mk x y turned =
          Placement iid k (if turned then (d, w) else (w, d)) (x + b, y + b) b (if turned then 90 else 0)
        update i sh = [if j == i then sh else s0 | (j, s0) <- zip [0 ..] shelves]
    -- preference order documented above; (shelf, may deepen it)
    candidates shelves =
      let n = length shelves
          open = [n - 1 | n > 0]
          earlier = [0 .. n - 2]
          declared = [(Left (i, True), False) | i <- open] ++ [(Left (i, False), False) | i <- earlier]
       in case rule of
            TurnAnyShelf ->
              declared
                ++ [(Left (i, True), True) | i <- open] ++ [(Left (i, False), True) | i <- earlier]
                ++ [(Right (), False), (Right (), True)]
            TurnWithinShelf ->
              declared
                ++ [(Left (i, False), True) | i <- open ++ earlier]
                ++ [(Right (), False)]
                ++ [(Left (i, True), True) | i <- open]
                ++ [(Right (), True)]
    firstJust xs = case [x | Just x <- xs] of
      x : _ -> Just x
      [] -> Nothing

-- | Unwrap a result that can only fail through an internal inconsistency
-- (a variant missing for a key): report it and exit 1 instead of crashing.
orInternal :: Either String a -> IO a
orInternal = either (\e -> hPutStrLn stderr ("Error: " ++ e) >> exitWith (ExitFailure 1)) return
