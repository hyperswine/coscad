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

-- | Unwrap a result that can only fail through an internal inconsistency
-- (a variant missing for a key): report it and exit 1 instead of crashing.
orInternal :: Either String a -> IO a
orInternal = either (\e -> hPutStrLn stderr ("Error: " ++ e) >> exitWith (ExitFailure 1)) return
