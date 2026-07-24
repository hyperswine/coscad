-- | CoScadNext, the manufacturing stage: per-variant mesh
-- compilation, FFF orientation search, multi-bed packing, and
-- per-bed STL + manifest emission for the slicer.
module Coscad.Next (module Coscad.Next) where

import Coscad.Assemble
import Coscad.Codegen
import Coscad.Geometry
import Coscad.Shape
import Data.List (foldl', intercalate, sortBy)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (dropExtension)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

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

