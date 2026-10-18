-- | CoScadNext, the manufacturing stage: per-variant mesh
-- compilation, FFF orientation search, multi-bed packing, and
-- per-bed STL + manifest emission for the slicer.
module Coscad.Next
  ( FffTerms (..)
  , fffTerms
  , instanceBrim
  , processNext
  , processNextWith
  )
where

import Coscad.Assemble
import Coscad.Codegen
import Coscad.Geometry
import Coscad.IO (writeFileUtf8)
import Coscad.Json
import Coscad.Mesh
import Coscad.OpenScad (renderStlFile)
import Coscad.Printability
import Coscad.Variants
import Control.Monad (when)
import Data.List (intercalate)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (dropExtension)
import System.IO (hPutStrLn, stderr)

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

faceCandidates :: [((Double, Double, Double), String)]
faceCandidates =
  [ ((0, 0, -1), "bot")
  , ((0, 0, 1), "top")
  , ((1, 0, 0), "rt")
  , ((-1, 0, 0), "lft")
  , ((0, -1, 0), "fwd")
  , ((0, 1, 0), "bak")
  ]

-- | The terms of the FFF orientation score for a mesh already rotated
-- into a candidate orientation (areas in mm², height in mm).
data FffTerms = FffTerms
  { fffContact :: Double -- ^ 'bedContact', the same number the manifest reports
  , fffOverhang :: Double -- ^ down faces starting above the first-layer band
  , fffTotal :: Double -- ^ total surface area
  , fffHeight :: Double
  , fffMaxDim :: Double
  , fffSlender :: Double -- ^ height over the narrower footprint side
  }
  deriving (Show)

fffTerms :: [Tri] -> FffTerms
fffTerms tris =
  let ((x0, y0, z0), (x1, y1, z1)) = meshBounds tris
      h = z1 - z0
      -- only near-horizontal downward faces need support; curved
      -- flanks of lying cylinders (45-65 deg) self-support in FFF
      overhang = sum (map triArea (overhangFaces tris))
   in FffTerms
        { fffContact = bedContact (planeTolerance tris) tris
        , fffOverhang = overhang
        , fffTotal = max 1e-9 (sum (map triArea tris))
        , fffHeight = h
        , fffMaxDim = maximum [x1 - x0, y1 - y0, h, 1]
        , fffSlender = h / max 1 (min (x1 - x0) (y1 - y0))
        }

-- | FFF orientation score for a mesh already rotated into a
-- candidate orientation. Higher is better; Nothing = exceeds bed Z.
fffScore :: Double -> [Tri] -> Maybe Double
fffScore maxZ tris =
  let t = fffTerms tris
      (contact, overhang, total) = (fffContact t, fffOverhang t, fffTotal t)
      (h, maxdim, slender) = (fffHeight t, fffMaxDim t, fffSlender t)
      -- stability: punish tall skinny prints (wobble, layer-shear)
      wobble = 0.1 * max 0 (slender - 2)
   in if h > maxZ
        then Nothing
        else Just (contact / total - 3 * overhang / total - 0.3 * h / maxdim - wobble)

-- | One print variant after orientation: its mesh at the origin, chosen
-- face and score, and its printability analysed once.
data Variant = Variant
  { vKey :: VKey
  , vPart :: FlatPart
  , vMesh :: [Tri]
  , vDims :: (Double, Double, Double)
  , vName :: String
  , vDown :: String
  , vMode :: String
  , vScore :: Double
  , vPrint :: Printability
  }

-- | Height of a mesh standing on each of the six axis faces.
faceHeights :: [Tri] -> [(String, Double)]
faceHeights tris =
  [ (lbl, z1 - z0)
    | (v, lbl) <- faceCandidates
    , let ((_, _, z0), (_, _, z1)) = meshBounds (meshMap (mApply (downMat v)) tris)
  ]

-- | Why a part cannot stand within the plate's max height, with the
-- faces it would fit on (if any) so the fix is obvious.
tooTall :: Double -> String -> Double -> [Tri] -> String
tooTall maxZ what h tris =
  let fitting = [l | (l, fh) <- faceHeights tris, fh <= maxZ]
   in what ++ " is " ++ showD (roundTo3 h) ++ " mm tall, over the plate's max height of " ++ showD maxZ ++ " mm"
        ++ case fitting of
          [] -> " on every face (lowest " ++ showD (roundTo3 (minimum (map snd (faceHeights tris)))) ++ " mm); split the part or print it on a taller printer"
          _ -> "; it fits standing on ▽" ++ intercalate ", ▽" fitting
        ++ " (raise the limit with `plate W D margin maxZ`)"

-- | How a variant stands on the bed.
data Orientation = Orientation
  { oMat :: M3 -- ^ rotation applied to the compiled mesh
  , oDown :: String -- ^ the face on the bed (▽ label)
  , oMode :: String -- ^ "declared" or "auto"
  , oScore :: Double -- ^ 'fffScore' in this orientation
  }
  deriving (Show)

-- | Choose orientation: declared ▽ wins; otherwise best FFF score. A part
-- taller than @maxZ@ in its declared orientation, or on every face, is
-- refused with the heights and the faces that would fit.
chooseOrientation :: Double -> Maybe ((Double, Double, Double), String) -> [Tri] -> Either String Orientation
chooseOrientation maxZ (Just (v, lbl)) tris =
  let m = downMat v
      rot = meshMap (mApply m) tris
      ((_, _, z0), (_, _, z1)) = meshBounds rot
   in case fffScore maxZ rot of
        Nothing -> Left (tooTall maxZ ("standing on its declared ▽" ++ lbl ++ " it") (z1 - z0) tris)
        Just sc -> Right (Orientation m lbl "declared" sc)
chooseOrientation maxZ Nothing tris =
  let scored =
        [ (sc, m, lbl)
          | (v, lbl) <- faceCandidates
          , let m = downMat v
          , Just sc <- [fffScore maxZ (meshMap (mApply m) tris)]
        ]
   in case scored of
        [] -> Left (tooTall maxZ "it" (minimum (map snd (faceHeights tris))) tris)
        _ ->
          let (sc, m, lbl) = foldl1 (\a@(s1, _, _) b@(s2, _, _) -> if s2 > s1 then b else a) scored
           in Right (Orientation m lbl "auto" sc)

processNext :: FilePath -> IO ()
processNext = processNextWith False

-- | @coscad next [--strict]@. With @strict@, any printability warning
-- makes the stage exit 2 (the design has a problem) after the beds and
-- manifest are written, so they can still be inspected.
processNextWith :: Bool -> FilePath -> IO ()
processNextWith strict inputFile = do
  r <- loadAssembly [] inputFile
  case r of
    Left err -> failAsm err
    Right ar -> do
      let base = dropExtension inputFile
          (pw, pd, marg) = arPlate ar
          bedZ = arPlateMaxZ ar
          printable = [fp | fp <- arFlat ar, fpCount fp > 0]
          asmOnly = [fp | fp <- arFlat ar, fpCount fp <= 0]
      when (null printable) $ do
        hPutStrLn stderr ("Error: " ++ inputFile ++ ": nothing to print: every part is ×0 (assembly-only) or no part is declared")
        exitFailure
      -- compile each variant to a mesh once
      variantsE <- mapM
        ( \(k, vname, fp) -> do
            let scadF = base ++ "_next_" ++ vname ++ ".scad"
                stlF = base ++ "_next_" ++ vname ++ ".stl"
            writeScad (resolve (fpShape fp)) scadF
            ro <- renderStlFile scadF stlF
            case ro of
              Left err -> return (Left (1 :: Int, err))
              Right mesh -> do
                case chooseOrientation bedZ (fpDown fp) mesh of
                  -- too tall for the printer: a design problem (exit 2)
                  Left err -> return (Left (2, inputFile ++ ": " ++ vname ++ ": " ++ err))
                  Right o -> do
                    let rot = meshMap (mApply (oMat o)) mesh
                        (lo, hi) = meshBounds rot
                        norm = meshMap (\v -> vsub v lo) rot
                        dims = vsub hi lo
                    return
                      ( Right
                          Variant
                            { vKey = k
                            , vPart = fp
                            , vMesh = norm
                            , vDims = dims
                            , vName = vname
                            , vDown = oDown o
                            , vMode = oMode o
                            , vScore = oScore o
                            , vPrint = analysePrintability norm
                            }
                      )
        )
        (printVariants printable)
      case sequence variantsE of
        Left (code, err) -> hPutStrLn stderr ("Error: " ++ err) >> exitWith (ExitFailure code)
        Right variants -> do
          mapM_ (\v ->
            mapM_ (hPutStrLn stderr . (("Warning: " ++ vName v ++ ": ") ++))
              (printabilityWarnings (vPrint v))) variants
          let byKey = variantMap vKey variants
              footprint v = let (w, d, _) = vDims v in (w, d)
          items <- orInternal (mapM (\(iid, k, fp) -> (\v -> (iid, k, footprint v, instanceBrim (arPlateBrim ar) fp (vPrint v))) <$> lookupVariant byKey k) (printInstances printable))
          case packBedsBrim (pw, pd, marg) items of
            -- a footprint bigger than the plate: a design problem (exit 2)
            Left err -> hPutStrLn stderr ("Error: " ++ inputFile ++ ": " ++ err) >> exitWith (ExitFailure 2)
            Right packed -> do
              beds <- orInternal (placedVariants byKey packed)
              mapM_
                ( \(i, placed) -> do
                    let tris = concat [meshMap (vadd (x, y, 0) . turn p) (vMesh v) | (p, v) <- placed, let (x, y) = plAt p]
                        turn p = if plRotZ p == 0 then id else turnZ (fst (plSize p))
                        f = base ++ "_bed" ++ show i ++ ".stl"
                    writeFileUtf8 f (stlAscii ("bed" ++ show i) tris)
                    putStrLn ("Wrote " ++ f ++ " (" ++ show (length placed) ++ " parts)")
                )
                (zip [(1 :: Int) ..] beds)
              writeFileUtf8 (nextManifestPath base) (nextManifest inputFile base (pw, pd, marg) (arPlateBrim ar) bedZ variants beds asmOnly)
              putStrLn ("Wrote " ++ nextManifestPath base)
              let warned = [v | v <- variants, not (null (printabilityWarnings (vPrint v)))]
                  count = sum (map (length . printabilityWarnings . vPrint) warned)
              when (strict && not (null warned)) $ do
                hPutStrLn stderr ("Error: " ++ inputFile ++ ": --strict: " ++ show count ++ " printability warning"
                  ++ (if count == 1 then "" else "s") ++ " on " ++ intercalate ", " (map vName warned)
                  ++ "; fix or reorient the parts, or slice with scripts/bambu-slice.py --accept-printability after review")
                exitWith (ExitFailure 2)

-- | Brim reserved around one instance: its own @brim=N@ hint wins
-- (@brim=0@ opts out); otherwise @plate … brim=N@ applies to parts whose
-- printability recommends a brim (small first layer, tall and narrow).
instanceBrim :: Maybe Double -> FlatPart -> Printability -> Double
instanceBrim plateBrim fp p = case brimHint fp of
  Just b -> b
  Nothing -> case plateBrim of
    Just b | recommendsBrim p -> b
    _ -> 0

nextManifest ::
  FilePath ->
  String ->
  (Double, Double, Double) ->
  Maybe Double ->
  Double ->
  [Variant] ->
  [[(Placement, Variant)]] ->
  [FlatPart] ->
  String
nextManifest src base (pw, pd, m) plateBrim bedZ variants beds asmOnly =
  encodePretty $
    jobj
      [ ("schema", JString nextSchema)
      , ("source", JString src)
      , ("bed", jobj ([("w", jnum3 pw), ("d", jnum3 pd), ("margin", jnum3 m), ("max_z", jnum3 bedZ)]
                       ++ [("brim_mm", jnum3 b) | Just b <- [plateBrim]]))
      , ("variants", jlist (map vj variants))
      , ("beds", jlist (map bj (zip [(1 :: Int) ..] beds)))
      , ("assembly_only", jlist (map aj asmOnly))
      ]
  where
    vj v =
      let fp = vPart v
          p = vPrint v
          vname = vName v
          (w, d, h) = vDims v
       in jobj
        [ ("id", JString vname)
        , ("part", JString (fpName fp))
        , ("source", JString (fst (vKey v)))
        , ("down", JString (vDown v))
        , ("orientation_mode", JString (vMode v))
        , ("fff_score", jnum3 (vScore v))
        , ("printability", jobj
            [("bed_contact_mm2", jnum3 (bedContactMm2 p))
            ,("elevated_start_heights_mm", jlist (map jnum3 (elevatedStartsMm p)))
            ,("overhang_mm2", jnum3 (overhangMm2 p))
            ,("first_layer_mm2", jnum3 (firstLayerMm2 p))
            ,("min_width_mm", jnum3 (minWidthMm p))
            ,("warnings", jlist (map JString (printabilityWarnings p)))
            ,("warning_codes", jlist (map JString (printabilityCodes p)))])
        , ("footprint", jlist (map jnum3 [w, d, h]))
        , ("mesh", JString (base ++ "_next_" ++ vname ++ ".stl"))
        , ("hints", jobj [(hk, JString hv) | (hk, hv) <- fpHints fp])
        ]
    bj (i, placed) =
      jobj
        [ ("index", JNumber (fromIntegral i))
        , ("stl", JString (base ++ "_bed" ++ show i ++ ".stl"))
        , ("placements", jlist (map pj placed))
        ]
    pj (Placement iid _ (w, d) (x, y) b r, v) =
      jobj [("instance", JString iid), ("variant", JString (vName v)), ("x", jnum3 x), ("y", jnum3 y), ("w", jnum3 w), ("d", jnum3 d), ("brim_mm", jnum3 b), ("rot_z", JNumber (fromIntegral r))]
    aj fp = jobj [("part", JString (fpName fp)), ("source", JString (fpSource fp))]
