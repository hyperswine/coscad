-- | CoScadNext, the manufacturing stage: per-variant mesh
-- compilation, FFF orientation search, multi-bed packing, and
-- per-bed STL + manifest emission for the slicer.
module Coscad.Next (module Coscad.Next) where

import Coscad.Assemble
import Coscad.Codegen
import Coscad.Geometry
import Coscad.IO (writeFileUtf8)
import Coscad.Json
import Coscad.Mesh
import Coscad.OpenScad (renderStlFile)
import Coscad.Printability
import Control.Monad (when)
import System.Exit (exitFailure)
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

-- | A down face that starts within this band above the bed is squashed
-- onto it by the first layer rather than needing support, so it is not
-- an overhang. It is deliberately not the contact tolerance: a face 0.2 mm
-- up does not hold the part down.
firstLayerBand :: Double
firstLayerBand = 0.3

fffTerms :: [Tri] -> FffTerms
fffTerms tris =
  let ((x0, y0, z0), (x1, y1, z1)) = meshBounds tris
      h = z1 - z0
      verts (a, b, c) = [a, b, c]
      -- only near-horizontal downward faces need support; curved
      -- flanks of lying cylinders (45-65 deg) self-support in FFF
      overhang = sum [triArea t | t <- tris, downFacing t, minimum [z | (_, _, z) <- verts t] > z0 + firstLayerBand]
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
  let FffTerms contact overhang total h maxdim slender = fffTerms tris
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

processNext :: FilePath -> IO ()
processNext inputFile = do
  r <- loadAssembly [] inputFile
  case r of
    Left err -> failAsm err
    Right ar -> do
      let base = dropExtension inputFile
          (pw, pd, marg) = arPlate ar
          bedZ = 250 -- default max print height; TODO: plate stmt 4th arg
          printable = [fp | fp <- arFlat ar, fpCount fp > 0]
          asmOnly = [fp | fp <- arFlat ar, fpCount fp <= 0]
          names = variantNames printable
          vkeys = map fst names
      when (null printable) $ do
        hPutStrLn stderr ("Error: " ++ inputFile ++ ": nothing to print: every part is ×0 (assembly-only) or no part is declared")
        exitFailure
      -- compile each variant to a mesh once
      variantsE <- mapM
        ( \k -> do
            let fp = head [f | f <- printable, vkey f == k]
                vname = maybe (sanitize (fpName fp)) id (lookup k names)
                scadF = base ++ "_next_" ++ vname ++ ".scad"
                stlF = base ++ "_next_" ++ vname ++ ".stl"
            writeScad (resolve (fpShape fp)) scadF
            ro <- renderStlFile scadF stlF
            case ro of
              Left err -> return (Left err)
              Right mesh -> do
                case chooseOrientation bedZ (fpDown fp) mesh of
                  Left err -> return (Left (vname ++ ": " ++ err))
                  Right (m, lbl, mode, sc) -> do
                    let rot = meshMap (mApply m) mesh
                        (lo, hi) = meshBounds rot
                        norm = meshMap (\v -> vsub v lo) rot
                        dims = vsub hi lo
                    return (Right (Variant k fp norm dims vname lbl mode sc (analysePrintability norm)))
        )
        vkeys
      case sequence variantsE of
        Left err -> hPutStrLn stderr ("Error: " ++ err) >> exitFailure
        Right variants -> do
          mapM_ (\v ->
            mapM_ (hPutStrLn stderr . (("Warning: " ++ vName v ++ ": ") ++))
              (printabilityWarnings (vPrint v))) variants
          let footOf k = head [(w, d) | v <- variants, vKey v == k, let (w, d, _) = vDims v]
              meshOf k = head [vMesh v | v <- variants, vKey v == k]
              instances = [(instId fp i, vkey fp) | fp <- printable, i <- [1 .. fpCount fp]]
          case packBeds (pw, pd, marg) [(iid, k, footOf k) | (iid, k) <- instances] of
            Left err -> hPutStrLn stderr ("Error: " ++ err) >> exitFailure
            Right beds -> do
              mapM_
                ( \(i, placed) -> do
                    let tris = concat [meshMap (vadd (x, y, 0)) (meshOf k) | (_, k, _, (x, y)) <- placed]
                        f = base ++ "_bed" ++ show i ++ ".stl"
                    writeFileUtf8 f (stlAscii ("bed" ++ show i) tris)
                    putStrLn ("Wrote " ++ f ++ " (" ++ show (length placed) ++ " parts)")
                )
                (zip [(1 :: Int) ..] beds)
              writeFileUtf8 (nextManifestPath base) (nextManifest inputFile base (pw, pd, marg) bedZ variants beds asmOnly)
              putStrLn ("Wrote " ++ nextManifestPath base)

nextManifest ::
  FilePath ->
  String ->
  (Double, Double, Double) ->
  Double ->
  [Variant] ->
  [[(String, VKey, (Double, Double), (Double, Double))]] ->
  [FlatPart] ->
  String
nextManifest src base (pw, pd, m) bedZ variants beds asmOnly =
  encodePretty $
    jobj
      [ ("schema", JString nextSchema)
      , ("source", JString src)
      , ("bed", jobj [("w", jnum3 pw), ("d", jnum3 pd), ("margin", jnum3 m), ("max_z", jnum3 bedZ)])
      , ("variants", jlist (map vj variants))
      , ("beds", jlist (map bj (zip [(1 :: Int) ..] beds)))
      , ("assembly_only", jlist (map aj asmOnly))
      ]
  where
    vj (Variant (srcf, _) fp _ (w, d, h) vname lbl mode sc p) =
      jobj
        [ ("id", JString vname)
        , ("part", JString (fpName fp))
        , ("source", JString srcf)
        , ("down", JString lbl)
        , ("orientation_mode", JString mode)
        , ("fff_score", jnum3 sc)
        , ("printability", jobj
            [("bed_contact_mm2", jnum3 (bedContactMm2 p))
            ,("elevated_start_heights_mm", jlist (map jnum3 (elevatedStartsMm p)))
            ,("warnings", jlist (map JString (printabilityWarnings p)))])
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
    pj (iid, k, (w, d), (x, y)) =
      jobj [("instance", JString iid), ("variant", JString (vn k)), ("x", jnum3 x), ("y", jnum3 y), ("w", jnum3 w), ("d", jnum3 d)]
    vn k = head [vName v | v <- variants, vKey v == k]
    aj fp = jobj [("part", JString (fpName fp)), ("source", JString (fpSource fp))]
