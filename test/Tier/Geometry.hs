-- | Geometry tier (needs OpenSCAD): rendered volume and bounds against
-- test/golden/geometry.txt, and cross-checks.
module Tier.Geometry
  ( mergeGeometryGolden
  , geometry
  , goldenMergeTier
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, when)
import Coscad.Assemble (AsmResult (..), loadAssembleFile, processAssemble, resolveAssembly)
import Coscad.Check (processCheckIn, processCheckWith, splitBodies)
import Coscad.Codegen (gen, renderScad)
import Coscad.Json (asArray, asString, decode, lookupKey)
import Coscad.Mesh (meshBounds, meshVolume, parseStlAscii)
import Coscad.Next (processNext)
import Coscad.OpenScad (bosl2Version, openscadFullRender, openscadPngArgs, openscadVersion, runOpenscadRaw)
import Coscad.Parser (parseProgramNamed)
import Coscad.Part (renderPart)
import MigrationCases (migrationCases)
import Coscad.Shape (Shape (..))
import Data.List (isInfixOf, isPrefixOf, tails)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map as Map
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO
import Text.Printf (printf)
import Harness
import Tier.Examples
import Tier.Manufacturing

-- | Golden table after a run. Without update, only missing entries are
-- added. With update, drifted or missing entries take the current value;
-- entries still within 'geoClose' tolerance keep their recorded value, so
-- re-accepting one deliberate change on a different OpenSCAD build does not
-- rewrite every line with sub-tolerance noise.
mergeGeometryGolden :: Bool -> Map.Map FilePath Geo -> [(FilePath, Geo)] -> Map.Map FilePath Geo
mergeGeometryGolden update golden current = Map.union accepted golden
  where
    accepted = Map.fromList [(f, g) | (f, g) <- current, accept f g]
    accept f g = case Map.lookup f golden of
      Nothing -> True
      Just want -> update && not (geoClose want g)

geometry :: T -> Bool -> FilePath -> IO ()
geometry t update bin = do
  section ("geometry via " ++ bin)
  t0 <- getCurrentTime
  let goldenF = "test/golden/geometry.txt"
  haveG <- doesFileExist goldenF
  golden <- if haveG then Map.fromList . concatMap parseLine . lines <$> readFile goldenF else return Map.empty
  dir <- tempDir "geo"
  fs <- exampleFiles ".coscad"
  rows <- forM fs $ \f -> do
    src <- readFile f
    case parseProgramNamed f src of
      Left e -> failT t ("render " ++ f) e >> return Nothing
      Right (_, m) -> do
        r <- renderMesh bin dir (renderScad m)
        case r of
          Left e -> failT t ("render " ++ f) e >> return Nothing
          Right tris -> do
            let g = geoOf tris
            case Map.lookup f golden of
              Just want | not update ->
                assertT t ("geometry " ++ f) (geoClose want g)
                  ("volume/bounds drifted\n  golden: " ++ showGeo want ++ "\n  now:    " ++ showGeo g ++ "\n(COSCAD_UPDATE_GOLDEN=1 cabal test to accept)")
              Nothing | not update ->
                failT t ("geometry " ++ f) "missing geometry baseline; review the mesh and record it explicitly with COSCAD_UPDATE_GOLDEN=1 cabal test"
              Just want | geoClose want g -> pass t
              _ -> putStrLn ("recorded " ++ f) >> pass t
            return (Just (f, g))
  let current = [x | Just x <- rows]
      table = Map.fromList current
      merged = mergeGeometryGolden update golden current
  when (update && merged /= golden) $
    writeFile goldenF (unlines ["# path volume xmin ymin zmin xmax ymax zmax (regenerate: COSCAD_UPDATE_GOLDEN=1 cabal test)"] ++ unlines [f ++ " " ++ showGeo g | (f, g) <- Map.toList merged])
  -- Check actual bracket material, not the planner's axis-aligned boxes.
  -- Independent world-space probes cover all 16 corners and 32 rail bores.
  cube <- loadAssembleFile [] "examples/assemble/plan/cube.assemble"
  case cube of
    Left e -> failT t "cube bracket geometry" e
    Right ar -> do
      let parts = Map.fromList [(n, if "br_" `isPrefixOf` n then sh else Hidden sh) | (n, sh) <- arParts ar]
      case resolveAssembly ar parts >>= \tab -> maybe (Left "missing asm") Right (Map.lookup "asm" tab) of
        Left e -> failT t "cube bracket geometry" e
        Right brackets -> do
          let corners = [(x,y,z) | x <- [10,190], y <- [10,190], z <- [-3,203]]
                     ++ [(x,y,z) | x <- [10,190], y <- [-3,203], z <- [10,190]]
              inward v = if v == 10 then 30 else 170
              ringHoles = concat [[(inward x,y,z),(x,inward y,z)] | x <- [10,190], y <- [10,190], z <- [-3,203]]
              sideHoles = concat [[(inward x,y,z),(x,y,inward z)] | x <- [10,190], y <- [-3,203], z <- [10,190]]
              probe dims (x,y,z) = "translate(" ++ show ([x,y,z] :: [Int]) ++ ") cube(" ++ show (dims :: [Int]) ++ ",center=true);"
              checkProbes name probes want = do
                let scad = "include <BOSL2/std.scad>\n$fn=50;\nunion(){translate([-1000,-1000,-1000]) cube(1); intersection(){" ++ gen brackets ++ "union(){" ++ concat probes ++ "}}}"
                r <- renderMesh bin dir scad
                case r of
                  Left e -> failT t name e
                  Right tris -> assertT t name (abs (meshVolume tris - want) < 0.01) (show (meshVolume tris, want))
          checkProbes "cube: material at all 16 bracket corners" (map (probe [2,2,2]) corners) 129
          checkProbes "cube: all 32 rail-aligned holes pass through brackets"
            (map (probe [2,2,8]) ringHoles ++ map (probe [2,8,2]) sideHoles) 1
          baseline <- loadAssembleFile [] "test/fixtures/cube-coordinate.assemble"
          case baseline of
            Left e -> failT t "cube coordinate pose baseline" e
            Right ref -> do
              let hidden = Map.fromList [(n, if n == "br" then sh else Hidden sh) | (n,sh) <- arParts ref]
              case resolveAssembly ref hidden >>= maybe (Left "missing baseline asm") Right . Map.lookup "asm" of
                Left e -> failT t "cube bracket pose equivalence" e
                Right original -> do
                  let actual = gen brackets
                      expected = gen original
                      sym = "include <BOSL2/std.scad>\nunion(){translate([-1000,-1000,-1000]) cube(1);difference(){"++actual++expected++"}difference(){"++expected++actual++"}}"
                  r <- renderMesh bin dir sym
                  assertT t "cube: all 16 bracket poses match coordinate reference exactly"
                    (either (const False) (\tris -> abs(meshVolume tris-1) < 1e-7 && length (splitBodies tris) == 1) r) (either id (show . meshVolume) r)
          let panelOnly = Map.fromList [(n,if n == "panel" then sh else Hidden sh) | (n,sh) <- arParts ar]
          case resolveAssembly ar panelOnly >>= maybe (Left "missing panel") Right . Map.lookup "asm" of
            Left e -> failT t "cube panel geometry" e
            Right panel -> do
              let bores = [(60,10),(140,10),(60,190),(140,190),(10,100),(190,100)] :: [(Double,Double)]
                  probes = concat ["translate([201.5,"++show y++","++show z++"]) cube([5,2,2],center=true);" | (y,z) <- bores]
                  scad = "include <BOSL2/std.scad>\nunion(){translate([-1000,-1000,-1000]) cube(1);intersection(){"++gen panel++"union(){"++probes++"}}}"
              r <- renderMesh bin dir scad
              assertT t "cube: all six panel bores match rail slot centres"
                (either (const False) (\tris -> abs(meshVolume tris-1) < 1e-7) r) (either id (show . meshVolume) r)
  -- Compare actual assembled solids, independently of mating metadata.
  -- Oblique rotations leave non-manifold zero-thickness slivers in CGAL
  -- subtraction. Their origins and all three axes are checked above instead;
  -- all individual printable parts still go through strict STL rendering.
  forM_ [c | c@(file,_,_,_) <- migrationCases, not (any (`isInfixOf` file) ["tesseract","orbit-tower"])] $ \(file,ref,_,_) -> do
    a <- loadAssembleFile [] file
    b <- loadAssembleFile [] ref
    case (a >>= maybe (Left "no asm") Right . arAsm, b >>= maybe (Left "no reference asm") Right . arAsm) of
      (Right actual,Right expected) -> do
        let x = gen actual
            y = gen expected
            scad = "include <BOSL2/std.scad>\nunion(){translate([-1000,-1000,-1000])cube(1);difference(){"++x++y++"}difference(){"++y++x++"}}"
            limit = 0.00001
        result <- renderMesh bin dir scad
        assertT t (file ++ ": assembled solid equivalence")
          (either (const False) (\tris -> abs(meshVolume tris-1) <= limit) result) (either id (\tris -> "symmetric difference mm3: "++show(abs(meshVolume tris-1))) result)
      _ -> failT t (file ++ ": geometry comparison") "cannot load migration pair"
  -- cross-check: the topological bracket is the coordinate bracket
  case (Map.lookup "examples/bracket.coscad" table, Map.lookup "examples/topological/tbracket.coscad" table) of
    (Just (v1, lo1, hi1), Just (v2, lo2, hi2)) ->
      let ext (a, b, c) (x, y, z) = (x - a, y - b, z - c)
          sameExt (a, b, c) (x, y, z) = abs (a - x) < 0.01 && abs (b - y) < 0.01 && abs (c - z) < 0.01
       in assertT t "tbracket is the same solid as bracket" (abs (v1 - v2) < 0.5 && sameExt (ext lo1 hi1) (ext lo2 hi2))
            (printf "bracket: vol %.3f ext %s\ntbracket: vol %.3f ext %s" v1 (show (ext lo1 hi1)) v2 (show (ext lo2 hi2)))
    _ -> failT t "tbracket is the same solid as bracket" "one of the two did not render"
  -- cross-check: a circle-to-circle loft is the frustum of the two 100-gons
  case parseProgramNamed "loft.coscad" "main = loft 0 (⭘ 10) 30 (⭘ 5)\n" of
    Left e -> failT t "loft frustum volume" e
    Right (_, m) -> do
      r <- renderMesh bin dir (renderScad m)
      case r of
        Left e -> failT t "loft frustum volume" e
        Right tris ->
          let k = (100 / (2 * pi)) * sin (2 * pi / 100) -- 100-gon area / disc area
              a1 = k * pi * 100
              a2 = k * pi * 25
              want = 30 / 3 * (a1 + a2 + sqrt (a1 * a2))
              got = meshVolume tris
           in assertT t "loft frustum volume" (abs (got - want) < 1e-3 * want) (printf "got %.3f want %.3f" got want)
  adhesionRetryContact t
  printabilityProbes t
  bedHeightProbes t
  -- cross-check: `coscad next` conserves volume across bed packing
  bow <- tempDir "bow3"
  srcs <- listDirectory "examples/assemble/bow3"
  forM_ srcs $ \s -> copyFile ("examples/assemble/bow3" </> s) (bow </> s)
  rn <- try (processNext (bow </> "bow3.assemble")) :: IO (Either SomeException ())
  case rn of
    Left e -> failT t "coscad next bow3" (show e)
    Right () -> do
      bed <- readStl (bow </> "bow3_bed1.stl")
      parts <- mapM (\n -> readStl (bow </> ("bow3_next_" ++ n ++ "_asis.stl"))) ["center", "larch", "rarch"]
      mf <- decode <$> readFile (bow </> "bow3_manifest.json")
      assertT t "coscad next manifest is valid JSON with one bed of three placements"
        (case mf of Right v -> maybe False (\bs -> case bs of [b] -> maybe False ((== 3) . length) (asArray =<< lookupKey "placements" b); _ -> False) (asArray =<< lookupKey "beds" v); Left _ -> False) (either id (const "parsed") mf)
      let vb = meshVolume bed
          vp = sum (map meshVolume parts)
          ((x0, y0, z0), (x1, y1, _)) = meshBounds bed
      assertT t "coscad next: bed volume == sum of variant volumes" (abs (vb - vp) < 1e-6 * vp) (printf "bed %.3f parts %.3f" vb vp)
      -- the bed and margin come from the fixture's `plate` line, via the manifest
      manifest <- readFile (bow </> "bow3_manifest.json")
      assertT t "coscad next manifest carries schema coscad.next/1"
        (either (const False) (\v -> (asString =<< lookupKey "schema" v) == Just "coscad.next/1") mf) (take 200 manifest)
      -- the design stage writes its own file and leaves the next manifest alone
      rd <- try (processAssemble (bow </> "bow3.assemble")) :: IO (Either SomeException ())
      after <- readFile (bow </> "bow3_manifest.json")
      design <- readJson (bow </> "bow3_design.json")
      assertT t "design stage after next: bow3_manifest.json untouched, bow3_design.json has schema coscad.design/1"
        (either (const False) (const True) rd && after == manifest
          && either (const False) (\v -> (asString =<< lookupKey "schema" v) == Just "coscad.design/1" && isJust (lookupKey "plate" v) && isNothing (lookupKey "beds" v)) design)
        (show (either show (const "ok") rd, after == manifest, either id (const "parsed") design))
      let num key = case dropWhile (not . isPrefixOf key) (tails manifest) of
            (m : _) -> read (takeWhile (`elem` "0123456789.") (dropWhile (`elem` "\": ") (drop (length key) m))) :: Double
            [] -> error ("manifest lacks " ++ key)
          (pw, pd, marg) = (num "\"w\"", num "\"d\"", num "\"margin\"")
      assertT t (printf "coscad next: placements inside the %.0fx%.0f bed with %.0fmm margin, on z=0" pw pd marg)
        (x0 >= marg - 1e-6 && y0 >= marg - 1e-6 && x1 <= pw - marg + 1e-6 && y1 <= pd - marg + 1e-6 && abs z0 < 1e-6)
        (show (meshBounds bed, pw, pd, marg))
  collisions <- tempDir "variant-collisions"
  writeVariantCollisions collisions
  forM_ [("top", [1000, 27000]), ("alike", [125, 729])] $ \(asm, volumes) -> do
    rv <- try (processNext (collisions </> asm <.> "assemble")) :: IO (Either SomeException ())
    case rv of
      Left e -> failT t ("coscad next " ++ asm ++ ": distinct variants") (show e)
      Right () -> do
        m <- manifestVariants "mesh" <$> readFile (collisions </> asm ++ "_manifest.json")
        case m of
          Left e -> failT t ("coscad next " ++ asm ++ ": manifest") e
          Right vs@(_, meshes, _) -> do
            assertT t ("coscad next " ++ asm ++ ": distinct variant ids, meshes and placements") (distinctVariants vs) (show vs)
            got <- mapM (fmap meshVolume . readStl) meshes
            assertT t ("coscad next " ++ asm ++ ": each variant mesh is its own part, not overwritten")
              (length got == length volumes && and (zipWith (\g w -> abs (g - w) < 1e-6 * w) got volumes)) (show (meshes, got))
  rc <- try (processCheckWith False (bow </> "bow3.assemble")) :: IO (Either SomeException ())
  assertT t "coscad check bow3: no overlaps" (either (const False) (const True) rc) (either show (const "") rc)
  leftovers <- filter ("chk" `isInfixOf`) <$> listDirectory bow
  assertT t "coscad check leaves no scratch files next to the assembly" (null leftovers) (show leftovers)
  -- Geometry from explicit frames must equal independently placed reference
  -- solids. A remote 1 mm3 marker makes an empty symmetric difference a
  -- successful STL export, without treating OpenSCAD failures as emptiness.
  cornerSrc <- readFile "examples/topological/mating-corner.coscad"
  case parseProgramNamed "mating-corner.coscad" cornerSrc of
    Left e -> failT t "oriented corner symmetric difference" e
    Right (_,s) -> do
      let reference = "union(){cube([20,20,4],center=true);translate([0,0,5]) cube([2,4,6],center=true);translate([13,0,0]) cube([6,2,4],center=true);}"
          actual = gen s
          symmetric = "include <BOSL2/std.scad>\nunion(){translate([-1000,-1000,-1000]) cube(1);difference(){" ++ actual ++ reference ++ "}difference(){" ++ reference ++ actual ++ "}}"
      r <- renderMesh bin dir symmetric
      assertT t "oriented corner symmetric difference is empty"
        (either (const False) (\tris -> abs (meshVolume tris - 1) < 1e-8 && length (splitBodies tris) == 1) r) (either id (show . meshVolume) r)
  framed <- tempDir "constraint-check"
  forM_ ["base.coscad", "post.coscad", "frame-stand.assemble"] $ \f ->
    copyFile ("examples/assemble/constraints" </> f) (framed </> f)
  checkDir <- tempDir "check-frame-stand"
  checked <- try (processCheckIn True (Just checkDir) (framed </> "frame-stand.assemble")) :: IO (Either SomeException ())
  case checked of
    Left e -> failT t "constraint check" (show e)
    Right () -> do
      forM_ [("left_post",(-13,-5,2),(-7,5,22)), ("right_post",(5,-3,2),(15,3,22))] $ \(n,wlo,whi) -> do
        tris <- readStl (checkDir </> ("chk_" ++ n ++ ".stl"))
        let bodies = filter (\body -> let ((x,_,_),_) = meshBounds body in x < 90000) (splitBodies tris)
            (lo,hi) = meshBounds (concat bodies)
            near (a,b,c) (x,y,z) = all (< 1e-6) [abs(a-x),abs(b-y),abs(c-z)]
        assertT t ("constraint check isolates world pose of " ++ n) (length bodies == 1 && near lo wlo && near hi whi) (show (lo,hi))
  writeFile (framed </> "collision.assemble") "base ← base.coscad ×1\nleft_post ← post.coscad ×1\nfixed base\nmate left_post @foot base @left_mount gap=-1\n"
  collision <- try (processCheckWith False (framed </> "collision.assemble")) :: IO (Either ExitCode ())
  assertT t "consistent constraints still expose actual interference (exit 2)" (collision == Left (ExitFailure 2)) (show collision)
  -- Hidden parts must retain their anchors during isolation, including
  -- chained attachments and transformed assemblies. Verify the rendered
  -- isolated child's actual bounds, not the bbox containing hidden parts.
  rel <- tempDir "relational-check"
  writeFile (rel </> "a.coscad") "main = box 20 20 20\n"
  writeFile (rel </> "b.coscad") "main = box 4 4 4\n"
  forM_ [ ("top", "a |> at top b", 10, 14)
        , ("on", "a |> on top b", 10, 14)
        , ("chain", "a |> at top b |> at top b", 10, 18)
        , ("moved", "(a |> at top b) |> z 30", 40, 44)
        , ("anchor", "(a |> at top b) |> anchor bot", 20, 24)
        , ("datum", "(a |> datum mount top) |> add (b |> place (a |> datum mount top) @mount bot)", 10, 14)
        ] $ \(name, expr, zlo, zhi) -> do
    let f = rel </> (name ++ ".assemble")
    writeFile f ("a ← a.coscad ×1\nb ← b.coscad ×1\nasm = " ++ expr ++ "\n")
    relDir <- tempDir ("check-" ++ name)
    result <- try (processCheckIn True (Just relDir) f) :: IO (Either SomeException ())
    case result of
      Left e -> failT t ("relational check " ++ name) (show e)
      Right () -> do
        tris <- readStl (relDir </> "chk_b.stl")
        let bodies = filter (\body -> let ((x, _, _), _) = meshBounds body in x < 90000) (splitBodies tris)
            ((_, _, lo), (_, _, hi)) = meshBounds (concat bodies)
        assertT t ("relational check " ++ name) (abs (lo - zlo) < 1e-6 && abs (hi - zhi) < 1e-6)
          (show (lo, hi))
  let overlapF = rel </> "overlap.assemble"
  writeFile overlapF "a ← a.coscad ×1\nb ← b.coscad ×1\nasm = a |> at top 0 0 -1 b\n"
  overlap <- try (processCheckWith False overlapF) :: IO (Either ExitCode ())
  assertT t "relational check still detects real overlap (exit 2)" (overlap == Left (ExitFailure 2)) (show overlap)
  -- design stage spills across plates instead of failing
  sp <- tempDir "spill"
  copyFile "examples/assemble/spill.assemble" (sp </> "spill.assemble")
  copyFile "examples/assemble/bracket90.coscad" (sp </> "bracket90.coscad")
  ra <- try (processAssemble (sp </> "spill.assemble")) :: IO (Either SomeException ())
  case ra of
    Left e -> failT t "design stage: multi-plate spill" (show e)
    Right () -> do
      p1 <- doesFileExist (sp </> "spill_plate1.scad")
      p2 <- doesFileExist (sp </> "spill_plate2.scad")
      mf <- readFile (sp </> "spill_design.json")
      assertT t "design stage: multi-plate spill" (p1 && p2 && "\"plate\": 2" `isInfixOf` mf && "\"count\": 2" `isInfixOf` mf) (show (p1, p2))
  -- `coscad stl` end to end
  st <- tempDir "stl"
  writeFile (st </> "p.coscad") "r = 5\nmain = ● r\n"
  rs <- try (renderPart (st </> "p.coscad") Nothing) :: IO (Either SomeException ())
  case rs of
    Left e -> failT t "coscad stl renders a part" (show e)
    Right () -> do
      tris <- readStl (st </> "p.stl")
      let v = meshVolume tris
      assertT t "coscad stl renders a part" (abs (v - 4 / 3 * pi * 125) < 0.03 * (4 / 3 * pi * 125)) (printf "sphere volume %.2f" v)
  -- step scenes render fully (Manifold) where OpenSCAD supports it, with the bench in the mesh
  full <- openscadFullRender
  tmpBase <- getTemporaryDirectory
  let sceneF = tmpBase </> "coscad-test" </> "cli" </> "corner_pair_step1.scad"
      scenePng = dir </> "step1.png"
      sceneStl = dir </> "step1.stl"
  haveScene <- doesFileExist sceneF
  if not haveScene
    then failT t "step scene from the cli tier exists" sceneF
    else do
      rp <- runOpenscadRaw (openscadPngArgs full scenePng sceneF)
      pngOk <- doesFileExist scenePng
      pngSize <- if pngOk then fromIntegral <$> withFile scenePng ReadMode hFileSize else return (0 :: Int)
      assertT t ("step scene renders to PNG (" ++ (if full then "full Manifold render" else "preview") ++ ")")
        (either (const False) (\(c, out, err) -> c == ExitSuccess && not (any ("ERROR" `isPrefixOf`) (lines (out ++ err)))) rp && pngOk && pngSize > 5000)
        (show rp ++ " size " ++ show pngSize)
      when full $ do
        rsFull <- runOpenscadRaw ["-o", sceneStl, "--export-format", "asciistl", "--backend=manifold", "--render=force", sceneF]
        tris <- either (const []) id . parseStlAscii <$> readFile sceneStl
        let ((_, _, z0), (_, _, z1)) = meshBounds tris
        assertT t "full render keeps the bench slab under the parts (scene extends 2.5 mm below the rest face)"
          (either (const False) (\(c, _, _) -> c == ExitSuccess) rsFull && not (null tris) && z0 < -2.4 && z1 > 0)
          (show rsFull ++ " bounds z " ++ show (z0, z1))
  -- toolchain probes used by `coscad doctor`
  ov <- openscadVersion
  assertT t "openscad --version is parsed" (maybe False (not . null) ov) (show ov)
  bv <- bosl2Version dir
  assertT t "BOSL2 answers the version probe" (either (const False) (not . null) bv) (show bv)
  t1 <- getCurrentTime
  printf "(geometry tier: %.1fs)\n" (realToFrac (diffUTCTime t1 t0) :: Double)
  where
    parseLine l
      | "#" `isPrefixOf` l = []
      | otherwise = case words l of
          (f : rest) | Just g <- readGeo rest -> [(f, g)]
          _ -> []

-- ------------------------------------------------------------------

goldenMergeTier :: T -> IO ()
goldenMergeTier t = do
  let geo v = (v, (0, 0, 0), (1, 1, 1)) :: Geo
      golden = Map.fromList [("kept.coscad", geo 100), ("drift.coscad", geo 50), ("absent.coscad", geo 7)]
      current = [("kept.coscad", geo 100.004), ("drift.coscad", geo 40), ("new.coscad", geo 3)]
      updated = mergeGeometryGolden True golden current
      checked = mergeGeometryGolden False golden current
  assertT t "golden update keeps entries within tolerance" (Map.lookup "kept.coscad" updated == Just (geo 100)) (show updated)
  assertT t "golden update replaces drifted entries" (Map.lookup "drift.coscad" updated == Just (geo 40)) (show updated)
  assertT t "golden update records missing entries" (Map.lookup "new.coscad" updated == Just (geo 3)) (show updated)
  assertT t "golden update retains entries that did not render" (Map.lookup "absent.coscad" updated == Just (geo 7)) (show updated)
  assertT t "without update, drift is never accepted" (Map.lookup "drift.coscad" checked == Just (geo 50)) (show checked)
  assertT t "a run with nothing drifted leaves the golden unchanged" (mergeGeometryGolden True golden [("kept.coscad", geo 100.004)] == golden) ""
