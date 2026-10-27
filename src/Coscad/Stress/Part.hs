-- | `coscad stress` on a part: resolve its support/load directives against the
-- part's geometry, voxelize the rendered mesh, solve with Hex20 elements and
-- judge the result against the material's strengths and the requested safety.
module Coscad.Stress.Part
  ( StressSetup (..), StressReport (..), RegionReport (..), setupFrom, defaultCell, analysePart, reportJson, reportSummary
  ) where

import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Coscad.Geometry (bbox, datumPoints, resolve)
import Coscad.Json (JValue (..))
import Coscad.Linalg (V3, vadd, vlen, vsub)
import Coscad.Mesh (Tri, meshBounds, meshVolume, triArea, triNormal)
import Coscad.Shape (Shape)
import Coscad.Stress.Hex20
import Coscad.Stress.Material
import Coscad.Stress.Spec
import Coscad.Stress.Voxel
import Coscad.Validate (finite)
import Text.Read (readMaybe)

data StressSetup = StressSetup
  { suMaterial :: Material
  , suSafety :: Double
  , suCell :: Maybe Double
  } deriving (Eq, Show)

data RegionReport = RegionReport
  { rrWhere :: Where
  , rrFaces :: Int        -- ^ exterior cell faces selected (0 for a point)
  , rrNodes :: Int
  , rrForce :: Maybe V3
  } deriving (Eq, Show)

data StressReport = StressReport
  { srSetup :: StressSetup
  , srCell :: Double
  , srGrid :: VoxelGrid
  , srMeshVolume :: Double
  , srSupports :: [RegionReport]
  , srLoads :: [RegionReport]
  , srResult :: Hex20Result
  , srMaxDisplacement :: (Double, V3)        -- ^ magnitude and where
  , srPeakVonMises :: (Double, V3)           -- ^ largest Gauss-point von Mises and the cell centre
  , srCentreVonMises :: Double               -- ^ largest cell-centre von Mises
  , srAcrossLayers :: (Double, V3)           -- ^ largest tensile stress along z
  , srSafetyInPlane :: Double
  , srSafetyAcross :: Double
  , srNotes :: [String]
  }

-- | Material, safety factor and cell size from the `stress` settings.
setupFrom :: Analysis -> Either String StressSetup
setupFrom a = do
  let settings = aSettings a
      latest key = lookup key (reverse settings)
  material <- maybe (materialNamed "pla") materialNamed (latest "material")
  safety <- case latest "safety" of
    Nothing -> Right 2
    Just raw -> case readMaybe raw :: Maybe Double of
      Just v | finite v && v >= 1 && v <= 100 -> Right v
      _ -> Left ("stress: safety= must be a number from 1 to 100, got '" ++ raw ++ "'")
  cell <- case latest "cell" of
    Nothing -> Right Nothing
    Just raw -> case readMaybe raw :: Maybe Double of
      Just v | finite v && v > 0 -> Right (Just v)
      _ -> Left ("stress: cell= must be a positive size in mm, got '" ++ raw ++ "'")
  return (StressSetup material safety cell)

-- | Default cell. Printed parts are mostly boxy, so first look for a cell that
-- fits every axis-aligned face of the mesh exactly: the greatest common
-- divisor (to 1 um) of the face plane offsets from the bounding-box corner.
-- When that cell, or a multiple of it, keeps the grid within twice the cell
-- budget it is used, and the geometry is then represented without partial
-- cells. Otherwise the cell is the typical wall thickness (2V/A of the mesh)
-- divided by a whole number, at least two, aimed at the budget.
defaultCell :: [Tri] -> Double
defaultCell tris =
  let volume = meshVolume tris
      area = sum (map triArea tris)
      thickness = if area > 0 then 2 * volume / area else 1
      budget = 4000 :: Double
      coarse = (volume / budget) ** (1/3)
      perWall = max 2 (round (thickness / coarse)) :: Int
      wallCell = thickness / fromIntegral perWall
      ((x0,y0,z0),(x1,y1,z1)) = meshBounds tris
      extents = [x1-x0, y1-y0, z1-z0]
      micro v = round (v * 1000) :: Integer
      offsets = [ micro (c - o) | (a,b,c') <- tris, let n = triNormal (a,b,c'), (axis, o) <- zip [0..2::Int] [x0,y0,z0], abs (component axis n) > 0.999999, let c = component axis a ]
                ++ map micro extents
      g = foldr gcd 0 offsets
      exact = fromIntegral g / 1000
      cellsAt h = volume / (h*h*h)   -- solid cells the grid will hold, roughly
      multiple = max 1 (floor (wallCell / exact)) :: Integer
      snapped = exact * fromIntegral multiple
  in if g > 0 && exact >= 0.25 && snapped <= thickness / 2 + 1e-9 && cellsAt snapped <= 2 * budget
       then snapped else wallCell
  where
    component :: Int -> V3 -> Double
    component 0 (x,_,_) = x
    component 1 (_,y,_) = y
    component _ (_,_,z) = z

analysePart :: Analysis -> StressSetup -> Shape -> [Tri] -> Either String StressReport
analysePart analysis setup shape tris = do
  unless (not (null (aSupports analysis))) (Left "stress: the part declares no support; add a line like `support bot`")
  unless (not (null (aLoads analysis))) (Left "stress: the part declares no load; add a line like `load top 0 0 -20`")
  let resolved = resolve shape
      (lo, hi) = bbox resolved
      datums = datumPoints resolved
      cell = fromMaybe (defaultCell tris) (suCell setup)
      extents = vsub hi lo
  unless (vlen extents > 0) (Left "stress: the part has no extent")
  let candidate = product [ceiling (e / cell) | e <- vec extents] :: Integer
  unless (candidate <= 200000) (Left ("stress: cell=" ++ show cell ++ " mm gives more than 200000 candidate cells; use a larger cell"))
  grid <- voxelize cell tris
  unless (voxelCount grid <= 60000) (Left ("stress: " ++ show (voxelCount grid) ++ " solid cells exceeds the 60000-cell budget; use a larger cell"))
  let bodies = connectedCells grid
  unless (length bodies == 1) (Left ("stress: the part voxelizes into " ++ show (length bodies) ++ " separate bodies at cell " ++ show cell ++ " mm (a wall or bridge thinner than a cell, or parts touching only along an edge); use a smaller cell"))
  let meshVol = meshVolume tris
      ratio = voxelVolume grid / meshVol
      faces = exteriorFaces grid
      h = vgCell grid
      datumPoint name = case Map.lookup name datums of
        Nothing -> Left ("stress: unknown datum '@" ++ name ++ "'; available datums: " ++ (case Map.keys datums of [] -> "none"; ns -> unwords (map ('@':) ns)))
        Just [p] -> Right p
        Just ps -> Left ("stress: ambiguous datum '@" ++ name ++ "': " ++ show (length ps) ++ " copies")
      axisOf (x,y,z) | x /= 0 = (0, signum x) | y /= 0 = (1, signum y) | otherwise = (2, signum z)
      planeOf face@(Face c axis sgn) = component axis (cellCorner grid c) + (if sgn > 0 then h else 0) + 0 * fromIntegral (faceSign face)
      component i (x,y,z) = [x,y,z] !! i
      -- faces with the given outward normal whose plane is within one cell of the target coordinate
      planarFaces normal target =
        let (axis, sgn) = axisOf normal
        in [ f | f@(Face _ a s) <- faces, a == axis, fromIntegral s == sgn, abs (planeOf f - target) <= h + 1e-9 ]
      selectFaces w = case w of
        AtBox normal -> let (axis, sgn) = axisOf normal
                            target = component axis (if sgn > 0 then hi else lo)
                        in Right (planarFaces normal target)
        AtDatumFace name normal -> do
          p <- datumPoint name
          let (axis, _) = axisOf normal
          return (planarFaces normal (component axis p))
        AtDatum _ -> Right []
      nearestNode p = case sortOn snd [ (k, vlen (vsub (nodePosition grid k) p)) | k <- Set.toList (allNodes grid) ] of
        ((k, _):_) -> k
        [] -> (0,0,0) -- the grid is never empty here
      regionNodes w = do
        selected <- selectFaces w
        case w of
          AtDatum name -> do
            p <- datumPoint name
            return (selected, [(nearestNode p, 1)])
          _ -> do
            unless (not (null selected)) (Left ("stress: " ++ describeWhere w ++ " selects no exterior face of the voxel mesh"))
            return (selected, [ (k, wgt / fromIntegral (length selected)) | f <- selected, (k, wgt) <- faceNodes f ])
  supportSel <- mapM regionNodes (aSupports analysis)
  loadSel <- mapM (\(w, f) -> (\(fs, ns) -> (w, f, fs, ns)) <$> regionNodes w) (aLoads analysis)
  let clamped = Set.fromList [ k | (_, ns) <- supportSel, (k, _) <- ns ]
      loads = Map.fromListWith vadd [ (k, scale wgt force) | (_, force, _, ns) <- loadSel, (k, wgt) <- ns ]
      absorbed = sum [ abs wgt | (_, _, _, ns) <- loadSel, (k, wgt) <- ns, Set.member k clamped ] / max 1e-12 (sum [ abs wgt | (_, _, _, ns) <- loadSel, (_, wgt) <- ns ])
      material = suMaterial setup
      problem = Hex20Problem grid (matYoungMPa material) (matPoisson material) clamped loads 1e-7 50000
  result <- solveHex20 problem
  let (maxDisp, at) = maximumOn fst [ (vlen d, p) | (_, p, d) <- hrNodes result ]
      (peak, peakAt) = maximumOn fst [ (csPeakVonMises s, c) | (_, c, s) <- hrCells result ]
      centrePeak = maximum [ csCentreVonMises s | (_, _, s) <- hrCells result ]
      (across, acrossAt) = maximumOn fst [ (csPeakTensionZ s, c) | (_, c, s) <- hrCells result ]
      safetyIn = if peak > 0 then matStrengthMPa material / peak else 1/0
      safetyAcross = if across > 0 then matAcrossLayersMPa material / across else 1/0
      notes = [ "Voxel mesh holds " ++ show (round (100*ratio) :: Int) ++ "% of the rendered volume; walls thinner than two cells are poorly represented, use a smaller cell" | abs (ratio - 1) > 0.05 ]
           ++ [ "Point loads at datums act on one node, which is a stress singularity; judge deflection, not the stress there" | (AtDatum _, _, _, _) <- loadSel ]
           ++ [ show (round (100 * absorbed) :: Int) ++ "% of the load weight sits on clamped nodes and goes straight into the support" | absorbed > 0.02 ]
           ++ [ "Peak stress is the largest Gauss-point value on a stair-stepped mesh; refine the cell to check it has settled" ]
           ++ [ "Across-layer strength assumes layers stacked along the part's z axis" | matPrinted material ]
      regionReport w fs ns force = RegionReport w (length fs) (length (Set.fromList (map fst ns))) force
  return StressReport
    { srSetup = setup, srCell = h, srGrid = grid, srMeshVolume = meshVol
    , srSupports = [ regionReport w fs ns Nothing | (w, (fs, ns)) <- zip (aSupports analysis) supportSel ]
    , srLoads = [ regionReport w fs ns (Just f) | (w, f, fs, ns) <- loadSel ]
    , srResult = result
    , srMaxDisplacement = (maxDisp, at), srPeakVonMises = (peak, peakAt), srCentreVonMises = centrePeak
    , srAcrossLayers = (across, acrossAt), srSafetyInPlane = safetyIn, srSafetyAcross = safetyAcross, srNotes = notes }
  where
    vec (x,y,z) = [x,y,z]
    scale s (x,y,z) = (s*x, s*y, s*z)
    maximumOn f = foldl1 (\a b -> if f b > f a then b else a)

reportJson :: Bool -> FilePath -> String -> StressReport -> JValue
reportJson full source backend r = JObject $
  [ ("schema", JString "coscad.stress/1")
  , ("source", JString source)
  , ("geometry_backend", JString backend)
  , ("material", JObject [ ("name", JString (matName m)), ("young_mpa", JNumber (matYoungMPa m)), ("poisson", JNumber (matPoisson m))
                         , ("strength_mpa", JNumber (matStrengthMPa m)), ("across_layers_mpa", JNumber (matAcrossLayersMPa m)), ("printed", JBool (matPrinted m)) ])
  , ("mesh", JObject [ ("cell_mm", JNumber (srCell r)), ("grid_cells", counts), ("solid_cells", JNumber (fromIntegral (voxelCount g)))
                     , ("degrees_of_freedom", JNumber (fromIntegral (hrDofs res))), ("voxel_volume_mm3", JNumber (voxelVolume g)), ("mesh_volume_mm3", JNumber (srMeshVolume r)) ])
  , ("supports", JArray (map region (srSupports r)))
  , ("loads", JArray (map region (srLoads r)))
  , ("result", JObject
      [ ("max_displacement_mm", JNumber (fst (srMaxDisplacement r))), ("max_displacement_at_mm", v3 (snd (srMaxDisplacement r)))
      , ("peak_von_mises_mpa", JNumber (fst (srPeakVonMises r))), ("peak_von_mises_at_mm", v3 (snd (srPeakVonMises r)))
      , ("max_cell_centre_von_mises_mpa", JNumber (srCentreVonMises r))
      , ("max_across_layer_tension_mpa", JNumber (fst (srAcrossLayers r))), ("max_across_layer_tension_at_mm", v3 (snd (srAcrossLayers r)))
      , ("support_reaction_n", v3 (hrReaction res)), ("support_moment_n_mm", v3 (hrMoment res)), ("strain_energy_n_mm", JNumber (hrEnergy res))
      , ("iterations", JNumber (fromIntegral (hrIterations res))), ("free_residual_relative", JNumber (hrResidual res)) ])
  , ("safety", JObject [ ("requested", JNumber (suSafety (srSetup r))), ("in_plane", JNumber (srSafetyInPlane r)), ("across_layers", JNumber (srSafetyAcross r))
                       , ("pass", JBool (passes r)) ])
  , ("notes", JArray (map JString (srNotes r)))
  , ("limitations", JArray (map JString
      [ "Small-strain linear elasticity of a homogeneous isotropic solid; infill, layer bonding, contact, friction, buckling, fatigue and large deflection are not modelled."
      , "Material values are table defaults, not measurements of this spool or print."
      , "Supports are fully clamped faces or nodes; loads are uniform tractions over selected faces or single nodes."
      , "Geometry is a stair-stepped voxel grid; refine the cell and compare before trusting a peak." ]))
  ] ++ (if full then
  [ ("nodes", JArray [ JObject [("position_mm", v3 p), ("displacement_mm", v3 d)] | (_, p, d) <- hrNodes res ])
  , ("cells", JArray [ JObject [("centre_mm", v3 c), ("xx_yy_zz_xy_yz_zx_mpa", JArray (map JNumber (csCentre s))), ("peak_von_mises_mpa", JNumber (csPeakVonMises s))] | (_, c, s) <- hrCells res ]) ]
  else [])
  where
    m = suMaterial (srSetup r); g = srGrid r; res = srResult r
    (nx,ny,nz) = vgCounts g
    counts = JArray (map (JNumber . fromIntegral) [nx,ny,nz])
    region rr = JObject ([ ("where", JString (describeWhere (rrWhere rr))), ("faces", JNumber (fromIntegral (rrFaces rr))), ("nodes", JNumber (fromIntegral (rrNodes rr))) ]
                         ++ maybe [] (\f -> [("force_n", v3 f)]) (rrForce rr))
    v3 (x,y,z) = JArray (map JNumber [x,y,z])

passes :: StressReport -> Bool
passes r = srSafetyInPlane r >= suSafety (srSetup r) && srSafetyAcross r >= suSafety (srSetup r)

-- | The lines printed after a run.
reportSummary :: StressReport -> [String]
reportSummary r =
  [ "material " ++ matName m ++ " (E " ++ showN (matYoungMPa m) ++ " MPa), cell " ++ showN (srCell r) ++ " mm, " ++ show (voxelCount g) ++ " cells, " ++ show (hrDofs res) ++ " unknowns, " ++ show (hrIterations res) ++ " iterations"
  , "max displacement " ++ showN (fst (srMaxDisplacement r)) ++ " mm at " ++ showV (snd (srMaxDisplacement r))
  , "peak stress " ++ showN (fst (srPeakVonMises r)) ++ " MPa von Mises at " ++ showV (snd (srPeakVonMises r)) ++ " (cell centres: " ++ showN (srCentreVonMises r) ++ " MPa)"
  , "across layers " ++ showN (fst (srAcrossLayers r)) ++ " MPa tension at " ++ showV (snd (srAcrossLayers r))
  , "safety factor " ++ showN (srSafetyInPlane r) ++ " in plane, " ++ showN (srSafetyAcross r) ++ " across layers; requested " ++ showN (suSafety (srSetup r)) ++ (if passes r then " (ok)" else " (NOT MET)")
  ] ++ map ("note: " ++) (srNotes r)
  where
    m = suMaterial (srSetup r); g = srGrid r; res = srResult r
    showN x = let s = show (fromIntegral (round (x * 1000) :: Integer) / 1000 :: Double) in if s == "Infinity" then "inf" else s
    showV (x,y,z) = "[" ++ unwords (map showN [x,y,z]) ++ "]"
