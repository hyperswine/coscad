-- | `coscad check foo.assemble` — assembly interference + clearance
-- checking on real meshes.
--
-- Method:
--   1. For each top-level part, re-resolve the asm expression with every
--      OTHER part hidden but retaining its bounding geometry for
--      attachment resolution: the render is that part exactly as
--      placed by the assembly (all instances, e.g. a mirrored pair).
--   2. Split each render into connected bodies (instances).
--   3. Pair up bodies across (and within) parts; prune by clearance-
--      expanded AABB overlap.
--   4. For surviving pairs, both bodies are emitted as OpenSCAD
--      polyhedron() literals and intersected by OpenSCAD, or intersected
--      directly as indexed native meshes. Nonzero volume = OVERLAP.
--   5. Non-overlapping close pairs are classified by vertex-to-triangle
--      and edge-to-edge distance (BVH-accelerated): CONTACT (< 0.02mm) is informational
--      (mating faces), a gap below the declared `clearance` is a WARNING.
--
-- Exit code 2 if any OVERLAP is found (1 means the check could not run).
module Coscad.Check
  ( processCheck
  , processCheckIn
  , processCheckInBackend
  , processCheckWithBackend
  , processCheckWith
  , splitBodies
  , splitSolidBodies
  , splitIndexedSolids
  )
where

import Coscad.Assemble
import Coscad.Backend (Backend (..), backendName, nativeIndexedCSGMesh)
import Coscad.Mesh.Distance (meshDistance)
import Coscad.Mesh.Indexed
import Coscad.Manifold (MeshOp (MeshIntersection), indexedOperation)
import Coscad.Codegen
import Coscad.IO (readFileLatin1, scratchDir)
import Coscad.Linalg (V3, cross, dot3, vlen, vsub)
import Coscad.Mesh (Tri, meshVolume, parseStlAscii, stlAscii)
import Coscad.OpenScad (openscadStlArgsIO, renderStlFile, runOpenscadRaw)
import Coscad.Shape
import Coscad.Report
import System.Exit (ExitCode (..))
import Control.Monad (forM, forM_, unless, when)
import Data.IORef
import Data.List (foldl', intercalate, minimumBy)
import qualified Data.Map as Map
import Data.Ord (comparing)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))

contactEps :: Double
contactEps = 0.02

volEps :: Double
volEps = 0.01 -- mm^3; below this an intersection is tessellation noise

-- ------------------------------------------------------------------
-- connected-body split (union-find over exact shared vertices)
splitBodies :: [Tri] -> [[Tri]]
splitBodies tris = Map.elems grouped
  where
    allVs = concat [[a, b, c] | (a, b, c) <- tris]
    vids = Map.fromList (zip (Map.keys (Map.fromList [(v, ()) | v <- allVs])) [(0 :: Int) ..])
    vid v = vids Map.! v
    n = Map.size vids
    parent0 = Map.fromList [(i, i) | i <- [0 .. n - 1]]
    find p i = if p Map.! i == i then i else find p (p Map.! i)
    unite p i j =
      let ri = find p i; rj = find p j
       in if ri == rj then p else Map.insert ri rj p
    pFinal =
      foldl'
        (\p (a, b, c) -> unite (unite p (vid a) (vid b)) (vid b) (vid c))
        parent0
        tris
    grouped =
      foldl'
        (\m t@(a, _, _) -> Map.insertWith (++) (find pFinal (vid a)) [t] m)
        Map.empty
        tris

-- | Group inward-oriented cavity shells with the smallest containing outward
-- shell. Splitting only by shared vertices would mistake an enclosed void for
-- another solid, causing false intersections against a part inside that void.
-- The input must be a closed, consistently oriented result from the backend.
splitSolidBodies :: [Tri] -> Either String [[Tri]]
splitSolidBodies tris = map concat <$> groupSolidShells [(s,s) | s<-splitBodies tris]

splitIndexedSolids :: IndexedMesh -> Either String [IndexedMesh]
splitIndexedSolids mesh = do
  groups<-groupSolidShells [(s,indexedTriangles s) | s<-indexedComponents mesh]
  mapM concatenateIndexed groups

-- Shell ownership uses geometry; payloads retain their original connectivity.
groupSolidShells :: [(a,[Tri])] -> Either String [[a]]
groupSolidShells shells = do
  let signed []=0
      signed ts@((origin,_,_):_) = sum [dot3 (vsub a origin) (cross (vsub b origin) (vsub c origin))/6 | (a,b,c)<-ts]
      outward=[(i,(payload,s)) | (i,(payload,s))<-zip [0::Int ..] shells,signed s>0]
      inward=[(payload,s) | (payload,s)<-shells,signed s<0]
      inside p surface = abs (sum (map angle surface))>2*pi
        where angle (a,b,c) =
                let u=vsub a p; v=vsub b p; w=vsub c p
                in 2*atan2 (dot3 u (cross v w))
                     (vlen u*vlen v*vlen w+dot3 u v*vlen w+dot3 v w*vlen u+dot3 w u*vlen v)
      owner (payload,(point,_,_):_) = case [(i,s) | (i,(_,s))<-outward,inside point s] of
        [] -> Left "assembly mesh contains an inward shell without a containing solid"
        bs -> Right (fst (minimumBy (comparing (meshVolume . snd)) bs),[payload])
      owner (_,[]) = Left "assembly mesh contains an empty shell"
  unless (all ((/=0) . signed . snd) shells) (Left "assembly mesh contains a zero-volume shell")
  cavities<-mapM owner inward
  return (Map.elems (Map.fromListWith (++) ([(i,[payload]) | (i,(payload,_))<-outward]++cavities)))

data CheckBody = NativeBody IndexedMesh | ScadBody [Tri]
bodyTriangles :: CheckBody -> [Tri]
bodyTriangles (NativeBody mesh)=indexedTriangles mesh
bodyTriangles (ScadBody tris)=tris

triBounds :: [Tri] -> (V3, V3)
triBounds ts =
  let vs = concat [[a, b, c] | (a, b, c) <- ts]
      xs = map (\(x, _, _) -> x) vs
      ys = map (\(_, y, _) -> y) vs
      zs = map (\(_, _, z) -> z) vs
   in ((minimum xs, minimum ys, minimum zs), (maximum xs, maximum ys, maximum zs))

aabbGap :: (V3, V3) -> (V3, V3) -> Double
aabbGap ((ax0, ay0, az0), (ax1, ay1, az1)) ((bx0, by0, bz0), (bx1, by1, bz1)) =
  let g lo0 hi0 lo1 hi1 = max 0 (max (lo1 - hi0) (lo0 - hi1))
      gx = g ax0 ax1 bx0 bx1
      gy = g ay0 ay1 by0 by1
      gz = g az0 az1 bz0 bz1
   in sqrt (gx * gx + gy * gy + gz * gz)

-- ------------------------------------------------------------------
-- emit a body as an OpenSCAD polyhedron (STL winding is CCW-outward;
-- polyhedron wants CW from outside, so faces are reversed)
polyScad :: [Tri] -> String
polyScad ts =
  let vkeys = Map.keys (Map.fromList [(v, ()) | (a, b, c) <- ts, v <- [a, b, c]])
      idx = Map.fromList (zip vkeys [(0 :: Int) ..])
      pts = vkeys
      shw (x, y, z) = "[" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "]"
      face (a, b, c) = "[" ++ show (idx Map.! c) ++ "," ++ show (idx Map.! b) ++ "," ++ show (idx Map.! a) ++ "]"
   in "polyhedron(points=["
        ++ intercalate "," (map shw pts)
        ++ "], faces=["
        ++ intercalate "," (map face ts)
        ++ "]);"

-- far-away marker: keeps OpenSCAD from failing on empty exports
marker :: String
marker = "translate([99999,99999,99999]) cube(0.001);"

-- | Like Next.runOpenscad but also reports CGAL trouble on stderr:
-- OpenSCAD exits 0 after a CGAL assertion and silently emits a
-- fallback (often the first operand), so the exit code alone lies.
runOpenscadChecked :: FilePath -> FilePath -> IO (Either String Bool)
runOpenscadChecked scadF stlF = do
  r <- openscadStlArgsIO stlF scadF >>= runOpenscadRaw
  return $ case r of
    Left e -> Left e
    Right (code, out, err) ->
      let cgalBad = any (\l -> contains "CGAL error" l || contains "assertion" l) (lines err ++ lines out)
       in case code of
            ExitSuccess -> Right cgalBad
            ExitFailure n -> Left ("openscad exit " ++ show n ++ ": " ++ take 300 err)
  where
    contains pat l = pat `isInfixOfS` l
    isInfixOfS pat l = any (\i -> take (length pat) (drop i l) == pat) [0 .. length l - length pat]

-- ------------------------------------------------------------------
processCheck :: Emit -> FilePath -> IO (Either Failure ())
processCheck emit = processCheckWith emit False

-- | Scratch renders go to a fresh folder under the system temp directory
-- (never shared with another running check) and are deleted afterwards
-- unless `keepTemp` (--keep-temp), which names the folder.
processCheckWith :: Emit -> Bool -> FilePath -> IO (Either Failure ())
processCheckWith = processCheckWithBackend OpenScadBackend

processCheckWithBackend :: Backend -> Emit -> Bool -> FilePath -> IO (Either Failure ())
processCheckWithBackend backend emit keepTemp = processCheckInBackend backend emit keepTemp Nothing

-- | As 'processCheckWith', with an explicit scratch directory (tests).
-- Overlaps are listed on stdout and fail with exit 2 and no further text.
processCheckIn :: Emit -> Bool -> Maybe FilePath -> FilePath -> IO (Either Failure ())
processCheckIn = processCheckInBackend OpenScadBackend

processCheckInBackend :: Backend -> Emit -> Bool -> Maybe FilePath -> FilePath -> IO (Either Failure ())
processCheckInBackend backend emit keepTemp scratch path = runCommand $ do
  dir <- maybe (scratchDir "check") return scratch
  createDirectoryIfMissing True dir
  res <- loadAssembly emit [] path
  case res of
    Left err -> abort (asmFailure err)
    Right ar -> case arAsm ar of
      Nothing -> abort (inputFailure "check needs an `asm = ...` definition.")
      Just _ -> do
        let clr = arClearance ar
            names = map fst (arParts ar)
        putStrLn ("check: " ++ show (length names) ++ " parts, clearance = " ++ show clr ++ "mm")
        putStrLn ("       geometry backend: " ++ backendName backend)
        -- 1. isolate each part inside the asm expression
        bodiesPer <- forM (arParts ar) $ \(pn, _) -> do
          let table0 = Map.fromList [(n, if n == pn then s else Hidden s) | (n, s) <- arParts ar]
          case resolveAssembly ar table0 of
            Left err -> abort (Failure 1 ["Error isolating " ++ pn ++ ": " ++ err])
            Right table -> case Map.lookup "asm" table of
              Nothing -> return (pn, [])
              Just asmS -> do
                let scadF = dir </> ("chk_" ++ pn ++ ".scad")
                    stlF = dir </> ("chk_" ++ pn ++ ".stl")
                writeScad asmS scadF
                r <- case backend of
                  NativeExperimental -> do
                    result <- nativeIndexedCSGMesh asmS
                    case result of
                      Right mesh -> writeFile stlF (stlAscii "isolated_part" (indexedTriangles mesh))
                      Left _ -> return ()
                    return (map NativeBody <$> (result >>= splitIndexedSolids))
                  OpenScadBackend -> do
                    appendFile scadF ("\n" ++ marker ++ "\n")
                    result <- renderStlFile scadF stlF
                    return (map ScadBody . filter farFromMarker <$> (result >>= splitSolidBodies))
                case r of
                  Left err -> abort (Failure 1 ["Error rendering/splitting " ++ pn ++ ": " ++ err])
                  Right bodies -> return (pn,bodies)
        let insts =
              [ (pn ++ (if length bs > 1 then "#" ++ show i else ""), b)
              | (pn, bs) <- bodiesPer
              , (i, b) <- zip [(1 :: Int) ..] bs
              ]
        putStrLn ("       " ++ show (length insts) ++ " placed instances: " ++ intercalate ", " (map fst insts))
        -- 2. pairwise checks
        overlapsR <- newIORef (0 :: Int)
        warnsR <- newIORef (0 :: Int)
        let pairs = [(x, y) | (x : rest) <- tails' insts, y <- rest]
        forM_ (zip [(0 :: Int) ..] pairs) $ \(i, ((na, bodyA), (nb, bodyB))) -> do
          let ba=bodyTriangles bodyA; bb=bodyTriangles bodyB
              gap0 = aabbGap (triBounds ba) (triBounds bb)
          if gap0 > max clr contactEps
            then return ()
            else do
              -- overlap through the selected backend
              let scadF = dir </> ("chk_pair" ++ show i ++ ".scad")
                  stlF = dir </> ("chk_pair" ++ show i ++ ".stl")
              let writePair x y = writeFile scadF ("intersection() {\n union() { " ++ polyScad x ++ marker ++ " }\n union() { " ++ polyScad y ++ marker ++ " }\n}\n")
                  measure = do
                    tris <- either (const []) id . parseStlAscii <$> readFileLatin1 stlF
                    return (sum (map meshVolume (filter farFromMarkerNot (splitBodies tris))))
              volM <- case backend of
                NativeExperimental -> do
                  result <- case (bodyA,bodyB) of
                    (NativeBody a,NativeBody b) -> indexedOperation MeshIntersection a b
                    _ -> return (Left "internal backend/body mismatch")
                  case result of
                    Left err -> abort (Failure 1 ["Error intersecting " ++ na ++ " and " ++ nb ++ ": " ++ err])
                    Right mesh -> return (Just (meshVolume (indexedTriangles mesh)))
                OpenScadBackend -> do
                  writePair ba bb
                  r1 <- runOpenscadChecked scadF stlF
                  case r1 of
                    Right False -> Just <$> measure
                    _ -> do
                      writePair bb ba -- CGAL assertions are often operand-order dependent
                      r2 <- runOpenscadChecked scadF stlF
                      case r2 of
                        Right False -> Just <$> measure
                        _ -> return Nothing -- boolean unstable near coplanar faces
              let note = case volM of Nothing -> "  (boolean unstable; distance verdict)"; Just _ -> ""
              case volM of
                Just vol | vol > volEps -> do
                  modifyIORef' overlapsR (+ 1)
                  putStrLn ("OVERLAP  " ++ na ++ " x " ++ nb ++ "  volume " ++ show (rnd vol) ++ " mm^3")
                _ -> do
                  d <- case meshDistance ba bb of
                    Just distance -> return distance
                    Nothing -> abort (Failure 1 ["Cannot resolve surface distance between " ++ na ++ " and " ++ nb])
                  if d < contactEps
                    then putStrLn ("contact  " ++ na ++ " x " ++ nb ++ note)
                    else
                      if d < clr
                        then do
                          modifyIORef' warnsR (+ 1)
                          putStrLn ("WARN     " ++ na ++ " x " ++ nb ++ "  gap " ++ show (rnd d) ++ " mm < clearance " ++ show clr ++ note)
                        else unless (gap0 > clr) $
                          putStrLn ("ok       " ++ na ++ " x " ++ nb ++ "  gap " ++ show (rnd d) ++ " mm" ++ note)
        no <- readIORef overlapsR
        nw <- readIORef warnsR
        putStrLn ("check: " ++ show no ++ " overlaps, " ++ show nw ++ " clearance warnings")
        if keepTemp then putStrLn ("scratch renders kept in " ++ dir) else removePathForcibly dir
        when (no > 0) (abort (Failure 2 []))
  where
    rnd x = fromIntegral (round (x * 1000) :: Integer) / 1000 :: Double
    tails' [] = []
    tails' l@(_ : xs) = l : tails' xs
    farFromMarker b = let ((x, _, _), _) = triBounds b in x < 90000
    farFromMarkerNot = farFromMarker
