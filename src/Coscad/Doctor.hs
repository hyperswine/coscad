-- | `coscad doctor`: is the machine able to run the whole pipeline?
-- Reports which geometry backends this binary has and which one is the
-- default, finds OpenSCAD and BOSL2 the same way the other commands do,
-- asks both for their versions, and renders a small boolean part end to
-- end through every available backend against a known volume. OpenSCAD
-- is checked even in a kernel build as an explicit alternative backend;
-- native planner/site images do not need it. Exit 1 if any checked dependency
-- is missing or a round trip disagrees.
module Coscad.Doctor (runDoctor) where

import Coscad.Backend (backendName, defaultBackend, nativeCSGMesh)
import Coscad.Codegen (renderScad)
import Coscad.IO (scratchDir)
import Coscad.Manifold (manifoldAvailable, manifoldKernelVersion)
import Coscad.Mesh (Tri, meshVolume)
import Coscad.OpenScad
import Coscad.Parser (parseProgramNamed)
import Coscad.Report (Failure (..))
import Data.Version (showVersion)
import Paths_coscad (version)
import System.Directory (doesFileExist, removePathForcibly)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Printf (printf)

runDoctor :: IO (Either Failure ())
runDoctor = do
  putStrLn ("coscad " ++ showVersion version)
  dir <- scratchDir "doctor"
  -- geometry backends: a build-time fact, reported before any probe
  putStrLn ("backends    default  " ++ backendName defaultBackend ++ " (for stl, check and next; --backend=openscad|native overrides)")
  case manifoldKernelVersion of
    Just kernel -> putStrLn ("native      ok       Manifold " ++ kernel ++ " kernel linked statically (pinned by native-kernel.lock.json, with CrossSection)")
    Nothing -> putStrLn "native      absent   this binary was built without the pinned kernel (-f-manifold); --backend=native is refused. Build it with scripts/build-manifold.py"
  putStrLn ("images      default  " ++ (if manifoldAvailable then "native CPU renderer (plan --png and site; no OpenSCAD needed)" else "OpenSCAD renderer (-f-manifold build)"))
  putStrLn ("openscad    needed   " ++ (if manifoldAvailable then "only for --backend=openscad and this doctor's comparison probes" else "for every render, including plan --png and site"))
  -- OpenSCAD
  bin <- findOpenscad
  okScad <- case bin of
    Nothing -> do
      putStrLn "OpenSCAD    MISSING  install from https://openscad.org (2021.01 or newer), put `openscad` on PATH, or set COSCAD_OPENSCAD=/path/to/openscad"
      return False
    Just b -> do
      v <- openscadVersion
      case v of
        Just ver -> do
          full <- openscadFullRender
          putStrLn ("OpenSCAD    ok       " ++ b ++ " (" ++ ver ++ (if full then ", Manifold backend for STL and images" else ", CGAL only: no --backend; STL renders are slower") ++ ")")
          return True
        Nothing -> putStrLn ("OpenSCAD    BROKEN   " ++ b ++ " did not report a version (is it runnable? on a headless box set COSCAD_OPENSCAD to an xvfb-run wrapper)") >> return False
  -- BOSL2
  dirs <- libraryDirs
  bosl <- findBosl2
  envB <- lookupEnv "COSCAD_BOSL2"
  envOk <- case envB of
    Just d | not (null d) -> doesFileExist (d </> "std.scad")
    _ -> return True
  okBosl <- case (okScad, bosl) of
    (_, Just d) | not envOk -> do
      putStrLn ("BOSL2       BROKEN   COSCAD_BOSL2=" ++ d ++ " but there is no std.scad in that directory (it must name the BOSL2 checkout itself)")
      return False
    (_, Nothing) -> do
      putStrLn "BOSL2       MISSING  git clone https://github.com/BelfrySCAD/BOSL2 into an OpenSCAD library folder, or set COSCAD_BOSL2=/path/to/BOSL2"
      putStrLn ("                     library folders searched: " ++ unwords dirs)
      return False
    (False, Just d) -> putStrLn ("BOSL2       found    " ++ d ++ " (version not probed: OpenSCAD unavailable)") >> return True
    (True, Just d) -> do
      v <- bosl2Version dir
      case v of
        Right ver -> putStrLn ("BOSL2       ok       " ++ d ++ " (" ++ ver ++ ")") >> return True
        Left why -> do
          putStrLn ("BOSL2       BROKEN   found at " ++ d ++ " but OpenSCAD cannot include it: " ++ why)
          putStrLn "                     (set COSCAD_BOSL2 to that directory, or move it into a library folder)"
          return False
  -- round trips: the same part through every backend this binary has
  let probe = parseProgramNamed "doctor.coscad" "main = box 10 10 10 ⊖ zcyl 2 20\n"
  okNative <-
    if not manifoldAvailable
      then return True
      else case probe of
        Left e -> putStrLn ("trip native BROKEN   compiler: " ++ e) >> return False
        Right (_, shape) -> nativeCSGMesh shape >>= report "trip native"
  okRt <-
    if not (okScad && okBosl)
      then putStrLn "trip scad   skipped" >> return False
      else case probe of
        Left e -> putStrLn ("trip scad   BROKEN   compiler: " ++ e) >> return False
        Right (_, shape) -> do
          let scadF = dir </> "doctor.scad"
              stlF = dir </> "doctor.stl"
          writeFile scadF (renderScad shape)
          renderStlFile scadF stlF >>= report "trip scad  "
  removePathForcibly dir
  if okScad && okBosl && okRt && okNative
    then putStrLn "all good" >> return (Right ())
    else return (Left (Failure 1 []))
  where
    -- 10^3 minus a 50-gon prism of radius 2 through the full height
    want = let k = (50 / (2 * pi)) * sin (2 * pi / 50) in 1000 - k * pi * 4 * 10
    report :: String -> Either String [Tri] -> IO Bool
    report label r = case r of
      Left e -> putStrLn (label ++ " BROKEN   " ++ e) >> return False
      Right tris -> do
        let got = meshVolume tris
            ok = abs (got - want) < 0.01 * want
        printf "%s %s   box 10 10 10 ⊖ zcyl 2 20 -> %.2f mm^3 (expected %.2f), %d triangles\n" label (if ok then "ok    " else "BROKEN") got want (length tris)
        return ok
