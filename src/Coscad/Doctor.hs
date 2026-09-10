-- | `coscad doctor`: is the machine able to run the whole pipeline?
-- Finds OpenSCAD and BOSL2 the same way the other commands do, asks
-- both for their versions, and renders a small boolean part end to end
-- against a known volume. Exit 1 if anything is missing.
module Coscad.Doctor (runDoctor) where

import Coscad.Codegen (renderScad)
import Coscad.Mesh (meshVolume)
import Coscad.OpenScad
import Coscad.Parser (parseProgramNamed)
import Data.Version (showVersion)
import Paths_coscad (version)
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Text.Printf (printf)

runDoctor :: IO ()
runDoctor = do
  putStrLn ("coscad " ++ showVersion version)
  tmp <- getTemporaryDirectory
  let dir = tmp </> "coscad-doctor"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  -- OpenSCAD
  bin <- findOpenscad
  okScad <- case bin of
    Nothing -> do
      putStrLn "OpenSCAD    MISSING  install from https://openscad.org (2021.01 or newer), put `openscad` on PATH, or set COSCAD_OPENSCAD=/path/to/openscad"
      return False
    Just b -> do
      v <- openscadVersion
      case v of
        Just ver -> putStrLn ("OpenSCAD    ok       " ++ b ++ " (" ++ ver ++ ")") >> return True
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
  -- round trip
  okRt <-
    if not (okScad && okBosl)
      then putStrLn "round trip  skipped" >> return False
      else case parseProgramNamed "doctor.coscad" "main = box 10 10 10 ⊖ zcyl 2 20\n" of
        Left e -> putStrLn ("round trip  BROKEN   compiler: " ++ e) >> return False
        Right (_, shape) -> do
          let scadF = dir </> "doctor.scad"
              stlF = dir </> "doctor.stl"
          writeFile scadF (renderScad shape)
          r <- renderStlFile scadF stlF
          case r of
            Left e -> putStrLn ("round trip  BROKEN   " ++ e) >> return False
            Right tris -> do
              -- 10^3 minus a 50-gon prism of radius 2 through the full height
              let k = (50 / (2 * pi)) * sin (2 * pi / 50)
                  want = 1000 - k * pi * 4 * 10
                  got = meshVolume tris
                  ok = abs (got - want) < 0.01 * want
              printf "round trip  %s   box 10 10 10 ⊖ zcyl 2 20 -> %.2f mm^3 (expected %.2f), %d triangles\n" (if ok then "ok    " else "BROKEN") got want (length tris)
              return ok
  removePathForcibly dir
  if okScad && okBosl && okRt
    then putStrLn "all good"
    else exitFailure
