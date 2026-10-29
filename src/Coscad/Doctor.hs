-- | `coscad doctor`: is this binary able to run the whole pipeline? Reports
-- the linked geometry kernel and the image renderer, then runs one boolean
-- part through the native geometry path against its known volume. Since 1.6.0.0 nothing external is needed:
-- no OpenSCAD, no BOSL2, no display. Exit 1 if a round trip disagrees.
module Coscad.Doctor (runDoctor) where

import Coscad.Backend (backendName, defaultBackend, nativeCSGMesh)
import Coscad.Manifold (manifoldKernelVersion)
import Coscad.Mesh (Tri, meshVolume)
import Coscad.Parser (parseProgramNamed)
import Coscad.Report (Failure (..))
import Data.Version (showVersion)
import Paths_coscad (version)
import Text.Printf (printf)

runDoctor :: IO (Either Failure ())
runDoctor = do
  putStrLn ("coscad " ++ showVersion version)
  putStrLn ("geometry    " ++ backendName defaultBackend ++ "   CoScad tessellation + Manifold " ++ maybe "?" id manifoldKernelVersion
            ++ " kernel, linked statically (pinned by native-kernel.lock.json, with CrossSection)")
  putStrLn "images      native   built-in CPU renderer for plan --png and site (no display or GPU needed)"
  putStrLn "external    none     OpenSCAD and BOSL2 are not used; `coscad part.coscad` still exports .scad for OpenSCAD users"
  okNative <- case parseProgramNamed "doctor.coscad" "main = box 10 10 10 ⊖ zcyl 2 20\n" of
    Left e -> putStrLn ("trip native BROKEN   compiler: " ++ e) >> return False
    Right (_, shape) -> nativeCSGMesh shape >>= report "trip native"
  if okNative
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
