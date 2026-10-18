-- | The test harness: pass/fail counters, assertions, scratch
-- directories, and helpers shared by the tiers.
module Harness
  ( T (..)
  , pass
  , failT
  , assertT
  , expectErr
  , expectGen
  , section
  , tempDir
  , firstOr
  , readJson
  , readStl
  , renderMesh
  , Geo
  , geoOf
  , showGeo
  , readGeo
  , geoClose
  )
where

import Coscad.Codegen (renderScad)
import Coscad.Json (JValue (..), decode)
import Coscad.Mesh (Tri, meshBounds, meshVolume, parseStlAscii)
import Coscad.OpenScad (renderStlFile)
import Coscad.Shape (Shape (..))
import Data.IORef
import Data.List (isInfixOf)
import System.Directory
import System.FilePath
import Text.Printf (printf)

data T = T {tPassed :: IORef Int, tFailed :: IORef [String]}

pass :: T -> IO ()
pass t = modifyIORef' (tPassed t) (+ 1)

failT :: T -> String -> String -> IO ()
failT t name detail = do
  putStrLn ("FAIL " ++ name)
  putStrLn (unlines (map ("     " ++) (lines detail)))
  modifyIORef' (tFailed t) (++ [name])

assertT :: T -> String -> Bool -> String -> IO ()
assertT t name ok detail = if ok then pass t else failT t name detail

-- | Expect a Left whose message contains every `yes` and no `no`.
expectErr :: T -> String -> Either String a -> [String] -> [String] -> IO ()
expectErr t name r yes no = case r of
  Right _ -> failT t name "expected an error, but it parsed"
  Left msg ->
    let missing = [y | y <- yes, not (y `isInfixOf` msg)]
        present = [n | n <- no, n `isInfixOf` msg]
     in assertT t name (null missing && null present) $
          "message:\n" ++ msg ++ "\nmissing: " ++ show missing ++ "\nunwanted: " ++ show present

-- | Expect a Right whose rendered .scad contains the fragment.
expectGen :: T -> String -> Either String (a, Shape) -> String -> IO ()
expectGen t name r frag = case r of
  Left e -> failT t name e
  Right (_, m) -> assertT t name (frag `isInfixOf` renderScad m) ("wanted fragment: " ++ frag ++ "\n" ++ renderScad m)

-- ------------------------------------------------------------------
-- build plans (coscad plan): pure, no OpenSCAD needed

section :: String -> IO ()
section s = putStrLn ("\n== " ++ s)

-- ------------------------------------------------------------------
-- 1. diagnostics

type Geo = (Double, (Double, Double, Double), (Double, Double, Double))

renderMesh :: FilePath -> FilePath -> String -> IO (Either String [Tri])
renderMesh _ dir scadText = do
  let scadF = dir </> "part.scad"
      stlF = dir </> "part.stl"
  writeFile scadF scadText
  r <- renderStlFile scadF stlF
  removePathForcibly stlF
  return r

geoOf :: [Tri] -> Geo
geoOf tris = (meshVolume tris, lo, hi)
  where
    (lo, hi) = meshBounds tris

showGeo :: Geo -> String
showGeo (v, (a, b, c), (d, e, f)) = unwords (map (printf "%.3f") [v, a, b, c, d, e, f])

readGeo :: [String] -> Maybe Geo
readGeo [v, a, b, c, d, e, f] = Just (read v, (read a, read b, read c), (read d, read e, read f))
readGeo _ = Nothing

geoClose :: Geo -> Geo -> Bool
geoClose (v1, lo1, hi1) (v2, lo2, hi2) =
  abs (v1 - v2) <= max 0.01 (1e-3 * abs v2) && cl lo1 lo2 && cl hi1 hi2
  where
    cl (a, b, c) (x, y, z) = all (\(p, q) -> abs (p - q) <= 0.01) [(a, x), (b, y), (c, z)]

-- | Decode a JSON file; a missing file is a Left, not an exception.
readJson :: FilePath -> IO (Either String JValue)
readJson f = do
  e <- doesFileExist f
  if e then decode <$> readFile f else return (Left (f ++ ": not written"))

readStl :: FilePath -> IO [Tri]
readStl f = either (\e -> error (f ++ ": " ++ e)) id . parseStlAscii <$> readFile f

-- ------------------------------------------------------------------
-- cli: the built binary, as an editor or pipeline sees it

tempDir :: String -> IO FilePath
tempDir name = do
  base <- getTemporaryDirectory
  let d = base </> "coscad-test" </> name
  removePathForcibly d
  createDirectoryIfMissing True d
  return d

-- | The first element, or a default for an empty list.
firstOr :: a -> [a] -> a
firstOr d xs = case xs of
  x : _ -> x
  [] -> d
