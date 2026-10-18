-- | Examples tier (pure): every example compiles and its `.scad`
-- matches the snapshot under test/golden/scad/.
module Tier.Examples
  ( exampleFiles
  , normKey
  , goldenScad
  , examples
  , firstDiff
  )
where

import Control.Monad (forM, forM_)
import Coscad.Assemble (loadAssembleFile)
import Coscad.Codegen (renderScad)
import Coscad.Parser (parseProgramNamed)
import Data.List (isPrefixOf, sort)
import System.Directory
import System.FilePath
import Harness

exampleFiles :: String -> IO [FilePath]
exampleFiles ext = do
  fs <- concat <$> mapM walk ["examples", "examples-next"]
  return (sort [normKey f | f <- fs, takeExtension f == ext, not ("examples/archive/" `isPrefixOf` normKey f)])
  where
    walk d = do
      es <- listDirectory d
      concat <$> forM es (\e -> do
        let f = d </> e
        isD <- doesDirectoryExist f
        if isD then walk f else return [f])

-- | Golden keys always use forward slashes so the files are shared across OSes.
normKey :: FilePath -> FilePath
normKey = map (\c -> if c == '\\' then '/' else c)

goldenScad :: FilePath -> FilePath
goldenScad f = "test/golden/scad" </> replaceExtension f ".scad"

examples :: T -> Bool -> IO ()
examples t update = do
  section "examples compile + .scad snapshots"
  fs <- exampleFiles ".coscad"
  forM_ fs $ \f -> do
    src <- readFile f
    case parseProgramNamed f src of
      Left e -> failT t ("compile " ++ f) e
      Right (_, m) -> do
        let out = renderScad m
            g = goldenScad f
        exists <- doesFileExist g
        if update || not exists
          then do
            createDirectoryIfMissing True (takeDirectory g)
            writeFile g out
            putStrLn ((if exists then "updated " else "created ") ++ g)
            pass t
          else do
            want <- readFile g
            assertT t ("snapshot " ++ f) (want == out) (firstDiff want out ++ "\n(COSCAD_UPDATE_GOLDEN=1 cabal test to accept)")
  section "assemblies load"
  as <- exampleFiles ".assemble"
  forM_ as $ \f -> do
    r <- loadAssembleFile [] f
    case r of
      Left e -> failT t ("load " ++ f) e
      Right _ -> pass t

firstDiff :: String -> String -> String
firstDiff a b =
  case [(i, x, y) | (i, x, y) <- zip3 [(1 :: Int) ..] (lines a ++ repeat "<eof>") (lines b ++ repeat "<eof>"), x /= y] of
    ((i, x, y) : _) -> "first difference at line " ++ show i ++ "\n  golden: " ++ x ++ "\n  now:    " ++ y
    [] -> "(no line differs; whitespace/eof?)"

-- ------------------------------------------------------------------
-- 3. geometry (OpenSCAD)
