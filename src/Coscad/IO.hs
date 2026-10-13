-- | UTF-8 file access regardless of the process locale: .coscad files
-- are full of glyphs, and Windows consoles default to a legacy code page.
module Coscad.IO (readFileUtf8, writeFileUtf8, readFileLatin1, scratchDir) where

import Control.Exception (throwIO, try)
import System.Directory (createDirectory, createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath ((</>))
import System.IO
import System.IO.Error (isAlreadyExistsError)

-- | Strict UTF-8 read (the handle is closed before this returns, so the
-- file can be rewritten immediately).
readFileUtf8 :: FilePath -> IO String
readFileUtf8 path = withFile path ReadMode $ \h -> do
  hSetEncoding h utf8
  s <- hGetContents h
  length s `seq` return s

-- | Byte-transparent read (one Char per byte) for files that may not be
-- text at all, such as an STL OpenSCAD wrote in binary; never throws a
-- decoding error.
readFileLatin1 :: FilePath -> IO String
readFileLatin1 path = withFile path ReadMode $ \h -> do
  hSetEncoding h char8
  s <- hGetContents h
  length s `seq` return s

-- | A fresh, empty directory `<tmp>/coscad-<name>-<n>` that no other
-- running coscad owns: the first `n` whose creation succeeds. Concurrent
-- `check`, `doctor` or test runs therefore never delete each other's
-- scratch renders.
scratchDir :: String -> IO FilePath
scratchDir name = do
  tmp <- getTemporaryDirectory
  createDirectoryIfMissing True tmp
  let go n = do
        let d = tmp </> ("coscad-" ++ name ++ "-" ++ show (n :: Int))
        r <- try (createDirectory d)
        case r of
          Right () -> return d
          Left e | isAlreadyExistsError e && n < 100000 -> go (n + 1)
          Left e -> throwIO e
  go 1

writeFileUtf8 :: FilePath -> String -> IO ()
writeFileUtf8 path s = withFile path WriteMode $ \h -> do
  hSetEncoding h utf8
  hPutStr h s
