-- | UTF-8 file access regardless of the process locale: .coscad files
-- are full of glyphs, and Windows consoles default to a legacy code page.
module Coscad.IO (readFileUtf8, writeFileUtf8) where

import System.IO

-- | Strict UTF-8 read (the handle is closed before this returns, so the
-- file can be rewritten immediately).
readFileUtf8 :: FilePath -> IO String
readFileUtf8 path = withFile path ReadMode $ \h -> do
  hSetEncoding h utf8
  s <- hGetContents h
  length s `seq` return s

writeFileUtf8 :: FilePath -> String -> IO ()
writeFileUtf8 path s = withFile path WriteMode $ \h -> do
  hSetEncoding h utf8
  hPutStr h s
