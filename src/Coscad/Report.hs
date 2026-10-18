-- | How commands report. A command that cannot finish returns a 'Failure'
-- (its exit status and the exact lines to print); warnings and notes go to
-- an 'Emit' callback supplied by the caller. Library code never prints to
-- stderr or exits the process: app/Main.hs does both, so the pipeline can be
-- called as functions (tests, tools, an editor) and diagnostics collected.
module Coscad.Report
  ( Failure (..)
  , Diag (..)
  , Emit
  , abort
  , runCommand
  , inputFailure
  , designFailure
  , designFailures
  , renderDiag
  , stderrEmit
  , silent
  , collecting
  )
where

import Control.Exception (Exception, throwIO, try)
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.IO (hPutStrLn, stderr)

-- | A command that could not finish. 'failExit' is the process exit
-- status: 1, the command could not run (usage, missing file, syntax or
-- geometry error, OpenSCAD failure); 2, the input is well formed but the
-- design has a problem. 'failLines' are printed to stderr as they are; the
-- first normally starts with @Error: @. Output already printed to stdout
-- (a check's overlap list) may leave 'failLines' empty.
data Failure = Failure {failExit :: Int, failLines :: [String]}
  deriving (Eq, Show)

instance Exception Failure

-- | A non-fatal diagnostic, printed by the default emitter as
-- @Warning: ...@ or @Note: ...@.
data Diag = Warning String | Note String
  deriving (Eq, Show)

type Emit = Diag -> IO ()

renderDiag :: Diag -> String
renderDiag d = case d of
  Warning m -> "Warning: " ++ m
  Note m -> "Note: " ++ m

-- | The CLI's emitter: one line per diagnostic on stderr.
stderrEmit :: Emit
stderrEmit = hPutStrLn stderr . renderDiag

-- | Discard diagnostics (tests and tools that only need the result).
silent :: Emit
silent _ = return ()

-- | An emitter that records diagnostics, and the action that reads them
-- back in order.
collecting :: IO (Emit, IO [Diag])
collecting = do
  ref <- newIORef []
  return (\d -> modifyIORef' ref (d :), reverse <$> readIORef ref)

-- | Stop the current command with a failure. Only 'runCommand' catches it,
-- so it never escapes a command's public function.
abort :: Failure -> IO a
abort = throwIO

-- | Run a command body, returning its failure instead of throwing it.
runCommand :: IO a -> IO (Either Failure a)
runCommand = try

-- | Exit 1 / exit 2 with a message: its first line is prefixed
-- @Error: @, further lines (notes, excerpts) are kept as they are.
inputFailure, designFailure :: String -> Failure
inputFailure = Failure 1 . errorLines
designFailure = Failure 2 . errorLines

-- | Several independent design problems, each on its own @Error:@ line.
designFailures :: [String] -> Failure
designFailures = Failure 2 . map ("Error: " ++)

errorLines :: String -> [String]
errorLines msg = case lines msg of
  [] -> ["Error: (no message)"]
  l : ls -> ("Error: " ++ l) : ls
