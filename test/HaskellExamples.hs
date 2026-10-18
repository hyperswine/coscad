-- | Keeps examples/haskell/ compiling against the library: each sample is
-- a module of embedded-DSL definitions meant for ghci, so this suite only
-- imports them and renders one shape from each.
module Main (main) where

import qualified ExampleBookend as B1
import qualified ExampleBookend2 as B2
import qualified ExampleGear as G
import qualified ExampleZigZag as Z
import Lib (renderScad)
import System.Exit (exitFailure)

main :: IO ()
main = do
  let samples =
        [ ("ExampleBookend.tip'", renderScad B1.tip')
        , ("ExampleBookend2.bookendDemo", renderScad B2.bookendDemo)
        , ("ExampleGear.gearDemo", renderScad G.gearDemo)
        , ("ExampleZigZag.spring", renderScad Z.spring)
        ]
      empty = [n | (n, s) <- samples, null s]
  mapM_ (\(n, s) -> putStrLn (n ++ ": " ++ show (length s) ++ " chars of OpenSCAD")) samples
  if null empty then putStrLn "haskell examples: all compile and render" else putStrLn ("empty render: " ++ unwords empty) >> exitFailure
