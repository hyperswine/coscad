-- Render physical instances separately, in the assembled upright pose.
-- Do not fuse touching printed components into one artificial boolean solid.
-- From the repo root:
-- cabal exec -- ghc -package coscad models/panel-braced-cube/Preview.hs -o /private/tmp/cube-preview
-- cd models/panel-braced-cube && /private/tmp/cube-preview
module Main (main) where

import Control.Monad (forM, unless)
import Data.List (isPrefixOf)
import Coscad.Assemble (loadAssembly)
import Coscad.Image
import Coscad.Plan.Model (buildModel, Model (..), Inst (..))
import Coscad.Report (silent)

main :: IO ()
main = do
  scenes <- forM ["bare", "back", "back-side"] $ \name -> do
    loaded <- loadAssembly silent [] (name ++ ".assemble")
    ar <- either (fail . show) pure loaded
    (model, errors) <- either fail pure (buildModel ar)
    unless (null errors) (fail (unlines errors))
    let colour i
          | "wall_" `isPrefixOf` iPart i = (235, 174, 58)
          | iPart i == "br" = (105, 137, 166)
          | otherwise = (170, 180, 190)
        scene = Scene [SceneObject (iName i) (iShape i) (iAff i) (colour i) | i <- mInsts model] defaultView
    pure ("preview-" ++ name ++ ".png", scene)
  result <- renderScenes scenes
  either fail pure result
