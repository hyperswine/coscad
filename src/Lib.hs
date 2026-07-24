-- | Backwards-compatible re-export of the CoScad library.
-- Existing embedded-DSL code (Examples.hs, Z.hs, Spec.hs) imports
-- Lib; new code can import the Coscad.* modules directly.
module Lib
  ( module Coscad.Shape
  , module Coscad.Geometry
  , module Coscad.Codegen
  , module Coscad.Dsl
  ) where

import Coscad.Codegen
import Coscad.Dsl
import Coscad.Geometry
import Coscad.Shape
