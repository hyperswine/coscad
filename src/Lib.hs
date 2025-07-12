{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Lib where

import Data.List (intercalate)

type D = Double

type I = Int

data Shape
  = Rectangle D D D
  | Sphere D
  | Cylinder D D
  | Cone D D
  | Prism I D D
  | Poly PD
  | Shape2D I D
  | Tx D Shape
  | Ty D Shape
  | Yz D Shape
  | Rx D Shape
  | Ry D Shape
  | Rz D Shape
  | Scale (D, D, D) Shape
  | Diff Shape Shape
  | Extrude D Shape
  | Union [Shape]
  deriving (Show)

-- | Polygon Data
data PD = PD [(D, D)] [[D]] deriving (Show)
