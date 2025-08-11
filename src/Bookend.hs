module Bookend where

import Lib (D, PD (..), Shape (..), writeScad)

{-
wₜ = total width
h₂ = vert h
w₂ = vert w
h₁ = base h
l₁ = base l
w₁ = support w
h₃ = support h
depth
-}

wₜ = 8.0
h₂ = 12.0
w₂ = 1.5
h₁ = 1.0
l₁ = 6
w₁ = 3.0
h₃ = 8.0
depth = l₁ + w₂

sect₁ = Rectangle w₂ wₜ h₂
sect₁' = Tz h₁ sect₁

base = Rectangle (l₁ - 1.5) wₜ h₁

tipshape = Poly $ PD points paths where
  points = [(0,0), (1.5, 0.0), (0.0, h₁)]
  paths = [[0,1,2]]
tip = Extrude wₜ tipshape
tip' = Rz 90 tip
