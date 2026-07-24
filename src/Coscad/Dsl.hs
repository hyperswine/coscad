{-# LANGUAGE UnicodeSyntax #-}

-- | Glyph and word aliases for using CoScad as an embedded Haskell
-- DSL (ghci, Examples.hs, ...). The .coscad file syntax lives in
-- Coscad.Parser; this module is only for Haskell-side modelling.
module Coscad.Dsl (module Coscad.Dsl) where

import Coscad.Geometry
import Coscad.Shape

-- Attachment aliases (Haskell-side DSL) ------------------------

-- | anchor vectors
top, bot, lft, rt, fwd, bak, ctr :: V3
top = (0, 0, 1)
bot = (0, 0, -1)
lft = (-1, 0, 0)
rt = (1, 0, 0)
fwd = (0, -1, 0)
bak = (0, 1, 0)
ctr = (0, 0, 0)

-- | posAt v parent child: child snapped to parent's anchor v (glyph ⌖)
posAt v = Position v (0, 0, 0)

-- | attachAt v parent child: child oriented +Z along v, bottom mated (glyph ⋈)
attachAt v = AttachTo v (0, 0, 0)

-- | cutAt v parent cutter: cutter centered at parent's anchor v, subtracted
cutAt v = CutAt v (0, 0, 0)

-- | anchorAt v shape: re-origin shape at its own anchor point (glyph ⚓)
anchorAt = Anchor
-- ACTUAL LANGUAGE
-- Glyph-style aliases

{-
cube = ■ 25A0
sphere = ● 25CF
cyl = ◎ 25CE
cone = ▻ 25BB
rec = ▬

diff = ⊖ 2296
union = ⊛ 229b

extrude = ⮕ 2B95

scale = ⬈ 2B08

BOSL2 layer:
cuboid (chamfer)  = ▣ 25A3   ▣ x y z c
cuboid (rounding) = ◙ 25D9   ◙ x y z r
cyl (chamfer)     = ⌭ 232D   ⌭ r h c
cyl (rounding)    = ⌽ 233D   ⌽ r h r2
tube              = ⊚ 229A   ⊚ or ir h
prismoid          = ⏢ 23E2   ⏢ x1 y1 x2 y2 h
torus             = ◉ 25C9   ◉ rmaj rmin
wedge             = ⊿ 22BF   ⊿ x y z
mirror            = ⇋ 21CB   ⇋ mx my mz shape
axis cylinders    = xcyl / ycyl / zcyl r l (keywords)
-}

-- | cube x
(■) x = Rectangle x x x

cube = (■)

rect x = Rectangle x x x

(▬) = rect

-- | sphere r
(●) = Sphere

sph = (●)

-- | cylinder radius height
(◎) = Cylinder

cyl = (◎)

-- | white up pointing triangle
-- | cone radius height
(▻) = Cone

cone = (▻)

-- | prism n radius height
prism = Prism

(⎏) = prism

-- 2D regular polygons

-- | equilateral triangle profile
(△) = Shape2D 3

-- | regular pentagon profile
(⬠) = Shape2D 5

-- | circle profile
(⭘) = Shape2D 100

-- | Create a polygon with points and paths
poly points paths = Poly (PD points paths)

-- | Unicode operator for polygon
(⟁) = poly
-- | Bezier path glyph (Haskell side takes the point list directly)
(✎) = bezPoly 24

-- BOSL2 aliases -----------------------------------------------

-- | chamfered cuboid: ▣ x y z chamfer
(▣) x y z c = Cuboid (x, y, z) c 0

cuboid x y z = Cuboid (x, y, z) 0 0

-- | rounded cuboid: ◙ x y z rounding
(◙) x y z r = Cuboid (x, y, z) 0 r

-- | chamfered centered cylinder: ⌭ r h chamfer
(⌭) r h c = Cyl r h c 0

-- | rounded centered cylinder: ⌽ r h rounding
(⌽) r h ro = Cyl r h 0 ro

-- | axis-aligned cylinders: radius length
xcyl = XCyl

ycyl = YCyl

zcyl = ZCyl

-- | tube: outer-r inner-r height
(⊚) = Tube

tube = Tube

-- | prismoid: x1 y1 x2 y2 h
(⏢) x1 y1 x2 y2 = Prismoid (x1, y1) (x2, y2)

-- | torus: major-r minor-r
(◉) = Torus

torus = Torus

-- | wedge: x y z (vertical face at -X, hypotenuse in XZ)
(⊿) x y z = Wedge (x, y, z)

wedge x y z = Wedge (x, y, z)

-- Transform glyphs --------------------------------------------

χ = Tx

ψ = Ty

ζ = Tz

-- | Rotation glyphs (axis‑specific)
θ = Rx

ϕ = Ry

ω = Rz

-- | Scaling (vector form)
(⬈) = Scale

-- | Mirror across plane normal (mx, my, mz)
(⇋) mx my mz = Mirror (mx, my, mz)

-- | Linear extrusion (2‑D → 3‑D)
(⮕) = Extrude

infixl 0 ⊞

infixl 0 ⇓

infixl 1 ↯

(⇓) = Hull

(⊞) = Minkowski

(↯) = Offset

infixl 0 ⊖

infixl 0 ⊝

infixl 0 ⊛

infixl 0 ⊕

-- Boolean difference
a ⊖ b = Diff a b

(⊝) = (⊖)

a ⊕ b = Union [a, b]

infixl 0 ∩

a ∩ b = Intersection [a, b]

(⊛) = (⊕)

-- COMPOSE OPERATIONS

infixl 1 |>

infixl 1 ▷

x |> f = f x

x ▷ f = f x

