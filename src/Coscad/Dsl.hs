{-# LANGUAGE UnicodeSyntax #-}

-- | Glyph and word aliases for using CoScad as an embedded Haskell
-- DSL (ghci, Examples.hs, ...). The .coscad file syntax lives in
-- Coscad.Parser; this module is only for Haskell-side modelling.
module Coscad.Dsl
  ( (|>)
  , (↯)
  , (⇋)
  , (⇓)
  , (∩)
  , (⊕)
  , (⊖)
  , (⊚)
  , (⊛)
  , (⊝)
  , (⊞)
  , (⊿)
  , (⌭)
  , (⌽)
  , (⎏)
  , (⏢)
  , (■)
  , (▣)
  , (▬)
  , (△)
  , (▷)
  , (▻)
  , (◉)
  , (◎)
  , (●)
  , (◙)
  , (✎)
  , (⟁)
  , (⬈)
  , (⬠)
  , (⭘)
  , (⮕)
  , anchorAt
  , attachAt
  , bak
  , bot
  , cone
  , ctr
  , cube
  , cuboid
  , cutAt
  , cyl
  , fwd
  , lft
  , loft
  , poly
  , posAt
  , prism
  , rect
  , rt
  , sph
  , top
  , torus
  , tube
  , wedge
  , xcyl
  , ycyl
  , zcyl
  , ζ
  , θ
  , χ
  , ψ
  , ω
  , ϕ
  )
where

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
posAt :: (D, D, D) -> Shape -> Shape -> Shape
posAt v = Position v (0, 0, 0)

-- | attachAt v parent child: child oriented +Z along v, bottom mated (glyph ⋈)
attachAt :: (D, D, D) -> Shape -> Shape -> Shape
attachAt v = AttachTo v (0, 0, 0)

-- | cutAt v parent cutter: cutter centered at parent's anchor v, subtracted
cutAt :: (D, D, D) -> Shape -> Shape -> Shape
cutAt v = CutAt v (0, 0, 0)

-- | anchorAt v shape: re-origin shape at its own anchor point (glyph ⚓)
anchorAt :: (D, D, D) -> Shape -> Shape
anchorAt = Anchor

-- | loft [(z, profile), ...]: skin 2D profiles into a solid (word `loft`, glyph ⟰)
loft :: [(D, Shape)] -> Shape
loft = Loft
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
(■) :: D -> Shape
(■) x = Rectangle x x x

cube :: D -> Shape
cube = (■)

rect :: D -> Shape
rect x = Rectangle x x x

(▬) :: D -> Shape
(▬) = rect

-- | sphere r
(●) :: D -> Shape
(●) = Sphere

sph :: D -> Shape
sph = (●)

-- | cylinder radius height
(◎) :: D -> D -> Shape
(◎) = Cylinder

cyl :: D -> D -> Shape
cyl = (◎)

-- | white up pointing triangle
-- | cone radius height
(▻) :: D -> D -> Shape
(▻) = Cone

cone :: D -> D -> Shape
cone = (▻)

-- | prism n radius height
prism :: I -> D -> D -> Shape
prism = Prism

(⎏) :: I -> D -> D -> Shape
(⎏) = prism

-- 2D regular polygons

-- | equilateral triangle profile
(△) :: D -> Shape
(△) = Shape2D 3

-- | regular pentagon profile
(⬠) :: D -> Shape
(⬠) = Shape2D 5

-- | circle profile
(⭘) :: D -> Shape
(⭘) = Shape2D 100

-- | Create a polygon with points and paths
poly :: [(D, D)] -> [[D]] -> Shape
poly points paths = Poly (PD points paths)

-- | Unicode operator for polygon
(⟁) :: [(D, D)] -> [[D]] -> Shape
(⟁) = poly
-- | Bezier path glyph (Haskell side takes the point list directly)
(✎) :: [(D, D)] -> Shape
(✎) = bezPoly 24

-- BOSL2 aliases -----------------------------------------------

-- | chamfered cuboid: ▣ x y z chamfer
(▣) :: D -> D -> D -> D -> Shape
(▣) x y z c = Cuboid (x, y, z) c 0

cuboid :: D -> D -> D -> Shape
cuboid x y z = Cuboid (x, y, z) 0 0

-- | rounded cuboid: ◙ x y z rounding
(◙) :: D -> D -> D -> D -> Shape
(◙) x y z r = Cuboid (x, y, z) 0 r

-- | chamfered centered cylinder: ⌭ r h chamfer
(⌭) :: D -> D -> D -> Shape
(⌭) r h c = Cyl r h c 0

-- | rounded centered cylinder: ⌽ r h rounding
(⌽) :: D -> D -> D -> Shape
(⌽) r h ro = Cyl r h 0 ro

-- | axis-aligned cylinders: radius length
xcyl :: D -> D -> Shape
xcyl = XCyl

ycyl :: D -> D -> Shape
ycyl = YCyl

zcyl :: D -> D -> Shape
zcyl = ZCyl

-- | tube: outer-r inner-r height
(⊚) :: D -> D -> D -> Shape
(⊚) = Tube

tube :: D -> D -> D -> Shape
tube = Tube

-- | prismoid: x1 y1 x2 y2 h
(⏢) :: D -> D -> D -> D -> D -> Shape
(⏢) x1 y1 x2 y2 = Prismoid (x1, y1) (x2, y2)

-- | torus: major-r minor-r
(◉) :: D -> D -> Shape
(◉) = Torus

torus :: D -> D -> Shape
torus = Torus

-- | wedge: x y z (vertical face at -X, hypotenuse in XZ)
(⊿) :: D -> D -> D -> Shape
(⊿) x y z = Wedge (x, y, z)

wedge :: D -> D -> D -> Shape
wedge x y z = Wedge (x, y, z)

-- Transform glyphs --------------------------------------------

χ :: D -> Shape -> Shape
χ = Tx

ψ :: D -> Shape -> Shape
ψ = Ty

ζ :: D -> Shape -> Shape
ζ = Tz

-- | Rotation glyphs (axis‑specific)
θ :: D -> Shape -> Shape
θ = Rx

ϕ :: D -> Shape -> Shape
ϕ = Ry

ω :: D -> Shape -> Shape
ω = Rz

-- | Scaling (vector form)
(⬈) :: (D, D, D) -> Shape -> Shape
(⬈) = Scale

-- | Mirror across plane normal (mx, my, mz)
(⇋) :: D -> D -> D -> Shape -> Shape
(⇋) mx my mz = Mirror (mx, my, mz)

-- | Linear extrusion (2‑D → 3‑D)
(⮕) :: D -> Shape -> Shape
(⮕) = Extrude

infixl 0 ⊞

infixl 0 ⇓

infixl 1 ↯

(⇓) :: [Shape] -> Shape
(⇓) = Hull

(⊞) :: [Shape] -> Shape
(⊞) = Minkowski

(↯) :: D -> Shape -> Shape
(↯) = Offset

infixl 0 ⊖

infixl 0 ⊝

infixl 0 ⊛

infixl 0 ⊕

-- Boolean difference
(⊖) :: Shape -> Shape -> Shape
a ⊖ b = Diff a b

(⊝) :: Shape -> Shape -> Shape
(⊝) = (⊖)

(⊕) :: Shape -> Shape -> Shape
a ⊕ b = Union [a, b]

infixl 0 ∩

(∩) :: Shape -> Shape -> Shape
a ∩ b = Intersection [a, b]

(⊛) :: Shape -> Shape -> Shape
(⊛) = (⊕)

-- COMPOSE OPERATIONS

infixl 1 |>

infixl 1 ▷

(|>) :: t1 -> (t1 -> t2) -> t2
x |> f = f x

(▷) :: t1 -> (t1 -> t2) -> t2
x ▷ f = f x

