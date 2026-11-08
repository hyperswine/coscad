-- | The word-syntax renderer: a shape tree back as the @!simple@
-- vocabulary plus the all-modes word shapes, so any file the compiler
-- accepts can be written without glyphs. Transforms become pipeline
-- stages, n-ary booleans become @*@/@-@ chains and @isect@/@hull@/@mink@
-- stages, and definitions keep their lines ('renderWords'). What the
-- language cannot spell in words yet is an honest 'Left' naming the
-- construct, never a guess: chamfered or rounded BOSL2 primitives
-- (the ▣ ◙ ⌭ ⌽ glyphs), arbitrary-axis rotations and matrix transforms
-- (from mate), mating frames, tags, and profiles outside
-- Triangle/Pentagon/Circle.
module Coscad.Words
  ( renderWords
  , wordsShape
  ) where

import Coscad.Format (showD)
import Coscad.Geometry (bbox, resolve)
import Coscad.Parser (parseProgramFull)
import Coscad.Shape

-- | Render one shape tree as a word expression. The tree is resolved
-- first, so attachment sugar arrives as plain transforms, the form the
-- compiler itself works in.
wordsShape :: Shape -> Either String String
wordsShape s = fst <$> go (resolve s)

-- | Render a whole part file: every definition in source order, numbers
-- as numbers, shapes as word expressions, under a @!simple@ pragma. A
-- definition's shared subexpressions are inlined where other definitions
-- used them, so the rendered file reparses to the same geometry.
renderWords :: FilePath -> String -> Either String String
renderWords file input = do
  (_, _, _, defs) <- parseProgramFull file input
  ls <- mapM defLine defs
  Right (unlines ("!simple" : "// rendered to word syntax by `coscad words`" : ls))
  where
    defLine (name, Left d) = Right (name ++ " = " ++ showD d)
    defLine (name, Right shape) = do
      (e, _) <- go (resolve shape)
      Right (name ++ " = " ++ e)

-- | How tightly a rendered expression binds, deciding where it needs
-- parentheses. The grammar's levels, loosest first: pipeline (stages),
-- boolean (@*@/@-@), transform (prefix @loft@/@Offset@), primary (the
-- words). A pipeline head accepts anything, but a stage's shape
-- arguments and boolean operands only take primaries bare.
data Form = Prim | Tight | Bool | Pipe deriving (Eq)

go :: Shape -> Either String (String, Form)
go s = case s of
  -- bed-anchored primitives
  Rectangle x y z -> prim ("block " ++ d3 x y z)
  Cylinder r h -> prim ("pillar " ++ showD r ++ " " ++ showD h)
  Cone r h -> prim ("spike " ++ showD r ++ " " ++ showD h)
  Prism n r h -> prim ("prism " ++ show n ++ " " ++ showD r ++ " " ++ showD h)
  -- centred primitives
  Sphere r -> prim ("sphere " ++ showD r)
  Cuboid (x, y, z) 0 0 -> prim ("box " ++ d3 x y z)
  Cuboid {} -> Left "a chamfered or rounded cuboid (the ▣ ◙ glyphs) has no word spelling"
  Cyl r h 0 0 -> prim ("cyl " ++ showD r ++ " " ++ showD h)
  Cyl {} -> Left "a chamfered or rounded cyl (the ⌭ ⌽ glyphs) has no word spelling"
  XCyl r l -> prim ("xcyl " ++ showD r ++ " " ++ showD l)
  YCyl r l -> prim ("ycyl " ++ showD r ++ " " ++ showD l)
  ZCyl r l -> prim ("zcyl " ++ showD r ++ " " ++ showD l)
  Tube ro ri h -> prim ("tube " ++ showD ro ++ " " ++ showD ri ++ " " ++ showD h)
  Prismoid (x1, y1) (x2, y2) h -> prim ("Prismoid " ++ d4 x1 y1 x2 y2 ++ " " ++ showD h)
  Torus rj rn -> prim ("torus " ++ showD rj ++ " " ++ showD rn)
  Wedge (x, y, z) -> prim ("wedge " ++ d3 x y z)
  Frustum r1 r2 h -> prim ("frustum " ++ showD r1 ++ " " ++ showD r2 ++ " " ++ showD h)
  -- 2D
  Poly (PD pts [])
    | length pts >= 3 -> prim ("poly " ++ unwords (concatMap (\(x, y) -> [showD x, showD y]) pts))
  Poly (PD _ _) -> Left "a polygon with holes has no word spelling"
  Shape2D n r
    | n == 3 -> prim ("Triangle " ++ showD r)
    | n == 5 -> prim ("Pentagon " ++ showD r)
    | n == 100 -> prim ("Circle " ++ showD r)
    | otherwise -> Left ("a " ++ show n ++ "-sided profile has no word spelling; the profiles are Triangle, Pentagon and Circle")
  -- transforms as pipeline stages
  Tx d x -> stage x ("move " ++ showD d ++ " 0 0")
  Ty d x -> stage x ("move 0 " ++ showD d ++ " 0")
  Tz d x -> stage x ("move 0 0 " ++ showD d)
  Translate (x, y, z) x' -> stage x' ("move " ++ d3 x y z)
  Rx a x -> stage x ("rotx " ++ showD a)
  Ry a x -> stage x ("roty " ++ showD a)
  Rz a x -> stage x ("rotz " ++ showD a)
  RotAxis a (0, 0, az) x | az /= 0 -> stage x ("rotz " ++ showD (if az > 0 then a else negate a))
  RotAxis a (ax, 0, 0) x | ax /= 0 -> stage x ("rotx " ++ showD (if ax > 0 then a else negate a))
  RotAxis a (0, ay, 0) x | ay /= 0 -> stage x ("roty " ++ showD (if ay > 0 then a else negate a))
  RotAxis {} -> Left "a rotation about an arbitrary axis has no word spelling"
  Scale (sx, sy, sz) x -> stage x ("scale " ++ d3 sx sy sz)
  Mirror (nx, ny, nz) x -> stage x ("mirror " ++ d3 nx ny nz)
  Transform3 {} -> Left "a matrix transform (from mate) has no word spelling"
  -- lifts
  Extrude h x -> stage x ("extrude " ++ showD h)
  Revolve a x -> stage x ("revolve " ++ showD a)
  RevolveSeg a n x -> stage x ("revolve " ++ showD a ++ " " ++ show n)
  Loft ps
    | length ps >= 2 -> do
        pairs <- mapM (\(z, p) -> (\(t, f) -> showD z ++ " " ++ (t <<+ f)) <$> go p) ps
        Right ("loft " ++ unwords pairs, Tight)
    | otherwise -> Left "a loft needs at least two profiles"
  Offset r x -> (\(t, f) -> ("Offset " ++ showD r ++ " " ++ (t <<+ f), Tight)) <$> go x
  -- booleans: a flat chain reparses left-nested, exactly the nesting the
  -- glyph operators build; anything else on a right-hand side gets parens
  Union xs -> chain "*" xs
  Diff a b -> do
    (ta, fa) <- go a
    (tb, fb) <- go b
    Right ((if fa == Pipe then "(" ++ ta ++ ")" else ta) ++ " - " ++ (tb <<+ fb), Bool)
  Intersection xs -> pipelineChain "isect" xs
  Hull xs -> pipelineChain "hull" xs
  Minkowski xs -> pipelineChain "mink" xs
  -- metadata
  Datum name (px, py, pz) x -> do
    (t, _) <- go x
    let ((x0, y0, z0), (x1, y1, z1)) = bbox x
        (cx, cy, cz) = ((x0 + x1) / 2, (y0 + y1) / 2, (z0 + z1) / 2)
    Right (t ++ " |> datum " ++ name ++ " ctr " ++ d3 (px - cx) (py - cy) (pz - cz), Pipe)
  Tag _ _ -> Left "a tagged shape (an assembly instance) has no word spelling"
  Frame {} -> Left "a mating frame has no word spelling (yet)"
  Empty -> Left "an empty shape has no word spelling"
  Hidden _ -> Left "a hidden shape has no word spelling"
  Anchor {} -> Left "attachment sugar survived resolution"
  Position {} -> Left "attachment sugar survived resolution"
  AttachTo {} -> Left "attachment sugar survived resolution"
  CutAt {} -> Left "attachment sugar survived resolution"
  where
    prim t = Right (t, Prim)
    d3 a b c = unwords (map showD [a, b, c])
    d4 a b c e = unwords (map showD [a, b, c, e])

    -- operand rendering: a stage's shape arguments and loft profiles are
    -- primaries, so anything looser is parenthesised
    t <<+ f = if f == Prim then t else "(" ++ t ++ ")"
    infixl 8 <<+

    stage x args = do
      (t, _) <- go x
      Right (t ++ " |> " ++ args, Pipe)

    chain _ [] = Left "an empty boolean group has no word spelling"
    chain op (x : rest) = do
      (t, f) <- go x
      rs <- mapM go rest
      let piece (t2, f2) = " " ++ op ++ " " ++ (if f2 == Bool || f2 == Pipe then "(" ++ t2 ++ ")" else t2)
      Right ((if f == Pipe then "(" ++ t ++ ")" else t) ++ concatMap piece rs, Bool)

    pipelineChain _ [] = Left "an empty boolean group has no word spelling"
    pipelineChain op (x : rest) = do
      (t, _) <- go x
      rs <- mapM go rest
      Right (t ++ concatMap (\(t2, f2) -> " |> " ++ op ++ " " ++ (t2 <<+ f2)) rs, Pipe)
