-- | The expression grammar: pipelines, boolean and attachment
-- operators, transforms and shape primaries. Mutually recursive by
-- nature (parentheses and `$` re-enter the expression parser), so it
-- lives in one module over 'Coscad.Parser.Core'.
module Coscad.Parser.Grammar
  ( expression
  ) where

import Control.Monad (foldM, void, when)
import Coscad.Loft (loftErrors)
import Coscad.Dim (Dim (..), dimOf)
import Coscad.Geometry (MatingFrame (..), PointRef (..), datumPoints, frameOf, makeFrame, matingPose, pointOf)
import Coscad.Linalg (vadd, vneg, vsub)
import Coscad.Parser.Core
import Coscad.Shape
import Coscad.Stdlib (standardShape)
import Coscad.Validate (maxQuantity, maxSides, suggestion, validateShape)
import qualified Data.Map as Map
import Text.Megaparsec

-- Parse an expression (pipelines bind loosest)
expression :: Ctx -> Parser Shape
expression = pipeExpression

-- | Shape argument for prefix forms: either a primary, or '$' followed
-- by the whole rest of the expression (Haskell-style loose application):
--   Translate.x 5 $ a * b        χ 5 $ a ⊕ b
shapeArg :: Ctx -> Parser Shape
shapeArg ctx = (symbol "$" *> expression ctx) <|> primaryExpression ctx

-- |> pipelines: each stage is a postfix operation on the shape so far.
--   plate |> at top 5.5 0 -5 flange |> cutat lft 3.5 0 0 (xcyl 2.7 12)
pipeExpression :: Ctx -> Parser Shape
pipeExpression ctx = do
  left <- booleanExpression ctx
  rest <- many $ do
    _ <- symbol "|>"
    off <- getOffset
    stage <- pipeStage ctx
    return (off, stage)
  -- `|>` binds loosest, so a boolean operator cannot follow a pipeline;
  -- point at the fix instead of "expecting |> or end of input"
  when (not (null rest)) $ do
    o <- getOffset
    op <- optional (lookAhead (oneOf ("\x2295\x229B\x2296\x229D\x2229\x21D3\x229E\x21AF*" :: String)))
    case op of
      Just c -> setOffset o *> fail ("'" ++ [c] ++ "' cannot follow a pipeline: |> binds loosest, so wrap the pipeline in parentheses, e.g. (a |> move 1 2 3) " ++ [c] ++ " b")
      Nothing -> return ()
  foldM apply left rest
  where
    apply s (off, stage) = either (\e -> setOffset off *> fail e) return (stage s)

pipeStage :: Ctx -> Parser (Shape -> Either String Shape)
pipeStage ctx = choice
  [ datumStage
  , frameStage
  , mateStage
  , placeStage
  , gridStage
  , anchorStage
  , fmap (\f s -> Right (f s)) plainStage
  ]
  where
    plainStage = choice
      [ num1 "x" Tx
      , num1 "y" Ty
      , num1 "z" Tz
      , num1 "rotx" Rx
      , num1 "roty" Ry
      , num1 "rotz" Rz
      , num1 "extrude" Extrude
      , do
          _ <- keyword "revolve"
          a <- num ctx
          mSegs <- optional (lookAhead (try (num ctx)) *> revolveSegs ctx)
          return (\p -> case mSegs of Nothing -> Revolve a p; Just n -> RevolveSeg a n p)
      , num3 "move" Translate
      , num3 "scale" Scale
      , num3 "mirror" Mirror
      , keyword "loft" *> (loftStage <$> num ctx <*> primaryExpression ctx)
      , rel "at" Position
      , rel "on" AttachTo
      , rel "cutat" CutAt
      , bin "add" (\p s -> Union [p, s])
      , bin "cut" Diff
      , bin "isect" (\p s -> Intersection [p, s])
      , do
          _ <- keyword "hull"
          child <- optional (shapeArg ctx)
          return (\p -> Hull (p : maybe [] (: []) child))
      , bin "mink" (\p s -> Minkowski [p, s])
      , do
          off <- getOffset
          name <- identifier
          setOffset off *> fail ("unknown pipeline stage '" ++ name ++ "'" ++ suggestion stages name ++ "; use e.g. |> move x y z")
      ]
    stages = ["x", "y", "z", "rotx", "roty", "rotz", "extrude", "revolve", "move", "scale", "mirror", "anchor", "datum", "frame", "mate", "place", "grid", "loft", "at", "on", "cutat", "add", "cut", "isect", "hull", "mink"]
    anchorStage = do
      at <- srcPos
      _ <- keyword "anchor"
      ref <- pointRef
      return $ \p -> case ref of
        BoxPoint v -> Right (Anchor at v p)
        NamedPoint _ -> (\v -> Translate (vneg v) p) <$> pointOf ref p
    datumStage = do
      _ <- keyword "datum"
      name <- identifier
      ref <- pointRef
      off <- pointOffsets "datum"
      return $ \p -> do
        when (Map.member name (datumPoints p)) (Left ("duplicate datum '@" ++ name ++ "'; choose a new name"))
        pt <- pointOf ref p
        return (Datum name (vadd pt off) p)
    frameStage = do
      _ <- keyword "frame"
      name <- identifier
      ref <- pointRef
      normal <- anchorVec <?> "frame outward normal (top bot rt lft fwd bak)"
      up <- anchorVec <?> "frame in-plane up direction"
      off <- pointOffsets "frame"
      return $ \p -> do
        requireSolid "frame" p
        when (Map.member name (datumPoints p)) (Left ("duplicate datum or frame '@" ++ name ++ "'; choose a new name"))
        pt <- pointOf ref p
        MatingFrame origin basis <- makeFrame (vadd pt off) normal up
        return (Frame name origin basis p)
    mateStage = do
      _ <- keyword "mate"
      parent <- primaryExpression ctx
      target <- frameRef
      own <- frameRef
      (roll, gap) <- ((0,0) <$ stageEnd) <|> ((,) <$> arg ctx "mate" "roll in degrees" <*> arg ctx "mate" "gap in mm (roll and gap are both required)")
      return $ \child -> do
        requireSolid "mate parent" parent
        requireSolid "mate child" child
        loftErrors parent
        validateShape parent
        pf <- frameOf target parent
        cf <- frameOf own child
        let (r,t) = matingPose pf cf roll gap
        return (Transform3 r t child)
    requireSolid context s = do
      d <- dimOf s
      when (d /= D3) (Left (context ++ " needs a 3D solid; extrude a 2D profile first"))
    -- Only the piped child is returned; the separate datum solid never enters
    -- the emitted CSG or assembly instance graph. Both points are explicit.
    placeStage = do
      _ <- keyword "place"
      parent <- primaryExpression ctx
      target <- pointRef
      own <- pointRef
      off <- pointOffsets "place"
      return $ \child -> do
        _ <- dimOf parent
        loftErrors parent
        validateShape parent
        pa <- pointOf target parent
        ca <- pointOf own child
        return (Translate (vsub (vadd pa off) ca) child)
    gridStage = do
      _ <- keyword "grid"
      nx <- gridCount "column count"
      ny <- gridCount "row count"
      when (nx * ny > maxQuantity) (fail ("grid copy count exceeds " ++ show maxQuantity ++ "; reduce columns or rows"))
      px <- arg ctx "grid" "column pitch"
      py <- arg ctx "grid" "row pitch"
      when ((nx > 1 && px == 0) || (ny > 1 && py == 0)) (fail "grid pitch must be nonzero on each repeated axis; negative pitch is allowed")
      return $ \child -> do
        -- Stop counting at the budget before allocating repeated subtrees.
        -- This bounds nested grids, not just one expansion at a time.
        let leaves = length (take (maxQuantity + 1) (leafShapes child))
        when (nx * ny * leaves > maxQuantity) (Left ("grid expanded geometry exceeds " ++ show maxQuantity ++ " primitive copies (including nested grids); reduce repetition"))
        return (Union
          [ if i == 0 && j == 0 then child
            else Translate (fromIntegral i * px, fromIntegral j * py, 0) child
          | j <- [0 .. ny - 1], i <- [0 .. nx - 1]
          ])
    leafShapes s = case shapeChildren s of [] -> [s]; xs -> concatMap leafShapes xs
    gridCount param = do
      off <- getOffset
      n <- arg ctx "grid" param
      if n >= 1 && n <= fromIntegral maxQuantity && n == fromInteger (truncate n)
        then return (round n :: Int)
        else setOffset off *> fail ("grid " ++ param ++ " must be a whole number from 1 to " ++ show maxQuantity)
    -- `keyword` backtracks by itself when the word does not match; the
    -- stage body is deliberately NOT under `try`, so an error inside a
    -- stage is reported where it happened (megaparsec >= 9.7 reports a
    -- failed `try` at its start).
    num1 w f = keyword w *> (f <$> num ctx)
    num3 w f = keyword w *> (f <$> ((,,) <$> num ctx <*> num ctx <*> num ctx))
    rel w f = do
      at <- srcPos
      _ <- keyword w
      v <- anchorVec
      off <- offsets w
      child <- shapeArg ctx
      return (\p -> f at v off p child)
    offsets w = option (0, 0, 0) $ do
      _ <- lookAhead (try (num ctx))
      vectorArgs w "third offset z (three offsets followed by a child shape are required)"
    -- These stages have no trailing child argument. Only a stage/expression
    -- boundary omits offsets; a misspelled numeric binding must stay an error.
    stageEnd = lookAhead (void (symbol "|>") <|> void (symbol ")") <|> eof)
    pointOffsets w = ((0, 0, 0) <$ stageEnd)
      <|> vectorArgs w "third offset z (three offsets are required)"
    vectorArgs w third = (,,) <$> arg ctx w "first offset x" <*> arg ctx w "second offset y" <*> arg ctx w third
    bin w f = do
      _ <- keyword w
      s <- shapeArg ctx
      return (`f` s)
    -- `p |> loft z q`: start a loft from p (at z = 0), or extend one
    loftStage z q (Loft ps) = Loft (ps ++ [(z, q)])
    loftStage z q p = Loft [(0, p), (z, q)]

-- Parse boolean expressions (union, difference, hull, minkowski, offset)
-- Glyph operators in !glyph/legacy; '*' (union) and '-' (difference)
-- in !simple/legacy.
booleanExpression :: Ctx -> Parser Shape
booleanExpression ctx = do
  left <- attachExpression ctx
  rest <- many $ do
    op <- choice (gOps ++ sOps) <?> opLabel
    right <- attachExpression ctx
    return (op, right)
  foldM applyBooleanOp left rest
  where
    gOps = if glyphOK ctx then map symbol ["⊖", "⊝", "⊕", "⊛", "∩", "⇓", "⊞", "↯"] else []
    sOps = if simpleOK ctx then map symbol ["*", "-"] else []
    opLabel = case cMode ctx of
      ModeGlyph -> "an operator (⊕ ⊖ ∩ ⇓ ⊞ ↯ ⌖ ⋈ |>)"
      ModeSimple -> "an operator (* - |>)"
      ModeLegacy -> "an operator (⊕ ⊖ ∩ ⇓ ⊞ ↯ ⌖ ⋈ * - |>)"
    applyBooleanOp left ("⊖", right) = pure (Diff left right)
    applyBooleanOp left ("⊝", right) = pure (Diff left right)
    applyBooleanOp left ("-", right) = pure (Diff left right)
    applyBooleanOp left ("⊕", right) = pure (Union [left, right])
    applyBooleanOp left ("⊛", right) = pure (Union [left, right])
    applyBooleanOp left ("*", right) = pure (Union [left, right])
    applyBooleanOp left ("∩", right) = pure (Intersection [left, right])
    applyBooleanOp left ("⇓", right) = pure (Hull [left, right])
    applyBooleanOp left ("⊞", right) = pure (Minkowski [left, right])
    applyBooleanOp left ("↯", right) = case offsetValue right of
      Just r -> pure (Offset r left)
      Nothing -> fail "↯ offsets by the radius of its right operand: use a shape with one radius (● ⭘ △ ⬠, sphere/cyl/zcyl/xcyl/ycyl/tube/cone/prism), not a solid without one"
    applyBooleanOp _ (op, _) = error $ "Unknown boolean operator: " ++ op

-- | The offset amount a `↯` right operand donates: its one characteristic
-- radius. There is no silent default -- anything else is a parse error
-- naming the fix.
offsetValue :: Shape -> Maybe Double
offsetValue s = case s of
  Sphere r -> Just r
  Cylinder r _ -> Just r
  Cone r _ -> Just r
  Prism _ r _ -> Just r
  Cyl r _ _ _ -> Just r
  XCyl r _ -> Just r
  YCyl r _ -> Just r
  ZCyl r _ -> Just r
  Tube ro _ _ -> Just ro
  Shape2D _ r -> Just r
  _ -> Nothing

-- Parse attachment expressions: bind tighter than boolean ops.
--   a ⌖ top b   -- position: b's bottom snapped to a's top (translate only)
--   a ⋈ rt b    -- attach: b rotated so +Z points right, bottom mated to face
attachExpression :: Ctx -> Parser Shape
attachExpression ctx = do
  left <- transformExpression ctx
  rest <- many (choice steps)
  return $ foldl (flip ($)) left rest
  where
    steps
      | glyphOK ctx =
          [ do
              at <- srcPos
              _ <- symbol "⌖"
              v <- anchorVec
              right <- transformExpression ctx
              return (\l -> Position at v (0, 0, 0) l right),
            do
              at <- srcPos
              _ <- symbol "⋈"
              v <- anchorVec
              right <- transformExpression ctx
              return (\l -> AttachTo at v (0, 0, 0) l right)
          ]
      | otherwise = []

-- Parse transformation expressions
transformExpression :: Ctx -> Parser Shape
transformExpression ctx = (transformation ctx <?> tLabel) <|> primaryExpression ctx
  where
    tLabel = case cMode ctx of
      ModeGlyph -> "a transform (χ ψ ζ θ ϕ ω ⬈ ⇋ ⮕ ⟲ ⟰ ⚓ loft)"
      ModeSimple -> "a transform (Translate Rotate Scale Mirror Extrude Revolve Loft Anchor Hull Union Intersect Minkowski Offset)"
      ModeLegacy -> "a transform (χ ψ ζ θ ϕ ω ⬈ ⇋ ⮕ ⟲ ⟰ ⚓ loft, or Translate/Rotate/Scale/...)"

-- Parse transformations
transformation :: Ctx -> Parser Shape
transformation ctx =
  choice (glyphTs ++ wordTs ++ simpleTs)
  where
    -- loft z0 p0 z1 p1 [z2 p2 ...] : skin 2D profiles at ascending z
    loftPairs = do
      ps <- some ((,) <$> try (num ctx) <*> primaryExpression ctx)
      if length ps < 2
        then fail "loft needs at least two profiles: loft z0 profile0 z1 profile1 ..."
        else return (Loft ps)
    wordTs = [keyword "loft" *> loftPairs]
    glyphTs
      | glyphOK ctx =
          [ g1 "χ" Tx, g1 "ψ" Ty, g1 "ζ" Tz
          , g1 "θ" Rx, g1 "ϕ" Ry, g1 "ω" Rz
          , g3 "⬈" Scale, g3 "⇋" Mirror
          , g1 "⮕" Extrude
          , do
              _ <- symbol "⟲"
              a <- num ctx
              -- a second number is the segment count; otherwise the
              -- next primary is the profile (numbers are never shapes)
              isNum <- optional (lookAhead (try (num ctx)))
              case isNum of
                Nothing -> Revolve a <$> shapeArg ctx
                Just _ -> RevolveSeg a <$> revolveSegs ctx <*> shapeArg ctx
          , symbol "⟰" *> loftPairs
          , do
              at <- srcPos
              _ <- symbol "⚓"
              v <- anchorVec
              Anchor at v <$> shapeArg ctx
          ]
      | otherwise = []
    g1 s f = do
      _ <- symbol s
      n <- num ctx
      f n <$> shapeArg ctx
    g3 s f = do
      _ <- symbol s
      a <- num ctx
      b <- num ctx
      c <- num ctx
      f (a, b, c) <$> shapeArg ctx

    -- !simple word set: namespaced transforms + prefix combinators.
    --   Translate (x, y, z) obj      Translate.x n obj
    --   Rotate (x, y, z) obj         Rotate.z n obj    (rotate order: x, y, z)
    --   Scale (x, y, z) / Scale.x n  Mirror (x, y, z) / Mirror.x
    --   Extrude h obj   Revolve deg obj   Revolve deg segs obj   Anchor top obj
    --   Hull a b   Union a b   Intersect a b   Minkowski a b   Offset n a
    simpleTs
      | simpleOK ctx =
          [ ns "Translate" Translate [("x", Tx), ("y", Ty), ("z", Tz)]
          , ns "Rotate" rotXYZ [("x", Rx), ("y", Ry), ("z", Rz)]
          , ns "Scale" Scale [("x", \n -> Scale (n, 1, 1)), ("y", \n -> Scale (1, n, 1)), ("z", \n -> Scale (1, 1, n))]
          , nsMirror
          , keyword "Extrude" *> (Extrude <$> num ctx <*> shapeArg ctx)
          , do
              _ <- keyword "Revolve"
              a <- num ctx
              isNum <- optional (lookAhead (try (num ctx)))
              case isNum of
                Nothing -> Revolve a <$> shapeArg ctx
                Just _ -> RevolveSeg a <$> revolveSegs ctx <*> shapeArg ctx
          , keyword "Loft" *> loftPairs
          , srcPos >>= \at -> keyword "Anchor" *> (Anchor at <$> anchorVec <*> shapeArg ctx)
          , bin2 "Hull" (\a b -> Hull [a, b])
          , bin2 "Union" (\a b -> Union [a, b])
          , bin2 "Intersect" (\a b -> Intersection [a, b])
          , bin2 "Minkowski" (\a b -> Minkowski [a, b])
          , keyword "Offset" *> ((\n s -> Offset n s) <$> num ctx <*> shapeArg ctx)
          ]
      | otherwise = []
    rotXYZ (a, b, c) s = Rz c (Ry b (Rx a s))
    ns w tupleF axes = do
      _ <- keyword w
      choice
        ( [ try (symbol ("." ++ ax)) *> (axF <$> num ctx <*> shapeArg ctx)
          | (ax, axF) <- axes
          ]
            ++ [tupleF <$> tuple3 ctx <*> shapeArg ctx]
        )
    nsMirror = do
      _ <- keyword "Mirror"
      choice
        [ try (symbol ".x") *> (Mirror (1, 0, 0) <$> shapeArg ctx)
        , try (symbol ".y") *> (Mirror (0, 1, 0) <$> shapeArg ctx)
        , try (symbol ".z") *> (Mirror (0, 0, 1) <$> shapeArg ctx)
        , Mirror <$> tuple3 ctx <*> shapeArg ctx
        ]
    bin2 w f = do
      _ <- keyword w
      a <- primaryExpression ctx
      f a <$> shapeArg ctx

-- Parse primary expressions (shapes, variables, parentheses)
-- NOTE: shapes come before variables so keyword primitives (xcyl etc.)
-- are not swallowed by the variable parser.
primaryExpression :: Ctx -> Parser Shape
primaryExpression ctx =
  choice
    ( [parenthesized]
        ++ (if glyphOK ctx then [basicShape, bosl2Shape] else [])
        ++ [wordShape]
        ++ (if simpleOK ctx then [simpleShape] else [])
        ++ (if glyphOK ctx then [shape2D] else [])
        ++ [variable]
    )
    <?> pLabel
  where
    pLabel = case cMode ctx of
      ModeGlyph -> "a shape (■ ● ◎ ▻ ▬ ⎏ ▣ ◙ ⌭ ⌽ ⊚ ⏢ ◉ ⊿ △ ⬠ ⭘ ✎ or a word shape), a variable, or '('"
      ModeSimple -> "a shape (Sphere Cube Box Cylinder Cone Tube Torus Wedge Prismoid Circle Triangle Pentagon Bezier or a word shape), a variable, or '('"
      ModeLegacy -> "a shape (glyph or word), a variable, or '('"
    varTable = cVars ctx
    parenthesized = between (symbol "(") (symbol ")") (expression ctx)

    -- an unknown name is reported at the identifier itself, not after it
    variable = do
      o <- getOffset
      name <- identifier
      case Map.lookup name varTable of
        Just shape -> return shape
        Nothing
          | Map.member name (cNums ctx) -> setOffset o *> fail ("'" ++ name ++ "' is a number, not a shape (numbers are used as arguments: ● " ++ name ++ ", box " ++ name ++ " ...)")
          | otherwise -> setOffset o *> fail (undefPfx ++ "'" ++ name ++ "'" ++ suggestion (Map.keys varTable ++ reservedNames (cMode ctx)) name)

    -- !simple capitalized shape words
    simpleShape =
      choice
        [ keyword "Sphere" *> (Sphere <$> num ctx)
        , keyword "Cube" *> ((\s -> Cuboid (s, s, s) 0 0) <$> num ctx)
        , keyword "Box" *> ((\a b c -> Cuboid (a, b, c) 0 0) <$> arg ctx "Box" "width" <*> arg ctx "Box" "depth" <*> arg ctx "Box" "height")
        , keyword "Cylinder" *> ((\r h -> Cyl r h 0 0) <$> num ctx <*> num ctx)
        , keyword "Cone" *> (Cone <$> num ctx <*> num ctx)
        , keyword "Tube" *> (Tube <$> num ctx <*> num ctx <*> num ctx)
        , keyword "Torus" *> (Torus <$> num ctx <*> num ctx)
        , keyword "Wedge" *> ((\a b c -> Wedge (a, b, c)) <$> num ctx <*> num ctx <*> num ctx)
        , keyword "Prismoid" *> ((\a b c d h -> Prismoid (a, b) (c, d) h) <$> num ctx <*> num ctx <*> num ctx <*> num ctx <*> num ctx)
        , keyword "Circle" *> (Shape2D 100 <$> num ctx)
        , keyword "Triangle" *> (Shape2D 3 <$> num ctx)
        , keyword "Pentagon" *> (Shape2D 5 <$> num ctx)
        , keyword "Bezier" *> bezierBody ctx
        ]

    basicShape =
      choice
        [cube, sphere, cylinder, cone, rectangle, prism]

    cube = do
      _ <- symbol "■"
      size <- num ctx
      return $ Rectangle size size size

    sphere = do
      _ <- symbol "●"
      Sphere <$> num ctx

    cylinder = do
      _ <- symbol "◎"
      radius <- num ctx
      Cylinder radius <$> num ctx

    cone = do
      _ <- symbol "▻"
      radius <- num ctx
      Cone radius <$> num ctx

    rectangle = do
      _ <- symbol "▬"
      x <- arg ctx "box / ▬" "width"
      y <- arg ctx "box / ▬" "depth"
      Rectangle x y <$> arg ctx "box / ▬" "height"

    prism = do
      _ <- symbol "⎏"
      off <- getOffset
      n <- num ctx
      if n >= 3 && n <= fromIntegral maxSides && n == fromInteger (truncate n)
        then return ()
        else setOffset off *> fail ("prism side count must be a whole number from 3 to " ++ show maxSides)
      radius <- num ctx
      Prism (round n) radius <$> num ctx

    -- BOSL2 primitives (all centered, matching BOSL2 defaults) ----
    bosl2Shape =
      choice
        [ cuboidChamfer,
          cuboidRound,
          cylChamfer,
          cylRound,
          tubeShape,
          prismoidShape,
          torusShape,
          wedgeShape
        ]

    cuboidChamfer = do
      _ <- symbol "▣"
      x <- num ctx
      y <- num ctx
      z <- num ctx
      c <- num ctx
      return $ Cuboid (x, y, z) c 0

    cuboidRound = do
      _ <- symbol "◙"
      x <- num ctx
      y <- num ctx
      z <- num ctx
      r <- num ctx
      return $ Cuboid (x, y, z) 0 r

    cylChamfer = do
      _ <- symbol "⌭"
      r <- num ctx
      h <- num ctx
      c <- num ctx
      return $ Cyl r h c 0

    cylRound = do
      _ <- symbol "⌽"
      r <- num ctx
      h <- num ctx
      ro <- num ctx
      return $ Cyl r h 0 ro

    tubeShape = do
      _ <- symbol "⊚"
      ro <- num ctx
      ri <- num ctx
      Tube ro ri <$> num ctx

    prismoidShape = do
      _ <- symbol "⏢"
      x1 <- num ctx
      y1 <- num ctx
      x2 <- num ctx
      y2 <- num ctx
      Prismoid (x1, y1) (x2, y2) <$> num ctx

    torusShape = do
      _ <- symbol "◉"
      rj <- num ctx
      Torus rj <$> num ctx

    wedgeShape = do
      _ <- symbol "⊿"
      x <- num ctx
      y <- num ctx
      z <- num ctx
      return $ Wedge (x, y, z)

    xcylShape = do
      _ <- keyword "xcyl"
      r <- num ctx
      XCyl r <$> num ctx

    ycylShape = do
      _ <- keyword "ycyl"
      r <- num ctx
      YCyl r <$> num ctx

    zcylShape = do
      _ <- keyword "zcyl"
      r <- num ctx
      ZCyl r <$> num ctx

    -- Standard connection recipes and word shapes, available in every mode.
    standardPrimitive = choice [do
      _ <- keyword name
      xs <- mapM (arg ctx name) labels
      either fail pure (standardShape name xs)
      | (name, labels) <-
        [("thread_male", threadArgs), ("thread_female", threadArgs ++ ["radial clearance", "axial flank clearance"]),
         ("globe_stud", ["root radius","major radius","pitch","length"]),
         ("globe_socket", ["root radius","major radius","pitch","length","radial clearance","axial flank clearance"]),
         ("clearance_pin", ["shaft radius","shaft length","flange radius","flange height"]),
         ("clearance_bore", ["shaft radius","length","radial clearance"]),
         ("captive_bearing", ["shaft radius","span","stop radius","stop height","radial gap","axial gap","sleeve wall"])]]
    threadArgs = ["root radius","major radius","pitch","length","root width","crest width","facets per turn"]
    wordShape =
      choice
        [ standardPrimitive
        , xcylShape
        , ycylShape
        , zcylShape
        , keyword "cube" *> ((\s -> Cuboid (s, s, s) 0 0) <$> num ctx)
        , keyword "box" *> ((\a b c -> Cuboid (a, b, c) 0 0) <$> arg ctx "box" "width" <*> arg ctx "box" "depth" <*> arg ctx "box" "height")
        , keyword "sphere" *> (Sphere <$> num ctx)
        , keyword "cyl" *> ((\r h -> Cyl r h 0 0) <$> num ctx <*> num ctx)
        , keyword "tube" *> (Tube <$> num ctx <*> num ctx <*> num ctx)
        , keyword "frustum" *> (Frustum <$> num ctx <*> num ctx <*> num ctx)
        , keyword "pillar" *> (Cylinder <$> num ctx <*> num ctx)
        , keyword "spike" *> (Cone <$> num ctx <*> num ctx)
        , keyword "prism" *> prismWord ctx
        , keyword "block" *> (Rectangle <$> num ctx <*> num ctx <*> num ctx)
        , keyword "poly" *> polyBody ctx
        , keyword "torus" *> (Torus <$> num ctx <*> num ctx)
        , keyword "wedge" *> ((\a b c -> Wedge (a, b, c)) <$> num ctx <*> num ctx <*> num ctx)
        ]

    shape2D =
      choice
        [triangle, pentagon, circle, bezier]

    bezier = symbol "✎" *> bezierBody ctx

    triangle = do
      _ <- symbol "△"
      Shape2D 3 <$> num ctx

    pentagon = do
      _ <- symbol "⬠"
      Shape2D 5 <$> num ctx

    circle = do
      _ <- symbol "⭘"
      Shape2D 100 <$> num ctx

-- | Shared body for ✎ / Bezier: 3k+1 control points as x y pairs
-- | Word prism: the bed-anchored n-sided prism, the same side-count
-- rules as the glyph.
prismWord :: Ctx -> Parser Shape
prismWord ctx = do
  off <- getOffset
  n <- num ctx
  if n >= 3 && n <= fromIntegral maxSides && n == fromInteger (truncate n)
    then return ()
    else setOffset off *> fail ("prism side count must be a whole number from 3 to " ++ show maxSides)
  r <- num ctx
  Prism (round n) r <$> num ctx

-- | Shared body for the poly word: x y pairs, at least three points, a
-- plain closed polygon (holes have no word spelling).
polyBody :: Ctx -> Parser Shape
polyBody ctx = do
  ns <- some (num ctx)
  let n = length ns
  if odd n || n < 6
    then fail ("poly needs at least three points as x y pairs (got " ++ show n ++ " numbers" ++ (if odd n then ", one is unpaired" else "") ++ ")")
    else return (Poly (PD (pairUp ns) []))
  where
    pairUp (a : b : r) = (a, b) : pairUp r
    pairUp _ = []

bezierBody :: Ctx -> Parser Shape
bezierBody ctx = do
  ns <- some (num ctx)
  let n = length ns
  if odd n || n < 8 || (n `div` 2) `mod` 3 /= 1
    then fail ("bezier needs 3k+1 control points as x y pairs (got " ++ show n ++ " numbers)")
    else return (bezPoly 24 (pairUp ns))
  where
    pairUp (a : b : r) = (a, b) : pairUp r
    pairUp _ = []
