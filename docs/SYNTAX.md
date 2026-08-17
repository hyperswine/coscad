# CoScad syntax modes

A pragma on the first code line selects the vocabulary:

```
!simple    -- ASCII word set only
!glyph     -- glyph set only
(nothing)  -- legacy: both accepted (all existing files unchanged)
```

The pragma line is blanked, not stripped, so error line numbers stay right.
`.assemble` files always resolve in legacy mode.

## Available everywhere (any mode)
- `|>` pipelines and all pipe-stage words (`x`, `y`, `z`, `rotx`, `move`,
  `scale`, `mirror`, `anchor`, `at`, `on`, `cutat`, `add`, `cut`, `isect`,
  `hull`, `mink`)
- `$` — Haskell-style loose application: every prefix form's shape argument
  may be `$ <rest of expression>`. Works after glyph transforms (`χ 5 $ a ⊕ b`),
  word transforms (`Translate.x 5 $ a * b`), and pipe stages (`|> cut $ ...`)
- lowercase primitive words: `xcyl ycyl zcyl cube box sphere cyl tube torus wedge`
- anchor words: `top bot lft rt fwd bak ctr` (+ combos like `top+rt`)

## !simple vocabulary
Transforms (namespace style):
```
Translate (x, y, z) obj        Translate.x n obj   (.y .z likewise)
Rotate (x, y, z) obj           Rotate.z n obj      (tuple order: x then y then z)
Scale (x, y, z) obj            Scale.x n obj
Mirror (x, y, z) obj           Mirror.x obj        (no number)
Extrude h obj                  Anchor top obj
```
Combinators (prefix, two shape args; second may be `$`):
```
Hull a b    Union a b    Intersect a b    Minkowski a b    Offset n a
```
Operators: `*` union, `-` difference. Same flat left-assoc precedence as the
glyph operators: `a - b * c` is `(a - b) * c` — parenthesize when mixing.

Shapes: `Sphere r`, `Cube s`, `Box x y z`, `Cylinder r h`, `Cone r h`,
`Tube ro ri h`, `Torus rj rm`, `Wedge x y z`, `Prismoid x1 y1 x2 y2 h`,
`Circle r`, `Triangle r`, `Pentagon r`, `Bezier x y x y ...` (3k+1 points).
All centered (BOSL2 family). Note `Box` is centered, unlike legacy `▬`.

## Known limits / notes
- Keywords win over identifiers: you can't name a variable exactly `Translate`
  (but `Translate2`, `myTranslate` are fine — same rule as `xcyl` today).
- Mode errors currently surface as the generic "Cannot resolve variables"
  message; per-line diagnostics would need the resolver to keep parse errors.
- `Rotate (x, y, z)` composes as Rz . Ry . Rx (OpenSCAD rotate([x,y,z]) order).
- `Translate (x,y,z)` emits one translate() vs three nested for χψζ chains —
  same geometry, cleaner scad.

# Assembly checking (`coscad check foo.assemble`)

`.assemble` files now take the same `!simple` / `!glyph` pragma (governs the
asm-side expressions; each referenced part file keeps its own pragma), plus:

```
clearance 0.15        -- required minimum gap in mm (0 = report contacts only)
```

`coscad check` isolates each part inside the asm expression (all other parts
bound to Empty), renders real meshes, splits instances (a mirrored pair =
wing#1, wing#2), and for each AABB-close pair:
- exact overlap via OpenSCAD boolean on emitted polyhedra -> `OVERLAP` + volume
- CGAL instability on coplanar mating faces is detected (stderr+stdout scan),
  retried with swapped operands, else falls back to a grid-accelerated
  vertex-to-triangle distance verdict (annotated in the output)
- `contact` = touching mating faces (< 0.02mm); `WARN` = gap below clearance
Exit code 1 iff any OVERLAP. Distance verdicts are vertex-sampled: exact for
vertex-involved closest features; pathological edge-edge-only cases can read
slightly high.
