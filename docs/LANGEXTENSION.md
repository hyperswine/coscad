# CoScad language reference

One definition per line: `name = expression`. `main` is rendered.
Definitions resolve by dependency, not order; `//` line comments; a line
beginning with `|>` continues the previous definition. Glyphs take
numeric arguments (`● 15` is the readable style; `●15` and `●(15)`
also work). Parens group expressions.

## Shapes

Plain OpenSCAD (corner/bottom-anchored, as OpenSCAD anchors them):

| syntax | meaning |
|---|---|
| `■ s` | cube s³ |
| `▬ x y z` | box |
| `● r` | sphere |
| `◎ r h` | cylinder (z 0..h) |
| `▻ r h` | cone |
| `⎏ n r h` | n-sided prism |

BOSL2 family (**all centered**; emits `include <BOSL2/std.scad>`):

| syntax | meaning |
|---|---|
| `▣ x y z c` | cuboid, chamfer c (0 = none) |
| `◙ x y z r` | cuboid, rounding r — must be < half the smallest dim or OpenSCAD errors at render |
| `⌭ r h c` / `⌽ r h r2` | cyl with chamfered / rounded ends |
| `xcyl r l`, `ycyl`, `zcyl` | axis-aligned cylinders |
| `⊚ or ir h` | tube |
| `⏢ x1 y1 x2 y2 h` | prismoid (emitted with `anchor=CENTER`) |
| `◉ R r` | torus |
| `⊿ x y z` | wedge — profile in the **YZ plane**, right angle at (−Y,−Z); reorient with rotations |

Word shapes (centered, pipeline-friendly): `cube s`, `box x y z`,
`sphere r`, `cyl r h`, `tube or ir h`, `torus R r`, `wedge x y z`.

2D + curves: `△ ⬠ ⭘` profiles; `✎ x0 y0 x1 y1 ...` — piecewise cubic
bezier outline, 3k+1 control points as bare pairs, evaluated in the
compiler to a plain closed polygon (24 samples/segment). Extrude with
`⮕ h shape` or `|> extrude h`.

## Numbers

A definition whose right-hand side is arithmetic is a numeric binding,
usable wherever a number is expected:

```
w = 20
t = w / 5 - 1          // + - * / and parens; numbers may reference numbers
main = box w (w / 2) t |> cutat top 0 0 (-t / 2) (zcyl 1 50)
```

Argument positions take a single atom — a literal, a name, `-name`, or a
parenthesized expression — so `box w h t` stays unambiguous; write
`(w / 2)` for arithmetic inline. Numbers are resolved before shapes and
can only depend on numbers. Using a number where a shape is expected (or
vice versa) is a compile error that names the binding. Numeric bindings
and arguments must be finite; non-finite results from division by zero or
overflow are compile errors.
In argument positions, unary minus must touch its operand (`-2`, `-w`,
`-(w / 2)`). `box w - 2 3` is rejected: write `box (w - 2) 3 4` for
subtraction. Spaces around minus remain valid in numeric bindings and
inside parentheses.

## Lofts

`loft z0 p0 z1 p1 [z2 p2 ...]` (glyph `⟰`, `Loft` in !simple) skins 2D
profiles at ascending z into one solid via BOSL2 `skin()`. The
compiler picks `method="reindex"` when every profile has the same
vertex count and `method="distance"` otherwise, so circle-to-triangle
is fine — but note "distance" is an O(n·m) search in the OpenSCAD
interpreter: two 100-point circles take ~30 s, a circle and a triangle
are instant. Keep mismatched-count lofts to small profiles, or match
counts (e.g. `⭘` to `⭘`). Pipeline form: `p0 |> loft z1 p1` starts a
loft with `p0` at z = 0; further `|> loft z p` stages append profiles.

Profiles must be single closed outlines: the 2D primitives, `✎`
beziers, and in-plane transforms of them (`χ ψ ω ⬈ ⇋`, `↯` offset,
anchors). Vector translations must have zero Z. Mirror normals must
lie in XY or point along Z (reflection in XY leaves the profile unchanged);
zero normals are rejected. Booleans between profiles, `ζ`, and `θ`/`ϕ` rotations are
rejected at compile time. Profiles are emitted as BOSL2 path
expressions (`circle()`, `move(p=)`, `zrot(p=)`, `offset()`), never as
modules.

## Booleans (left-assoc, one precedence level)

`⊕`/`⊛` union · `⊖`/`⊝` difference · `∩` intersection · `⇓` hull ·
`⊞` minkowski · `↯` offset (offset amount is the RIGHT operand's
characteristic radius: sphere/cylinder/2D-profile only — anything else
silently defaults to 1.0).

## Transforms (prefix, apply to the next primary expression)

`χ ψ ζ` translate x/y/z · `⟰`/`loft` (see Lofts above) · `θ ϕ ω` rotate about x/y/z (degrees, about
the **origin** — rotated cut geometry gets pulled toward the origin;
compose inner translates or verify numerically) · `⬈ sx sy sz` scale ·
`⇋ nx ny nz` mirror · `⚓ anchor` re-origin at own anchor.

## Pipelines

`expr |> stage |> stage ...` — binds loosest. Stages:

- `x d`, `y d`, `z d`, `move dx dy dz`, `rotx/roty/rotz a`,
  `scale sx sy sz`, `mirror nx ny nz`, `extrude h`, `anchor v`
- `add s`, `cut s`, `isect s`, `hull s`, `mink s`
- `at v [dx dy dz] s` · `on v [dx dy dz] s` · `cutat v [dx dy dz] s`
  (see docs/TOPOLOGICAL.md — `at`/`on` mate the child's opposite
  anchor; `cutat` centers the cutter)

Anchor vocabulary: `top bot lft rt fwd bak ctr` (+ aliases
up/down/left/right/front/back/center), combinable: `top+rt`, `lft+fwd`.
Choose at most one direction per axis: opposing directions, repeated
aliases (`top+up`), and combinations with `ctr`/`center` are errors.
`ctr` alone is valid.

Shape and prefix-transform keywords active in the selected syntax mode
are reserved as definition names (`cube`, `loft`, and, in legacy/simple
mode, `Box`, `Translate`, etc.). Pipeline-only stages and anchor words
can still be bindings: `x = 2`, `top = 3`. Keyword prefixes such as
`cube2` are ordinary names. Capitalized simple-mode keywords are ordinary
names in `!glyph` mode.

## Named datums, placement and grids

These word pipeline stages work in legacy, `!glyph` and `!simple` modes.

| Stage | Meaning |
|---|---|
| `s \|> datum name ref [dx dy dz]` | Save a construction point at `ref` on the shape built so far; emit the same solid |
| `s \|> anchor @name` | Move a saved point to the origin |
| `child \|> place parent target own [dx dy dz]` | Translate `own` on the child to `target` on the parent, plus offset; return only the child |
| `s \|> grid columns rows pitch_x pitch_y` | Union copies at `(i*pitch_x, j*pitch_y, 0)`, starting with the original at `(0,0,0)` |
| `s \|> hull` | Convex hull of the whole piped group; `s \|> hull other` keeps its binary meaning |

`ref`, `target` and `own` accept the usual bounding-box anchor words or
`@name`. Named points form a separate namespace from bindings. Parent
expressions with pipelines or booleans need parentheses. Offsets are optional
but must contain all three numbers when present; they use the current XYZ
axes. Placement translates only, so rotate the child beforehand if needed.
The parent is validated even though it is not emitted. It can have a different
dimension from the child: it supplies a point, while the result has the
child's dimension.

```
plate = box 40 30 4 |> datum hole top+lft 8 0 0
blank = plate |> at rt (box 20 10 4)
tool = zcyl 2 6 |> place blank @hole ctr 0 0 -2
main = blank |> cut tool
```

The saved hole follows its plate through translations, rotations, scales,
mirrors and attachments. Adding the ear does not recalculate it against the
new bounding box. Datums are construction points, not automatically tracked
surfaces or oriented mating frames. Booleans retain points from all operands,
including cutters; removing material does not remove a datum. Extrusion keeps
profile points on their original plane; loft places each profile's points at
its specified Z layer. Datums do not change bounds, dimensions or output solids.

Defining a name already present on that shape is an error. Combining or
repeating shapes can produce several points with the same name; selecting
such a name is an ambiguity error. Select/place individual features before
combining them, or give the result a new datum name. A missing name is an
error listing the available names.

Grid counts must be whole numbers from 1 to 10000, with at most 10000 copies
and at most 10000 expanded primitive leaves, including nested grids. Pitches
must be finite and nonzero on repeated axes; negative pitches are allowed.
A one-copy axis may use zero pitch. Copies retain the input's orientation and
dimension. Use grids for features inside a part; assembly instances still use
`.assemble` part counts/poses and their distinct identities.

See [datum-plate.coscad](../examples/topological/datum-plate.coscad) for a
mounting grid positioned against a saved corner datum.

## .assemble / coscad next

See docs/MANUFACTURING.md.
