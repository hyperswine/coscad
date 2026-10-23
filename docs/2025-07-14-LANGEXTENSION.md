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
  (see docs/2026-07-24-TOPOLOGICAL.md — `at`/`on` mate the child's opposite
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

## Oriented mating frames

`|> frame name ref normal up [dx dy dz]` saves a point and a right-handed
orientation on a **3D solid**. `ref` is a box anchor or `@name`; offsets use
the current XYZ axes. `normal` and `up` use direction words, including
combinations such as `top+rt`. The normal becomes local +Z; up is projected
onto the face plane and normalized to become local +Y. Neither direction
may be zero, and they must not be parallel. Local +X completes the frame.

```
plate = box 40 30 4 |> frame socket top top bak
post = box 6 10 20 |> frame foot bot bot bak
main = post |> mate plate @socket @foot 90 0.2
```

`child |> mate parent @target @own [roll gap]` returns only the rigidly
transformed child. Frame origins coincide at zero gap, their normals oppose,
and their up axes agree at zero roll. Roll is in degrees, right-handed about
the parent's outward normal; gap is in mm along that normal. Both optional
numbers must be supplied together; both default to zero. Parent expressions
with transforms or booleans need parentheses. Parent and child must be 3D.

Frame names share the datum namespace. A frame's origin also works with
`place` and `anchor @name`; a point-only datum cannot be used for `mate`.
Frames retain their original feature locations through CSG and transforms.
Repeated names are ambiguous. Scaling axes is allowed if the transformed
axes remain orthogonal and right-handed; axis lengths are normalized when
selected. Reflections, degenerate axes and skewed frames are rejected on
selection: define a fresh frame **after** the geometry transform instead.
Frames are explicit construction metadata, not automatically recognized
faces or guarantees that mating surfaces fit or remain after a cut.

See [mating-corner.coscad](../examples/topological/mating-corner.coscad)
for upright and sideways mates with explicit up directions.

## Rigid assembly constraints

A `.assemble` can generate its assembled view from `fixed` and `mate`
statements instead of an `asm = ...` expression:

```
base ← base.coscad ×1
left_post ← post.coscad ×1
right_post ← post.coscad ×1
fixed base
mate left_post @foot base @left_mount
mate right_post @foot base @right_mount roll=90 gap=0
```

Each declaration names one physical instance. Reusing a part file is fine;
use a distinct alias for every copy and `×1` to print it or `×0` for a premade
instance. Counts above one are rejected in constrained assemblies because
they do not identify individual poses. A nested assembly acts as one rigid
instance. Each connected component needs at least one fixed instance.

A constrained subassembly keeps its instances apart: each placed instance
carries its alias, so the parent can address one copy of a frame with a
dotted path, `@alias.frame`. With `sub.assemble` placing `p` and `q` (both
from `link.coscad`, which has frames `a` and `b`):

```
s ← sub.assemble ×1
l ← link.coscad ×1
fixed s
mate l @a s @q.b        // @b alone is ambiguous: 2 copies
```

Paths go through as many levels as needed (`@s.q.b` from a grandparent), and
a shorter path works when it is unique. An unqualified frame that exists in
several instances is still an error; the message lists the qualified choices
(`@p.b, @q.b`). Only constraint-generated assemblies tag their instances; a
manual `asm = …` expression does not.

`fixed instance [x y z rx ry rz]` defaults to the identity pose. If provided,
all six numbers are required: mm translation and XYZ rotation in degrees (X,
then Y, then Z). Each pose number, and the `roll=degrees` / `gap=mm` mate
options, is one numeric atom, as in `.coscad` argument position: a literal,
a numeric binding of the `.assemble` file, `-atom`, or parenthesized
arithmetic. Bindings may be defined anywhere in the file:

```
w = 10
half = w / 2
fixed lid (w - 1) -w 0 0 0 45
mate hinge @pin lid @hole roll=(half * 9) gap=-0.2
```

Arithmetic outside parentheses (`w+1`) is refused with a hint, and so are
`^` and `**`, which are not operators (write `w * w`). An undefined
name, a part name used as a number, or a non-finite value (`(1 / 0)`) is an
input error at its token.
`mate child @own parent @target` uses the same face convention as the pipeline.
Statements and declaration order do not determine solving order: poses
propagate in either direction from fixed roots. Every declared instance is
included exactly once in the generated `asm`.

Multiple fixed roots and closed loops are accepted only if all constraints
agree, by default within a per-component translation tolerance of 0.000001 mm
and a matrix-entry orientation tolerance of 0.00000001. `tolerance <mm>
[<rotation>]` changes both for the file (the rotation tolerance keeps its
default when omitted), e.g. `tolerance 0.00001` for a loop whose fixed poses
are typed to five decimals. The error for a loop that does not close reports
its residuals next to the tolerance in force. A contradiction is reported as the whole
loop (or the chain between two fixed roots that disagree): the error sits on
the constraint declared last on it, the line that closed the loop, with the
position and orientation residuals, and every constraint on the loop follows
as a `  note: file:line:col: …` line, since any of them may be the wrong one.
A floating component is reported at its part declaration. Contradictions,
floating components and duplicate fixed poses are design errors (exit 2);
the rest below are input errors (exit 1). Floating components, missing instances,
missing or ambiguous frames, duplicate fixed poses, unknown/duplicate options
and mixed manual `asm`/constraint definitions are errors. Graph size is
bounded to 10000 parts and 10000 constraints.

This first solver handles rigid mates with all six degrees of freedom fixed.
It does not solve hinges, sliders, distances, flexible parts, contacts or
collision avoidance. Use `coscad check` to test physical interference.
Existing `fastener` statements still describe hardware and planning joints;
geometric mates do not invent fasteners or assembly precedence. Design,
checker and planner views share the resolved poses. Manufacturing validates
the same constraints, then orients and packs the original parts for printing.

See [frame-stand.assemble](../examples/assemble/constraints/frame-stand.assemble)
for two copies of a post, one rolled 90 degrees.

## .assemble / coscad next

See docs/2026-07-24-MANUFACTURING.md.
