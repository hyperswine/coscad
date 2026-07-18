---
name: coscad
description: Use this skill whenever the user wants to write, edit, or convert .coscad files, or mentions CoScad, CoScad glyphs, or the CoScad compiler. CoScad is a glyph-based Haskell DSL that compiles to OpenSCAD (optionally BOSL2), covering solid primitives, booleans, transforms, and a bounding-box-based attachment/joining system. Use this instead of writing raw OpenSCAD when the user's project already uses CoScad, when they ask for CoScad specifically, or when a .coscad file is present/referenced. Do NOT use this for plain OpenSCAD/BOSL2 requests with no CoScad mention — write .scad directly for those.
license: N/A — project-internal tool, not third-party licensed software.
---

# CoScad

## What this is

CoScad is a small glyph-syntax language, implemented as a Haskell compiler
(`Main.hs` + `Lib.hs`), that parses `.coscad` files and emits `.scad`
(OpenSCAD) source. One variable, `main`, is the rendered shape. Every other
variable is a named sub-expression you can reuse.

The compiler is not a published package — it's a project-specific tool.
Treat `Main.hs`/`Lib.hs` as the source of truth over this document if they
ever disagree; this file can drift, the compiler cannot.

## Before writing any .coscad

1. Locate the compiler (`Main.hs`, `Lib.hs`) and the `BOSL2/` checkout —
   ask for their paths or check the conversation/project files if not
   obvious. There is no installed `coscad` binary by default; it must be
   built with GHC (`ghc --make Main.hs -o coscad`, needs `megaparsec`,
   `directory`, `filepath` — on Debian/Ubuntu: `apt-get install ghc
   libghc-megaparsec-dev`).
2. `BOSL2/` must be a sibling of (or symlinked next to) the `.scad` output
   file, since generated files that use BOSL2 shapes emit
   `include <BOSL2/std.scad>` as a relative path.
3. Build once per session, then convert with `./coscad file.coscad` →
   produces `file.scad` next to it. Non-zero exit / stderr message means
   a parse error (megaparsec's error, often points at the exact glyph).

## Core syntax rules

- One definition per line: `name = expression`.
- `main` must be defined — it's the only thing rendered.
- Forward references are fine; variables resolve by dependency, not by
  order. Circular references fail with a clear error.
- `//` line comments only, no block comments.
- Whitespace-separated glyph + numeric arguments, e.g. `● 15` (sphere,
  radius 15), not `●(15)` or `●15`.
- Parens group sub-expressions: `χ 5 (● 3 ⊕ ◎ 2 10)`.
- All shapes are 3D solids or (for the small 2D set) profiles meant to be
  extruded — there's no 2D-only pipeline beyond that.

## Full glyph reference

### Basic shapes (plain OpenSCAD, no BOSL2 needed)
| Glyph | Args | Meaning |
|---|---|---|
| `■ s` | size | cube s×s×s |
| `▬ x y z` | dims | rectangular box |
| `● r` | radius | sphere |
| `◎ r h` | radius height | cylinder |
| `▻ r h` | radius height | cone (tapers to 0) |
| `⎏ n r h` | sides radius height | n-sided prism |

### 2D profiles (extrude with `⮕`)
| Glyph | Args | Meaning |
|---|---|---|
| `△ r` | radius | triangle profile |
| `⬠ r` | radius | pentagon profile |
| `⭘ r` | radius | circle profile |

### BOSL2 shapes (auto-adds the BOSL2 include; all centered at origin)
| Glyph | Args | Meaning |
|---|---|---|
| `▣ x y z c` | dims, chamfer | cuboid, chamfer 0 = none |
| `◙ x y z r` | dims, rounding | cuboid, rounding 0 = none |
| `⌭ r h c` | radius height chamfer | centered cyl, chamfered ends |
| `⌽ r h r2` | radius height rounding | centered cyl, rounded ends |
| `xcyl r l` | radius length | cylinder along X axis |
| `ycyl r l` | radius length | cylinder along Y axis |
| `zcyl r l` | radius length | cylinder along Z axis |
| `⊚ or ir h` | outer-r inner-r height | tube |
| `⏢ x1 y1 x2 y2 h` | base dims, top dims, height | prismoid |
| `◉ rmaj rmin` | major-r minor-r | torus |
| `⊿ x y z` | dims | wedge — right-angle triangular prism. **Profile is in the YZ plane, right angle at (−Y,−Z)**; rotate/mirror to reorient (see Gotchas) |

`xcyl`/`ycyl`/`zcyl` are keyword-style (lowercase word, not a glyph) —
matched so `xcyl2` etc. still parses as an ordinary variable name.

### Booleans (all left-associative, same precedence, chain freely)
| Glyph | Alt | Meaning |
|---|---|---|
| `⊕` | `⊛` | union |
| `⊖` | `⊝` | difference (left minus right) |
| `∩` | — | intersection |
| `⇓` | — | convex hull |
| `⊞` | — | minkowski sum |
| `↯` | — | offset (offsets left by the right operand's characteristic radius — sphere/cylinder radius or 2D-profile radius) |

### Transforms (apply to the *next* primary expression only — wrap
multi-step chains in parens if precedence bites)
| Glyph | Args | Meaning |
|---|---|---|
| `χ d shape` | distance | translate X |
| `ψ d shape` | distance | translate Y |
| `ζ d shape` | distance | translate Z |
| `θ deg shape` | degrees | rotate about X |
| `ϕ deg shape` | degrees | rotate about Y |
| `ω deg shape` | degrees | rotate about Z |
| `⬈ sx sy sz shape` | scale factors | non-uniform scale |
| `⇋ nx ny nz shape` | plane normal | mirror across plane through origin |
| `⮕ h shape` | height | linear_extrude a 2D profile |
| `⚓ anchor shape` | anchor vector | re-origin shape at its own anchor point |

### Attachment / joining (see below for the model)
| Glyph | Form | Meaning |
|---|---|---|
| `⌖` | `parent ⌖ anchor child` | position: translate child so its opposite anchor meets parent's anchor point |
| `⋈` | `parent ⋈ anchor child` | attach: rotate child's +Z to point along anchor, then mate its bottom face there |

Anchor vectors: `top bot lft rt fwd bak ctr` (aliases `up`/`down`/
`left`/`right`/`front`/`back`/`center` also work), combinable with `+`,
e.g. `top+rt` for a top-right edge. `⌖`/`⋈` bind tighter than the
boolean ops, so `a ⌖ top b ⊕ c` parses as `(a ⌖ top b) ⊕ c`.

## The attachment model — how it actually works

BOSL2's real `attach()` only works on attachable modules, so a union or
difference can't be a parent or child. CoScad sidesteps this: anchors
are computed **in the Haskell compiler**, from the axis-aligned
bounding box of the *fully resolved* shape tree (unions merge boxes,
diffs keep the positive's box, hulls/minkowski propagate through,
rotations transform the 8 corners). `⌖`/`⋈`/`⚓` are desugared into
plain `translate`/`rotate` before any OpenSCAD is emitted — so the
generated `.scad` never contains BOSL2 attachment calls, and *any*
CoScad expression, including composites you built with `⊕`/`⊖`, has
usable anchors and can be attached to again.

Practical consequence: anchors on a composite are **bounding-box**
anchors, not surface anchors. `(L_shaped_bracket) ⋈ bak stud` anchors
at the bbox's back face at bbox mid-height, which may be empty air on
an irregular shape. When that matters, attach to the sub-part *before*
unioning it into the composite, or nudge afterward with `χ`/`ψ`/`ζ`.

## Known gotchas (verified by hand, worth re-checking after any edit)

- **`⊿` wedge orientation**: profile is in the YZ plane, not XZ — a
  bare `⊿ x y z` has its right-angle edge along Y at -Y,-Z. Confirmed
  by rendering a solo wedge and checking its OpenSCAD-computed
  centroid offset before trusting an assumed orientation in a
  composite. Get this wrong and volumes silently come out ~5-10% off
  with no error — always sanity-check volume/bounds after using `⊿`.
- **BOSL2 rounding limit**: `rounding` on `◙`/`⌽` must be less than
  half the smallest relevant dimension or OpenSCAD hard-errors at
  render time — CoScad does not validate this at compile time, so a
  thin panel (e.g. 2.5mm thick) with rounding 2 will fail; use
  rounding ≤ ~1 for thin stock.
- **`ζ` vs `ψ`**: easy to mistype/misremember since both are
  translate-family glyphs on nearby axes — `ψ` is Y, `ζ` is Z. (This
  was actually swapped once in an earlier version of Lib.hs and
  produced two Y-translates; if edits to Lib.hs are ever needed,
  re-verify `Tz`/`Ty` map to `ζ`/`ψ` correctly, not by naming
  convention alone.)
- **No per-edge chamfer/rounding.** `▣`/`◙` chamfer or round *all*
  edges uniformly; there's no way to chamfer just one edge in the
  current language. If a design needs that, either accept the
  uniform version or drop to raw BOSL2 in the emitted `.scad` by hand.
- **`↯` offset value** comes from `getOffsetValue` on the *right*
  operand: it reads a sphere or cylinder's radius, or a 2D profile's
  radius, defaulting to `1.0` for anything else — so `shape ↯ (● 2)`
  offsets by 2, but `shape ↯ (■ 2)` silently offsets by the 1.0
  default, not 2, since cube isn't handled. Use a sphere/cylinder/2D
  profile as the right operand of `↯`, never a cube/rectangle.

## Workflow: verify, don't eyeball

Rendered preview images of small/thin features are frequently
ambiguous or fail to load in-session. The reliable verification loop:

```bash
./coscad file.coscad                       # .coscad -> .scad
xvfb-run -a openscad -o file.stl file.scad  # .scad -> .stl, headless
python3 -c "
import trimesh
m = trimesh.load('file.stl')
print('watertight:', m.is_watertight)
print('volume:', m.volume, '| bounds:', m.bounds.round(2).tolist())
"
```

- `watertight: False` almost always means two faces are exactly
  coplanar/tangent (e.g. a hole's flat end sitting exactly on another
  cut's boundary) — nudge one surface by ~0.1-0.5mm of intentional
  overlap rather than leaving them touching.
- Cross-check volume/bounds against hand-calculated expectations
  (stack heights should sum, symmetric parts should have zero
  centroid offset on the symmetry axes, etc.) — this is what catches
  wrong-axis mistakes like the wedge orientation issue above.
- For comparing two versions of "the same part" (e.g. hand-written
  BOSL2 vs. CoScad output), a boolean symmetric difference
  (`difference(){A;B;} `+`difference(){B;A;}`, or just check both
  volume and centroid match) is a strong equivalence check — near-zero
  residual volume confirms they're the same solid, not just
  similar-looking.
- For fit/tolerance parts (jigs, channels, slots), render the part
  intersected with a nominal mating solid (expect ~0 interference
  volume) and again with that solid shifted by the tolerance amount
  (expect nonzero interference) — confirms the fit is neither too
  loose nor colliding, without needing to physically print it first.

## Style conventions worth following

- Give named intermediate variables real names (`plate`, `web`,
  `m5_hole`) rather than inlining everything into `main` — matches
  existing project files and keeps diffs/edits localized.
- Comment nonobvious dimensions inline, especially anything derived
  from a real-world constraint (rail slot centerlines, tolerance
  values, screw clearance diameters) so the reasoning survives edits.
- Prefer the BOSL2-shape glyphs (`▣`/`◙`/`⌭`/`⌽`/`xcyl` etc.) over the
  plain-OpenSCAD ones (`■`/`◎`) whenever chamfer, rounding, or
  axis-aligned cylinders are relevant — mixing both is fine, but
  default to BOSL2 shapes for anything mechanical/printable since
  they carry more useful parameters.
