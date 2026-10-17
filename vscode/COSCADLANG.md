# CoScad Language Reference

CoScad compiles `.coscad` part descriptions and `.assemble` manufacturing
assemblies to OpenSCAD, STL, and print-bed artifacts. This is the editor's
compact reference; the compiler's docs (docs/LANGEXTENSION.md,
docs/TOPOLOGICAL.md, docs/MANUFACTURING.md) are authoritative.

## Syntax modes

The first non-comment code line can select a syntax mode: `!glyph` or
`!simple`. Without a pragma, legacy mode accepts both families. `$` loose
application and `|>` pipelines work in every mode.

## Part files

Part files contain line-oriented definitions and require `main` as the
render entry. Definitions are order-independent; circular and undefined
references are errors.

```coscad
!glyph
base = ▣ 40 40 10 1
post = ⌽ 5 26 2
main = base ⋈ rt post
  |> at top+rt 0 0 2 (⌭ 3 8 0.5)
  |> cutat top 0 0 -2 (zcyl 2.15 30)
```

### Numbers

A definition whose right-hand side is arithmetic is a numeric binding.
Numbers may reference numbers, in any order, and are resolved before shapes:

```coscad
w = 20
t = w / 5 - 1          // + - * / and parens; unary minus
main = box w (w / 2) t
  |> cutat top 0 0 (-t / 2) (zcyl 1 50)
```

Argument positions take one atom: a literal, a name, `-name`, or a
parenthesized expression, so `box w h t` stays unambiguous. Using a number
where a shape is expected (or the reverse) is an error naming the binding.

### Glyph primitives

- Basic 3D: `■ size`, `● radius`, `◎ radius height`, `▻ radius height`,
  `▬ x y z`, `⎏ sides radius height`
- BOSL2 (centered): `▣ x y z chamfer`, `◙ x y z rounding`, `⌭ r h chamfer`,
  `⌽ r h rounding`, `xcyl/ycyl/zcyl r l`, `⊚ ro ri h`, `⏢ x1 y1 x2 y2 h`,
  `◉ major minor`, `⊿ x y z`
- 2D: `△ r`, `⬠ r`, `⭘ r`, `✎ x1 y1 ...` (bezier, 3k+1 control points)
- Word aliases in every mode: `cube`, `box`, `sphere`, `cyl`, `tube`,
  `torus`, `wedge`

### Transforms and operations

- Translate `χ ψ ζ`, rotate `θ ϕ ω`, scale `⬈`, mirror `⇋`, extrude `⮕`,
  anchor `⚓`
- Union `⊕`/`⊛`, difference `⊖`/`⊝`, intersection `∩`, hull `⇓`,
  Minkowski `⊞`, 2D offset `↯`
- Position `⌖` and attach `⋈`

Transforms take numeric arguments followed by a primary expression; use
parentheses or `$` for compound arguments: `main = χ 10 $ a ⊕ b`.

### Lofts

`loft z0 p0 z1 p1 [...]` (glyph `⟰`, `Loft` in simple mode) skins 2D
profiles at ascending z into one solid through BOSL2 `skin()`. Profiles
may differ in vertex count (circle to triangle is fine). The pipeline form
`p0 |> loft z1 p1` starts a loft with `p0` at z = 0 and further `|> loft`
stages append profiles.

```coscad
r_top = 20
r_bot = 8
h = 40
outer = loft 0 (⭘ r_bot) h (⭘ r_top)
lug = ⭘ 3 |> loft 6 (ω 30 (△ 4))
```

Profiles must be single closed outlines: the 2D primitives, beziers, and
their in-plane transforms (`χ ψ ω ⬈ ⇋`, `↯`). Booleans between profiles,
`ζ`, and `θ`/`ϕ` rotations are rejected.

### 2D and 3D

Every expression is a 2D profile or a 3D solid. Extruding a solid,
offsetting a solid, combining a profile with a solid, or a 2D `main` are
errors; extrude profiles first (`⮕ h p`, `|> extrude h`) or loft them.

### Simple mode

```coscad
!simple
ball = Sphere 10
moved = Translate.x 5 ball
scaled = Scale (2, 1, 0.5) ball
main = moved * scaled - Mirror.y moved
```

Capitalized primitives, vector or namespaced transforms, prefix combinators
(`Union`, `Hull`, `Loft`, ...), `*` union, `-` difference.

### Pipelines and anchors

```coscad
main = box 40 40 8
  |> at top (cyl 6 20)
  |> on rt 0 0 1 (⌭ 3 8 0.5)
  |> cutat top 0 0 -2 (zcyl 2.15 30)
  |> move 10 0 0
```

Stages: `x y z rotx roty rotz move scale mirror extrude loft anchor add cut
isect hull mink at on cutat`. Anchors: `top bot lft rt fwd bak ctr`, long
aliases, and `+` combinations such as `top+rt`. `at`/`on` mate the child's
opposite anchor; `cutat` centers the cutter.

## Assembly files

```coscad
!glyph
plate 250 300 6
clearance 0.15
gap = 2

bracket ← bracket.coscad ×2 ▽top seam=rear
corner ← corner.assemble ×1
rail ← rail.coscad ×0

asm = bracket ⊕ (corner |> move (55 + gap) -13 -7) ⊕ rail
```

- `plate width depth [margin]` configures the bed; parts pack across as
  many plates as needed.
- `clearance distance` is the minimum gap `coscad check` enforces.
- `×count` sets printable instances; `×0` marks premade assembly-only parts.
- `▽anchor` declares the face placed on the bed.
- `key=value` entries are manufacturing hints.
- `asm` is the optional assembled-view expression (continuation lines may
  start with `|>` or a boolean glyph; comment lines may sit between them).
- `fastener spec clamped host face [×n] [at=mm] [nut=dropin|slidein]
  [head=button|socket] [through=mm] [torque=Nm]` describes one screw group
  for `coscad plan`; part hints `profile=2020`, `material=`, `ends=`,
  `torque=`, `mass=` describe rails and torque classes; `plan beam=N
  rest=faces ...` tunes the planner.

## CLI

```bash
coscad part.coscad [-o part.scad]     # compile
coscad stl part.coscad [-o part.stl]  # compile + render, prints volume and bounds
coscad frame.assemble                 # design stage: view, plate(s), frame_design.json
coscad next frame.assemble            # manufacturing: beds + frame_manifest.json
coscad check frame.assemble           # interference / clearance on real meshes
coscad plan frame.assemble [--png]    # numbered build steps, preload sheet, BOM, step scenes
coscad doctor                         # verify OpenSCAD + BOSL2 + a test render
```

Errors carry `file:line:col` and quote the offending line.


## Oriented frames and constrained assemblies

```coscad
plate = box 30 20 4 |> frame socket top top bak
post = box 6 6 20 |> frame foot bot bot bak
main = post |> mate plate @socket @foot 90 0.2
```

Frames retain an origin, outward normal (+Z) and in-plane up (+Y).
Mates oppose normals, align up directions at zero roll and return the child
only. Roll is degrees about the parent normal; gap is millimetres outward.
`datum` / `place` provide translation-only point placement. `grid n m px py`
repeats geometry; `|> hull` takes the hull of the whole preceding group.

```coscad
base ← base.coscad ×1
post ← post.coscad ×1 after=base
fixed base
mate post @foot base @socket roll=90 gap=0.2
```

Use one alias per physical instance (×0 or ×1). Each constraint component
needs a fixed root. Redundant edges verify closed loops. Geometric mates do
not infer fasteners or assembly order: retain `fastener` and `after=` lines.
`coscad validate model.assemble` checks source and constraint solving without
packing, rendering or writing files. Interference still needs `coscad check`.

## Frame-based fasteners

```coscad
center ← center.coscad ×1 preload=hand
arm ← arm.coscad ×1
// Placement via fixed/mate is independent of hardware declarations.
fastener M4x12 arm center @root_hole seat=@head_seat head=socket nut=hex pocket=@nut_seat
```

The host hole frame points toward the head; the clamped head seat has the
same normal. The nut bearing seat has the opposite normal. Reach is measured
between the bearing seats, including recesses. Each frame declaration locates
one screw; do not mix `at=`, `through=`, `inset=` or `×count` with a host frame.
`pocket=clamped.@frame` locates the nut seat on the clamped part. Use
`preload=hand` when a part must be turned independently to load its nuts.
