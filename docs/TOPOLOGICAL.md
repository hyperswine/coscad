# Topological modelling in CoScad

Model by relating features to the part's own anchors instead of placing
them at absolute coordinates. Placement intent survives dimension changes,
and the only absolute `move`s left in a project belong in `.assemble`
poses — where absolute placement is the actual meaning.

## Anchors

Every shape expression — including composites built with `⊕`/`⊖` — has
bounding-box anchors: `top bot lft rt fwd bak ctr`, combinable with `+`
(`top+rt` = top-right edge midpoint, `lft+fwd` = near-bottom corner...).
Anchors are computed in the compiler (Coscad.Geometry) and desugared to
plain translates/rotates, so the emitted OpenSCAD never contains
attachment machinery and any result is itself anchorable.

## The three relational operations

| pipeline | glyph | semantics |
|---|---|---|
| `p \|> at v [dx dy dz] c` | `p ⌖ v c` | union; c's **opposite** anchor mates to p's anchor v (+offset). Stacking: `base \|> at top post` |
| `p \|> on v [dx dy dz] c` | `p ⋈ v c` | union; c rotated so its +Z points along v, its bottom mated to the anchor. Studs out of faces, diagonal anchors ok |
| `p \|> cutat v [dx dy dz] c` | — | difference; c **centered** at the anchor point (+offset). Drilling: `plate \|> cutat top 0 0 -2 (zcyl 2.15 30)` |

Note the asymmetry: `at`/`on` mate faces/corners (child's opposite
anchor), `cutat` centers the cutter. Getting this wrong shifts things
by half the child's size.

`⚓ v shape` / `\|> anchor v` re-origins a shape at its own anchor.

Two cases where a bounding-box anchor does not sit on the geometry print
a warning when the part is compiled:

- `at` or `on` on a face that lies wholly inside a cut's bounding box,
  cut from the inside. The face is gone, so the child floats where it was.
  A `cutat` there is not reported: it only cuts air around a point that is
  still meaningful.
- Any anchor on a part whose combined rotation is not a multiple of 90°.
  Its axis-aligned box is larger than the part, so the anchor misses it.
  Anchor before turning it, or name a datum or frame (below).

## The stable-datum rule (learned by breaking things)

Composite anchors move as the part grows. Two real failures from the
bow project, worth internalizing:

1. Bolt holes were anchored via the composite's *y-center*. The limb's
   knob height later differed from the assumed value by 0.33 mm — which
   silently shifted the y-center and consumed the entire M4 clearance.
   The interference check caught it (0.14 mm³ graze); eyeballing never
   would have.
2. The fix was not tuning offsets but changing datum: anchor the tab
   and holes to `lft+fwd` — the root corner adjacent to the joint —
   which no change to the distant tip can move.

**Rule: anchor a feature to the nearest corner/edge of the geometry it
belongs to, never to a composite center or far face that unrelated
geometry can shift.** Offsets from a stable datum are honest local
dimensions; offsets from a drifting one are coordinates in disguise.

Also remember ordering: anchors are evaluated against the shape built
*so far*, so attach features while the relevant bbox is still simple
when you can (cuts don't change the bbox — `Diff` keeps the positive's
box — so hole order never matters).

## Named feature points and separate construction geometry

Capture a stable point before adding geometry that could move its bounding
box. `@name` selects that point later, including after transforms:

```
plate = box 40 30 4 |> datum hole top+lft 8 0 0
blank = plate |> at rt (box 20 10 4)
tool = zcyl 2 6 |> place blank @hole ctr 0 0 -2
main = blank |> cut tool
```

`place` explicitly names the parent target and the child's own point. It
returns only the translated child, making it suitable for cutters and hull
endpoints. It does not union the construction parent or rotate the child.
`|> anchor @hole` can also re-origin a shape at its saved point.

Datums are named construction points, not discovered surfaces or oriented
frames. They remain after Boolean cuts. Copies retain their names, so a
reference duplicated by a union or grid is ambiguous and rejected; use a
single feature as the datum or define a new named point on the combined
result. See the [language reference](LANGEXTENSION.md#named-datums-placement-and-grids)
for transform, extrusion and loft behaviour.

`|> grid n m pitch_x pitch_y` repeats a feature with its first copy unchanged.
Re-origin at a suitable corner, grid the cutters, then `place` that group
against the box's saved first-cell point. `|> hull` takes the convex hull of a
whole group without repeating one member. The native egg-box pilot in
`experiments/topological/native-egg-box.coscad` combines these operations;
its N×M layout needs only numeric bindings, with no Python generation.

## Oriented interfaces and constrained assemblies

Save an interface's outward normal and in-plane up direction explicitly:
`|> frame socket top top bak`. A child with
`|> frame foot bot bot bak` can then use
`|> mate parent @socket @foot`. Normals oppose and up directions agree;
optional `roll gap` controls rotation about the parent normal and separation.
The parent supplies the reference and is not included in the result.
Unlike a point datum, an oriented frame fully specifies the rigid placement.

In `.assemble`, declare separate aliases for each physical copy, fix a root,
and connect the interfaces with `mate` statements. The solver creates the
assembled view, handles edges in either direction, and diagnoses floating
components and contradictory closed loops. The checker and planner reuse
those same placements. Existing coordinate assemblies remain supported.
See the [frame and constraint reference](LANGEXTENSION.md#oriented-mating-frames)
and `examples/assemble/constraints/frame-stand.assemble`. This first increment
supports rigid mates; it does not infer hardware, solve moving joints or
establish interference-free fit without `coscad check`.

The canonical [200 mm 2020 cube](../examples/assemble/plan/cube.assemble)
now uses one fixed rail and 54 mates for its 29 physical parts. Each bracket
mates through both bores, so the second interface checks its corner rotation
and contact face. Redundant rail and panel interfaces also check closed
loops. All 16 bracket poses match an independent coordinate reference;
the [verification report](reviews/2026-10-12-cube-constraints.md) records mesh,
hole, screw-position and instruction checks.

The remaining canonical assemblies now use the same constraint style: the
ball, three-piece bows, corner pair, tesseract, enclosure and Orbit Tower.
Packing and collision fixtures use fixed independent roots rather than
invented connections. A reflected recursive frame retains its reflection
inside a separate part definition, because a rigid mate cannot mirror a part.
The [migration report](reviews/2026-10-12-assembly-migrations.md) describes the
retained geometry references, instruction checks and numerical tolerances.

`coscad validate file.assemble` checks part sources and solves the rigid graph
without creating artifacts. It is intended for editor diagnostics; hardware
access and interference remain separate `plan` and `check` operations.

## Worked example

`examples/topological/tbracket.coscad` rebuilds the project's seam
bracket with zero absolute translations and was verified
boolean-identical to the coordinate-built original (0.000 mm³
symmetric-difference residual). `topo_tour.coscad` is a smaller
annotated tour.
