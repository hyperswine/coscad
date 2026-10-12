# Oriented mating frames and rigid assembly constraints, 12 October 2026

The previous datum, grid, placement and hull increment was committed and
pushed to `origin/main` as `d7d1211`. This follow-up implements explicit rigid
interfaces and declarative assembly placement. Canonical models and existing
manual assembly expressions remain unchanged.

## Authoring contract

`shape |> frame name ref normal up [dx dy dz]` records an origin, outward
normal (+Z) and in-plane up direction (+Y) on a 3D solid. The compiler
projects up onto the normal plane and completes a right-handed basis.
Frames follow transforms and CSG as construction metadata. Names share the
datum namespace; a frame origin can be selected as a point, but a point datum
cannot supply orientation. Ambiguous, mirrored, skewed and degenerate frame
selections fail explicitly. A fresh frame can be defined after a geometry
transform when an inherited frame is unsuitable.

`child |> mate parent @target @own [roll gap]` returns the child only.
Normals oppose, up directions agree at zero roll, and gap is along the
parent's outward normal. Roll is right-handed about that normal, in degrees.
The compiler emits a rigid `multmatrix`, avoiding Euler-angle decomposition.

`.assemble` adds:

```
fixed base
mate left_post @foot base @left_mount
mate right_post @foot base @right_mount roll=90
```

Each part alias identifies one physical instance, including when several
aliases use the same source file. Constrained declarations use ×0 or ×1.
Each connected component needs a fixed root; roots may have explicit
translation and XYZ rotation. A queue propagates full poses in either
direction and checks every already-positioned endpoint. Consistent redundant
edges, closed loops and multiple fixed roots work; contradictory loops report
position and orientation residuals at the mating declaration. Floating
components, unknown references and ambiguous multiplicities fail.

The loader, planner and interference checker share `resolveAssembly`.
Planner tags and checker-hidden reference solids therefore retain the same
metadata and poses. Manufacturing continues to orient and pack the original
part geometry, after validating assembly constraints.

See the [complete syntax and limitations](../LANGEXTENSION.md#oriented-mating-frames),
[corner example](../../examples/topological/mating-corner.coscad), and
[three-instance stand](../../examples/assemble/constraints/frame-stand.assemble).

## Validation

- Full `stack test --fast --test-arguments='+RTS -N1 -RTS'`:
  **520 passed, 0 failed**, including actual OpenSCAD rendering.
- Final source-location diagnostic refinements and four additional feature
  stability/resize cases: `COSCAD_RENDER=0 stack test --fast
  --test-arguments='+RTS -N1 -RTS'`: **434 passed, 0 failed**.
- CLI corpus: **462 inputs**, all expectations met, no crashes or timeouts.
  The corpus includes 19 new curated cases; all 16 new required rejections
  assert that no output artifacts were generated. Mutation seeds include
  oriented mating expressions. There are 30 valid curated controls.
- All **24 axis-aligned normal/up pairs** produce coincident mating origins,
  opposing normals and agreeing up axes. Further tests cover diagonal normals,
  transformed frames, roll, gap, resize and feature stability after CSG growth.
- The upright/sideways corner has **empty symmetric difference** against
  independent coordinate-built reference solids, verified using a separate
  1 mm³ marker and warning-free rendering. Its volume is 1696 mm³.
- The stand's checker render isolates both posts at independently specified
  world bounds, including the 90° roll. It reports **zero overlaps and zero
  clearance warnings**. A consistent graph with gap=-1 produces the expected
  **60 mm³ overlap** and fails the interference check.
- The stand's instruction planner includes all **three named instances once**
  and reports no design errors. Existing cube, bow, Orbit Tower and other
  plans, mesh baselines and snapshots continue to pass. Only the three new
  part examples add SCAD snapshots and geometry baselines.

## Scope of this increment

All mates fix six degrees of freedom. This is not a solver for hinges,
sliders, distances, flexible bodies, collision avoidance or automatic surface
recognition. Geometric mates do not infer hardware or assembly precedence;
existing fastener declarations still describe those joints. The full cube
was outside this initial increment. The subsequent
[cube conversion and verification](2026-10-12-cube-constraints.md) now covers
that canonical assembly. Changing the default style remains separate work.
