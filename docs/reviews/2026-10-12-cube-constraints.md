# 2020 cube constraint conversion, 12 October 2026

The oriented-frame and assembly-constraint implementation was committed and
pushed to `origin/main` as `5cf16da`. This follow-up converts the canonical
[200 mm cube](../../examples/assemble/plan/cube.assemble). The audit ran before
this follow-up was committed; it records the base commit, source hashes and
a dirty-worktree flag so its measurements are tied to the converted source.

## Placement and bracket findings

The assembly has 29 named physical instances: 12 rails, 16 flat L brackets
and one panel. One rail is fixed; 54 rigid mating edges determine the other
poses and check 26 independent loops. Both bores of every bracket mate to
their supporting rails. Front and rear brackets explicitly choose different
local contact faces; their outward normals must oppose the rail interfaces.

All 16 bracket positions and rotations in the coordinate-built source were
correct and are preserved. An exact OpenSCAD symmetric difference between
the converted bracket solids and the retained
[coordinate pose reference](../../test/fixtures/cube-coordinate.assemble)
is empty, apart from the separate 1 mm³ marker used to check empty results.
Independent tests also check every bracket centre, bore position and normal,
material at all 16 corners, and clear passages through all 32 bracket bores.

| Bracket group | Count | Contact plane | Bracket extent perpendicular to face |
| --- | ---: | --- | --- |
| Bottom ring, four corners | 4 | z = 0 | z = -6 to 0 mm |
| Top ring, four corners | 4 | z = 200 | z = 200 to 206 mm |
| Front, two lower and two upper corners | 4 | y = 0 | y = -6 to 0 mm |
| Rear, two lower and two upper corners | 4 | y = 200 | y = 200 to 206 mm |

Regression cases swap a bracket leg, flip its contact face, or rotate a rear
bracket incorrectly. Each fails with a conflicting-constraints diagnostic.
Increasing bracket thickness from 6 to 8 mm keeps all interfaces closed and
moves the outer surfaces outward, without editing assembly coordinates.

Manufacturing quantities are preserved: the rails are premade (`×0`), the
16 bracket aliases each request one print, and the panel requests one print.
Identical bracket geometry coalesces into a shared manufacturing variant.

## Panel and planner corrections

The old panel had four bores, while the instruction source requested six
screws at other positions. The panel now has six bores with matching mating
frames and explicit host distances. Their assembled host-face coordinates
are `(200,60,10)`, `(200,140,10)`, `(200,60,190)`, `(200,140,190)`,
`(200,10,100)` and `(200,190,100)` mm. Actual mesh probes verify all six
passages; the panel's closed mesh volume is 107873.475374 mm³.

The conversion exposed a separate planner defect: the 190 mm inset panel
only covers part of each 20 mm rail face. Taking the transverse centre of
that overlap put screws 2.5 mm away from the slot centre. Side-face screws
now stay on the 2020 host's cross-section centre. A clamped part that misses
that centre fails explicitly. Non-rail hosts and rail end faces retain the
overlap-centre rule.

Plan JSON includes `position_mm`, the assembled host-face contact point.
The independent audit matches all 38 emitted coordinates, host names and
faces: 32 bracket screws and six panel screws.

## Executed verification

- Full `stack test --fast --test-arguments='+RTS -N1 -RTS'`:
  **586 passed, 0 failed**, including actual OpenSCAD rendering, bracket
  symmetric difference, bore/material probes, resize and wrong-rotation cases.
- CLI diagnostic corpus: **462 inputs**, **zero expectation failures**,
  crashes or timeouts. It accepted 67 and rejected 395 inputs; curated
  assertions cover 30 valid controls and 179 required rejections.
- `coscad check examples/assemble/plan/cube.assemble --keep-temp`:
  **29 instances, zero overlaps, zero clearance warnings**. The audit also
  verifies all 29 independent world-space bounds and requires every body
  to be one connected, closed manifold mesh.
- `coscad plan examples/assemble/plan/cube.assemble --png`:
  **28 steps, six flips, zero design errors**. Each physical instance is
  placed once and all 38 fasteners are driven. The final render was inspected.
- [Audit runner](../../experiments/topological/verify-cube.py) and
  [recorded measurements](../../experiments/topological/cube-results.json)
  preserve source hashes, per-part mesh measurements and actual screw positions.

The regenerated [instructions](../../examples/assemble/plan/cube_plan.md),
[plan data](../../examples/assemble/plan/cube_plan.json) and
[final render](../../examples/assemble/plan/cube_step28.png) accompany the
converted example. The manual deployment regenerates the companion website
from the assembly source.

These are nominal CAD and instruction checks. Rails are 20 mm envelope
solids, not detailed T-slot profiles; nuts and screw heads are instruction
graphics rather than collision solids. This work does not establish physical
fit, strength or print success. The canonical part migration and compiler
defaults remain separate decisions beyond this assembly conversion.
