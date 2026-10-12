# First topological language features, 12 October 2026

Implemented the first authoring features from the
[migration study](2026-10-12-topological-migration.md). They work in legacy,
`!glyph` and `!simple` modes. The canonical models and compiler defaults
remain unchanged.

| Feature | Syntax and behaviour |
|---|---|
| Named construction points | `shape \|> datum name ref [dx dy dz]` captures a point before later geometry grows; `@name` selects it |
| Re-origin at a datum | `shape \|> anchor @name` |
| Child-only placement | `child \|> place parent target own [dx dy dz]` translates the child's explicit point to the parent's explicit point, returning only the child |
| Native rectangular grids | `shape \|> grid columns rows pitch_x pitch_y`, starting at the original's frame |
| Unary hull | `group \|> hull`; the existing binary form still accepts a child |

Datums follow transforms, including attachment rotations, and survive Boolean
cuts as construction references. Unknown names, duplicate definitions and
ambiguous names after copies have explicit errors. Named points have no
orientation. Placement offsets use the current XYZ axes, and placement does
not rotate the child. The separate parent is validated even though it is not
emitted. Grid counts and expanded primitive copies are bounded, including
nested grids; fractional/zero/negative counts, zero pitch on repeated axes,
non-finite values and overflowing translations are rejected.

The AST stores datum metadata independently of assembly instance tags.
Codegen, bounding boxes, dimensionality, loft paths, validation and planner
enumeration all handle the new wrapper. Placement and grids lower to existing
transforms/CSG, so no new OpenSCAD runtime machinery is required. Checker
isolation retains hidden datum geometry, and placement's construction parent
does not become an extra assembly instance.

See the [language reference](../LANGEXTENSION.md#named-datums-placement-and-grids),
[authoring guide](../TOPOLOGICAL.md#named-feature-points-and-separate-construction-geometry)
and [mounting-plate example](../../examples/topological/datum-plate.coscad).

## Validation

- Full `stack test --fast --test-arguments='+RTS -N1 -RTS'`:
  **440 passed, 0 failed**, including OpenSCAD geometry, planner identity and
  hidden-parent datum isolation. Existing SCAD snapshots stay unchanged; the
  mounting plate adds one snapshot and one geometry baseline.
- Final warning-only cleanup: rebuilt and ran the pure suite with
  `COSCAD_RENDER=0`: **357 passed, 0 failed**.
- CLI diagnostic probes: **443 inputs**, all expectations met, with no crashes
  or timeouts. Mutation seeds now include named datums, placement and grids.
- [Recorded geometry and instruction checks](../../experiments/topological/native-results.json):
  **11 nominal comparisons**, **8 resize cases**, **28 material/cavity probes**.
  Each nominal solid is closed and has one connected component; its bounds
  agree with its original to 0.00001 mm.
- Fresh Orbit Tower plans are identical, except for the source path, across
  canonical parts, the original five pilots, and the set using the new rotor:
  **20 parts, 19 fasteners, 19 steps, no design errors**.

The [native rotor](../../experiments/topological/native-orbit-rotor.coscad)
captures its axle and satellite bores before adding asymmetric pads. Its
nominal Boolean symmetric difference is empty. Increasing its span, thickness
and counterweight together preserves both bores and restores material at the
former satellite-hole location.

The [native egg box](../../experiments/topological/native-egg-box.coscad)
needs only `columns` and `rows` bindings to change its N×M layout. Its direct
symmetric difference contains numerical slivers and is not accepted as an
exact comparison. Instead, both solids are contained in the other's
**0.00001 mm per-axis expansion**, with both residuals empty and warning-free.
The 3×1 variant passes independent floor, divider, wall and cell-clearance
probes. This is geometric validation; no new print was started.

The original bow pilot retains its previously measured 0.000173 mm³ residual,
confined to a layer under 0.000001 mm thick. All other original pilots and the
new rotor have empty symmetric differences.

## Remaining work

Oriented mating frames, explicit roll, per-instance feature selection and
closed-loop assembly constraints remain separate design work. The current
datums are points, not a general constraint solver or automatic surface
tracking. Canonical example migration and an eventual default authoring style
still need the fit, resize and instruction checks described in the study.
