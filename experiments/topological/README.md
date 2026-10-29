# Topological migration pilots

Nine original migration pilots plus two using the new
[datum, placement, grid and hull operations](../../docs/2026-07-24-TOPOLOGICAL.md).
These pilots leave canonical models and compiler defaults unchanged. The
subsequent canonical cube conversion is described separately below.
The findings and migration proposal are in the
[review](../../docs/2026-10-12-TOPOLOGICAL-MIGRATION.md).

For these pilots, topological **placement** means:

- No `χ`/`ψ`/`ζ`, `move`, or pipeline `x`/`y`/`z` inside a part.
- Features reference anchors on a suitable local parent; offsets express
  named dimensions such as edge inset, overlap, or pocket depth.
- `anchor` establishes the reusable feature or print frame. Rotations,
  dimensions, and intrinsic sketch coordinates remain legitimate geometry.
- Assembly poses are a separate concern. `orbit-tower.assemble` retains the
  existing poses while replacing all five part sources with the pilots.

`cases.json` pairs each pilot with its canonical original. No canonical source
is overwritten. To compile a pilot or generate its assembly instructions:

```sh
stack exec coscad -- experiments/topological/bow-center.coscad -o /tmp/bow-topological.scad
stack exec coscad -- plan experiments/topological/orbit-tower.assemble
```

Run the comparisons from the repository root, with a Python that has numpy
and manifold3d:

```sh
python3 experiments/topological/verify.py --coscad "$(cabal list-bin exe:coscad)"
```

The runner renders originals and pilots with `coscad stl` in scratch
directories, checks closed meshes, connected components and bounds, and
compares their solids with manifold3d. It then changes eight sets of dimensions
and checks bounds plus 28 small material/cavity probes. Finally it compares fresh
Orbit Tower plans with the original parts, original pilots, and new native rotor,
excluding the source path. Compiler warnings fail the checks. No OpenSCAD,
slicing, printing or network commands run.

Until 2026-10-29 the runner compiled to .scad and used OpenSCAD with BOSL2
for rendering and every boolean probe (`COSCAD_OPENSCAD`, `COSCAD_BOSL2` and
`OPENSCADPATH` configured it). The two result files below were produced that
way and are kept unchanged as records. Since then it uses the native compiler
and manifold3d, needs neither OpenSCAD nor BOSL2, and writes
`manifold-results.json` by default (its `geometry` field replaces `openscad`).
A residual that is not empty must still have at most 0.001 mm³ of volume, and
its thickness, now either the minimum bounding extent or the mean thickness
2 × volume / surface area, must be at most 0.00001 mm. A 2026-10-29 run passed
every comparison, resize bound, probe and the plan comparison. Volumes agree
with native-results.json to within 4e-7 relative. Differences from the
OpenSCAD run: bow-center's residual is now empty (was 0.000173 mm³), while
orbit-axle and orbit-rotor leave zero-volume slivers (under 2e-13 mm³, mean
thickness under 4e-15 mm) where OpenSCAD reported empty output.

In the initial nine pilots, eight comparisons have empty differences. The bow has a measured
0.000173 mm³ difference at its grip floor, under 0.000001 mm thick, consistent
with numerical roundoff from evaluating/translating the hull in a different
order. The runner records this explicitly and allows a non-empty difference
only when both volume is at most 0.001 mm³ and its minimum extent is at most
0.00001 mm. Cavity probes always require truly empty output.

The new rotor has an empty symmetric difference from its original. The native
egg box uses grids for corner cylinders and N×M cells, plus unary hull and a
saved first-cell datum. Changes to hull evaluation and transform order produce
dispersed numerical slivers in a direct difference, so its comparison instead
requires **both** solids to be contained in the other's 0.00001 mm per-axis
expansion. Both containment residuals must be truly empty and warning-free;
this is a tolerance comparison, not an exact empty symmetric difference.

[native-results.json](native-results.json) records the current eleven
comparisons, eight resize checks and instruction comparison, along with the
source commit, dirty-worktree flag, compiler binary hash and OpenSCAD version
(the OpenSCAD-era run).
[results.json](results.json) preserves the initial nine-pilot study.
`--case NAME` limits a development run; use a separate `--output` path for such
partial results. Scratch files are removed on success and retained on failure.

The lexical inventory is a way to find candidates, not proof of stable design
intent. The pilots are outside the normal example scan and are not yet in CI.

## Canonical cube conversion

The [2020 cube](../../examples/assemble/plan/cube.assemble) now uses rigid
mating constraints. Its bracket pose equivalence and bore probes run in
`stack test`. To additionally audit all 29 checker meshes and all 38 actual
planned screw coordinates, run from the repository root:

```sh
coscad check examples/assemble/plan/cube.assemble --keep-temp > /tmp/cube-check.log
coscad plan examples/assemble/plan/cube.assemble
python3 experiments/topological/verify-cube.py --check-log /tmp/cube-check.log
```

[cube-results.json](cube-results.json) records that audit. World bounds and
screw positions are specified independently of the mating declarations.
The [report](../../docs/2026-10-12-CUBE-CONSTRAINTS.md) explains the
panel-hole and planner corrections and the validation boundaries.
