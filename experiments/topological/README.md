# Topological migration pilots

Nine original migration pilots plus two using the new
[datum, placement, grid and hull operations](../../docs/TOPOLOGICAL.md).
The canonical models and compiler defaults are unchanged.
The findings and migration proposal are in the
[review](../../docs/reviews/2026-10-12-topological-migration.md).

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

Run the comparisons from the repository root:

```sh
stack build
python3 experiments/topological/verify.py \
    --coscad "$(stack path --local-install-root)/bin/coscad"
```

The standard-library Python runner compiles and renders originals and pilots
in scratch directories, checks closed meshes, connected components and bounds,
and compares their solids. It then changes eight sets of dimensions
and checks bounds plus 28 small material/cavity probes. Finally it compares fresh
Orbit Tower plans with the original parts, original pilots, and new native rotor,
excluding the source path.
OpenSCAD warnings fail the checks. `COSCAD_OPENSCAD`, `COSCAD_BOSL2`, and
`OPENSCADPATH` configure local tools. No slicing, printing or network commands run.

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
source commit, dirty-worktree flag, compiler binary hash and OpenSCAD version.
[results.json](results.json) preserves the initial nine-pilot study.
`--case NAME` limits a development run; use a separate `--output` path for such
partial results. Scratch files are removed on success and retained on failure.

The lexical inventory is a way to find candidates, not proof of stable design
intent. The pilots are outside the normal example scan and are not yet in CI.
