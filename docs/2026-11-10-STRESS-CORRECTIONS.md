# Stress selection, tension and publication corrections

Status: implemented 2026-11-10. Fixes three reproduced bugs in the single-part
stress workflow. The existing report schema remains `coscad.stress/1`, with an
additive `artifacts` field and changed companion-file locations.

## One selected plane, not every nearby step

Previously a support or load selected all outward faces within one cell of its
requested plane. For two adjacent 2 mm cubes with a 1 mm height offset, at 1 mm
cell size, `support bot` clamped both underside levels and `load top` loaded
both top levels: eight faces instead of four in each case.

The selector now finds the nearest exterior voxel plane with the requested
normal within one cell of the target. It selects only that plane. Exact matches
therefore exclude adjacent step levels. Equally close distinct levels are an
error that asks for an explicit datum on the intended face or a finer grid.
A target outside that distance still selects no faces and fails.

This retains allowance for the voxel boundary's approximation of a CAD surface.
A datum plane can still select multiple disjoint patches on that same plane;
it is not a bounded interface-region declaration. Inspect selected face counts
and refine the mesh before interpreting results.

## Tensile demand excludes compression

The maximum tensile Z stress is now `max(0, maximum sigma_zz)`. A compressed
2 mm PLA cube previously reported about -0.218737 MPa as its maximum
across-layer tension; it now reports zero. The signed stress tensor remains
available in full cell results. The existing safety calculation already treated
nonpositive tension as no tensile demand. Positive tensile demand is unchanged.

## Coherent report generations

Previously rendering replaced the flat STL/SCAD files before analysis could
fail. The prior successful JSON could then describe a different geometry from
its newly overwritten companion mesh.

Now each run claims a unique directory beside its requested JSON report, for
example `part_stress.json.assets-1/`. It renders there, completes analysis,
saves the exact root source bytes used by the parser, computes SHA-256 hashes,
and writes the prepared report. One same-filesystem rename publishes the JSON
only after all artifacts are complete. A failed preparation/publication removes
that run's unpublished generation, leaving previous reports and generations
untouched. A valid analysis that misses its safety factor still publishes its
results and exits 2.

Example report field:

```json
"artifacts": {
  "source": {"path": "part_stress.json.assets-1/source.coscad", "sha256": "..."},
  "mesh": {"path": "part_stress.json.assets-1/mesh.stl", "sha256": "..."},
  "scad": {"path": "part_stress.json.assets-1/model.scad", "sha256": "..."}
}
```

Paths are relative to the report directory; hashes cover exact file bytes.
The source snapshot is the root input, not a transitive dependency archive.
Hashing uses pinned `cryptohash-sha256` 0.11.102.1 inside the executable, with
no external hash command. A report destination resolving to the source input
is refused.

**Compatibility:** the report filename and existing result fields are stable,
but tools must follow `artifacts.mesh.path` rather than derive a sibling
`part_stress.stl`. Pre-existing flat companion files are not touched and should
not be used as the new report's geometry.

Published generations are retained so a reader holding an older report keeps
its exact artifacts. Archive a report together with its referenced directory.
Unused generations can be removed when no reader or archived report needs them.
An uncatchable process termination can leave an unreferenced directory; it
cannot publish a partially prepared JSON. This is atomic process-level
publication, not an fsync-based power-loss durability guarantee.

## Validation

The focused experiment/stress suite passed 413 checks; the full non-rendering
suite passed 1507 checks, both with zero failures. The geometry/oracle tier
was not rerun for these stress-only runtime changes. An independent Python
SHA-256 check matched all three artifact hashes, including a Unicode source
snapshot. New cases cover exact
stepped faces, midpoint ambiguity, a uniquely nearest off-grid datum, a missing
plane, compression and tension, report/source overwrite refusal, failure before
publication, failure of the final rename, and preservation of earlier artifacts
on both failed and successful replacement runs.

The independent shelf comparison was rerun after the solver change: all six
cases and both medium-to-fine refinement gates passed. The fresh
[comparison evidence](evidence/2026-11-10-stress-corrections/shelf-comparison.json)
records the source and executable hashes. Peak stress and physical printed
strength remain outside those acceptance claims.
