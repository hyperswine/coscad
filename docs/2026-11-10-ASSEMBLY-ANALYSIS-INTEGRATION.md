# Shared assembly instances and experimental bonded FEA

Implemented 2026-11-10. This connects the resolved `.assemble` model to an explicit
analysis configuration. Placement constraints still only place geometry.

```sh
coscad instances experiments/assembly-fea/shelf.assemble -o /tmp/shelf-instances.json
coscad stress experiments/assembly-fea/shelf.assemble \
  --bonded experiments/assembly-fea/shelf-analysis.json -o /tmp/shelf-stress.json
```

The asymmetric example is `shelf-soft-right-analysis.json` in the same directory.
It gives the right arm a lower Young's modulus. These configurations reproduce the
existing [bonded shelf benchmark](2026-11-10-BONDED-SHELF-VALIDATION.md) from actual
part geometry and assembly placements, rather than hand-entered voxel bodies.

## Shared instance contract

`Coscad.Instance` owns affine composition and occurrence numbering, now also used
by the build planner. `Coscad.Assembly.Instances` recursively enumerates physical
leaf references after resolving `.assemble` expressions and `fixed`/`mate` constraints.
The planner retains its existing top-level grouping; the analysis export expands
subassemblies to their leaves.

`coscad instances` writes JSON to stdout, or atomically to `-o`. Schema
`coscad.instances/1` contains:

- `sources`: canonical source paths and SHA-256 of the UTF-8 bytes read. Each path
  is read once per invocation, including shared/repeated dependencies. Export and
  analysis use those parsed values without reloading files during the solve.
- `instances`: physical occurrences with qualified `id`, canonical leaf `source`,
  `part_to_assembly`, `rigid`, `part_to_print`, `print_to_assembly`,
  `print_layer_normal_assembly`, `declared_print_count`, `assembly_only`, and `hints`.
- Matrix convention: row-major 4×4 matrices multiplying column `[x,y,z,1]`, in mm.
  Parent and child matrices compose as `parent * child`.

An alias placed twice produces `alias#1`, `alias#2`. A nested leaf can be
`frame#2/rail`. Reordering occurrences can renumber IDs; consumers must revalidate
configs against the current export. A reference's print quantity does not create
physical occurrences. An unused reference creates none. `×0` geometry still appears
when placed, with `assembly_only: true`, including when inherited from an ancestor.
`declared_print_count` is the leaf declaration's hint, not an expanded manufacturing
quantity. Outer hints override inner hints, matching the manufacturing loader.
Fastener declarations do not synthesize geometry or masses.

`part_to_print` exists only when a leaf declares a print orientation (`▽bot`, etc.).
It maps that part's local geometry to the oriented, minimum-bound-normalized print
frame. `print_to_assembly` composes its inverse with the physical placement.
`print_layer_normal_assembly` maps print +Z into the assembly for rigid placements.
Missing orientation means JSON `null`, not an assumed upright print. Subassembly
print orientation and automatic bed packing are not inferred. For a sliced mesh
already translated/rotated onto a bed, also compose the inverse of that known bed
placement before applying `print_to_assembly`; it is not a drop-in bed-to-assembly
matrix. See [mass analysis](2026-11-10-MASS-ANALYSIS.md) for the sliced-mass consumer.

Transforms around references may include scale/reflection; `rigid` means orthogonal
with unit scale and permits reflection. Assembly-level cuts, intersections, hulls,
extrusions or unreferenced solids are rejected because separate physical identity
would be ambiguous. Put geometric construction inside a referenced `.coscad` part.

## Explicit mechanical configuration

Schema `coscad.assembly-bonded/1` requires `cell_mm`, `bodies`, `supports`, `loads`
and `bonds`. Unknown/duplicate fields are rejected. See the complete runnable
[configuration](../experiments/assembly-fea/shelf-analysis.json).

Each `bodies` row has `instance`, `young_mpa`, and `poisson`; declare exactly one
isotropic material for every exported physical leaf. There is no implicit omission
of assembly-only parts or interpretation of material hints.

A region specifies a rectangle in **assembly/world coordinates**, for example:

```json
{
  "instance": "left",
  "axis": "y",
  "side": -1,
  "plane_mm": -2,
  "bounds_mm": [[0, -2, 0], [2, -2, 2]]
}
```

`axis` is `x`, `y`, or `z`; `side` is the outward normal sign. Bounds have the same
coordinate as `plane_mm` on the selected axis, and positive extent on the others.
All coordinates align to the common cell grid. Every face in the rectangle must
exist on the exterior; empty selections, partial faces and rectangles crossing
holes fail. This avoids nearest-plane or whole-body selection heuristics.

`supports` contains fully clamped regions (all three displacement components).
Each load has `region` and `force_n`: a total force vector in world axes, distributed
uniformly over the region. Each bond has `name`, `a`, and `b`, both regions. The
existing solver requires matching, coincident, opposite-facing patches and enforces
their displacement equality. Geometric `fixed` and `mate` declarations never create
supports or bonds. Omitting them leaves floating bodies, which fail validation.

## Solver scope and reports

The bridge renders each part independently with the native backend, applies its
placement, and builds the existing bonded Hex20 problem. This first bridge accepts
only rigid placements whose resulting mesh has orthogonal, grid-aligned boundaries.
It checks vertices and triangle planes before voxel sampling and requires full cells.
Axis-aligned right-angle rotations and reflections can be represented; arbitrary
rotation, curved/sloping faces, scaled placements and sub-cell details fail rather
than producing an unqualified approximation. Refine `cell_mm` for alignable geometry.

Limits: 32 bodies, 1,000 total occupied cells, and at most 1,000 candidate bounding-box
cells per body. The last restriction intentionally rejects sparse bodies with large
bounding boxes. Coordinates must lie within 10 million cell widths of the world
origin; grid alignment uses a tolerance of 1e-7 cell widths. The bonded solver
retains its overlap, connectivity, patch and
constraint checks. Tolerance is `1e-9`, with at most 50,000 PCG iterations.

`coscad.assembly-stress/1` includes the complete instance export, configuration and
its SHA-256, and the bonded solver report: body grids, selected face indices, nodal
results, cell stresses, reactions, interface forces/moments, residual and energy.
The grid/face lists make the actual discretized selections auditable. Source hashes
identify input bytes; original source files are not archived in this report.
A later edit cannot change the in-memory inputs used by that invocation, but reading
multiple files is not a filesystem-wide transactional snapshot.

Output publication is atomic. Failed runs preserve an existing report; output may
not overwrite the configuration, assembly, or any referenced source. Usage/schema
and unsupported bridge inputs exit 1; invalid mechanical designs exit 2.

This remains experimental linear-elastic, small-displacement, perfectly bonded
analysis. There is no contact, friction, slip, separation, fastener preload, joint
compliance, infill or layer-dependent stiffness, failure criterion or strength
verdict. Exporting print normals does not introduce anisotropic material behavior.
The earlier independent numerical benchmark supports the solver; this bridge has
no physical validation. Region previews and measured assembly correlation remain
next work.

## Verification

Regression tests in `test/Tier/Instances.hs` cover nested repeated identities,
transform composition, mating constraints, print hints, dependency hashes, ambiguous
geometry rejection and output preservation. Both shelf material cases compare actual
geometry-derived grids, selected faces, materials, energy and every nodal displacement
against the existing explicit-grid benchmark. Failure tests exercise missing
materials, unsupported grids/transforms and the absence of inferred supports/bonds.

The independent scikit-fem comparison can also exercise this bridge directly:

```sh
python scripts/compare-bonded-reference.py --assembly-exe /path/to/coscad \
  --output /tmp/assembly-reference.json
```

Use `experiments/requirements-fea.txt` in a separate Python environment. The
[local evidence](evidence/2026-11-10-assembly-analysis/reference.json) records six
passing comparisons (symmetric/soft-right, cell sizes 2, 1 and 2/3 mm), source and
executable hashes, package versions and refinement checks. Maximum relative nodal
error was approximately 2.2e-11. Medium-to-fine displacement and interface resultant
checks pass the existing thresholds; no peak-stress convergence or physical
validation is claimed.

The `-Werror` build, 477 focused checks and final 1,718-check native suite pass.
The full rerun used `GHCRTS=-N2`; the earlier default-worker run hit an existing
parser timing threshold. See the [validation record](evidence/2026-11-10-assembly-analysis/README.md)
for commands, the initial failure and the isolated reproduction. OpenSCAD oracle
and hosted CI runs were not performed.
