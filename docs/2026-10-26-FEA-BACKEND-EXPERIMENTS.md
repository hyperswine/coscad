# FEA and native backend: experiments and promotion gates

Status: useful experimental scope implemented and locally validated, updated
2026-10-26. This is the execution record and remaining promotion plan for
[the earlier intent](2026-10-17-FEA-AND-BACKEND.md), not a claim that
CoScad can yet qualify a printed part's strength or replace OpenSCAD throughout
the pipeline. The production backend remains OpenSCAD.

Start with the [quick-start guide](2026-10-26-FEA-NATIVE-QUICKSTART.md) for the
current commands. Earlier checkpoint counts below are historical; the final
acceptance audit at the end records the latest tested source.

## Order of work

| Stage | Deliverable | Gate before proceeding |
| --- | --- | --- |
| 1. Numerical and mesh baselines (implemented) | A bounded isotropic Hex8 beam experiment; a shape-to-mesh boundary and opt-in native primitive mesher. | Axial extension/stress, reaction balance, energy/work, load reversal, residual and bending refinement tests; native winding/closed-edge tests and OpenSCAD volume/bounds comparisons. Unsupported input and solver nonconvergence must fail. |
| 2. Improve the element and mesh contract | Compare incompatible-mode or selectively integrated elements against this full-integration baseline. Introduce an indexed mesh, binary STL and explicit backend capability/error types. | Bending and near-incompressibility benchmarks, patch tests and rigid modes, mesh convergence, independent solver comparison; reject hourglass modes. Binary round trips, malformed input, scale/reflection and component/cavity orientation tests. |
| 3. First actual CAD stress experiment | Occupancy for a declared primitive/CSG subset; supports and distributed loads on explicit face/frame regions; disconnected-body and unresolved-feature diagnostics. | Mesh refinement changes displacement by <5% on selected bracket fixtures; thin features and missing supports/loads cannot disappear silently. Compare with independent FEA and measured deflection before adding strength claims. |
| 4. Direct Manifold backend | Pinned Manifold C API or a small owned C++ shim; booleans and overlap measurement; add remaining primitive tessellation, then rounded forms and lofts. | Full geometry corpus: bounds, volume, topology, minimum clearances and symmetric difference where appropriate. Explicit unsupported-operation errors or explicitly requested fallback with recorded provenance. Verify allocation ownership, errors and repeated use across Linux/macOS/Windows. |
| 5. Printing and planner integration | Route `next`/`check` through the backend; native multipart 3MF and joint calculations; print orientation and experimentally calibrated material inputs. | Same part/build identities, slicer warnings, fits and physical regression prints as the established pipeline. No claim that solid isotropic FEA represents sparse infill or layer strength. |
| 6. Optional default replacement | Choose the validated native backend by default; decide separately whether step-image rendering should also become native. | Hosted release/packaging tests, interruption and rollback tests, representative performance measurements and full geometry gates. OpenSCAD remains an explicit comparison/export option. |

The first step deliberately changes the earlier proposed order: establish the
backend boundary and numerical baselines before implementing all primitives or
a general `coscad stress` command. Full integration is an experimental reference,
not the final element choice proposed in the intent document.

## Try the numerical experiment

```sh
cabal run coscad-experiments -- beam axial 4 2 2
cabal run coscad-experiments -- beam bend 10 2 2
cabal run coscad-experiments -- beam bend 20 4 4
cabal run coscad-experiments -- beam bend 30 6 6
```

The beam is 20 x 4 x 4 mm, E = 2000 MPa (an illustrative input, not a calibrated
PLA datasheet value). The x=0 face is fixed in all three directions. The TOTAL
1 N force is applied as uniform traction on x=20, integrated with face shape
functions. `axial` uses +X and nu=0 so the analytical axial-bar solution is an
exact boundary-condition match; `bend` uses -Z and nu=0.3. Outputs are JSON with
inputs, end-face average displacement, resultant support force and moment, strain
energy, maximum **cell-centre** von Mises stress, iterations and the recomputed
relative residual. There is no safety factor or material-strength verdict.

The baseline uses trilinear eight-node hexahedra, 2x2x2 Gauss integration and
Jacobi-preconditioned conjugate gradients on an assembled sparse operator.
This intentionally uses the GHC `array` boot library rather than introducing
`vector` and an optimized matrix-free implementation before measuring a need.
It is bounded to 2048 elements, 10000 total DOFs, 128 cells per axis and 10000
iterations. Default convergence tolerance is 1e-8 and default cap is 2000.
Invalid values, unsupported near-incompressible material parameters and failure
to converge produce an error instead of an accepted result.

The elasticity bilinear form follows the standard small-strain isotropic form
documented by [MFEM](https://mfem.org/bilininteg/). Full-integration Hex8 elements
can be too stiff, and near-incompressibility needs special treatment; see
[MOOSE's locking discussion](https://mooseframework.inl.gov/modules/solid_mechanics/VolumetricLocking.html).
The experiment therefore restricts nu to [0, 0.45]. Bending comparisons use
Euler-Bernoulli PL^3/(3EI); that reference excludes transverse shear and the
local clamped-end effects present in 3D elasticity. Refinement must be examined
as a trend, not interpreted as proof that every fine 3D result must equal that
formula exactly. Centre stresses omit boundary/singularity peaks.

## Try the native mesher

```sh
cabal run coscad -- stl experiments/beam.coscad --backend=native-experimental -o /tmp/native-beam.stl
cabal run coscad -- stl experiments/beam.coscad --backend=openscad -o /tmp/oracle-beam.stl
```

Supported: rectangular boxes; centred boxes/cylinders, including rounded
cuboids/cylinders; spheres and ordinary tori;
cylinders, cones, frusta and regular prisms; axis cylinders, tubes, wedges and
prismoids (including pyramids/ridges), chamfered cylinders; translations,
rotations, rigid transforms, scaling and reflection; identity metadata wrappers.
Round radial primitives use the emitter's existing 50 fragments, and prisms use
their explicit count. Negative scaling and reflection reverse triangle winding.
With the optional Manifold build, unions, differences, intersections, hulls and
Minkowski sums, chamfered cuboids and extrusion of simple polygon/circle profiles
are supported. Extrusion distributes over supported 2D CSG, including holes
expressed as differences. Rounded cuboids use an explicit convex boundary matching the sampled
octahedral sphere; ordinary tori require major radius > minor radius. Offsets, lofts, spindle/horn tori and explicit polygon
path lists remain unsupported. Missing kernel support is an explicit error.

Both choices pass through `Coscad.Backend.renderShape` for single-part `stl`.
The default and existing `renderPart` library API preserve OpenSCAD behavior.
Native output retains a companion .scad export and currently writes ASCII STL.
`check` and `next` accept the same explicit backend flag and default to
OpenSCAD. `doctor`, planner images and design-stage SCAD output retain their
established paths; there is no global environment selector.
Neither writer is yet an atomic immutable build-bundle publisher.

Sphere sampling matches the existing 50-fragment latitude-ring convention
([OpenSCAD source](https://github.com/openscad/openscad/blob/master/src/core/primitives.cc));
focused tests compare against the installed OpenSCAD binary. Extrusion accepts
one simple outline (clockwise or counterclockwise), at most 4096 vertices. A
repeated closing vertex is normalized, but self-crossing/touching outlines and
repeated interior vertices are refused. Explicit polygon path lists remain
unsupported; use 2D differences for holes. Coordinates/height are bounded to
1e9 mm at the extrusion ABI, and nearly degenerate polygons are refused.

## Verification and adoption

```sh
cabal build all --offline
COSCAD_RENDER=0 cabal test all --test-show-details=direct
COSCAD_BOSL2=../BOSL2 cabal test coscad-test --test-show-details=direct
# Fast iteration on just the new experiments and their OpenSCAD comparisons:
COSCAD_BOSL2=../BOSL2 cabal test coscad-test --test-show-details=direct --test-options=--experiments-only
```

The experimental numerical and closed-mesh tests are part of the normal suite.
Native/OpenSCAD comparisons run only when the geometry tier is enabled and
OpenSCAD is available. They compare volume and bounds, plus independent native
surface topology/winding checks; these are not yet full geometric equivalence
tests for arbitrary CSG.

Changing source intentionally invalidates the old pipeline source fingerprint.
Do not silently repin or deploy these experiments. Update dependency/release
pins only after the applicable promotion gates pass. No printer dispatch or
physical testing is part of this implementation.

The next gates include wider native coverage and release qualification.
Indexed topology and cooperative operation cancellation are now implemented. Rectilinear Hex8 and conforming Tet4/Tet10 paths are
usable within their explicit contracts below; general contact/printed-material
analysis and default backend replacement remain unfinished.

## First measured numerical results

Saved inputs, source/executable hashes, residuals and timings are in
[beam-results.json](evidence/2026-10-26-fea-backend/beam-results.json).
Initial local macOS/arm64 results, using GHC 9.8.2:

| Case / cells | Tip displacement in load direction | Difference from analytical reference |
| --- | --- | --- |
| Axial 4x2x2 | 0.000625 mm | 2.7e-15 relative |
| Bending 10x2x2 | 0.055181 mm | 11.71% |
| Bending 20x4x4 | 0.061053 mm | 2.32% |
| Bending 30x6x6 | 0.062370 mm | 0.21% |

The bending reference is 0.0625 mm. These observations justify continuing the
experiment; they do not establish stress convergence or printed-material
accuracy. Independent solver and element patch/rigid-mode checks now pass;
see the subsequent checkpoint below.

Local validation: the initial non-rendering suite passed 1000 checks and the
Haskell example suite passed. The full suite, including the OpenSCAD geometry
tier and 13 native/oracle comparisons, passed 1185 checks on an isolated copy
of tracked fixtures plus this change's sources. This excluded an unrelated
untracked phone-stand design with no geometry baseline. Following two final
native transform guards (zero axis and underflow-safe reflection parity), the
focused suite passed 76 checks including those same 13 OpenSCAD comparisons.
`cabal build all --offline`, the documentation checker and `git diff --check`
also passed. These are local results; hosted CI and other platforms were not run.


## Current checkpoint: pinned CSG and actual CAD FEA

Local functionality is experimental. The default remains OpenSCAD; native
assembly checks and manufacturing are opt-in. Planner images still use OpenSCAD.

### Native kernel

`native-kernel.lock.json` pins Manifold 3.5.2 to commit
`11235e6b8ebea2dbed8aec4285685aafd3d95667`. Build it explicitly:

```sh
python3 scripts/build-manifold.py
PKG_CONFIG_PATH="$PWD/.native/manifold-3.5.2/install/lib/pkgconfig" \
  cabal build all -fmanifold --builddir=dist-native
PKG_CONFIG_PATH="$PWD/.native/manifold-3.5.2/install/lib/pkgconfig" \
  cabal run --builddir=dist-native -fmanifold coscad -- \
  stl examples/bracket.coscad --backend=native-experimental -o /tmp/bracket.stl
```

The builder refuses a dirty or wrong-revision checkout, disables dependency
network downloads, builds a static library, and records configuration and library
hash in the installation. It does not change Cabal configuration. The optional
flag defaults off; `cabal build -f-manifold` remains the ordinary build.
CrossSection and parallel processing are disabled in this first pin.

The baseline covered 14/74 tracked examples; this build covers **71/74**,
including the seam bracket and several rail/tesseract parts. All 71 match the
established volume and bounds goldens. See
[coverage](evidence/2026-10-26-fea-backend/coverage-manifold.json) and
[comparison](evidence/2026-10-26-fea-backend/corpus-comparison.json).
`test/golden/native-supported.txt` prevents silently shrinking the accepted set.
The other three (`loft`, `offset_demo`, `waterbottle_advanced`) fail explicitly. Volume/bounds agreement alone is not geometric
equivalence; focused tests also check oriented edges and native/oracle symmetric
difference on primitive and selected existing CSG examples.

The C++ shim owns allocations and catches exceptions at the ABI. Haskell installs
cleanup before calling it, including for asynchronous exceptions. Boundary tests
passed 500 CSG operations and 100 extrusions, empty output, malformed/open/nonfinite/oversized
inputs and null arguments under AddressSanitizer and UndefinedBehaviorSanitizer.
This instruments the shim/test, **not the separately built kernel archive**.
Reproduce (adjust the install prefix):

```sh
clang++ -std=c++17 -g -O1 -fsanitize=address,undefined -fno-omit-frame-pointer \
  -I.native/manifold-3.5.2/install/include \
  cbits/coscad_manifold.cpp test/native/bridge_smoke.cpp \
  .native/manifold-3.5.2/install/lib/libmanifold.a -o /tmp/coscad-bridge-sanitizers
/tmp/coscad-bridge-sanitizers
```

The native backend now retains indexed connectivity across CSG and transforms,
and native assembly checking keeps it through shell splitting and intersections.
Only primitive/legacy triangle-soup imports weld exact coordinates. Output
property vertices collapse solely through the kernel's declared merge relations;
separate equal-position vertices are retained. Ordinary kernel results are not re-imported: doing so can change nearly
coincident thin differences. Exact zero-area transient faces may be normalized
once through the indexed importer, guarded by volume conservation (maximum of
1e-8 mm³ and 1e-10 relative). Operand import has the same volume guard; an
unresolved near-coincident operand is refused rather than silently altered. The legacy triangle-soup API and STL file format cannot carry
these connectivity identities and are not lossless round-trip formats.

Each indexed kernel operation gets a fresh execution context and a 60-second
cooperative deadline. `indexedOperationWithLimit` accepts 1..600000 milliseconds
for library callers. Cancellation produces an error and releases output buffers;
a subsequent operation starts independently. A large Boolean may finish its
current phase before observing cancellation, so this is **not a hard wall-clock
or memory guarantee**. Input/output bounds remain 500000 triangles and 1500000
vertices; kernel intermediate memory is not bounded by those counts. Hard process
isolation, full-kernel sanitizer coverage, portability and release packaging
remain open. No default backend switch yet.

### Exact rectilinear CAD analysis

```sh
cabal run coscad-experiments -- cad \
  experiments/cad/l-bracket.coscad experiments/cad/l-bracket-fine.json
cabal run coscad-experiments -- cad \
  examples/assemble/constraints/post.coscad experiments/cad/existing-post.json
```

The strict JSON sidecar specifies grid counts, E in MPa, Poisson ratio, total
force in N, and one support/load region each. A region has an axis-aligned unit
`normal` and planar `bounds_mm`; supports clamp all displacement components,
and force is integrated as uniform traction over the selected exterior faces.
Unknown/duplicate fields, missing faces, partial cell faces and loads on support
nodes fail. Results include the exact active grid, boundary-node weights,
per-node displacement, per-cell centre stress tensor, resultants, energy and
fresh residual. Support moments are about the world origin.

This first CAD subset accepts rectangular CSG and transforms that preserve world
axis alignment. All interior primitive boundaries must coincide with cell planes
(to 1e-8 cell spacing); it refuses unresolved features instead of approximating
occupancy. Multiple bodies, edge/point-only connections, insufficient constraints,
invalid inputs and solver failure are errors. It uses the same bounded,
fully integrated isotropic Hex8 formulation as the beam tests. This is not a
joint, contact, anisotropic, infill or large-deformation model.

[Independent CAD evidence](evidence/2026-10-26-fea-backend/independent-cad-fea.json)
checks occupancy for the fixtures, independently assembles/integrates traction
using scikit-fem, and uses SciPy's direct solver. All nodal displacements and
cell-centre stress components agree to better than 1e-8 relative L2 in the four
cases. The separate [beam comparison](evidence/2026-10-26-fea-backend/independent-fea.json)
passes as well. This verifies the implementation on those discretizations; it
is not physical evidence.

| L-bracket grid | Solid elements | Load-average deflection (mm) | Maximum centre von Mises (MPa) |
| --- | --- | --- | --- |
| 8 x 8 x 2 | 96 | 0.0319954 | 0.388502 |
| 12 x 12 x 3 | 324 | 0.0345178 | 0.510879 |
| 20 x 20 x 4 | 1200 | 0.0360473 | 0.618042 |

The last deflection change is 4.24% of the fine result. Stress continues to rise
near the sharp corner/clamp and is **not converged**. There is no material
strength or safety-factor verdict. The existing post's example load gives
0.00719272 mm deflection for the illustrative E = 2000 MPa, not a measured spool.

For independent checks use an isolated Python environment with
`scikit-fem==12.0.2 numpy==2.3.5 scipy==1.18.1`, then:

```sh
python scripts/compare-fea-reference.py --exe /path/to/coscad-experiments
python scripts/compare-cad-fea-reference.py --exe /path/to/coscad-experiments
```

Run from the repository root. The sidecars and comparison scripts are executable
acceptance examples, not committed printer dispatches or deployment changes.


### Regression checkpoint history

The first pinned-kernel/CAD checkpoint (35 accepted native examples) passed the
full local suite: **1310 checks, 0 failures**, including 177 seconds of OpenSCAD
geometry/assembly/manufacturing tests. It used an isolated copy of tracked
fixtures and the new implementation, excluding the unrelated untracked
phone-stand design. See [receipt](evidence/2026-10-26-fea-backend/native-cad-validation.json)
for hashes of that exact source snapshot and
[log](evidence/2026-10-26-fea-backend/native-cad-full-regression.log).
The subsequent sphere/chamfer/extrusion expansion is separately checked by the
focused suite (**247 passed**) and the expanded 60-example accepted corpus.
The kernel-disabled focused suite passed **119 checks**; both build configurations
and the embedded Haskell example suite passed. Independent numerical comparisons
were rerun successfully against the current default executable. Do not attribute that
later source to the earlier full-suite hashes. Current focused/build results are
recorded in [expanded-validation.json](evidence/2026-10-26-fea-backend/expanded-validation.json).

## Conforming curved-part FEA checkpoint

The existing `examples/assemble/plan/flat90.coscad` bracket, including its two
through-holes, now runs through native STL, checked Gmsh volume meshing and
CoScad's Tet4 or quadratic Tet10 solver. Tet10 uses affine geometry with shared
edge midpoints, full degree-two integration and the same residual-checked sparse
PCG as the Hex8 baseline. It improves bending without an under-integrated element
or an assumed correction factor. The independent scikit-fem/SciPy implementation
assembles and solves both orders separately, including surface traction and all
nodal displacements and cell-centre stress tensors.

```sh
# First build the optional native kernel as above, or use the default OpenSCAD export.
cabal run --builddir=dist-native -fmanifold coscad -- stl \
  examples/assemble/plan/flat90.coscad --backend=native-experimental -o /tmp/flat90.stl
python3 -m venv /tmp/coscad-fea
/tmp/coscad-fea/bin/pip install -r experiments/requirements-fea.txt
/tmp/coscad-fea/bin/python scripts/mesh-fea-tetra.py /tmp/flat90.stl \
  --size 2 --output /tmp/flat90-tet.json
cabal run coscad-experiments -- tetra2 /tmp/flat90-tet.json \
  experiments/cad/flat90-tetra-loads.json
# Independent check (pass the path returned by cabal list-bin coscad-experiments):
/tmp/coscad-fea/bin/python scripts/compare-tetra-fea-reference.py --order 2 \
  --exe /path/to/coscad-experiments --mesh /tmp/flat90-tet.json \
  --conditions experiments/cad/flat90-tetra-loads.json
```

The mesh adapter pins Gmsh 4.15.2, uses one thread and a fixed seed, checks closed
oriented topology, face connectivity, volume (0.5% limit) and bidirectional sampled
surface distance (at most 0.05 mm). This is finite vertex/face-centre sampling,
**not an exact Hausdorff certificate**. An initial 40-degree surface classification
lost hole detail: 0.218 mm sampled deviation, correctly refused. Preserving the
one-degree facet boundaries reduced the accepted meshes' deviation to about
1e-8 mm and conserved volume and the two holes. The adapter runs in a worker with
a configurable 120-second deadline and only replaces an output after validation.

Sidecars use `coscad.fea-tet-loads.v1`: E in MPa, nu, total force in N and planar
rectangular support/load regions with outward normals. Support clamps all DOFs;
traction integrates the requested TOTAL force. Cut triangles, missing faces,
clamp/load overlap, disconnected bodies, duplicate/inverted/degenerate elements,
nonfinite data and solver nonconvergence fail explicitly. Quadratic analyses are
bounded to 10000 tetrahedra and 20000 elevated nodes; Tet4 input limits are 30000
elements and 15000 nodes. These limits do not guarantee a low memory footprint.

For the synthetic end-clamped flat90 case, E=2000 MPa, nu=0.3 and -1 N in Z:

| Target size | Tet4 displacement Z | Tet10 displacement Z | Tet10 cells |
| --- | --- | --- | --- |
| 4 mm | not measured | -0.044556 mm | 2554 |
| 3 mm | -0.026135 mm | -0.044861 mm | 3469 |
| 2 mm | -0.032494 mm | -0.045192 mm | 6607 |
| 1.5 mm | -0.036490 mm | beyond quadratic experiment cap | 12311 Tet4 |

Tet4's last displacement change is **10.95%**, so it fails the 5% refinement gate
here. Tet10's last change is **0.73%**, passing that fixture's displacement gate.
The independent same-mesh Tet10 displacement errors are below 2e-11 relative L2,
centre-stress errors below 7e-10, with balanced resultant force and moment. See
[tetra2-flat90-2.json](evidence/2026-10-26-fea-backend/tetra2-flat90-2.json), and the
adjacent 3/4 mm and Tet4 receipts. This demonstrates numerical consistency and a
useful deflection trend for this setup. Peak stress convergence is not established;
cell-centre values omit boundary peaks and clamp/reentrant singularities. The
fixture clamps end faces, **not actual bolts or bearing contact**. No sparse
infill, anisotropy, fatigue, failure load or printed-material strength is modeled.

### Second curved fixture: tesseract strut

`examples/assemble/tesseract/bolted/strut.coscad` is a 46 mm long round strut
with a flat printing face. Its existing joint frames are retained as geometry
metadata; this experiment does not model friction in the mating sockets. Use
`strut-tetra-bend.json` or `strut-tetra-axial.json` in `experiments/cad/` after
meshing its STL as above. Both select the full end faces at x=-23 and x=23.

With a 0.1 N transverse end force, Tet10 deflection changed from -0.086872 mm
(2090 cells, target 3 mm) to -0.086991 mm (2969 cells, target 2 mm): **0.14%**.
Both meshes passed the independent solver comparison. With a 1 N axial force
and nu=0, extension was 0.001368817 mm, matching PL/(EA) using the source mesh's
cross-sectional area to about 1e-9 relative. See
[tetra-refinement.json](evidence/2026-10-26-fea-backend/tetra-refinement.json).
The bending solves needed about 5700-6400 PCG iterations, so slender-part
performance and stronger preconditioning remain worthwhile work. These are
small-strain solid-part results, not printed joint strength predictions.

## Native assembly and manufacturing checkpoint

```sh
coscad check examples/assemble/ball/ball.assemble --backend=native-experimental
coscad next examples/assemble/ball/ball.assemble --backend=native-experimental
python3 scripts/compare-native-pipeline.py --exe /path/to/native/coscad
```

`check` renders isolated resolved parts natively and uses Manifold intersections.
Empty hidden instances are valid; kernel failures are errors, never distance-only
success. Inward cavity shells are grouped with their containing solid, fixing the
false-solid interpretation of enclosed voids. This grouping also applies to the
OpenSCAD path. Clearance now includes vertex/face and edge/edge minima plus
triangle crossings, with a bounding-volume hierarchy to prune distant pairs.
This fixes crossed bars whose closest approach lies between edges. Empty or
numerically unresolved distances fail explicitly rather than trying to format
an infinite gap. A triangle crossing uses a 1e-9 mm point-on-surface tolerance;
this remains floating-point geometry, not an exact-arithmetic certificate.

`next` uses the selected backend for each variant, retains orientation/packing
and printability policy, and records `geometry_backend` in the existing manifest
as an additive field. Scores within 1e-6 retain candidate order so rounding cannot
flip equivalent symmetric orientations. It preserves part IDs and declared print
faces. Neither backend publishes an atomic immutable bundle yet.

The comparison script copies tracked fixtures to a temporary directory and tests
frame-stand, bow3, corner, ball and tesseract fit-test assemblies. Native runs use
an intentionally nonexistent OpenSCAD executable, ruling out implicit fallback.
It compares interference verdicts/exits plus variant and placement identities,
orientations, warning codes, mesh volumes and bounds. See
[native-pipeline-comparison.json](evidence/2026-10-26-fea-backend/native-pipeline-comparison.json).
The corner and tesseract fit-test have overlap verdicts in both backends; matching
those diagnostics does not mean those assemblies are interference-free.

### Earlier tetrahedral regression evidence

The complete local native-enabled suite passed **1432 checks, zero failures**,
including the established OpenSCAD geometry, assembly and manufacturing tiers.
The kernel-disabled focused suite passed **160 checks**, and the Haskell example
suite passed. All ten `check`/`next` comparisons passed after the orientation
fix. The Gmsh mesh replay was byte-identical; invalid input and an actual worker
timeout preserved the previous output. The over-budget fine Tet10 mesh failed
explicitly before stiffness assembly. See
[tetra-native-validation.json](evidence/2026-10-26-fea-backend/tetra-native-validation.json)
and [full log](evidence/2026-10-26-fea-backend/tetra-native-full-regression.log).
The full-suite snapshot excludes unrelated untracked user designs. Numerical
comparison receipts are additional runs, not tests of a physical print. No
hosted CI, other-platform binary packaging, default backend switch or deployment
has been performed.

Further extensions and production promotion work:

1. Expand FEA refinement fixtures and improve slender-part solver performance;
   retain explicit rejection of contact/assembly solves.
2. Qualify hard process/resource isolation and cross-platform kernel builds;
   indexed topology, cooperative cancellation and edge-to-edge clearances are
   implemented and covered by the current checkpoint.
3. Add offsets and lofts only against passing geometry oracles; preserve the
   accepted corpus and explicit unsupported errors in the meantime.
4. Run broader regression and release packaging gates before any default switch.
   Physical stiffness/material calibration remains separate from numerical proof.


## Indexed topology and clearance checkpoint

Focused regressions cover point-contact and edge-contact solids, repeated CSG
after a reflection, preservation of separate assembly instances, enclosed
cavities, malformed indices/nonfinite positions, crossed-edge clearance,
parallel/near-parallel segments, rigid-motion invariance and interior triangle
crossings. The accepted 71-example corpus remains intact. A thin rounded panel
also has an explicit OpenSCAD symmetric-difference check: it exposed transient
Minkowski faces during the indexed-boundary change. Rounded cuboids now use
eight corner patches, twelve edge strips and six planar faces directly, the
same convex Minkowski boundary without the general kernel's transient faces.
Both 200 mm and 210 mm panel widths pass the dedicated oracle tests.

The ASan/UBSan boundary test now exercises 500 indexed and 500 legacy soup CSG
operations, 100 extrusions, malformed/null/oversized inputs, empty output,
an actual 1 ms deadline on a dense input, and successful recovery with a fresh
context. It instruments the owned shim and test, not the prebuilt kernel archive.
The updated assembly comparison and full-suite evidence are recorded separately
from the earlier tetrahedral checkpoint so their source hashes remain meaningful.

## Final acceptance audit: useful experimental scope

The user goal was useful, numerically checked FEA and a native backend for
representative existing examples. The following evidence meets that bounded
goal. It does not promote the backend to the default or complete every item in
the longer-term roadmap above.

| Requirement | Implemented behavior and acceptance evidence |
| --- | --- |
| Inventory and supported native geometry | 71 of 74 tracked geometry fixtures pass volume/bounds baselines; the three offset/loft fixtures fail explicitly. The accepted path list prevents coverage shrinking silently. Primitive and selected CSG symmetric-difference oracles, closed/oriented meshes and indexed point/edge-contact cases provide checks beyond volume alone. |
| Usable FEA inputs and results | Beam, exact rectangular CAD and conforming Tet4/Tet10 workflows accept explicit material, clamp and total traction inputs. They return displacement, centre stress, reactions, moment, energy and actual solver residual. Invalid geometry, partial/missing boundary regions and nonconvergence are errors. |
| Analytical and independent numerical checks | Axial beam and strut extension, element patch/rigid-mode tests and reaction/energy checks pass. Independent scikit-fem assembly and direct solves compare all displacements and cell-centre stresses. FEA module hashes are unchanged from the earlier tetrahedral verification. |
| Mesh refinement on actual example kinds | The rectangular L-bracket's last displacement change is 4.24%; Tet10 holed flat90 bracket is 0.73%; Tet10 tesseract strut is 0.14%, each below its 5% gate. The coarse Tet4 bracket failed this gate and is not presented as converged. Peak stresses remain unqualified. |
| Joints, mechanisms and manufacturing | Ten native/OpenSCAD `check`/`next` cases across frame-stand, bow3, corner, ball and tesseract fit-test agree on verdicts, variant/placement identities, orientations and warnings. Native runs cannot invoke OpenSCAD. These exercise geometry and assembly fit, not contact FEA or physical joint strength. |
| Failure handling and preservation | Indexed topology retains distinct touching shells; cavity grouping and edge/edge clearances are tested. Boundary sanitizers cover allocation ownership, malformed inputs, cancellation and recovery. Meshing has a worker deadline and preserves prior output on invalid input or timeout. Default OpenSCAD behavior and unrelated user designs are preserved. |
| Reproducible local validation | The final full suite passed **1473 checks, zero failures**. Native focused/oracle tests passed **362**, kernel-disabled focused tests **182**, and the Haskell examples suite passed. Source hashes, executable hashes, logs and replay receipts are linked below. |

Evidence for this final source:

- [Validation receipt](evidence/2026-10-26-fea-backend/indexed-validation.json),
  [full regression log](evidence/2026-10-26-fea-backend/indexed-full-regression.log),
  [focused oracle log](evidence/2026-10-26-fea-backend/indexed-oracles.log),
  [kernel-disabled log](evidence/2026-10-26-fea-backend/indexed-default-focused.log)
  and [shim sanitizer log](evidence/2026-10-26-fea-backend/indexed-bridge-sanitizers.log).
- [Current corpus inventory](evidence/2026-10-26-fea-backend/coverage-indexed.json),
  [corpus comparison](evidence/2026-10-26-fea-backend/indexed-corpus-comparison.json)
  and [pipeline comparison](evidence/2026-10-26-fea-backend/indexed-pipeline-comparison.json).
- Fresh native STL → Gmsh → Tet10 → independent-solver replays for
  [flat90](evidence/2026-10-26-fea-backend/indexed-tetra2-flat90-3.json) and
  [strut](evidence/2026-10-26-fea-backend/indexed-tetra2-strut-3.json), both at
  3 mm target size. The earlier [refinement receipts](evidence/2026-10-26-fea-backend/tetra-refinement.json)
  retain their original provenance rather than being relabeled as new runs.

Remaining extensions are offsets/lofts, wider profile support, faster slender-part
solves, stress refinement and calibrated printed-material models. Production
promotion additionally needs hard resource isolation, cross-platform kernel and
release testing, immutable artifact publication, and physical validation where
claims depend on it. Cooperative cancellation is not a hard wall-clock bound;
sanitizer coverage of the shim is not instrumentation of the prebuilt kernel.
No hosted CI, deployment, default switch or printer dispatch was performed.

## Publication checkpoint

The all-pending-changes publication also includes the AeroFold phone-stand
example. Its OpenSCAD and native meshes are closed/oriented and agree in volume
and bounds; its SCAD and geometry baselines are now recorded. The full example
corpus now has 75 entries; the earlier 71/74 evidence above refers to its original
snapshot, and the accepted native regression list remains 71 paths.
The final local full suite passed **1475 checks, zero failures**; kernel-disabled
focused tests passed 182 and the Haskell examples passed. The ten pin-policy,
31 slicer, five tank-study and three mocked dispatch checks also passed. See
[publication receipt](evidence/2026-10-26-fea-backend/publication-validation.json)
and [full log](evidence/2026-10-26-fea-backend/publication-full-regression.log).
