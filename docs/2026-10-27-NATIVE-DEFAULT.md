# The native backend becomes the default

> Since 1.6.0.0 the native backend is the only one: `--backend=openscad` and the
> `-f-manifold` build are gone. See [OPENSCAD-RETIRED](2026-10-29-OPENSCAD-RETIRED.md).

Kind: record of a change, with its measurements. Supersedes the "no default
backend switch yet" status in
[FEA-BACKEND-EXPERIMENTS](2026-10-26-FEA-BACKEND-EXPERIMENTS.md); the
numerical FEA work there is untouched and remains an opt-in experiment.

## What changed

- **Default selection.** `Coscad.Backend.defaultBackend` is
  `NativeExperimental` when the pinned kernel is linked (`manifoldAvailable`)
  and `OpenScadBackend` otherwise. `coscad stl`, `coscad check` and `coscad
  next` use it when no `--backend` is given. `--backend=openscad` is the
  explicit opt-out; `--backend=native` is accepted as an alias of the
  original spelling `native-experimental` (nothing was renamed: manifests
  still record `native-experimental`). `--help` prints the default of the
  binary at hand; `coscad doctor` reports both backends, the default, the
  kernel version and pin, that OpenSCAD is still needed (for the opt-out and
  for `plan --png` / `site` step images), and renders its test part through
  every available backend.
- **Cabal flag.** `manifold` now defaults to `True` (still `manual`, so
  `cabal build -f-manifold` is the OpenSCAD-only build of before, in which
  `--backend=native` is refused with a message naming the flag). The kernel
  is found through pkg-config: `manifold ==3.5.2` and `Clipper2 ==2.0.1`,
  the latter because `manifold.pc` lists Clipper2 only under
  `Requires-private` and the link is static.
- **Kernel pin.** `native-kernel.lock.json` adds Clipper2 at Manifold
  3.5.2's own FetchContent revision (`46f63917…`, Clipper2 2.0.1) and turns
  `MANIFOLD_CROSS_SECTION=ON` with the builtin Clipper2.
  `scripts/build-manifold.py` checks out both pins, refuses dirty or moved
  checkouts, points Manifold's FetchContent at the local Clipper2 checkout
  with `FETCHCONTENT_FULLY_DISCONNECTED=ON` (so the kernel build still never
  downloads), and records both library hashes in the install receipt.
- **Native coverage.** Two gaps closed, so every tracked example renders
  natively:
  - `↯` (OpenSCAD `offset(r = …)`): a new shim entry
    `coscad_mesh_extrude_offset` validates one simple outline exactly as the
    plain extrusion does, offsets it with
    `CrossSection::Offset(delta, Round, 2.0, 50)` (OpenSCAD derives the same
    arc tolerance from `$fn = 50`, and both sides are Clipper2), extrudes the
    resulting contours and returns the mesh under the existing ownership and
    error rules. An offset that removes the profile, an offset of a composite
    2D shape, a nested offset and an offset inside a loft profile are
    explicit errors.
  - `loft`: `Coscad.Loft` tessellates lofts as BOSL2's `skin()` does:
    profiles in BOSL2's path order (`circle()` is clockwise from angle 0,
    polygons go through `ccw_polygon`), `reindex` (equal vertex counts:
    rotate each profile to the least total distance from the previous one)
    and `distance` (the dynamic programme over every cyclic shift of the
    larger profile, with vertex duplication; the first best shift and the
    diag/left/up tie order of `min_index` are kept), `slices = 0`, the
    `min_edge` quad split with BOSL2's 1e-9 tolerance, its degenerate-face
    cull, ear-clipped caps, and orientation by the sign of the volume. The
    method is taken from `Coscad.Codegen.loftMethod`, so the native and the
    OpenSCAD renders always agree on which correspondence is used. Both
    methods are compared against OpenSCAD; a reindex loft is the same
    triangulation on both sides and agrees to 1e-9 relative volume.
- **OpenSCAD path.** `renderStlFile`, `runOpenscad` and `check`'s overlap
  intersections pass `--backend=manifold` when the installed OpenSCAD
  understands `--backend` (the probe the planner images already used;
  `openscadStlArgs` takes the flag, `openscadStlArgsIO` probes, 0.1 s per
  call). Older OpenSCAD builds keep their CGAL arguments. One finding on
  the way: OpenSCAD's Manifold backend is multithreaded and, on solids with
  *exactly* coincident faces, not stable from run to run. The test suite's
  symmetric-difference probes (two equal solids must leave 0 mm³) are such
  inputs: four of them failed in one full run and passed in the next with
  the same OpenSCAD binary and SCAD text. Those exactness certificates now
  render through `renderStlFileWith False` (CGAL, exact arithmetic), and
  the library exports that choice for any caller with the same need. User
  models rarely subtract a solid from an identical copy of itself; the
  goldens of all 75 tracked examples were unaffected across three full runs.
- **CI.** `ci.yml` and `release.yml` build the kernel with
  `scripts/build-manifold.py` before `cabal build`, caching
  `.native/manifold-3.5.2/install` by the lock file and the builder; `ci.yml`
  keeps an `openscad-only` job that builds and tests with `-f-manifold`.
  The Homebrew formula is unchanged and will need the kernel build step (or
  `-f-manifold`) before the next tagged release; the changelog says so.

## Measurements (this Mac, Apple silicon, OpenSCAD 2025.05.16, BOSL2 2.0.775)

| Case | native (default) | `--backend=openscad` | note |
| --- | --- | --- | --- |
| `coscad stl models/beach-outrigger/beach-outrigger.coscad` | 2.23 s, 343534.021 mm³ | 2.95 s, 343533.904 mm³ | before this change the OpenSCAD path failed with a CGAL assertion (STL renders did not pass `--backend=manifold`); the two volumes agree to 3.4e-7 |
| `coscad check examples/assemble/ball/ball.assemble` | 0.79 s, 0 overlaps, 0 warnings | 2.57 s, 0 overlaps, 0 warnings | same verdicts |
| `coscad stl examples/loft.coscad` | 0.66 s, 8252.72 mm³ | golden 8252.719 mm³ | reindex outer/inner lofts plus a distance-method lug |
| `coscad stl examples/offset_demo.coscad` | 0.02 s, 8465.956 mm³ | golden 8465.956 mm³ | bounds within 0.003 mm |
| `coscad stl examples/waterbottle_advanced.coscad` | 0.10 s, 58945.535 mm³ | golden 58945.535 mm³ | three offsets, unions and differences |
| `coscad stl examples/aerofold_phone_stand.coscad` | 0.28 s, 76996.245 mm³ | golden 76996.244 mm³ | bezier extrusions; now in the accepted native list |

The native timings were taken with `COSCAD_OPENSCAD` pointing at a
nonexistent file, so no OpenSCAD process could have been involved.

## Goldens

`test/golden/native-supported.txt` grows from 71 to 75 paths
(`aerofold_phone_stand`, `loft`, `offset_demo`, `waterbottle_advanced`),
the whole tracked example corpus. `test/golden/geometry.txt` is unchanged:
the switch of OpenSCAD's STL renders to its Manifold backend moved no
tracked example beyond the existing tolerance (1e-3 relative volume, 0.01 mm
bounds), so nothing was re-recorded. (The one geometry-tier change is in the test harness, not the goldens: the exactness probes render through CGAL, see above.)

## Tests

Full suite, `COSCAD_BOSL2=/path/to/BOSL2 cabal test coscad-test --test-show-details=direct`:

| Configuration | Result | Before |
| --- | --- | --- |
| default build (kernel linked, `--builddir=dist-native`) | **1542 passed, 0 failed** (geometry tier 117 s) | 1475 |
| `-f-manifold` (`--builddir=dist-nomanifold`) | **1339 passed, 0 failed** (geometry tier 139 s) | 1322 |
| `haskell-examples`, both configurations | pass | pass |

`python3 scripts/check-docs.py` (README, man page and `--help` agree) and
`python3 scripts/pipeline-lock.py --check-ci` (every workflow action and the
BOSL2 revision pinned) pass with the workflow changes. The suite was run three
times in full on the way; the only non-deterministic failure seen was the
coincident-face Manifold case described above, now routed through CGAL, and a
0.5 s resolution-timing budget missed by 25 ms while two other builds were
running on the same machine (it passed on the idle re-runs).

New checks: the default backend follows the build (pure, and through the
CLI with OpenSCAD unreachable: a kernel build renders `stl`, `next` and
`check` natively with no flag, a kernel-less build names OpenSCAD as the
missing piece); `--backend=native` is accepted; `--backend=openscad` still
renders `stl`, and `next --backend=openscad` records `openscad` in its
manifest while the default `next` records `native-experimental`; lofts and
offsets against OpenSCAD on volume and bounds (three reindex and three
distance lofts, five offsets including a negative one and a bezier, and the
four example files), each native mesh closed and positively oriented; the
pure loft tessellation without a kernel (BOSL2's frustum formula to 1e-9,
the clockwise circle and `ccw_polygon` conventions, a distance loft, refusals
of mismatched counts, offsets inside profiles and 3D profiles, and the cap
triangulator on a concave outline, repeated points and a self-intersecting
input); the refusal fixtures now use a spindle torus, which stays
unsupported.

## Open items

- An offset *inside* a loft profile (BOSL2's `offset(path, r=, closed=true)`
  is a round path offset, not Clipper's) and offsets of composite 2D shapes
  (2D booleans under `↯`) are explicit native errors; no tracked example
  uses them.
- Arc discretisation of offsets comes from Clipper2 2.0.1 in the kernel and
  from the Clipper2 bundled with the installed OpenSCAD; they agree within
  the goldens' tolerance here, and the oracle tests hold them to 1e-3
  relative volume, not to identical vertices.
- The Homebrew formula still builds without the kernel step.
- Hard resource isolation, cross-platform kernel testing and immutable
  artifact publication remain as listed in the 2026-10-26 record.
