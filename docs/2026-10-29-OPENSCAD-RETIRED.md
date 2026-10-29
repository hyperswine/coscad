# OpenSCAD retired

Kind: record of a change. Supersedes the backend sections of
[NATIVE-DEFAULT](2026-10-27-NATIVE-DEFAULT.md) and the build notes of
[FEA-NATIVE-QUICKSTART](2026-10-26-FEA-NATIVE-QUICKSTART.md). Released in
1.6.0.0.

CoScad no longer uses OpenSCAD or BOSL2. Every command meshes parts with the
compiler's own tessellation and the pinned Manifold kernel, and draws images
with the built-in CPU renderer. OpenSCAD stays in one place only: the test
suite's independent oracle, which compares native meshes against OpenSCAD's
renders of the emitted `.scad` when OpenSCAD 2025.05 or newer is installed.

## Why now

Native geometry became the default in 1.4.0.0 and native images in 1.5.0.0.
After that, OpenSCAD was reachable only through `--backend=openscad` and the
`-f-manifold` build. Every tracked example rendered natively, and the opt-out
path failed where native did not: the beach outrigger hit a CGAL assertion
under the OpenSCAD path. Keeping two renderers meant two code paths in `stl`,
`check`, `next`, `plan` and `doctor`, an OpenSCAD-only CI job, and install
instructions for OpenSCAD and BOSL2 that users did not need.

## What changed for users

| Before (1.5) | Now (1.6) |
| --- | --- |
| `--backend=openscad` on stl, check, next, plan, site and stress | An error that says the backend was retired and points at the `.scad` export |
| `--backend=native-experimental` | Still accepted, as is `--backend=native`; both are no-ops |
| Manifest `geometry_backend: "native-experimental"` | `"native"` |
| `coscad doctor` probed OpenSCAD and BOSL2 | Reports the linked kernel and renderer and runs one native round trip |
| Design stage wrote `_asm.scad` and `_plate.scad` to view in OpenSCAD | Also writes `_asm.png` and `_plate.png`, rendered natively; the `.scad` files remain as exports |
| `-f-manifold` built an OpenSCAD-only binary | Removed: the kernel is part of every build |
| `COSCAD_OPENSCAD`, `COSCAD_BOSL2`, `COSCAD_OPENSCAD_TIMEOUT` | Not read by the compiler; the test suite uses the first two for its oracle |
| `capabilities --json` listed `openscad` | Lists `native` only and adds the `openscad-retired` feature |

`coscad part.coscad` still writes OpenSCAD source, for people who continue
in OpenSCAD. The step scenes `plan` writes and the companion `.scad` beside
each STL are the same kind of export. Nothing in CoScad reads them.

## The test oracle

The geometry tier now renders every example natively and compares it with
`test/golden/geometry.txt`. `COSCAD_UPDATE_GOLDEN=1` records goldens from the
native renders. With OpenSCAD installed the tier also renders each example
through OpenSCAD and checks it against the same golden, and runs the
independent probes that only make sense in a second engine: bracket material
at the cube's corners and bores, assembled-solid equivalence of the migration
pairs, and the oriented-corner symmetric difference. The native-versus-OpenSCAD
tier compares primitives, lofts, offsets and selected examples. Without
OpenSCAD, or with `COSCAD_OPENSCAD=none`, those checks are skipped and the skip
is printed in the tier header.

The OpenSCAD runner moved from the library (`src/Coscad/OpenScad.hs`) to the
test suite (`test/Oracle/OpenScad.hs`). `test/golden/native-supported.txt` is
gone: every golden must render natively.

`pipeline.lock.json` keeps the OpenSCAD and BOSL2 pins with unchanged hashes,
moved from the `render` and `source` scopes to `test`. CI keeps an OpenSCAD and
BOSL2 install in its main job for the oracle. The OpenSCAD-only job is replaced
by a no-OpenSCAD job that builds with the kernel, checks that `openscad` is not
installed, runs `coscad doctor` and the whole suite with `COSCAD_OPENSCAD=none`,
and requires the skip announcement. The release workflow now runs every tier,
geometry included, on the shipped binary with the oracle off.

## Code removed

- The `OpenScadBackend` constructor; `Backend` has one constructor, `Native`,
  kept so library and command signatures stay stable.
- The CPP stub branch of `Coscad.Manifold` and the Cabal `manifold` flag.
- `check`'s polyhedron export, far-away marker, CGAL retry and operand-order
  retry; overlaps are the kernel's exact intersection volume.
- `plan`'s OpenSCAD PNG path and its preview-mode note.
- The OpenSCAD and BOSL2 sections of `doctor`.

## Tooling migrated off OpenSCAD

Every script that called the OpenSCAD binary now renders with `coscad stl`
(or reads meshes the pipeline already produced) and does mesh booleans with
manifold3d, the Python binding of the same kernel. None passes `--backend`.
Recorded evidence files were not rewritten; comparison runs went to scratch
directories.

| Script | Change | Result against the recorded OpenSCAD-era numbers |
| --- | --- | --- |
| `models/orbit-tower/verify.py` | probes in manifold3d; meshes found through the manifest | pass; shifted-probe volumes within 7e-6 relative |
| `models/bow-center-reinforced/verify.py` | probes in manifold3d | identical to `validation.json` |
| `models/two-tier-2020-shelf/verify.py` | draws its preview PNGs itself instead of writing `.scad` for OpenSCAD | pass; every volume, bound, hash, hardware path and packing identical |
| `models/beach-outrigger/render.py`, `models/twin-boat-trials/render.py` | `coscad stl` | pass; volumes within 3.4e-7 relative |
| `models/print-in-place-globe/build.py` and `v2-threaded/build.py` | `coscad stl` per part; `--render-only` | pass, zero rotation-sweep intersection; volumes 0.2 to 0.5% lower (see below) |
| `experiments/topological/verify.py` | `coscad stl` and manifold3d; writes `manifold-results.json` | pass on all comparisons, resizes and probes |
| `scripts/compare-native-pipeline.py` | compares the live native pipeline with the recorded receipts instead of a live OpenSCAD run | pass against both receipts |
| `scripts/shelf-bracket-workflow.py` | no `--backend`; accepts both backend names when reading | 4 tests pass; full workflow with images passes |
| `scripts/build-manual.sh`, `scripts/manual-update.sh`, `deploy/` | no OpenSCAD; the updater builds the pinned kernel | not run on the host (see below) |
| `scripts/openscad-stl.sh` | deleted | |

The two globes were modelled at `$fn = 96`, which only the OpenSCAD path
honoured; native tessellation uses 50 segments for every round primitive. A
rebuilt globe is therefore slightly smaller (stand 49273 against 49367 mm³,
rotor 137391 against 138054 mm³) and the faceted bore clearance is 0.594
against 0.598 mm. The physically tested prints came from the 96-segment
meshes, so a rebuilt globe needs re-slicing, and v2 a new thread coupon,
before printing. A segment-count setting in the language is the follow-up.

## Results

Local, macOS arm64, OpenSCAD 2025.05.16 installed for the oracle run:

| Run | Checks passed | Failed |
| --- | --- | --- |
| Full suite with the OpenSCAD oracle | 1646 | 0 (an earlier run missed a 0.5 s timing check while a 4.5-minute globe rebuild ran alongside; the idle rerun passed) |
| Full suite with `COSCAD_OPENSCAD=none` | 1409 | 0 |
| `haskell-examples` | pass | |
| VS Code extension tests | pass | |
| Docs agreement, pipeline lock, slicer and preflight tests | pass | |

The 237-check difference between the two suite runs is the oracle: OpenSCAD
renders of every example and the native-versus-OpenSCAD comparisons. Every
native geometry, pipeline, image, stress and CLI check runs in both.

The manual site host (the Mac mini behind manual.cswine.cloud) has not
updated for a while for an unrelated reason: its checkout fails because
`git-lfs` is not installed there. It also lacks `cmake`, which the kernel build
needs. `brew install git-lfs cmake` on that host, then reinstalling
`deploy/cloud.cswine.manual-update.plist`, should let it resume.

## Not changed

- Native refusals stay explicit errors: polygon paths with holes, spindle and
  horn tori, an offset inside a loft profile, nested offsets and offsets of
  composite 2D shapes. No tracked file uses them. OpenSCAD used to render them;
  now a design needing one should express it differently, for example a hole
  as a 2D difference.
- There is no fallback renderer. A Manifold failure on a future design is
  fixed in the design or the kernel input.
- Historical records under `models/` and `docs/evidence/` keep their
  OpenSCAD-era numbers and provenance; they describe what was run at the time.
