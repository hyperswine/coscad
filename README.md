# coscad

A concise glyph/pipeline CAD language in Haskell that compiles to OpenSCAD
(optionally BOSL2), with an assembly layer and a manufacturing stage that
emits print-ready per-bed STLs. Intelligence lives upstream in the compiler
— anchors, beziers, attachments, fit, orientation, and packing are all
computed here so the emitted OpenSCAD (and the downstream slicer) stay dumb.

```
.coscad  ──compile──▶  .scad ──openscad──▶ .stl        (one part)
.assemble ─coscad───▶  asm view + plate + _design.json  (design stage)
.assemble ─coscad next▶ bedN.stl + _manifest.json       (manufacturing)
manifest  ─bambu-slice▶ bedN.gcode.3mf → printer        (scripts/bambu-slice.py)
```

## Install

On Apple Silicon with macOS 15 or newer, install the prebuilt native-kernel
release from the shared [Homebrew tap](https://github.com/hyperswine/homebrew-tap):

```sh
brew install hyperswine/tap/coscad
coscad --version
```

This downloads a checksum-verified binary; GHC, Cabal and a source checkout
are not required. It installs `coscad`, `man coscad`, the docs under
`$(brew --prefix)/share/doc/coscad`, and the examples under
`$(brew --prefix)/share/coscad/examples`.

Prebuilt binaries for macOS and Linux are attached to each GitHub
release (`coscad-<os>-<arch>.tar.gz`) with the man page
and examples; put `coscad` on your PATH and, if you want `man coscad`,
copy `man/coscad.1` into a `man1` directory on your MANPATH. Or build
from source with [cabal](https://www.haskell.org/cabal/) and GHC 9.8
(both installable with [ghcup](https://www.haskell.org/ghcup/); the
dependency set is pinned in `cabal.project.freeze`). The default build
links the pinned native geometry kernel (Manifold 3.5.2 plus Clipper2,
`native-kernel.lock.json`), which needs CMake, pkg-config and a C++17
compiler; `scripts/build-manifold.py` fetches the two pinned checkouts,
builds them statically under `.native/` and never downloads during the
kernel build itself:

```sh
python3 scripts/build-manifold.py
export PKG_CONFIG_PATH="$PWD/.native/manifold-3.5.2/install/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
cabal update
cabal install exe:coscad --install-method=copy --installdir=bin   # binary lands in bin/
```

Without the kernel, `cabal install exe:coscad -f-manifold ...` builds the
OpenSCAD-only binary: every render goes through OpenSCAD and
`--backend=native` is refused. `coscad doctor` says which build you have.

macOS note: ghcup's GHC links against the Command Line Tools SDK. If a
CLT update leaves that SDK newer than Xcode's linker, linking fails with
`tapi error ... unknown architecture arm64e.x1-macos`. Until Xcode and
the CLT match again, tell GHC to link with the CLT toolchain by adding
to `cabal.project.local` (git-ignored):

```
package coscad
  ghc-options: -pgml /Library/Developer/CommandLineTools/usr/bin/clang
```

In a kernel build, `coscad stl`, `coscad check` and `coscad next` render
natively by default (`--backend=openscad` opts out per command; the
manufacturing manifest records the backend used). OpenSCAD is still
needed: for that opt-out, for the planner's step images (`plan --png`,
`site`), and for the test suite. Install
[OpenSCAD](https://openscad.org) 2021.01 or newer (2025.05 or newer
renders STL through its own Manifold backend, which `coscad` requests
when available) and a [BOSL2](https://github.com/BelfrySCAD/BOSL2)
checkout in an OpenSCAD library folder (`~/Documents/OpenSCAD/libraries`
on macOS, `~/.local/share/OpenSCAD/libraries` on Linux) — or anywhere,
with `COSCAD_BOSL2=/path/to/BOSL2`. Check the whole chain with:

```sh
coscad doctor
```

which reports the default backend and the linked kernel version, the
OpenSCAD and BOSL2 versions it found, and renders a boolean test part
through every available backend against a known volume.

## Quick start

The working compiler-to-printer toolchain is recorded in
[`pipeline.lock.json`](pipeline.lock.json), with QOS-style repository revisions
and content hashes for local tools, profiles and deployed artifacts. Use
`python3 scripts/pipeline-lock.py --scope source --scope render --check` to
detect drift in a plain checkout, or
`python3 scripts/pipeline-lock.py --scope source --scope render --run coscad doctor`
to run through the checked entry point. A bare `--check` (no `--scope`) is the
complete check: it also SSHes to the print host (`root@localhost-0`) and hashes
the local printer profiles, so it only passes on the maintainer's machine. See
[pins, deliberate upgrades and graceful recovery](docs/2026-10-15-PIPELINE-LOCK.md).

```sh
coscad part.coscad           # -> part.scad
coscad validate part.coscad # source checks, no output files
coscad validate model.assemble # source + rigid constraint checks
coscad validate --json --stdin part.coscad < buffer  # JSON diagnostics for unsaved text
coscad validate --json --project-stdin model.assemble < snapshot.json # recursive editor buffers
coscad capabilities --json # build features, supported backends and materials
coscad stl part.coscad       # -> part.scad + part.stl, prints volume + bounds
coscad frame.assemble        # -> _asm view, packed _plate(N) scads, frame_design.json
coscad next frame.assemble   # -> frame_bed1.stl ... + frame_manifest.json
coscad check frame.assemble  # interference / clearance check on real meshes
coscad stress bracket.coscad # deflection, peak stress and safety factor from the part's support/load lines
coscad plan frame.assemble   # numbered build steps, preload sheet, BOM, step scenes
coscad site out/ *.assemble  # phone-first static site: step images, search across builds
scripts/bambu-slice.py frame_manifest.json [--print 1]  # slice beds with Bambu Studio, optionally print over LAN
scripts/bambu-print-run.py frame.assemble  # photo, model → slice → print, watch, photo, verdict
coscad --help                # command + language cheat sheet
```

The two Bambu scripts need Bambu Studio; printing over the LAN also needs
`python3 -m pip install -r scripts/requirements.txt` (paho-mqtt) and the
bambu-lan scripts (`BAMBU_LAN_DIR`). The VS Code extension lives in
`vscode/` (`npm install && npm test && npm run package`, or take the
`.vsix` attached to each release).

`COSCAD_OPENSCAD` points at the OpenSCAD binary or a wrapper script
(e.g. `xvfb-run -a openscad "$@"` on a headless machine); otherwise
PATH and the usual install locations are searched.

Errors carry `file:line:col` and quote the offending line — an unknown
name, a stray glyph, a circular definition, a duplicate, or a 2D/3D
mismatch (extruding a solid, offsetting a cube, unioning a profile with
a box) all fail at compile time instead of rendering silently wrong.
OpenSCAD warnings during a render (a missing include, a dropped child)
are treated as failures too. Every diagnostic goes to stderr as
`Error: file:line:col: reason`, optionally followed by an excerpt; exit 1
means the command could not run, exit 2 means the input was fine but the
design has a problem (`validate`/`plan` design errors, `check` overlaps,
`next --strict` printability warnings, a part that does not fit the plate
in the design stage or `next`, and, from every command that loads an assembly, mating constraints that
contradict each other, parts no fixed root reaches, or an instance fixed
twice). A constraint conflict is reported on the line that closed the loop,
followed by one `  note: file:line:col: …` line per constraint on the loop.
A single OpenSCAD run is killed after `COSCAD_OPENSCAD_TIMEOUT` seconds
(default 600) instead of hanging a pipeline.

## Two syntaxes, one language

Glyph style and pipeline style are interchangeable and mix freely:

```
// glyphs
bracket = (plate ⊕ flange) ⊖ χ 10 (zcyl 2.75 12)

// pipelines + word aliases
bracket = plate
  |> add flange
  |> cutat top 2.5 0 -4 (zcyl 2.75 12)
```

A line starting with `|>` continues the previous definition.

## Feature map

- **Shapes**: plain OpenSCAD glyphs (`■ ● ◎ ▻ ▬ ⎏`), BOSL2 family
  (`▣ ◙ ⌭ ⌽ ⊚ ⏢ ◉ ⊿`, `xcyl/ycyl/zcyl`), centered word shapes
  (`cube box sphere cyl tube torus wedge`), 2D profiles + `⮕`/`extrude`,
  and `✎` — compiler-evaluated piecewise cubic bezier outlines.
- **Numbers**: `w = 20`, `t = w / 5 - 1`; use anywhere a number goes
  (`box w (w / 2) t`). Resolved before shapes, mistakes name the binding.
- **Lofts**: `loft 0 (⭘ 10) 30 (△ 5)` / `p |> loft h q` skins 2D
  profiles (any vertex counts) into a solid via BOSL2 `skin()`.
- **Booleans**: `⊕ ⊖ ∩ ⇓ ⊞ ↯` and pipeline stages `add cut isect hull mink`.
- **Topological modelling**: bbox anchors (`top bot lft rt fwd bak ctr`,
  combos like `lft+fwd`), relational ops `⌖`/`at`, `⋈`/`on`, `cutat`
  with offsets, and `⚓`/`anchor` re-origining. See docs/2026-07-24-TOPOLOGICAL.md.
- **Assemblies**: `.assemble` files declare physically separate parts
  (recursive references, counts, print orientation `▽`, free hints).
  Union = one solid; separate reference = separate object.
- **Build plans**: `coscad plan` orders an assembly for a human — nuts
  before the slot they need is covered, every screw driven with the
  driver clear, each step on a stable rest face, torque per host
  material — from `fastener` lines and a few part hints. Emits the
  step list, preload sheet, BOM, and per-step renders; `coscad site`
  turns the plans of several assemblies into a static companion site
  you can read on a phone. See docs/2026-09-25-PLAN.md.
- **Manufacturing**: `coscad next` compiles each unique
  (part, orientation) variant once, searches FFF print orientation
  where undeclared within the bed height (`plate W D margin maxZ`,
  default 250 mm; 180 for an A1 mini), packs beds first-fit decreasing
  (gaps filled by later parts, 90° turns when they save a bed), and emits
  per-bed STLs plus a variants+placements manifest (`foo_manifest.json`,
  schema `coscad.next/1`) for slice-once /
  stamp-many slicing; `coscad check` verifies fits on the real meshes.
- **Stress**: `coscad stress` solves a part's `support`/`load` lines with
  20-node hexahedra in Haskell and reports deflection, peak stress and
  safety factors (docs/2026-10-27-STRESS-COMMAND.md).
  See docs/2026-07-24-MANUFACTURING.md.

See [the shelf bracket integration workflow](docs/2026-10-27-EDITOR-WORKFLOW.md)
for editor diagnostics, backend selection and a reproducible stress-to-slice example.

## Module map

```
src/Coscad/Shape.hs     AST + bezier evaluation (pure data)
src/Coscad/Linalg.hs    vectors, 3x3 matrices, rotations, bed orientation
src/Coscad/Geometry.hs  bboxes, anchors, attachment resolve (re-exports Linalg)
src/Coscad/Codegen.hs   OpenSCAD emission
src/Coscad/Dsl.hs       Haskell-embedded glyph DSL
src/Coscad/Parser.hs    .coscad parser (glyphs, words, pipelines)
src/Coscad/Dim.hs       2D/3D dimensionality check
src/Coscad/Validate.hs  shared numeric/quantity checks and suggestions
src/Coscad/Constraint.hs  mate/fixed constraint solver, conflicts, tolerance
src/Coscad/Assemble.hs  .assemble design stage
src/Coscad/Variants.hs  print variants: names, instance ids, orientation, plate packing
src/Coscad/Next.hs      manufacturing stage (orientation, beds, manifest)
src/Coscad/Printability.hs  bed contact, overhang, slenderness, warnings
src/Coscad/Check.hs     assembly interference / clearance checker
src/Coscad/Plan.hs      `coscad plan` CLI and summaries, over:
  Plan/Model.hs         instances, fasteners, requires-before relation
  Plan/Search.hs        stability/access predicates, A*/beam search, steps
  Plan/Render.hs        plan Markdown, JSON, per-step scenes
src/Coscad/Site.hs      `coscad site` static build-instruction pages
src/Coscad/Mesh.hs      ASCII STL in/out, volume, bounds
src/Coscad/Json.hs      minimal JSON encode/decode for manifests
src/Coscad/IO.hs        UTF-8 file IO, scratch directories
src/Coscad/Report.hs    command failures (exit status + lines) and warning emitters
src/Coscad/Format.hs    number formatting (showD)
src/Coscad/OpenScad.hs  finding + running OpenSCAD and BOSL2
src/Coscad/Mesh/Distance.hs triangle/edge surface distance with bounding-volume pruning
src/Coscad/Mesh/Indexed.hs  validated indexed mesh contract; connectivity independent of position
src/Coscad/Backend.hs   STL/check/next backend boundary: build-time default, native meshing and CSG
src/Coscad/Loft.hs      lofts tessellated as BOSL2 skin() builds them (reindex/distance, min_edge, caps)
src/Coscad/Manifold.hs  optional pinned C++ geometry kernel bridge
src/Coscad/Stress/Experiment.hs  bounded isotropic Hex8 solver and beam validation
src/Coscad/Stress/Cad.hs  exact rectilinear CAD grid, supports and surface loads
src/Coscad/Stress/Tetra.hs  experimental conforming Tet4/Tet10 solver for curved parts
src/Coscad/Stress/Linear.hs  shared sparse solver and residual checks
src/Coscad/Stress/Spec.hs    support/load/settings directives
src/Coscad/Stress/Material.hs material properties and strength assumptions
src/Coscad/Stress/Hex20.hs   quadratic hexahedra and stress recovery
src/Coscad/Stress/Sparse.hs  CSR matrix and preconditioned solver
src/Coscad/Stress/Voxel.hs   mesh voxelization and connectivity
src/Coscad/Stress/Part.hs    part regions, solve and safety report
src/Coscad/Stress/Command.hs stress CLI and report output
src/Coscad/Part.hs      single-part compile / stl commands
src/Coscad/Diagnose.hs  `coscad validate`, structured diagnostics, --json output
src/Coscad/Doctor.hs    `coscad doctor`
src/Lib.hs              re-export shim for the embedded Haskell DSL
app/Main.hs             CLI dispatch only
experiments/Main.hs     coscad-experiments numerical trial CLI (JSON results)
examples/haskell/       embedded-DSL samples (compile-checked by the haskell-examples suite)
```

## Manual

`man coscad` (source: `man/coscad.1`) covers every command, the
environment variables, a language cheat sheet, the assembly format, and
worked examples. `coscad --help` is the short form.

## Manual site

The [generated Orbit Tower instructions](https://manual.cswine.cloud/builds/orbit-tower/index.html)
walk through building the 20-piece spinning desktop kit, with a render for
each step. A separate [hand-written guide](https://manual.cswine.cloud/orbit-tower.html)
includes handling and print notes.

`scripts/build-manual.sh DIR` renders this documentation, the man page,
and the example build plans into a static site (needs pandoc, mandoc,
OpenSCAD). manual.cswine.cloud rebuilds it from `main` automatically; see
`deploy/README.md`.

## Releasing

1. Bump `version` in `coscad.cabal` and the CHANGELOG, commit, tag
   `v<version>`, push the tag: the release workflow runs the pure test
   tiers, checks the tag against `coscad --version`, and attaches binaries.
2. After the release succeeds, run `scripts/update-release.py coscad v<version>`
   in the shared `homebrew-tap` repository. It downloads the archive and verifies
   the published checksum and build metadata before updating the binary formula.

## Print photos

Camera photos from print runs (`models/**/*.jpg`, `.jpeg`, `.heic`, such as
`photos-before/` and `*-bed-photos/`) are stored in [Git LFS](https://git-lfs.com).
Install it once with `git lfs install` before cloning, or run `git lfs pull`
afterwards. Without LFS those files check out as small pointer files.
Nothing in the build or the test suite reads them. Renders and diagrams
(`*.png`) stay as ordinary files.

## Docs

[docs/README.md](docs/README.md) indexes every page by date. The ones to start with:

- [Language reference](docs/2025-07-14-LANGEXTENSION.md)
- [Topological modelling](docs/2026-07-24-TOPOLOGICAL.md): anchors, pipelines, and the stable-datum rule
- [Manufacturing](docs/2026-07-24-MANUFACTURING.md): .assemble and coscad next
- [Build plans](docs/2026-09-25-PLAN.md): fasteners, material hints, and coscad plan
- [Examples index](docs/2025-08-11-EXAMPLES.md)
- [Agent skill file](docs/SKILL.md): gotchas and the verification workflow
- [Stress analysis and a native backend](docs/2026-10-17-FEA-AND-BACKEND.md): intent for `coscad stress` and a native geometry backend
- [FEA/backend experiments](docs/2026-10-26-FEA-BACKEND-EXPERIMENTS.md): runnable CAD/beam FEA and pinned native CSG, validation results and promotion gates
- [Native backend by default](docs/2026-10-27-NATIVE-DEFAULT.md): the native kernel becomes the default for `stl`, `check` and `next`; offsets and lofts; timings and affected goldens
- [Try FEA and native geometry](docs/2026-10-26-FEA-NATIVE-QUICKSTART.md): build, mesh and analyze the existing bracket and strut examples

## Tests

```sh
cabal test                          # all tiers; geometry tier needs OpenSCAD
COSCAD_RENDER=0 cabal test          # pure tiers only (a few seconds)
COSCAD_UPDATE_GOLDEN=1 cabal test   # accept changed .scad / geometry snapshots
```

The suite's entry point is `test/Spec.hs`; the tiers live in `test/Tier/*.hs` and share `test/Harness.hs`. The main tiers: diagnostics (error messages carry
position and cause), examples (every example compiles, every
`.assemble` loads, emitted `.scad` matches `test/golden/scad/`), and
geometry (every example renders through OpenSCAD and its mesh volume +
bounds match `test/golden/geometry.txt`, plus cross-checks: the
topological bracket equals the coordinate bracket, `coscad next`
conserves volume across bed packing, `coscad check` passes on bow3), and cli (the built binary is spawned:
exit codes, stdout/stderr split, `validate`, and the manifests parse as
JSON).
When a snapshot fails, the message shows golden vs current; accept
deliberate changes with `COSCAD_UPDATE_GOLDEN=1` and review the diff.
Geometry entries still within tolerance keep their recorded values, so
an update only rewrites the lines that drifted or were missing.
OpenSCAD is found via `COSCAD_OPENSCAD`, then PATH, then the macOS app
bundle.
A second, small suite, `haskell-examples`, only compiles
`examples/haskell/` and renders one shape from each sample.

Tests generate everything they read. Build output (scenes, manifests,
plans, step images) is gitignored; only the print records under `models/`
and the tesseract `verification.json` are committed. See
[Examples](docs/2025-08-11-EXAMPLES.md) for the list.

## Verification culture

Every nontrivial part in the examples was validated numerically, not
visually: trimesh watertightness + volume/bounds cross-checks, boolean
interference tests for fits and fastener paths (expect 0 mm³), and
ASCII occupancy rasters for 2D outlines. docs/SKILL.md describes the
loop; copy it for new parts.
