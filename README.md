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

Homebrew (macOS and Linux), from this repository's tap:

```sh
brew tap hyperswine/coscad https://github.com/hyperswine/coscad
brew install coscad          # tagged release; --HEAD for current main
```

That installs `coscad`, `man coscad`, the docs under
`$(brew --prefix)/share/doc/coscad`, and the examples under
`$(brew --prefix)/share/coscad/examples`.

Prebuilt binaries for macOS and Linux are attached to each GitHub
release (`coscad-<os>-<arch>.tar.gz`) with the man page
and examples; put `coscad` on your PATH and, if you want `man coscad`,
copy `man/coscad.1` into a `man1` directory on your MANPATH. Or build
from source with [cabal](https://www.haskell.org/cabal/) and GHC 9.8
(both installable with [ghcup](https://www.haskell.org/ghcup/); the
dependency set is pinned in `cabal.project.freeze`):

```sh
cabal update
cabal install exe:coscad --install-method=copy --installdir=bin   # binary lands in bin/
```

macOS note: ghcup's GHC links against the Command Line Tools SDK. If a
CLT update leaves that SDK newer than Xcode's linker, linking fails with
`tapi error ... unknown architecture arm64e.x1-macos`. Until Xcode and
the CLT match again, tell GHC to link with the CLT toolchain by adding
to `cabal.project.local` (git-ignored):

```
package coscad
  ghc-options: -pgml /Library/Developer/CommandLineTools/usr/bin/clang
```

Rendering needs [OpenSCAD](https://openscad.org) 2021.01 or newer and a
[BOSL2](https://github.com/BelfrySCAD/BOSL2) checkout in an OpenSCAD
library folder (`~/Documents/OpenSCAD/libraries` on macOS,
`~/.local/share/OpenSCAD/libraries` on Linux) — or anywhere, with
`COSCAD_BOSL2=/path/to/BOSL2`. Check the
whole chain with:

```sh
coscad doctor
```

which reports the OpenSCAD and BOSL2 versions it found and renders a
boolean test part against a known volume.

## Quick start

The working compiler-to-printer toolchain is recorded in
[`pipeline.lock.json`](pipeline.lock.json), with QOS-style repository revisions
and content hashes for local tools, profiles and deployed artifacts. Use
`python3 scripts/pipeline-lock.py --check` to detect drift, or
`python3 scripts/pipeline-lock.py --scope source --scope render --run coscad doctor`
to run through the checked entry point. See
[pins, deliberate upgrades and graceful recovery](docs/PIPELINE-LOCK.md).

```sh
coscad part.coscad           # -> part.scad
coscad validate part.coscad # source checks, no output files
coscad validate model.assemble # source + rigid constraint checks
coscad stl part.coscad       # -> part.scad + part.stl, prints volume + bounds
coscad frame.assemble        # -> _asm view, packed _plate(N) scads, frame_design.json
coscad next frame.assemble   # -> frame_bed1.stl ... + frame_manifest.json
coscad check frame.assemble  # interference / clearance check on real meshes
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
and, from every command that loads an assembly, mating constraints that
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
  with offsets, and `⚓`/`anchor` re-origining. See docs/TOPOLOGICAL.md.
- **Assemblies**: `.assemble` files declare physically separate parts
  (recursive references, counts, print orientation `▽`, free hints).
  Union = one solid; separate reference = separate object.
- **Build plans**: `coscad plan` orders an assembly for a human — nuts
  before the slot they need is covered, every screw driven with the
  driver clear, each step on a stable rest face, torque per host
  material — from `fastener` lines and a few part hints. Emits the
  step list, preload sheet, BOM, and per-step renders; `coscad site`
  turns the plans of several assemblies into a static companion site
  you can read on a phone. See docs/PLAN.md.
- **Manufacturing**: `coscad next` compiles each unique
  (part, orientation) variant once, searches FFF print orientation
  where undeclared within the bed height (`plate W D margin maxZ`,
  default 250 mm; 180 for an A1 mini), packs beds first-fit decreasing
  (gaps filled by later parts, 90° turns when they save a bed), and emits
  per-bed STLs plus a variants+placements manifest (`foo_manifest.json`,
  schema `coscad.next/1`) for slice-once /
  stamp-many slicing; `coscad check` verifies fits on the real meshes.
  See docs/MANUFACTURING.md.

## Module map

```
src/Coscad/Shape.hs     AST + bezier evaluation (pure data)
src/Coscad/Geometry.hs  vectors, bboxes, anchors, attachment resolve
src/Coscad/Codegen.hs   OpenSCAD emission
src/Coscad/Dsl.hs       Haskell-embedded glyph DSL
src/Coscad/Parser.hs    .coscad parser (glyphs, words, pipelines)
src/Coscad/Dim.hs       2D/3D dimensionality check
src/Coscad/Assemble.hs  .assemble design stage (+ plate packing)
src/Coscad/Next.hs      manufacturing stage (orientation, beds, manifest)
src/Coscad/Check.hs     assembly interference / clearance checker
src/Coscad/Mesh.hs      ASCII STL in/out, volume, bounds
src/Coscad/OpenScad.hs  finding + running OpenSCAD and BOSL2
src/Coscad/Part.hs      single-part compile / stl commands
src/Coscad/Doctor.hs    `coscad doctor`
src/Lib.hs              re-export shim for the embedded Haskell DSL
app/Main.hs             CLI dispatch only
examples/haskell/       embedded-DSL samples (not built)
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
2. `scripts/update-formula.sh <version>` rewrites the formula's `url` and
   `sha256` for that tag; commit `Formula/coscad.rb`.

## Docs

- docs/LANGEXTENSION.md — full language reference
- docs/TOPOLOGICAL.md — anchors, pipelines, and the stable-datum rule
- docs/MANUFACTURING.md — .assemble and coscad next
- docs/PLAN.md — fasteners, material hints, and coscad plan
- docs/EXAMPLES.md — index of the examples tree
- docs/SKILL.md — agent skill file (gotchas + verification workflow)
- docs/FEA-AND-BACKEND.md — intent: `coscad stress` and a native geometry backend (Manifold, then lighter)

## Tests

```sh
cabal test                          # all tiers; geometry tier needs OpenSCAD
COSCAD_RENDER=0 cabal test          # pure tiers only (a few seconds)
COSCAD_UPDATE_GOLDEN=1 cabal test   # accept changed .scad / geometry snapshots
```

Four tiers in `test/Spec.hs`: diagnostics (error messages carry
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

## Verification culture

Every nontrivial part in the examples was validated numerically, not
visually: trimesh watertightness + volume/bounds cross-checks, boolean
interference tests for fits and fastener paths (expect 0 mm³), and
ASCII occupancy rasters for 2D outlines. docs/SKILL.md describes the
loop; copy it for new parts.
