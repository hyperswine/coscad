# coscad

A concise glyph/pipeline CAD language in Haskell that compiles to OpenSCAD
(optionally BOSL2), with an assembly layer and a manufacturing stage that
emits print-ready per-bed STLs. Intelligence lives upstream in the compiler
— anchors, beziers, attachments, fit, orientation, and packing are all
computed here so the emitted OpenSCAD (and the downstream slicer) stay dumb.

```
.coscad  ──compile──▶  .scad ──openscad──▶ .stl        (one part)
.assemble ─coscad───▶  asm view + plate + manifest      (design stage)
.assemble ─coscad next▶ bedN.stl + manifest             (manufacturing)
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

Prebuilt binaries for macOS, Linux, and Windows are attached to each
GitHub release (`coscad-<os>-<arch>.tar.gz` / `.zip`) with the man page
and examples; put `coscad` on your PATH and, if you want `man coscad`,
copy `man/coscad.1` into a `man1` directory on your MANPATH. Or build
from source with [Stack](https://haskellstack.org)
(the snapshot pins GHC 9.8.2; `stack.yaml` uses the system GHC, so have
9.8.2 installed, e.g. via ghcup):

```sh
stack install                # binary lands in bin/
```

macOS note: ghcup's GHC links against the Command Line Tools SDK. If a
CLT update leaves that SDK newer than Xcode's linker, linking fails with
`tapi error ... unknown architecture arm64e.x1-macos`. Until Xcode and
the CLT match again, tell Stack to link with the CLT toolchain by adding
to `~/.stack/config.yaml`:

```yaml
ghc-options:
  "$locals": -pgml /Library/Developer/CommandLineTools/usr/bin/clang
```

Rendering needs [OpenSCAD](https://openscad.org) 2021.01 or newer and a
[BOSL2](https://github.com/BelfrySCAD/BOSL2) checkout in an OpenSCAD
library folder (`~/Documents/OpenSCAD/libraries` on macOS,
`~/.local/share/OpenSCAD/libraries` on Linux, `Documents\OpenSCAD\libraries`
on Windows) — or anywhere, with `COSCAD_BOSL2=/path/to/BOSL2`. Check the
whole chain with:

```sh
coscad doctor
```

which reports the OpenSCAD and BOSL2 versions it found and renders a
boolean test part against a known volume.

## Quick start

```sh
coscad part.coscad           # -> part.scad
coscad stl part.coscad       # -> part.scad + part.stl, prints volume + bounds
coscad frame.assemble        # -> _asm view, packed _plate(N) scads, manifest
coscad next frame.assemble   # -> frame_bed1.stl ... + manifest
coscad check frame.assemble  # interference / clearance check on real meshes
coscad --help                # command + language cheat sheet
```

`COSCAD_OPENSCAD` points at the OpenSCAD binary or a wrapper script
(e.g. `xvfb-run -a openscad "$@"` on a headless machine); otherwise
PATH and the usual install locations are searched.

Errors carry `file:line:col` and quote the offending line — an unknown
name, a stray glyph, a circular definition, a duplicate, or a 2D/3D
mismatch (extruding a solid, offsetting a cube, unioning a profile with
a box) all fail at compile time instead of rendering silently wrong.
OpenSCAD warnings during a render (a missing include, a dropped child)
are treated as failures too.

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
- **Manufacturing**: `coscad next` compiles each unique
  (part, orientation) variant once, searches FFF print orientation
  where undeclared, packs beds largest-first with spill, and emits
  per-bed STLs plus a variants+placements manifest for slice-once /
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

## Releasing

1. Bump `version` in `package.yaml` and the CHANGELOG, commit, tag
   `v<version>`, push the tag: the release workflow attaches binaries.
2. `scripts/update-formula.sh <version>` rewrites the formula's `url` and
   `sha256` for that tag; commit `Formula/coscad.rb`.

## Docs

- docs/LANGEXTENSION.md — full language reference
- docs/TOPOLOGICAL.md — anchors, pipelines, and the stable-datum rule
- docs/MANUFACTURING.md — .assemble and coscad next
- docs/EXAMPLES.md — index of the examples tree
- docs/SKILL.md — agent skill file (gotchas + verification workflow)

## Tests

```sh
stack test                          # all tiers; geometry tier needs OpenSCAD
COSCAD_RENDER=0 stack test          # pure tiers only (~1s)
COSCAD_UPDATE_GOLDEN=1 stack test   # accept changed .scad / geometry snapshots
```

Three tiers in `test/Spec.hs`: diagnostics (error messages carry
position and cause), examples (every example compiles, every
`.assemble` loads, emitted `.scad` matches `test/golden/scad/`), and
geometry (every example renders through OpenSCAD and its mesh volume +
bounds match `test/golden/geometry.txt`, plus cross-checks: the
topological bracket equals the coordinate bracket, `coscad next`
conserves volume across bed packing, `coscad check` passes on bow3).
When a snapshot fails, the message shows golden vs current; accept
deliberate changes with `COSCAD_UPDATE_GOLDEN=1` and review the diff.
OpenSCAD is found via `COSCAD_OPENSCAD`, then PATH, then the macOS app
bundle.

## Verification culture

Every nontrivial part in the examples was validated numerically, not
visually: trimesh watertightness + volume/bounds cross-checks, boolean
interference tests for fits and fastener paths (expect 0 mm³), and
ASCII occupancy rasters for 2D outlines. docs/SKILL.md describes the
loop; copy it for new parts.
