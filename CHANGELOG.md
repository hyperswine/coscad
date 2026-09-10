# Changelog for `coscad`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- Assembly checks retain hidden parts as anchor references, preserving
  relational placements during isolation, including chained attachments.
- Loft profiles reject vector Z translations and oblique or zero mirror
  normals instead of silently dropping components; Z-normal reflection
  correctly leaves an XY profile unchanged.

## 1.1.0.0 - 2026-09-10

- CLI: `--help` (with a language cheat sheet), `--version`, `-o` for
  `coscad part.coscad` and the new `coscad stl part.coscad` (compile +
  render + volume/bounds), `coscad doctor` (finds OpenSCAD and BOSL2,
  reports versions, renders a test part), `coscad check --keep-temp`.
- `coscad check` renders into the system temp directory and cleans up;
  no more `_chk_*` files next to the assembly.
- Design stage (`coscad foo.assemble`) packs across as many plates as
  needed (`foo_plate1.scad`, ...; manifest placements carry `plate`)
  instead of failing on overflow, matching `coscad next`.
- OpenSCAD is located via `COSCAD_OPENSCAD`, PATH, then the usual
  install paths; BOSL2 via `COSCAD_BOSL2` or the OpenSCAD library
  folders (its parent is added to OPENSCADPATH when running OpenSCAD).
  OpenSCAD WARNING/ERROR output fails a render instead of being ignored.
- Emitted .scad: numbers print as `10` not `10.0`, rotation noise like
  `3.06e-16` prints as `0`, identity translates/rotates from attachment
  desugaring are elided.
- Source files are read and written as UTF-8 regardless of locale.
- Embedded-DSL sample modules moved from `src/` to `examples/haskell/`.
- CI on Linux/macOS/Windows (build, `coscad doctor`, full test suite
  with OpenSCAD + BOSL2) and tagged releases with prebuilt binaries.
- License id corrected to `GPL-3.0-only`.

- Numeric bindings: `w = 20`, `r = w / 2 - 1` (arithmetic with parens,
  numbers may reference numbers); usable in any numeric argument
  position as a name or a parenthesized expression. Using a number as a
  shape or a shape as a number is a compile error naming the binding.
- Lofts: `loft z0 p0 z1 p1 ...` (glyph `⟰`, `Loft` in !simple, and the
  pipeline stage `|> loft z p`) skin 2D profiles into a solid through
  BOSL2 `skin()` (`method="reindex"` when vertex counts match,
  `"distance"` otherwise); profiles are emitted as path expressions. Invalid profiles (solids, booleans, out-of-plane
  rotations) are rejected at compile time. `examples/loft.coscad`.
- Diagnostics: errors carry `file:line:col` and quote the offending
  line. Syntax errors, undefined names, circular dependencies (only the
  cycle is listed), and duplicate definitions are distinguished; the
  old catch-all "Cannot resolve variables (possible circular
  dependency)" is gone. `.assemble` part-option errors point at the
  real position too.
- Dimensionality check: extruding a 3D solid, offsetting a solid,
  combining a 2D profile with a solid, or a 2D `main` are compile
  errors instead of silently wrong OpenSCAD output.
- Multi-line pipelines keep their newlines, so a trailing `//` comment on
  the first line no longer swallows the `|>` continuation lines.
- OpenSCAD is located via `COSCAD_OPENSCAD`, PATH, then the macOS app
  bundle, and STL export explicitly requests ASCII.
- Regression suite (`stack test`): diagnostics, example compilation,
  `.scad` snapshots, and an OpenSCAD geometry tier with golden
  volume/bounds per example plus cross-checks (tbracket == bracket,
  `next` conserves volume, `check` passes on bow3).
- Examples: tbracket resynced with bracket (tunnel d10); the zigzag
  springs extrude a circle instead of a sphere (holes now cut);
  `corner`/`frame`/`autotest`/`example_check` assemblies reference
  files that exist.

## 0.1.0.0 - YYYY-MM-DD

A lot of stuff

## 1.0.0.0 - 2026-07-24

First edition of Coscad with Next.
