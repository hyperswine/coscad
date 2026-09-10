# Changelog for `coscad`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

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
