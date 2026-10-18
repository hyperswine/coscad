# Changelog for `coscad`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- **Internal: `Coscad.Variants`.** Variant naming, instance ids, bed
  orientation and plate packing moved out of `Coscad.Assemble` into
  `Coscad.Variants`, shared by the design stage and `coscad next`. Both
  stages now look variants up in a `Map` keyed by (source, orientation)
  instead of with `head` over lists; an impossible miss is reported as an
  internal error (exit 1) instead of crashing. No behaviour change: every
  generated file is byte-identical.

- **Internal: one linear-algebra module.** `Coscad.Linalg` now holds the
  vector and matrix helpers, the rotations and the bed-orientation helpers
  (`downRot`, `downMat`) that were copied across `Geometry`, `Next`,
  `Plan`, `Assemble` and `Check`. The design stage's variants and
  `coscad next`'s chosen orientation are records instead of tuples. There
  is no behaviour change: generated `.scad`, STL files and manifests are
  byte-identical.

- **Bed height and packing (bed layouts change).**
  - `plate W D margin maxZ`: an optional 4th argument gives the printer's
    build height in mm. It defaults to 250, the value `coscad next` always
    used, so existing assemblies are unchanged; an A1 mini is
    `plate 180 180 6 180`. Both manifests record it (`bed.max_z` in
    `coscad.next/1`; the design manifest's `plate` gains `max_z`).
  - `coscad next` refuses a part that is taller than `maxZ` in its
    declared `▽`, or on every face, with **exit 2** (a design problem) and
    a message naming the part, its height, the limit and the faces it
    would fit on. It used to exit 1 with "declared ▽… exceeds bed
    height". An undeclared part still lies on the best face that fits.
  - The design stage warns (exit 0) when a part's bounding box is taller
    than `maxZ`, since it doesn't render meshes.
  - A footprint bigger than the plate either way round is now a design
    error too: **exit 2** instead of 1, from both stages, and the message
    starts with the `.assemble` path.
  - The packer is now first-fit decreasing over shelves. A bed stays open
    until every remaining part has been tried, so smaller, later parts
    fill the gaps, and a part is turned 90° about Z when that fits it on
    an existing shelf or a new one. A part that fits the plate only
    turned is turned instead of refused. Layouts the old packer filled
    without gaps are unchanged. Every placement in `coscad.next/1`
    records `rot_z` (0 or 90) with `w`/`d` as placed, and the bed STL
    carries the turned part. Design placements carry `rot_z` only when
    it is 90. Readers that ignore `rot_z` still get correct bounding
    boxes from `x`, `y`, `w`, `d`.
  - `examples-next/enclosure2x2` now packs on 4 beds instead of 5 (the
    brackets fill the two panel beds). The committed example manifest is
    regenerated; it was also stale from before `schema` and
    `printability`.
  - `bambu-slice.py` warns when the manifest's `bed.max_z`, or a part on
    the selected beds, is taller than the printer's `printable_height`.

- **Printability you can act on.**
  - `coscad next --strict` exits 2 when any part has a printability
    warning. It still writes the beds and manifest so you can inspect them.
    Without `--strict`, `next` behaves as before.
  - Each manifest variant's `printability` gains `warning_codes`, one per
    warning: `small_contact`, `tall_narrow`, `flat_overhang` or
    `elevated_start`.
  - `scripts/bambu-slice.py` prints the warnings for each bed and records
    them in `<base>_print.json` (`printability`, and
    `printability_warnings` per bed).
  - `bambu-slice.py` refuses to slice (exit 2) when a warning isn't
    addressed by the process. Small contact and tall/narrow need an
    explicit brim (`brim_type` outer/inner and `brim_width` > 0; auto brim
    doesn't count). Flat overhang and elevated starts need
    `enable_support`. `--accept-printability` slices anyway and records it.
  - `bambu-slice.py --bed N` with no such bed is now an error instead of
    an empty `_print.json`.
- **Brim reservation (packing positions change when a brim is reserved).**
  - A part hint `brim=N` makes the packer grow that part's footprint by
    N mm on every side, so the margin is kept outside the brim.
  - `plate W D [margin] brim=N` (or `brim=auto` for 5 mm) does the same in
    `coscad next` for parts whose printability recommends a brim. A part's
    own `brim=N` wins, and `brim=0` opts out. The design stage applies the
    hints only.
  - Each `coscad next` placement records `brim_mm`, and `bed.brim_mm`
    records the plate setting. Design placements carry `brim_mm` only when
    it is non-zero.
  - Without any brim settings, packing is unchanged.
  - `bambu-slice.py` prints the reserved width as an outer brim when the
    process overrides don't set one, and warns when the brim it prints is
    wider than a part's reservation plus half the margin.
- The tesseract adhesion retry declares `brim=8` on its parts instead of a
  hand-raised 12 mm plate margin and a brim in `process-overrides.json`.
  Its supports, speeds and cooling stay explicit overrides.
- `bambu-slice.py` removes its temporary preset and slice directories on
  every failure path. `scripts/test-bambu-slice.py` now runs in CI.

- `coscad next` printability warnings: a new **flat overhang** warning for
  down faces that are neither bridged nor within 2 mm of a supporting wall
  (a mushroom cap, a hook arm; an upside-down cup's ceiling is a bridge and
  does not warn). **Tall and narrow** now uses the narrowest footprint width
  (rotating calipers) instead of the bounding box, with a limit of
  2.5 + 10/h, so a 2 mm wall turned 45° warns and the 7×7×18.8 fit-test bar
  no longer does. **Small contact** looks at the first layer (down faces
  within 0.3 mm of the bed, projected) against max(25 mm², 0.05·h²), so
  lying rods no longer warn. The manifest's `printability` gains
  `overhang_mm2`, `first_layer_mm2` and `min_width_mm`; thresholds are in
  `PrintabilityConfig`.

- `coscad next`: bed contact has one definition, shared by the orientation
  score and the manifest's `printability.bed_contact_mm2` (down-facing
  triangles with every vertex within 0.01 mm of the bed, or 1e-5 of the
  part size above a metre). The score used to count faces up to 0.3 mm
  above the bed as contact and the manifest used an exact 1e-5 mm plane,
  which missed quantised vertices on rotated faces. Contact areas and
  `fff_score` values can shift slightly (the tesseract corner's contact goes
  from 102.55 to 103.27 mm², matching an independent trimesh measurement);
  no example changes orientation or placement. Printability is analysed
  once per variant.

- **Manifest contract change.** The design stage (`coscad foo.assemble`)
  now writes `foo_design.json` (`"schema": "coscad.design/1"`) instead of
  `foo_manifest.json`, which is now written only by `coscad next`
  (`"schema": "coscad.next/1"`). Previously the design stage overwrote the
  print manifest with a differently shaped file. A design manifest left
  under the old name gets a stderr note and is not deleted.
  `bambu-slice.py` checks `schema`, still accepts pre-schema `coscad next`
  manifests, and refuses design manifests with the command to run.

- **Constraint errors report the whole loop and exit 2.** A mating loop
  that does not close (or two fixed roots that disagree) is reported on the
  constraint declared last on it, followed by a `  note: file:line:col:`
  line for every constraint on the loop; previously one joint was named,
  wherever the breadth-first walk happened to meet, often not the closing
  mate. Floating parts are reported at their declaration instead of the
  first constraint, and a duplicate `fixed` points back at the first one.
  Constraint conflicts, floating parts and duplicate `fixed` now exit 2
  (design problem) from `validate`, the design stage, `next`, `plan`,
  `check` and `site`; unknown instances or frames and syntax errors stay
  exit 1. Scripts that treated these failures as exit 1 will see 2.

- Windows is no longer supported: CI and release builds are macOS/Linux
  only (no `coscad-windows-x86_64` archive), and the Windows OpenSCAD and
  library search paths, lock branch and extension quoting are removed.

- `plan --png` and `site` render step images fully with OpenSCAD's Manifold
  backend when available (2025.05 or newer): no z-fighting on mated faces,
  at preview speed. Older OpenSCAD builds keep the preview with a note. The
  bench is a solid slab stopped just short of the rest face, so it survives a
  full render and cannot flicker in a preview.

## 1.2.0.0 - 2026-10-13

- CLI contract for editors and pipelines: every diagnostic goes to stderr
  and starts with `Error: file:line:col: reason` on one line (the source
  excerpt follows); assembly loader and fastener errors name the file and
  line; `plan` design errors go to stderr as `file: design: reason`.
  Exit 2 now means "the design has a problem" (`validate`/`plan` design
  errors, `check` overlaps) and exit 1 "could not run". Bare `coscad`
  prints usage to stderr. `validate` on a missing part file says so.
- JSON: manifests, plan JSON and the site's build index are emitted through
  a real encoder (`Coscad.Json`, also a decoder for tests and tools), so
  quotes, control characters and `</script>` in names, paths or hints can no
  longer corrupt them. Plan JSON `nut` values are the `nut=` words
  (`dropin`, `slidein`, `hex`, `none`) instead of Haskell constructor names.
- Rendering: each OpenSCAD run is killed after `COSCAD_OPENSCAD_TIMEOUT`
  seconds (default 600) and reported, instead of hanging `stl`/`next`/
  `check`/`plan --png` forever. A binary or truncated STL, a non-finite
  vertex, and an empty mesh are diagnosed instead of crashing with a UTF-8
  or `Prelude.read` error. `check` and `doctor` use fresh per-run scratch
  directories (`--keep-temp` names it), so concurrent runs no longer delete
  each other's renders. `/opt/homebrew/bin/openscad` is a search candidate.
  `next` refuses an assembly with nothing to print instead of writing an
  empty manifest.
- VS Code extension: moved into this repository under `vscode/` (0.4.0; see
  its CHANGELOG): probes `coscad --version` before relying on `validate`,
  reads the new diagnostic layout and exit codes, `coscad.workingDirectory`
  replaces the misnamed `coscad.executablePath`, terminal commands no longer
  depend on the shell, VS Code 1.82, GPL-3.0, a tenth of the package size.
  CI tests and packages it; releases attach the `.vsix`.
- Scripts: `scripts/requirements.txt` lists the Python dependency of the
  Bambu LAN printing path (paho-mqtt).
- Tests: a `cli` tier spawns the built binary and checks exit codes, the
  stdout/stderr split, `validate`, and that manifests and plan JSON parse.

- Build: the project builds with cabal-install and GHC 9.8 (`cabal build`,
  `cabal test`, `cabal install exe:coscad`); Stack files, hpack and
  `package.yaml` are gone and `coscad.cabal` is the source of truth.
  Hackage dependencies are pinned in `cabal.project.freeze`. CI, the release
  workflow, the Homebrew formula and the manual host's update script use
  cabal.

- Planner: locate screws with host `@frame`, clamped `seat=@frame` and hex
  nut `pocket=@frame` bearing seats. Derive transformed axes, head positions,
  thread reach and access from those declarations. Add `preload=hand` for
  independent nut preparation, and validate hardware without artifacts via
  `coscad validate`. Keep existing face/distance declarations supported.
- Examples: repair the old corner's reversed outer bracket, constrain both
  bores of each outer bracket, and describe all six M5 screws. Complete the
  toy bow's hardware metadata with four M4x12 screws and captive nuts.
  Generate both instruction guides from the planner; include the repaired
  corner in the manual site build.

- Examples: migrate fourteen remaining assembly and packing examples to
  `fixed` / `mate` authoring, with independent coordinate fixtures and
  per-instance orientation probes. Preserve print quantities and instruction
  joints, including the tesseract's closed loops and Orbit Tower capture order.
- Manual: responsive content and cards, contained tables/code, and persistent
  navigation shared across documentation, build indexes and instruction pages.
  Keyboard step navigation ignores typing in search fields.
- CLI: `coscad validate file.coscad|file.assemble` validates source and solves
  constraints without producing artifacts, for editor diagnostics.

- Examples/planner: convert the 200 mm 2020 cube to named mating frames and
  rigid assembly constraints. Verify all 16 bracket poses and 32 bores against
  the coordinate reference, and regenerate its 28-step instructions. Replace
  the panel's mismatched four-hole pattern with six rail-aligned bores. Keep
  side-face screws on 2020 slot centres when a panel only partly covers the
  rail face; reject clamps that miss the slot. Plan JSON now includes each
  fastener's assembled host-face `position_mm`.

- Language/assembly: oriented construction frames (`|> frame`) and child-only
  rigid mating (`|> mate`) with explicit normal, up, roll and gap. Declarative
  `.assemble` `fixed` / `mate` constraints propagate poses in both directions,
  validate closed loops and diagnose floating/conflicting assemblies. Design,
  manufacturing, interference checking and planning share the generated poses.
  Existing manual assembly expressions remain supported. Moving joints and
  collision solving are outside this first increment.

- Language: named construction datums (`|> datum name ref`, `@name`),
  child-only placement against a separate shape (`|> place parent target own`),
  bounded rectangular feature grids (`|> grid n m pitch_x pitch_y`), and unary
  pipeline hull (`|> hull`). Datums follow transforms; unknown, duplicate and
  ambiguous references have explicit errors. Existing attachment and binary
  hull semantics are preserved. See `docs/TOPOLOGICAL.md`.

- CI: temporarily remove the macOS test job because Homebrew's OpenSCAD
  cask is disabled by Gatekeeper. Refresh the bow3 limb geometry goldens
  after the paddle-tip/string-groove redesign and record missing ball
  and tesseract baselines; Linux and Windows retain the full rendering tests.

- Language: reject spaced unary minus in numeric argument positions with
  a subtraction/negation hint; reject non-finite numeric bindings and
  arguments before geometry evaluation. Reject opposing/repeated anchor
  directions and combinations with the center anchor. Active shape and
  prefix-transform keywords cannot be definition names; stage-only words
  such as `x` remain usable. Clarify compact glyph argument syntax.

- Plan: `pocket=clamped.face` for a hex nut pocket in the clamped part;
  a driver that would come up through the bench is a hard block (flip),
  not a cost. The tesseract example's second cut: 19 mm corner blocks,
  M3x16 + hex nut per bar end, D-profile struts that print on their flat.
- Plan: `nut=none` (screw threads into the host) and `fastener peg ...`
  (friction joints: no screw, driver or torque). Driver clearance is
  tested against each part's oriented box, a part can be placed held
  against anything it will be joined to, slender parts stood on end are
  penalised like rails, and a clamped part resting on the bench no longer
  counts as loose. `examples/assemble/tesseract/`: a 100 mm wire-frame
  cube with a 30 mm cube hung on eight diagonal pegs, 29 parts on one bed.
- `fastener ... nut=hex pocket=face`: captive hex nuts in a pocket of a
  non-rail host. Preload, BOM and step wording follow (no T-slot
  positions), the pocket face may not be on the bench or covered while
  the nuts go in. Driver access now requires the driver to really enter
  another part's box, not merely touch it. When the beam search runs out
  of states the error lists what the last states could not do.
  `examples/assemble/ball/`: a bolted 40 mm ball (two hemisphere shells,
  core disc) that runs the full chain to a sliced bed.
- `scripts/bambu-slice.py manifest.json [--print N]`: slice the beds from
  `coscad next` with Bambu Studio's CLI (system presets flattened and the
  printer's template gcode files merged so filament settings and the real
  start/end sequences apply), write `_bedN.gcode.3mf` and `_print.json`,
  optionally upload and start a bed on a LAN-mode Bambu printer.
- `scripts/bambu-print-run.py foo.assemble [--dry-run]`: unattended run:
  camera photo, coscad → next → plan → slice → print, watch to the end,
  photo, `foo_run.json` with a verdict.
- `coscad plan foo.assemble [--png]`: assembly instruction generator.
  `fastener spec clamped host face [×n at= nut= head= through= torque=]`
  lines and part hints (`profile=2020`, `material=`, `ends=`, `mass=`,
  `torque=`) describe the joints; the planner derives the requires-before
  graph (nut-first, slot-end sealing, host and clamp before screw), checks
  support, balance, driver access, and nut entry on every step, searches
  rest-face phases with a cost table (flip, rail not flat, driver off
  vertical, shrinking support, sibling screw, loose parts), and writes
  `_plan.md` (BOM, T-nut preload sheet, numbered steps with torque),
  `_plan.json`, and `_stepN.scad` scenes (`--png` renders them). Design
  errors: colliding screws, too little thread reach, a clamp that does
  not sit on its host face. Fixtures under `examples/assemble/plan/`.
- `coscad site DIR spec.assemble ...`: static phone-first companion site
  from the build plans (per-build step pages with images and torque, an
  index with search across builds).
- Assembly and part files: expression continuation lines may start with a
  boolean glyph (`⊕ ...`), and comment-only lines may sit between
  continuation lines.

- Assembly checks retain hidden parts as anchor references, preserving
  relational placements during isolation, including chained attachments.
- Loft profiles reject vector Z translations and oblique or zero mirror
  normals instead of silently dropping components; Z-normal reflection
  correctly leaves an XY profile unchanged.

- Examples: migrate fourteen remaining assembly and packing examples to
  `fixed` / `mate` authoring, with independent coordinate fixtures and
  per-instance orientation probes. Preserve print quantities and instruction
  joints, including the tesseract's closed loops and Orbit Tower capture order.
- Manual: responsive content and cards, contained tables/code, and persistent
  navigation shared across documentation, build indexes and instruction pages.
  Keyboard step navigation ignores typing in search fields.
- CLI: `coscad validate file.coscad|file.assemble` validates source and solves
  constraints without producing artifacts, for editor diagnostics.

- `man coscad`: full manual page (commands, environment, language cheat
  sheet, assemblies, examples), shipped in release tarballs.
- Homebrew formula in `Formula/` (tap this repository; builds with Stack
  and Homebrew's ghc@9.8; installs the man page, docs, and examples) and
  `scripts/update-formula.sh` to pin it to a tagged release.

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
- CI on Linux/macOS/Windows with Stack (build, `coscad doctor`, full
  test suite with OpenSCAD + BOSL2) and tagged releases with prebuilt
  binaries.
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
