# CoScad Language Support for VS Code

Language support for CoScad `.coscad` part files and `.assemble` assembly files,
in step with the CoScad compiler 1.1 (numeric bindings, lofts, positioned
diagnostics, named datums, oriented mating frames and rigid assembly constraints).

## Features

- Syntax and semantic highlighting for glyph, simple, pipeline, anchor, and
  assembly syntax; numeric bindings are highlighted as read-only variables
- Contextual hover for every recognized non-comment token, including `=`,
  numeric literals, arithmetic, punctuation and assembly options. Bindings show
  Number / Solid3D / Profile2D and evaluated numeric values; argument hovers
  explain lengths, angles and counts. Unresolved symbols are identified honestly.
- Completion, hover and go to definition for named datums and mating frames,
  including frames in referenced part files and unsaved open buffers
- Diagnostics that mirror the compiler's own checks and wording:
  syntax and mode errors, undefined names, circular dependencies, duplicates,
  missing `main`, a number used as a shape (and vice versa), 2D/3D mismatches
  (extruding a solid, offsetting a solid, unioning a profile with a box, a 2D
  `main`), loft profile rules, and missing assembly part files
- Compiler diagnostics: on open/save, parts and assemblies are checked with
  `coscad validate`, without producing files or running a renderer. This catches
  missing or ambiguous frames, inconsistent loops and invalid rigid poses in
  the actual compiler. A missing or pre-1.1 compiler is reported once; a
  design problem (exit 2) and a compile error (exit 1) both show inline
- Document symbols (numbers as constants) and go to definition for
  variables, parts, and assembly source paths
- Commands: **Compile to OpenSCAD**, **Validate**, **Render to STL**,
  **Check Assembly**, **Manufacture (coscad next)**, **Build Plan** (with or
  without step images), **Doctor** (verify OpenSCAD + BOSL2); they run in a
  COSCAD terminal opened in the file's folder

## Supported syntax

- `!glyph`, `!simple`, and legacy mixed mode; `$` loose application
- Glyph, word, BOSL2 and simple-mode primitives; `✎` bezier profiles
- Numeric bindings: `w = 20`, `t = w / 5 - 1`, used as `box w (w / 2) t`,
  `χ -r part`, `|> cutat top 0 0 (-t / 2) ...`
- Lofts: `loft z0 p0 z1 p1 ...`, `⟰ ...`, `Loft ...` (simple mode), and the
  pipeline stage `p0 |> loft z1 p1 |> loft z2 p2`
- Booleans, `⌖` positioning, `⋈` attachment, multiline `|>` pipelines with
  `at`, `on`, `cutat`, transforms and boolean stages
- Named and combined anchors such as `top`, `lft`, `top+rt`
- `datum`, `frame`, `place`, `mate`, `grid` and unary `hull` stages; `@name` references
- `fixed` / `mate` assembly constraints, `after=alias,alias` capture dependencies
  and peg `inset=mm`; floating components and invalid instance counts are diagnosed
- Assembly plate, clearance, part count, bed orientation, hints, recursive
  `.assemble` references, numeric bindings in `asm` expressions, and the
  `fastener` / `plan` lines of the assembly instruction generator

See COSCADLANG.md for examples and a compact syntax reference.

## Configuration

| setting | default | meaning |
|---|---|---|
| `coscad.executableCommand` | `coscad` | command that runs the compiler: a binary on PATH, a full path, or `cabal run coscad --` inside a checkout |
| `coscad.workingDirectory` | `""` | working directory for that command; empty = the file's folder (`coscad.executablePath` is the deprecated name) |
| `coscad.compilerDiagnostics` | `true` | validate parts and assemblies on open/save and show compiler errors; needs compiler 1.1.0.0 or newer |

For a local checkout:

```json
{
  "coscad.workingDirectory": "/path/to/coscad",
  "coscad.executableCommand": "cabal run coscad --"
}
```

Rendering commands need OpenSCAD and BOSL2 on the machine; run **COSCAD:
Doctor** (or `coscad doctor`) to verify both.

## Development

```bash
npm install
npm test                                    # parser, hover, diagnostics and stdio integration
COSCAD_ROOT=/path/to/coscad node test-hover.js # all active examples and models
npm run package                             # build the .vsix
```

Run the extension through VS Code's Extension Development Host to exercise
protocol features and TextMate highlighting.

## Requirements

- VS Code 1.82 or newer
- CoScad compiler 1.1.0.0 or newer (`coscad validate`, positioned diagnostics,
  exit codes); the extension checks `coscad --version` once and says so when
  the compiler is missing or too old

## License

GPL-3.0-only, like the compiler (see LICENSE).

Frame-based hardware is supported in assembly files:

```coscad
fastener M4x12 arm center @hole seat=@head_seat head=socket nut=hex pocket=@nut_seat
```

Host holes and nut seats resolve on the host part; head seats resolve on the
clamped part. `pocket=clamped.@frame` resolves on the clamped part. Completion,
hover and definition lookup use the selected part, including unsaved buffers.
`preload=hand` prepares nuts before part placement. The compiler validates
coaxial seating frames and screw reach through `coscad validate`.
