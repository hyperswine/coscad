# CoScad Language Support for VS Code

Language support for CoScad `.coscad` part files and `.assemble` assembly files,
with compiler capability discovery for stress analysis, geometry backend selection
and unsaved recursive project validation.

## Features

- Syntax and semantic highlighting for glyph, simple, pipeline, anchor, and
  assembly syntax; numeric bindings are highlighted as read-only variables
- Contextual hover for every recognized non-comment token, including `=`,
  numeric literals, arithmetic, punctuation and assembly options. Bindings show
  Number / Solid3D / Profile2D and evaluated numeric values; argument hovers
  explain lengths, angles and counts. Unresolved symbols are identified honestly.
- Completion, hover and go to definition for named datums and mating frames,
  including frames in referenced part files and unsaved open buffers
- Diagnostics from the extension's own parser when no compiler is available
  (with a compiler that supports `validate --json`, the compiler's diagnostics
  replace them, so only one set of rules applies):
  syntax and mode errors, undefined names, circular dependencies, duplicates,
  missing `main`, a number used as a shape (and vice versa), 2D/3D mismatches
  (extruding a solid, offsetting a solid, unioning a profile with a box, a 2D
  `main`), loft profile rules, and missing assembly part files
- Compiler diagnostics: parts and assemblies are checked with `coscad validate`,
  without producing files or running a renderer. A compiler with
  `validate --json --stdin` checks the buffer as you type (after a short pause)
  as well as on open and save, warnings included; older compilers are run on
  open and save. This catches
  missing or ambiguous frames, inconsistent loops and invalid rigid poses in
  the actual compiler. A missing or pre-1.1 compiler is reported once; a
  design problem (exit 2) and a compile error (exit 1) both show inline
- Document symbols (numbers as constants) and go to definition for
  variables, parts, and assembly source paths
- Commands: **Compile to OpenSCAD**, **Validate**, **Render to STL**,
  **Check Assembly**, **Manufacture (coscad next)**, **Build Plan** (with or
  without step images), **Run Stress Analysis**, **Select Geometry Backend** and
  **Doctor** (verify runtime dependencies); output commands save the active
  source and its dirty dependencies, then run in a
  COSCAD terminal opened in the file's folder

With a compiler advertising `project-buffers`, validation sends a bounded
snapshot of the root and all reachable open buffers. Changes to parts, nested
assemblies or closed files recheck dependent open assemblies. Closing an unsaved
part rechecks against disk. Superseded compiler processes are cancelled and
stale results cannot publish. External dependencies are watched too.
Older compilers retain root-buffer validation and show a warning when references
come from disk. Parser diagnostics remain available without a compiler. A
compiler that predates `support`, `load` and `stress` is reported once, and its
errors on those lines are ignored: the extension's parser owns them.

The status bar shows the selected backend (or the compiler default). Capability
probes are invalidated by configuration and direct executable changes. A compiler
behind a wrapper such as `cabal run` should be refreshed by reloading the extension
after a rebuild. OpenSCAD source output and plan images retain their own renderer;
the backend choice applies to STL, check, next and stress.

## Supported syntax

- `support`, `load` and `stress`: force/unit hovers, datum navigation, material
  and setting completion, and compiler validation of materials/settings/datums
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
| `coscad.backend` | `auto` | compiler default, `native-experimental`, or `openscad`; availability comes from the compiler |
| `coscad.compilerDiagnostics` | `true` | validate parts and assemblies with the compiler and show its errors and warnings (as you type with `validate --json --stdin`, else on open/save); needs compiler 1.2.0.0 or newer |

For a local checkout:

```json
{
  "coscad.workingDirectory": "/path/to/coscad",
  "coscad.executableCommand": "cabal run coscad --"
}
```

Native geometry commands need a compiler built with Manifold. OpenSCAD rendering
and plan images need OpenSCAD and BOSL2. Run **COSCAD: Doctor** to verify the
installed dependencies. Run **COSCAD: Run Stress Analysis** on
`examples/stress/shelf-bracket.coscad`; inspect the terminal summary and adjacent
`_stress.json` (including full node/cell fields). See the repository
[workflow guide](../docs/2026-10-27-EDITOR-WORKFLOW.md) for the remaining steps.

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
- CoScad compiler 1.2.0.0 or newer (`coscad validate`, positioned diagnostics,
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
