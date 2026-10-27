# Editor integration and shelf bracket workflow

Implemented 2026-10-27. The extension and compiler share capability discovery
and the compiler reads the same open project buffers as the editor. OCCT remains
an additional proposed backend; this change uses the existing native and
OpenSCAD backends.

## Editor

Build the compiler with Cabal and configure `coscad.executableCommand` to the
result of `cabal list-bin exe:coscad` (use the same build directory/flags as your
build). A direct executable is recommended for prompt cancellation. Package
`vscode/` with `npm run package`, install the VSIX, then reload VS Code.

- `support`, `load`, and `stress` have parsing, highlighting, documentation,
  completion and datum navigation. Material completion follows the compiler's
  advertised registry, with a fallback list when no compiler is available.
- **COSCAD: Run Stress Analysis** runs `stress --full` on the saved part.
  The terminal gives the summary; the adjacent `_stress.json` has full node and
  cell fields. It is an explicit analysis, not a solve on every keystroke.
- **COSCAD: Select Geometry Backend** and `coscad.backend` choose `auto`,
  `native-experimental` or `openscad`. The status bar displays the choice/default.
  STL, check, next and stress use it. Missing advertised support is refused;
  source generation and planner images still use their existing OpenSCAD path.
- Explicit output commands save the active file and its open dirty dependencies
  first. A declined or failed save blocks the command.

`coscad capabilities --json` returns `coscad.capabilities/1`, including version,
features, build default, compiled backends and material names/moduli. It does not
prove the OpenSCAD executable/BOSL2 are installed: use `doctor` for that. Compiler
identity includes command, working directory and direct binary size/mtime.
Configuration changes invalidate the probe and all pending diagnostics. If the
compiler is hidden behind a wrapper such as `cabal run`, reload after rebuilding.

## Unsaved project protocol

```
coscad validate --json --project-stdin /absolute/model.assemble < snapshot.json
```

```json
{
  "schema": "coscad.project/1",
  "files": [
    {"path": "/absolute/model.assemble", "text": "part ← part.coscad ×1\nasm = part\n"},
    {"path": "/absolute/part.coscad", "text": "main = box 20 20 4\n"}
  ]
}
```

Buffer paths must be absolute `.coscad`/`.assemble` paths; they need not exist
on disk. Supplied buffers override disk, recursive references use the same
reader, and each source is read once per validation. Canonical path identities
reject duplicate aliases and stop recursive cycles. The envelope limit is 256
buffers and 8 Mi characters. Source files are never written by validation.

The existing `coscad.validate/1` response gains `dependencies`, canonical paths
read while checking, including missing paths. Syntax failures can leave a partial
list. The extension combines that list with its parsed source graph, keeps
previous dependencies across temporary errors, watches closed dependencies,
and rechecks parents when a child changes or closes. Superseded checks are
aborted and guarded against late publication, including configuration changes.
Referenced-file errors remain visible on the parent with the child's path and
position; open child documents get their own check. Unsaved alias paths also
resolve consistently for LSP hover/definition.

Legacy `--stdin` remains a root-only overlay. Older compilers keep root-buffer
validation with a warning about disk references. Compiler-independent editor
parsing remains a fallback; it does not replace compiler semantic checks.
Validation checks stress settings and referenced datum identities without
meshing. Region selection, missing loads/supports, convergence and strength
remain the analysis command's responsibility.

## Reproduce the shelf bracket

```
python3 scripts/shelf-bracket-workflow.py \
  --coscad /absolute/path/to/coscad \
  --output output/shelf-bracket-run --slice --images
```

Use a new output directory. `--slice` checks the existing slice pins and needs
Bambu Studio plus its local BBL presets. `--images` needs OpenSCAD/BOSL2.
Without those options the workflow stops at the plan and manifests.

The runner copies the example sources, validates the part and assembly, checks
interference, solves the 20 N stress case, manufactures a bed, builds the plan,
and optionally slices for an A1. Every command has a timeout and its own log.
`workflow.json` records stage results, compiler/source/tool hashes and output
hashes. A failed stage stops the run; stress exit 2 does not proceed to slicing.
The runner has no print dispatch option.

The assembly explicitly uses `▽bot` and PLA so its print Z direction matches
the analysis's layer-axis assumption. The process uses 4 walls, 100% infill,
normal supports (including on the base, underneath the arm) and a 5 mm brim
reserved by the manifest. This is a simple integration specimen; its bottom
clamp is idealized, not a model of a real wall mounting or screw connection.

The exported 3MF is read back: explicit process settings, printer identity and
brim must match, and G-code must contain support paths. This caught a real
silent substitution during development: Bambu 02.08.02.61 changed the unrecognized
`rectilinear` preset value to cubic. Its Rectilinear UI label uses `zig-zag` in
JSON, also shown in [Bambu's configuration source](https://github.com/bambulab/BambuStudio/blob/master/src/libslic3r/PrintConfig.cpp).
The corrected exported settings were checked locally.

The modeled result at 2 mm cells is approximately 0.145 mm maximum displacement,
0.929 MPa peak von Mises stress and 0.774 MPa across-layer tensile stress. This
uses 5,130 cells and 79,842 unknowns. Safety factors are about 53.8 in plane and
45.2 across layers against the requested 2. These are results for homogeneous
isotropic PLA and a fully clamped base; they do not validate printed strength,
creep, layer bonding, mounting hardware or lifetime. Review toolpaths and the
stress assumptions before physical testing.

## Verification

- `COSCAD_RENDER=0 cabal test`: existing regression suite plus nested/virtual
  overlays, semantic analysis errors, canonical duplicate/cycle checks and CLI
  protocol/capability tests. The separate OpenSCAD geometry tier is skipped.
- `npm test` in `vscode/`: parser/hover, real stdio LSP, dependency scheduling,
  cancellation/stale-result tests and a simulated extension host for commands,
  dependency saves, capability probes and settings changes.
- `python3 scripts/test-shelf-bracket-workflow.py`: refuses substituted exported
  settings and missing support toolpaths.
- `python3 scripts/check-docs.py`: CLI/man/README agreement.

The extension host tests are automated simulations; interactive VS Code UI
validation and a physical bracket print are separate checks.

Local result: 1,309 Cabal checks passed, all six extension test groups passed,
34 Python workflow/slicer tests passed, docs checks passed, and the 0.5.0 VSIX
packaged successfully. The verified run is in
`output/shelf-bracket-verified/` (ignored generated artifacts): all nine stages
passed, including exact OpenSCAD instruction rendering and Bambu 02.08.02.61
slicing with the existing four slice pins. All 25 artifact hashes were checked.
The A1 slice estimates about 1 h 46 min total and 65.63 g. No printer was contacted.
