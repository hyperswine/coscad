# Tesseract tall parts: spaced per-object retry

## X1C trial requested - 31 October

After cleaning all three printers, the owner requested the new mixed plate on
X1C. A [separate native X1C slice and dispatch](x1c/README.md) passed geometry,
actual sequential order/clearance, thermal and unsafe-case regression checks.
Upload read-back matched and one start was acknowledged. Fresh telemetry
confirmed the unique 787-layer job RUNNING without faults. It reached layer
1/787 at 08:52:05 Sydney time, at measured 55/220 C and with no active faults.
Its run record preserves startup; physical completion is still unverified. The P1S layouts below are retained as prepared on 30 October;
no remaining-set or repeated-run authorization is implied by this single trial.

Prepared on 30 October 2026 after the owner reported that the P1S Bed 4 struts
were going okay until after roughly halfway, then collapsed into spaghetti.
Momentary wobble or uneven extrusion leading to nozzle contact are hypotheses,
not a diagnosed cause. [Three P1S photographs and the observation record](../bed4/outcome-20261030-202153/outcome.json)
confirm the poor physical outcome. The printer nevertheless reported FINISH/291
with no active faults. The exact first failing layer/height is unknown.

## Files for the next trial

The two reusable P1S projects cover all 8 struts and 12 bars previously grouped
on Beds 4 and 5. They do not replace the caps, hubs or inner frame. Start with
one mixed plate; inspect its result before deciding to print the remaining runs.

| Layout | Order and contents | Eventual runs | Estimate per run | PLA |
|---|---|---:|---:|---:|
| [Mixed](mixed/slice/tesseract-bed4-p1s.gcode.3mf) | One complete strut, then one complete bar | 8 | 2 h 31 min | 6.19 g |
| [Bar only](bar-only/slice/tesseract-bed4-p1s.gcode.3mf) | One bar | 4 | 1 h 31 min | 2.58 g |

That is **12 manual plate runs**, represented by two reusable sliced files.
The full remaining set estimates about 26 h 17 min of printing including startup,
plus clearing/reloading time. Those P1S files were staged without dispatch on 30 October. The separate X1C
trial above was requested after owner cleanup on 31 October. A later print needs a clear plate and a fresh live
readiness/filament check; there is no automatic restart or dispatch helper here.

## Layout and unchanged geometry

The mixed strut is centred at X128/Y64 and the bar at X128/Y192: **128 mm centre
spacing**, compared with the original 40 mm grid. Reserved 8 mm brim envelopes
have about 101 mm gap. A single bar is centred at X128/Y128. All brims and models
stay inside the existing 20 mm bed inset and outside the keep-out area.

The existing CoScad-produced STL parts are translated only: no thread, shoulder,
bar-tip, tolerance or support geometry is changed. Source hashes are recorded
in `packing.json`. Six walls, 20% gyroid, 0.2 mm layers, 8 mm brims, slow initial
layers and 55 C Textured PEI / 220 C nozzle intent are retained. Support is off.
A single shaft now has less time between layers, so Generic PLA cooling has a
12-second layer-time target and a 5 mm/s minimum speed, with layer cooling enabled.
This is a slicer cooling target, not a guarantee of measured layer duration.

The upper strut collar's roughly 1.62 mm unsupported radial expansion remains.
Sequential mode reduces inter-object travel and the chance of one failing part
spoiling several others; it does not prove that curling or self-collision is fixed.

## Clearance and actual G-code checks

The installed P1S profile retains its original physical limits: 68 mm maximum
head radius, 34 mm height to the rod, 33 mm rod distance and 90 mm lid height.
The 58.28 mm strut is first and its Y band is far from the rear bar. The
99.20 mm bar is always last, or alone. The manufacturer clearance values are
never reduced to force acceptance. This follows the slicer's distinction
between rod overlap, lid clearance and the last object:
[Bambu Studio clearance implementation](https://github.com/bambulab/BambuStudio/blob/master/src/libslic3r/Print.cpp).

Both actual slices have no warnings or collision errors. The shared geometry
validator checks exported objects, bed bounds/keep-out, all actual brim paths,
settings and package integrity. `sequential-validation.json` in each folder
additionally establishes:

- `print_sequence=by object` in the exported package, not just the input preset.
- The mixed file completes 291 strut layers before 496 bar layers, with no return
  to the strut. The single-bar file has 496 layers.
- Each object starts at 0.2 mm and progresses in 0.2 mm steps to its full height.
- Every deposition layer matches the independent 55/220 C thermal intent.
- Exported active head/rod/lid limits match the unchanged machine input. Studio
  omits the legacy `extruder_clearance_radius`; it is checked in the input only.
- Sampled actual transitions, including custom moves, nozzle offset, helical
  lifts and end G-code, do not return into conservative finished-strut head or
  rod envelopes. The final bar is within full printable height.

These are static/sampled checks with extra margins, not measured hardware
clearances, continuous collision proof, or physical adhesion/quality acceptance.
`check_rejections.py` uses offline modified copies to confirm rejection of
by-layer mode, lowered clearance, interleaved objects and a low return move into
an already finished strut. It uses no credentials and contacts no printer.

## Reproduce

Use the existing modelling Python environment from repository root:

```sh
python3 models/tesseract-v3-threaded/p1s-split-retry/sequential-retry/prepare.py
```

Slice each generated `full-project.3mf` using its machine/process/filament presets
with `--arrange 0 --orient 0`. The exact local command is saved in each
`slice/slice-command.json`. Do not manually reorder the objects or weaken the
clearance limits. Then verify both layouts:

```sh
python3 models/tesseract-v3-threaded/p1s-split-retry/verify.py --bed 4 --printer p1s --output-dir models/tesseract-v3-threaded/p1s-split-retry/sequential-retry/mixed
python3 models/tesseract-v3-threaded/p1s-split-retry/verify.py --bed 4 --printer p1s --output-dir models/tesseract-v3-threaded/p1s-split-retry/sequential-retry/bar-only
python3 models/tesseract-v3-threaded/p1s-split-retry/sequential-retry/verify.py
python3 models/tesseract-v3-threaded/p1s-split-retry/sequential-retry/check_rejections.py
```

`plan.json` lists the 12 eventual runs and component counts. The first mixed run
is the next proposed physical trial, not an authorization to print all runs.
These files are P1S-specific; A1 or X1C use needs a fresh machine slice and
clearance/order/thermal verification for that printer.
