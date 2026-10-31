# Tesseract III split Bed 3 on P1S - 30 October 2026

Four remaining corner hubs. Freshly regenerated for Bambu Lab P1S, 0.4 mm nozzle,
using the existing packed CoScad meshes and unchanged thread geometry. The
objects are separated and inset from the bed edges. All four have 8 mm outer
brims, six walls, 20% gyroid, 0.2 mm layers and slow first-layer settings.
No support toolpaths are required or emitted.

The new slice estimates 4 h 45 min including startup, 82.1 g Generic PLA and
120 layers. `package-validation.json` checks the actual exported object bounds,
settings, bed keep-out and deposited brim paths for every hub. No slicer warnings
were reported. `thermal-preflight.json` checks the exact package throughout all
120 layers: Textured PEI Plate, bed 55 C and nozzle 220 C, including startup
heater commands. Preview: `plate-preview.png`.

At 02:18 Sydney time, read-only P1S telemetry reported the previous corrected
Bed 1 job FINISH at 78/78 layers, no active errors/HMS and a loaded external PLA
spool. This is printer telemetry, not owner inspection of the completed parts.
The missing temporary public TLS CA file was restored from the existing local
printer trust bundle; certificate/serial verification remains enabled.

The owner confirmed the plate clear and explicitly authorized starting now on
30 October. `print_bed3.py --start --bed-clear` reran the full thermal check,
froze those checked bytes for upload, checked fresh live readiness, uploaded over
verified FTPS and read back a matching SHA-256. One start command received a
successful acknowledgement at about 02:21 Sydney time. Fresh telemetry reached
layer 1/120 at 02:28:34 Sydney time, RUNNING with no active errors/HMS and actual
nozzle/bed temperatures approximately 220/55 C. `print-run.json` preserves startup
and fresh follow-up observations; `dispatch-summary.json` summarizes acceptance.
A part-cooling fan warning appeared during startup and cleared on fresh status
without reset or alert clearing, as recorded in `startup-fan-alert.json`.
This confirms first-layer startup, not completed adhesion or physical printing.

`print_bed3.py` without flags reads status. An existing `bed3/print-run.json`
blocks another start; do not delete it or retry an uncertain acknowledgement.
Any future separate attempt needs a new reviewed run identity and bed-clear
confirmation, fresh printer/spool/fault status and fresh thermal preflight.

Reproduce from repository root with the modelling Python environment:

```sh
python3 models/tesseract-v3-threaded/p1s-split-retry/prepare.py --bed 3 --printer p1s
```

Slice `full-project.3mf` with its generated machine/process/filament presets,
`--arrange 0 --orient 0`, to `slice/tesseract-bed3-p1s.gcode.3mf`. The exact
argument list is in `slice/slice-command.json`. Then run:

```sh
python3 models/tesseract-v3-threaded/p1s-split-retry/verify.py --bed 3 --printer p1s
python3 scripts/bambu_preflight.py models/tesseract-v3-threaded/p1s-split-retry/bed3/slice/tesseract-bed3-p1s.gcode.3mf --presets models/tesseract-v3-threaded/p1s-split-retry/bed3/presets --plate 'Textured PEI Plate'
```

These are sliced geometry/toolpath checks; physical adhesion and final assembly
fit remain to be tested.
