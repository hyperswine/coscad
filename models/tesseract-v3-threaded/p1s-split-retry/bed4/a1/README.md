# Tesseract III split Bed 4 on A1 - 30 October 2026

Eight upright diagonal struts with male threads at both ends, using unchanged
CoScad meshes copied from the split Bed 4 layout. This is an A1-specific slice,
not reused P1S G-code. The owner requested the print, confirmed the plate clean
and ready, and selected physical AMS slot 1. Fresh status reported white PLA
in that slot (`ams0`), yellow PLA in physical slot 3 (`ams2`), and no active faults.

Before dispatch, the A1 retained terminal FAILED/496 status from the known
28 October full-tesseract attempt, with empty active file and error zero. The
new dispatcher accepts that specific historical terminal state; it does not
clear errors or permit an unrelated failed/running job. New-job matching uses
its unique name and expected 291 layers, avoiding the cached previous result.

## Slice and verification

- Bambu Lab A1, verified model ID N2S, 0.4 mm nozzle, Generic PLA.
- Textured PEI Plate, bed 55 C and nozzle 220 C for all print layers.
- 0.2 mm layers, six walls, 20% gyroid, no support paths.
- 8 mm outer brim on every strut; first-layer walls 20 mm/s, fill 25 mm/s.
- Estimate: 2 h 38 min including startup, 29.0 g PLA, 291 layers.

`package-validation.json` checks actual exported geometry, printer identity,
settings, placement, bed keep-out and deposited brim paths for all eight parts.
No slicer warnings were reported. `thermal-preflight.json` checks the exact
package throughout all 291 layers and 214,198 deposition moves against the
independent Textured PEI / 55 C / 220 C preset intent. `plate-preview.png` shows
the upright layout. These checks do not prove physical adhesion or thread fit.

## Dispatch

`../../print_bed4_a1.py --start --bed-clear` performs fresh readiness/AMS and
thermal checks, freezes the checked package bytes, uploads via verified FTPS,
reads back matching SHA-256, records exclusive intent and sends one start.
The command explicitly sets `use_ams=true` and `ams_mapping=[0]`. Startup checks
verify live `tray_now=0` when the new job reaches its first layer. Existing
`print-run.json` blocks another start, including an uncertain acknowledgement.
Without flags the script reads status only. Credentials stay in the existing
local runtime environment and are never recorded here.

Upload read-back matched the validated SHA-256 and one start received a success
acknowledgement at 09:20:54 Sydney time. Fresh status at 09:21 reports the new
291-layer job RUNNING, AMS tray 0 selected and no active faults. The first packet
after acknowledgement still carried the prior FAILED/496 state; it is preserved
in the record and was superseded by fresh RUNNING/291 status.

At 09:28:24 Sydney time the job reached layer 1/291, RUNNING with no errors or
HMS faults. Live `tray_now=0` confirmed physical AMS slot 1; bed/nozzle targets
were 55/220 C and measured approximately 55.3/219.9 C. Remaining time was 151
minutes. `dispatch-summary.json` summarizes this evidence; `print-run.json`
preserves all startup observations. This confirms first-layer startup, not
completed adhesion or successful printing of all eight parts.
Completed printing and fit remain unverified.

## Reproduction

From repository root, with the modelling Python environment:

```sh
python3 models/tesseract-v3-threaded/p1s-split-retry/prepare.py --bed 4 --printer a1 --variant a1
```

Slice `full-project.3mf` with its A1 machine/process/filament presets and
`--arrange 0 --orient 0`, writing `slice/tesseract-bed4-a1.gcode.3mf`.
The exact arguments are saved in `slice/slice-command.json`. Then:

```sh
python3 models/tesseract-v3-threaded/p1s-split-retry/verify.py --bed 4 --printer a1 --variant a1
python3 scripts/bambu_preflight.py models/tesseract-v3-threaded/p1s-split-retry/bed4/a1/slice/tesseract-bed4-a1.gcode.3mf --presets models/tesseract-v3-threaded/p1s-split-retry/bed4/a1/presets --plate 'Textured PEI Plate'
```

## Owner-reported physical failure and review - 30 October

The owner reports that this print came out badly and left it on the A1 for
photography. Fresh photographs and `outcome-20261030-171001/outcome.json` record
substantial spaghetti and displaced struts. The printer nevertheless reports
FINISH/291 with no active errors/HMS. This is a failed physical-quality outcome;
a successful acknowledgement or FINISH state does not establish print quality.
The earlier layer-96 photographs showed only the then-intact lower portions.

Rechecking the exact dispatched package found no clear malformed G-code defect:
all 291 layers have deposition with 0.2 mm progression, thermal targets remain
55/220 C during deposition, all eight actual brims are present, and full part
cooling is commanded from layer 2 until the end. A geometric risk remains:
near layer 237 (47.3 mm mid-layer), the upper collar grows about 1.62 mm radially
past the previous shaft. Overhang walls there run at 10 mm/s; this reduces risk
but does not prove that an unsupported ledge will print cleanly. Curling/nozzle
contact or motion-related disturbance of slender parts are plausible, unproven
contributors. Fast vendor infill defaults are retained. See the detailed layer,
feed/fan and geometry-screen JSON files in that outcome directory.

The owner requested an unchanged Bed 4 comparison on the P1S after review and
confirmed its bed clear. It uses a fresh P1S-specific slice; A1 G-code is never
sent to the P1S. The different loaded PLA spool and native machine/cooling
profiles mean this is not a strict isolation of hardware alone. No A1 controls,
resets or alert clearing were sent during this investigation.
