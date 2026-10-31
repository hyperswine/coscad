# X1C mixed sequential trial - 31 October 2026

The owner cleaned all three printers and requested printing the new bed on X1C.
This is the first mixed per-object trial: one strut completes at the front,
then one 99.2 mm bar prints at the rear. Only this plate is authorized now; no
repeat or full remaining-set dispatch is implied. Fresh status reported X1C
FINISH from the preceding Bed 2 job, no active errors/HMS, 0.4 mm nozzle and
loaded external PLA. Bed-ready authorization is the owner's current request.

The geometry, 128 mm centre spacing, 8 mm brims, six walls, 20% gyroid and
12-second small-layer cooling target / 5 mm/s minimum are unchanged from the
staged P1S sequential trial. This project and G-code use native X1C presets and
startup/end code, verified model ID BL-P001. Machine head/rod/lid clearance
values are taken from the installed X1C profile and are not reduced.

The exported package has actual `print_sequence=by object`. Verification checks
291 strut layers followed by 496 bar layers (787 total), all 0.2 mm, correct
full heights, original placement, actual brims, no support paths or slicer
warnings, 55 C Textured PEI / 220 C nozzle deposition targets, and sampled
head/rod return clearances. The upper-collar overhang is unchanged and remains
a printability risk; no physical success is claimed by these checks.
Estimate: 2 h 34 min total, 6.43 g Generic PLA.

`../../print_sequential_x1c.py --start --bed-clear` repeats sequence/clearance
verification and thermal checks on a frozen upload snapshot, checks live
readiness, uploads over verified FTPS, reads back a matching SHA-256, saves
exclusive start intent and sends exactly one project-file command. An existing
`mixed/print-run.json` blocks any repeat. A lost acknowledgement is reconciled
with read-only status, never by resending the start. Python optimization is
refused so safety assertions remain active. Credentials remain in the existing
protected local runtime and are never recorded here.

Normal bed levelling, vibration calibration and pre-print flow calibration are
enabled. Timelapse and first-layer inspection are disabled for this per-object
trial, avoiding extra autonomous inspection moves around a completed tall part.
Startup observations record real job identity, temperatures, faults and layer
counts. If firmware reports per-object rather than global totals, the unique
job and known first-object count are distinguished from stale preceding-job data.

`mixed/package-validation.json` is geometry/toolpath evidence;
`mixed/sequential-validation.json` is order/clearance/thermal evidence;
`mixed/print-run.json` is the actual dispatch/startup record. Completed printing,
adhesion and physical clearances remain unverified until observed.

Reproduce from repository root with the modelling Python environment:

```sh
python3 models/tesseract-v3-threaded/p1s-split-retry/sequential-retry/x1c/prepare.py
# Slice mixed/full-project.3mf with generated X1C presets; see slice-command.json.
python3 models/tesseract-v3-threaded/p1s-split-retry/verify.py --bed 4 --printer x1c --output-dir models/tesseract-v3-threaded/p1s-split-retry/sequential-retry/x1c/mixed
python3 models/tesseract-v3-threaded/p1s-split-retry/sequential-retry/x1c/verify.py
```

Preparation refuses to overwrite this variant after a dispatch record exists.

## Startup acceptance

The X1C acknowledged the single start successfully at about 08:42 Sydney time.
At 08:52:05 it reported the unique job RUNNING at layer 1/787, measured nozzle
220 C and bed 55 C, with no active errors/HMS and 144 minutes remaining. This
confirms first-layer startup only. Firmware reports the global 787-layer count;
the first strut occupies 291 layers, followed by the 496-layer bar.
`mixed/dispatch-summary.json` summarizes upload/startup evidence and
`mixed/print-run.json` preserves observations. No repeated start, reset or alert
clearing was sent. Full-height print quality and fit remain unverified.

## Owner-reported completed outcome

On 31 October the owner reports this mixed sequential bed worked perfectly.
Both the strut and bar have owner-reported physical acceptance; see
[mixed/physical-outcome-20261031.json](mixed/physical-outcome-20261031.json).
This supersedes the earlier startup-only quality status above.
