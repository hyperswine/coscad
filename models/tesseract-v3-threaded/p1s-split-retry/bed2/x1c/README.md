# Tesseract split Bed 2 on X1C - 28 October 2026

The owner requested this plate on the X1C and confirmed its bed clear.
Contents: one inner frame and four hubs, unchanged from the staged Bed 2
packing. This is an X1C-specific re-slice, not the P1S G-code.

## Configuration and validation

- Bambu Lab X1 Carbon, verified model ID `BL-P001`, 0.4 mm nozzle.
- Loaded external PLA, sliced with Generic PLA: nozzle 220 C, textured PEI 55 C.
- Six walls, 20% gyroid, 0.2 mm layers, 20 mm/s initial walls and 25 mm/s fill.
- 8 mm outer brims on the four hubs. The slicer suppresses a separate model
  brim on the inner frame where its tree support bases surround the contact
  pads. Explicit `raft_first_layer_expansion=8` widens those first-layer bases;
  it does not enable a raft (`raft_layers=0`).
- Tree supports on the bed only, with the existing thread-bore blockers.
- Estimate: 7 h 56 min, 118.4 g, 282 layers.

`package-validation.json` checks all five objects, exported geometry and
settings, actual brim/support paths, bed bounds and keep-out. It found zero
support-thread intrusions or support footprint violations. The sampled support
continuity check found one bed-connected base and no unsupported starts.
Expanded base anchoring has 13,811 occupied 0.5 mm sample cells, covers all four
quadrants around the inner frame, and approaches a model contact path to
0.651 mm. These checks do not prove physical adhesion or support strength.

`thermal-preflight.json` independently checks actual heater commands throughout
all 282 layers and 539,272 deposition moves. Every dispatch reruns that check;
a saved report cannot bypass it. `print-run.json` records the upload hash,
single start intent, acknowledgement and bounded startup observations.

## Reproduction

From the repository root, use the modelling Python environment. Copy Bed 2's
`packing.json` and five named meshes into this `x1c/` directory, then:

```sh
python3 models/tesseract-v3-threaded/p1s-split-retry/prepare.py --bed 2 --printer x1c --variant x1c --support-base-brim 8
```

Slice `full-project.3mf` with the generated X1C machine/process/filament presets,
`--arrange 0 --orient 0`, and output
`slice-expanded/tesseract-bed2-x1c.gcode.3mf` plus `result.json` and `plate_1.gcode`.
Then run:

```sh
python3 models/tesseract-v3-threaded/p1s-split-retry/verify.py --bed 2 --printer x1c --variant x1c --slice-dir slice-expanded
```

`print_bed2_x1c.py` without flags only reads status. Its start mode refuses an
existing run record and never retries a lost acknowledgement. Do not delete
that record to resend an uncertain start.

Exploratory `slice/`, `slice-brim-check/`, `slice-support-brim/` and `slice-final/`
outputs were not dispatched. The obsolete `tree_support_brim_width` setting was
silently omitted by the installed slicer, so only the supported and verified
`raft_first_layer_expansion=8` output under `slice-expanded/` is selected.

## Latest startup observation

The single start command was acknowledged. A transient HMS
`03000D0000020001` appeared during bed levelling at 22:45:02; it cleared on
fresh reads without any alert-clear, resume or repeat-start command. See
`startup-homing-warning.json`. The underlying cause is not diagnosed.

At 22:52:09 Sydney time the matching job was RUNNING, with 282 expected layers,
about 468 minutes remaining, stage 0, nozzle 220 C and bed 55 C, both matching
their targets. No active HMS or print error was reported. Levelling (stage 1)
and extrusion calibration (stage 8) had completed. The layer counter was still
0, so first-layer completion and physical adhesion are not yet verified. The
bounded read-only startup observer has ended; no recurring monitor was created.

## Owner-requested cancellation - 29 October 2026

The owner reported a spool issue and requested cancellation to replace the
filament. Fresh X1C telemetry at 09:25:45 Sydney time showed the matching job
PAUSE at layer 2/282 (4%), with print error 50364418. One stop command was sent
and acknowledged SUCCESS. At 09:25:49 the printer reported terminal FAILED
with print error 50348044 after that cancellation. This terminal state records
the intentional cancellation; it is not a new observed geometry failure.
See `cancel-request-20261029.json`. The spool problem's exact cause was not
diagnosed. No filament unload, resume or new print command was sent.

## Restart after filament replacement - 29 October 2026

The owner replaced the filament, cleared the X1C plate, and explicitly requested
a new Bed 2 attempt. Fresh readiness showed the known cancelled job in terminal
FAILED state, with error 0, no HMS faults, 0.4 mm nozzle, SD card available and
external PLA loaded. The cancellation was checked against its successful stop
acknowledgement before allowing that terminal state for this retry. An unrelated
failure, active error, PAUSE or running job is still refused.

The unchanged X1C package was rechecked against its geometry validation hash and
a fresh thermal scan of all 282 layers and 539,272 deposition moves: textured
PEI, bed 55 C and nozzle 220 C for initial and subsequent layers. The upload was
read back with an identical SHA-256. The printer acknowledged one new start
command successfully at about 12:57 Sydney time.

The original cancelled attempt is preserved in `print-run.json`; this new
attempt uses `print-run-retry-20261029.json` and a unique remote filename.
`print_bed2_x1c.py --start --bed-clear --restart-after-cancel` refuses an
existing retry record. Do not delete either record or resend an uncertain start.
A bounded 120-second startup observation was used; no recurring monitor was
created. Acknowledgement alone is not first-layer or completed-print evidence.

Latest bounded observation at 2026-10-29T12:58:58+11:00: matching retry job RUNNING, layer 0/282, no active HMS or print error. Startup/heating is confirmed; physical adhesion and print completion remain unverified. The startup observer has ended.

## Confirmed retry outcome: automatic bed levelling failure

The owner reported screen code `0300-4002 100529`. Fresh status at 14:21:46
Sydney time retained `fail_reason=50348034` (hex `03004002`) although active
`print_error` was already 0. Bambu's catalog identifies this as automatic bed
levelling failure. The retry stopped before layer 1. The file read back from
the printer matches the verified local package; its vendor startup template
matches the successful upright rail, differing only in the generated probing
rectangle once progress annotations are removed. The exact physical cause of
the levelling failure remains unknown. See [the investigation](investigation-20261029/README.md).

## Full-bed calibration passed at room temperature

On 29 October 2026 the owner requested a full-bed calibration. The X1C's built-in
bed-levelling-only routine finished at 14:35:47 Sydney time with FINISH/100%,
error 0, retained reason 0 and no HMS faults. It did not reproduce the failure.
The bed stayed unheated at approximately 26 C, unlike the failed 55 C print, so
this does not establish that the heated startup issue is fixed. Probe coordinates
and camera frames were unavailable. See [the calibration record](calibration-20261029/README.md).

## Owner-approved repeat after calibration - 29 October 2026

The owner saw nothing wrong during the full-bed calibration and requested
another print after reviewing the G-code, to see whether the earlier problem
was intermittent. This does **not** establish that the failure was a fluke.
A fresh review found no obvious startup defect: the embedded X1C vendor start
template is unchanged, with the same generated probing rectangle, verified
Textured PEI setting, 55 C bed and 220 C printing temperatures throughout all
282 layers. No startup or model geometry was modified. See
`review-before-calibration-retry-20261029.json`.

The new attempt uses `print-run-after-calibration-20261029.json`, preserving
all earlier records. Its dispatcher requires the recorded levelling failure
and successful calibration, rechecks live readiness and actual G-code, refuses
an existing attempt record, and never retries an uncertain start automatically.
The owner-authorized request uses `--retry-after-calibration --start --bed-clear`.

The new start command was acknowledged successfully and the uploaded file was
read back with its matching validated SHA-256. On this attempt the X1C passed
heated-bed levelling and advanced to extrusion calibration with no errors.
At **14:51:58 Sydney time** it reached **RUNNING, layer 1/282**, nozzle **220 C**
and bed **55 C**, active error 0, retained reason 0 and HMS empty. About
467 minutes remained. See `heated-levelling-after-calibration-20261029.json`,
`dispatch-summary-after-calibration-20261029.json` and the new run record.

This is first-layer telemetry, not physical adhesion or completion proof.
The bounded startup observer ended after that checkpoint; no recurring
monitor was created. Passing the same file after calibration does not separate
a transient fault from improvement caused by recalibration. The prior
physical cause remains unknown.

## Owner-reported partial physical failure - 31 October 2026

The owner reports the middle/inner frame printed partially poorly: the lower
diagonal female-threaded sections did not print well **despite their supports
being present and printing properly**. The upper diagonals printed well.
Lower/upper describe the original print orientation. The
[owner photo and outcome record](physical-outcome-20261031/README.md) preserve the observation.
The cause remains unknown; this report does not accept female-thread fit or
assess the other four hubs on Bed 2.
