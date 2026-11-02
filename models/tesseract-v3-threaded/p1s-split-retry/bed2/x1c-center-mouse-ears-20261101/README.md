# Center-only X1C comparison - 1 November 2026

The owner reports the center printed a lot better with the revised A1 settings,
but cannot separate revised supports/adhesion, isolating the center and using
A1. This is the requested X1C comparison of the same center-only setup.

Frame and all four 0.2 mm CoScad mouse-ear pad meshes are byte-identical to the
improved A1 variant. Every explicit process override and the three cooling
overrides match in the actual exported packages; `comparison-to-a1.json`
records that check. Orientation, placement, six walls, tree walls/branches,
0.16 mm support gaps, three interface layers, interface spacing/speed and
thread blockers are retained. Full brims/rafts are disabled; the four pads are
removable sacrificial geometry. Physical AMS slot 1 belongs only to A1; this
X1C comparison uses its already loaded external PLA.

Native X1C machine/startup settings and external PLA differ from A1. This
therefore does not isolate printer hardware from filament and every profile
default. The owner-reported A1 improvement is not a claim of perfect thread fit.
The owner has now physically inspected this X1C result; the outcome is recorded
below and in `physical-outcome-20261101.json`.

Fresh checks pass native X1C identity, unchanged geometry/pads, actual pad
paths, bed/keep-out, bed-connected trees, zero support-thread intrusions,
no slicer warnings, and 55 C Textured PEI / 220 C nozzle for every deposition
layer. Estimate **3 h 31 min, 39.2 g PLA**.

`prepare.py` refuses an existing print-run record. It uses the original CoScad
frame mesh plus `mouse-ear.coscad`, then native X1C presets. Slice
`full-project.3mf` using `slice/slice-command.json` and run `verify.py`.
`../../print_batch_20261101.py x1c-center` only reads status; `--start
--bed-clear` is one-shot, verifies upload read-back and saves durable intent.
See `print-run.json` / `dispatch-summary.json` for actual startup evidence.

## First-layer startup

At **13:08:35 Sydney time on 1 November**, the unique X1C center job reached
**RUNNING, layer 1/282**, with **202 minutes remaining**, measured nozzle
220 C / bed 55 C, matching targets, no active print error and no HMS.
The checked upload read back with an identical hash and one start was accepted.
This confirms startup, not improved lower-thread quality; physical inspection
is required to compare the completed center with the owner's improved A1 part.

## Owner-reported physical outcome - 1 November 2026

The center-only X1C print with the revised tree supports and mouse-ear pads
printed better overall. However, **one lower diagonal had the same defect as
the original X1C center print**. The owner describes the corresponding revised
A1 center as **almost perfect**. This is a partial improvement, not full
acceptance of the lower diagonal or measured female-thread fit.

The defect recurring with the center printed alone shows that isolation alone
did not eliminate it on X1C. Together with the near-perfect A1 result, this
strengthens the suspicion of an X1C-specific printing/setup difference rather
than bed crowding alone. It does not establish an X1C hardware fault: loaded
filament, native machine defaults and calibration still differ, despite the
matching geometry, explicit support/process/cooling settings and mouse-ear pads.
The exact cause and failing layer remain unknown. No new photo or dimensional
measurements accompany this owner report.
