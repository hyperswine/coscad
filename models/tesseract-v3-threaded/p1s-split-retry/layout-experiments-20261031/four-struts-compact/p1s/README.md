# P1S compact four-strut trial - 31 October 2026

Native P1S slice of the existing compact 2 x 2 layout: 28 mm centres,
by-layer printing, four 58.28 mm upright struts and 8 mm brims. Geometry is
unchanged. Six walls, 20% gyroid, crossing avoidance and 12-second layer cooling
with 5 mm/s minimum speed are retained. Actual sliced brim footprints have a
5.647 mm minimum gap, and all four objects deposit on every one of 291 layers.

Actual package geometry, P1S identity C12, Textured PEI bed, final startup
55 C bed wait/target and all deposition-layer 55 C bed / 220 C nozzle settings
pass. No slicer warnings. Estimate 1 h 28 min and 14.54 g PLA.
`package-validation.json` and `compact-validation.json` preserve evidence.

Owner requested this trial on the P1S and explicitly confirmed both plates clear.
The checked upload was read back with a matching hash and one start accepted. After confirmation, the one-shot
`../../../print_strut_trial.py p1s-compact --start --bed-clear` command
revalidates the package and live readiness, freezes checked upload bytes,
reads back the upload hash and sends one start. An exclusive print-run.json
prevents repeats after any uncertain start. No timelapse or first-layer
inspection; normal bed leveling and vibration calibration are enabled.

Shorter travel is an experiment, not proof against curling or nozzle contact.

## Dispatch and latest startup observation

Owner explicitly confirmed both build plates clear on 31 October. One start
was acknowledged successfully after full validation and matching upload
read-back. Remote job: `tesseract-v3-p1s-compact-20261031-124453.gcode.3mf`.

Latest saved observation (2026-10-31T12:45:56+11:00): RUNNING,
layer 0/291, 87 minutes remaining.
Phase: acknowledged. See `dispatch-summary.json` and `print-run.json`.
Completed print quality remains unverified. No repeated start was sent.

## Fresh first-layer follow-up

At 12:52:45 Sydney time the P1S reported this job RUNNING at layer 1/291,
with 81 minutes remaining, measured bed 55 C and nozzle 220.3 C, targets
55/220 C, and no print error or HMS faults. The recurring startup fan-speed
warning cleared on fresh status without any alert clearing or control command.
`print-run.json` preserves the original warning; `startup-followup.json`
and `dispatch-summary.json` record the later cleared/first-layer state.
Completed quality and adhesion remain unverified.

## Physical outcome - 31 October 2026

The owner reports this compact by-layer plate printed almost perfectly before
the threaded portions of the struts, then failed around the thread region.
They suspect head contact knocked components over during travel between the
threads; no initiating collision or exact layer is confirmed. Three fresh
[photos and the outcome record](outcome-20261031-214805/outcome.json) preserve
spaghetti/displaced material. Glare and framing limit evaluation of lower
sections and thread detail. Owner observations remain separate from the
end-state camera review. The correct job reports FINISH/291 with no active
print error/HMS despite the physically failed result.

Closer spacing and shorter travel did not prevent failure in this trial.
The same geometry succeeded in X1C per-object mode, but different printers,
filaments, spacing and scheduling prevent isolating a single causal factor.
No corrective printer commands, movement, alert clearing or new start sent.
