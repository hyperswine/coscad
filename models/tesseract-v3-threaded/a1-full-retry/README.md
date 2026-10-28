# A1 retry after P1S adhesion failure — 28 October 2026

The owner reported an early P1S failure with detached pieces and spaghetti in
several places, supplied a photograph, and requested the same tesseract on A1
using **physical AMS slot 1**, avoiding yellow PLA in slot 3. The A1 bed was
explicitly confirmed clear. P1S telemetry subsequently reported FAILED at layer
5/496; that is the last reported layer, not proof of the first detachment time.

The [P1S failure record](../p1s-full/physical-failure-20261028.json) preserves the
photo and observations. Its cause is unknown. Bed contamination, Z-belt friction
or tension, and lead-screw friction are hypotheses. The owner reports recent
lead-screw cleaning/lubrication. This retry changes printer and filament as well
as machine-specific slicing/calibration, so its outcome will not identify a
single root cause by itself.

All 37 geometry meshes, positions, brims and thread-support exclusion volumes
match the P1S attempt. `retry-context.json` records that comparison. Only the
machine profile, its start/end/calibration routines and filament routing change.
The A1 uses its own 0.4 mm machine profile, 0.2 mm layers, Generic PLA, textured
PEI, six walls and 20% gyroid. The one-plate slice reports no warnings and
estimates **20 h 16 min, 309.4 g**. This is a retry, not a claim that adhesion or
motion issues have been fixed.

Before dispatch the A1 reported white PLA in `ams0` (physical slot 1), yellow
PLA in `ams2` (physical slot 3), 0.4 mm nozzle, SD card available and no faults.
`print_full.py` explicitly requests `use_ams=True, ams_mapping=[0]`, rechecks that
white PLA is present in physical slot 1, verifies the remote file hash, and sends
one start command. `full-print-run.json` is the durable dispatch record. Read
status without flags; never resend after an uncertain acknowledgement.

The selected package is `slice/tesseract-full-a1.gcode.3mf`. `verify_full.py`
checks the actual exported geometry, settings, one-plate count, paths, brims,
thread exclusions and bed-connected support samples. `package-validation.json`
records those results before the dispatch helper permits starting.

The A1 acknowledged the verified upload/start successfully. Fresh startup
telemetry reported the expected 496-layer job RUNNING during heating, with
print error zero and no HMS faults. `dispatch-summary.json` records that
observation; the active tray is recorded as filament loads. Startup does not
establish that the earlier adhesion failure will not recur.

At 12:43 Sydney, live telemetry reported `tray_now=0`, confirming physical
AMS slot 1 had loaded. The expected job was RUNNING in preparation with no
print error or HMS faults. This verifies the selected spool, not print quality.

## Owner-reported failure

The owner reports that this A1 attempt also failed early, before or around
layers 4–5. The cause is unknown. The A1 had no known pre-existing issues like
the P1S, and recent unrelated prints worked well. The owner has seen similar
problems on crowded plates, irregular parts, parts with little/no brim, and
geometry near bed edges. These are observations and hypotheses, not a diagnosis.
The structured record is `physical-failure-20261028.json`.

The revised experiment separates geometry classes across plates and gives every
object a substantial brim. Only the first, flat-cap plate is authorized to start
on the confirmed-clear P1S; the other layouts are prepared for later testing.
