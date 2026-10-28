# Split plates with substantial brims — 28 October 2026

## Bed 2 on X1C

On 28 October 2026 the owner requested Bed 2 on the X1C and confirmed its bed
clear. The X1C-specific five-part slice (inner frame plus four hubs) passed
geometry, support/thread clearance and full G-code thermal checks. The upload
was read back with a matching SHA-256 and one start command was acknowledged
successfully at about 22:39 Sydney time. Fresh telemetry confirmed the new
282-layer job RUNNING, with bed 55 C and nozzle 220 C targets and no faults.
A transient homing warning during levelling cleared on fresh reads without
control commands. At 22:52:09 the job had passed levelling/extrusion calibration
and reported stage 0, nozzle 220 C and bed 55 C, without faults. The layer counter
was still 0; first-layer completion and adhesion remain unverified. See [the Bed 2 record and reproduction notes](bed2/x1c/README.md).
This is dispatch/startup evidence; completed printing and fit remain unverified.

## Owner update: Bed 1 printing well

On 28 October 2026 the owner reports that the corrected **P1S split Bed 1 is
printing well**. This is an in-progress physical observation; completion has
not yet been reported. See [the progress record](bed1/physical-progress-20261028.json).

The owner also confirms that the **previous fastener-free tesseract printed
well on the A1**; see [that earlier-design outcome](../../../examples/assemble/tesseract/physical-outcome-a1-reported-20261028.json).
The early failures of the newer, full one-bed layout therefore remain
unexplained by the observations alone. The confirmed 35°C plate-profile error
is a candidate contributor, while this retry also changes cleaning, grouping,
brims and first-layer settings. This is not a controlled comparison that can
separate those factors or rule out a new-design/layout issue.

**Current state: corrected first plate restarted after bed cleaning.** On
28 October 2026 the owner cleaned the P1S bed and requested the retry, superseding
the earlier hold. The package under `bed1/slice-corrected/` passed fresh plate/
thermal checks, was uploaded and read back with a matching SHA-256, and received
a successful start acknowledgement. Fresh telemetry confirmed the new 78-layer
job RUNNING. At 17:44:24 Sydney time it reached layer 1/78, with bed 55°C and
nozzle 220°C targets and no active faults. A brief startup fan-speed warning
cleared before the first layer. This is startup evidence, not a completed print
or proof that adhesion is fixed. See `bed1/print-run-corrected.json` and
`bed1/status-corrected-latest.json`.

The owner reports that the A1 full-plate retry also failed early. The A1 had no
known pre-existing issues comparable to the P1S, and unrelated recent prints
worked very well. The owner has seen similar failures on crowded plates,
irregular geometry, small/no brims and features near edges. The initial cause was unknown. A shared slicing configuration error was
subsequently confirmed (below). These observations make a P1S-only explanation less persuasive but do
not diagnose either printer or rule out a layout/adhesion problem.

A1 telemetry was later FAILED at layer 13/496, with error zero and no HMS faults.
The owner's description concerns trouble early in the print; the terminal layer
is not necessarily the first detachment. See
[the owner report](../a1-full-retry/physical-failure-20261028.json) and the earlier
[P1S report/photo](../p1s-full/physical-failure-20261028.json).

The previous packed files specified brims on every part, but only 1.5 mm on the
hubs and caps. That setting did not establish adequate adhesion. The revised
experiment separates geometry classes and sets **8 mm outer brims on every
object**, with 20 mm/s first-layer walls/brims, 25 mm/s first-layer infill and
500 mm/s² initial-layer acceleration. Model and thread geometry is unchanged.

| Plate | Parts | Purpose |
|---|---|---|
| 1 | 8 caps | Broad flat contact; initial P1S retry |
| 2 | Inner frame + 4 hubs | Irregular geometry and inner-frame supports |
| 3 | 4 hubs | Additional corner bodies, with space around each |
| 4 | 8 struts | Uniform, shorter upright pieces |
| 5 | 12 bars | Tall pieces isolated from other shapes |

All 37 items are present exactly once. Every reserved brim/support outline is
inside the 20–236 mm region of the 256 mm bed and clears the exclusion. The first
plate's cap centres are at 64/128/192 mm in a ring, leaving the middle empty;
actual projected bodies are at least about 44 mm from the bed edges. Reserved
envelopes are separated by at least 8.03 mm on that plate. The other layouts also
have separate, non-overlapping brim/support envelopes.

## First-plate evidence and dispatch

The P1S slice contains eight objects, no supports and no slicer warnings.
Estimate: **3 h 3 min, 54.5 g Generic PLA**, 0.4 mm nozzle, textured PEI, 78 layers.
Six walls, 20% gyroid and 0.2 mm layers are retained. The actual first-layer brim
paths are checked per cap: each has over 1,200 deposited brim segments, a wide
outer ring, and all 36 angular sectors represented. `bed1/package-validation.json`
records those paths as well as exported placement, settings, bounds and hashes.
This checks generated toolpaths, not actual adhesion on a physical plate.

The owner initially confirmed the P1S bed clear and approved printing when ready.
The first package was uploaded, read back and dispatched once, but it still had
the now-confirmed wrong plate profile. It was stopped at layer 1 to correct that
error; this was an intentional stop, not a new adhesion-failure observation.
Fresh telemetry reported the expected 78-layer job RUNNING during heating with
no faults. `bed1/print-run.json` preserves that stopped attempt. The corrected retry would
use a distinct `print-run-corrected.json` only after new owner authorization. The first
status packet still carried FAILED/496 from the preceding job; that cached
packet is preserved and distinguished from the subsequent fresh RUNNING/78
report. It was not a failure of this retry.

The earlier instruction to keep it stopped after the temperature correction was
superseded by the owner’s cleaned-bed restart request. The corrected first plate was dispatched on the P1S; Bed 2 has since been
re-sliced and dispatched on the X1C as described above. Beds 3 through 5 have
packed meshes and editable projects with 8 mm brim settings, but **have not
been sliced or dispatched**.
Check and slice each when its physical test is requested; do not reuse a prior
G-code file. The inner frame retains thread-bore support blockers.

## Reproduce

Run with the modelling Python environment from the repository root:

```sh
python3 models/tesseract-v3-threaded/p1s-split-retry/pack.py
python3 models/tesseract-v3-threaded/p1s-split-retry/prepare.py --bed 1
# Slice that bed's full-project.3mf with its presets, --arrange 0 --orient 0.
python3 models/tesseract-v3-threaded/p1s-split-retry/verify.py --bed 1
```

`print_bed1.py` reads status without flags. `--start --bed-clear` refuses an
existing print record and never retries an uncertain start. It accepts the
terminal FAILED state only for the known earlier cancelled tesseract job with no
active fault, not an unrelated failed or running job. Startup reports are gated
by the new job's expected layer count so cached old-state fields are not treated
as a new failure.

The experiment changes packing, part grouping, brim area and initial-layer
speeds together. A better result will establish that this combination helped;
it will not independently identify which factor caused the earlier failures.

## Confirmed plate-profile error and correction

The custom process preset omitted `curr_bed_type`. Although the project metadata
said Textured PEI, CLI `--load-settings` overrode it with **Cool Plate**. The two
failed full plates and the initial split retry therefore emitted `M140 S35` and
`M190 S35`, instead of the intended **55°C** for textured PEI. Live P1S telemetry
also showed about 35°C at layer 1. This was an agent workflow/verification mistake,
not a confirmed mechanical fault. It is a plausible contributor to the adhesion
failures, but the corrected physical trial has not yet established sole causality.
The successful thread coupon and upright rail files used Textured PEI/55°C.

Evidence: [slicing-plate-mismatch-20261028.json](../slicing-plate-mismatch-20261028.json).
The old files/reports remain under `bed1/incorrect-cool-plate/`; their validation
is explicitly invalidated. Both old full-plate reports are invalidated too.

All active custom builders now set the plate in the CLI process preset. All
validators and dispatch helpers require both the exported Textured PEI setting
and final startup bed-target/wait commands at 55°C. The corrected first-plate
package passed those checks and its existing brim/geometry checks. It was
subsequently restarted after owner bed cleaning, as recorded above. The startup fan alert separately observed during calibration cleared
on a fresh report; see `bed1/startup-fan-alert.json`.
