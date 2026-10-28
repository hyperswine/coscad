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
