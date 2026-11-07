# Full bilateral wall-panel trial, 2026-11-07

The owner reported all three double-flange coupons fit the existing rails and
requested a whole panel next, explicitly confirming the X1C plate clear.
This trial uses **0.25 mm per face**, the middle tested clearance, rather than
claiming it was the best of the three.

`panel-wall-double-c025-upright.coscad` is one complete **70 x 70 mm wall** for the
100 mm tank frame. Its 5.7 mm neck and bilateral 10.5 mm stepped heads match the
tested V3 profile. Heads remain only on the two edges parallel to insertion;
the other two edges pass through the rail mouths. The full-length heads stop
5.3 mm short of the corners, with 3 mm ramps, as required to clear the horizontal
rails and print without supports.

Print upright, along the insertion direction. Four integral 0.20 mm mouse ears
stabilize the plain lower tongue. **Trim the ears flush before dry fitting** so
the lower edge can enter its rail. Slide the keys into open upright ends and
add the top rail afterwards; do not press a head through the narrow slot mouth.

The existing `../panel_wall.coscad`, floor, assembly and historical print packages
are unchanged by this isolated whole-panel trial. In particular, these samples
remain reproducible from their original one-sided intermediate source.

## Checks and reproduction

Run with the analysis environment documented in `../panel-fit-v2/README.md`:

```sh
python prepare.py
python prepare_print.py
python verify_print.py
```

`prepare.py` uses CoScad's native backend. It checks a closed, connected solid,
70 mm full panel dimensions, support-free downward angles, 401 insertion positions
between both actual tank uprights, the final four-rail fit, and collision controls
for overtravel on both sides. `geometry-validation.json` records the evidence and
source hashes. The ear-free model is used for assembled fit checks.

`verify_print.py` reuses the fit-sample package audit with exactly **one** expected
object; the sample workflows still require three. It checks source/STL identity,
actual exported mesh and pose, topology, every deposited model path, all layer
heights, the actual X1C printer/plate/material and G-code checksum, and thermal
preflight. The project is centred at 128,128 mm, away from the X1C front calibration
area and exclusion zone.

Use the same 0.10 mm layers (first layer 0.20 mm), four walls, 100% infill,
220 C nozzle and **55 C textured PEI bed** as the successful coupons. No supports
or extra full brim; the mouse ears are part of the model.

`print_x1c.py` is read-only by default. `--start --bed-clear` uses the existing
one-shot tank dispatcher: check fresh idle/fault/spool status, repeat thermal
preflight, verify the full upload by SHA-256, then send one start command.
The durable job record prevents sending that job twice. Startup and actual
physical success are separate evidence; full-panel fit remains untested until
owner feedback. This is not a waterproofness test: the tank still needs sealant
along all inside seams after dry assembly.

## X1C dispatch

`x1c-full-panel-c025-20261107/` records the exact source/STL identities, presets,
project, slice, package audit, dispatch settings and run. The package contains one
object and 699 layers; Bambu Studio estimates 205.52 minutes and 36.46 g.
The verifier checked 188,532 deposited model segments, all 699 layers' thermal
settings and the exported mesh (maximum vertex discrepancy 0.0000035 mm).

At 17:44 Sydney time on 2026-11-07, the X1C accepted the one-shot start with
SUCCESS after full remote SHA-256 read-back matched
`4ec58c4c473b338066abfa2424f1d9f643316a75e8d716dac01cc0c3f61ccd16`.
The job is `tank-x1c-full-panel-c025-20261107-20261107-174420.gcode.3mf`.
The existing external PLA spool is selected. Calibration and startup observations
are appended to `print-run.json`; acknowledgement alone is not evidence that
the first layer has printed or that the part fits.

At **17:54:06 Sydney time**, the same job reported RUNNING at **layer 1/699**,
220 C nozzle, 55 C bed, about 197 minutes remaining, print_error=0 and no HMS
alerts. The startup monitor then ended normally. This confirms the first layer
was reached; completion, print quality and whole-panel fit require later evidence.
