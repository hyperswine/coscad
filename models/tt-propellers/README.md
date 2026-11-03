# TT gearmotor propellers, 2026-11-02

Four water propellers and a shaft fit coupon for the yellow TT hobby gearmotor (1:48), as a
first step toward motorised versions of the beach outrigger and the beach monohull. All
geometry is CoScad written by design.py. Printed on the X1C on 2026-11-02 (black PLA, external spool, sent 20:20, layer 1 of
158 at 20:26) with flow calibration and first-layer inspection off. Record:
`x1c-props/print-run.json`.

**Result (owner report, 2026-11-03):** printed well, no issues. Not tested on a motor yet:
shaft fit and thrust are still unmeasured.

| Design | Blades | Diameter | Pitch | Hub | Mass | 6 V: rpm, static thrust | 3 V |
| --- | --- | --- | --- | --- | --- | --- | --- |
| P2-50 | 2 | 50 mm | 75 mm | 17 mm | 3.5 g | 229 rpm, ~3 gf | ~0.7 gf |
| P3-50 | 3 | 50 mm | 60 mm | 14 mm | 3.4 g | 229 rpm, ~4 gf | ~0.9 gf |
| P3-60 | 3 | 60 mm | 90 mm | 19 mm | 5.7 g | 227 rpm, ~9 gf | ~2 gf |
| P4-40 | 4 | 40 mm | 60 mm | 12 mm | 2.6 g | 230 rpm, ~2 gf | ~0.5 gf |

## What the estimate says

A TT gearmotor turns only about 230 rpm at 6 V (110 at 3 V). Any propeller small enough to
sit under these hulls barely loads it: the largest, P3-60, uses about 1% of the motor's
stall torque and pushes about 9 gram-force. The estimate is blade-element momentum theory
at zero boat speed, with thin printed sections at low Reynolds number. Treat it as a
ranking, good to perhaps a factor of 1.5.

Two ways to match the motor better:
- **Speed it up.** Thrust grows with the square of speed. A printed step-up gear pair of
  about 3.5:1 would give P3-60 roughly 100 gf, and the motor would work near half its
  stall torque, where it gives the most power.
- **Paddle wheels.** A paddle wheel 60–80 mm across turns happily at the motor's own
  speed and needs almost no draft, which suits these shallow hulls (20–25 mm deep).

These four are still worth printing: they test the shaft fit and give a measured
baseline. Hang the motor over a bucket on a kitchen scale and read the thrust.

## Design

- **Blades.** At each of nine radii, a thin section is set at the helix angle
  atan(pitch / 2πr): 60–70° at the root, 21–26° at the tip. Neighbouring sections are
  hulled in pairs, which makes a twisted blade. The chord is widest at mid-span and
  thickness tapers from 2.2–2.8 mm at the root to 1.0–1.2 mm at the tip.
- **Hub.** 12 mm across, long enough that no blade reaches past either end.
- **Handedness.** Right-handed: seen from the motor, they push water away when turning
  clockwise. Swap the motor leads to reverse.
- **Bore.** Double-D through the hub: 5.4 mm round with flats 3.7 mm apart, plus 0.15 mm
  clearance. The TT shaft's centre screw can hold the propeller on.
- **Coupon.** Three rings with the same bore at 0.10, 0.15 and 0.25 mm clearance, marked
  with 1, 2 and 3 notches. TT shafts vary by maker. If 0.15 is loose or tight on yours,
  change `CLEAR` in design.py and regenerate.

## Printing (X1C)

`x1c-props`: all five parts on one plate, standing on their hub ends, kept at y ≥ 82 mm,
clear of the X1C's flow calibration lines at the front.
- **Settings.** Bambu's stock 0.12mm Fine process, PLA at 220 C, 3 walls, 100% infill,
  3 mm brim, and tree supports from the plate under the blades.
- **Brim.** Next time use mouse-ear brim pads instead of a full brim (see the tank model
  README).
- **Estimate.** 1.7 h, 20 g, 158 layers.
- **Checks.** `verify_packages.py` passed: slicer success, no warnings, supports present,
  paths clear of the exclusion corner, weight, printer model id. The thermal preflight
  passed too.

`dispatch.py x1c-props --spool external --confirm-bed-clear --no-flow-cali
--no-layer-inspect --authorization-note '...'` sends it. The two `--no-...` flags turn
off the X1C's lidar flow calibration and first-layer inspection, which struggle with very
dark filament such as black.

## Files

Run in this order: design.py, then `coscad stl NAME.coscad -o stl/NAME.stl` for each
part, then verify.py (Python 3.13 with trimesh, matplotlib), prepare_slice.py, slice.py and
verify_packages.py. `design.json` holds every station; `validation.json` and `preview.png`
hold the checks and the estimates.
