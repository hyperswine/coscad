# Orbit Tower v2 - tighter-fit candidate

Based on the successfully printed and assembled original Orbit Tower. The owner
found assembly intuitive but the fits somewhat loose. The two-hand top's tighter
stems also worked well, but no single winning clearance was recorded.

| Interface | Original | Candidate |
|---|---|---|
| 6 mm square stacking peg | 6.6 mm socket, 0.30 mm/side | 6.2 mm socket, 0.10 mm/side |
| 4.8 mm satellite peg | 5.5 mm hole, 0.35 mm radial | 5.1 mm hole, 0.15 mm radial |
| 9.6 mm rotating journal | 10.4 mm bore, 0.40 mm radial | unchanged |

Axial stack dimensions, engagement depth, entry bevels and assembly interface
frames remain unchanged. This transfers the top's successful smaller-clearance
approach, not a proven calibration for these smaller square and round features.
It remains a removable gravity-fit kit, not a snap-retained toy.

Before a full kit, print one base/axle/rotor/satellite/cap set to check stack
wobble, hand insertion/removal and free rotation; compare 0.10 and 0.15 mm
square clearances if the smaller socket binds. Do not use the locating-joint
clearance for the rotating bearings without a separate running-fit test.

No slicing or printer dispatch is authorized by this design change. The native static
assembly check passed for all 20 parts with zero overlaps and zero clearance
warnings (`validation.json`). Full rotation sweep, packing and physical fit
remain untested.

Run from the repository root:

```sh
cabal run coscad -- check models/orbit-tower-v2/orbit-tower.assemble
```

## X1C full-kit print preparation - 2026-11-12

Owner requested the full new tower, confirmed the X1C bed clear and reported
white filament loaded. The separate `x1c-tower` project has all 20 parts in a
central layout with at least 5 mm bounding-box separation, outside the front-left
exclusion. It uses PLA, 0.20 mm layers, three walls, 15% gyroid, four top/bottom
layers, no supports and no brim. Package audit passed: 100 layers, about 1.6 h
and 39 g. Actual temperature commands are independently checked before dispatch.

`prepare_print.py`, `slice.py x1c-tower`, and `verify_packages.py x1c-tower`
prepare and verify the package. `print_job.py` rechecks pinned inputs and delegates
to the existing one-shot LAN dispatcher, which verifies uploaded bytes and records
its single control attempt in `x1c-tower/print-run.json`. Do not resend that job.
Startup does not establish adhesion, completion or the tighter joints' fit.


### Offline Sol workflow

From this model directory run `"$SOL" slice.sol` to slice the prepared
`x1c-tower` job in a scratch build. It checks source and project hashes before
and after the slicer and writes the new input receipt last. Run
`SOL="$SOL" python verify_packages.py x1c-tower` on the resulting package.
