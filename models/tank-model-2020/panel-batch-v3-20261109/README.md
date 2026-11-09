# Three successful wall panels and one notched floor, 2026-11-09

The owner reported that the complete 0.25 mm bilateral wall panel works very well,
requested three more and one base plate with corner notches, and explicitly
confirmed the X1C plate clear.

- **Three walls:** byte-for-byte copies of the successful 70 x 70 mm wall source
  and STL, including its four 0.20 mm mouse ears. Upright as before.
- **One floor:** the same bilateral stepped profile and 0.25 mm clearance, with
  all four original 5.3 x 5.3 mm corner notches to clear the full-height uprights.
  It prints diagonally upright, rotated 45 degrees in its own plane. This makes
  the notch ledges printable at 45 degrees instead of unsupported flat undersides.
  Four removable ears brace its two bottom contacts separately; the notch stays open.
  The four needle-thin notch tips are blunted by 0.249809 mm in print Z, removing
  under 2 cubic mm, so the first and last layers have at least 0.499617 mm width.
  The floor prints 91 mm high; nominal dimensions, slot clearance and all four
  notches are preserved.

All four are solid, four-wall PLA parts, using the complete saved machine,
process, filament and project presets from the successful wall: 0.10 mm layers
after 0.20 mm first layer, 220 C nozzle, **55 C textured PEI bed**, no supports
or extra full brim. The layout prints by layer, with wall centres at
(82,75), (82,128), (82,181) and floor at (176,128) mm. The nearest bounding boxes
have over 13 mm clearance, and everything stays away from the plate perimeter,
front calibration lines and exclusion area. This is a simultaneous four-object
print; it is not a sequential-object job.

**Trim all mouse ears flush before fitting**, especially around the floor's
bottom-facing notch. Walls slide axially into open upright ends before the top
rail is fitted. The floor enters the front rail, then the side rails slide onto
its keys from the rear before the rear uprights are installed. Do not push the
wide keys through narrow slot mouths. The floor's bilateral fit and complete
tank waterproofness remain untested; use sealant along all interior seams after
successful dry assembly.

## Rebuild and check

Use the existing analysis environment pinned in `../panel-fit-v2/README.md`:

```sh
python prepare.py
python prepare_print.py
python verify_print.py
```

`prepare.py` preserves the exact successful wall mesh and renders the floor with
the checkout's native compiler. It verifies closed connected solids, no fitting
undersides steeper than 45 degrees, all four corner notches, expected removed
volume, zero overlap with all four bottom rails and four uprights, 1,203 sampled
floor/front-rail and side-rail insertion positions, and collision controls in
both normal directions. `geometry-validation.json` pins source/mesh hashes.

`prepare_print.py` copies all four successful presets byte-for-byte and fixes
the layout. `verify_print.py` independently checks exactly three walls and one
floor, the saved presets, exported meshes and poses, topology, model paths,
actual plate/printer/material, G-code checksum, every layer height and actual
thermal settings. The trimmed floor is exactly 91 mm tall; all expected 0.10 mm
layers must be present. The shared audit keeps its strict layer-height checks.

`print_x1c.py` is read-only unless `--start --bed-clear` is supplied. Dispatch
rechecks readiness and loaded PLA, repeats thermal preflight, verifies the full
upload by SHA-256 and sends one start command. `print-run.json` records the
owner's authorization, command identity and observations, preventing duplicate
starts or rebuilding an already-dispatched batch. Do not resend on an uncertain
acknowledgement. First-layer telemetry and physical completion are separate evidence.

Historical print packages, earlier fit samples and the standalone successful
wall trial remain intact. This batch does not change the older one-sided tank
assembly sources or the published manual.

## Dispatch and evidence

The final slice contains four objects and 909 layers, estimated **788.35 minutes
(13 h 8 min)** and **144.02 g**. The audit checked 765,368 deposited model segments
and all 909 layers' actual thermal settings. Each wall has all 699 expected layers
through 70 mm; the floor has all 909 through 91 mm. The initial untrimmed floor
slice omitted its final thin tips; that failed the layer audit and was never sent.
The tiny flat lands described above resolve it without relaxing the verifier.

At **12:17 Sydney time on 2026-11-09**, the X1C accepted the one-shot start with
SUCCESS after full uploaded-content SHA-256 read-back matched
`fdee6c9a5e7b9e0201313bc7068c9896e4425c2c8d1eefa49523f48129a4c7a3`.
The exact remote job is
`tank-x1c-three-walls-notched-floor-20261109-20261109-121726.gcode.3mf`.
The existing loaded external PLA spool was selected, with no printer faults
reported before dispatch. `x1c-three-walls-notched-floor-20261109/print-run.json`
records acknowledgement and the bounded startup observations. Acknowledgement
alone does not establish a successful first layer, finished parts or floor fit.

At 12:19:25 Sydney time, that exact job reported RUNNING in warm-up, layer 0/909,
788 minutes remaining, bed 54 C, nozzle 164 C, print_error=0 and no HMS alerts.
The bounded startup observer continues recording through first layer or a fault;
this warm-up reading does not yet establish first-layer adhesion.
