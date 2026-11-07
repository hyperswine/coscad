# Panel fit V3: complete bilateral flange

The owner confirmed all three V2 one-sided samples fit the rails on 2026-11-07.
This second set adds the missing flange on the opposite face of the tongue,
forming a complete stepped T-head that engages both sides of the rail pocket.
The owner subsequently confirmed **all three V3 clearances worked**, on 2026-11-07.
This is recorded in `physical-outcome-20261107.json`; there was no ranked winner or
measured sliding force/play. A [full wall-panel trial](../panel-full-v3/README.md)
uses the middle 0.25 mm clearance next. Full-panel fit and sealing remain untested.

| Diamond holes | Clearance per face | Neck | Full head width | File |
| --- | --- | --- | --- | --- |
| 1 | 0.20 mm | 5.80 mm | 10.60 mm | `double-c020-1holes.coscad` |
| 2 | 0.25 mm | 5.70 mm | 10.50 mm | `double-c025-2holes.coscad` |
| 3 | 0.30 mm | 5.60 mm | 10.40 mm | `double-c030-3holes.coscad` |

Both flange faces follow the actual tank rail's 11 mm and 8.4 mm stepped pockets.
There is clearance at the shoulders, too. The tongue still enters 5 mm into a
6.1 mm-deep slot. The sample is 30 mm long in its sliding direction, with 2 mm
plain noses and 3 mm ramps at both ends to help it enter the rail axially.

## Printing and using the samples

- **Print upright as supplied**, along the sliding direction. The lower flange
  ramps expand by at most 2.4 mm over 3 mm of height, within 45 degrees. Printing
  these two-sided samples flat would suspend the opposite flange above the bed.
- Two integral 0.20 mm mouse-ear pads stabilize the handle. Their edges stay at
  least 0.5 mm outside the rail face and do not change the mating profile. They
  can be trimmed off after printing.
- One/two/three diamond-shaped through-holes identify the variants. Their roofs
  have 45-degree slopes so support material is not needed on fitting surfaces.
- Use 0.10 mm layers after a 0.20 mm first layer to preserve the 0.10 mm neck
  differences, as in the successful V2 test. Keep all three samples in the same
  material/profile when comparing them.
- Slide each sample from an **open rail end** and compare free sliding, rocking
  and pullout resistance. The full T-head is captured behind both lips.

An optional `rail35.coscad` has the unchanged tank slot profile. Prefer the
existing printed tank rails for the physical comparison. This set changes only
test samples; the current full wall/floor sources still use the tested one-sided
design pending these results. Sealant remains required along all inside seams
of the eventual tank after dry assembly.

## Rebuilding and verification

```sh
python3 prepare.py --render --coscad /path/to/coscad
python3 verify.py --coscad /path/to/coscad
```

Run from this directory. The verifier uses the same numpy, trimesh, scipy and
manifold3d versions pinned in `../panel-fit-v2/README.md`. `prepare.py` derives the
geometry from the actual wall source, reflects its flanges, crops the edge,
adds lead-ins and print pads, and writes standalone CoScad files and STLs.
It does not contact or control a printer.

`fit-samples.assemble` groups the three upright samples for native preview and
later print preparation. `coscad plan fit-samples.assemble --png` regenerates the
preview images; the assembly is not a printer-specific sliced package.

`validation.json` records closed single-component meshes, both flange steps on
both sides, symmetric cross-sections, downward-face angles, and axial sweeps
against the actual rail at 0.25 mm intervals. Normal overtravel in both directions,
radial pullout, excess insertion depth and an oversized sample must collide.
These are geometric and sampled-path checks, not physical fit or print-quality
evidence for V3.

## X1C dispatch, 2026-11-07

The owner requested slicing and printing all three samples and explicitly confirmed
the X1C bed clear. `x1c-double-flange-fit-20261107/` records the prepared project,
intent, exported-package verification and one-shot LAN dispatch. Only the three
samples are included; their mouse-ear pads are part of the normal meshes.

- Upright, 0.10 mm layers after 0.20 mm first layer; 299 layers.
- Four walls, 100% infill, no support structures or additional brim.
- Loaded external PLA; textured PEI, bed 55 C and nozzle 220 C.
- No slicer warnings. Estimate 55.44 minutes including startup and 5.99 g.
- Exported meshes, identification holes, plate position and all 299 layers' actual
  temperatures passed verification. Full upload read-back SHA-256 matched before
  the single start command was sent and acknowledged SUCCESS.

`prepare_print.py`, `verify_print.py` and `print_x1c.py` reuse the verified V2
workflow with a separate V3 job identity. The verifier derives the expected layer
count from the actual sample height, so it accepts 30 mm upright samples without
weakening the original flat-sample check. A durable `print-run.json` prevents
duplicate starts. `print_x1c.py` is read-only unless `--start --bed-clear` is given.
Startup observations are in that run record; completion and physical fit require
separate evidence.

At 2026-11-07 16:13:44 Sydney time, the X1C reported RUNNING at layer 1/299,
220 C nozzle, 55 C bed, about 47 minutes remaining, print_error=0 and no HMS alerts.
That reading confirmed startup only. A later read-only check reported FINISH,
299/299 layers and no faults, and the owner reported all three samples fit.
