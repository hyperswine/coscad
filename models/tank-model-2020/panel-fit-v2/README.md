# Keyed sliding panels, 2026-11-07

The owner reported the original 5 mm flat panels were floppy. The modeled rail has
a 6.2 mm throat, leaving 1.2 mm total thickness play with those panels. This revision
uses a 5.7 mm neck: 0.25 mm nominal clearance per face, 0.5 mm total. These are design
clearances, not measured printed gaps. On 2026-11-07 the owner confirmed that all
three X1C samples fit. No preferred clearance or measured insertion force/play was
reported; see `physical-outcome-20261107.json`. This confirms the one-sided samples,
not full panels or waterproofing. The [next samples](../panel-fit-v3-double-flange/README.md)
have flanges on both faces of the tongue.

The two keyed edges follow the rail's stepped pockets, with clearance from the
shoulders as well as the walls. Each has a shallow wide rib at 2.7 mm insertion depth
and a narrower rib at 4.25 mm. Their outer faces lie 5.25 and 3.95 mm from the panel
midplane. Only one face has ribs, so the other is flat on the print bed, without
support material on the sliding surfaces. The ribs stop 0.3 mm inside the opening's
ends and taper over 2 mm at both ends. The tongue tip stops 1.1 mm short of the
modeled slot floor.

Wall keys run vertically in the upright slots. The top/bottom edges remain narrow
enough to pass through a rail mouth. Floor keys run along the side bottom rails;
slide those rails onto the floor from the open rear before fitting rear uprights.
Four undercut edges on every panel would prevent the original drop-in assembly.
The revised assembly declares the axial joints and ordering explicitly.

## Fit samples

`prepare.py` crops a small keyed edge from the **actual wall source**, rather than
maintaining a separate approximation. Three through-hole counts identify samples:

| Holes | Per-face clearance | Neck | File |
| --- | --- | --- | --- |
| 1 | 0.20 mm | 5.80 mm | `panel-c020-1holes.coscad` |
| 2 | 0.25 mm | 5.70 mm | `panel-c025-2holes.coscad` |
| 3 | 0.30 mm | 5.60 mm | `panel-c030-3holes.coscad` |

Print samples flat, ribs up. Test by **sliding from an open rail end**, not by
forcing the key through the throat. Prefer an existing tank rail so the sample
captures that print's dimensional error. `rail35.coscad` is an optional short rail
with the unchanged tank profile; print it upright. Pick the tightest sample that
slides by hand along the rail without force, binding or obvious rocking. If a
different clearance wins, update `clearance` in **both** full panel files, regenerate
the samples, and rerun verification before slicing full panels.

```sh
python3 models/tank-model-2020/panel-fit-v2/prepare.py --render --coscad /path/to/coscad
```

This generates sources and STLs only; it does not slice, schedule or start a print.
Old tank print packages still contain the original flat panels.

## X1C fit-sample print, 2026-11-07

`x1c-panel-fit-20261107/` contains the three samples only, printed flat at the
centre of the plate. The owner explicitly confirmed the X1C bed clear. Loaded
external PLA was reported by the printer. `prepare_print.py` reuses the tank's
project builder; `verify_print.py` checks the actual exported meshes, identifying
holes, central toolpaths, machine identity, actual plate type and temperatures.

- 0.10 mm layers after a 0.20 mm first layer preserve the 5.8/5.7/5.6 mm necks
  instead of rounding two variants onto the same 0.20 mm layer height.
- Four walls, solid infill, no supports or brims on the mating surfaces.
- Textured PEI, 55 C bed and 220 C nozzle, verified across all 81 G-code layers.
- Slicer estimate: 40 minutes including startup, 5.77 g. No slicer warnings.
- The existing tank LAN dispatcher verifies the full upload by SHA-256, writes an
  exclusive intent record and sends one start command. `print_x1c.py` reads status
  by default; `--start --bed-clear` explicitly starts this job once.

The accepted command and startup observations are in `print-run.json`. Successful
startup does not establish completed print quality or physical fit in a rail.

The X1C acknowledged the single start, and the uploaded file read back with the
expected SHA-256. At 2026-11-07 14:42:43 Sydney time it reported RUNNING, layer 1/81,
220 C nozzle, 55 C bed, about 31 minutes remaining, no print error and no HMS alerts.

## Geometry checks

```sh
python3.13 -m venv /tmp/coscad-panel-check
/tmp/coscad-panel-check/bin/pip install numpy==2.5.4 trimesh==5.2.0 scipy==1.18.1 manifold3d==3.5.4
/tmp/coscad-panel-check/bin/python models/tank-model-2020/panel-fit-v2/verify.py --coscad /path/to/coscad
```

The verifier renders the actual sources with CoScad's pinned native kernel, checks
closed single-component meshes and flat print undersides, then uses an independent
Manifold Python build to check seated interference and insertion sweeps at 0.5 mm
increments for all three clearances. Normal overtravel, radial pullout and an
oversized panel must collide, so a disconnected or undersized non-fitting part
cannot pass just because intersections are zero. It also runs the whole assembly's
native interference check and planner and checks the required assembly order.
`validation.json` records source hashes and results. These are sampled geometric
checks, not a physical sliding-fit, stiffness, sealing or print-quality test.

Sealant is still required continuously along every inside edge after assembly.
Leave the dry sliding interfaces free of sealant until the panels are fitted.
