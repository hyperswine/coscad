# Two-hand spinning top, 2026-11-02

A large top you spin like a fire drill: rub the stem between both palms, then let it go
onto a hard floor. All geometry is CoScad written by design.py. Sliced for the A1 in PLA. Printing on the A1 since 2026-11-02 19:53 (white PLA,
AMS slot 1, the owner confirmed the plate clear); layer 1 of 522 at 20:02. Not spun yet.

| | Value |
| --- | --- |
| Size | 160 mm across, 44 mm body, 134 mm tall with the stem |
| Parts | body (printed upside down) + stem (hex peg into the body), one A1 plate |
| Mass | 345 g solid PLA; 407 g with 12 optional M8 nuts in the rim |
| Print | 7.1 h, 344 g, no supports, 100% infill |

## How it's shaped

- **Mass where it helps.** Most of the plastic is in a rim ring 20 mm wide at 60–80 mm
  radius, low down. A wide, low rim gives a high spin inertia and a low centre of gravity,
  so it stays upright at slow speeds and keeps spinning longer.
- **Tilt room.** The rim sits 20 mm above the tip, so the top can lean 14.6° before the
  rim edge touches the floor.
- **Prints upside down with nothing overhanging.** With its flat top face on the plate,
  the body is:
  - a 5 mm web on the plate;
  - the rim ring and the central post growing upward from it;
  - the post tapering to a 2.5 mm rounded tip at the very top.

  Every surface faces up or is vertical. The hex socket's blind end is pointed (a 45° roof),
  not a flat bridge. The overhang scan found nothing steeper than 45° apart from a
  0.3 mm² facet at the point of that roof.
- **Stem.** The stem is a 16 mm, 12-sided grip, 90 mm long, so palms don't slip. It has a
  12 mm hex peg that goes 14.5 mm into the socket (0.25 mm clearance per flat). The hex
  carries the twist; a drop of glue makes it permanent. It prints standing on its flat
  grip end, peg up.
- **Optional ballast.** 12 hex pockets on the rim's underside take M8 nuts (13 mm across
  flats, 6.5 mm deep, 2.3 mm walls). Glue them in with CA or epoxy. They add 62 g on the
  rim: spin inertia +23%, centre of gravity 2 mm lower.

## What the numbers say (verify.py)

A top stays upright ("sleeps") when its spin speed is above
`sqrt(4 m g l I_tilt) / I_spin`: m is the mass, l the height of the centre of gravity above
the tip, and the inertias are about the axis (spin) and about the tip (tilt).

| | Plain | With 12 M8 nuts |
| --- | --- | --- |
| Mass | 345 g | 407 g |
| Centre of gravity above the tip | 37.8 mm | 35.6 mm |
| Spin inertia | 1.34e-3 kg m² | 1.64e-3 kg m² |
| Tilt inertia about the tip | 1.25e-3 kg m² | 1.44e-3 kg m² |
| Spin / tilt | 1.07 | 1.14 |
| Stays upright above | 181 rpm | 166 rpm |

A firm two-hand rub moves the grip's surface at roughly 1 to 2 m/s, about 1,200 to
2,500 rpm on this stem: 7 to 14 times the sleeping speed. How long it spins depends on
the tip and the floor. These are static, rigid-body numbers, not measurements.

## Printing (A1)

`a1-top`: body at (105, 128) upside down and stem at (215, 128) standing, on Bambu's stock
0.20mm Standard A1 process with Bambu PLA Basic.
- **Settings.** Nozzle 220 C, textured PEI at 65 C, 4 walls, 100% infill, 5 top and
  bottom layers, 3 mm brim. Infill is 100% so the mass and inertia match the numbers
  above.
- **Checks.** `verify_packages.py` passed: slicer success, no warnings, no supports, paths
  on the plate, 344 g against 345 g modelled, printer model id. The thermal preflight
  passed too.

`dispatch.py a1-top --spool <name> --confirm-bed-clear --authorization-note '...'` sends
it. Use the spool's AMS slot name (`ams0`...) or `external`. It refuses unless the A1 is
idle with no faults and the named PLA spool is loaded.

## Using it

1. Press the stem's hex peg fully into the socket (glue optional).
2. Hold it upright with the tip just above a hard, smooth floor.
3. Rub your palms quickly in opposite directions along the grip, then open them and let
   it drop the last few millimetres.
4. If the PLA tip wears flat, sand it round again.

## Files

Run in this order: design.py, then `coscad stl body.coscad -o body.stl` and
`coscad stl stem.coscad -o stem.stl`, then verify.py (Python 3.13 with trimesh,
matplotlib), then prepare_slice.py, slice.py and verify_packages.py.

| File | What it is |
| --- | --- |
| `body.coscad`, `stem.coscad` | The two parts, as written by design.py |
| `design.json` | Every dimension |
| `validation.json`, `preview.png` | Mass properties, stability, overhang scan, pocket walls |
| `stl/` | The print-oriented meshes |
| `a1-top/` | Presets, layout and package validation |
