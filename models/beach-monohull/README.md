# Beach monohull with sealed buoyancy tanks, 2026-11-01

A much larger version of the bath-size open monohull (`../twin-boat-trials`), designed with reserve buoyancy. That boat sank in surf at Monterey beach: solid PLA is denser than water and
it had no sealed air, so once a wave filled it the plastic alone was 8.7 g heavier than the
water it displaced. This one keeps an open cockpit but surrounds it with ten sealed tanks.
It floats upright when swamped, and still does with any one tank leaking.

All geometry is CoScad written by design.py. Sliced for the P1S, A1 and X1C in PLA;
**printed successfully on X1C and sink-tested by the owner on 2 November 2026**. No script here talks to a printer except
`dispatch.py`, which needs the owner's bed-clear and spool confirmation on the command line.

| | Value |
| --- | --- |
| Size | 236 x 124 x 64 mm (2x the bath boat's length; the longest that fits a 256 mm plate with a brim) |
| Skin | 3.0 mm hull, 2.4 mm cockpit walls and bulkheads, 4 mm solid floor; thinnest checked wall 2.27 mm |
| Cockpit | open, 230 x 118 mm at the gunwale, a 136 x 24 mm floor; walls lean 38 degrees |
| Tanks | 10 sealed chambers, 191 cm3 of air: bow and stern (split port/starboard) and three per side |
| Mass | 306 g sliced (309 g modelled solid PLA) |
| Print | about 16 h on any of the three printers, 320 layers at 0.2 mm, no supports, no bridges |

## What the checks say (verify.py, static)

| | Fresh water | Sea water |
| --- | --- | --- |
| Dry: draft / side above water | 25.4 / 38.6 mm | 25.0 / 39.0 mm |
| Dry: water comes over the rim at | 40 degrees heel | 40 degrees |
| Dry: largest righting arm | 19.9 mm at 55 degrees | 20.0 mm at 55 degrees |
| Swamped: gunwale above water | 24.8 mm | 25.7 mm |
| Swamped: largest righting arm, gone near | 10.8 mm, 80 degrees | 11.3 mm, 80 degrees |
| Swamped + worst tank flooded (stern starboard): gunwale above water | 21.4 mm | 22.5 mm |
| Payload on the cockpit floor before the rim is 10 mm from the water | 614 g | 637 g |

Compared with the bath boat (12.7 mm freeboard, sinks when swamped): three times the
freeboard, and swamping now costs freeboard instead of the boat. It is a monohull, so it
is not as stiff as the beach outrigger (48 mm righting arm at 20 degrees). Past about 115
degrees dry (about 80 swamped) it goes on over and floats upside down; it does not
self-right. Static only: waves, surf, slamming and how fast a leak fills a tank are not
modelled.

## Design notes

- **Tanks close by meeting walls, never flat roofs.** Each tank lies between the hull skin
  and the cockpit wall. The cockpit wall leans out 38 degrees from vertical and meets the
  skin 3 mm below the gunwale, so the tank roof is that wall's underside, a 38 degree
  overhang. A flat deck over a tank would bridge open air inside a sealed chamber.
- **The cockpit is a true offset of the gunwale outline**, inset by 3 mm at the top and
  by tan(38 degrees) more per mm going down, so the wall leans the same at the pointed bow
  and the transom. A scaled outline would thin the wall at the bow.
- **The cockpit wall reaches into the floor**, so no tank has a ceiling over the cockpit
  floor. The centreline bulkheads in the bow and stern also carry the cockpit's pointed
  ends.
- **Why 124 mm wide and 38 degrees.** Widening alone made it worse swamped (the cockpit
  grew and the tanks shrank: 85 cm3 of air at 124 mm and 32 degrees). The steeper cockpit
  wall is what makes the tanks big. The first try (112 mm, 32 degrees, 4 mm rim) had
  130 cm3 and a 2 mm deck over the transom tank corners; a 3 mm rim fixed the corners.
- **Exterior overhang** is at most 44 degrees (the bilge flare). Nothing is steeper than
  45 degrees from vertical; the overhang scan in the record found no area beyond that.
- Chamber pressure in the sun is small (a 30 K rise is about 10 kPa); no vent, since a
  vent would let water in.

## Print settings

The beach outrigger's process, made sturdier:

- **Walls.** Eight wall loops instead of six, so the 3 mm skin is all perimeters. 100%
  zig-zag fill in the modelled plastic and six top and bottom layers, so every wall is
  solid.
- **Temperature.** Nozzle 225 C instead of 220 C (Bambu PLA Basic) for stronger, tighter
  layer bonds; bed 55 C on a textured PEI plate.
- **Layers and brim.** 0.2 mm layers, outer wall at 35 mm/s, 5 mm outer brim, no
  supports.
- **Placement.** Centred at (128, 136) on the P1S and X1C, which clears their 18 x 28 mm
  front-left exclusion, and at (128, 128) on the A1.

## Before the beach

1. **Print** (about 16 h, 306 g), then weigh it: about 306 g.
2. **Sink test.** It should sit about 25 mm deep. Press it down until the cockpit fills:
   it should stay up with about 25 mm of side above the water.
3. **Leak test.** Hold the hull under in warm water and watch for bubbles from any
   chamber. A leaking chamber can be found by its bubbles and sealed with CA glue or a
   hot-air pass.
4. **Rock pool first, then the surf**, the way the outrigger went.

## Files and rebuilding

Run in this order. The environment needs trimesh, manifold3d, numpy, scipy, matplotlib,
rtree; manifold3d had no wheel for Python 3.15 on 2026-11-01, so use Python 3.13.

- **Geometry: design.py, render.py, verify.py.** CoScad generation, a native render
  (`coscad stl`; the compiler comes from `--coscad`, `$COSCAD` or `cabal list-bin
  exe:coscad`), then the wall and hydrostatic checks.
  - `beach-monohull.coscad` is the printable part.
  - `-envelope` is the dry displacement body and `-sealed` the swamped one; neither is a
    part.
- **Results: validation.json, preview-and-stability.png, design.json.**
- **Slicing: prepare_slice.py, slice.py, verify_packages.py.** Bambu Studio 02.08.02.61
  CLI jobs `p1s-pla`, `a1-pla` and `x1c-pla`, each `<job>/slice/beach-monohull.gcode.3mf`.
  All three passed: success, no warnings, no support paths, no bridges, paths inside the
  model, filament within 1% of the solid mesh. Results are in packages.json.
- **Printing: dispatch.py and printer-context.py.** Copied from the outrigger. They send
  one deduplicated job only with `--confirm-bed-clear`, `--spool` and an authorization
  note.

## Owner physical outcome - 2 November 2026

The large boat printed very well on X1C, with no problems reported. It floated
in the sink at the expected waterline. The owner observed good stability and
no developing list in relatively calm sink water with some waves. Substantial
water turbulence has not been tested; this does not establish surf behaviour,
measured draft, long-term watertightness or damaged-chamber performance. See
[physical outcome](physical-outcome-20261102.json) and the
[review qualifications](review-20261101/README.md), including the reduced rim
clearance when a flooded chamber is allowed to heel/trim the boat.
