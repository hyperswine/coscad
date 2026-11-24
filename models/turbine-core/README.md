# Turbine core (printed demonstrator)

Blades and shaft only, for a small gas-turbine core. Printed PLA parts, glued on an 8 mm shaft. Not a
working engine: PLA cannot take combustion heat, and the turbine blade pitch (72 to 48 deg) is much
steeper than a real turbine so that it prints without heavy overhangs.

## v1 (this folder): shaft + centrifugal compressor + one turbine wheel

- Parts: `shaft.coscad`, `compressor.coscad`, `turbine.coscad`; assembly view `turbine-core.coscad`.
- Sliced for the P1S (PLA, 0.20 mm, 4 walls, 15% gyroid, supports on): 1 h 57 m, 36.4 g.
- Printed on the P1S 2026-11-22 from `p1s/dispatch.py`. **Owner: it printed well.**
- Known limits: wheels are glued at marked z positions (no shoulders on the shaft).

## v2 (`v2/`): three turbine stages + front compressor + SG90 coupler

- Front (z 0 end): the centrifugal compressor, inlet toward z 0. Behind it, three axial turbine wheels
  at z 96, 112 and 128 (the same wheel printed three times; 16 mm pitch, 12.2 mm tall).
- `v2/shaft.coscad`: 162 mm long, bearing seats at z 38-50 and 76-88. Bores are r4.05 for glue fit.
  Wheel z positions are in the file header; compressor occupies z 8-34.
- **SG90 interface** at the top end: a 45 deg flare to a 13 mm hub with a blind 4.95 mm bore, 9 mm deep and
  0.4 mm chamfered. This is the bore proven on `models/sg90-rack/press-fit` (4.95 press-fits the SG90
  spline; keep it). Prints standing, bore opening up, no support needed there.
- `v2/turbine-core-v2.coscad`: assembly view (not printed).
- Slice: `v2/slice/turbine-v2.gcode.3mf`, 5 objects on one plate, 3 h 09 m, 59 g PLA, 953 layers.
  **Not printed yet** (was only built and sliced).
- Assumption: "3 blades in the core and one at the front" read as three turbine wheels (blade rows) behind
  one compressor wheel. Rotor-only: there are no stator rows between them.
- Untested: the SG90 bore on this shaft (8 mm shaft, so only 1.5 mm of wall around the bore at the plain
  shaft; the hub is r6.5 for 2 mm wall), and a 162 mm shaft printing standing (v1 was 130 mm).

## v3 (`v3/`): plain shaft + screw-on blockers (supersedes v2, which was never printed)

Owner notes: the 4.95 mm SG90 bore fit well on the earlier rack-and-pinion print, but the shaft's fixed
flanges (bearing seats, coupler hub) were in the way of sliding the wheels on. So the shaft is now one plain
8 mm cylinder, with one fixed collar in the middle and a screw thread at each end; the end blockers screw
on after the wheels are installed.

- `v3/shaft.coscad`: 94.5 mm, fixed collar r6 at z 40-46, 3 mm-pitch threads at z 0-16 and z 84-94.5. The
  thread is the tesseract-v3 coupon thread (the P1S screw/socket coupon worked) moved 1 mm inward: root r3.05,
  crest r4.0, clearances radial 0.35 and flank 0.18 on the nut side. Wheel bores are r4.15, so wheels pass
  over the crests.
- Stack, front to back: nose nut (`front-nut`, 8 flats, z 5.8-13.8), compressor (z 14-40, inlet front),
  collar, turbine 1 (z 46), spacer (9 mm), turbine 2 (z 62), spacer, turbine 3 (z 78), then the
  `coupler-nut` (z 85-107). Wheels slide on from the ends: compressor from the front, the rest from the rear.
- `coupler-nut`: 10 mm thread pocket, then the 4.95 mm chamfered blind SG90 bore, 9 mm deep (from the
  rack-pinion print). Prints thread side down, bore up.
- Slice `v3/slice/turbine-v3.gcode.3mf`: 9 objects (shaft, 2 nuts, 2 spacers, compressor, 3 turbines),
  2 h 58 m, 59.6 g, 572 layers, supports on. Not printed. Assembly view `v3/turbine-core-v3.coscad`.
- Checked: front nut vs shaft thread has zero interference in the CoScad solids at a matched thread phase
  (rear nut: interference only because the test seat sat 1.2 mm low; at the real seat z 85 the pocket top
  is z 95.3 vs the shaft tip 94.5). Untested physically: how the nut threads turn on, whether the wheels
  glue/clamp tightly enough (nuts clamp the stack against the collar; no glue needed in principle).

**v3 outcome (owner, 2026-11-23): printed fine on the P1S, everything fit with no wobble.** The v3
numbers below are therefore proven and reusable.

### Proven printed fits (P1S, 0.4 mm nozzle, 0.20 mm layers, Bambu PLA Basic, 4 walls)

| Fit | Value |
|---|---|
| Screw-on thread (shaft or bolt side) | pitch 3 mm, root r3.05, crest r4.0 (depth 0.95), tooth 2.2 mm wide at the root and 0.2 at the crest |
| Matching nut or socket cutter | root r3.40 (+0.35 radial), crest r4.35, teeth widened 0.18 on each flank (+0.36 total), 0.4 mm lead-in mouth |
| Same thread at 10 mm major (tesseract coupon) | root r4.05, crest r5.0, same clearances |
| Wheels sliding on an 8 mm shaft | bore r4.15 (0.15 radial clearance) |
| SG90 spline press fit | 4.95 mm blind bore, 0.4 mm entry chamfer, 9 mm deep |
| Nut/pocket depth | 8 mm engagement (nose nut), 10 mm pocket (coupler nut) |

How to reuse: `gen3.py`-style generation in `v3/*.coscad` (shaft thread `th_front`, nut cutter `f_thread`)
can be copied; only the radii change if the shaft diameter changes, keep the 0.95 mm depth and the clearances.
Orientation that worked: shaft standing with the front thread on the bed, nuts printed entrance-down.

Dispatch: `p1s/dispatch.py` reads `BAMBU_ACCESS_CODE` from the environment (`~/.config/bambu/p1s.env`);
point its `PKG` at the v2 3mf to print v2, and remove `p1s/print-run.json` first.
