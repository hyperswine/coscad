# 2020 water-tank model, 100 mm, 2026-11-02

A small test of the full-size tank idea: a 2020 extrusion cube frame with panels in
the slots. Twelve rails, five panels, sixteen flat L brackets, all printable, with
standard M5 hardware. Outside 100 x 100 x 100 mm, open top, about 0.35 L to just
under the top rails.

The original flat 5 mm panels were reported floppy by the owner. The revised
sliding-key panels (2026-11-07) pass `coscad validate`, `check` (0 overlaps,
0.25 mm panel/rail gaps) and `plan` (33 steps, 6 flips). The owner confirmed all
three [one-sided fit samples](panel-fit-v2/README.md) work on 2026-11-07; full panels
remain untested. The owner also confirmed all three
[bilateral flange samples](panel-fit-v3-double-flange/README.md) fit on the same
date. A [whole wall-panel trial](panel-full-v3/README.md) now uses the middle
0.25 mm clearance, printed upright with mouse ears. The owner reported on
2026-11-09 that it works very well; a [four-panel batch](panel-batch-v3-20261109/README.md)
adds three more matching walls and a bilateral floor with the original corner notches.

## Parts

| Part | Count | Size | File |
| --- | --- | --- | --- |
| Upright | 4 | 2020 x 100 mm | rail100.coscad |
| Rail between uprights | 8 | 2020 x 60 mm | rail60.coscad |
| Wall panel | 4 | 70 x 70 mm, 5.7 mm web, 8.1 mm maximum at keys | panel_wall.coscad |
| Floor panel, corners notched 5.3 mm | 1 | 70 x 70 mm, same web and keys | panel_floor.coscad |
| Flat L bracket | 16 | 40 x 40 x 4 mm, 20 mm legs | bracket.coscad |
| M5 x 8 button head screw | 32 | | |
| M5 drop-in T-nut (2020, slot 6) | 32 | | |

- **Rails.** Real 2020 slot-6 profile: a 6.2 mm opening behind 1.8 mm lips, and a pocket
  tapering in three steps to 6.1 mm deep. The steps leave diagonal webs, as on real
  extrusion, so stock T-nuts and brackets fit. You can swap in aluminium 2020 cut to
  4 × 100 mm and 8 × 60 mm.
- **Panels.** Each fills its 60 x 60 mm opening and sits 5 mm deep in the slots on all
  four sides. A 5.7 mm neck leaves 0.25 mm on each face of the 6.2 mm throat
  (0.5 mm total, previously 1.2 mm). One-sided stepped ribs fit the 11 and 8.4 mm
  pockets behind the lips. Only edges parallel to insertion have keys: the wall's
  two vertical edges and the floor's left/right edges. Other edges remain narrow
  enough to enter a slot mouth. Keys stop short of the corners and have 2 mm entry
  ramps; the opposite panel face stays flat for printing. The uprights run all the
  way to the bench, which is why the floor panel has notched corners. This matches
  the modeled rail profile; check actual aluminium or printed rails with a sample.
- **Brackets.** Only on the outside faces, so the inward slots stay free for the panels.
- **Screws.** M5 x 8: through the 4 mm bracket and 4 mm into the T-nut, clear of the slot
  floor.

## Printing (X1C and P1S, sliced 2026-11-02)

These are historical packages for the original flat 5 mm panels, **not** the
revised keyed panels. Keep their print records intact. Render the revised sources
and generate and verify new projects/slices before any revised-panel print.

Bambu Studio 02.08.02.61 CLI jobs. Each part is a separate object at a fixed position
(`layout.json`), clear of the 18 x 28 mm front-left exclusion that the X1C and P1S share.
All use Bambu PLA Basic (220 C, bed 55 C, textured PEI) on Bambu's stock 0.20mm Standard
process, which is also the P1S's.

| Job | Holds | Settings | Estimate |
| --- | --- | --- | --- |
| `x1c-bed1` | 5 panels, 12 brackets | 4 walls, 100% infill, 5 top and bottom layers, no brim | 3.8 h, 220 g, 25 layers |
| `x1c-bed2` | 12 rails standing on end, 4 brackets | 3 walls, 25% gyroid, 4 top and bottom layers, 5 mm brim | 15.4 h, 217 g, 500 layers |
| `p1s-bed2` | bed 2 for the P1S, same layout and settings | as `x1c-bed2` | 15.3 h, 216 g, 500 layers |

- **Bed 2's time.** Bed 2 is slow because the T-slot profile has about 60 corners per wall
  loop and the printer decelerates at every one. At full speed it would be about 3.6 h.
  Cut aluminium 2020 (4 × 100 mm, 8 × 60 mm) skips that bed, apart from 4 brackets.
- **Checks.** `verify_packages.py` checks:
  - slicer success with no warnings, and the intended walls and infill;
  - no supports;
  - every extrusion on the plate and outside the exclusion corner (accounting for the
    X1C's 2 mm G-code Y offset);
  - the weight against the parts;
  - the printer model id, filled in without touching the G-code.

  All three passed, as did the thermal preflight that `dispatch.py` repeats.
- **Rebuilding.** Run `prepare_slice.py`, `slice.py`, then `verify_packages.py`. The parts
  in `stl/` render with `coscad stl NAME.coscad -o stl/NAME.stl`.
- **Printing.** `dispatch.py x1c-bed1 --spool external --confirm-bed-clear
  --authorization-note '...'` sends exactly one verified job. It refuses unless the
  printer is idle with no faults and the named PLA spool is loaded.
  `--accept-failed-state` also accepts a printer left FAILED by a stopped job, still
  with no error code or warnings. Use it only when the owner has cleared the plate.

## Print runs

Records are in `<job>/print-run.json`: the owner's confirmation, the spool, the verified
upload, the printer's acknowledgement and status up to the first layer.

- **x1c-bed1.** Sent 2026-11-02 15:09 after the owner removed the finished beach monohull.
  The X1C acknowledged the job and reached layer 1 of 25 at 15:18.
  - **Result (owner report, 2026-11-02).** It printed well, with no problems.
  - **Calibration lines.** The X1C prints a flow calibration pattern along the front of
    the plate before each job (`flow_cali` in the start command). A layout that reaches
    the front of the plate, as this one does (brackets from y = 7 mm), prints on top of
    those lines. For such layouts, either start without flow calibration or keep parts
    back from the front edge.
  - **Dark filament.** The X1C has had trouble with very dark filament such as black,
    which is loaded now (it was yellow for this job). Its lidar-based flow calibration and
    first-layer inspection rely on seeing the extruded line, which black makes hard. With
    dark filament, start without flow calibration and check the first layer by eye.
- **p1s-bed2.** Sent 2026-11-02 15:33 after the owner removed the other session's failed
  Tesseract parts print (FAILED at layer 1004/1369, no fault); external yellow PLA.
  - **Start.** The P1S acknowledged the job but kept reporting the old FAILED state for
    about two minutes, so the dispatch monitor stopped early. That is fixed now: a FAILED
    state counts only after this job has started running. A read-only follow-up watched
    the job start; it reached layer 1 of 500 at 15:43, with about 15 h 13 min left.
  - **Record.** The follow-up's readings are in `followup_events` in the record.

`coscad next tank.assemble` also packs two plates (`tank_bed1.stl`, `tank_bed2.stl`).
Its packer starts at the plate corner, so its beds are not used for the X1C.

## Brims: use mouse ears (owner preference, 2026-11-03)

Use mouse-ear brim pads (small discs at the corners that tend to lift) instead of full
brims. They come off more easily and only cover the places that need them. In Bambu
Studio this is `brim_type = brim_ears`. The slicer places ears at sharp corners, or uses
ear points saved in the 3MF (`Metadata/brim_ear_points.txt`, the brim-ears tool in the
GUI). Round parts with no sharp corners, such as the top's body or the propeller hubs,
may get no ears at all, so check the sliced preview and fall back to manual brim pads or a narrow brim if needed.

## Build

Follow `tank_plan.md` and the step images `tank_stepN.png`. They're generated and not
committed; `coscad plan tank.assemble --png` writes them.
The [interactive build instructions](https://manual.cswine.cloud/builds/tank/index.html)
are regenerated with native renders whenever the manual site is rebuilt.

1. **Front frame.** Build it lying flat (two uprights, bottom rail, brackets), drop the
   front wall into the bottom rail's slot, then close it with the top rail.
2. **Floor.** Stand the frame up and slide the floor into the bottom rail's inward slot.
3. **Side bottom rails.** Slide them from the open rear onto the floor's keyed edges,
   then fit the rear uprights. Do not push a wide key through the narrow rail mouth.
4. **Side walls and back.** Slide the side-wall keys down the upright slots, drop
   their plain bottom tongues into the bottom rails, then add the top rails. Add
   the rear bottom rail, slide the back wall down and close it with its top rail.

Each top rail must go on after its wall, because the wall drops in from above onto its
bottom rail. The file says so with `after=wall_*`. The side bottom rails wait for the
floor and the rear uprights wait for those rails; explicit axial joints describe how
the side rails slide onto the floor. The side walls are rotated so their keyed edges
engage the vertical uprights. Without the top-rail hints the planner put a top rail
on first and got stuck: its look-ahead for parts that would get blocked misses a part
that also needs another part placed first. That's a planner gap worth fixing.

Preload all T-nuts as the plan's preload sheet lists, before each rail goes in. Tighten
screws to finger tight plus a quarter turn; they're going into printed parts.

## Making it hold water

The tighter mechanical fit is **not a water seal**. After dry fitting and assembly,
apply aquarium silicone continuously along **every inside panel-to-frame edge**,
including the floor perimeter, floor corner notches and all vertical corners.
The locating keys stay inside the slots and leave these interior seams accessible.
Do not fill sliding pockets with sealant before fitting the parts. Follow the chosen
sealant's preparation and cure instructions, then leak-test the small tank. No product
brand or successful sealed-tank test has been recorded.

A freezer-bag liner remains an alternative for an unsealed geometry test; the intended
configuration here is sealed rigid panels.

## Files

| File | What it is |
| --- | --- |
| `tank.assemble` | Placements, screws, panel joints and order hints |
| `*.coscad` | The five parts; `stl/` has them rendered |
| `tank_plan.md`, `tank_plan.json`, `tank_stepN.png` | The build plan (generated by `coscad plan tank.assemble --png`) |
| `tank_bed1.stl`, `tank_bed2.stl`, `tank_manifest.json` | Print beds |
