# PLA bow launcher for a 10 g glider (2026-11-15)

Goal: launch a 10 g glider at 8 m/s with a printed PLA spring and an 8 g
carriage. The spring is a pair of tapered PLA limbs, built like a small
crossbow:
- a riser carries the two limbs
- a cord runs between the limb tips
- the carriage slides on a printed rail
- a drop-in pin holds the carriage at full draw

Result, modelled but not yet printed or tested: **8.6 m/s at 24.3 MPa peak
limb stress** with nominal PLA (E 3.5 GPa).
- That figure already takes off 10% of the energy for rail friction, string
  stretch and PLA hysteresis.
- Without that deduction the figure is 9.0 m/s.
- If the PLA is softer than assumed (E 3.0 GPa), the result is 7.9 m/s.

## Files

- `sim.py`: quasi-static bow model. Each limb is 24 rigid segments joined by
  rotational springs and clamped at the riser, so large deflection is handled
  (the limb bends about 0.9 rad). The string is taut. Launch speed counts the
  limb mass along its real motion, using the quasi-static mode.
- `search.py`: grid search over limb length, thickness, root width,
  pre-curvature and brace. Results are in `search-25.json` and `search-30.json`.
- `build.py`: writes every `.coscad` part and `launcher.assemble` from the
  chosen numbers.
- `glider.py`: writes the four glider parts, renders them, computes mass and
  CG from the meshes, and places the wing for a 10% static margin. It writes
  `glider.json` and `glider.assemble`. Run it before `build.py`, because the
  launcher assemblies read `glider.json`.
- `launcher.assemble`: the carriage at full draw with the latch pin in and the
  glider loaded.
- `launcher-brace.assemble`: the carriage at rest at the bumper, the moment of
  release.
- `launcher-release.assemble`: the glider 93 mm further on, with the stab right
  over the mast cap.
- `print/`: the three print packages and their records (see Print below).

The parts need the CoScad 1.6 build from 2026-11-13 or later, because they use
`poly`. The `bin/coscad` binary from 2026-10-30 rejects it.

## Design

| Item | Value |
|---|---|
| Limb | 210 mm active length, 3.0 mm thick, width tapering from 44 mm at the clamp to 8 mm at the tip; 13 mm bolted tab; 22.5 g solid each |
| String | 455 mm between tip hole centres (braided line, about 0.3 g) |
| Brace (nock at rest) | 39.7 mm behind the limb line |
| Full draw | 201 mm, set by the rail's rear stop (1 mm overdraw allowed, 25.1 MPa) |
| Draw force at full draw | 8.4 N |
| Stored energy | 0.83 J, of which 0.13 J stays in the braced limbs |
| Peak glider acceleration | about 45 g, so the glider's hook sees about 4.4 N |
| Carriage | 6.9 g solid printed, plus the M3×16 screw pin (about 0.6 g): about 7.5 g (budget 8 g) |

Why this shape:
- **Long and thin limbs win.** Limb mass costs speed, so the most efficient
  limb is the longest one that fits on the bed (229 mm print).
- **The width taper keeps stress nearly even.** At full draw the stress is
  24–25 MPa over the root half of the limb, falling to 13 MPa near the tip.
- **Reflex did not help.** Limbs printed with a forward curve (κ0 up to
  0.002/mm) never beat straight limbs. A reflexed limb at rest leaves the
  string nearly along the limb, so the early draw has little leverage.
- **Bracing must stay low.** Straight 180 mm limbs braced 63 mm deep held
  half their capacity as preload and managed only 6.7 m/s.

Speed for other glider masses: 5 g gives 10.0 m/s, 15 g gives 7.5 m/s.

## Sensitivity at the fixed draw stop

| E (GPa) | Limb t (mm) | Peak MPa | Stored J | v (m/s, 90%) | Draw force N |
|---|---|---|---|---|---|
| 3.5 | 3.0 | 24.3 | 0.83 | 8.57 | 8.4 |
| 3.0 | 3.0 | 20.8 | 0.71 | 7.93 | 7.2 |
| 4.0 | 3.0 | 27.8 | 0.95 | 9.16 | 9.6 |
| 3.5 | 2.9 | 23.5 | 0.75 | 8.14 | 7.6 |
| 3.5 | 3.1 | 25.1 | 0.91 | 9.00 | 9.3 |
| 2.6 | 3.0 | 18.1 | 0.62 | 7.38 | 6.2 |

The draw is fixed by position, not force. Stress therefore follows the real
stiffness: a stiffer print stores more and is stressed more. Check by
measuring the draw force at the stop with a luggage scale; 8.4 N is nominal.

## Glider (10 g)

A printed PLA chuck glider made of four parts:
- **Fuselage:** a profile fuselage. The boom is 1.2 mm thick, the nose pod
  1.6 mm, and the fin 0.4 mm.
- **Wing:** a flat 0.4 mm plate, 230 × 36 mm, with a 0.6 mm spar ridge at 30%
  chord.
- **Saddle:** holds the wing at 2° incidence and 6° dihedral each side. The
  wing bends into the saddle's V and is glued there.
- **Stab:** a flat 0.4 mm plate, 70 × 22 mm, through a slot in the tail boom.

| Item | Value |
|---|---|
| Printed parts (solid PLA) | fuselage 3.02 g, wing 4.15 g, stab 0.74 g, saddle 0.77 g |
| Trim weight | 1.0 g of M3 screws and nuts through the two nose holes |
| Total (with 0.3 g glue) | 9.98 g |
| Wing area, loading | 80.3 cm², 12.4 g/dm² |
| Glide speed at CL 0.6 | about 5.8 m/s |
| CG | 35% chord, 21.4 mm behind the hook slot; wing leading edge at x = −8.7 |
| Tail volume, static margin | 0.42, 0.10 (rough neutral-point estimate) |

How it rides the carriage:
- **Launch hook:** the rear of the nose pod has a 3.3 mm slot, open to the
  rear, which slides over the carriage's 3 mm forward pin. The pin tip pushes
  on the slot's closed end. When the carriage stops at the bumper, the glider
  slides forward off the pin.
- **Roll lock:** the boom's bottom edge sits in a 1.5 mm groove in a cap on
  the mast. Without it the glider could roll about the pin, which points
  along the flight direction.
- **Clearances:** the cap is only 1.2 mm tall, so the stab passes 2.9 mm over
  it after release. Gravity takes about 1.3 mm of that in the 16 ms the tail
  needs to clear the mast.

The first layout was too tail-heavy: a 200 mm fuselage with the hook at the
nose put the wing at x = −58, a 0.06 margin. The fixes:
- Put the CG near the hook rather than the hook at the nose.
- Shorten the fuselage to 180 mm.
- Thin the boom and fin.
- Add 1 g of nose trim.

**Trim before launching.** Hand-glide it over grass first:
- **CG:** add or remove M3 nuts until it balances at the CG mark (21 mm behind
  the slot end).
- **Pitch:** bend the stab trailing edge for a flat glide.

The 6° dihedral and the fin carry the roll and yaw stability; none of it is
measured.

## Print

Three single-plate packages in `print/`, one per printer. All are Bambu PLA
Basic, 0.2 mm layers, Textured PEI, no supports, and modelled 0.2 mm
mouse-ear pads (no slicer brim). All are 100% infill, because the launch
model and the glider balance assume solid parts. Each folder holds:
- the flattened presets
- the project file
- the sliced `.gcode.3mf`
- `package-validation.json` (slicer success, settings, plate bounds, weight
  against the solid parts)
- `print-run.json` once dispatched

The print tooling (prepare, slice, verify, thermal preflight and dispatch) is
the pre-Sol bolted-glider pipeline from `926e93b^`. It was adapted in a scratch
directory and is not committed, because main is Sol-only.

- **Limbs:** print flat, 100% infill, `infill_direction` 0 so the lines run
  along the limb, 15 layers. Bending stress then runs along the extrusions.
  Holding the bow drawn creeps PLA, so drop the pin and launch within seconds
  of drawing.
- **Riser and rail:** print upright with the bottom face down.
- **Carriage:** print with the bottom face down. The top plate bridges 12.6 mm.
- **Latch pin:** print with the handle down.
- **Bumper:** TPU 95A, or a stack of foam tape.
- **Hardware:**
  - M4×35 ×4 through the limb tabs and M4×60 ×2 through the riser and rail,
    with nuts.
  - One M3×16 screw threaded into the mast as the glider pin, leaving 8 mm
    protruding. A printed horizontal pin was refused by the slicer as a
    floating cantilever.
  - M3 screws and nuts as glider trim weight.

| Package | Printer | Parts | Time | Filament | Layers |
|---|---|---|---|---|---|
| `x1c-limbs` | X1C | 2 limbs | 0.8 h | 45 g | 15 |
| `p1s-launcher` | P1S | rail, riser, carriage, latch pin | 2.4 h | 110 g | 317 |
| `a1-glider` | A1 | wing, fuselage, stab, saddle | 0.4 h | 9 g | 34 |

All three were dispatched on 2026-11-15; `print/*/print-run.json` records each
one.

## Assembly

1. Slide the carriage onto the rail from the front.
2. Fit the rail into the riser socket and bolt it.
3. Glue the bumper to the riser's rear face above the rail.
4. Slide the limb tabs into the riser's side slots and bolt them.
5. Tie the string through the tip holes, 455 mm between hole centres, and lay
   it in the carriage's string slot.
6. Build the glider:
   - Glue the saddle onto the fuselage top edge with its slot over the edge.
     Its front rib goes at the wing leading edge, x = −8.7 (8.7 mm behind the
     slot end).
   - Bend the wing into the saddle's V and glue it.
   - Slide the stab through the boom slot, centre it and glue it.
   - Fit 1 g of M3 trim through the nose holes.
7. Load the glider: slide its hook slot back over the carriage pin so that
   the boom drops into the cap groove. The fuselage rides 11 mm above the
   riser, with the wings above the limbs.
8. To launch:
   - Draw the carriage back to the rear stop and drop the latch pin through
     the carriage and rail.
   - Load the glider.
   - Aim and lift the pin.

## Checks done

- **Renders:** every part renders with the native backend. Volumes match hand
  calculation; for example, the limb is 18,141 mm³ against a 6,080 mm² plan
  × 3 mm.
- **Interference:** `coscad check` finds 0 overlaps and 0 clearance warnings
  in all three positions: full draw, brace, and the glider 93 mm past release.
  The glider is loaded in each.
  - At full draw: carriage to rail 0.3 mm, carriage to latch pin 0.2 mm.
  - At brace: carriage to bumper 5.6 mm.
  - The only contact is the boom resting in the cap groove. The glider's own
    assembly is clean too.
- **String path:** worked out from the geometry. At full draw the string leaves
  the slot at 0.53 mm forward per mm outward and clears the latch handle by
  about 2 mm.
- **Beds:** the print packages (`print/*/package-validation.json`) check plate bounds, the X1C/P1S front-left exclusion corner, settings and weight on the sliced G-code itself.

## Not checked

- **Dynamics:** limb vibration, string shock and PLA hysteresis are covered
  only by the flat 10% deduction.
- **Dry fire:** a launch with no glider sends all 0.83 J into the limbs, string
  and carriage. Don't dry-fire.
- **Clamp edge:** the 3 mm rounds on the slot exits are not analysed. The limb
  pivots on them, so they are where a crack would start.
- **Fatigue and creep:** PLA at 24 MPa over repeated shots is unknown. The
  first test should be about 20 shots, measuring the draw force before and
  after.
- **The glider:** its stability, trim and flight are estimates (rough
  neutral point, CL 0.6). The 0.4 mm wing's stiffness at 8.6 m/s is untested,
  and so is the 45 g launch load on the slot jaw.
