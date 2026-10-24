# Orbit Tower: hand-written guide

For the instruction generator's step-by-step renders, use the
[generated Orbit Tower instructions](builds/orbit-tower/index.html).
This separate guide adds practical assembly, handling and print notes.

Build a small desktop sculpture with six arms that spin independently. The
20 printed pieces fit together by hand with loose peg-and-socket connections;
no tools, glue or extra hardware are needed.

![The assembled Orbit Tower, with its six arms fanned into a spiral.](assets/orbit-tower/assembled.png)

The finished tower is **107 mm tall**, with a **58 mm diameter base**. Leave
at least **61 mm of space across** for the arms to turn.

## Check your pieces

| Piece | Quantity | How to recognise it |
| --- | ---: | --- |
| Base | 1 | Large round foot with a square peg on top |
| Axle | 6 | Small round pedestal with a square socket underneath and a square peg on top |
| Rotor | 6 | Flat arm with a large centre hole and a small hole near one end |
| Satellite | 6 | Mushroom-shaped piece with a small round peg |
| Top cap | 1 | Tapered tip with a square socket underneath |
| **Total** | **20** | |

![All 20 pieces laid out on a single print bed.](assets/orbit-tower/bed.png)

Let the plate cool before removing the parts. Remove any loose strands or
brim material from the pegs, sockets and holes before assembly.

## Assemble from the bottom up

1. **Set down the base.** Place the large round base on a flat table with its
   square peg pointing up.
2. **Add the first axle.** Hold an axle with its square socket facing down.
   Lower it onto the base peg until its round foot rests on the base.
3. **Add an arm.** Lower a rotor's large centre hole over the axle's square
   peg and onto the round section below it. The arm rests on the circular
   shoulder. Check that it turns freely.
4. **Add a satellite.** Turn a mushroom-shaped satellite so its peg points
   down. Insert the peg into the small hole near the end of the arm. The
   thicker opposite end of the arm is its built-in counterweight.
5. **Build the remaining five levels.** Fit another axle onto the exposed
   square peg. Its foot keeps the arm underneath in place. Add that level's
   rotor and satellite, then repeat until you have used all six axles,
   six rotors and six satellites. Check each arm turns before adding the
   next level.
6. **Finish with the cap.** Fit the tapered cap, socket down, onto the top
   square peg. Fan the arms around the tower into a spiral, or turn them
   individually.

Add each arm **before** the axle above it. If you have already stacked the
axles, lift them off and rebuild one level at a time.

## Play, adjust and take apart

Turn the arms gently by hand. Each satellite is removable, and each level
can face a different direction. The joints are intentionally loose gravity
fits: **lift the complete tower by its base**. Lifting it by the top or an arm
can pull the stack apart.

To dismantle it, remove the cap and work downwards, lifting off one axle,
rotor and satellite at a time.

If an arm catches, lift off the levels above it and check for a stray strand
or rough edge in the centre hole. If a peg will not seat, check that its socket
is facing down and clear of print debris. The parts should not need force.
The printed fit and spinning action still need to be confirmed on the first
physical kit.

## Print another kit

All 20 pieces fit on one **256 × 256 mm A1 bed**. The prepared profile uses
PLA, a 0.4 mm nozzle, 0.20 mm layers, three perimeter walls and no supports.
The slicer estimated about **40 g of filament** and **1 hour 37 minutes**
including preparation.

The design allows 0.3 mm clearance on each side of the square pegs, 0.4 mm
around each rotor journal and 0.35 mm around each satellite peg. These gaps
allow easy assembly rather than a tight press fit.

[Get the CoScad source and assembly file](https://github.com/hyperswine/coscad/tree/main/models/orbit-tower).
