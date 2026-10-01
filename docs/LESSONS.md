# Lessons from the first prints

What went wrong between the first `.assemble` file and a finished part on
the bed, and what fixed it. Dates are late September 2026; the printers
were a Bambu Lab A1 with an AMS Lite and a P1S with a single spool. The
fixes live in the code, so this page is the reasoning behind them.

## Slicing with Bambu Studio's CLI

**Presets did not apply.** `BambuStudio --load-settings ... --load-filaments`
accepts Bambu's own preset files but does not resolve the `inherits` chain
inside them, so a filament preset loaded raw sliced with density 0, a flow
limit of 2 mm³/s and a 200 °C nozzle. A bracket estimated at 17 minutes
took 11 with the real profile. `scripts/bambu-slice.py` flattens the chain
before loading.

**The head moved but nothing came out.** The printer's start, end,
filament-change, layer-change and timelapse gcode are not in the machine
preset either: Bambu ships them as `<printer> template <key>.json` files
next to it and the GUI merges them in. Without them the sliced file ran a
generic placeholder start sequence that never loads filament. The first
ball print was cancelled at layer 9 for this. The script now merges the
template files, with a fallback to the 0.4-nozzle templates for other
nozzle sizes.

**The printer asked for filament to be pushed in.** The start command was
not told which spool to use, so the A1 targeted the external holder, and
loading from there with an AMS Lite present is a manual step: it unloads,
then waits for a hand. Both scripts take `--spool external|ams0..ams3`
and the run script defaults to whatever is loaded, then the single AMS
slot holding the right material.

**Calibration was skipped.** The start command's `flow_cali` and
`layer_inspect` flags default to off in a hand-written command and on in
Bambu Studio's print dialog. They are on now.

## Talking to the printer

- Status, gcode, file upload and the camera all work over the LAN with
  the access code once developer mode is on, on both the A1 (LAN-only
  mode) and the P1S. The P1S reports no AMS, which crashed the status
  summary in the bambu-lan scripts until it tolerated an empty list.
- The first camera frame after connecting can be stale: the P1S once
  returned the previous print still on the plate after it had been
  cleared. The verdict never depends on the photo, only on the printer's
  final state and the file name.
- Photos taken at night are black. A `chamber_light` command exists;
  the scripts do not send it.
- With two LAN printers on the network, discovery must be told which one
  (`--device A1|P1S`); the machine preset follows the discovered model.
- Piping the run's log through `sed` buffered it until the run ended.
  Watch the printer's state directly, or use unbuffered output.

## The manual host (mac mini)

OpenSCAD launched by a launchd job blocked for half an hour per render
inside `open()`: BOSL2 lived under `~/Documents`, macOS asks each new
process for Documents access, and nobody was at the screen to answer. The
library now lives at `~/srv/BOSL2` with `COSCAD_BOSL2` pointing at it. A
stack sample (`sample <pid>`) is what showed the blocked `open()`.

## The planner meeting real assemblies

Each new assembly broke an assumption in `coscad plan`; the fixes are in
[PLAN.md](PLAN.md), the reasons are here.

- **Bounding boxes lie about round and diagonal parts.** A hemisphere
  shell's box brushed the neighbouring shell's box and "blocked" its
  screw. A diagonal strut's axis-aligned box is a block that swallowed
  every corner of the frame. Driver clearance now requires the driver to
  really enter the other part's *oriented* box, and touching does not
  count.
- **Not every nut is a T-nut.** The ball's core holds M3 hex nuts in
  pockets, the tesseract's bars are threaded by the screw itself, its
  struts are friction pegs. `nut=hex pocket=face`, `nut=none` and the
  `peg` spec came from those three. A pocket in the *clamped* part
  (`pocket=clamped.face`) is loaded with the part in hand, which changes
  the ordering: the nuts go in before the part goes down, not after the
  host is placed.
- **Holding is symmetric for screws, one-way for pegs.** A part can be
  placed held against anything it will be screwed to, on either side of
  the joint (a corner block onto placed bars). A friction peg only holds
  the part pushed onto it, or a corner block would be "held" in mid-air by
  a strut.
- **The bench is solid.** A driver pointing down into the bench used to
  cost 3 points; it is impossible, so it is now a hard block. A bar stood
  on end used to be free unless it was a rail; any part three times
  longer than wide now pays the same penalty and gets the same warning.
- **A part on the bench is not loose.** Counting a corner block as loose
  while it waited for its screws made the planner start with bars instead,
  which set the bench too high and forced flips for every corner.
- **Say why.** When the beam search ran out of states it said "no states
  left". It now lists what the last states could not do, which is how the
  bounding-box blocks above were found.

## Designing for the printer

- **Round pegs printed lying down were the tesseract's only bad parts.**
  A cylinder touches the bed along a line. The second cut uses a D
  profile, one face cut flat 1 mm off the round, printed on that face,
  in an unchanged round hole.
- **A hole down a body diagonal leaves a sliver at the corner.** The
  inner cube's corner holes left a thin tip of material with nothing under
  it, which the slicer flagged as floating. A chamfer across the corner
  replaced it with a 55° overhang, also flagged. A vertical flute down
  each corner edge fixed both, at the cost of printing the cube on one
  declared face.
- **Corner blocks cannot be much smaller than two socket depths plus
  the bar.** With 6 mm bars and a 5 mm peg needing room at the inner
  corner, that is about 18 to 19 mm. Smaller means thinner bars or thinner
  strut ends, not cleverer sockets.
- **Three screws in one block cross unless their axes are cyclic.** Screws
  along Z, X and Y for the X, Y and Z sockets never meet, keep the block
  symmetric about its diagonal, and put every head on an outer face.
  Orient hex pockets with flats toward the block's faces, or a vertex
  breaks through.
- **Pegs with friction at both ends are inserted with a slide.** Push
  the strut fully into the deeper hole, place the part, slide the strut
  back into the shallower hole. The plan lists the pushes but cannot say
  that.

## Checking

`coscad check` splits each part's render into bodies by connectivity, so
two instances of a part that touch face to face (the ball's shells at
the equator) count as one. The pairs it does test are still exact; only
the count is off.
