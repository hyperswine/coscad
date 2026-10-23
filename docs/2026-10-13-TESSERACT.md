# Tesseract with captured joints

The current tesseract replaces the 19 mm bolted corner blocks with **12 mm
split hubs** and chamfered **7 mm bars**. The outer envelope remains
100 × 100 × 100 mm; the centre remains a 30 mm cube. The new hubs are 37%
smaller across and have about one quarter of the old block's bounding volume.
The original 24-screw version is retained in
`examples/assemble/tesseract/bolted/` for comparison and migration tests.

## How the connection works

Each bar has an integral 4.2 mm neck and 5.4 mm collar. The corner base has
open channels so the bar can enter sideways, perpendicular to its length.
An outer cap closes those channels. Once both halves are together, the
collar cannot pass through the narrower neck opening: the frame's retention
comes from the captured shape rather than friction on the bar alone.

The cap has an integral split pin, initially 3.36 mm diameter, in a 3.30 mm
hole. Its slight interference holds the cap shut. The nominal **0.03 mm
radial interference** is a starting point for a printed fit test, not a
printer-independent guarantee. There are no screws, nuts, loose dowels or
adhesives. The `.assemble` keyword `fastener peg` records an integral joint
for the instruction planner; it does not add a separate manufactured part.

## Files and printing

- `examples/assemble/tesseract/tesseract.assemble`: 38 printable pieces,
  including a temporary assembly support. Use this for `next`, `plan` and
  `site`. All parts fit one 256 × 256 mm bed with a 6 mm margin.
- `examples/assemble/tesseract/finished.assemble`: the **37-piece finished
  object**, with the support removed. Use this for viewing and interference
  checks. The unsupported intermediate skeleton is not a bench-stable build
  sequence, so use the supported assembly above to generate instructions.
- `examples/assemble/tesseract/fit-test.assemble`: one base, one cap and one
  short bar end; the original three-piece coupon.
- `examples/assemble/tesseract/adhesion-retry/fit-test.assemble`: the current
  four-piece trial, adding one full-length bar and an explicit A1 slicing
  profile. Use this before a full build; see its README for the saved print.

The support is 35 mm tall, with four posts under the inner cube. The corner
halves print with their outer diagonal face down; the cap's outer tip is
trimmed at x+y+z=9 to give it about 69.7 mm² of flat contact (the original
x+y+z=3 trim had only 7.4 mm²). The pin still prints vertically. `next` uses real mesh bounds when putting
these diagonal faces on the bed. Full-length bars print lying down, and the diagonal
struts retain a flat underside. The short fit-test bar stands on its broad
end so its integral collar prints upward without an isolated first layer.
The full-length bars require local slicer supports beneath their end collars:
the collars start 0.8 mm above the bed and their necks start at 1.4 mm.
The upright short coupon does not test this. The retry explicitly enables
snug supports and an 8 mm brim, and verifies the resulting toolpaths.

The supported build contains eight bases, eight caps, twelve bars, eight
struts, one inner cube and one removable support. Use the same filament and
slicing settings for the fit test and the full build. If the cap is too tight,
reduce `fit` in `cap.coscad` and reprint the fit test. For example, `fit = 0`
gives equal nominal diameters, and `fit = -0.05` gives 0.10 mm diametral
clearance. These settings may be loose; the fit test must establish retention.

## Assembly sequence

1. Place the inner cube on the temporary support.
2. Insert the eight diagonal struts into the cube, then slide each corner
   base onto its exposed strut end from outside the cube. The shallow socket
   seats the base; it is not yet a locked frame.
3. Seat the twelve bars in the open corner bases. Each bar enters from
   outside its cube edge, perpendicular to its own axis, into both bases
   together. There is no final bar that must be pushed axially between two
   closed sockets.
4. Check all collars are seated. Press the eight caps diagonally inward,
   **after every bar is installed**. Their split pins retain the caps, and
   the closed hubs capture the bar collars.
5. Lift the frame and withdraw the temporary support straight down through
   the bottom opening. The support is not part of the finished object.

The generator enforces the support/centre/strut/base/bar/cap prerequisites
using the existing generic `after=` hints. Its generated plan has 36 steps
and no flips. **Support removal is the final manual operation above**: the
current planner does not generate removal operations. Its legacy face labels
on peg joints are locations, not the diagonal insertion directions described
here. Insertion motion is checked separately by the model regression.

## Verification and limits

Run the repeatable geometry regression with Python 3.12 or newer:

```sh
python3 -m venv /tmp/tesseract-check
/tmp/tesseract-check/bin/pip install -r examples/assemble/tesseract/requirements.txt
/tmp/tesseract-check/bin/python examples/assemble/tesseract/verify.py
coscad next examples/assemble/tesseract/fit-test.assemble
coscad next examples/assemble/tesseract/tesseract.assemble
coscad plan --png examples/assemble/tesseract/tesseract.assemble
```

The regression separates the deliberate pin press fit from unwanted
interference: it uses a clearance pin for all-pair assembly checks and
sampled entry paths, then checks that the nominal pin alone has a small
positive interference. It checks connected watertight parts, bar capture,
cap/base/bar approach paths and downward support removal. Path positions are
sampled every 0.1 mm; this is not a continuous swept-volume proof or a
flexible-material simulation.

The [verification report](2026-10-13-TESSERACT-INTERLOCK.md) records
748 compiler checks, 4,629 sampled motion positions and the one-bed meshes.

The original A1 coupon lost bed adhesion on subsequent layers; the center
base initially adhered, while the side cap and short bar failed. The revised
four-piece retry is prepared offline pending restoration of the home network.
No successful revised print, cap pull-off test or long-term creep test has been run.
The split pin and its fit are the remaining practical uncertainty. This is a
display model; its captured bars should not be treated as a rated structural
connection. Avoid repeated forced disassembly until the fit test establishes
how the selected filament behaves.
