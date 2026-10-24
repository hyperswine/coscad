# Slimmer, fastener-free tesseract

Implemented 13 October 2026.

## Design

- Corner envelope reduced from 19 mm to 12 mm; edge bars changed from 6 mm
  to chamfered 7 mm sections. Finished outer envelope remains 100 mm cubed.
- Each corner is an inner base plus an outer cap. Integral bar-end collars
  enter open channels and are captured when the cap closes.
- Cap retention is an integral split pin with 0.03 mm nominal radial
  interference. No screws, nuts, loose pegs or glue are required.
- A removable 35 mm support holds the inner cube during assembly. It pulls
  down through the finished frame's bottom opening.
- `tesseract.assemble` prints 38 pieces including that support;
  `finished.assemble` contains the final 37. `fit-test.assemble` is a
  three-piece trial of one base, cap and short bar end.
- The old bolted design is retained under `bolted/`, with its independent
  migration reference and instruction regression preserved.

## Evidence

- Compiler suite: **748 passed, 0 failed** with the rendering tier disabled.
  The new tests check the finished count/envelope, the fit-test count, zero
  separate hardware, and prerequisite order from support through cap closure.
- Real mesh regression: **38 watertight, connected pieces**. Independent
  all-pair Manifold intersections with the clearance pin have maximum residual
  volume about **0.000011 mm³**, below the **0.01 mm³** overlap threshold.
- **4,629 motion positions** at 0.1 mm spacing: all eight corner bases, all
  twelve bars, all eight caps and downward support removal. Caps are checked
  with every other part already present; bars with every other bar present.
- Moving each of three representative bars axially outward 1 mm causes
  about **0.628 mm³** interference with its closed hub, confirming collar
  capture instead of relying on friction along the bar.
- Nominal pin/base interference: **0.469 mm³**, confined to the split pin.
  Cap/bar/strut interference remains below the overlap threshold.
- Manufacturing: **38 connected pieces on one 256 × 256 mm bed** with
  6 mm margins; actual packed bounds [6,6,0] to [248.573,180.165,35] mm.
  The fit-test bed contains exactly **three connected pieces**. An initial
  coplanar neck/collar boundary separated the test bar into two meshes;
  intentional overlap fixes it and is covered by the bed regression.
- Generated supported instruction plan: **36 steps, zero flips, zero design
  errors**, with every cap after every bar via existing generic `after=` hints.
- Fresh local manual: seven guides and the added design/assembly notes.
  Chromium: **80 page/viewport checks** across twenty pages at four widths,
  including loaded images, long names and sticky navigation.

`examples/assemble/tesseract/verify.py` repeats the mesh, motion, capture,
press-fit and print-bed checks. Its Python dependencies are listed beside it.

## Remaining limits

The intended press fit needs a physical coupon test on the selected printer
and filament. No print, pull-off measurement, flexible-pin simulation or creep
measurement was performed. Motion tests sample positions rather than proving
continuous swept volumes. The planner does not generate support removal;
that final manual step and the true insertion directions are documented in
`docs/2026-10-13-TESSERACT.md`. Generated legacy peg face labels locate the joints and
do not encode the diagonal or sideways insertion paths.
