# Next tesseract batch - 1 November 2026

The owner reported substantial improvement of the A1 center and requested an
X1C center comparison, also authorizing the next A1/P1S batch and confirming
all beds clean. The A1 improvement is recorded separately at
`../bed2/a1-center-mouse-ears-20261031/physical-outcome-20261101.json`.

| Printer | Plate | Mode | Slice estimate | Filament source |
|---|---|---|---|---|
| X1C | Center only, same support overrides and four removable pads as A1 | By layer, one object | 3 h 31 min | Loaded external PLA |
| P1S | Three struts, then one tall bar | By object, front to back | 4 h 32 min | Loaded external PLA |
| A1 | Three tall bars | By object, front to back | 4 h 21 min | White PLA, physical AMS slot 1 |

The center recipe is under `../bed2/x1c-center-mouse-ears-20261101/`.
Both tall-part layouts keep unmodified meshes, six walls, 20% gyroid, 8 mm
brims, 0.2 mm layers and 12-second minimum layer cooling target / 5 mm/s minimum.
P1S puts its 99.2 mm bar last because of the 90 mm non-last lid limit.
The A1's installed limits are different: lid 256 mm, rod height 25 mm, maximum
head radius 73 mm and rod distance 56.5 mm. Its three bars use staggered
centres (55,50), (201,128), (55,206), retaining those limits and 5 mm margins.

The initial native A1 end routine crossed the plate at max height +0.4 mm,
which failed the conservative extra 0.5 mm head-return margin. It was rejected
before dispatch. Only that initial end lift is now max height +5 mm; all
manufacturer limits remain unchanged. The re-sliced file passed actual full
object order, layer progression and sampled head/rod return-path checks.
`three-bars/native-end-clearance-rejection.json` preserves the finding; it
is not evidence of a measured collision. The P1S layout passed unchanged.

`prepare.py` reproduces the two tall-part projects without dispatch and refuses
existing print-run records. Slice each with its generated presets, no automatic
arranging/orienting, then run the shared geometry validator and sequential
validator for the corresponding printer. Static/sampled checks do not prove
dynamic clearance, adhesion or completed physical quality.

`../print_batch_20261101.py` accepts x1c-center, p1s-parts or a1-bars. Without
start flags it only reads status. Starts require bed-clear confirmation,
fresh thermal/geometry/sequence checks, correct machine/nozzle/material,
frozen upload bytes and matching FTPS read-back, followed by one saved start
intent and one project-file command. Existing run records block repeats.
Bounded startup observations stop after first layer or an active fault; no
recurring monitor is created.

Inventory planning uses five owner-accepted struts (one earlier mixed trial,
four X1C staggered) and one owner-accepted bar (the mixed trial). If this batch
succeeds physically, it adds the three remaining struts and four bars, reaching
8 struts / 5 bars; seven bars would still be needed. This is a planning count,
not acceptance of newly printed parts. The damaged compact P1S struts are not
counted as good threaded components.

## Startup acceptance

All three uploaded packages were read back with matching SHA-256 hashes, and
one start was accepted on each printer. The P1S reached layer 1/1369 at
13:06:29; its recurring startup fan warning cleared on fresh reads without
controls. A1 reached layer 1/1488 at 13:07:40 with physical AMS slot 1 selected.
X1C reached layer 1/282 at 13:08:35. All three showed 55/220 C targets and
no active faults at their checkpoints. `dispatch-summary.json` links the
individual observations; P1S retains the earlier warning and later follow-up.
These are first-layer observations, not acceptance of completed parts. Bounded
startup observers have ended; no recurring monitor or repeat start was created.

## X1C center physical outcome - 1 November 2026

Owner inspection found overall improvement with the revised supports and
mouse-ear pads, but one lower diagonal repeated the original X1C defect even
with the center isolated. The corresponding A1 center was almost perfect.
See the [physical outcome](../bed2/x1c-center-mouse-ears-20261101/physical-outcome-20261101.json).
This points toward an X1C-specific printing/setup difference, with filament,
machine defaults, calibration and hardware not yet separated. It is not full
acceptance of the center's female-threaded diagonals or a diagnosed cause.
