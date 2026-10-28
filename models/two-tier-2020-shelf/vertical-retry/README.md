# Upright rail retry — 28 October 2026

The owner reported that two of the original three fit-test pieces were decent,
but the horizontal rail's top lips had printing defects. This retry keeps the
same 50 mm rail and slot dimensions, and changes its declared down face to
`lft`, putting the rail length along Z. It prints one piece only.

Verified slice: X1 Carbon, 0.4 mm nozzle, external Generic PLA, textured PEI,
0.2 mm layers, six walls, 35% gyroid, 5 mm outer brim, supports disabled.
The 20 × 20 × 50 mm part is centered at (128,128), clear of the X1C exclusion.
The package has 250 layers, no support paths and 61,366 checked extrusion
endpoints. Estimate: 11.36 g and roughly 72 minutes of sliced print time;
the summary's broader estimate including overhead is about 81 minutes.

The owner explicitly confirmed a clear X1C bed and requested printing when
ready. One LAN API start was acknowledged; the printer reached layer 1 of 250
at 06:27 Sydney with no print error or HMS faults. This is startup evidence,
not a completed-print or improved-lip claim. At 06:35 Sydney it was at layer 22,
with about 65 minutes remaining and no faults. `fit_run.json` retains the dispatch
intent, acknowledgement and observations; it prevents sending the job twice.

After cooling, remove the brim and try the same sliding nut in all four slots.
Compare lip surfaces with the horizontal coupon, then repeat the gusset/panel
bolt test. Do not infer full-shelf strength from a good fit: upright rails have
a different layer direction and need physical load testing. Earlier full-build
horizontal toolpaths must be reoriented and re-sliced before another full build.

`fit.assemble` declares the new orientation. Running `coscad next` regenerates
packing at the default origin; run `center_bed.py` to center it again before
slicing. The checked `fit_manifest.json` and `evidence/slices.json` describe the
actual centered slice. The existing `print_fit.py` is a one-shot dispatch helper;
without `--start --bed-clear` it only reads status.

`full-manufacture.assemble` is the proposed full inventory with every rail's
long axis upright. It preserves the earlier horizontal project for comparison.
It has not been packed or sliced; review tall-rail stability and the result of
this coupon before preparing that build. Do not reuse the old row packer, which
was written for horizontal rails.
