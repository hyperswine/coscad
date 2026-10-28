# P1S thread-fit test — 28 October 2026

The owner requested this print on the P1S and confirmed its bed clear. This is
the same CoScad screw/socket coupon as the X1C trial, re-sliced for **Bambu Lab
P1S, 0.4 mm nozzle**, Generic PLA and textured PEI. It uses the loaded external
PLA spool, a 5 mm brim, six walls and no supports.

Estimate: **3.01 g, 11 minutes 32 seconds of model toolpaths, about 18 minutes
including startup**; 55 layers and 11 mm maximum height. The actual toolpaths,
settings, extrusion bounds and package integrity are checked in
`evidence/slices.json`. The installed P1S vendor profile supplies the slicer's
missing `C12` model identifier; G-code bytes are unchanged.

`print_fit.py` uses verified printer TLS, validates the package hash, checks
idle/fault/nozzle/spool state, verifies the uploaded file by reading its bytes
back, then sends one start command. `fit_run.json` records the durable intent,
reply and startup observations and prevents a second start. Calling the helper
without `--start --bed-clear` only reads status. A print starting is not evidence
of a successful fit: after cooling, screw the two parts together by hand and
check several turns without force, binding or excessive wobble.

The printer acknowledged the single start command successfully. The latest
recorded startup observation is in `dispatch-summary.json`; it reported the
expected 55-layer job RUNNING and no active faults. This does not establish
completion or physical thread fit.
