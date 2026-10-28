# Looser sliding-nut retry — 28 October 2026

The owner reported that the upright rail printed well but its sliding T-nuts
were very tight, and requested at least 1 mm more tolerance and another print.
Both beds were confirmed clear; this retry uses the X1C.

The 50 × 20 × 20 mm rail stays upright. The pocket's widest width changes from
10.0 to **11.2 mm**, its central depth from 3.0 to **4.2 mm**, and the mouth from
6.4 to **7.4 mm**. These are total increases, not an extra millimetre per side.
The 1.5 mm lip thickness stays intact. Lower pocket corners are chamfered to
preserve diagonal connecting webs; simply enlarging four rectangular pockets
would disconnect the rail's corners from its core. The 5.6 mm screw-tip channel
stays unchanged because it is not the nut-foot clearance.

The actual mesh is one connected, watertight solid. A nominal 9.6 × 2.6 mm nut
foot clears all four slots at three longitudinal positions; an oversized foot
collides as a negative control. The actual owner's nut has not been measured,
so this remains a physical fit trial, not a claim of universal nut compatibility.

Verified X1C 0.4 mm / Generic PLA / textured PEI slice: six walls, 35% gyroid,
0.2 mm layers, 5 mm brim, no supports, 250 layers. Estimate: 9.45 g and about
77 minutes including overhead. The owner-authorized LAN dispatch is recorded
in `fit_run.json`; the script sends one start only, after an upload read-back
hash check. Neither fit nor load capacity is established by successful startup.

After cooling, compare the same nut in each slot and check whether it can slide
and clamp with the gusset and M5 bolt without spreading or whitening the lips.
Keep earlier profiles and toolpaths for comparison. Apply a successful clearance
to full-length rails only after this trial; do not scale the complete profile.

The X1C acknowledged this single dispatch and reached layer 1 of 250 at
11:33 Sydney, with no print error or HMS faults. `dispatch-summary.json`
retains the startup evidence; this is not completion or a confirmed nut fit.
