# Tapered tube glider, standard PLA

Use the two closed wings from ../shell and the balanced-v2 stabilizer and fin.
The octagonal tube has a flat keel, 16 × 30 mm forward section and 10 × 13 mm
aft section with nominal 0.85 mm wall offsets. Two 19 mm saddle lands retain
the original wing mounting faces at the fasteners. The original tail mounting
region joins the tube at the aft end. The short hollow nose retains the original
tongue and twin transverse screw locations. Its tip is 40 mm farther aft.

The native mesh and nose-joint checks produced one body and one nose with a
0.20 mm gap and zero overlaps. Run `"$SOL" models/bolted-glider-light/tube/slice-study.sol` to reproduce standard PLA
deposited mass and longitudinal moment using the shell X1C process. Wings
are reused from the previous slice. Nose hardware allowance is 5 g.

The 132.24 g assembly balances at y=87.25 mm. With the prior neutral point and
MAC, 3.69 g centred at y=194 mm gives 135.93 g at 15% static margin.
About 7.02 g at that station gives 139.27 g near the middle of the band.
These are design estimates. Allow for the actual trim weight holder and
measure the assembled CG. The ballast holder has not been modeled.
Reduced saddle contact, tube bending and torsion, fastener bearing, and
physical mating under load need checking. The short nose has a small keel
contact area in the study slice. Bed adhesion has not been demonstrated.
