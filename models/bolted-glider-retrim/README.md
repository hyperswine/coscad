# Original bolted glider retrim study

The reference is ../bolted-glider, with 265 mm wing panels. The original
tail surfaces are reused for this study. build.sol generates mirrored shell
wings with skins, internal webs, and ribs from the original wing loft.
fuselage.coscad adds hollow bays and a shallow tapered belly grip, closes
the old ballast pockets, and moves the saddle 24 mm aft as a trim trial.
Fastener access, seating and print setup require assembly verification.
No release for printing or flight has been established.

Run sol flight-screen.sol for an approximate longitudinal balance and lift-speed
comparison. The 194 g input is an assumed printed mass, not a slice result.
The CG is 86 mm aft of the original root leading-edge datum and the estimated
neutral point is at 68 mm. The negative margin fails the ballast-free
target. No tail incidence or glide-distance prediction follows from this
configuration. Estimated speed at CL=0.6 falls from 12.6 to 10.2 m/s.

The existing beam solver uses a fixed root at span station 20 mm and 1 N
uniformly distributed over the remaining panel. At 1 mm element spacing,
the reference and shell estimates respectively are 0.471 and 0.658 mm tip
motion, with nominal peak bending stresses of 0.319 and 0.425 MPa.
The shell's internal webs are homogenized. This does not resolve local
wall failure, torsion, or fastener loading. The native 3 mm solid FEM
identified disconnected voxels on the thin-skin candidate, so it cannot
provide a matched solid stress comparison at that resolution.
