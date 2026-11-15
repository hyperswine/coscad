# Lightweight glider study and replaceable nose

Development candidate, 2026-11-14. The previously printed balanced-v2 remains
unchanged. Do not use this folder as an accepted manufacturing package.

## Is 218 g heavy?

The saved slice-derived baseline is 218.34 g including hardware/ears, with a
473 mm span and 4.442 dm2 wing area: 49.15 g/dm2. For a small gentle hand-launched
glider this is a relatively demanding loading, not proof that it cannot fly.
At assumed CL=0.6 and rho=1.225 kg/m3 the lift-equation speed is 11.45 m/s.
This is not measured stall, launch or best-glide speed. Maximum lift and drag
polars remain unknown. Lower mass reduces speed at fixed CL approximately
with the square root of mass, from [NASA's lift equation](https://www1.grc.nasa.gov/beginners-guide-to-aeronautics/lift-equation/).

| Scenario | Mass | Loading | Speed at CL=0.6 |
|---|---:|---:|---:|
| Existing estimate | 218.3 g | 49.2 g/dm2 | 11.45 m/s |
| Study target | 180 g | 40.5 g/dm2 | 10.40 m/s |
| Study target | 160 g | 36.0 g/dm2 | 9.81 m/s |
| More ambitious target | 140 g | 31.5 g/dm2 | 9.17 m/s |

Targets are not achieved estimates. Lighter does not by itself improve glide
ratio; it changes the speed/loading regime. Tail balance, aeroelastic twist,
Reynolds effects and trim must be reconsidered.

## Screw-on nose candidate implemented

`nose.coscad` retains the existing forward outline through y=-65 mm and adds
a 12 x 24 x 24 mm tongue. `fuselage.coscad` retains the aft body from y=-64.8,
with a 12.4 x 24.4 x 24.4 socket: 0.20 mm clearance per face and a 0.20 mm
external seam. The 24 mm engagement and broad tongue provide alignment instead
of relying on loose screw shanks. There are two 3.4 mm transverse holes at
y=-53, z=12/24 mm and flat exterior bearing pads, 23 mm overall width.

Proposed hardware: two M3 x 30 bolts and ordinary nuts. At 23 mm outside width,
a 30 mm under-head length allows a typical 2.4 mm nut and about 4.6 mm protrusion
without washers. Confirm the actual head/nut dimensions and access before slicing;
fastener envelopes have not yet been checked. Heads/nuts remain outside, with
no inserts or inaccessible captive hardware. A fit coupon and light tightening
are required: a clearance socket can distort if overtightened. Do not claim this
unmeasured joint absorbs crash energy; the intention is replacement after damage.

The candidate also cuts three diamond side-web openings, centred at y=-20,48,105
and z=15, with 45-degree roofs. Original wing/tail mounting features remain, but
the changed web load path needs analysis. Mesh/assembly static checks passed:
two solids, no overlap, 0.20 mm nose/body gap. See `geometry-validation.json`.
No physical fit, reinforcement strength, screw envelope, printability or flight
qualification has been established.

Raw CAD solid volume is only about 1% lower than the old fuselage. This does NOT
prove lower print mass: new interior perimeters, thicker local walls and bolts
can make it heavier. This candidate primarily establishes the removable joint.

## Where useful mass savings should come from

Current sliced estimates: wings 112.34 g together, fuselage 71.85 g, tail surfaces
16.55 g, hardware 17.6 g. The baseline wings/body already use 8% infill, so reducing
that alone offers little certainty and may sacrifice skin support.

1. Keep the wing root pads and fastener-bearing regions robust. Develop explicit
   lightweight skins, spanwise spar caps/web and spaced ribs. Preserve a closed
   torsion-carrying leading-edge region rather than an unsupported thin shell.
   This is a next design step, not implemented by the nose files.
2. Use a lighter rear fuselage/tail structure before indiscriminately removing
   nose mass. Nose mass contributes to the current zero-ballast balance.
3. Compare actual slices with regional wall settings: four walls at nose/joint
   and root interfaces, baseline two elsewhere first. Do not globally halve walls
   and call it structurally equivalent. These settings are not yet sliced here.
4. Recompute mass/CG with the new nose, hardware and ear removal. Target the
   existing nominal 10-15% static-margin band, then re-evaluate sensitivity/trim.
5. Bench-check root and outboard deflection, twist, bolt slip and local crushing
   using known small loads before low-energy glide trials.

## FEA and acceptance

The repository now has experimental sampled curved-assembly pressure loads and
spring connectors; see the baseline `structural/README.md`. Its 4 mm wing/saddle
comparison showed substantial sensitivity to assumed joint stiffness. It is not
calibrated to printed skin/infill or this nose. No old green safety factor is
inherited by these modified parts. Use explicit distributed aerodynamic loads,
new supports/connectors and mesh refinement, alongside physical deflection tests.

Before printing: source/mesh checks, a nose-joint coupon, bolt-envelope/access
checks, full slice mass/CG, print orientation/support review and thermal audit.
Before flight: actual weight/CG, measured stiffness and controlled trim tests.

Reproduce the current static geometry check from the repository root:

```sh
bin/coscad stl models/bolted-glider-light/nose.coscad
bin/coscad stl models/bolted-glider-light/fuselage.coscad
bin/coscad check models/bolted-glider-light/nose-joint.assemble
```

## Film-covered wing trial, 14 November

The mirrored wing frames retain the balanced-v2 outline and screw locations.
Two swept spars and 1.2 mm ribs support separately applied film on both faces.
The longer exposed bays are about 17 mm wide spanwise. Keep the counterbores
accessible when covering. The STL files contain the printed frames only.

The four candidate slices used the saved X1C PLA process. Object extrusion
gives 53.04 g for both frames, 64.20 g for the body and 14.08 g for the nose.
With the saved tail and mounting hardware, 5 g assumed for two M3 bolt/nut
sets, and 2.44 g of covering (25 g/m² installed, 10% area allowance), the
estimated total is **172.93 g**. The longitudinal CG is **68.0 mm aft of
the root leading edge**. Using the baseline neutral point of 105.06 mm and
99.42 mm MAC gives **37.3% static margin**. The nominal target is 90.15–95.12
mm aft. Even the forward end would require about 36.85 g added at y=194 mm,
giving 209.8 g. A mass-neutral move of about 14 g from the nose centroid to
y=194 mm gives the same shift. That illustrates the scale of redesign.

A spanwise Euler–Bernoulli beam FEA uses a clamp at 20 mm, 1 N uniformly
distributed over the remaining 216 mm, nominal PLA E=2.5 GPa, and section
properties estimated from two perimeters, top/bottom shells and 8% core.
At 0.125 mm element length the baseline and candidate tip deflections are
0.322 and 1.95 mm. Nominal peak bending stresses are 0.283 and 1.83 MPa.
Halving the spacing from 0.25 mm changes candidate deflection by 1.4%.
The film contributes mass only. Sweep-induced twisting, rib/cap junctions,
bolt compliance, layer failure, film bonding and buckling remain unresolved.
These results compare bending stiffness and stress demand. Ultimate strength
requires material and assembly tests. This candidate misses the balance target.

Run `"$SOL" models/bolted-glider-light/wing-study.sol` to generate both CoScad frames and the beam comparison.
Render both STLs using the loft/poly-capable CoScad build. Run
`"$SOL" models/bolted-glider-light/slice-study.sol` (Bambu Studio CLI) to
reproduce the slice mass and CG. The skin allowance uses y=115 mm. The
separate nose and body use the baseline global two-wall process. Regional
reinforcement, adhesion aids and actual hardware weights can change the result.
