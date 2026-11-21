# `coscad gears` and `coscad float`

Status: implemented 2026-11-21. Two read-only analysis commands in the
Haskell core, with no external libraries beyond the linked Manifold kernel.

## Why

Single-part FEA (`coscad stress`) answers "will it snap or sag". It said
nothing useful about the SG90 rack and pinion, whose problems were mesh
geometry: centre distance, height and play. The bath and beach boats had
their own hydrostatics in Python (`verify.py`: trimesh, manifold3d), and the
Sol cutover (926e93b) deleted those scripts without a replacement. These two
commands cover both gaps.

## `coscad gears mesh.json`

A coscad.gear-mesh/1 spec names two parts, each placed as its own .coscad
file places it, and a motion for each: `"rotate": {"axis": [...], "origin":
[...]}` in degrees, or `"translate": [...]` in mm. The driven part moves
`ratio` driven units per driver degree. When `ratio` is absent it follows
from `teeth` and `module_mm` for a rack (the pitch circle's arc length per degree),
or from `teeth` and `driven_teeth` for an external pair (`-z1/z2`). The sweep
defaults to one driver tooth (`360/teeth`) in 24 steps.

At each driver angle:

- **Overlap:** the kernel's exact intersection volume. Anything over
  1e-4 mm^3 is interference.
- **Gap:** the surface distance (the same BVH distance `coscad check` uses).
- **Backlash:** with the driver held, the driven part's free play each way
  before the meshes intersect. The search grows from 1/256 of one tooth's
  driven motion, then bisects 20 times. It is reported in driver degrees, in
  driven units and, given `module_mm` and `teeth`, in mm at the driver's
  pitch circle. If no contact occurs within one tooth's motion either way, the
  drive has lost mesh at that angle.
- **Contact pairs:** after first contact, the driven part is pushed a
  further `contact_push` (default 0.1% of one tooth's driven motion), and the
  separate intersection bodies are counted. The mean over the sweep
  approximates the contact ratio.

The verdict is `interference`, `loses contact`, `backlash over limit` (when
`max_backlash_deg` is given) or `ok`. Every verdict except `ok` exits 2.

With `torque_n_mm`, `module_mm`, `teeth` and `face_mm`, the report adds the
classic Lewis root stress `Ft / (b m Y)`, with `Ft = T / r`. It reports this for
the driver and, when `driven_teeth` is given (0 = rack), for the driven
part. It also gives a safety factor against the material's in-plane strength.
Y comes from Shigley's 20 degree full-depth table.

### SG90 press-fit revision 4

`models/sg90-rack/press-fit/gear-mesh.json` (22 teeth, module 1.5, the rack
translating along -x as the pinion turns +z):

| Quantity | Result | Expected |
| --- | --- | --- |
| Interference | none; the reversed rack gives 33 mm^3 | |
| Gap | 0.172-0.174 mm | 0.174 from `coscad check` |
| Backlash at the pitch circle | 0.368-0.372 mm | 0.22 flank thinning + 2 x 0.2 x tan 20 = 0.366 |
| Tooth pairs, mean (min) | 1.54 (1) | analytic contact ratio about 1.6 |
| Lewis stress at 176 N mm (stall) | 5.4 MPa pinion, 3.7 MPa rack | safety 9.3 and 13.6 in PLA |

The run takes about 3 s.

The contact-pair count depends on the push. Over the same 24 steps it reads
2.17 at a 1% push, 1.88 at 0.3%, 1.54 at 0.1% and 1.25 at 0.03%. The
generated involutes have only seven points per flank, so very small pushes
measure tessellation, not the tooth profile. Treat the mean as an estimate
and the minimum (at least 1) as the check.

## `coscad float part.coscad | float.json`

Hydrostatics comes from the mesh itself. Each triangle of the closed,
outward-oriented displacer is clipped to the half space below the waterline,
and its signed tetrahedron is taken against an apex on the waterline plane.
The cap the clip leaves lies in that plane and contributes no volume or
moment, so it is never built. The waterline is found by bisection (40
steps) for each heel. Trim is found by bisection on pitch until the centre of buoyancy
lies under the centre of gravity.

The conventions match the old scripts. The hull's length runs along x and z
is up. Heel is a rotation about +x. The righting arm `GZ = g_y - b_y` after
rotation, positive when the hull turns back upright. Trim is positive bow
(+x) up.

A bare .coscad input is solid PLA in fresh water. Its displacer is the body
with inward shells dropped, so sealed chambers keep water out. A
coscad.float/1 spec adds:

- `displacer`: the solid that keeps water out, for example the closed
  envelope over an open hull.
- `opening` or `opening_points`: once these go under water, the hull floods.
  The report gives the downflooding heel and the largest righting arm before it.
- `material` or `density_g_cm3`, or a given `mass_g` and `center_mm`.
- `payload`: point masses.
- `water`: fresh, sea or a density.
- `heel_deg`: the heel angles to sample.

Unknown keys are rejected.

The report gives draft, waterline, opening freeboard, static trim, reserve
buoyancy (displacer volume times water density minus mass), the
righting-arm curve with buoyancy and gravity centres, the largest righting
arm and its heel, the first sampled heel with no righting, and the righting
arm inverted.

### Validation against the deleted Python results

| Model | Quantity | `coscad float` | verify.py |
| --- | --- | --- | --- |
| beach-outrigger (default, sealed voids) | draft | 20.335 mm | 20.335 |
| | trim | -0.619 deg | -0.619 |
| | righting arm 5/20/45/85/90 deg | 21.8 / 49.3 / 37.9 / 1.0 / -4.0 mm | identical |
| twin-boat monohull (envelope, opening) | waterline 0/20/40 deg | 15.29 / 13.60 / 9.19 | identical |
| | opening clearance 30/35 deg | 0.72 / -1.19 | identical |
| twin-boat outrigger | righting arm 15/20 deg | 21.34 / 21.62 | identical |

The default and the explicit-envelope runs of the beach outrigger agree, which
confirms the void filling. Each run takes under 2 s. The example specs sit next to
the models (`twin-boat-trials/*-float.json`, `beach-outrigger/float.json`).

## Limits

Both commands are static and rigid. `gears` does not model printed size error,
tooth or shaft deflection, or dynamic loads. `float` does not model waves,
dynamic capsize or leak rates, and it heels about x only. The payload-sweep
checks the old monohull script ran (payload until the rim is near the water)
are not built in. Run them by sweeping `payload` from a script.
