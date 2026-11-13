# Glider structural feasibility screen - 2026-11-11

The single-part solver can analyse this lofted wing. The original exact-grid bonded-assembly
solver rejected it: the actual wing fails its grid-aligned orthogonal
boundary requirement. Refining the grid cannot make the curved wing orthogonal.
`bonded-run.json` records that capability rejection; it is not a loaded assembly test.

## Actual geometry, deliberately simplified mechanics

The probes copy the ballast-free right-wing source, without modifying the printed
model. In its print frame, chord is +X, span is -Y, and thickness is +Z.
`support bak` clamps the root end; `load fwd 0 0 1` applies a total 1 N upward
force on the selected tip-end voxel faces. These are artificial test conditions,
not the actual saddle/bolt supports or distributed flight lift.

| Cell | Cells | DOF | Time | Maximum displacement | Peak von Mises |
|---|---:|---:|---:|---:|---:|
| 4 mm | 2573 | 46632 | 50.2 s | 0.679 mm | 0.757 MPa |
| 3 mm | 5770 | 96894 | 153.6 s | 0.743 mm | 0.750 MPa |

Deflection changes by about 9.5%; this is a refinement comparison, not established
mesh convergence. The stair-stepped load/support patches also change with grid
size. Solver residuals and reaction forces are retained in the JSON reports.
The 4 mm run balances the applied 1 N with approximately -1 N root reaction.
Initial 45-second budgets timed out; allowing 180 seconds completed both runs.

The material is default homogeneous isotropic PLA (E = 3500 MPa, nu = 0.36).
The printed wing has two walls and 8% gyroid: the solver models neither that shell
and infill architecture nor actual spool properties. Its printed-layer strength
check does not supply anisotropic stiffness. Do not interpret the reports' safety
pass as a strength qualification of the printed aircraft. Buckling, bolt contact,
slip, preload, fatigue and impact are absent.

## Useful next steps

1. Weigh both printed wings and compare actual assembled balance with the saved
   mass estimate before aerodynamic tests.
2. Bench-test wing deflection with a known small load and measured saddle support;
   calibrate a shell/infill representation against that observation.
3. Add distributed pressure loads and explicit saddle/bolt load transfer. Preserve
   the actual wing shape rather than simplifying it to glued rectangular blocks.
4. Check positive and negative bending, local root/bolt bearing and thin-skin
   buckling separately. Only then use a structural result to guide flight loading.

## Reproduce

From this directory, use a current executable whose capabilities include stress:

```sh
GHCRTS=-N2 /absolute/path/to/coscad stress wing-right-cell4.coscad -o rerun-cell4.json
GHCRTS=-N2 /absolute/path/to/coscad stress wing-right-cell3.coscad -o rerun-cell3.json
```

Allow at least three minutes for the finer case on this machine. The archived
reports retain original temporary source paths as provenance; their immutable
artifact paths are relative and were copied with the reports. The assets include
the exact source and analysed mesh. No printer control occurs in these probes.

## Assembled wing and fuselage

Run these commands from the repository root with the rebuilt CLI. The fixture
uses the actual ear-free right wing and fuselage in a common saddle frame.
The wing is turned 90° and moved 2 mm; the inverse incidence, dihedral and
saddle translation put the fuselage in the same frame. Analysis +Z is the
right saddle normal. The print recipe is represented by a nominal isotropic
E of 3500 MPa and Poisson ratio 0.36 for both bodies.

```sh
d=models/bolted-glider-balanced/structural
bin=$(cabal list-bin exe:coscad)
export GHCRTS=-N2
"$bin" stress "$d/wing-saddle.assemble" --bonded "$d/wing-saddle.json" --preview -o /tmp/regions.json
python3 scripts/assembly-region-preview.py /tmp/regions.json /tmp/regions.svg
"$bin" stress "$d/wing-saddle.assemble" --bonded "$d/wing-saddle.json" -o /tmp/wing-result.json
python3 "$d/wing_gauges.py" /tmp/wing-result.json
```

The fixture clamps a 4 × 4 mm voxel patch on the fuselage. An inward pressure
of 0.0625 MPa on a 4 × 4 mm underside patch applies 1 N along analysis +Z.
Two explicitly selected 4 × 4 mm saddle patches transmit the load. They are
offset from the drilled holes. This is a deliberately small clamp and two
idealized load-transfer patches; physical clamps, actual screw head areas,
nut pockets and saddle contact can give substantially different stiffness.
The loading pad is inboard of the extreme tip. The gauge script averages
its four corner displacements and uses a 16 mm baseline to measure wing
rotation relative to the fuselage. Angles are in radians.

For a compliance sensitivity run, add `"stiffness_n_mm": [1000,1000,1000]`
to each bond object. These are illustrative total patch stiffnesses in the
three analysis axes; each is divided among matching node pairs. The patches
resist rotation through their finite area. All three stiffnesses must be
positive. Both signs of normal force are transmitted. Physical bolt preload,
clearance, separation, released directions and slip remain open. For material
sensitivity, change each instance's `young_mpa` and optionally supply a
`material_source` identifying the coupon or assumption. Unknown print
orientation yields a null layer tension result.

The opt-in `coscad.assembly-sampled/1` schema retains 27-point solid-fraction
sampling and scales each Hex20 stiffness and recovered nodal force. Perfect
and compliant joint faces additionally require complete planar surface coverage
on the source triangle mesh. Support and pressure rectangles describe the
voxel surface and can approximate a curved physical footprint. Region preview
exports actual selected face corners and per-body sampled/source volumes prior
to solving; its XY rendering shows supports in blue, loads in red and joints in
green. Preview does not establish equilibrium or exclude all rigid modes.
The solver rejects excess work above 2 million candidate cells per sampled
body, 8,000 occupied cells per assembly or 160,000 reduced degrees of freedom.

For the bench comparison, record fixture footprint and motion, print recipe,
filament, actual load, gauge stations and measurement uncertainty. Measure
unloaded, loaded and unloaded again. Use an independent load station to check
any fitted modulus or connector stiffness. No measured stiffness or flight
loading is assumed here. The CLI regression is
`python3 scripts/test-sampled-assembly.py "$bin"`.

### Matched 4 mm runs

Both native solves used 7,544 occupied cells and a relative residual target
of 1e-6. The maximum absolute component of the global equilibrium error was
below 1e-5 N and 1e-3 N mm in both cases.

| Saddle assumption | Reduced DOF | Pad travel | Root angle relative to fuselage |
| --- | ---: | ---: | ---: |
| Two perfect bonds | 122,583 | 1.714 mm | 0.0753° |
| Two compliant patches, 1,000 N/mm per axis each | 122,631 | 6.893 mm | 1.7555° |

The change in root angle makes a two-station bench measurement especially
informative. The calculations use one resolution and uncalibrated stiffness.
See the [gauge and equilibrium results](wing-saddle-comparison.json) and
[XY region preview](wing-saddle-preview.svg).

### 2 mm refinement

The 2 mm mesh contains 18,336 wing cells and 35,915 fuselage cells. The two
connected bodies have zero overlapping occupied cells. The underside of the
fuselage clamp changes height across its projected 4 × 4 mm footprint.
At x = 0–2 mm it lies at z = −44 mm and at x = 2–4 mm it lies at z = −42 mm.
The finer configuration clamps both exposed strips. Their projected area
totals 16 mm², with the same xy coordinates as the 4 mm case. The load pad,
pressure, both joint rectangles, material constants and gauge coordinates
are identical. Changes in response include the change in clamp elevation.

The refinement executable accepts up to 56,000 occupied cells and 950,000
reduced DOF. The ordinary sampled CLI accepts 8,000 cells and 160,000 DOF.
To reproduce the finer perfectly bonded case from the repository root:

```sh
d=models/bolted-glider-balanced/structural
cabal run coscad-experiments -- assembly-refine "$d/wing-saddle.assemble" \
  "$d/wing-saddle-cell2.json" /tmp/saddle-2mm.json +RTS -N2 -M7G -s -RTS
python3 "$d/wing_gauges.py" /tmp/saddle-2mm.json
```

For the compliant case add `"stiffness_n_mm": [1000, 1000, 1000]` to both
bond objects. This specifies the total stiffness of each patch.
