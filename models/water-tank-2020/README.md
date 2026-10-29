# 2020-framed tank: four-height design experiment

Owner request: approximately 1 m tall × 1 m wide × 1 m deep, sealed rigid
panels, existing 2020 rails/panel mounts/nuts/bolts. The owner requested thinner
PLA panels; this study chooses **2.0 mm PLA** instead of the existing 2.5 mm PETG.

Open **[report.html](report.html)** for the model, four-height table and plots.
[results.csv](results.csv) holds the numerical summary. The assembly and cutaway
are [tank.assemble](tank.assemble) and [tank-cutaway.coscad](tank-cutaway.coscad).

## Outcome

This is a useful **screening experiment, not a build-ready or fill-approved tank**.
At every tested depth, the 2 mm panel deflection is already outside the selected
small-deflection screen (w/t ≤ 0.5). Linear stress/displacement fields therefore
identify a problem but cannot predict the final bowed shape, leakage or failure.
The frame and bracket are separate idealized component calculations; their
numbers must not be added to panel deflection and called a coupled tank result.

The geometry gives 976 × 976 × 1000 mm of water space within a nominal 1020 mm
frame footprint and 1060 mm post height. Full depth means about 953 L / 953 kg
of water, without freeboard. There are 100 wall panels and 25 floor panels.
Interior panels keep the 200 mm pattern; edge panels are trimmed or extended
(188/212 mm as needed), preserving the original four mounting holes. Seam and
screw-hole seals are still unspecified. Rounded panel edges leave gaps which
must be bridged by a real seal design.

## Reused geometry and derivatives

`build_model.py` records original source hashes in `model.json`:

- `examples-next/panel_center.coscad`: four M3 holes at ±80 mm; new 2 mm PLA
  thickness, 0.8 mm rounding and adjusted boundary extents.
- `examples-next/rail_x180.coscad`: slotted 2020 profile; 180/980/1060 mm lengths.
  Its modeled section is not a measurement of the purchased aluminium profile.
- `examples-next/bracket_seam.coscad`: unchanged PETG seam bracket geometry.
- `examples-next/tnut_m5.coscad`: unchanged crude purchased T-nut preview.
- `examples/assemble/plan/flat90.coscad`: wall-frame L connectors, assumed PETG.
- `examples-next/corner_90.coscad`: purchased inside-corner previews for the floor.

M3/M5 screw solids show nominal shanks/heads only. Threads, preload, grade,
actual vendor T-nuts and purchased connector shapes are not qualified. Hardware
is not silently fused into a solid elastic tank. See `bom.json`; it is a concept
inventory, not a released cutting, hardware or build specification.

## Analysis contracts

The current Haskell solid FEA does not provide hydrostatic thin plates, a mixed
beam/frame assembly, nonlinear contact or seals. This folder adds a **separate
reproducible Python FEA companion**, using the already pinned scikit-fem/SciPy
experiment environment. Native CoScad still generates the rail and bracket STL.
No new general-purpose `coscad stress` capability is claimed.

- Hydrostatics: ρ=1000 kg/m³, g=9.80665 m/s². Integrate p=ρg max(H−z,0)
  over every wetted wall tile; constant bottom pressure. Mesh lines include all
  four free-surface heights. Wall/floor resultants and moments are checked.
- Panels: Morley Kirchhoff plates, illustrative isotropic PLA E=2300 MPa,
  ν=0.35, t=2 mm. Main support model includes four 12 × 15 mm tab patches plus
  normal-restraint lines at rail inner faces where the panel reaches the rail.
  These are **bilateral** restraints; real unilateral contact may separate.
  The four-tab-only and ideal continuous-edge cases remain in the evidence.
  Holes, rounding, local screw contact and in-plane/membrane behaviour are omitted.
  Surface stress follows the recovered curvature; support peaks are not stress-
  converged, and even the area-weighted P95 is not a strength criterion.
- Frame: 3D Euler–Bernoulli beams, aluminium E=69000 MPa, ν=0.33. Section area
  and moments come from signed tetrahedral integrals of the native slotted rail
  STL. J is assumed as 0.1(Iy+Iz), with 0.05/0.5 sensitivity runs. Beam nodes
  are joint centres, without rigid end offsets. Joints are fully rigid. Original
  base-grid nodes are fixed to an ideal rigid foundation. Foundation anchorage,
  joint compliance/slip, buckling and connector load sharing remain unqualified.
  Reported frame stress is an axial + bending normal-stress envelope, not a
  full von Mises or alloy-yield assessment. Panel membrane stiffness is excluded.
- Seam bracket: actual native geometry → Gmsh → quadratic tetrahedral elasticity
  with PETG E=2000 MPa, ν=0.3. Apply 1 N uniformly over the top flange and clamp
  the entire back plate, excluding the protruding key. This is an optimistic
  fixture for the actual bolted joint. Scale its response by the computed tab
  load; bracket displacement is not fed back into the panel/frame model.
  The companion solver is used because load and clamp share edge nodes, which
  the current CoScad tetra input explicitly rejects. No validation is bypassed.
- Fasteners: only nominal M5 gross-shank single-shear demand is tabulated.
  M3 pull-through, M5 prying/bending/preload, T-nut thread/lip pull-out and the
  separate frame-connection fastener demands need their own contact/detail model.
- Printed-material anisotropy, temperature, creep, fatigue, seal peel/leakage,
  water exposure and actual strength are not included. Illustrative modulus
  inputs do not qualify a real spool, printed wall or long-term tank.

## Verification

`evidence/` contains raw numerical results and source hashes:

- Navier series benchmark for a simply supported uniformly loaded square plate;
  cantilever beam displacement and stress; point-load force/moment interpolation.
- Five focused tests: partial wetting, dry panel, load scaling/validity warning,
  rotated cantilever and off-frame attachment rejection.
- Panel displacement refinement: 24, 48, 72 grid subdivisions plus fitted local
  support/free-surface lines, last change below 5% on the named worst panels.
  Frame refinement: 1, 2, 4 elements per nominal member, below 5% displacement.
- Bracket meshes at 2 and 1.5 mm: loaded-face displacement changes 0.47%; residual,
  resultant force and moment checks pass. This is not proof of peak-stress accuracy.
- Whole assembly syntax/resolution validation. Native interference checks pass
  for a central panel bay, frame corner and floor/wall corner. **Not** a complete
  interference/contact qualification of all 3,729 instances or schematic threads.

Refinement proves consistency of the stated equations, not adequacy of linear
plate theory where panel deflection already exceeds thickness.

## Reproduce

Run from the repository root. Use the native build described in
[the FEA quick start](../../docs/2026-10-26-FEA-NATIVE-QUICKSTART.md).
These commands write only local artifacts; they never contact a printer.

```sh
python3.12 -m venv /tmp/coscad-tank-venv
/tmp/coscad-tank-venv/bin/python -m pip install -r models/water-tank-2020/requirements.txt
TANK_PY=/tmp/coscad-tank-venv/bin/python
# Set PKG_CONFIG_PATH to the pinned Manifold installation first.
COSCAD_BIN="$(cabal list-bin exe:coscad -fmanifold --builddir=dist-native)"
"$TANK_PY" models/water-tank-2020/build_model.py
"$COSCAD_BIN" stl models/water-tank-2020/parts/rail_180.coscad -o /tmp/tank-rail.stl
"$COSCAD_BIN" stl models/water-tank-2020/parts/seam_bracket.coscad -o /tmp/tank-bracket.stl
"$TANK_PY" models/water-tank-2020/test_study.py
"$TANK_PY" models/water-tank-2020/analyse.py --rail-stl /tmp/tank-rail.stl
for size in 2 1.5; do
  "$TANK_PY" scripts/mesh-fea-tetra.py /tmp/tank-bracket.stl \
    --size "$size" --output "/tmp/tank-bracket-$size.json"
  "$TANK_PY" models/water-tank-2020/bracket_fea.py "/tmp/tank-bracket-$size.json" \
    --output "models/water-tank-2020/evidence/bracket-unit-$size.json"
done
"$TANK_PY" models/water-tank-2020/make_report.py
"$COSCAD_BIN" models/water-tank-2020/tank-cutaway.coscad
"$COSCAD_BIN" validate --json models/water-tank-2020/tank.assemble
for fixture in unit-bay corner-joint floor-corner; do
  "$COSCAD_BIN" check "models/water-tank-2020/$fixture.assemble"
done
```

## Next design decisions

Before deciding on a fill height, define the seal system, actual aluminium
profile/hardware and base anchorage. Test thicker panels or substantially smaller
unsupported spans, then run geometrically nonlinear panel/contact analysis with
realistic joint compliance. Calibrate printed material and seals separately.
This experiment deliberately provides no safe water-level recommendation.

Sources: [NASA hydrostatics](https://www.grc.nasa.gov/www/k-12/WindTunnel/Activities/fluid_pressure.html),
[Prusament PLA reference data](https://prusament.com/wp-content/uploads/2022/10/PLA_Prusament_TDS_2021_10_EN.pdf),
[scikit-fem plate formulation](https://raw.githubusercontent.com/kinnala/scikit-fem/12.0.2/docs/examples/ex02.py).
