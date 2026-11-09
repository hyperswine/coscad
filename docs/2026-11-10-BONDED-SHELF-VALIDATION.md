# Bonded shelf load sharing and refinement

Status: implemented and locally validated numerical experiment, 2026-11-10.
This advances the [assembly FEA proposal](2026-11-09-ASSEMBLY-FEA-PROPOSAL.md)
beyond the [split-beam foundation](2026-11-09-BONDED-FEA-EXPERIMENT.md): a
three-body shelf, stiffness-dependent support reactions, independent solver
comparison and a three-level mesh study now run reproducibly.

The current bonded solver needed no changes. `.assemble` interface syntax,
nonmatching meshes, compliant joints, contact and physical validation remain
outside this experiment.

## Fixture and mechanical assumptions

The shelf is 12 x 6 x 2 mm at x=0..12, y=0..6, z=2..4. Two rectangular
cantilever arms are 2 x 8 x 2 mm at x=0..2 and x=10..12, y=-2..6, z=0..2.
Each arm's back face at y=-2 is fully clamped; its top over y=0..6 is explicitly
bonded to the shelf's bottom. Keeping the clamp behind the shelf avoids the
currently unsupported overlap between clamped and tied nodes.

A total 1 N downward force is uniformly distributed over the shelf's top. The
small load keeps this numerical fixture in a small-deformation regime. It is
not a calibrated model of a real printed shelf or its fasteners.

All bodies have Poisson ratio 0.3. The symmetric fixture uses E=2000 MPa
everywhere. The asymmetric fixture changes only the right arm to E=500 MPa.
The shelf and left arm retain E=2000 MPa; force sharing is solved, not prescribed.

`scripts/bonded_shelf.py` generates the exact same physical geometry at cell
sizes 2, 1 and 2/3 mm: 26, 208 and 702 elements. Every interface matches exactly;
all cases fit the existing 1000-cell budget. The two checked-in JSON fixtures
are the coarse cases; refinement inputs are generated temporarily by the
comparison runner.

## Independent comparison

`scripts/compare-bonded-reference.py` uses scikit-fem's quadratic Hex20 shape
functions and quadrature to assemble each body separately. It integrates
tractions over exterior facets, builds an explicit displacement-equality
projection for the declared bonds, and solves the reduced system with SciPy's
sparse direct solver. Coincident nodes outside explicit ties remain separate.
It does not use CoScad's stiffness matrix, shape gradients or solution.

Compare all corresponding body-node displacements, total and per-body energy,
each support wrench and both sides of each interface wrench. The reference
also checks reduced DOF count and complete body/node/interface identity coverage.
The same prescribed physical problem and discretization are shared; the element
implementation, assembly and numerical solve are independent.

Acceptance thresholds were set before comparison:

- Displacement error normalized by the largest reference displacement, relative
  energy errors, and normalized wrench differences: at most 1e-6.
- Global and interface force/moment balance: at most 1e-7, normalized by the
  applied-force magnitude and that magnitude times assembly extent respectively.
- Symmetric arms each carry half the vertical load within 1e-7; softening the
  right arm shifts load to the left while both vertical reactions remain positive.
- Medium-to-fine probe displacement and interface wrench changes: below 5%.
  Displacement uses the front-centre probe at (6,6,4); interface force and moment
  vectors are compared separately using the finer vector norm, with explicit
  near-zero floors. Moments are about the world origin at every mesh level.

All six comparisons passed. Worst normalized nodal displacement error was
2.18e-11, per-body energy error 2.37e-10, force-resultant difference 1.30e-11,
and moment-resultant difference 8.11e-12. Global/interface balance residuals
were below 2.40e-11 normalized force and 1.41e-11 normalized moment. These are
numerical agreement measurements, not accuracy bounds against reality.

## Refinement results

The displacement column is the magnitude of the probe's Z component. Peak
stress is CoScad's largest Gauss-point von Mises value, recorded separately;
it was not independently stress-compared or accepted as converged.

| Case | Cell mm | Elements | Probe downward mm | Left vertical share | Peak stress MPa |
|---|---:|---:|---:|---:|---:|
| Symmetric | 2 | 26 | 0.0134464 | 50.0000% | 1.0901 |
| Symmetric | 1 | 208 | 0.0146083 | 50.0000% | 1.5118 |
| Symmetric | 2/3 | 702 | 0.0148077 | 50.0000% | 1.6566 |
| Soft right | 2 | 26 | 0.0225012 | 53.8829% | 1.6355 |
| Soft right | 1 | 208 | 0.0245461 | 53.6993% | 2.2578 |
| Soft right | 2/3 | 702 | 0.0249025 | 53.6777% | 2.3958 |

Medium-to-fine changes in the full probe displacement vector were 1.374%
(symmetric) and 1.455% (soft right). The largest interface force/moment vector
change was below 0.297%. Both cases pass the selected 5% refinement gate.

Coarse-to-medium probe changes were 8.045% and 8.431%, so the coarse meshes do
not meet that gate. Peak stresses continue to rise with refinement; sharp
clamps and interface edges can cause mesh-dependent peaks. These results do
not establish peak-stress convergence, strength or a safety factor.

The 4:1 material-stiffness ratio does not produce 80/20 vertical load sharing:
the flexible shelf and moment-transmitting bonded connections also participate.
At the finest mesh, left/right vertical reactions are about 53.68/46.32%.

## Regression and evidence

The normal Haskell test tier now covers both coarse fixtures, global and
interface equilibrium, equal symmetric reactions, redistribution with changed
stiffness, mirrored material assignments and linear-load/quadratic-energy
scaling. The focused experiment/stress suite passed **392 checks, zero failures**.
The geometry/oracle tier and broader suite were not rerun for these fixture
and validation-tool additions; the runtime solver was unchanged.

The independent runner passed all six cases and both final refinement gates.
Additional checks confirmed that the checked-in fixtures match the generator
and that deliberate perturbations to displacement, interface force or energy
cause the comparator to reject the result. Documentation consistency and
whitespace checks passed.

[Machine-readable evidence](evidence/2026-11-10-bonded-shelf/comparison.json)
records the thresholds, all six summaries and comparisons, both refinement
steps (including coarse failures), reference quantities, package versions,
executable hash and relevant source hashes. Runs used the pinned optional
`experiments/requirements-fea.txt` environment. Timing fields are observational,
not isolated performance benchmarks. This is local evidence, not hosted CI or
physical measurement.

## Reproduce and next step

See [the experiment guide](../experiments/bonded/README.md#shelf-validation)
for exact commands. The next integration task is instance-qualified bounded
interface regions and experimental `.assemble` analysis with explicit physical
supports and bonds. Existing placement `fixed`/`mate` declarations must retain
their geometric meaning. Physical measurements and calibrated joint/material
models are still required before claiming printed-assembly strength prediction.
