# Extending FEA to assemblies

> 2026-11-10: [Shelf validation](2026-11-10-BONDED-SHELF-VALIDATION.md)
> now adds symmetric/asymmetric load sharing, an independent numerical
> comparison and mesh-refinement checks. The [assembly bridge](2026-11-10-ASSEMBLY-ANALYSIS-INTEGRATION.md)
> now adds explicit bounded regions from `.assemble`; physical validation remains open. The original status below records the earlier stage.

Status: proposal, 2026-11-09. The [first bonded experiment](2026-11-09-BONDED-FEA-EXPERIMENT.md)
now implements the initial coupled-solver foundation. `.assemble` analysis and
the broader stages below remain proposed. The first target is coupled linear analysis of
explicitly bonded parts, followed by compliant joints and, separately, contact.

This extends [the single-part stress command](2026-10-27-STRESS-COMMAND.md)
and the [FEA experiments](2026-10-26-FEA-BACKEND-EXPERIMENTS.md). It follows
[the backend review](2026-11-09-BACKEND-REVIEW.md): retain native Manifold;
OCCT is not a prerequisite for assembly mechanics.

## Problem and intended outcome

A shelf on two brackets cannot generally be assessed by splitting its load in
half and solving each bracket independently. Load sharing depends on the
stiffness of the shelf, brackets and connections. CoScad should solve their
coupled deformation and report which parts and interfaces carry the forces.

The proposed entry point is `coscad stress model.assemble`, alongside the
existing single-part command. Its first version would answer assembly
stiffness and load-path questions under explicit bonded-interface assumptions.
It would not predict bolt slip, pull-out or contact separation.

## Existing foundation

- `Assemble.hs` retains part definitions, assembly metadata, fasteners and
  rigid constraints. Instance identities and solved placements can provide
  the geometry and provenance for analysis.
- `Constraint.hs` solves rigid poses from `fixed` and `mate`. These establish
  placement, not mechanical support or connection behavior.
- `Stress/Part.hs` maps single-part support/load directives to a voxel mesh,
  calls the solver and evaluates material strengths.
- `Stress/Hex20.hs` implements quadratic 20-node hexahedra, stress recovery
  and sparse assembly. Its current problem has one grid and one isotropic
  material; it is not yet a multi-body assembly API.
- `Stress/Sparse.hs`, `Voxel.hs` and the numerical tests provide reusable
  solver, meshing and verification machinery.

The existing physical assumptions remain: small-strain linear elasticity,
homogeneous isotropic material within a part, and approximate voxel geometry.
Existing numerical comparisons do not establish measured printed-assembly
accuracy. The dated stress docs also contain historical OpenSCAD options;
the current geometry path is native only.

## Mechanical semantics

### Placement is separate from mechanics

Do not silently interpret `mate` as a bonded interface, or assembly `fixed`
as a physical clamp. An assembly needs a coordinate reference even when its
actual support is elsewhere. Fastener declarations can locate candidate
connections, but screw size/count alone does not specify joint stiffness,
preload, friction or failure capacity.

Declare physical supports, loads and connection laws separately. Existing
metadata may suggest an explicit analysis declaration, never create one
silently. Do not inherit a part's standalone load case into every instance;
assembly load cases must select the intended instances and regions.

### Interface regions

A frame supplies a position and orientation, not the area over which a joint
transfers forces. Define bounded patches in named part-local frames, initially
planar rectangles or explicit face selections with declared tolerances.

Resolve patches to exterior mesh faces and report their selected area, nodes,
centroid and normal. Reject empty or ambiguous selections, unsupported geometry,
excessive mismatch and unintended overlapping constraints. Do not attach every
face in a datum plane or connect arbitrary nearby nodes across a gap.

A bonded pair ties displacement across its declared common interface. It is an
ideal perfect bond with no separation or slip, not a prediction of adhesive
strength. Begin with coincident, matching planar mesh interfaces; explicitly
refuse nonmatching interfaces until interpolation is implemented and validated.
Assembly positioning tolerance and mechanical interface tolerance are separate.

### Instances, materials and print orientation

Keep each physical instance distinct, including repeated and recursively
included parts. Stable qualified instance paths should appear in diagnostics
and results. Select structural instances explicitly; print-bed multiplicity
and assembly-only display conventions must not silently determine the model.

Assign material per instance, with explicit reusable defaults. Keep local
geometry and material identity through placement. Do not union all bodies into
one solid: that could fuse joints and erase material boundaries.

Record a print-layer normal in part coordinates, with provenance from an
explicit setting or a selected manufacturing orientation. Transform that
normal into assembly coordinates. Evaluate across-layer tension as
`max(0, n^T sigma n)` using the stress tensor in the same coordinate system.
This is a directional strength check, not anisotropic elasticity. If print
orientation is unknown, report that the directional check is unavailable
rather than silently using assembly Z.

### Loads, supports and units

Retain mm, N and MPa. State whether vectors use assembly axes or a named local
frame; transform them once. Distributed loads are total forces over selected
patches. Report applied and reacted moments about a documented common origin.
Use consistent nodal load distributions that preserve resultant force and
moment. Flag point-load singularities and loads applied directly to supports.

The first stage can retain fully clamped selected patches. Released directions,
remote moments and connector rotational behavior need explicit contracts when
introduced. Unsupported cases must fail rather than become rigid constraints.

## Staged implementation

| Stage | Scope | Useful result |
|---|---|---|
| 1. Bonded assemblies | Multiple elastic parts and materials, explicit matching planar ties, assembly loads/supports, coupled linear solve | Global displacement, part stresses and interface load transfer |
| 2. Compliant joints | Translational/rotational springs, beam connectors and explicit released directions | Joint flexibility and stiffness-dependent fastener load sharing |
| 3. Contact | Compression-only contact and opening, then friction and preload | Separation, slip and redistribution through clamped interfaces |
| 4. Failure models | Calibrated bearing, pull-out, adhesive or layer failure laws | Connection capacity under documented material and joint assumptions |

### Stage 1: coupled linear system

Refactor element assembly from the single-part wrapper without changing its
results. Give nodes instance-qualified identities and assign global degrees of
freedom. Cache element stiffness by material and cell geometry, rather than
assuming one element matrix for the entire assembly.

For the initial matching interfaces, identify tied displacement degrees of
freedom or eliminate their equalities exactly. Preserve the symmetric positive
definite reduced-system contract required by the current conjugate-gradient
solver when supports remove all rigid modes. Adding Lagrange multipliers would
produce a different matrix class and requires a suitable solver; do not feed
that system to the current CG path unchanged. Avoid arbitrary very-stiff penalty
springs as an undocumented substitute for a bond.

Start with a common aligned grid and supported matching planar patches. This
limits the first implementation; rotated or nonmatching interfaces must be
refused when they cannot satisfy the contract. Later independent local grids
require correct transformation of stiffness, displacement, loads and stresses,
and validated interpolation across nonmatching faces.

Recover each body's residual forces before tied-node reduction is discarded,
so interface reactions remain attributable. Define how shared patch edges and
multiway junctions are handled; initially refuse ambiguous joint attribution.
Sum interface forces and moments using a common origin, with equal-and-opposite
results on the two sides. Do not derive a joint force from a peak stress sample.

Check connectivity and mechanical rank: a connected graph alone does not prove
that all rigid-body modes are restrained. Diagnose unsupported components,
redundant/conflicting constraints and unresolved mechanisms. Bound total cells,
degrees of freedom, memory and iterations for the assembled problem; per-part
budgets alone do not bound an assembly. Never publish an accepted analysis after
nonconvergence or invalid numerical output.

### Stage 2: compliant connections

Add explicit stiffness laws between bounded interface patches. Distribute
connector forces to the patches rather than loading a single voxel node.
Rotational connectors require a defined reference-frame coupling to the solid's
translational degrees of freedom; avoid unintentionally rigidifying an entire
part. Test released directions and mechanism diagnostics.

Fastener metadata may help parameterize a model, but bolt stiffness is not the
whole joint stiffness. Printed bearing regions, washers and clamped parts can
control compliance. Report assumed stiffness and perform sensitivity checks.
Connector force estimates do not establish thread pull-out or bearing capacity.

### Stage 3: contact and optional solver adapters

Contact requires determining which interfaces are active as bodies deform.
Friction and preload add further state and load-step requirements. Treat this
as a separate nonlinear capability, not a flag on the bonded solver.

Evaluate an optional established solver adapter before implementing general
contact internally. CoScad can retain responsibility for instance identities,
geometry, boundary selection, material/load definitions, validation and reports.
Qualify export/import semantics, solver versions, convergence, cancellation and
result provenance on representative fixtures before accepting that path.

Established implementations distinguish bonded, frictionless and frictional
interfaces and several enforcement methods; see
[MOOSE mechanical contact](https://mooseframework.inl.gov/docs/site/source/constraints/MechanicalContactConstraint.html).
Adhesive damage belongs to a later traction-separation model, not the perfect
bond; see [MOOSE cohesive-zone modeling](https://mooseframework.inl.gov/syntax/Physics/SolidMechanics/CohesiveZone/index.html).
These references motivate the capability separation, not a dependency decision.

## Reports and user workflow

Use a versioned assembly report while preserving `coscad.stress/1` for existing
single-part consumers. Include:

- Source, resolved instance paths/poses, material values, print orientations,
  load case, solver version/settings, mesh sizes and approximation warnings.
- Global displacement, reactions, residual and strain energy.
- Per-part displacement and stresses, with directional strength checks only
  where orientation and material data support them.
- Per-interface selected areas, force/moment resultants, connection law and its
  assumptions. Compliant joints later add relative displacement and rotation.
- Explicit unsupported or unchecked failure mechanisms. A body-strength pass
  must not be presented as a verified joint-strength pass.

Provide a preview of selected supports, loads and interfaces before relying on
results. Deformed-shape and stress views are useful follow-up presentation work;
any displacement amplification must be labeled. Keep outputs separate from
manufacturing manifests and do not make analysis trigger printer dispatch.

Detailed declaration grammar should follow the region and instance contracts.
The proposed command is an intended interface, not syntax available today.

## First milestone and acceptance gates

Implement a two-part bonded cantilever, then a shelf on two brackets. Before
calling this a supported assembly-analysis path, require:

1. **Single-part regression:** existing displacement, reaction and energy
   fixtures remain within their established tolerances after refactoring.
2. **Perfect-bond equivalence:** a matching-mesh beam split into identical-
   material parts agrees with the corresponding single-piece discretization
   in displacement and energy within solver/numerical tolerance.
3. **Material and load sharing:** a two-material axial bar agrees with the
   series-compliance formula. Symmetric brackets share a symmetric load equally;
   changing one bracket's stiffness redistributes load consistently with an
   independent solution, rather than retaining an assumed 50/50 split.
4. **Equilibrium:** applied loads and support reactions balance in force and
   moment; each interface's two resultants are equal and opposite. Declare
   normalized tolerances before running fixtures and handle near-zero resultants
   with absolute tolerances.
5. **Constraint failures:** floating parts, insufficient support, empty patches,
   unintended gaps, conflicting ties and unsupported interface orientations
   produce actionable errors. Valid contact-free nearby bodies stay separate.
6. **Refinement:** on named nonsingular fixtures, successive mesh refinements
   change selected displacement and interface resultants by less than 5%.
   Peak stresses need a separate convergence study; singular corner peaks cannot
   pass merely because displacement converges.
7. **Independent comparison:** compare the same materials, supports, patch areas
   and loads with an independent FEA implementation, explaining discretization
   differences rather than only comparing final pass/fail verdicts.
8. **Physical comparison:** measure deflection of a representative assembly
   under known loads, recording dimensions, material/print settings, actual
   connection construction, fixture compliance and measurement uncertainty.
   Set an error target before the experiment; do not tune and validate on the
   same measurement alone. Until this passes, describe results as numerically
   validated idealized assembly analysis, not calibrated printed strength.

Stage 2 adds spring/beam analytical fixtures, released-mode tests and stiffness
sensitivity. Stage 3 adds opening/closing, sliding, preload and load-step
convergence cases. Each stage retains all applicable earlier gates.

## Priorities and open decisions

First settle bounded region selection, instance identity and mechanical
connection semantics. Then implement matching-interface bonded Hex20 assembly
and the numerical fixtures. Physical validation should inform whether compliant
joints or material calibration produces the larger improvement next.

Open decisions include declaration grammar, the first supported interface
orientations, nonmatching-mesh coupling, material overrides, solver scaling and
external-contact solver choice. None requires switching the geometry backend.
The largest uncertainty remains how closely the chosen material and connection
models represent the manufactured assembly.
