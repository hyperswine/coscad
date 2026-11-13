# Enhancing FEA for real assemblies

Status: proposal with a working native prototype, 2026-11-12. The [wing–saddle experiment](../models/bolted-glider-balanced/structural/README.md#assembled-wing-and-fuselage) documents the implemented subset and how to reproduce it. It follows the
[assembly FEA proposal](2026-11-09-ASSEMBLY-FEA-PROPOSAL.md) and its three
landed pieces: the [bonded experiment](2026-11-09-BONDED-FEA-EXPERIMENT.md),
the [shelf validation](2026-11-10-BONDED-SHELF-VALIDATION.md) and the
[assembly bridge](2026-11-10-ASSEMBLY-ANALYSIS-INTEGRATION.md). That proposal
set the stages (bonded, compliant joints, contact, failure models) and their
acceptance gates. This one says what to do next now that stage 1 exists and
has met a real model, and in what order.

## Where it stands

The bonded solver is numerically sound within its contract: it agrees with an
independent scikit-fem assembly to about 2e-11 on the shelf fixture and passes
the medium-to-fine refinement gate. The contract is narrow, though, and the
first real model fell outside it.

The [glider structural screen](../models/bolted-glider-balanced/structural/README.md)
(2026-11-11) records what happened:

- `coscad stress model.assemble --bonded` rejected the wing. The bridge accepts
  only bodies whose every face is orthogonal and grid-aligned, with fully
  filled cells. A lofted wing can never satisfy that at any cell size.
- The single-part solver did analyse the wing, at 2,573 cells (4 mm) and 5,770
  cells (3 mm). The assembly path caps the whole assembly at 1,000 cells.
- Those runs took 50 s and 154 s. Deflection moved 9.5% between them, so the
  result was not yet mesh-converged, and a finer run was not practical.
- The model treated the wing as solid isotropic PLA. The printed wing is two
  walls with 8% gyroid infill.
- Loads and supports were artificial (clamped root end, 1 N on the tip end),
  because pressure loads and bolt or saddle load transfer cannot be expressed.

So five things limit assembly analysis today, in this order of how soon they
bite: accepted geometry, problem size and speed, how the printed material is
represented, how joints are represented, and how analyses are declared.

## Goals and non-goals

Goal: `coscad stress` on an assembly of real printed parts (curved bodies,
flat mating faces, bolted joints) gives deflection and load-path answers that
have been checked against at least one bench measurement.

Non-goals for this proposal:

- Strength or safety verdicts for assemblies. Peak stress stays mesh dependent
  on a voxel grid; the existing warnings stand.
- Buckling, fatigue, creep, impact, large deflection.
- A general nonlinear contact solver written in Haskell.
- A change of geometry kernel. The [backend review](2026-11-09-BACKEND-REVIEW.md)
  conclusion stands: none of this needs OCCT.

## Work items

### A. Bench calibration first

No physical measurement exists for any stress result. Before widening the
solver, measure two things:

1. One printed part under a known small load with a measured support, for
   example the glider wing on its saddle, or a plain printed beam. Record
   dimensions, walls, infill, filament, load, deflection and the uncertainty
   of each.
2. One bolted joint of the kind the models use (M5 through two printed
   plates), loaded so that joint flexibility is visible in the deflection.

These answer two questions that decide the rest of the order: how far the
solid isotropic assumption is from a walls-plus-infill print, and whether
joint compliance or material error is the larger part of the gap. Set the
error target before measuring, and do not tune and validate on the same
measurement, as the earlier proposal already requires.

This item costs no code and should not wait for the others.

### B. Accept curved bodies with flat interfaces

The single-part solver already represents curved boundaries through partly
filled cells whose stiffness is scaled by their solid fraction. The assembly
bridge refuses them wholesale. Narrow the refusal to where it matters:

- Allow partly filled cells in a body, exactly as the single-part path does,
  including its volume-deviation report.
- Keep the strict requirement only on the cells that touch a bonded patch, a
  support or a load region: those faces must be planar, grid-aligned and fully
  filled, and the two sides of a bond must still match face for face.
- Keep rejecting, with the same explicit errors, bodies whose mating faces are
  not on the common grid after placement (arbitrary rotations), sub-cell
  features on an interface, and overlapping bodies.

This covers bolted plates and railed panels with grid-aligned, fully covered patches. The balanced glider needs the frame and hole checks below. It
does not cover two bodies meeting at an arbitrary angle; that needs
interpolated (non-matching) ties, which change the reduced system and are
deferred until a design needs them.

Acceptance: the existing split-beam and shelf fixtures are unchanged to their
current tolerances; a new fixture with a curved body bonded on a flat face
agrees with the independent scikit-fem comparison on a body-fitted mesh within
a tolerance stated beforehand (it will be a discretization-level agreement,
not 1e-11); the single-part and one-body assembly results for the same curved
part agree to solver tolerance.

### C. Size and speed

Limits today, in `Stress/Assembly.hs` and `Stress/Bonded.hs`: 32 bodies, 1,000
occupied cells in total, 1,000 bounding-box cells per body. The single-part
path has no such cap and solves about 5,800 cells in about 150 s.

Step 1 is to measure, not to choose a method. Profile a 50k and a 100k unknown
solve and split the time into voxelization, element assembly, matrix build
(the CSR is built from Haskell lists) and the preconditioned CG iterations.
The iteration counts already reported (704 for 80k unknowns on the shelf
bracket) suggest the solve dominates, but that has not been measured.

Candidates, to be picked by that profile:

- Raise the assembly cap to what the single-part path already handles, and
  replace the per-body bounding-box cap with a bound on occupied cells and
  memory. This is needed regardless.
- A stronger preconditioner. The grid is regular and every element is the same
  cube, which suits geometric multigrid, as the stress command doc already
  notes. Quadratic elements and partly filled boundary cells make this less
  simple than it sounds; a two-level scheme (quadratic to linear on the same
  grid, then grid coarsening) is the likely shape. Treat it as an experiment
  with its own gate, not a committed design.
- Matrix-free application of the single element matrix, which removes the CSR
  build and most of the memory.
- Parallel CG kernels. The runs so far used `GHCRTS=-N2`.

Acceptance: identical results to the current solver within its tolerance on
all fixtures; a stated time target on the glider wing at 3 mm and at 2 mm,
set after the profile; nonconvergence remains an error.

### D. Represent the printed material

The largest known modelling error is treating a two-wall, 8% infill print as
a solid. Two levels, the second only if item A says it is needed:

1. Effective modulus. Let a part or instance declare an effective Young's
   modulus calibrated from a bench test of that print recipe. This is a
   material-table entry and a provenance note in the report, nothing more.
2. Skin and core. Give exterior cells the wall material and interior cells a
   reduced infill modulus. The voxel grid makes the classification easy, but
   two 0.4 mm walls are far thinner than a practical cell, so the skin has to
   be homogenized into the boundary cells. How to do that without losing the
   bending stiffness the skin provides is an open question; do not implement
   it before a measurement shows level 1 is not enough.

Print-layer direction is already exported per instance. Use it for the
across-layer tension check in assemblies (`max(0, n·σ·n)` with the transformed
normal), and report the check as unavailable when the orientation is unknown,
as the earlier proposal specifies. This is a strength-direction check, not
anisotropic stiffness.

### E. Compliant joints

This is stage 2 of the earlier proposal, unchanged in substance: explicit
springs or beam connectors between bounded patches, with stated translational
and rotational stiffness and released directions, forces distributed over the
patch and not applied to one node. Linear connectors preserve symmetry. The reduced system is positive definite only if the supports and connectors restrain every rigid mode. Reject an underconstrained release, and test that a released direction carries zero force in a separately stabilized fixture.

What is new is the ordering and the source of numbers. Do it after A, because
the joint bench test supplies the first stiffness values and shows whether a
bonded model is already close enough for the bolted designs. Fastener
declarations in `.assemble` may propose where connectors go; they must not set
stiffness silently.

Acceptance: the stage 2 gates already written (spring and beam analytical
fixtures, released-mode tests, stiffness sensitivity), plus agreement with the
joint bench test within the target set in A.

### F. Declaring an analysis

Today an assembly analysis is a hand-written JSON file of rectangles in world
coordinates. Three additions make it usable on a real model:

- Region preview. Render the selected support, load and bond faces on the
  assembly, as an image and as the face lists already in the report, before
  solving. The earlier proposal asked for this; it is the cheapest protection
  against analysing the wrong patch.
- Regions from frames. A mating frame names where two parts meet. Offer the
  coplanar contact patch at a mate as a candidate bond region that the
  analysis file must confirm by name. `mate` and `fixed` still never create a
  bond or a support on their own.
- Distributed loads beyond a uniform total force: a pressure on a selected
  face set (needed for a lift distribution), and gravity from the mass
  analysis densities.

### G. Contact through an external solver

Separation, slip and bolt preload are nonlinear. The earlier proposal says to
evaluate an adapter to an established solver before writing contact
internally, and that remains the recommendation. CoScad keeps instance
identity, geometry, region selection, materials, loads, validation and the
report; the adapter exports a problem and imports a result with solver
version and convergence recorded.

Do this only when a design decision actually depends on slip or separation.
The solver choice is open. CalculiX is a reasonable first candidate to
evaluate; it has not been tried against these models.

## Order

| Step | Item | Why here |
|---|---|---|
| 1 | A. Bench calibration | No code; decides the weight of D and E |
| 2 | B. Curved bodies, flat interfaces | Enables a curved-body control with a grid-aligned patch |
| 3 | C. Caps, profile, then speed | B is of little use at 1,000 cells |
| 4 | F. Region preview and frame-derived candidates | Needed as soon as real models run |
| 5 | D level 1. Effective modulus | Small; uses the result of A |
| 6 | E. Compliant joints | Only if A shows joints dominate the error |
| 7 | D level 2, non-matching ties, G | Each waits for a design that needs it |

Every step keeps all earlier acceptance gates, including the single-part
regression and the independent comparison.

## First glider experiment: right wing and fuselage

On 12 November the current CLI completed the checked-in shelf case and wrote
its bonded report. A separate 4 mm one-body probe of the actual right wing
exited 1 at the orthogonal-boundary check. That probe used the print frame,
isotropic E = 3500 MPa and ν = 0.36, and empty support, load and bond lists.
It exercises geometry admission only. The 3 mm and 4 mm single-wing values
above are archived measurements. Neither run establishes assembled stiffness.

1. **Make the placement and regions inspectable.** Build a two-instance
   structural fixture from the ear-free wing and fuselage. Use the rigid
   placements in [assembly.py](../models/bolted-glider-balanced/assembly.py).
   The right saddle is flat in its own frame, tilted in flight coordinates by
   the 6° dihedral and 2.5° incidence transforms. As a first experiment orient
   the *entire analysis grid* to the right saddle, keeping the parts' relative
   pose and transforming loads, supports and reported displacements consistently.
   The two right M5 axes in saddle coordinates are at (15, 26.4) and
   (15, 74.4) mm. Preview those holes and the two actual bearing footprints.
   A single rectangular patch across both holes would tie empty space. Start
   with explicitly selected fully covered patches away from the holes. Report
   their areas, locations and gaps. Repeat with the opposite panel only after
   testing how off-grid ties behave.
2. **Test geometry and mechanics separately.** First use a curved coupon with
   grid-aligned flat ends so both solvers can use identical occupied cells,
   fill fractions, face loads and constraints. The bonded solver currently
   passes unit weights to both matrix assembly and internal-force recovery.
   Carry the same per-cell fill through both paths and compare displacement,
   force and moment balance. A full sampled fraction (27 of 27 points) cannot
   certify an unperforated face. Include a small drilled patch that sampling
   could miss and require geometric rejection. Retain a bound on candidate
   grid work before voxelization as well as a budget for occupied cells and
   solver memory. Thin swept bodies can have large, mostly empty bounds.
3. **Run a reproducible joint comparison.** Restrain the fuselage over the
   measured bench fixture footprint. Apply a known transverse force over a
   marked loading pad at a measured span station. Measure tip displacement
   and root rotation relative to the fuselage, recording the gauge positions,
   fixture motion, repeatability and unloading offset. Run the same case with
   bonded patches and then with two calibrated M5 connectors distributing
   force over the head and nut bearing areas. Report each connector force,
   total reactions and moments about the fixture. Use a second load station
   or a held-out specimen for validation. A tie transmits tension at the
   saddle; deciding whether it opens requires the contact work in G.
4. **Expand only after this comparison.** If a single rigid grid orientation
   cannot represent the required patches, test independent body meshes and
   interpolated ties with constant-displacement, rigid-rotation and
   force/moment transfer fixtures before using them on the glider. Then add
   the left wing and the tail assembly with its two M3 joints. For a
   distributed lift case record the integrated pressure force and moment,
   gravity and the chosen equilibrium or inertial constraints. Keep bolt
   bearing, shell buckling and flight qualification outside the bench result.

Bench data collection and the curved coupon can proceed in parallel. The
most useful first assembled result is a load–deflection curve and root
rotation for the same fixture the owner can measure.

## Open decisions

- Whether partly filled cells may touch a bonded patch at all, or whether the
  first version requires a full layer of whole cells behind every interface.
- The time and size targets for item C, to be set from the profile.
- Whether an effective modulus belongs to the material table, the part, or the
  instance, and how its provenance (which bench test) is recorded.
- Declaration syntax: keep analysis in a separate JSON file, or move regions
  and load cases into `.assemble`. The earlier proposal left this open too.
- Which external solver, if any, to qualify for contact.

## What this does not change

Results remain small-strain linear elasticity on a voxel grid. Until the bench
comparison in item A passes its target, assembly results should still be
described as numerically validated idealized analysis, not calibrated
printed-assembly prediction.
