# First bonded assembly FEA experiment

Status: implemented experimental foundation, 2026-11-09. This completes the
initial two-body matching-interface experiment in the
[assembly FEA proposal](2026-11-09-ASSEMBLY-FEA-PROPOSAL.md), not its full first
milestone or promotion gates. The shelf-on-brackets, independent assembly
solver comparison and physical measurements remain future work.

## Changes

- `Stress/Hex20.hs` separates sparse element assembly, element-force recovery
  and stress recovery from the single-part wrapper. Both paths use the same
  element formulation and solver; single-part interfaces remain unchanged.
- `Stress/Bonded.hs` adds per-instance nodes, per-body material matrices,
  exact displacement identification at matching planar interfaces, and one
  coupled linear solve. Coincident untied nodes stay distinct.
- Each body's internal nodal forces are retained for recovery of support and
  interface force/moment resultants, including subtraction of external loads.
- `coscad-experiments bonded PROBLEM.json` exposes a strict, bounded JSON
  experiment. It does not add `.assemble` syntax or change `coscad stress`.
- Split-beam and two-material-bar fixtures and a regression tier cover
  numerical agreement, equilibrium and rejection of unsupported inputs.

See [the experiment guide](../experiments/bonded/README.md) for commands,
schemas, supported interfaces, limits and result conventions.

## Local validation

The final focused experiment/stress suite passed 369 checks with no failures,
including existing single-part stress checks and external loading directly on a
tied interface. The broader suite passed 1458 checks with `COSCAD_RENDER=0`
before the final interface-load regression and recovery/cache cleanup; the
focused suite was rerun after those changes. The geometry/oracle tier was not
run for this solver change. Direct CLI checks also confirmed valid JSON output
for both fixtures and exit 1 with no result JSON for gaps, floating bodies and
empty interfaces. The split beam agrees with its single-piece
discretization within 1e-9 mm at every node, 1e-7 MPa at cell centres, and 1e-8
relative strain energy. These are explicit test thresholds, not claimed exact
arithmetic. The two-material bar agrees with the analytical series-compliance
solution within 1e-8 relative displacement/energy.

A direct run of `split-cantilever.json` produced 963 reduced displacement DOFs,
117 iterations and a recomputed relative residual of 5.97e-11. Maximum nodal
displacement was about 0.0638041 mm; strain energy was 0.0315482 N mm. Global
force/moment balance and splice force/moment balance residual components were
below 3e-11 in their respective units (N and N mm).

These are local numerical checks of the current source build. They are not
hosted CI, a new independent solver comparison or physical printed-part proof.
No geometry-kernel migration was needed.

## Next boundary

The first interface contract is deliberately narrow: common aligned grids,
fully filled cells and matching planar perfect bonds. It refuses gaps,
nonmatching meshes, overlapping bodies, floating components, ambiguous shared
joint nodes and supports on tied nodes. It makes no strength claim.

Next, validate a shelf-on-two-brackets fixture and mesh refinement, then decide
region/instance syntax for `.assemble`. Broader regions, nonmatching coupling,
compliant joints and contact retain the proposal's separate acceptance gates.
