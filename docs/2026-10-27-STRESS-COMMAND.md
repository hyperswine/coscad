# `coscad stress`: part stress and deflection in Haskell

Status: implemented 2026-10-27 on top of the
[FEA experiments](2026-10-26-FEA-BACKEND-EXPERIMENTS.md). This is the first
user-facing stress command. It answers the design-loop questions from the
[intent note](2026-10-17-FEA-AND-BACKEND.md): will this part hold the load,
how far does it bend, and is the stress across the printed layers safe. It
needs nothing beyond the compiler and whichever geometry backend renders the
part: no Python, no Gmsh, no external solver.

## Writing the analysis into the part

Three kinds of line sit next to the geometry in a `.coscad` file. The compiler
and both geometry backends ignore them; `coscad stress` reads them.

```
support bot                   // clamp every exterior face on the part's bottom plane
support @foot bot             // clamp the -z faces in the plane through datum foot
load top 0 0 -20              // 20 N total, spread over the faces on the top plane
load @tip 0 0 -20             // 20 N on the surface node nearest datum tip
load @tip top 0 0 -20         // 20 N spread over the +z faces coplanar with datum tip
stress material=pla safety=2 cell=1.5
```

Forces are in newtons, in world axes, and are totals. A face is named by one
of the six bounding-box anchors `top bot lft rt fwd bak`; edges and corners are
not faces. A datum (`|> datum name ...` in a pipeline) names a plane through a
point, so `support @foot bot` holds every face with a −z normal lying in that
plane, not only the datum's own feature. A load at a bare datum is a point load
on one node, which is a stress singularity: judge the deflection there, not the
stress. Settings may repeat; the last value wins. `material` defaults to `pla`,
`safety` to 2, and `cell` to a size derived from the part (below).

Materials (modulus, Poisson ratio, in-plane strength, across-layer strength):

| name | E MPa | ν | strength MPa | across layers MPa |
| --- | --- | --- | --- | --- |
| pla | 3500 | 0.36 | 50 | 35 |
| petg | 2100 | 0.38 | 45 | 32 |
| abs | 2200 | 0.35 | 35 | 25 |
| asa | 2300 | 0.35 | 40 | 28 |
| pa | 1800 | 0.40 | 50 | 38 |
| tpu | 30 | 0.45 | 30 | 25 |
| aluminium | 69000 | 0.33 | 240 | 240 |
| steel | 200000 | 0.30 | 250 | 250 |

These are typical datasheet values, not measurements of a spool or a print.
Across-layer strength is applied to the largest tensile stress along the part's
z axis, i.e. layers are assumed stacked along z, the way parts are modelled for
printing.

## Running it

```sh
coscad stress examples/stress/shelf-bracket.coscad            # default backend
coscad stress part.coscad -o out/part_stress.json --full      # every node and cell
coscad stress part.coscad --backend=openscad                  # render with OpenSCAD instead
```

The part is rendered with the selected geometry backend, the mesh is written
next to the report (`part_stress.stl` and `.scad`), analysed, and the report
`part_stress.json` (schema `coscad.stress/1`) is written. The summary printed
afterwards gives the material and mesh, the largest displacement and where,
the peak von Mises stress and where (plus the largest cell-centre value, which
is the smoother, lower bound), the largest across-layer tension, and both
safety factors against the requested one. Exit status 0 means the requested
safety factor is met, 2 that it is not, 1 that the analysis could not run
(no support, unknown material, a mesh that falls apart at this cell size).

## How it works

1. **Mesh.** The rendered triangle mesh is voxelized on a cube grid. Each cell
   is sampled at 27 points by ray parity along grid columns; the sampled solid
   fraction is kept, cells below 20% are dropped, and a cell's stiffness is
   scaled by its fraction. A wall that is not a whole number of cells thick is
   therefore a softer boundary cell rather than being rounded to the nearest
   cell. The voxel volume is compared with the mesh volume and a deviation
   above 5% is reported.
2. **Cell size.** By default the typical wall thickness is estimated as 2V/A of
   the mesh and the cell is that thickness divided by a whole number, at least
   two, chosen so the grid holds about 2,500 cells. `cell=` overrides it.
3. **Element.** 20-node serendipity hexahedra (quadratic displacement,
   3×3×3 Gauss) with one element matrix per problem, assembled into a CSR
   matrix. Quadratic elements avoid the bending stiffness of plain 8-node
   bricks, so two cells through a wall already give useful deflections.
4. **Solve.** Conjugate gradients preconditioned by symmetric Gauss–Seidel,
   relative tolerance 1e-7, with the true residual recomputed before a result
   is accepted. Nonconvergence is an error, never a number.
5. **Results.** Displacements at nodes; stresses at the 27 Gauss points and the
   centre of every cell, with the full modulus (material stress); reactions and
   their moment about the origin; strain energy.

## Checks

- The 20-node element matrix is symmetric and carries no force under rigid
  translations; a 2×2×2 block under uniform traction returns the exact uniform
  stress at every centre and Gauss point.
- The 20×4×4 mm cantilever matches the axial bar formula to 1e-12 and the
  Euler–Bernoulli tip deflection within 1.2–1.5% at every cell size from 4 mm
  to 1 mm (the remainder is transverse shear and the clamp, which the beam
  formula omits).
- `scripts/compare-hex20-reference.py` assembles the same cantilevers with
  scikit-fem's 20-node element and SciPy's direct solver: tip displacement and
  strain energy agree to 6e-11 relative at 1, 2 and 3 cells per side. This is
  an optional validation tool; the command does not need it.
- The validated Tet10 solver, run on a Haskell Kuhn split of the same 1 mm
  voxels (`coscad-experiments voxel-mesh`), gives the same tip deflection to
  five digits (0.063424 against 0.063423 mm).

## Worked example and what refinement does

`examples/stress/shelf-bracket.coscad` is a 60 mm wide PLA L-bracket with 6 mm
walls, clamped on its base, with 20 N spread over the top of a 36 mm arm. The
default cell snaps to 2 mm because every face of the part lies on a 2 mm
plane, so the geometry is exact; coarser cells misrepresent the 6 mm walls
through partially filled cells and the deflection moves accordingly:

| cell | solid cells | unknowns | iterations | tip deflection | peak von Mises (Gauss / centre) | time |
| --- | --- | --- | --- | --- | --- | --- |
| 3 mm | 2,060 | 33,396 | 424 | 0.085 mm | 0.93 / 0.53 MPa | 25 s |
| 2.6 mm | 3,059 | 48,270 | 515 | 0.113 mm | 1.03 / 0.68 MPa | 25 s |
| 2 mm (default, exact fit) | 5,130 | 79,842 | 704 | 0.145 mm | 0.93 / 0.59 MPa | 70 s |

Times are on an M-series Mac while another build was running. The lesson is
the one the limits section states: a wall needs whole cells, or at least two
cells, before the deflection is trustworthy, and the command's default tries
to arrange that. Safety factors here are above 45 either way, so the verdict
does not change, but a marginal part needs the refinement check.

A 10 mm round PETG post, 40 mm tall, clamped at its base with 5 N sideways on
its top face: at 1 mm cells the tip moves 0.103 mm against 0.1035 mm from the
cantilever formula; the Gauss-point peak of 2.5 MPa and the cell-centre peak
of 1.6 MPa bracket the formula's 2.0 MPa root stress. The same load applied as
a point at a datum gives 0.107 mm and a flagged 9.5 MPa singularity at the node.


## Limits

Small-strain linear elasticity of a homogeneous isotropic solid. Infill, layer
bonding, contact between parts, friction, buckling, fatigue, creep and large
deflection are not modelled. Supports are fully clamped. Geometry is a voxel
grid: peaks at sharp corners and on stair steps are mesh dependent, so compare
two cell sizes before trusting a peak, and keep cells at most half the thinnest
wall. Slender parts converge slowly; a stronger (multigrid) preconditioner is
the next performance step. `plan` images and assemblies are untouched: this is a
single-part command.

## Files

- `src/Coscad/Stress/Spec.hs` directive data; `Parser.hs` parses the lines.
- `src/Coscad/Stress/Voxel.hs` voxelizer; `Hex20.hs` element, assembly, solve;
  `Sparse.hs` CSR and the preconditioned solver; `Material.hs` the table;
  `Part.hs` region selection, judgement and the report; `Command.hs` the CLI.
- `examples/stress/shelf-bracket.coscad` worked example; `test/Tier/Stress.hs`.
- `coscad-experiments hex20 axial|bend N` and `voxel-mesh MODEL CELL OUT.json`
  keep the numerical experiments reproducible without Gmsh; the earlier
  `scripts/mesh-fea-tetra.py` and its Gmsh pin are gone.
