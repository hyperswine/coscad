# Bonded Hex20 experiment

This is the first implementation step of
[the assembly FEA proposal](../../docs/2026-11-09-ASSEMBLY-FEA-PROPOSAL.md).
It is a coupled linear solve of explicitly bonded bodies, not yet
`coscad stress model.assemble` and not a printed-assembly strength assessment.

From the repository root:

```sh
cabal build exe:coscad-experiments
FEA_EXE="$(cabal list-bin exe:coscad-experiments)"
"$FEA_EXE" bonded experiments/bonded/split-cantilever.json > /tmp/split-result.json
"$FEA_EXE" bonded experiments/bonded/two-material-bar.json > /tmp/bar-result.json
COSCAD_RENDER=0 cabal test coscad-test --test-options=--experiments-only
```

Run the executable directly for JSON-only stdout; `cabal run` may add build
messages. Invalid input/nonconvergence exits 1 with a diagnostic on stderr and
no result JSON. Shell redirection itself truncates an existing output file;
choose a new destination when preserving a prior result matters.

## Model and format

`coscad.bonded-problem/1` is a strict scratch format, not proposed assembly
language syntax. Unknown/duplicate fields are errors. Lengths are mm, forces N,
Young's modulus/stresses MPa. Positions and load vectors use world axes.

- `bodies`: unique instance `name`, `origin_mm`, positive `cell_mm`, integer
  `counts` `[nx,ny,nz]`, `young_mpa` and `poisson`. Each describes a completely
  filled rectangular grid. All bodies share a cell size and aligned origins;
  no rotations, overlap or fractional occupancy are supported.
- A patch is `{"body":"root","faces":[[i,j,k,axis,sign], ...]}`. Indices
  identify a local cell; axis 0/1/2 means X/Y/Z and sign is -1/+1. Faces must
  be unique exterior faces in one plane with the same outward normal. This
  explicit list bounds the selected area; it does not select a whole datum plane.
- `bonds`: unique `name`, patch `a` and patch `b`. They must belong to different
  bodies, have opposing normals and match face-for-face. No proximity bonding,
  gaps, interpolation or penalty stiffness. Duplicate/multiway bonds sharing
  nodes and supports touching bonded nodes are refused because their reaction
  attribution is not implemented.
- `supports`: patches fully clamped in all three displacement directions.
  Each connected component must be supported. Full-face ties constrain relative
  rigid motion; this check is not a general rank test for future released joints.
- `loads`: `patch` and `force_n`, a total uniform traction force distributed
  with consistent quadratic face weights. Multiple declarations add. A load
  on an interface is allowed; external load is subtracted before its interface
  reaction is reported. Entirely clamped or cancelled loads are refused.
- Optional `tolerance` defaults to 1e-9 (range 1e-12..1e-4); `iterations`
  defaults to 20000 (range 1..50000). The accepted residual is recomputed.

The initial experiment is limited to 32 bodies, 64 declarations of each kind,
1000 total cells and 60000 reduced displacement degrees of freedom. These are
size guards, not a hard wall-clock/memory isolation guarantee. Poisson ratios
are restricted to 0..0.45, as in the existing solver.

## What the fixtures prove

`split-cantilever.json` represents a 20 x 4 x 4 mm beam split at x=10, with
2 mm cells, E=2000 MPa, nu=0.3, a clamp at x=0 and a total -1 N Z force at
x=20. Tests compare every node displacement and cell-centre stress, total
energy and reactions to the matching single-piece discretization. The splice
has 16 mm² area and 21 quadratic face nodes per side.

`two-material-bar.json` applies +1 N in X with nu=0 on both halves, E=2000
and 1000 MPa respectively. Its expected tip extension is
`10/(2000*16) + 10/(1000*16) = 0.0009375 mm`, with uniform axial stress
`1/16 = 0.0625 MPa`. It checks that each body keeps its material and that
series compliance, energy and interface balance are correct.

The test tier also checks translation of moments, instance ordering,
independently supported touching bodies, and malformed or unsupported inputs.

## Results and interpretation

`coscad.bonded-result/1` includes per-body node displacements and cell stresses,
material/mesh data, per-body and global energy/support reactions, applied loads,
explicit supports/loads/bonds, convergence and interface forces/moments.

Interface `on_a` and `on_b` are the forces and moments **applied to the named
body by the other body**. They are recovered from each body's unreduced
internal nodal forces minus its external loads, not from stress multiplied by
area. Moments are about the world origin. The report includes interface and
global equilibrium residuals; they should be small, not rounded to zero.

No safety-factor verdict, contact, preload, compliant joints, print orientation,
material anisotropy or physical validation is provided. Peak stress remains
mesh dependent. The matching-beam comparison validates coupling against the
existing solver; it is not an independent solver or physical comparison.

## Shelf validation

`shelf-symmetric.json` and `shelf-soft-right.json` add a shelf bonded to two
rectangular cantilever arms. Both apply 1 N downward; the latter reduces the
right arm's modulus from 2000 to 500 MPa. Clamps lie behind the shelf and do
not touch the tied faces. The coarse fixtures run in the ordinary Haskell
regression tier, including mirrored-stiffness and load-scaling checks.

The optional independent check runs both cases at 2, 1 and 2/3 mm cells using
scikit-fem's Hex20 implementation and SciPy's direct solver:

```sh
python3.12 -m venv /tmp/coscad-fea-reference
/tmp/coscad-fea-reference/bin/python -m pip install -r experiments/requirements-fea.txt
cabal build exe:coscad-experiments
FEA_EXE="$(cabal list-bin exe:coscad-experiments)"
/tmp/coscad-fea-reference/bin/python scripts/compare-bonded-reference.py \
  --exe "$FEA_EXE" --output /tmp/shelf-comparison.json
```

A failed reference or final refinement threshold exits nonzero. The report
retains coarse-to-medium failures even when the medium-to-fine gate passes.
To regenerate the two committed coarse inputs into a scratch directory:

```sh
python3 scripts/bonded_shelf.py --output-dir /tmp/bonded-shelf-fixtures
```

See [the validation record](../../docs/2026-11-10-BONDED-SHELF-VALIDATION.md)
for measured load sharing, refinement, independent agreement and evidence.
These fixtures establish numerical behavior for ideal perfect bonds; they do
not validate physical prints, joint failure or peak-stress convergence.
