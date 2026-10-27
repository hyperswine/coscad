# Try FEA and native geometry

Run the commands from the repository root. The
[validation record](2026-10-26-FEA-BACKEND-EXPERIMENTS.md) documents numerical
checks, accepted geometry, limits and the release gates as they stood on
2026-10-26; since [2026-10-27](2026-10-27-NATIVE-DEFAULT.md) the native kernel
is the default geometry backend of a normal build, and the FEA workflows remain
opt-in experiments.

## Part stress with `coscad stress`

The user-facing command is documented in [STRESS-COMMAND](2026-10-27-STRESS-COMMAND.md):

```sh
coscad stress examples/stress/shelf-bracket.coscad
```

## Beam or rectangular CAD analysis

No native kernel or Python installation is needed for this path:

```sh
cabal build exe:coscad-experiments
cabal run coscad-experiments -- beam axial 4 2 2
cabal run coscad-experiments -- cad experiments/cad/l-bracket.coscad \
  experiments/cad/l-bracket-fine.json
```

The CAD path accepts exact grid-aligned rectangular CSG. Use the coarse,
medium and fine bracket condition files to compare displacement as the mesh
is refined. A feature that the declared grid cannot resolve is an error.

## Native STL, assembly checks and manufacturing

The native CSG backend needs a C++17 compiler, CMake and pkg-config as well as
Cabal/GHC. The builder fetches and verifies the pinned Manifold and Clipper2
checkouts (`native-kernel.lock.json`) and builds them statically; the Cabal
`manifold` flag is on by default, so an ordinary build links the kernel once
pkg-config can see it:

```sh
python3 scripts/build-manifold.py
export PKG_CONFIG_PATH="$PWD/.native/manifold-3.5.2/install/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
cabal build all
COSCAD_BIN="$(cabal list-bin exe:coscad)"
FEA_BIN="$(cabal list-bin exe:coscad-experiments)"
mkdir -p /tmp/coscad-fea-demo
"$COSCAD_BIN" doctor
"$COSCAD_BIN" stl examples/assemble/plan/flat90.coscad -o /tmp/coscad-fea-demo/flat90.stl
"$COSCAD_BIN" check examples/assemble/ball/ball.assemble
```

`stl`, `check` and `next` render natively by default in that build;
`--backend=openscad` renders through OpenSCAD instead, and `--backend=native`
(alias `native-experimental`) names the default explicitly. `coscad next`
records the backend used in its manifest. Neither command dispatches a printer
job. Unsupported shapes fail explicitly; there is no silent OpenSCAD fallback.
Round 2D offsets of a single outline and lofts are supported natively; an
offset inside a loft profile and offsets of composite 2D shapes are still
explicit errors. Planner instruction images continue to use OpenSCAD. A build
made with `cabal build -f-manifold` has no kernel and defaults to OpenSCAD.

## Curved or holed CAD analysis

Superseded on 2026-10-27 by `coscad stress`, which voxelizes any rendered part
in Haskell and needs no Python or Gmsh; see
[STRESS-COMMAND](2026-10-27-STRESS-COMMAND.md). The tetrahedral experiments
remain reproducible from a Haskell mesh:

```sh
"$FEA_BIN" voxel-mesh examples/assemble/plan/flat90.coscad 2 /tmp/coscad-fea-demo/flat90-mesh.json
"$FEA_BIN" tetra2 /tmp/coscad-fea-demo/flat90-mesh.json \
  experiments/cad/flat90-tetra-loads.json > /tmp/coscad-fea-demo/flat90-result.json
```

`voxel-mesh` splits every solid voxel into six tetrahedra (a conforming Kuhn
subdivision), so the geometry is stair-stepped at the chosen cell. The optional
scikit-fem comparison scripts still run against these meshes with the pinned
`experiments/requirements-fea.txt` environment (scikit-fem, numpy, scipy; the
Gmsh pin is gone).

## Loads, outputs and interpretation

Condition files specify Young's modulus in MPa, Poisson's ratio, and a **total**
force in N distributed uniformly over the selected load surface. Support/load
regions use outward normals and rectangular bounds in mm and must select whole
exterior faces. The support is fully clamped. Missing, partial or conflicting
regions are rejected. The bracket's end clamp is a synthetic boundary condition,
not a model of its bolts. The strut files clamp one end and load the other.

Result JSON includes node displacements, cell-centre stresses, support force and
moment, strain energy, iterations and the recomputed residual. A passing residual
only establishes a solved discrete system. Mesh refinement and an independent
comparison address different errors. Inspect all three before interpreting a
new part. No automatic strength or safe-load verdict is produced.

The supplied material values are illustrative homogeneous isotropic inputs.
Infill, layer anisotropy, joint contact, fatigue and failure are not modeled.
Displacement refinement passed on the named fixtures; peak stress convergence
and printed-material strength have not been established.
