# Try FEA and native geometry

These are opt-in experimental workflows for supported solid parts. The ordinary
compiler still uses OpenSCAD. Run the commands from the repository root.
The [validation record](2026-10-26-FEA-BACKEND-EXPERIMENTS.md) documents numerical
checks, accepted geometry, limits and remaining release gates.

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
Cabal/GHC. The builder fetches and verifies the pinned Manifold source:

```sh
python3 scripts/build-manifold.py
export PKG_CONFIG_PATH="$PWD/.native/manifold-3.5.2/install/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
cabal build all -fmanifold --builddir=dist-native
COSCAD_BIN="$(cabal list-bin exe:coscad -fmanifold --builddir=dist-native)"
FEA_BIN="$(cabal list-bin exe:coscad-experiments -fmanifold --builddir=dist-native)"
mkdir -p /tmp/coscad-fea-demo
"$COSCAD_BIN" stl examples/assemble/plan/flat90.coscad \
  --backend=native-experimental -o /tmp/coscad-fea-demo/flat90.stl
"$COSCAD_BIN" check examples/assemble/ball/ball.assemble \
  --backend=native-experimental
```

`coscad next INPUT.assemble --backend=native-experimental` also uses the native
backend and records it in the manifest. It writes the usual manufacturing
outputs. Neither command dispatches a printer job. Unsupported shapes fail
explicitly; there is no silent OpenSCAD fallback. Offsets and lofts are not yet
supported. Planner instruction images continue to use OpenSCAD.

## Curved or holed CAD analysis

Use a separate Python environment for Gmsh and the independent reference solver.
The pinned dependency set was verified with Python 3.12 on macOS arm64:

```sh
python3.12 -m venv /tmp/coscad-fea-demo/venv
/tmp/coscad-fea-demo/venv/bin/python -m pip install -r experiments/requirements-fea.txt
FEA_PY=/tmp/coscad-fea-demo/venv/bin/python
"$FEA_PY" scripts/mesh-fea-tetra.py /tmp/coscad-fea-demo/flat90.stl \
  --size 3 --output /tmp/coscad-fea-demo/flat90-mesh.json
"$FEA_BIN" tetra2 /tmp/coscad-fea-demo/flat90-mesh.json \
  experiments/cad/flat90-tetra-loads.json > /tmp/coscad-fea-demo/flat90-result.json
"$FEA_PY" scripts/compare-tetra-fea-reference.py --exe "$FEA_BIN" \
  --order 2 --mesh /tmp/coscad-fea-demo/flat90-mesh.json \
  --conditions experiments/cad/flat90-tetra-loads.json
```

`tetra2` uses quadratic Tet10 displacement interpolation on the same faceted
geometry. Repeat meshing/solving at sizes 4, 3 and 2 mm, using separate output
names, before trusting a bending trend. For this bracket the saved final
displacement change is 0.73%. First-order `tetra` was substantially too stiff at
the tested resolutions; it did not pass the displacement refinement gate.

To try the existing tesseract strut, render
`examples/assemble/tesseract/bolted/strut.coscad` and use
`experiments/cad/strut-tetra-bend.json`. Target sizes 3 and 2 mm passed the saved
displacement gate with a 0.14% change. Slender-part solves can take thousands of
iterations; nonconvergence is an error, not a usable result.

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
