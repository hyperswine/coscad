# Stress analysis and a native geometry backend

Status: **intent**, written 2026-10-17. Nothing here is implemented. This
note records what we mean to build, why, what is deliberately out of scope,
and the order of work, so later changes can be checked against it.

Two projects share most of their foundations:

1. **`coscad stress`**: fast, simple stress, deflection and joint checks for
   conventional printed and bolted parts.
2. **A native geometry backend**: produce meshes without OpenSCAD and BOSL2,
   first by calling the Manifold library directly, later with something
   lighter or our own in Haskell.

## Why

**Stress.** The compiler already knows a part's exact geometry, its
material, how it will lie on the print bed (so which way its layers run),
and where its screws, nuts and mating frames are. That is most of what a
stress check needs. The questions we want answered while designing are
plain ones: will this bracket hold 20 N at its tip, how far does the arm
bend, is the stress across the layers (where FFF parts break) safe, and
will this screw crush the plastic under its head at the planned torque.

**Backend.** OpenSCAD and BOSL2 are the largest source of setup and
pipeline friction this project has had:

- macOS CI lost its test job because Homebrew's OpenSCAD cask is disabled
  by Gatekeeper; Linux needs an `xvfb-run` wrapper.
- OpenSCAD launched by launchd on the mac mini blocked for half an hour per
  render waiting for Documents access to BOSL2 (docs/LESSONS.md).
- Warnings are scraped from text output; `check` retries intersections in
  a different operand order because CGAL asserts and silently emits a
  fallback mesh.
- Every variant costs a process launch, a `.scad` text round trip and an
  ASCII STL parse; `pipeline.lock.json` has to pin a whole application
  bundle and a BOSL2 checkout.
- `coscad doctor` exists mostly to diagnose this chain.

## Part 1: `coscad stress`

### Scope

Goals:

- **Fast**: seconds for a typical part on a laptop, so it can run on save.
- **Simple, conventional geometry**: brackets, plates, beams, arms, bosses,
  pegs, clips; printed parts held by screws, nuts and pegs.
- **Loads and joints**: point and face loads, held faces, and the joints
  already declared with `fastener` lines.
- **Honest output**: peak stress with its location, deflection, safety
  factors in-plane and across layers, and a clear note where the numbers
  are not trustworthy.

Non-goals: contact between parts, friction, buckling, plasticity, large
deflection, fatigue, dynamics, heat, composites beyond a simple layer
direction, and anything resembling certification. A part that needs those
should go to a real FEA package; `stress` may say so.

### What the user writes (proposal)

Names below are proposals and will change if they read badly. `fixed` is
already an assembly constraint, hence `support`.

```
support base bot                  // this face is held still
load arm @tip 0 0 -20             // 20 N downward at a frame of part arm
load arm top+rt 0 -5 0            // or at an anchor
stress cell=0.5 safety=2          // optional settings, like `plan`
```

Units are newtons, millimetres and megapascals. Loads and supports name
anchors, `@datums` and `@frames`, so they follow the geometry when it
changes. The command is `coscad stress part.coscad|spec.assemble`; it
follows the CLI contract (diagnostics on stderr, exit 2 when a safety
factor is below the requested one).

### Joint checks (hand calculations, no mesh)

For each `fastener` line, using what the planner already computes (screw
size, head type and radius, host and clamped materials, planned torque,
position and axis):

- Screw preload from torque, `F = T / (K d)`, with a nut factor `K`
  defaulting to the usual 0.2 and overridable.
- Clamp pressure under the head and under the nut against the compressive
  strength of the material they bear on: the common way printed parts fail
  when a screw is overtightened.
- Bearing stress of the shank on a printed hole under a shear load, and
  tear-out distance to the nearest edge.
- Screw tension against its class; T-nut and hex-pocket bearing.

These run in milliseconds and come first: they cover most real failures of
the assemblies in `examples/` without any finite element model.

### Finite element method

- **Discretization**: a voxel grid of equal 8-node hexahedra, aligned with
  the print orientation that `coscad next` chooses (or the declared `▽`),
  so the layer direction is one grid axis. The default cell size is chosen
  so the thinnest wall is several cells thick, with `cell=` to override.
- **Which cells are solid**: point membership tested directly on the
  resolved shape tree (primitives, transforms and booleans), with no mesh
  and no backend involved. Shapes that membership cannot answer cheaply
  (hull, Minkowski, 2D offset, loft) fall back to ray parity on the
  rendered mesh.
- **Elements**: plain 8-node hexahedra are too stiff in bending when a wall
  is only two or three cells thick. Use incompatible modes or selective
  reduced integration; which one is decided by the beam tests below.
- **Solver**: small-strain linear elasticity, matrix-free preconditioned
  conjugate gradients. Every element has the same shape, so one element
  stiffness per material is computed once and reused. This adds the
  `vector` package as a dependency.
- **Materials**: one table with modulus, Poisson ratio, in-plane strength,
  across-layer strength and density, for printed polymers, aluminium and
  steel. Printed values come from filament datasheets and are defaults, not
  truths; part hints override them. An `infill=` hint scales stiffness and
  strength crudely, since walls and infill are not modelled separately.
- **Output**: `_stress.json` (peak von Mises stress and where, peak
  displacement, safety factors, joint results) and a colour-mapped scene
  rendered like the plan step images. Peaks at sharp internal corners and
  on voxel staircases are flagged and also reported a cell or two inside.

### Validation

Analytic cases become tests, the way geometry goldens are now:

- cantilever tip deflection `P L³ / 3 E I` and root bending stress `M c / I`;
- axial bar extension `P L / A E` and uniform stress;
- convergence: halving the cell size changes the cantilever result by a
  bounded amount, and the result approaches the formula.

Then real parts with a physical check, in the project's verification
culture: hang a known weight on a printed part (the flat 90° bracket, the
bow centre) and compare measured deflection with the prediction.

### Milestones

1. Single part: `support` and `load`, voxel solver, beam and bar tests.
2. Print-direction stresses, material table, safety factors, `infill=`.
3. Joint checks from `fastener` lines.
4. Assemblies: each part analysed with its joints as supports and loads;
   stress scenes on the companion site.
5. Maybe: a coupled assembly model. Only if milestone 4 proves too crude.

## Part 2: a native geometry backend

### Where OpenSCAD and BOSL2 are used (measured 2026-10-17)

The language, resolution, anchors, frames, constraints and the planner are
already Haskell. OpenSCAD does three jobs at the end:

- **Meshing**: `stl`, `next`, `check` and `doctor`, all through
  `renderStlFile`.
- **Exact overlap volumes** in `check`, by intersecting polyhedra.
- **Step images** for `plan --png` and the manual site.

BOSL2 supplies nine primitives and the `skin()` loft. Use is heavy but
narrow:

| Measure | Count |
|---|---|
| Part files using a BOSL2-backed shape | 57 of 84 |
| Golden `.scad` files including BOSL2 | 57 of 74 |
| `cuboid` / `zcyl` / `xcyl` / `ycyl` calls in goldens | 125 / 117 / 56 / 12 |
| `cyl` / `prismoid` / `tube` / `wedge` / `torus` / `skin` | 14 / 9 / 5 / 5 / 2 / 3 |

Facet counts are explicit almost everywhere (`$fn = 50` in 74 goldens,
`$fn = 100` in 23), so a native backend can reproduce OpenSCAD's
tessellation exactly instead of approximating its `$fa`/`$fs` rules.

### What stays

- Writing `.scad` stays as an export (`coscad part.coscad`), for people who
  want to continue in OpenSCAD.
- OpenSCAD stays a selectable backend until the native one has passed the
  full geometry tier for a release.

### Stage 1: native tessellation in Haskell

Mesh every primitive in Haskell, the BOSL2 ones included, with the same
fragment counts. Axis cylinders, tubes, prismoids, wedges and tori are
short; rounded and chamfered cuboids and cylinders are matched by volume and
bounds rather than triangle for triangle; `skin()` with equal vertex counts
first, mismatched counts after. The oracle already exists:
`test/golden/geometry.txt` holds OpenSCAD's volume and bounds for every
example, and a new test compares each primitive against an OpenSCAD render.

### Stage 2: one backend interface

A backend turns a resolved shape into an in-memory mesh, measures the
intersection of two meshes, and (optionally) renders an image. The
current OpenSCAD code becomes the first implementation. Selection by
`--backend=` and `COSCAD_BACKEND`; `coscad doctor` reports which backends
are usable. This stage also brings an in-memory mesh type, binary STL and
3MF output, which `stress` needs anyway.

### Stage 3: Manifold

Facts from the OpenSCAD 2025.05.16 bundle on this Mac: it ships
`libmanifold` 3.0.1 (with Clipper2 for 2D) as a C++ library only; that
build exports no C bindings. It provides booleans (also batched), hull,
extrude, revolve, 2D offset and hull, slicing, splitting, volume, surface
area, minimum gap between two solids, and level-set meshing. It has **no
Minkowski sum**.

- Link Manifold built with its optional C bindings, or a small C++ shim of
  the calls we use, through the Haskell FFI.
- Minkowski appears in one or two part files: the convex case is the hull of
  pairwise vertex sums; anything else falls back to OpenSCAD.
- `check` becomes a direct intersection plus Manifold's minimum gap,
  replacing the polyhedron round trip, the CGAL retry and the
  vertex-sampled distance.
- Releases link Manifold statically on Linux, macOS and Windows, built in
  CI; `pipeline.lock.json` pins Manifold instead of the OpenSCAD bundle
  for meshing.

### Stage 4: images

Either keep OpenSCAD only for step images, or write a small software
rasterizer in Haskell (flat shading, depth buffer, PNG writer) so plans
need no external tool at all.

### Stage 5: lighter, or our own

Two directions, to be chosen after stages 1 to 3 have made the
requirements concrete (which operations, which tolerances, measured
times):

- **Signed distance fields in Haskell.** Booleans are minimum and maximum,
  rounding is free and nothing can fail numerically; it shares point
  membership with `stress`. The weakness is sharp edges and exact hole
  diameters, which need fine grids or feature-preserving contouring;
  fits checked to 0.02 mm are demanding. Manifold's own level-set meshing
  gives a reference to compare against.
- **Exact mesh booleans in Haskell**, along Manifold's lines. Robustness is
  the entire difficulty. Worth it only if the C++ dependency turns out to be
  the main cost of the project.

## Shared foundations and order

- Point membership on the shape tree: `stress` milestone 1 and a distance
  field backend.
- An in-memory mesh type with binary STL and 3MF writers: backend stage 2,
  `stress` output, and faster `next`.

Proposed order: `stress` milestones 1 to 3 can start now, since membership
does not wait for any backend; backend stages 1 and 2 next; then Manifold.

## Risks and open questions

- Tessellation drift would move geometry goldens, fits and `check` results;
  stage 1 must match facet counts before anything switches over.
- BOSL2's rounded edges will match by volume and bounds, not exactly.
- Linking C++ into Haskell release binaries, Windows especially.
- Printed-part numbers are only as good as the material table and the
  `infill=` approximation; the output has to say so every time.
- Keyword names (`support`, `load`, `stress`) and whether joint checks
  belong in `plan` or `check` as well as in `stress`.
