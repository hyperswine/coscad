# An OCCT (Open CASCADE) geometry backend

Status: **intent**, written 2026-10-26, revised 2026-10-27 after review
against `main` at `2b307d6`. No OCCT integration is implemented here.
OCCT is an **additional, opt-in backend alongside OpenSCAD and the existing
native/Manifold backend**, not a replacement for either. This extends
[2026-10-17-FEA-AND-BACKEND](2026-10-17-FEA-AND-BACKEND.md) and preserves
its native geometry work and pins.

The proposed integration uses a small C ABI shim called from Haskell.
For pipeline use, kernel work runs in a supervised worker mode of the
**same `coscad` executable**. There is no Python/OCP dependency for OCCT.
One executable does not imply one process or a self-contained distribution;
shared-library packaging is a separate acceptance gate. The original Linux
spike results below remain historical evidence, not Mac or production proof.

**STL is the default output.** coscad is mostly for 3D printing, so the
default path (`coscad stl`, `coscad next`, bed STLs, slicing, `verify.py`)
stays mesh/STL under OCCT too. OCCT's main job on that path is to produce
good print meshes by default: the right tolerance, watertight, manifold,
one valid solid per body. STEP and BREP are an optional secondary export
(assemblies, CAD interchange, exact checks), asked for explicitly.

This amendment is confined to the proposal. The "Relationship to the
existing roadmap" section explains how a later roadmap/index update should
present the additional backend without superseding existing work.

## Decisions

Revised decisions on 2026-10-27:

1. **One executable with a Haskell API and C++ shim, no Python for OCCT.**
   The parent supervises a worker invocation of the same executable.
   A bounded, versioned request/result protocol is part of the work;
   its encoding and batching are implementation choices to measure.
2. **STL first.** STL meshes are the default output; STEP/BREP is
   optional (`--step`, `coscad step`).
3. **Mesh tolerance:** start by evaluating 0.01 mm / 0.1 rad as OCCT's
   deflection-based defaults; qualify accuracy and cost on the fixtures.
   Existing OpenSCAD and native sampling behavior stays unchanged.
4. **Hull and Minkowski:** support narrowly specified, tested cases;
   report approximation or refusal explicitly. Kernel API availability
   alone does not establish correct support for arbitrary operands.
5. **Keep all three backends.** OpenSCAD remains the default and the
   native/Manifold backend remains available and independently tested.
   OCCT adds CAD features and exchange; no backend retirement is planned.
   Mesh checks in `verify.py` stay.
6. **When STEP is asked for, it covers what it can do well:** assemblies
   with placements, names and colours for the whole `coscad next` project.
7. **macOS only** for OCCT: the only OCCT CI job is macOS. The pure tier
   and existing CI stay OCCT-free.
8. **The cabal flag `occt` is off by default.**

## Why

The strongest expected gains are CAD features, analytic curved geometry,
STEP exchange and a better geometry foundation for FEA. Deflection-controlled
meshing may improve printing results, but speed, reliability and fit must be
measured against both existing backends. A valid B-rep alone does not prove
a watertight mesh or a printable part.

- **Exchange (optional).** You cannot hand a coscad part or assembly to a
  machinist, a CNC CAM package or a colleague's Fusion/FreeCAD as STEP.
  Cylinders reach them as facets.
- **Real features.** Chamfers and roundings exist only on BOSL2's
  `cuboid` and `cyl` (`Cuboid … ch ro`, `Cyl … ch ro` in
  `src/Coscad/Shape.hs`). Nothing can fillet the edge where two booleaned
  solids meet. OCCT's `BRepFilletAPI_MakeFillet`/`MakeChamfer` can.
- **Measurement.** `Mesh.meshVolume` and `meshBounds` measure a
  tessellation. B-rep supports volume, area, centroid, inertia
  (`BRepGProp`) and minimum distance (`BRepExtrema_DistShapeShape`) on the
  CAD geometry, subject to numerical tolerances. The current mesh checker
  already includes triangle and edge-to-edge distances with BVH acceleration;
  OCCT's benefit is avoiding tessellation error, not fixing vertex-only checks.
- **Sweeps, lofts, offsets, threads.** `Loft` is BOSL2 `skin()` today. OCCT
  has `ThruSections`, `MakePipe`/`MakePipeShell`, `MakeRevol`, 3D
  `MakeOffsetShape`/`MakeThickSolid` and `DraftAngle`. These are the
  natural base for real threads (helical sweeps) instead of `$fn`
  approximations.

## Goals

1. **Print-ready STL by default.** `--backend=occt` (and
   `COSCAD_BACKEND=occt`) on `coscad stl` and `coscad next`.
   Default compilation to `.scad` retains its existing behavior. The OCCT
   render path produces the STL meshes the existing pipeline consumes
   (packer, `scripts/bambu-slice.py`, the `verify.py` scripts) from a
   validated B-rep in the worker, using the candidate meshing defaults,
   watertight and edge-manifold, one valid solid per body, checked before
   writing.
2. **Optional exact outputs.** `coscad step` (or `--step` on `stl` and
   `next`) for a part or a whole `.assemble` (an XDE assembly with
   placements, names, colours), plus `.brep` (OCCT native, for debugging
   and golden tests). Only with `--step` does `next` write STEP beside the
   bed STLs and record it in the `coscad.next/1` manifest (new optional
   keys, so no version bump).
3. Every existing `Shape` constructor works under OCCT, exactly or with an
   explicit `approximate` flag, or is refused with a diagnostic that names
   the constructor and the source position. No silent fallback.
4. Exact measurement (volume, area, bounds, topology counts, minimum
   distance) for `check`, `doctor`, the tests, the `next` manifest and
   later `stress`.
5. Language growth that only an exact kernel makes honest: `fillet` and
   `chamfer` on selected edges, `shell`, 3D `Offset`, `Revolve`, `Sweep`,
   smooth `Loft`, `draft` and threads, with the syntax below. They come
   incrementally: one useful fillet or loft belongs in the initial experiment;
   the rest are later options. Each backend declares support or refusal,
   with approximations explicit where an equivalent is available.

## Non-goals

- STEP as the default output. The default stays STL for printing; STEP is
  never written unless asked for.
- Replacing OpenSCAD or native/Manifold, or changing the default backend.
  Qualifying OCCT for optional distribution does not imply any such change.
- Byte-identical meshes across backends. Parity is measured (volume,
  bounds, topology), not diffed.
- A GUI or viewer. The VS Code extension keeps its current preview path.
  glTF preview (`RWGltf_CafWriter`) is a later idea.
- OCCT on Linux or Windows. The backend is built and tested on macOS only.
  The code is not deliberately Mac-specific, and a Linux build with a
  distro OCCT probably works (this page's compile check ran on Linux),
  but it is unsupported and not in CI.
- A Python dependency for OCCT. (Python stays for slicing and `verify.py`,
  as today.)

## What the code looks like today

The original inventory was written against `421ef45`. At the review baseline
`2b307d6`, `Coscad.Backend` already selects `openscad` or
`native-experimental`; the latter uses native tessellation and optional
Manifold for CSG, and is wired into `stl`, `check` and `next`. Indexed meshes
retain topology across CSG and distinguish touching solids and cavity shells.
The original native corpus evidence covers 71/74 fixtures; the current geometry
corpus has 75 entries after an additional separately checked phone stand.
The publication receipt records 1,475 local regression checks, not an OCCT
qualification or a new run during this review. See
[the validation record](2026-10-26-FEA-BACKEND-EXPERIMENTS.md) and
[the quickstart](2026-10-26-FEA-NATIVE-QUICKSTART.md).

- **The tree.** `Coscad.Shape.Shape` is the one AST and the IR at the
  same time. It holds primitives (`Rectangle`, `Sphere`, `Cylinder`,
  `Cone`, `Frustum`, `Prism n`, `Poly PD`, `Shape2D n r`), nine BOSL2
  primitives (`Cuboid`, `Cyl`, `XCyl/YCyl/ZCyl`, `Tube`, `Prismoid`,
  `Torus`, `Wedge`), transforms (`Tx…Rz`, `Scale`, `Mirror`, `Translate`,
  `RotAxis`, `Transform3`), attachment sugar (`Anchor`, `Position`,
  `AttachTo`, `CutAt`), metadata (`Tag`, `Datum`, `Frame`, `Hidden`),
  CSG (`Union`, `Diff`, `Intersection`), `Hull`, `Minkowski`, 2D
  `Offset`, `Extrude` and `Loft [(z, profile)]`. `bezPoly` already
  evaluates beziers in the compiler. There is no text, polyhedron or
  revolve.
- **Resolution is generic.** `Coscad.Geometry` (`resolve`, anchors, bounding
  boxes, mating frames) runs before codegen. `gen` only resolves attachment
  sugar defensively.
- **Emission is OpenSCAD-specific.** `Coscad.Codegen` (`gen`,
  `renderScad`, `usesBosl2`, `fn50`, `pathOf`/`loftMethod`/`loftErrors`
  for `skin()`) turns the resolved tree into text. `Coscad.OpenScad` finds
  the binary and BOSL2, builds the environment and runs it with a
  timeout (`renderStlFile :: FilePath -> FilePath -> IO (Either String
  [Tri])`). `Coscad.Mesh` parses ASCII STL into `[Tri]` and measures it.
- **Assemblies.** `Coscad.Assemble` parses `.assemble` files into
  `AsmResult`: `arParts` (top-level part name to resolved `Shape`),
  `arPartMeta` (count and `key=value` hints per part), `arFlat ::
  [FlatPart]` (every part reached through nested `.assemble` files, with
  `fpTrail`, `fpCount`, `fpDown`, `fpHints`), `arAsm` (the placed
  assembly expression), `arConstraints` (`fixed`/`mate` frames),
  `arFasteners` and the plate. `Coscad.Plan.Model` turns this into
  instances, `Inst { iName, iPart, iAff, iShape, iBox, iMat, … }`, where
  `iAff` is the instance placement and `iMat` is `Aluminium | Printed |
  Steel`. `check` splits mirrored pairs into instances (`wing#1`,
  `wing#2`). `next` writes `foo_manifest.json` with schema
  `coscad.next/1` (`Assemble.nextSchema`).
- **Callers.** `Next`, `check` and `Main` already have backend selection
  for supported mesh operations. Extend that boundary rather than rebuild
  those integrations. `plan --png`, `site` and preview workflows retain
  their OpenSCAD image path. Default compilation remains `.scad` emission.

- **Build.** One cabal package, `coscad` 1.3.0.0, GPL-3.0-only, no
  Template Haskell anywhere in `src`, `app` or `test`. CI and release use
  GHC 9.8.2. `release.yml` builds the macOS artifact on `macos-15` and
  notes that `macos-latest` (macos-26) failed to link with "tapi error …
  unknown architecture arm64e.x1-macos".
- **Tests.** `test/Spec.hs` runs tiers from `test/Tier/`. The pure tiers
  are `Diagnostics` and `Examples`, which compare `.scad` snapshots in
  `test/golden/scad/`. `Geometry` and the later tiers need OpenSCAD:
  `test/golden/geometry.txt` holds 75 lines of `path volume xmin ymin
  zmin xmax ymax zmax`. They are skipped when OpenSCAD is missing or
  `COSCAD_RENDER=0`. `COSCAD_UPDATE_GOLDEN=1` regenerates the goldens. CI
  (`ci.yml`) runs on `ubuntu-latest` only, installs `openscad` and `xvfb`
  through apt, and wraps the binary as `openscad-headless`.
- **Verification.** The per-model `verify.py` scripts
  (`models/orbit-tower`, `models/bow-center-reinforced`, the tesseract
  examples) read ASCII STL with regexes, check edge manifoldness and use
  OpenSCAD as their boolean engine. The tesseract scripts also use
  trimesh/manifold3d. None of them reads B-rep.

The rule for the split: everything up to and including `resolve` is
generic. `Codegen`, `OpenScad`, the `$fn` footer, BOSL2 detection and the
`skin()` path validation belong to the OpenSCAD backend.

## Backend abstraction

### A small lowered IR: `Coscad.Solid`

This is a proposed later shared representation, not a prerequisite to the
first OCCT experiment. Initially adapt the supported resolved `Shape` subset
at the existing backend boundary. Do not simultaneously rewrite OpenSCAD
codegen or require all constructors and language extensions before measuring
OCCT's value. If the shared IR is adopted, migrate incrementally and keep
existing OpenSCAD snapshots and native geometry tests passing.

A later common lowering pass could normalize `Shape`, which carries
sugar, metadata, anchor-only nodes (`Hidden`), two families of the same
primitive (`Cylinder` vs `Cyl`/`ZCyl`), and BOSL2 anchoring conventions
(centred `Cuboid`, `Prismoid … anchor = CENTER`) that each backend would
otherwise have to rediscover. The sketch below proposes a pure lowering
pass after `resolve`; its eventual result must include geometry and metadata,
not just `Solid`:

```haskell
data Solid                                   -- 3D
  = SBox V3 Bool                             -- size, centred?
  | SSphere D Facets
  | SCone D D D Facets                       -- r1 r2 h (covers cylinder/cone/frustum, Prism n)
  | STorus D D Facets
  | SPrismoid (D,D) (D,D) D
  | SWedge V3
  | SRounded SolidPrim Edges D D             -- chamfer, rounding on a primitive's edges (BOSL2 Cuboid/Cyl)
  | SXform M4 Solid                          -- all transforms collapse to one affine matrix
  | SBool BoolOp [Solid]                     -- union / diff (first minus rest) / intersection
  | SHull [Solid] | SMinkowski Solid Solid
  | SExtrude D Profile
  | SLoft LoftKind [(D, Profile)]            -- Ruled (today's skin) | Smooth (OCCT only)
  -- Later OCCT feature nodes; `supports` refuses them on OpenSCAD unless noted
  | SRevolve D Profile                       -- degrees about Z, profile in the XZ half-plane x >= 0
  | SSweep Path Profile SweepMode
  | SOffset3 D Solid                         -- 3D offset, arc joins
  | SShell D [FaceSel] Solid                 -- thickness, faces removed
  | SFillet D [EdgeSel] Solid | SChamfer D [EdgeSel] Solid
  | SDraft D V3 FaceSel Solid                -- angle, pull direction, neutral face
  | SSrc SrcPos Solid                        -- source position for diagnostics from the kernel
  | SEmpty
data Profile                                 -- 2D
  = PCircle D Facets | PPolygon [(D,D)] [[Int]]
  | PXform M3 Profile | PBool BoolOp [Profile] | POffset D Profile | PHull [Profile]
data Facets = Exact | Fn Int                 -- Fn only where the source asked for an n-gon
```

The key point is `Facets`. `Prism n` and `Shape2D n` are *polygons* in
coscad semantics: a hexagonal prism is a hexagon, not a tessellated
circle. They lower to `PPolygon`, which is exact under OCCT too. Every
other curved primitive lowers to `Exact`. OpenSCAD then prints its
global `$fn = 50` as today, and OCCT builds a true circle and leaves the
facet count to the mesh tolerance (see "Mesh tolerance").

`SSrc` is a target for diagnostics, not data that lowering can recover today.
Current `Shape` records positions mainly on attachment nodes. Parser and
resolution changes must preserve operation spans, file identity and provenance
through expansion before every boolean or feature can report its exact source
location. Haskell-built shapes may have no source span. Initially report the
part and operation plus any known location; never invent a line number.

`Tag`, `Datum` and `Frame` do not reach `Solid`. Instead `lower` returns
a side table `[(tag, Solid)]`, so `next` and STEP export can name bodies.
Datums and frames stay available to the edge selectors (below), which
`lower` resolves into geometric predicates before the kernel sees them.

### The interface

```haskell
data Backend = Backend
  { backendName  :: String
  , backendProbe :: IO (Either String String)           -- version line for doctor; Left = unusable
  , renderMesh   :: MeshOpts -> Solid -> IO (Either Diag IndexedMesh)
  , exportExact  :: Maybe (ExactFormat -> ExportTree -> FilePath -> IO (Either Diag ()))  -- Nothing for OpenSCAD
  , measure      :: Solid -> IO (Either Diag Measures)  -- volume, area, bbox, centroid, topology, valid, approximate
  , minDistance  :: Maybe (Solid -> Solid -> IO (Either Diag D))
  , supports     :: Solid -> [Unsupported]              -- pure, checked before running anything
  }
data ExportTree = ExportPart String Solid               -- one body
                | ExportAsm  String [ExportNode]        -- see "Optional STEP export and assemblies"
```

`Coscad.Backend.OpenScad` wraps today's `Codegen` (fed from `Solid`) and
`OpenScad.renderStlFile`. `Coscad.Backend.Occt` is new and calls the shim
inside the supervised worker. The existing native adapter remains part of
this interface. Extend current callers without removing their native path.
Selection for mesh commands is `--backend=openscad|native-experimental|occt`,
then the proposed `COSCAD_BACKEND`, then `openscad`. Image commands and default
`.scad` compilation keep their existing behavior until explicitly extended.
The parent must validate indexed results before exposing them to consumers.

Two compatibility rules:

1. `coscad part.coscad` (compile to `.scad`) keeps generating from
   `Shape`. A later IR migration adds a test that `gen` from `Solid` gives the same text as
   the snapshots in `test/golden/scad/`. When the two agree, the old path
   can go. If any snapshot changes, that is a regression.
2. `supports` is pure: it reports statically unsupported constructs and
   expected approximations without an installed kernel. It cannot predict
   every failed boolean, offset or fillet. Runtime validation and structured
   kernel errors remain mandatory. The VS Code extension exposes this same
   distinction between static diagnostics and execution results.

### BOSL2 features under OCCT

| BOSL2 / coscad | OCCT mapping | notes |
|---|---|---|
| `cuboid(size, chamfer, rounding)` | `BRepPrimAPI_MakeBox` centred, then `MakeChamfer`/`MakeFillet` on all 12 edges | exact. BOSL2's rounding is a circular arc, the same as a constant-radius fillet. The volume should match within the tessellation error |
| `cyl(r,h,chamfer,rounding)` | `MakeCylinder` centred, then fillet/chamfer on the two circular edges | exact |
| `xcyl/ycyl/zcyl` | cylinder + rotation | trivial |
| `tube(h, or, ir)` | revolved rectangle (one solid, no boolean) | exact |
| `prismoid(size1,size2,h)` | `ThruSections` of two rectangles (ruled) | exact, planar faces |
| `torus(r_maj,r_min)` | `MakeTorus` | exact |
| `wedge(size)` | extruded right triangle | exact |
| `skin(profiles, z, method)` | `BRepOffsetAPI_ThruSections(isSolid, ruled=True)` | differs: `skin` resamples profiles to matching counts using `method` (`loftMethod`). OCCT needs compatible wires. Lowering resamples to equal vertex counts in Haskell so both backends get the same polygons |
| BOSL2 anchors (`CENTER`, …) | none needed | already resolved in `Geometry` into explicit transforms |

Because BOSL2 is only used for primitives and `skin`, nothing BOSL2 does is
unreachable from OCCT.

## Integration: FFI inside a supervised worker

The parent launches the same executable in an internal OCCT worker mode.
The worker owns all kernel handles; raw pointers never cross the process
boundary. Requests and results need a versioned protocol, bounded sizes,
validated indices and finite coordinates. Benchmark per-part jobs against
bounded batches; do not assume that batching is unnecessary.

Before relying on this path in the planner or printer pipeline, establish:

- Parent-enforced wall-clock deadlines, cancellation, worker termination and
  reaping; do not depend on a Haskell timeout interrupting a C++ call.
- Input, output, job-count and resource budgets. Measure and qualify memory
  enforcement on the supported Mac environment; a worker alone does not
  protect the host from system-wide memory pressure.
- Temporary job outputs and validation before atomic publication. Preserve
  prior outputs on crash, timeout or invalid geometry. Publish multi-file
  manufacturing bundles as a coherent unit rather than mixing old and new files.
- Structured failure results, bounded stderr, and the last operation context
  recorded outside the worker. Signal handlers are not a recovery mechanism.
- Tests for crash, hang, malformed response, cancellation and a subsequent
  successful job. No automatic backend fallback and no printer dispatch.

Direct in-process calls are acceptable for the initial scratch spike only.
The C ABI below describes the worker's internal bridge, not a crash-safe
parent interface. Optional library loading must also be qualified if the
parent is expected to start without OCCT dylibs; worker mode alone does not
remove startup dependencies from a dynamically linked executable.

### Facts, checked on 2026-10-26

Checked on the web and, where marked, by building on the agent box
(Linux x86_64, GHC 9.6.6, cabal 3.10.3, Debian `libocct-*-dev` 7.8.1,
the apt candidate there; there is no Homebrew on that box).

- **OCCT.** LGPL-2.1 with the Open CASCADE exception. Homebrew
  `opencascade` (formulae.brew.sh API): stable **7.9.3**, licence
  `LGPL-2.1-only`, runtime deps `fontconfig`, `freetype`, `tbb`, build
  deps `cmake`, `doxygen`, `rapidjson`, bottles for `arm64_tahoe`,
  `arm64_sequoia`, `arm64_golden_gate`, `arm64_linux`, `x86_64_linux`.
  The `arm64_sequoia` bottle is 39.5 MB compressed. OCCT ships a CMake
  config but **no pkg-config files** (true of the Debian package; assumed
  for Homebrew), and Debian ships **shared libraries only** (no
  `libTK*.a`); the 7.8.1 shared libraries total 85 MB. OCCT 7.8 renamed
  the data exchange toolkits (`TKSTEP` and friends became `TKDESTEP`,
  `TKDESTL`, `TKDEGLTF`), so library lists differ before and after 7.8.
- **`opencascade-hs`** on Hackage. Author and maintainer **Joe Warren**
  (Hackage user `josephwarren`), repo
  `github.com/joe-warren/opencascade-hs`, a monorepo that also holds
  `waterfall-cad`, `waterfall-cad-svg` and a WASM playground. Latest
  **0.8.0.0, uploaded 2026-10-25**, 27 releases since 2023, licence
  **LGPL-2.1-only**, Haskell deps only `base` and `resourcet`. Needs OCCT
  >= 7.8 (releases before 0.3.0.0 support older OCCT). The README says it
  is tested on Debian and macOS. Its CI (`test.yml`) builds with Stack on
  Ubuntu 26.04's apt OCCT and on conda-forge OCCT 8.0.1, **not on
  macOS**. The Stack resolver is LTS 23.16, which is **GHC 9.8.4**, so
  GHC 9.8 is its primary compiler.
  - *Build, verified on the box:* `cabal get opencascade-hs-0.8.0.0 &&
    cabal build --extra-include-dirs=/usr/include/opencascade` succeeded
    against OCCT 7.8.1 with GHC 9.6.6: 250 Haskell modules plus 119 C++
    wrapper files, 2 min 47 s wall.
  - *How it is built:* one `hs_*` C function per wrapped C++ call in
    `cpp/*.cpp` (`cxx-sources`, `-std=c++17 -Werror`), imported with
    `foreign import capi unsafe`. Lifetimes use `resourcet`'s `Acquire`
    (`mkAcquire … delete…`), not `ForeignPtr`. Every call passes a flag
    and an exception pointer, and `OpenCascade.Internal.Exception`
    rethrows `Standard_Failure` and `std::exception` as a Haskell
    `OpenCascadeException`.
  - *Build configuration:* `include-dirs: /usr/include/opencascade` is
    hard-coded, and `extra-libraries` is a fixed list including
    `stdc++`, `TKV3d` and `TKService` (visualisation toolkits). For
    macOS the README asks for `CPATH`/`LIBRARY_PATH` pointing into
    `$HOMEBREW_PREFIX/Cellar/opencascade/7.*/…`, a versioned Cellar path
    that breaks on every `brew upgrade`. Whether its unconditional
    `-lstdc++` links with Apple clang (whose C++ runtime is `libc++`) is
    **unverified**.
  - *Coverage (0.8.0.0 module list and a grep of its `cpp/`):*
    BRepPrimAPI box, cone, cylinder, prism, revol, sphere, torus (**no
    wedge**); BRepAlgoAPI Fuse/Cut/Common and BOPAlgo (with
    `SetFuzzyValue`); BRepBuilderAPI edges, wires, faces, polygon, solid,
    sewing, transform, GTransform, copy; BRepFilletAPI
    MakeFillet/MakeChamfer; BRepOffsetAPI **MakeOffsetShape, MakePipe and
    ThruSections only**; BRepMesh IncrementalMesh; BRepGProp; Bnd box
    and OBB; STEPControl Reader/Writer; STEPCAFControl **Writer** (name and
    colour modes); StlAPI Reader/Writer; RWGltf and RWObj CAF
    readers/writers; TDocStd document; XCAFDoc ShapeTool (**only
    `addShape`, `addSubShape`, `findShape`**), ColorTool, VisMaterial;
    ShapeFix Solid; ShapeUpgrade UnifySameDomain; Font text; HLR.
  - *Missing for coscad:* `BRepExtrema_DistShapeShape` (exact distance
    for `check`), `BRepOffsetAPI_MakeThickSolid` (shell),
    `MakePipeShell` (sweeps, threads), `BRepOffsetAPI_MakeOffset` (2D
    wire offset for `Offset`), `DraftAngle`, `MakeWedge`,
    `BRepCheck_Analyzer` (validity), `Interface_Static` (STEP schema and
    unit), XCAF assembly structure (`NewShape`, `AddComponent`,
    `TDataStd_Name`, instance colours), `STEPCAFControl_Reader`
    (read-back tests), and boolean history (`SectionEdges`,
    `Generated`/`Modified`, needed for edge selection).
- **Our own shim, verified on the box.** The sketch below (`cbits/
  coscad_occt.{h,cpp}`, 95 lines of C++, and a 69-line `Main.hs`) builds
  as a cabal executable with `cxx-sources` behind a cabal flag, and runs:
  20×20×10 box minus an r = 5 cylinder, meshed at 0.01 mm / 0.1 rad,
  binary STL (520 triangles) and AP242 STEP written; volume
  3214.601836602551 against 4000 − 250π = 3214.601836602552; one valid
  solid; a degenerate box comes back as `Left (OcctError
  "Standard_DomainError: ")` instead of crashing. Run time 0.09 s. The
  dynamically linked binary is 10 MB and pulls in 24 `libTK*`
  libraries. A separate 60-line C++ XDE spike (`xcaf_spike.cpp`) wrote a
  two-level assembly (one plate, one post definition placed twice, names
  and a colour); `STEPCAFControl_Reader` read back the root, three
  components, their names, the shared `post` definition at both
  placements, and the colour. The files are in `out/occt-spike/` beside
  this plan, not in the repo.

### The options

**(a) Depend on `opencascade-hs`** and write the `Solid` interpreter in
Haskell on its 1:1 bindings.

- For: maintained (a release the day before this check), GHC 9.8 is
  its home compiler, exceptions already translated, and waterfall-cad
  shows it can carry a real CAD library.
- Against: about a third of what coscad needs is missing (list above),
  so we would carry upstream PRs or a fork from day one. Fine-grained
  calls mean thousands of FFI crossings per model, all `unsafe` (a long
  boolean inside an `unsafe` call stops the garbage collector on every
  capability, and `coscad` runs with `-N`). The hard-coded include path,
  the visualisation toolkits and the Cellar-path advice all work against
  a clean Homebrew build. 250 extra modules and about 3 minutes per clean
  CI build. `Acquire`-scoped lifetimes suit a script, not a cache of
  shapes keyed by subtree.

**(b) Our own thin C ABI shim**: coarse, coscad-shaped `extern "C"`
functions over OCCT in `cbits/occt/*.cpp`, imported with `foreign import
ccall`, compiled by cabal's `cxx-sources`, linked with `extra-libraries`.

- For: we choose the granularity (one call per `Solid` node, or per
  subtree), so a model is tens of crossings. One place catches every C++
  exception. Exactly the toolkits we use (12 direct in the spike).
  Everything we need (XDE assemblies, `BRepExtrema`, `MakeThickSolid`,
  `Interface_Static`, boolean history) is more C++ in the same style. No
  Haskell dependency added. It was proven on the box in about an hour.
- Against: we own perhaps 1,500–2,500 lines of C++ at broader feature coverage, its build
  flags, and OCCT API drift (7.9 to 8.0 changes land on us).

**(c) Hybrid**: `opencascade-hs` where it covers the need, our shim for
the gaps. Two exception regimes, two lifetime regimes (`Acquire` vs
`ForeignPtr`), and two representations of a `TopoDS_Shape` at the
boundary that must be bridged by copying. All of (a)'s build costs plus
all of (b)'s C++.

### Recommendation: (b), our own shim

CoScad already has a backend boundary and a closed `Shape` tree; `Solid`
is a proposal rather than an existing abstraction. A small C API over the
supported subset lets us control ownership, errors and dependencies. Its
performance advantage over a general binding must be measured, and it does
not make OCCT memory-safe or crash-safe. Process supervision supplies the
failure boundary. `opencascade-hs` stays useful as a reference for tricky
signatures and for quick experiments in M0; if its coverage grows, a later
switch touches only `Coscad.Backend.Occt`. Licence does not decide it:
LGPL-2.1 code (OCCT, or `opencascade-hs`, which GHC would link statically)
may be combined into GPL-3.0-only coscad, whose source is public.

The original 1–2 day estimate covered only the Mac FFI spike, not worker
containment or distribution. Re-estimate the bridge after the first build;
additional operations grow behind fixture-specific acceptance checks.

### The shim, concretely

Layout:

```
cbits/occt/coscad_occt.h      -- plain C; the only header Haskell sees
cbits/occt/shape.cpp          -- handle, guard(), primitives, booleans, transforms
cbits/occt/profile.cpp        -- wires, faces, 2D offset, extrude, revolve, loft, sweep
cbits/occt/feature.cpp        -- fillet/chamfer/shell/draft, edge selection, history
cbits/occt/exchange.cpp       -- mesh, BREP, STEP (XDE), measures, BRepExtrema
src-occt/Coscad/Backend/Occt/FFI.hs    -- foreign imports, ForeignPtr wrappers
src-occt/Coscad/Backend/Occt.hs        -- the Solid interpreter and the Backend record
src-occt-stub/Coscad/Backend/Occt.hs   -- same module name, built when the flag is off
```

Header (the historical spike, trimmed; not a production ownership contract):

```c
typedef struct cs_shape cs_shape;   /* owns one TopoDS_Shape (a handle; copies share the TShape) */
/* Every call takes `char **err`. On failure: constructors return NULL, others -1,
   and *err is a malloc'd message the caller frees with cs_free_error. */
cs_shape *cs_box(double dx, double dy, double dz, int centred, char **err);
cs_shape *cs_cylinder(double r, double h, int centred, char **err);
cs_shape *cs_bool(int op, const cs_shape *a, const cs_shape *b, char **err); /* 0 fuse 1 cut 2 common */
cs_shape *cs_transform(const cs_shape *s, const double m[12], char **err);  /* row-major 3x4 */
void      cs_free(cs_shape *s);                                             /* ForeignPtr finalizer */
int  cs_mesh(cs_shape *s, double lin_defl, double ang_defl, char **err);
int  cs_write_stl(const cs_shape *s, const char *path, char **err);
int  cs_write_step(const cs_shape *s, const char *path, const char *name, char **err);
int  cs_props(const cs_shape *s, double out[7], char **err); /* volume, cx, cy, cz, area, valid, solids */
void cs_free_error(char *err);
const char *cs_occt_version(void);
```

The C++ side; the sketch routes calls through a guard. Production code must
also make allocation/error paths non-throwing, handle allocation failure,
validate null inputs and output bounds, and prevent exceptions from escaping
cleanup functions. This illustrative guard alone does not establish those properties:

```cpp
struct cs_shape { TopoDS_Shape s; };
struct Fail { std::string msg; };
[[noreturn]] static void fail(const char *what) { throw Fail{what}; }

template <class F> static auto guard(char **err, F f, decltype(f()) bad) -> decltype(f()) {
  std::string m;
  try { *err = nullptr; return f(); }
  catch (const Fail &e) { m = e.msg; }
  catch (const Standard_Failure &e) { m = std::string(e.DynamicType()->Name()) + ": " + (e.GetMessageString() ? e.GetMessageString() : ""); }
  catch (const std::exception &e) { m = e.what(); }
  catch (...) { m = "unknown C++ exception"; }
  *err = strdup(m.c_str());
  return bad;
}
/* OCCT's default messenger prints transfer statistics to stdout; coscad owns stdout. */
static const bool g_quiet = [] { Message::DefaultMessenger()->RemovePrinters(STANDARD_TYPE(Message_PrinterOStream)); return true; }();

extern "C" cs_shape *cs_bool(int op, const cs_shape *a, const cs_shape *b, char **err) {
  return guard(err, [&] {
    TopoDS_Shape r;
    auto run = [&](BRepAlgoAPI_BooleanOperation &&alg) {
      alg.SetFuzzyValue(1e-6); alg.Build();
      if (!alg.IsDone() || alg.HasErrors()) fail("boolean failed");
      r = alg.Shape(); };
    switch (op) { case 0: run(BRepAlgoAPI_Fuse(a->s, b->s)); break;
                  case 1: run(BRepAlgoAPI_Cut(a->s, b->s)); break;
                  case 2: run(BRepAlgoAPI_Common(a->s, b->s)); break;
                  default: fail("bad op"); }
    return wrap(r); }, (cs_shape *)nullptr);   /* wrap: new cs_shape{r}, fail() on a null shape */
}
```

The Haskell side:

```haskell
data CShape
newtype Shape = Shape (ForeignPtr CShape)
type Err = Ptr CString

foreign import ccall unsafe "cs_box"  c_box  :: CDouble -> CDouble -> CDouble -> CInt -> Err -> IO (Ptr CShape)
-- safe: booleans and meshing can run for seconds; an unsafe call would stall GC on every capability
foreign import ccall safe   "cs_bool" c_bool :: CInt -> Ptr CShape -> Ptr CShape -> Err -> IO (Ptr CShape)
foreign import ccall unsafe "cs_free_error" c_free_err :: CString -> IO ()
foreign import ccall unsafe "&cs_free"      p_free     :: FunPtr (Ptr CShape -> IO ())

newtype OcctError = OcctError String

-- Run one shim call with an error slot; the C side fills it on failure.
call :: (Err -> IO a) -> IO (a, Maybe OcctError)
call k = alloca $ \ep -> do
  poke ep nullPtr
  r <- k ep
  e <- peek ep
  if e == nullPtr then pure (r, Nothing)
  else do m <- peekCString e; c_free_err e; pure (r, Just (OcctError m))

-- A fresh shape, owned by a ForeignPtr whose finalizer calls cs_free.
own :: (Err -> IO (Ptr CShape)) -> IO (Either OcctError Shape)
own k = do
  (p, e) <- call k
  case e of
    Just err -> pure (Left err)
    Nothing  -> Right . Shape <$> newForeignPtr p_free p

cut :: Shape -> Shape -> IO (Either OcctError Shape)
cut (Shape a) (Shape b) = withForeignPtr a $ \pa -> withForeignPtr b $ \pb -> own (c_bool 1 pa pb)
```

Production wrappers need async-exception-safe ownership transfer (`mask` and
cleanup established before allocation-returning calls), null-result checks,
and boundary sanitizer tests. The unmasked `own` sketch is not sufficient.

`Coscad.Backend.Occt` maps `OcctError` to a `Diag` at an available `SSrc`
position, so a failing fillet reads `part.coscad:12:5: fillet 2 top:
BRepFilletAPI_MakeFillet failed (StdFail_NotDone)`.

Rules the shim follows:

- **Handles.** `cs_shape` is a heap box around a `TopoDS_Shape`, itself a
  cheap reference to a shared, reference-counted `TShape`. Freeing one box
  never invalidates another, so GHC finalizers may run in any order on
  any thread. Shapes are not mutated after creation, except that meshing
  attaches triangulation. A shallow `TopoDS_Shape` copy still shares the
  underlying topology; cleaning it can alter cached aliases. Use explicitly
  independent topology/triangulation ownership and test different-tolerance
  meshing of shared subtrees. Qualify the chosen `BRepBuilderAPI_Copy`
  options against the pinned kernel rather than treating assignment as a copy. Non-shape
  OCCT objects (`TDocStd_Document`, XCAF tools) never cross the boundary:
  an export is one call that takes the whole export tree and builds and
  frees the document inside C++.
- **Errors.** No C++ exception may cross into GHC (undefined behaviour,
  in practice a dead process). `guard` catches everything. Errors return
  through an out-parameter, not thread-local state, because a Haskell
  thread can move between OS threads between two FFI calls (the spike's
  first version used a thread-local and was changed for this reason).
  Algorithms that fail without throwing (`IsDone() == false`,
  `HasErrors()`, an invalid `BRepCheck_Analyzer` result) also `fail`. A
  hard crash still kills the worker; the parent reports failure, preserves
  prior artifacts and can start a fresh worker. Exception guards do not
  handle segmentation faults or enforce time/memory limits.
- **Threads.** OCCT modelling algorithms may run concurrently on
  different shapes, but some state is global: `Interface_Static` (STEP
  parameters), the messenger, the `XCAFApp_Application` singleton. Initially
  runs all kernel work inside each worker on **one** dedicated bound thread (a `forkOS`
  worker fed by an `MVar` queue; the rest of coscad's `-N` runtime is
  unaffected). Parallel booleans and meshing for `next` variants can come
  later, with exports still serialised by a lock. Inside one call
  `BRepMesh_IncrementalMesh` runs with `isInParallel = true`.
- **Static initialisers, GHCi and Template Haskell.** OCCT registers RTTI
  and default messengers in static initialisers when its dylibs load;
  in the compiled binary that is ordinary program start through the
  system loader. GHCi and Template Haskell differ: they load object code
  into GHC's own linker, which must then load C++ objects and `libc++` and
  run their initialisers, a known source of trouble. coscad has no
  Template Haskell today, and the OCCT code lives in its own internal
  library (below), so a future TH splice in the main library never loads
  it. `cabal repl` on the OCCT library needs the C++ runtime passed
  explicitly (`--repl-options=-lc++`) and is a convenience, not a
  supported workflow. Our own static (`g_quiet`) only calls OCCT's
  function-local singleton, so initialisation order is safe.
- **C++ runtime.** `extra-libraries: c++` on Darwin, `stdc++` elsewhere
  (`if os(darwin)`).
- **Library list.** Name every toolkit whose symbols we call. The spike
  failed first with `DSO missing from command line` for
  `GProp_GProps::Mass` until `TKG3d`/`TKG2d` were added: modern linkers do
  not resolve through indirect dependencies. Candidate broader library list (reduce to the spike subset initially): `TKernel TKMath
  TKG2d TKG3d TKGeomBase TKBRep TKGeomAlgo TKTopAlgo TKPrim TKBO TKBool
  TKFillet TKOffset TKShHealing TKMesh TKXSBase TKDESTEP TKDESTL TKLCAF
  TKCAF TKXCAF TKCDF`.

### Cabal flag and the stub

```
flag occt
  description: Build the optional OCCT worker backend (needs OCCT libraries)
  default:     False
  manual:      True

library occt-backend                         -- internal library
  visibility:      private
  hs-source-dirs:  src-occt
  exposed-modules: Coscad.Backend.Occt, Coscad.Backend.Occt.FFI
  build-depends:   base, coscad
  cxx-sources:     cbits/occt/shape.cpp cbits/occt/profile.cpp cbits/occt/feature.cpp cbits/occt/exchange.cpp
  cxx-options:     -std=c++17 -Wall -fPIC
  include-dirs:    cbits/occt
  extra-libraries: TKernel TKMath … (the list above)
  if os(darwin)
    extra-libraries: c++
  else
    extra-libraries: stdc++
  if !flag(occt)
    buildable: False

executable coscad
  if flag(occt)
    build-depends: coscad:occt-backend
  else
    hs-source-dirs: src-occt-stub            -- same module name, no C++
    other-modules:  Coscad.Backend.Occt
```

(The test suite gets the same `if flag(occt)` stanza.)

- **Flag off (default).** `cabal build` and `cabal test` on
  `ubuntu-latest`, the VS Code extension job and the existing release
  build are unchanged: no C++ compiled, no OCCT linked. The stub's
  `occtBackend` has `backendProbe = pure (Left "this coscad was built
  without OCCT; build with: cabal build -f occt (needs brew install
  opencascade)")`. `--backend=occt` and `COSCAD_BACKEND=occt` exit 1 with
  that message before any work, and `coscad doctor` prints it in an
  `occt` section. `lower`, `supports` and their diagnostics live in the
  main library, so `validate --backend=occt` works in both builds.
- **Flag on.** `cabal.project.local` on the Mac (not committed):

  ```
  package coscad
    flags: +occt
    extra-include-dirs: /opt/homebrew/opt/opencascade/include/opencascade
    extra-lib-dirs:     /opt/homebrew/opt/opencascade/lib
  ```

  The `opt/opencascade` symlink is used instead of the
  `Cellar/opencascade/7.*` glob in `opencascade-hs`'s README, so a `brew
  upgrade` does not break the paths. `scripts/occt-env.sh` writes this
  file using `$(brew --prefix opencascade)`, which also covers Intel
  Macs. CI passes the same paths on the command line.
- **Tests.** `test/Tier/Occt.hs` is compiled in both builds; it asks
  `backendProbe` and reports itself skipped when OCCT is not built in.

### Linking and distribution on macOS

- **Dynamic, against Homebrew's OCCT (development / managed installation).** An optional artifact
  built with `-f occt` links `/opt/homebrew/opt/opencascade/lib/
  libTK*.dylib`, and the formula gains `depends_on "opencascade"` (about
  40 MB download plus `tbb`, `freetype`, `fontconfig`). The coscad binary
  stays small (10 MB in the Linux spike, mostly the GHC runtime and
  coscad). Distribution must document the applicable licence and notice
  obligations. This is not a self-contained downloadable binary. Things for M0 to
  check: the install names in the binary (`otool -L`; if Homebrew's OCCT
  dylibs use `@rpath/…`, add `ghc-options:
  -optl-Wl,-rpath,/opt/homebrew/opt/opencascade/lib`, as the
  `opencascade-hs` README's Apple-silicon notes hint), and **soname
  churn**: the dylibs are versioned (`libTKernel.7.9.dylib`), so an OCCT
  bump to 8.0 in Homebrew breaks an installed coscad until it is rebuilt.
  Mitigation: a managed formula dependency and rebuild policy, plus clean-machine
  and upgrade tests. `doctor` may report build/runtime version information only
  if the executable starts: it cannot repair a missing startup dylib. A compile-time
  `cs_occt_version` string alone also does not prove the loaded library version.
- **Static OCCT (not now).** Neither Homebrew nor Debian ships static
  OCCT, so we would build it ourselves in CI (CMake,
  `BUILD_LIBRARY_TYPE=Static`, modelling and data-exchange modules only,
  probably 30–60 min uncached), the binary would grow by tens of MB
  (unverified; the shared libraries total 85 MB and a static link keeps
  only what is referenced), and each release would owe LGPL relinking
  material (object files). Worth it only if coscad ships outside Homebrew.
- **Code signing and notarisation.** Apple-silicon executables must
  carry at least an ad-hoc signature, which the linker adds; Homebrew's
  bottled dylibs are ad-hoc signed. A Homebrew-installed binary has no
  quarantine attribute, so Gatekeeper is not involved. The GitHub
  `.tar.gz`, if downloaded by a browser, is quarantined today regardless
  of OCCT; notarising it needs a Developer ID and is a separate decision.
  Any `install_name_tool` rewrite invalidates the signature and must be
  followed by `codesign -s - -f`.
- **Runner.** `release.yml` builds on `macos-15` because `macos-latest`
  (macos-26) failed to link with a `tapi` error. The OCCT CI job must use
  the same runner image as the release, or it proves nothing about the
  release.

### Distribution acceptance gate

Choose and document either a Homebrew-managed installation or a relocatable
bundle containing its required libraries (or a separately qualified static
build). Until this gate passes, OCCT is an experimental source build only.
Test on a clean Mac without the developer's Cellar paths, including transitive
libraries, install names/rpaths, signatures, upgrades and missing-library
behavior. Record dependency versions and hashes in the pipeline lock and
artifact provenance. Source-build success is not release-artifact evidence.
The existing OCCT-free distribution remains supported.

### What the M0 spike must prove

On `macm4.local` (Apple silicon, Homebrew `opencascade` 7.9.3, GHC 9.8
with cabal), on a scratch branch:

1. `brew install opencascade`; `cabal build -f occt` of the spike
   executable (`out/occt-spike/`) with only the `opt/opencascade` include
   and lib dirs. Record `otool -L` and whether an rpath flag was needed.
2. The executable makes a box minus a cylinder, meshes it at 0.01 mm /
   0.1 rad, writes binary STL and AP242 STEP, prints the exact volume
   (4000 − 250π to 1e-9 relative) and one valid solid, and returns `Left`
   for a degenerate box without crashing.
3. The STL opens in Bambu Studio, and the STEP in FreeCAD or Fusion with
   the hole as a true cylinder and units in mm.
4. Record single-part STEP read-back geometry and mm units. The historical
   XDE spike is useful reference material; full assembly import acceptance
   belongs to Later C, not the initial integration gate.
5. A clean `cabal build -f occt` on a GitHub macOS runner with cached
   Homebrew OCCT, and its wall time.
6. Compare runtime, peak memory and mesh size with OpenSCAD and native/Manifold
   on representative parts; include a 100-cylinder union and a 50-variant loop.
   Measure worker startup and batching costs rather than assuming either away.
7. Exercise the supervised worker's timeout, crash and malformed-output paths;
   verify prior-output preservation and recovery with a fresh job.

**Go to the bounded experiment** only when build, geometry checks, error paths
and worker containment pass locally and on the Mac CI runner. STEP/STL import
and comparative measurements are required evidence, not optional demonstrations.
This is not a release or backend-replacement gate. Failure means narrow the
supported subset or investigate packaging; source-built static OCCT is an
option to evaluate, not an automatic commitment.

**Unverified so far:** everything on macOS (header location under
`opt/opencascade/include/opencascade`, dylib install names and rpath,
`-lc++`, OCCT 7.9.3 against the 7.8.1 the shim was compiled with, the
`macos-latest` linker issue); GHC 9.8 (the box has 9.6.6, and the shim
uses nothing version-specific); `opencascade-hs` on macOS; FreeCAD and
Fusion import of the spike files.

## Feature parity

| coscad | OpenSCAD today | OCCT | status / behaviour difference |
|---|---|---|---|
| `Rectangle` / `Cuboid` | `cube` / BOSL2 `cuboid` | `MakeBox` (+fillet/chamfer) | parity. Exact rounding |
| `Sphere`, `Cylinder`, `Cone`, `Frustum`, `Cyl`, `X/Y/ZCyl`, `Torus` | facets at `$fn = 50` | exact surfaces | **differs**: the OpenSCAD volume is the polygon's. A 50-gon cylinder is ~0.26 % smaller than the true circle (`sin(2π/50)·50/2π`), and geometry goldens must allow for this (see Testing) |
| `Prism n`, `Shape2D n r` | `$fn = n` | lowered to an n-gon polygon | exact parity |
| `Poly PD` (with holes) | `polygon` | wire + face, inner wires as holes | parity. Orientation fixed in `lower` |
| `Tube`, `Prismoid`, `Wedge` | BOSL2 | see the BOSL2 table | parity |
| transforms (`Tx…`, `Scale`, `Mirror`, `RotAxis`, `Transform3`) | `translate/rotate/scale/mirror/multmatrix` | `gp_Trsf` (rigid, mirror) / `gp_GTrsf` (non-uniform scale) | non-uniform `Scale` of curved surfaces turns a cylinder into a B-spline through `BRepBuilderAPI_GTransform`; slower and less exact, so `supports` warns |
| `Union`, `Diff`, `Intersection` | CGAL / Manifold | `BRepAlgoAPI_Fuse/Cut/Common` (arguments/tools batched, fuzzy 1e-6) | parity. Then `ShapeUpgrade_UnifySameDomain` so topology counts are meaningful |
| `Extrude` | `linear_extrude` | `BRepPrimAPI_MakePrism` | parity (twist/scale: see syntax) |
| `Loft` | BOSL2 `skin` | `ThruSections(solid, ruled)` | parity after resampling in Haskell. `Loft smooth` is OCCT-only |
| `Offset` (2D) | `offset(r)` | `BRepOffsetAPI_MakeOffset` on the wire, `GeomAbs_Arc` joins | parity for `r` |
| `Hull` | `hull()` | **none native** | exact for recognised cases, faceted and flagged otherwise (below) |
| `Minkowski` | `minkowski()` | **none general** | exact for recognised cases, refused otherwise (below) |
| `Empty`, `Hidden` | `union(){}` | empty compound | parity |
| new: `fillet`, `chamfer`, `shell`, 3D `Offset`, `Revolve`, `Sweep`, `Loft smooth`, `draft`, `Thread` | — | see "Language extensions" | OCCT-only, or approximated on OpenSCAD where BOSL2 can |

## Hull and Minkowski under OCCT

OCCT has no convex hull or Minkowski operator. OCCT-based CAD tools
(FreeCAD, CadQuery, build123d) have no general 3D hull either; where one
exists it is 2D (build123d's hull of arcs and lines), and their idiom for
"a rounded blob between round things" is a loft, revolve or fillet. The
orthodox approach is therefore to build the exact B-rep that the hull or
sum *is* whenever we can recognise it, and to fall back to facets,
flagged, only when we cannot.

### Hull: decision

`lower` classifies every `Hull` node (pure, so `validate` reports the
class) and takes the first rule that applies:

1. **2D hull of circles and polygons** (any `Profile` hull, and every 3D
   case that reduces to one). Computed exactly in Haskell: the convex hull
   of discs and points is a cycle of arcs and bitangent segments. Built as
   a wire of `GC_MakeArcOfCircle` arcs and segments, then a face. Exact.
2. **Hull of prisms sharing an axis and extent**: every child is an
   extrusion or cylinder (`Cylinder`, `Cyl`, `ZCyl`, `Prism n`, `Extrude h
   p`, boxes) with parallel axes and the same extent along them, possibly
   moved in the plane. That is `Extrude h` of the 2D hull of the cross
   sections; exact. This covers the repo's real uses: the "hull of 3
   short posts" (glider wing), "hulls of three small round pads"
   (ship-anchor flukes), `grid … |> hull` of round ends
   (`native-orbit-rotor`).
3. **Hull of two spheres** (any radii): two spherical caps and the
   tangent frustum, built as a revolve of the 2D hull of two circles about
   their centre line. Exact. A chain of pairwise sphere hulls ("piecewise
   hulls of spheres", the glider fuselage) is a union of these.
4. **Hull of two coaxial discs** at distinct axial positions: a frustum
   between the circles, subject to degenerate/contained-case handling.
   Finite cylinders additionally require the axial convex envelope, including
   any cylindrical portions; a single frustum is not generally their hull.
   Refuse unimplemented cases rather than misclassifying them as exact.
5. **Hull of polyhedral solids** (boxes, prisms, prismoids, wedges,
   polygon extrusions, in any orientation): a 3D quickhull in Haskell over
   their exact vertices; faces rebuilt as planar polygons, sewn
   (`BRepBuilderAPI_Sewing`, `MakeSolid`) and merged
   (`UnifySameDomain`). Exact.
6. **Anything else** (a sphere with a box, a torus with anything, a tilted
   cylinder with a sphere): the hull of the children's tessellations at
   the current mesh deflection, built as in rule 5. A valid solid with
   planar faces, with `approximate = true` in its measures; `validate
   --backend=occt` and every render print a warning naming the source
   position and the reason ("hull: no exact form for sphere + box;
   faceted at 0.01 mm"); `--strict` makes it an error; its STEP body name (if exported)
   gets an `(approximate)` suffix.

The hulls live in a new pure module, `Coscad.Hull`, so the supported rules and the
classifier are tested in the pure tier without OCCT. Which rule each
existing hull in `examples/`, `experiments/` and `models/` hits is a broader-parity
deliverable. The comments in those files suggest rules 2 and 3 cover
most of them; `grease_claw`'s taper (a thin rectangular end-cap hulled
with a thin disk further along the axis) is a hull of two parallel flat
sections, whose exact form is a developable surface made of planar
triangles and cone patches. It falls to rule 6 at first; an exact
"parallel sections" rule can follow if it matters.

### Minkowski: decision

A mathematical Minkowski sum with a ball is a dilation, but OCCT's offset
API is not a general robust implementation of that operation. Its pinned
[7.9.3 header](https://raw.githubusercontent.com/Open-Cascade-SAS/OCCT/V7_9_3/src/BRepOffsetAPI/BRepOffsetAPI_MakeOffsetShape.hxx)
documents self-intersection limitations, surface restrictions and failure cases.
In particular, self-intersection elimination is not implemented. Do not
advertise support for "any solid" or "any profile" merely because an API exists.

| operands | candidate construction | acceptance contract |
|---|---|---|
| specifically qualified solid + sphere | arc-joined 3D offset, translated by the sphere centre | Only tested shape/offset domains; check topology, orientation, geometry and tolerances; otherwise explicit refusal/failure |
| specifically qualified planar profile + circle | 2D offset with arc joins | Test holes, collapse, merging and concavities; no general guarantee |
| qualified Z-extrusion + aligned Z-cylinder | extrusion of qualified profile offset, combined interval and height | Only if the profile operation passes its contract |
| convex planar/extruded profile + aligned rectangle/box | hull of translated profiles, with combined axial interval where applicable | Verify convexity, frames and degenerate cases |
| convex polyhedral + convex polyhedral | hull of pairwise vertex sums | Explicit finite convex input contract and geometry tests |
| other cases | none initially | Refuse with operands and operation identified; other backends remain explicitly selectable |

Classify the actual corpus before claiming support for examples such as
`waterbottle_advanced.coscad`. Static `supports` diagnostics describe supported
forms, not a promise that a particular kernel operation will succeed. A failed
or invalid offset must never be accepted as the requested Minkowski sum.
Numerical B-rep geometry also has tolerances: "exact" distinguishes it from
intentional faceting, not from floating-point or kernel approximation error.

## Language extensions

These follow the existing conventions (`2026-08-17-SYNTAX`,
`2025-07-14-LANGEXTENSION`, and the "Planned" section PR #27 added to the
plan doc): capitalised prefix forms in `!simple` (`Extrude h obj`, `Offset
n a`, `Loft z0 p0 …`, `Hull a b`), lowercase pipe stages (`|> datum name
ref`, `|> hull`, `|> anchor bot+lft+fwd`), anchor words `top bot lft rt
fwd bak ctr` with `+` combinations, `@name` for datums and frames, and
`key=value` for optional arguments (`mate … roll=90 gap=0`, `op …
over=… before=…`). Glyph spellings are left to the glyph vocabulary and
not proposed here. Every new construct gets a `supports` rule.

| construct | `!simple` syntax | OCCT | OpenSCAD backend |
|---|---|---|---|
| fillet | `s \|> fillet r EDGES` | `BRepFilletAPI_MakeFillet` | refused (the existing `Cuboid`/`Cyl` rounding stays) |
| chamfer | `s \|> chamfer d EDGES`, `s \|> chamfer d1 d2 EDGES` | `MakeChamfer` | refused (same exception) |
| shell | `s \|> shell t [FACES]`: hollow inward by `t`, removing the listed faces | `BRepOffsetAPI_MakeThickSolid` | refused |
| 3D offset | `Offset n solid` (2D today; the operand's dimension decides) | `MakeOffsetShape`, arc joins | refused |
| revolve | `Revolve profile`, `Revolve profile angle=270` | `BRepPrimAPI_MakeRevol` about Z, profile in the XZ half-plane | `rotate_extrude` at `$fn` |
| extrude options | `Extrude h obj twist=30 scale=0.8` | `ThruSections` of transformed sections, or `MakePipeShell` with a law | `linear_extrude(twist, scale)` |
| sweep | `Sweep path profile`, `… frame=fixed\|frenet` | `MakePipeShell` | BOSL2 `path_sweep` (approximate) |
| paths | `Path x y z x y z …`, `Arc r a0 a1`, `Helix pitch h r` | edges and wires; a helix as a 2D line on a `Geom_CylindricalSurface` | point lists |
| smooth loft | `Loft smooth z0 p0 z1 p1 …` | `ThruSections(ruled = false)` | refused |
| draft | `s \|> draft deg bot [FACES]`: pull along +Z, neutral face `bot` | `BRepOffsetAPI_DraftAngle` | refused |
| threads | `Thread M5 len` (external), `s \|> tap M5 @hole depth=8` (internal) | helical `MakePipeShell` of the ISO 68-1 profile, then fuse or cut | BOSL2 `threaded_rod` (approximate) |

Thread specs reuse the spec strings `coscad plan` already parses (`M5x8`:
`fSpec`, `fDiam`), so a modelled hole and its `fastener` line agree. A
`next` hint `threads=model|clearance` picks per part whether threads are
modelled or left as plain holes, as today.

### Edge and face selection

Fillets need stable edge names, and coscad has the stable-datum rule
(`2026-07-24-TOPOLOGICAL`): anchor a feature to the nearest corner or edge
of the geometry it belongs to, never to a composite centre or a far face
that unrelated geometry can shift, and attach features while the relevant
geometry is still simple. Selection follows the same rule.

**Primary design: geometric selectors, evaluated where the stage is
written.** A `fillet` stage selects among the edges of the shape built
*so far*, exactly as `anchor` does, using the same words. `lower` turns a
selector into a geometric predicate (planes, directions, points and a
tolerance, in the stage's local frame, so later transforms carry them
along); the shim evaluates it on the B-rep produced by the preceding
operation. No OCCT edge index ever appears in source or IR.

```
EDGES := SEL (',' SEL)*
SEL   := face                 -- top bot lft rt fwd bak: edges bounding the faces on that side of the bbox
       | face+face            -- top+rt: edges shared by the two (the usual "round this edge")
       | face+face+face       -- a corner: the edges meeting there
       | |x  | |y  | |z       -- edges parallel to an axis ("|z": the vertical edges)
       | seam                 -- edges created by the immediately preceding add/cut/isect stage
       | @name [r]            -- edges within r (default 0.5 mm) of a datum or frame origin
       | all
       | SEL & SEL            -- intersection: "top & |x"
FACES := face (',' face)* | @frame   -- a frame selects the face through its origin, normal to its normal
```

```
plate = box 40 30 4 |> fillet 1 |z |> datum hole top+lft 8 0 0
lid   = box 60 40 3 |> chamfer 0.6 top
boss  = plate |> add $ cyl 5 6 |> on top |> fillet 1 seam     -- round only the new joint
cup   = cyl 20 30 |> shell 1.6 top                            -- open top, 1.6 mm wall
peg   = box 8 8 20 |> draft 1 bot                             -- 1 degree draft above the base
```

Why it is stable: a face word means "the faces on this side of the
current bounding box", which is what `anchor top` already means, and it is
evaluated before later geometry can move the box. `seam` uses OCCT's
boolean history (`BRepAlgoAPI_BuilderAlgo::SectionEdges`, with
`Generated`/`Modified`), which does not depend on edge order. `@name`
reuses the datum namespace, so "capture the datum before it drifts"
carries over unchanged. An empty selection is an error at the stage's
source position ("fillet 1 top+rt: no edges selected"); so is a fillet
OCCT cannot build, naming the selector and the radius.

**Alternative: named faces carried by history.** Tag faces when they are
created (`box 40 30 4 |> face lid top`), carry the names through every
boolean and transform with `BRepTools_History` (the idea behind OCAF's
`TNaming`), and select by name (`fillet 1 lid & wall`). More expressive
across long chains of operations, but it needs history plumbing through
every OCCT call, fails confusingly when a named face is split or
removed, and means nothing to OpenSCAD. Revisit only if the primary
design proves too weak.

## Mesh tolerance

OCCT meshes with `BRepMesh_IncrementalMesh(shape, linDefl, isRelative =
false, angDefl, isInParallel = true)`.

- **Candidate defaults: δ = 0.01 mm, α = 0.1 rad (5.7°).** Use absolute
  rather than relative deflection initially. Ideal circle chord geometry
  suggests roughly 157 segments at radius 50 mm for a 0.01 mm sagitta,
  but actual kernel triangulation must be measured. These parameters are
  meshing controls, not a universal guarantee of feature accuracy or
  printability. Compare mesh size, local dimensions, mating clearances,
  render time and slicer time on the representative parts. Avoid assumptions
  about Bambu's resolution defaults or a universal FFF accuracy limit;
  record the actual slicer profile and measured results before choosing
  final defaults. Smaller deflection cannot repair incorrect topology.
- **Exposure**, in precedence order:
  - CLI: `--deflection=MM` and `--angular=RAD` on OCCT `stl`, `next`
    and `check`. Reject OCCT-specific mesh options for other backends
    rather than silently ignoring them. Default `.scad` compilation is unchanged.
  - Project: a new `.assemble` statement, `mesh deflection=0.01
    angular=0.1`, in the style of the existing `clearance` and `tolerance`
    lines (`tolerance` already means mating-loop tolerance, hence `mesh`).
    Nested `.assemble` files inherit the top level's. A `.coscad` file can
    carry the same line, for `coscad stl part.coscad`.
  - The default above.
- **Manifest.** `coscad.next/1` gains an optional top-level `mesh` object,
  written by each backend, so a bed can be reproduced:

  ```json
  "mesh": {"backend": "occt", "occt": "7.9.3", "deflection": 0.01, "angular": 0.1, "relative": false, "source": "default"}
  "mesh": {"backend": "openscad", "fn": 50}
  "mesh": {"backend": "native-experimental", "fn": 50, "manifold": "3.5.2"}
  ```

  `source` is `cli`, `assemble` or `default`. Optional keys keep the
  schema at `coscad.next/1`; `scripts/bambu-slice.py` needs no change.
- **The OpenSCAD backend is unchanged**: `$fn = 50`, byte-identical
  `.scad`.

## Outputs

- **Mesh (default):** binary STL, written by coscad from the triangulation
  (the shim returns vertex and triangle arrays from `Poly_Triangulation`;
  no STL file round trip), through the in-memory mesh type of the 10-17
  Stage 2. `next` keeps writing `foo_bed<N>.stl`; the packer and
  `bambu-slice.py` are unchanged. Before writing, every body is checked:
  `BRepCheck_Analyzer` valid, a valid outer shell and correctly oriented
  cavity shells for each solid, and a closed, edge-manifold, consistently
  oriented mesh. Enclosed cavities are valid; do not require exactly one shell.
  OCCT triangulations are face-local. Reconstruct shared-edge connectivity
  using topology and edge polygons, including periodic seams, reversed faces
  and locations, while retaining distinct touching bodies. Preserve the existing
  `IndexedMesh` representation until STL serialization; coordinate welding or
  triangle soup must not silently merge topologically distinct vertices.
  Validate results before publishing, with errors at any available source context.
  See [OCCT's mesh architecture](https://github.com/Open-Cascade-SAS/OCCT/blob/master/dox/user_guides/mesh/mesh.md).
- **Exact (optional):** `coscad step part.coscad -o part.step`, `coscad
  step foo.assemble -o foo.step` (the whole assembly, below), `--step` on
  `stl` and `next`, and `--brep` (`BRepTools::Write`) for goldens and
  debugging.
- **Measures** per body: `volume`, `area`, `bbox`, `centroid`,
  `faces/edges/vertices/solids`, `valid` (`BRepCheck_Analyzer`),
  `approximate` (a faceted hull was used) and `meshVolume`. `next` writes
  them into the manifest per body (an optional `exact` object), so
  `verify.py` can use them without any CAD library.

### Optional STEP export and assemblies

Nothing here is on the default print path: STEP is written only by
`coscad step` or with `--step`.

XDE (OCCT's extended data framework: `TDocStd_Document`,
`XCAFDoc_ShapeTool`, `XCAFDoc_ColorTool`, `STEPCAFControl_Writer`) is the
standard way to write a STEP product structure, and it fits what coscad
has: a tree of part definitions and placed instances with names and
colours. The XDE spike wrote and read back a two-level assembly with a
shared definition, two placements, instance names and a colour. So when
asked for, STEP covers the whole `next` project, not just single bodies.

**Mapping from coscad.**

| coscad (`Assemble`, `Plan.Model`, `Variants`) | XDE | STEP (AP242) |
|---|---|---|
| a `.assemble` file (the root, or nested via `fpTrail`) | `XCAFDoc_ShapeTool::NewShape()` assembly label, named from the file stem | `PRODUCT` with an assembly definition |
| a part definition: part name + source + variant (`arParts`, `FlatPart.fpShape`), in its own local frame | `AddShape(shape, makeAssembly = false)` + `TDataStd_Name` | one `PRODUCT` with its B-rep, written once however often it is used |
| an instance (`Inst iName iPart iAff`; `fpCount` copies; `wing#1`) | `AddComponent(asm, part, TopLoc_Location(iAff))`, named `iName` | `NEXT_ASSEMBLY_USAGE_OCCURRENCE` with a placement |
| a mirrored instance (`check` splits mirrored pairs) | a **separate part definition** with mirrored geometry, `wing (mirrored)` | STEP placements are rigid (`AXIS2_PLACEMENT_3D`); a reflection cannot be an instance transform, as `2026-07-24-TOPOLOGICAL` notes for mates |
| a new `color=#rrggbb` hint on a `part` line, else a default per `iMat` | `XCAFDoc_ColorTool::SetColor(label, …, XCAFDoc_ColorSurf)`; `SetInstanceColor` per instance | presentation styles |
| `iMat` (`Aluminium`, `Printed`, `Steel`) | `XCAFDoc_MaterialTool` name and density | material entities, **often ignored by importers**, so the material name also goes in the product description |
| `fastener` lines (`M5x8`, socket or button head) | optional simple screw bodies (head + shank), one definition per spec, instanced per fastener | purchased-part products; behind `--fasteners` |
| beds in `next` | a separate document per bed, parts placed as packed | `foo_bed<N>.step`, for CAM and print-farm tools |

Placements come from the code `check` and `plan` already use
(`Plan.Model`'s `Inst` and `iAff`), so STEP and the assembly checker
cannot disagree. With `--step`, `next` writes the design assembly (`foo.step`, one per
`DesignVariant`) and the bed files, and lists them in the manifest
(`"step": {"assembly": "foo.step", "beds": ["foo_bed1.step", …]}`, plus a
`step` name per body). All of it is one shim call per file, taking the
whole export tree.

**Pitfalls, and what we do about them.**

- **Schema** (see Open questions). OCCT writes AP214 unless told otherwise.
  `Interface_Static::SetCVal("write.step.schema", "AP242DIS")` must come
  after `STEPControl_Controller::Init()` and *before* the writer is
  constructed; set afterwards it was silently ignored in the spike (the
  file still said `AUTOMOTIVE_DESIGN`, i.e. AP214). Default AP242, with
  `--step-schema=ap214` for older CAM. Both carry assemblies and colours.
- **Units.** `write.step.unit` = `MM` (verified: the file declares
  `SI_UNIT(.MILLI.,.METRE.)`). coscad is millimetres throughout.
- **Names.** An unnamed component is written under its label entry
  (`=>[0:1:1:1]` in the spike's read-back), so every label and component
  gets a `TDataStd_Name`: coscad identifiers plus `#n`.
- **Shared definitions.** Only `AddShape` of the *same* `TopoDS_Shape`
  gives one product used many times; building a part twice gives two
  products. The exporter caches definitions by (source, part, variant).
- **Colours.** Part and instance colours are separate, and importers
  differ in which they show (FreeCAD reads both; some CAM tools only part
  colours). Write part colours, and instance colours only where instances
  of one definition differ.
- **Global state.** `Interface_Static` is process-global and not
  thread-safe; exports run only on the kernel thread.
- **Stdout.** OCCT's default messenger prints transfer statistics to
  stdout (seen in the spike); the shim removes that printer at load and
  routes warnings to coscad's diagnostics.
- **Approximate bodies** (faceted hulls) are exported as they are, named
  with `(approximate)`.
- **Size.** The spike's box-minus-cylinder STEP was 19 KB; the three-
  instance assembly 512 entities. Fine for any coscad project.

### verify.py

The `verify.py` scripts check the STL meshes, and that stays: printability
is a property of what the slicer receives, so verify keeps working on the
default STL output and never needs STEP. The mesh checks (edge manifoldness,
and thin walls and booleans with trimesh/manifold3d in the tesseract
scripts) are separate from the backend and stay as they are. Changes:

1. **Shared helper.** `scripts/coscad_verify.py` takes the duplicated
   regex ASCII-STL reader and edge-manifold check out of the model
   scripts. It must also read **binary** STL, since OCCT-backed `next`
   writes binary STL.
2. **Exact numbers from coscad, not from a CAD library.** The first draft
   had verify load STEP through Python OCP; that is dropped. The helper
   reads each body's `exact` measures from the `next` manifest and, for
   clearance and loose-fit checks, the exact pairwise distances that
   `coscad check --backend=occt --json` reports (`BRepExtrema`). No new
   Python dependency.
3. **Two-number check.** Compare B-rep and mesh volumes per body. `δ·area`
   is an initial heuristic for investigation, not a proof of correct or
   incorrect tessellation. Add local dimension, clearance and topology checks.
4. Scripts keep working on OpenSCAD output; exact checks are skipped with
   a notice when the manifest has no `exact` object. Scripts that use
   OpenSCAD as a boolean engine may keep doing so; moving them to `coscad
   check` is optional.

## Relationship to the existing roadmap

This proposal adds an OCCT track to
[2026-10-17-FEA-AND-BACKEND](2026-10-17-FEA-AND-BACKEND.md). It does not
supersede the native tessellation or Manifold stages, remove their pins,
or alter the default backend. Their implementation and historical validation
remain documented in [the experiment record](2026-10-26-FEA-BACKEND-EXPERIMENTS.md).
Any later roadmap/index edits should describe OCCT as an additional backend.

Shared infrastructure should evolve from the existing backend selection,
indexed mesh, validation and manufacturing integrations. Keep the native
backend independently testable and use both existing backends as comparison
oracles. A common lowered IR is a later migration with compatibility gates,
not a requirement to rewrite working code before the experiment.

## Early CAD-to-FEA experiment

Bring single-part BREP/STEP export and one FEA example forward, rather than
waiting for full XDE assembly export. Use a rail or bracket with identifiable
support and load regions. Preserve named geometric regions through export
and volume meshing; verify that the applied force, moment and selected area
still match the intended boundary conditions after refinement.

The current tetrahedral solver uses affine geometry; its Tet10 displacement
interpolation does not make the element geometry curved. OCCT export alone
adds no curved-element formulation, joint contact, large-deformation behavior,
creep, anisotropy or failure model. Curved high-order elements would require
separate solver work and validation. The thin PLA tank study's large-deflection
and joint/seal limitations therefore remain even with better CAD geometry.

Acceptance: a reproducible CAD-to-volume-mesh path, preserved boundary regions,
geometry checks, force/moment balance, residual checks, displacement refinement
and an independent reference or analytic comparison. Compare with the existing
STL-derived workflow and state whether the benefit is geometry fidelity,
region selection, runtime or none of these. Do not claim improved physical
accuracy merely because the source is B-rep.

## Testing

### Pure tier (no OCCT, no OpenSCAD; `cabal test` on any machine)

- For the initial subset, `lower` either succeeds or explicitly refuses each
  unsupported example; unsupported syntax must never crash. If the shared IR
  is adopted later, snapshot the pretty-printed
  `Solid` (a stable text form) under `test/golden/solid/`, next to
  `test/golden/scad/`, with the same `COSCAD_UPDATE_GOLDEN=1` flow.
- If OpenSCAD generation is later migrated to `Solid`, retain byte-identical
  `.scad` snapshots. Keep native regression tests and its accepted path list.
- `Coscad.Hull`: 2D circle hulls and the 3D quickhull against
  hand-checked cases (areas, vertex counts), and the classifier (which
  rule fires) for every hull in the examples.
- `supports` diagnostics: Minkowski refusals, approximate-hull warnings,
  OCCT-only constructs on the OpenSCAD backend, empty or malformed edge
  selectors, `--backend=occt` in a build without the flag. Added to
  `test/Tier/Diagnostics.hs` and the fixtures.
- Parser tests for the new syntax in the existing `!simple` fixtures.

### OCCT tier (`test/Tier/Occt.hs`)

Compiled always; runs only in a `-f occt` build with `COSCAD_RENDER` not
`0`, otherwise reported as skipped.

- **Golden** `test/golden/occt.txt`: `path volume area bbox… faces edges
  vertices valid approximate`, to 1e-6 relative. Separate from
  `geometry.txt`, so an OCCT upgrade only touches its own file.
- **Cross-backend** against the committed `test/golden/geometry.txt`
  (OpenSCAD's numbers; no OpenSCAD needed on the Mac runner). Bounds
  within 0.01 mm, except curved extremes where OpenSCAD's polygon is
  inscribed: there `r·(1−cos(π/50))`, 0.2 % of r. Volume within an
  allowance computed per example from its curved content: for a first
  cut, 0.5 % relative on curved parts and 1e-6 on purely polygonal parts
  (boxes, `Prism n`, polygons), which must agree exactly. Faceted hulls
  get the hull's own allowance. Every disagreement prints both numbers.
- **Mesh check:** the OCCT mesh is closed and edge-manifold, with a
  fixture-qualified B-rep/mesh volume allowance. Record `δ·area` as a useful
  heuristic alongside local geometry checks, not a universal error bound.
- **Crash isolation:** test the production parent/worker path as well as
  individual child fixtures. Inject worker crashes, hangs and malformed replies;
  verify deadlines, cleanup, bounded results, prior-artifact preservation and
  success of the next job. Exception-only tests do not establish isolation.
- **Topology and geometry:** test enclosed cavities, disconnected bodies,
  point/edge contact, tiny gaps, periodic seams, reversed/mirrored faces and
  shared shapes meshed at different tolerances. Check local dimensions and
  clearances and use analytic or symmetric-difference checks where applicable;
  volume/bounds agreement and the `δ·area` heuristic alone are insufficient.
- **STL quality:** every supported example's OCCT STL is closed, edge-manifold,
  consistently oriented and has no degenerate triangles; this is the
  primary OCCT test.
- **STEP read-back** (optional export): write each assembly example, read it back through a
  shim test entry point (`STEPCAFControl_Reader`), and compare product
  tree, names, placements, colours and per-body volumes.
- **`next` volume conservation:** the existing cross-check, with
  `--backend=occt`.

### CI

- **Unchanged:** the `ubuntu-latest` `test` job, the VS Code extension
  job and `release.yml` build with the flag off. No Ubuntu OCCT job. The
  OCCT tier reports "skipped (built without OCCT)".
- **New job `occt`, macOS only**, on `macos-latest` as asked, or pinned
  to `macos-15` (the release runner) if M0 hits the `tapi` link error
  there:
  - `HOMEBREW_NO_AUTO_UPDATE=1 brew install opencascade`, with
    `~/Library/Caches/Homebrew/downloads` restored by `actions/cache`,
    keyed on the formula version (`brew info --json=v2 opencascade`), so
    the bottle and its dependencies are not downloaded every run.
  - GHC 9.8.2 via `haskell-actions/setup`, the cabal store cached as in
    `ci.yml` under its own key (`occt-macos-…`).
  - `cabal build -f occt --extra-include-dirs=$(brew --prefix
    opencascade)/include/opencascade --extra-lib-dirs=$(brew --prefix
    opencascade)/lib`, then `cabal test` with the same flags and
    `COSCAD_RENDER=1`. OpenSCAD is not installed, so the OpenSCAD tiers
    skip and the cross-backend test uses the committed golden.
  - Uploads the STEP and STL of failing examples as artifacts.
  - Exploratory at first; required before the corresponding features are advertised as supported.
- **Pins:** `pipeline.lock.json` records the OCCT version the release was
  built against; `scripts/pipeline-lock.py` and the pin-policy step learn
  about it.

### Mac dev setup

```
brew install opencascade
scripts/occt-env.sh            # writes cabal.project.local with +occt and the opt/ paths
cabal build && cabal test
coscad doctor                  # occt backend: OCCT 7.9.3 (built against 7.9.3)
```

## Milestones

Start with a bounded experiment, then decide whether to expand. The original
37–51 focused-day estimate covered a much broader, unqualified implementation;
it is provisional historical planning, not a delivery commitment. Re-estimate
using measured integration, geometry and packaging results.

| | milestone | deliverable and acceptance |
|---|---|---|
| **M0** | Mac integration and containment | Optional Cabal flag, small shim, primitives/booleans, indexed mesh, single-part STEP/BREP, structured errors and same-executable worker. Pass the M0 gates above locally and on Mac CI. Keep existing backends and OpenSCAD codegen intact. |
| **M1** | Representative parts | Rail, bracket, threaded-globe components and boat hull. Record supported/refused features; compare geometry, clearances, runtime, peak memory, mesh size and Bambu Studio acceptance against both existing backends. Add a capability only as needed for a named fixture; no claim of whole-corpus parity. |
| **M2** | One useful new feature and one FEA example | A selected-edge fillet or useful loft unavailable in the current native subset, plus CAD-to-volume meshing with identifiable load/support regions and displacement convergence. Preserve source provenance for the new feature. Demonstrate practical benefit rather than just a successful API call. |
| **Decision gate** | Continue, narrow or stop | Review M0–M2 evidence and maintenance cost. Qualify the optional distribution on a clean Mac before a release. Adoption for selected workloads is independent of default selection; neither existing backend is retired. |
| **Later A** | Broader parity and manufacturing | Incremental profile/offset/loft/hull/Minkowski support with explicit contracts, `next` integration and validated atomic bundles. Optional shared IR migration with existing-backend compatibility tests. Inventory the entire current corpus and retain explicit refusals. |
| **Later B** | Additional CAD features and checks | Expand fillets, shell, sweep, revolve, draft and threads only against useful fixtures; add B-rep measurements and distances to `check`, shared verification helpers and recorded tolerance provenance. |
| **Later C** | STEP assemblies | XDE shared definitions, names, colours, mirrored/nested placements and optional bed exports. Read-back tests plus manual import in FreeCAD and a target CAM tool. Single-part STEP is already in M0. |

Mac CI can remain exploratory during the spike. Before an OCCT feature is
advertised as supported, its acceptance checks become required for that
configuration. Cross-platform OCCT distribution requires separate qualification;
Mac results do not qualify Linux or Windows.

## Risks

- **Build and link on macOS.** Unverified until M0: header paths, dylib
  install names and rpath, `-lc++`, the `tapi` linker error already seen
  on `macos-latest` in `release.yml`, and OCCT 7.9.3 against the 7.8.1
  the shim was compiled with. Mitigation: M0 checks this; evaluate a
  source-built kernel only if needed, with its own cost and packaging gate.
- **Kernel crashes, hangs and allocations.** Guards and a bound thread do
  not contain these. Supervise production worker jobs, enforce and test budgets,
  retain operation context in the parent and preserve published artifacts.
  Memory isolation on macOS must be demonstrated, not inferred from subprocess use.
- **Homebrew OCCT upgrades.** A soname bump breaks installed binaries
  until coscad is rebuilt; API changes (7.9 to 8.0) break the shim build.
  Mitigations: the distribution gate, pinned dependencies and managed rebuilds,
  plus separate upgrade CI. `doctor` is only available after successful startup.
- **Boolean failures.** OCCT booleans fail differently from CGAL's, on
  near-tangent and coincident curved cases and with invalid results.
  Mitigations: declared operation tolerances, input/output validation and
  explicit diagnostics. Fuzzy booleans, healing and unification can alter
  geometry or feature history: record their use and bound resulting changes;
  do not silently heal away thin walls, holes or clearance gaps. No silent fallback.
- **Fillet failures** on complex edge chains are common in all OCCT-based
  CAD. Mitigation: the selector grammar encourages filleting early on
  simple geometry, and failures name the selector.
- **Approximate hulls.** Rule 6 output is faceted; flagged in measures,
  `validate`, `--strict` and (when exported) STEP names.
- **Mesh size.** The finer default gives more triangles than `$fn = 50`
  on large curved parts; measure part/slicer costs in M1 and `next` costs during Later A.
- **Golden churn.** Two goldens per example; `occt.txt` is separate.
- **Linux.** A `-f occt` build on Linux is untested by policy and may rot.
  Accepted.

## Open questions

Each has a recommended default, used unless decided otherwise.

1. **Fillet selection.** Is the geometric selector grammar (anchor face
   words, `face+face`, `|z`, `seam`, `@datum`, `&`) the right design, or
   should named faces carried through operations be primary? *Default:*
   geometric selectors; named faces only if they prove too weak.
2. **Release artifact.** Choose a Homebrew-managed optional build or a
   relocatable dependency bundle. *Default:* experimental source build until
   the distribution gate passes; then an explicitly optional OCCT artifact.
   Shipping it does not change the default backend or remove the native build.

3. **STEP colours and fasteners** (only relevant to the optional STEP
   export). A `color=#rrggbb` hint on `part` lines plus a default colour per
   material, and are fasteners exported by default? *Default:* yes to the
   hint and material colours; fasteners only with `--fasteners`.
4. **Faceted hulls.** Rule 6 (approximate hull): a warning that `--strict`
   turns into an error, or always an error under OCCT? *Default:* warning,
   error with `--strict`.
5. **Threads.** Plain clearance holes by default (as today), or modelled
   above some size? *Default:* clearance holes; modelled threads opt-in
   per part (`threads=model`).
6. **STEP schema.** AP242 or AP214 by default, and which tool reads the
   files first? *Default:* AP242 with `--step-schema=ap214`; first reader
   to be confirmed (FreeCAD assumed for the later assembly import check).

Already decided (see Decisions): OCCT is an additional backend alongside
OpenSCAD and native/Manifold. One executable provides the parent and supervised
worker modes, using a small C ABI shim without Python for OCCT. Initial OCCT
qualification is macOS-only, the Cabal flag is off by default, STL remains
the default manufacturing output, and existing backend defaults are unchanged.
