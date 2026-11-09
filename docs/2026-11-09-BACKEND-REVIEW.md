# Geometry backend review

Status: recommendation, 2026-11-09. Based on the current checkout and upstream
documentation; no new comparative benchmarks or physical print trials were run.
This supersedes the backend-selection recommendation and current-state assumptions
in [the OCCT proposal](2026-10-26-OCCT-BACKEND.md), while preserving its historical
spike results and useful integration requirements.

## Recommendation

Keep native Manifold as the default. Do not replace it with OCCT now. Consider
OCCT as an optional capability when a concrete part requires post-boolean edge
fillets or useful STEP interchange. First invest in tolerance-controlled
tessellation, actionable geometry diagnostics and regression coverage for real
parts. These are priorities, not claims that those improvements are implemented.

## Current implementation

[Backend.hs](../src/Coscad/Backend.hs) has a single `Native` backend: CoScad
constructs meshes and uses the pinned Manifold kernel for solid operations.
[The kernel lock](../native-kernel.lock.json) pins Manifold 3.5.2 and Clipper2;
the sources are vendored and compiled into the library. OpenSCAD and BOSL2 are
not runtime requirements. `.scad` remains an export, and OpenSCAD can serve as
an independent test oracle. See [OpenSCAD retired](2026-10-29-OPENSCAD-RETIRED.md).

The existing assembly, interference-checking, manufacturing and image paths
consume meshes. The [shape AST](../src/Coscad/Shape.hs) supports rounded and
chamfered primitives, but has no general operation for filleting an edge formed
by a boolean. An OCCT integration is still proposed, not selectable today.

## What would justify a change?

| Need | Recommended direction |
|---|---|
| Printable parts, booleans and assembly checks | Keep Manifold; fix demonstrated failures with regression cases |
| Smoother curves or tighter dimensional approximation | Improve tessellation controls and measure approximation error first |
| Fillets/chamfers on edges formed by booleans | Prototype OCCT and a usable edge-selection contract |
| STEP exchange with CAD/CAM tools | Prototype OCCT from the shape representation, preserving analytic geometry where supported |
| Organic blends or lattices | Investigate an implicit/SDF feature path; this alone does not justify replacing the kernel |
| Faster rendering or fewer geometry failures | Benchmark representative failing or slow models before selecting a replacement |

Manifold is designed for manifold triangle meshes and robust mesh booleans.
OCCT supplies curve/surface modeling, fillets, meshing and STEP interchange.
Those capabilities make OCCT a useful candidate for additional CAD features;
they do not establish a speed or reliability improvement on CoScad's workload.
See [Manifold documentation](https://manifoldcad.org/docs/html/) and
[OCCT documentation](https://occt3d.com/dev/doc/overview/html/).

Another mesh kernel would need a demonstrated advantage on actual CoScad cases.
A switch merely to obtain smoother tessellation is not justified by this review.

## Integration cost and acceptance

OCCT requires more than a C++ binding. Define tolerances, shape validity,
edge selection and stable feature references, error reporting, and parity for
existing hulls, Minkowski operations and polygonal lofts. B-rep validity does
not prove that a generated STL is printable. Converting an existing triangle
mesh into STEP alone does not recover the original analytic geometry.

Start with one representative part that needs a boolean-edge fillet or STEP:

1. Preserve the current native path and make the experiment explicit and opt-in.
2. Compare dimensions, bounds and volume within declared tolerances; check
   watertightness, manifoldness and expected connected bodies in the STL.
3. Demonstrate the requested fillet or reopen the STEP in a target CAD tool;
   a successful file write alone is insufficient.
4. Measure runtime, peak memory and mesh size against the native equivalent
   where the feature can be represented, recording any semantic differences.
5. Qualify packaging on the intended platform. Before pipeline adoption,
   implement the proposal's supervised-worker deadlines, cancellation, bounded
   resources, failure recovery and validated atomic output publication.

Only expand the experiment after it demonstrates a concrete benefit. Changing
the default would require broader corpus and manufacturing-pipeline validation;
a successful optional feature does not by itself justify that change.
