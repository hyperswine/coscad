# Frame-based fasteners and instruction repairs

Implemented 12 October 2026. The earlier migration report records the
pre-repair baseline.

## Generic model

`fastener M4x12 arm center @hole seat=@head nut=hex pocket=@nut` selects a
host hole frame, a clamped head bearing frame and a host nut bearing frame.
`pocket=clamped.@nut` is also supported. The compiler resolves these in the
solved instance poses, checks their axes and directions, and calculates
thread reach from the bearing-seat distance. A recessed head does not move
to the clamped part's outer box. Driver checks and red screw markers use
the same actual head seat. Nut opening positions are calculated by rays
through the holder's local bounds and then transformed into world space.
Rail-slot checks use rail-local coordinates, including under arbitrary
rigid rotations. Skew screw collision checks use finite segment distance.

`preload=hand` describes nut preparation on an independent part. It creates
nut-before-holder placement dependencies, permits turning that independent
part, and emits a retention note. The default remains `preload=placed`.
Preparation notes survive step grouping. Flips warn about loose parts
that must be held together. No model names, precomputed world coordinates,
fastener counts or example sequences are selected in the compiler.

Existing face/at/through/inset declarations remain supported. Frame joints
are one screw per declaration and reject conflicting offset conventions,
missing/ambiguous/point-only frames, reversed or misaligned seats, invalid
rail slot positions and insufficient screw reach. `coscad validate` now
checks hardware without searching for a plan or creating artifacts.

## Repaired examples

- **Corner:** bracket1 previously used Rx(-90), sending its second leg
  downwards below the upright. Selecting the other bore with its appropriate
  in-plane up axis gives Rx(-90) Rz(-90). Both bores of each outer bracket
  now close against rail frames, so restoring the original wrong rotation
  is rejected as conflicting constraints. All six M5x10 screws have hole
  and head frames, with 5 or 5.1 mm reach into their T-nuts. The generated
  plan has five parts, six screws, four steps and one flip.
- **Toy bow:** four distinct root holes, four M4x12 screws and four captive
  M4 nuts. Head seats are z=4.3, nut bearing seats z=-3.7, giving 8 mm grip
  and 4 mm reach. The generated plan preloads the centre in hand, seats and
  fastens each arm in turn, in three steps without flips.
- All six changed part files emit **byte-identical solid OpenSCAD** to HEAD
  280c328. Corner bracket1's assembled pose intentionally changes, and that
  repair propagates to recursive/mirrored corner assemblies. Print counts
  are unchanged. The original incorrect coordinate fixture remains intact;
  `corner-repaired.assemble` is a separate independent coordinate oracle.

## Verification

- Full Stack suite with real OpenSCAD renders: **812 passed, 0 failed**.
- Final non-rendering rerun after retaining preparation notes:
  **709 passed, 0 failed**. Includes renamed/resized examples, arbitrary
  rotations, local rail slot validation, actual nut entry/head positions,
  short screws, missing/reversed/misaligned/ambiguous seats, duplicate holes,
  blocked drivers and restoration of the original wrong corner rotation.
- **468 CLI diagnostic probes**, zero expectation failures/crashes/timeouts.
- Rebuilt local manual: seven generated build guides, including the repaired
  corner, with fresh renders and required-image checks.
- Chromium: **76 page/viewport checks** (19 pages at 320, 375, 768 and 1440
  pixels), including long names, sticky navigation and loaded images.
- LSP 0.3.1: parser and extension tests, **12,328 token hover positions across
  94 current source files**, plus stdio integration for cross-file hardware
  frame hover, definition and completion, including unsaved part buffers.
  VSIX packaged and installed; VS Code reload is required for activation.

The manual build generates these instruction pages under `/builds/corner/`
and `/builds/bow3/`.

## Remaining accuracy boundary

Bore/seat frames, hardware specifications and preparation mode are explicit
model metadata. They are not discovered from arbitrary boolean solids.
Bounds-based support/access is still approximate; frame validation alone
does not prove that a hole is empty, a real screw fits, or the default torque
is suitable for a specific print. No physical assembly or print was run.
