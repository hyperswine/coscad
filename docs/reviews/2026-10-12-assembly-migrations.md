# Assembly frame migration verification

The remaining fourteen canonical assembly / packing examples now use `fixed`
and `mate`. The already converted cube and frame-stand remain unchanged.
Original coordinate expressions are retained under `test/fixtures/assemblies`;
`test/MigrationCases.hs` maps old repeated-instance names to new aliases.

## Scope

| Assembly | Top-level aliases | Mates | Treatment |
|---|---:|---:|---|
| autotest | 6 | 0 | Independent packing samples |
| corner | 5 | 5 | Rail and bracket interfaces |
| frame | 2 | 0 | Two independent recursive corner assemblies |
| spill | 7 | 0 | Independent packing samples |
| corner_pair | 3 | 3 | Rail end and both bracket supports |
| ball | 3 | 2 | Core and upper/lower shell interfaces |
| bow3 | 3 | 4 | Both root bore stations on each limb |
| tesseract | 29 | 40 | 24 bar ends, 8 diagonals and 8 inner-cube ends |
| Orbit Tower | 20 | 19 | Peg seats, clocked rotors and satellites |
| one-piece bow | 1 | 0 | Fixed single part |
| bow2 | 3 | 4 | Both root bore stations on each limb |
| enclosure2x2 | 49 | 48 | Rails, brackets, nut seats, corners and panels |
| example_check | 3 | 0 | Preserve deliberately overlapping collision fixture |
| greaser | 1 | 0 | Fixed single part |

Repeated copies have separate aliases, each with ×0 or ×1. Fastener and
`after=` references follow those aliases. Disconnected test pieces have
independent fixed roots; no artificial mechanical connection is invented.

The recursive frame's second corner was reflected in the coordinate source.
A reflection changes handedness and cannot be represented by a rigid mate.
`corner-mirrored.assemble` therefore retains that reflection in its geometry;
the parent fixes the resulting subassembly at its original location.

## Geometry evidence

- Full `stack test --fast --test-arguments='+RTS -N1 -RTS'`: **777 passed,
  0 failed**, with the actual OpenSCAD geometry tier enabled.
- All 36 modified `.coscad` files, including the Orbit Tower pilots, emit
  byte-identical solid OpenSCAD to baseline `c6a8496`. The additions are
  construction metadata; part dimensions, bores and printable solids stay intact.
- Each migrated assembly preserves its bounds and flattened print quantity.
  Independent tagged probes compare each instance's origin and three axis
  points against the coordinate reference. These catch rotations even for
  symmetric geometry. The reflected wrapper's local handedness is included.
- Twelve assembled unions pass strict CGAL symmetric-difference rendering
  within 0.00001 mm³. Renderer warnings remain failures.
- Oblique tesseract / Orbit Tower poses produce degenerate slivers when CGAL
  subtracts nearly coincident faces. The regression uses per-instance probes
  for these, and separate warning-free **Manifold** renders measured residuals
  of **0.0076503524 mm³** and **0.0000087969 mm³**, respectively.
- Tesseract's rounded 35.2644° diagonal is replaced with the exact
  `atan(1/sqrt(2))` angle so its closed graph agrees. Its bound/probe tolerance
  is 0.0001 mm; the others use 0.000001 mm.
- Eleven topological/native pilot comparisons, eight resize scenarios and
  the canonical/topological/native Orbit Tower plan comparison passed again.
  See `experiments/topological/assembly-pilots-results.json`.

Packing-only fixtures intentionally retain coincident display copies so their
manufacturing counts and original displayed union remain unchanged. The
collision fixture intentionally retains interference. This verification does
not claim every fixture has interference-free fit, or establish physical
print/assembly success for unprinted models.

## Instruction evidence

The planner compares all declared joints with the coordinate references,
after renaming aliases, and verifies each physical instance is placed once.
All six published build pages regenerate with no planner design errors:

| Build | Parts | Steps | Declared fasteners |
|---|---:|---:|---:|
| corner_pair | 3 | 3 | 2 |
| cube | 29 | 28 | 38 |
| bow3 | 3 | 1 | 0 |
| ball | 3 | 3 | 2 |
| tesseract | 29 | 28 | 40 |
| Orbit Tower | 20 | 19 | 19 |

A broader fixture check also reproduced the old `corner.assemble` planner
rejection, with the same messages after alias renaming: one bracket would
float and another would be placed underneath the current assembly. Its
geometry and constraint graph load, but its missing support/order metadata
still prevents a usable instruction plan. It is not one of the six published
instruction examples.

Orbit Tower retains rotor-before-capture prerequisites. Mates determine
geometry; explicit fasteners and placement prerequisites still determine
instruction order. The bow source has never declared its four M4 fasteners;
its generated guide remains placement-only. This migration does not invent
screw lengths, driver locations or a complete fastening guide for it.

## Manual and editor verification

- Responsive layout: **72 page/viewport checks** over 18 pages at 320, 375,
  768 and 1440 px, including long unbroken component names, instruction-image
  loading, sticky navigation and contained dropdowns. Browser JavaScript
  errors fail the check. Reproduce with `scripts/test-manual-layout.cjs`.
- Documentation, build indexes and step pages share persistent navigation.
  Tables/code scroll internally; cards and images stay within their content
  area. Step keyboard shortcuts ignore typing in search fields.
- `coscad validate` checks part source and recursive assemblies / rigid
  constraints without rendering, packing or writing output. Six CLI probes
  assert that it creates no files. The complete diagnostic probe run passed
  **468 cases**, with zero expectation failures, crashes or timeouts.
- CoScad LSP 0.3.0 recognizes the new topology and constraint syntax, `after=`
  lists and peg inset. Hover coverage passed **11,944 token positions** across
  94 active compiler examples/models, including operators, `=`, numeric
  literals and punctuation. Bindings show numeric/shape types; literals and
  options explain units or counts where known. Unresolved context is labelled.
- Actual stdio LSP integration passed hover, cross-file frame definitions,
  completion edits without doubled `@`, semantic tokens and unsaved frame
  changes. Compiler diagnostics use `validate` on parts and assemblies;
  source/frame existence and graph solving remain compiler-authoritative.


The deployed updater now explicitly discovers the server's `~/srv/BOSL2`
checkout, fails if `coscad doctor` fails, and removes copied previews before
rendering. The manual postprocessor verifies every expected step image exists
before the atomic publish, so a failed renderer cannot reuse old previews.
