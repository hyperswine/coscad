# CoScad diagnostic fixes, 12 October 2026

All **15 remaining asserted gaps** from the [original audit](2026-10-11-DIAGNOSTIC-AUDIT.md)
are fixed. Invalid fastener values, duplicate parts, invalid plate settings and
clearances now fail before they can silently change a plan or stall packing.
The follow-up also validates geometry boundaries, consumed part metadata,
recursive quantities, and the grammar cases listed in that audit.

Validation used changes based on `3247fe4d46cccde4366f92cccf90db3876105f0e`.
Existing bow geometry and its golden files were already modified and were
preserved. No printer, slicer, or website deployment was performed.

## The 15 gaps closed

| Inputs | Result |
|---|---|
| Duplicate part declaration | Error at the second declaration, naming the first source location |
| Infinite plate width or negative plate margin | Finite positive dimensions and a non-negative margin required |
| Negative or infinite clearance | Finite, non-negative distance required |
| `M0x0` | Screw diameter and length must be positive; use `peg` for a friction joint |
| `inset=NaN` | Finite, non-negative distance required |
| Fastener `×0` | At least one joint required; remove the declaration to omit it |
| `at=oops` or `at=NaN` | Finite, non-negative positions in mm required |
| `torque=-2` or `torque=oops` | Finite, positive torque in Nm required |
| `nut=drpin` or `head=buton` | Unknown enum value error with a close spelling suggestion |
| `through=oops` | Finite, non-negative thickness in mm required |

Fastener options form a closed set, so misspelled keys also fail. Recognized part
hints (`material`, `profile`, `ends`, `mass`, `torque`) are validated; custom hints
such as `seam=rear` are retained. Polymer material names use the printed torque
class, including on premade parts. Repeated part/fastener options and repeated
plan keys across multiple lines are errors. Statement bodies stay on their own
lines, so a missing path or fastener field cannot consume the next declaration.

## Geometry and grammar

- Invalid dimensions, inverted tube radii, excessive chamfer/rounding, fractional
  prism sides, zero mirror normals, and collapsing scale factors are rejected.
  Negative scale remains a valid reflection; zero Z scale on a 2D profile is
  allowed. Zero tube inner radius, a tapered apex, and rounding at the supported
  half-dimension boundary also remain valid.
- Prism side counts are whole numbers from **3 to 4096**, checked before integer
  conversion. Loft heights must be finite and strictly increasing. Bounds that
  overflow during transformations fail before SCAD generation. Large finite
  numbers no longer overflow the output formatter, and vector normalization
  avoids overflow/underflow for extreme finite mirror normals.
- Missing box dimensions name the argument. Partial `at`/`on`/`cutat` offset
  triples report the missing offset instead of backtracking into a child-shape
  error. Unknown pipeline stages and shape names receive unique close spelling
  suggestions without changing the input.
- Unknown, repeated, or misplaced syntax mode directives fail with a source
  caret. A valid directive may have a trailing comment. A Unicode minus in a
  direct numeric argument explains how to replace it. Missing arguments to
  `coscad next` now show that command's usage.

Print and fastener quantities are capped at **10,000**, including recursive
print expansion and totals. Multiplication is checked before converting to
machine integers. Plate packing also validates direct API arguments and rejects
non-finite or non-positive footprints before its packing loop.

While testing expanded quantities, a separate planner bug surfaced: IDs computed
as `declaration * 100 + index` collided when a group contained more than 100
joints. IDs are now assigned uniquely after expansion. A regression plans 101
peg joints plus one further joint and verifies that all 102 appear in the steps.

## Verification

`stack test` passed **382 checks, zero failures**, including example SCAD
snapshots, assembly loading, planning, and the OpenSCAD geometry/checker tier.
This follow-up adds 72 regression checks beyond the first diagnostic increment.
Normal examples were rendered; malformed/extreme corpus inputs were not rendered.

The final [443-probe CLI run](evidence/2026-10-12-diagnostic-data/after.json) contains
193 curated cases and 250 deterministic single-character mutations:

| Result | Count |
|---|---:|
| Accepted | 65 |
| Rejected with a normal diagnostic | 378 |
| Valid controls accepted | 27 of 27 |
| Required rejection cases rejected with their expected messages | 163 of 163 |
| Unmet status/message expectations | 0 |
| Raw runtime exceptions / timeouts | 0 / 0 |
| Expected rejections that generated output files | 0 |

All original 419 cases were rerun, including the 15 failures; 24 new fixtures
cover further boundaries and regressions. Mutation outcomes are observations:
an edit can preserve valid syntax. The longest invocation took 0.7044 seconds;
each had a three-second timeout and a single runtime capability. The raw JSON
preserves every input, command, response, exit code, duration and output filename.

Reproduce with the commands in the [corpus README](../test/diagnostics/README.md),
then run `stack test`. `git diff --check` also passes.

## Deliberate boundaries

Custom part metadata remains extensible, so an unknown key such as `materail`
is still metadata; only recognized keys and their consumed values are validated.
No parser precedence, bounding-box attachment rules, or print-versus-placement
count semantics were changed. Geometry validation does not prove a non-empty
CSG result or discover physical insertion paths. These checks apply to parsed
CoScad inputs; direct Haskell constructors remain a lower-level interface.

The quantity and prism limits bound those specific expansions, not arbitrary
input size, CSG complexity or planner search time. Stable diagnostic codes and
structured LSP output remain future work. The corpus passes locally but is not
yet wired into CI.
