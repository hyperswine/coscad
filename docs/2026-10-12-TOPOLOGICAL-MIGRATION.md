# Topological example migration: initial findings, 12 October 2026

The current CoScad language can already express anchor-based versions of many
existing parts. Nine pilots preserve their nominal solids and print frames
within the measured numerical tolerance, and six dimension-change checks show
that their intended features follow the appropriate anchors. This is enough
to begin migrating ordinary mechanical parts and prefer this style in new
examples. It is not yet a complete migration of every example or assembly.

The bow and other pending model work was committed and pushed separately as
`1e14e52d4864cb5a60bc7df3d0f4a62d631d9104`; the associated P1S maintenance note
was pushed in `3d-models` as `1ca83ffba8013d49ffaef6d5a55bba839b647619`.
All ten model source files compiled with the current compiler. The full
382-check suite from the diagnostic work included the reinforced bow and its
updated goldens. The topology work below is a separate local experiment.

## Definition to use for migration

Follow [the existing convention](2026-07-24-TOPOLOGICAL.md): place features relative
to suitable anchors, with meaningful local dimensions. A hole 20 mm from an
edge and a socket 5.6 mm deep still need numbers. Intrinsic Bezier control
points define a curve; they are not necessarily misplaced features.

For part internals, replace coordinate translations with `at`, `on`, `cutat`
and `anchor`. Prefer named dimensions for insets, depth, pitch, and deliberate
overlap. Preserve the original print frame so existing assembly poses,
fastener hints, and manufacturing orientation keep their meaning.

The essential check is **stable design intent**, not simply removing `move`.
A feature anchored to an evolving composite centre can drift. An offset copied
from an old global coordinate can disguise that same problem. Put the feature
on the smallest suitable parent, or use a stable edge/corner datum, and test a
dimension change that would expose an incorrect dependency.

Assembly poses remain coordinate-based in this pilot. Simple chains can use
the existing relational operations, but a fully relational frame has additional
questions about instance identity, roll/orientation, and which feature mates
to which. Converting all assembly poses is a separate stage.

## Inventory

The [recorded inventory](../experiments/topological/results.json) covers
73 active `.coscad` files in `examples`, `examples-next`, and `models`;
archived examples, Haskell examples, raw SCAD, and assembly poses are excluded.
It counts placement syntax after removing comments, so these are candidate
categories rather than semantic judgements.

| Source family | Coordinate placement only | Mixed anchors/coordinates | Anchors, no coordinate placement | No placement operation |
|---|---:|---:|---:|---:|
| `examples` | 22 | 6 | 7 | 12 |
| `examples-next` | 8 | 0 | 5 | 3 |
| `models` | 8 | 1 | 1 | 0 |
| Total | 38 | 7 | 13 | 15 |

The 15 simple shared-origin models often need no change. The 13 anchor-only
files still need semantic review: for example `panel190.coscad` uses fixed
`±80` offsets from a face centre rather than an edge inset. `tbracket.coscad`
uses a centre anchor after adding geometry. The curved bow arms describe their
root interfaces relationally, but still position the stalk trim, hook, nock and
cord hole with coordinates. Translation tutorials should continue to teach
translations; they are not production-style migration failures.

## Nine pilots and evidence

[Sources, runner and reproduction](../experiments/topological/README.md)
are separate from the canonical examples.

| Pilot | Relationship made explicit | Nominal symmetric difference |
|---|---|---:|
| Coin cylinder | Slot depth below the top face | Empty |
| Flat 90-degree bracket | Cutout and mounting holes relative to outer edges | Empty |
| Corner panel | Interior/perimeter edge insets | Empty |
| Reinforced bow centre | Root pockets below the top, nuts above the bottom, grip below the top | 0.000173 mm³ |
| Orbit base | Taper on the foot, peg overlapping the taper | Empty |
| Orbit axle | Journal and peg stacked; fixed socket depth from the bottom | Empty |
| Orbit cap | Roof on the body; socket from the bottom | Empty |
| Orbit satellite | Neck, shaft and tip follow their preceding top faces | Empty |
| Orbit rotor | Bores belong to the symmetric hub/arm before asymmetric pads are added | Empty |

Both versions of each nominal part rendered as one connected, closed manifold,
with matching mesh bounds to 0.00001 mm. Eight Boolean symmetric differences
are empty. The bow difference is confined to a layer only
0.00000095367431640625 mm thick at the recess floor, consistent with numerical
roundoff when a hull is evaluated and translated in a different order. This
is recorded as a tolerance comparison, not an empty residual. The comparison
requires both residual volume ≤0.001 mm³ and minimum extent ≤0.00001 mm.

The six resizing cases check actual geometry, not just compilation:

- Increase the coin body from 50 to 70 mm: the slot floor follows to Z=44.
- Increase the bow body from 14 to 18 mm and its width from 36 to 42 mm:
  grip depth, root-pocket depth and tested bolt paths remain correct.
- Increase the flat bracket span from 40 to 50 mm: mounting holes follow
  the edges; material returns at a former hole location.
- Resize the panel to 250 ×230 mm: all four edge insets remain correct.
- Increase the axle foot from 7 to 10 mm: the journal/peg stack follows,
  while the bottom socket retains its 5.6 mm depth.
- Increase the satellite disc from 4 to 6 mm: its total height increases
  from 10.8 to 12.8 mm, with one connected solid.

There are 18 independent small material/cavity probes across those cases;
all pass. These are chosen dimension changes within the intended designs,
not a proof for every possible parameter value. The runner also regenerates
Orbit Tower instructions using all five converted parts and compares the
complete plan data apart from the source filename.

## What needs more language support or design work

1. **Named feature datums.** Current operations use a shape's bounding box.
   They cannot directly select a surviving shoulder, a hole axis, a sketch
   endpoint, or an earlier feature's frame after unrelated geometry grows.
   Such references would help the curved bow, glider, and complex brackets.
2. **Placement without adding the datum solid.** `at` and `on` include both
   parent and child. A way to position a cutter or hull endpoint against a
   separate datum while returning only that feature would avoid duplicated
   construction geometry. The rotor pilot currently drills both symmetric
   pieces before unioning them to avoid an asymmetric centre.
3. **Explicit mating features and orientation.** `on` aligns +Z to an anchor
   direction; general assembly joints also need a chosen roll, parent and
   child mating features, instance identity, and a policy for closed loops.
   The cube's correctly rotated brackets are a useful acceptance fixture.
4. **Repeated features.** The egg box uses a Python generator for its N×M
   cells. Stable-corner/pitch offsets are possible today, but native repetition
   would make the whole parameterized authoring process CoScad.
5. **Unary hull convenience.** Hull placement can be expressed today, but a
   hull of an attached group requires repeating one member with the binary
   hull operator. That workaround appears in the bow and rotor pilots.

## Suggested route to the default style

Start with an authoring convention, preserving existing language semantics.
Migrate panels, bores, simple brackets and stacked solids after reviewing
equivalence, fit probes and resize behaviour. Then migrate the more involved
brackets and generated grids. Keep curved sketches' intrinsic coordinates,
and introduce named feature references before forcing their attachments into
long lists of bounding-box offsets.

For each canonical migration, require the same print frame, a Boolean solid
comparison, relevant mating/clearance probes, at least one useful resize check,
and unchanged instruction data when it is part of an assembly. Review SCAD
and mesh golden updates only after those checks. Move coordinate construction
examples into an explicitly educational section where appropriate.

Once that first group is accepted, update the main examples, `docs/SKILL.md`,
and new-model templates to teach anchor placement first. A later opt-in style
lint can flag coordinate placement in part internals while allowing assembly
poses and sketch coordinates. The current lexical inventory is a starting
point for that lint, not a compiler-enforced topology mode. No compiler
default or canonical source has been changed by this investigation.

Follow-up: [the first language features are now implemented](2026-10-12-TOPOLOGICAL-FEATURES.md):
named construction points, child-only placement, native grids and unary hull.
That report records their validation and the remaining assembly-frame work.
