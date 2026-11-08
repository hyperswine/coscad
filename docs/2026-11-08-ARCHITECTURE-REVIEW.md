# Architecture review, 8 November 2026

Kind: review. Reviews the module architecture of main `d6882fd` (1.6.0.0
plus Unreleased, 49 Haskell modules, about 11.8k lines) against the
question: have the features remained ideally clean and composable? The
reference points are the [frontend review](2026-10-01-FRONTEND-REVIEW.md)
and the [planner review](2026-10-29-PLAN-REVIEW.md). Nothing was changed
as part of the review; the first fixes are recorded at the end.

**Verdict.** The spine is clean, the leaves have accreted. The bottom of
the module graph is pure, acyclic and honestly factored, and a whole
feature family (datums, frames, attachments) was absorbed in one-line
increments, which is composability working as designed. The erosion is
concentrated in leaf primitives and the 2D-profile semantics, and it has
already produced one live correctness bug: a program the front end
accepts that the native kernel refuses.

## What held up

- **A pure, acyclic base.** `Shape.hs` imports nothing; `Linalg`,
  `Geometry`, `Dsl` sit on it; `Mesh` sits below `Backend`/`Manifold`
  (healthier than the layering sketch in the README). No hs-boot files,
  no cycles, no dead modules.
- **Centralised shared math.** Bounding boxes have one home
  (`Geometry.hs` `bcorners`/`bmerge`/`bbox`); `Mesh.meshBounds`
  delegates to it. `Report`/`Json`/`IO` are dependency-free leaf
  frameworks and the "library never prints" contract holds everywhere.
- **Stdlib dogfoods.** Every recipe is built from the same `Shape`
  surface users write, with `Either String` checks exactly like the
  front end. No internal geometry APIs.
- **Features consume the core through interfaces.** Plan and Stress take
  `Shape`, `Mesh` and `Backend` results; neither re-tessellates. Stress
  numerics are self-contained. `Check`, `Diagnose`, `Doctor` and
  `Printability` are four distinct probes, not re-implementations of
  each other, and all funnel into `Manifold`/`Mesh` shared code.
- **Wrappers compose.** `Datum`, `Frame`, `Anchor`, `Position`,
  `AttachTo`, `CutAt` desugar to plain transforms before anything else
  sees them, so `Dim`, `Validate` and `Plan` each needed one line for
  the whole family. This is the pattern to copy for future features.
- **The OpenSCAD retirement is coherent.** Runtime execution is gone;
  `Codegen` survives as the `.scad` export and the test oracle, which is
  the documented, intended role.

## Where "ideally" has eroded

1. **The 2D-profile algebra exists five times** (concerning). "What is a
   2D profile under a lift" is implemented separately in
   `Codegen.pathOf` (scad text), `Loft.profilePath` (native points),
   `Backend.outlineFor`, the `extruded` profile cases and
   `revolved`/`inlineRevolve`. The copies have drifted: an offset inside
   a loft profile is accepted at parse time and exported to `.scad`
   (`Codegen.hs:183`) but the native renderer refuses it
   (`Loft.hs:53`). The same program validates, exports, then fails to
   compile. The 2D sub-language is closed under different operations
   per consumer: union works under `⮕` and `⟲` but not under `⟰`.
2. **Parallel case-expressions over `Shape`** (concerning). About 25
   walkers enumerate the constructor set, every one ending in a
   catch-all `_ ->`, so each new constructor is a silent 13-module
   checklist (revolve touched 34 files). Exhaustiveness is never
   enforced. `Frustum` is reachable only through the Haskell DSL
   embedding (the haskell-examples suite uses it), not through the
   language -- no glyph, word or recipe builds one -- yet it rides in
   six walkers; it should either be promoted into the language or
   dropped from the DSL, and until then it is the standing example of
   the checklist cost.
3. **The parser pulls downward** (minor). `Parser/Program.hs:18` imports
   `Codegen.loftErrors`, so parsing depends on the emitter; and
   `Program.hs:23` imports `Coscad.Stress.Spec`, so every `.coscad`
   parse carries stress-directive vocabulary. `Stress.Spec` itself is a
   leaf, so there is no cycle, but the arrows point the wrong way.
4. **`Assemble.hs` is the one god module** (concerning). 903 lines
   holding the `.assemble` grammar, the recursive loader, the hint
   validators and the design-stage command. Worse, the plan option
   vocabulary is pinned on its wrong side: `planWeightNames` and
   `planSettingRanges` live in `Assemble.hs:21-25` while the weights'
   defaults and parsers live in `Plan/Model.hs:210-249`, and Plan
   imports Assemble (for `AsmResult`), so adding one plan weight is a
   two-file edit in opposite directions.
5. **Grammar leaf accretion** (minor). Primitives are spelled up to
   three times across hand-written choice lists (`basicShape`,
   `wordShape`, `simpleShape`, `shape2D`); the unknown-stage suggestion
   list is manually parallel to the real choice list; the reserved-word
   list is a third hand list. The editor's `coscad-parser.js` is a
   1,226-line parallel reimplementation of the whole grammar, a textual
   contract with no guard.
6. **Small tables drifting apart** (minor). Six-world-face anchor
   tables exist in five modules (`Geometry.anchorName`,
   `Plan/Model.anchorDir`, `Stress/Spec.faceAnchor`, `Next.faceCandidates`,
   `Plan/Render`'s entry words) and the alias sets already differ.
   `Plan/Geometry.withMass` re-implements the signed-tetra volume sum
   that `Mesh.meshVolume` provides; two 2D convex hulls
   (`Plan/Model.convexHull`, `Printability`); three rounding
   conventions (`Json.jnum3`, `Assemble.roundTo3`, `Plan/Model.fmtMm`);
   local `eps` values scattered across seven modules.
7. **Open findings from the frontend review, still live** (known).
   `getOffsetValue` (`Parser/Core.hs:48-53`) still silently defaults to
   1 for an unsupported radius carrier, so `↯ (cyl 4 2)` offsets by 1,
   not 4; and `instancesOf` still walks the cutter side of a difference
   (`Plan/Model.hs:102`), so a referenced cutter can become a planned
   part. Two stale comments in `Next.hs:31-33` still describe the
   OpenSCAD era.

## Suggested order of fixes

1. Collapse the loft third of the profile algebra into `Loft.hs`:
   move `loftErrors` out of `Codegen`, and make validation speak the
   native truth (`profilePath`'s refusals), not the scad emitter's.
   This removes the parser-to-`Codegen` import and the offset
   divergence in one move.
2. Move `Analysis`/`Where` from `Stress.Spec` into a neutral leaf
   module that the parser and Stress both import; give the plan option
   vocabulary (hint validators, weight names, setting ranges) a shared
   home off `Assemble`. Both are file moves, not redesigns.
3. Fix the two stale frontend-review findings: reject unsupported
   radius carriers instead of defaulting to 1, and stop
   `instancesOf` from treating cutters as placed parts.
4. Decide `Frustum`: promote it into the language or drop it from the
   DSL (the haskell-examples suite is its only producer). When the next
   primitive or lift operator is added, collapse the remaining profile
   algebra into one module and make the walkers share a traversal, with
   no catch-alls.
5. Later: centralise the anchor tables, the mesh volume+CoM fold and
   the rounding helpers; give the JS grammar mirror a generated or
   conformance-tested contract.

## First implemented increment, 8 November 2026

Done the same day, items 1-3 plus the cheap cleanups of item 4:
loft validation now lives in `Loft` and speaks native semantics, so an
offset inside a loft profile is refused at parse time with the fix the
kernel would give; `Analysis`/`Where` moved from `Stress.Spec` to the
new neutral leaf `Coscad.Directive`, and the parser no longer imports
anything from `Codegen` or `Stress`; the plan option vocabulary moved
to `Plan.Options`, imported by both `Assemble` and `Plan.Model`;
`getOffsetValue` is gone -- the `↯` offset comes only from the right
operand's own radius, radius carriers now include
cyl/zcyl/xcyl/ycyl/tube/cone/prism, and anything else is a parse error
naming the fix; `instancesOf` no longer walks cutters; the stale
OpenSCAD comments in `Next.hs` are corrected. Deleting `Frustum` was
attempted and reverted: it has a legitimate DSL consumer
(examples/haskell/ExampleGear.hs), so the promote-or-drop decision of
item 4 stays open. Also folded `roundTo3` into `Coscad.Format` and made
`Json.jnum3` use it, removing one of the three rounding conventions.

## Frustum promoted, 8 November 2026

The promote-or-drop question resolved to promotion, and the same day the
word `frustum r1 r2 h` joined the language in every mode (the
`xcyl`/`ycyl`/`zcyl`/`tube` route: a family word, no new glyph), sitting
on the XY plane like `◎` and `▻`. The DSL constructor was realigned to
the family's radii-first-height-last order. The deciding evidence was in
the project's own models: orbit-tower spells a truncated cone four times
as `(▻ r h1) ∩ (◎ r h2)` with per-site arithmetic, and no other
primitive in the language carries two radii while its compiler cases
(bbox, validation, mesh, emission) were already written and swept.
`loft` of two circles remains the general taper for arbitrary profiles;
a taper parameter on `extrude` stays on the ideas list.
