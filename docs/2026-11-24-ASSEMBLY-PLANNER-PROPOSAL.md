# Assembly planning for the workbench

**Proposal, 24 November 2026.** Teach `coscad plan` to evaluate a simple temporary support when a protruding part prevents assembly on a flat bench. Start with one setup that can be described, checked and reproduced: two parallel packing strips under a stationary part. Retain the ordinary flat-bench plan as a comparison. The first milestone is a credible one-person build sequence for the M5 propeller gearbox.

## What the gearbox exposed

The test used the installed geometry of the base, input gear, three idlers, output gear and lid. The planner put the lid on last and explicitly warned about loose parts. Its three-step plan nevertheless starts with the base standing on end, turns the assembly twice, and asks for another pair of hands or a jig. With the rest face fixed to the bottom, the search reached a state containing only the input gear and reported that the base would float. The underside of the input journal projects about 0.95 mm below the case. Raising the entire bench plane would still leave the interference. Two strips could support the case on either side of that journal, provided the input gear's insertion path also clears them. This proposed sequence awaits a bench trial.

The seven-part test omits the motor bridge, collar, propeller and eight M5 fasteners. To reproduce it, export the named solids `base`, `driver`, `idler`, `output` and `cover` from the M5 source as separate same-named CoScad files beside the fixture, then run `coscad plan train.assemble`. For the constrained run append `plan rest=bot`. [Gearbox source](../models/tt-propeller-gearbox/m5/gearbox.coscad) · [Fixture](evidence/2026-11-24-assembly-planner/train.assemble) · [Unconstrained plan](evidence/2026-11-24-assembly-planner/train_plan.md) · [Bottom-down result](evidence/2026-11-24-assembly-planner/flat_plan.md)

## Proposed changes

1. **Represent the bench setup explicitly.** In `Plan.Model`, carry the support footprints, height, orientation and supported part as part of the search state. In `Plan.Geometry`, generate a bounded set of strip pairs from the stationary part's underside contact patches in the allowed rest orientations. Check both strips contact material, the projected centre of mass lies inside the support polygon with a reported edge margin, and protrusions clear the bench. Repeat the stability check as the centre of mass changes. Sweep each insertion against the strips as well as the surrounding parts. A support that obstructs a shaft, screw or driver must be rejected for that operation.
2. **Count the actual handling.** In `Plan.Search`, compare ordinary and supported starts. Charge for placing and removing strips. Track which loose parts must be held during each turn, and whether the operator still has a hand free for the next operation. A declared press fit or installed fastener can supply retention; mere contact or gear mesh cannot establish it. Offer a one-person search setting. Preserve distinct early states for each viable base and support setup so beam pruning does not discard the only useful starting order. Report candidate counts and elapsed time before increasing any search budget.
3. **Explain the setup and the result.** In `Plan.Render`, show where each strip goes, its dimensions, which protrusion needs clearance, and when the assembly can be lifted off. Keep temporary shop aids out of the product BOM. Distinguish final-pose interference, a clear final pose with no tested insertion path, instability, and a search limit. If the search exhausts its budget, report the last viable partial sequence and the limiting operation.

## Acceptance tests

| Case | Evidence required |
|---|---|
| Seven-part gearbox | Produce a bottom-down sequence with the base first, lid last, zero flips and no second operator. Report strip placement and bench clearance. Replay every placement against that same setup. |
| Invalid support | Move a strip under the input journal, then move both strips to one side of the centre of mass. Reject the first for interference and the second for tipping. |
| Improvement beyond the gearbox | Test an unrelated base with a protruding locating pin, then mirror or rotate it. Require a valid setup without part-name rules or gearbox-specific dimensions. |
| Closed shell and overlapping solids | A loose insert inside a sealed shell must fail the insertion-path check. An insert sharing solid volume with its host must report final-pose interference. Include an open-shell control that succeeds. |
| Existing assemblies | Preserve the rail corner's nut preload and short-screw diagnostic, the SG90 keeper order and the Orbit Tower's declared dependencies. Compare runtime, flips and required hands with the same inputs and settings. |

## Delivery order and physical check

First land the geometry fixtures and capture the current outcomes. Next implement explicit strip placement and collision replay, then automatic candidate generation and ranking. Only promote the new behavior after it passes an unrelated assembly and a one-person gearbox build. During that build, record strip dimensions, unexpected handholds, loose gears and any step that requires lifting the case. A disagreement with the printed parts becomes a new fixture.

Then extend the gearbox fixture to the motor bridge, external hex nuts and split propeller collar. Check access for both the screw driver and the hand or tool holding the opposite nut. This later trial is needed before claiming the planner can produce the complete gearbox instructions.
