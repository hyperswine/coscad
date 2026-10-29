# Build planner review

Kind: review. Reviews `coscad plan` as of 1.6.0.0 (main e89b77e) against
the question "does it account for how easy the build is, without
hardcoding?" The working reference is [PLAN](2026-09-25-PLAN.md); the
follow-up work this review proposes is recorded separately when done.

## Summary

The planner finds an order that is physically possible. It does not judge
well which order is easy. Nothing in it names a model, but its sense of
"awkward" is six hand-set penalties evaluated on bounding boxes, not a
model of how a person handles the parts.

## What works

- **Hard feasibility.** A part rests on the bench or on placed parts, or
  is held against something it joins. The partial assembly balances on
  its contact polygon. Driver cylinders are checked against the other
  parts' oriented boxes, nuts must be able to enter their slots, and
  nothing may go under the assembly without a flip.
- **Search.** A beam over rest-face phases (width 30), optionally A*,
  chooses the order, the rest faces and the flips. Every assembly on the
  manual site plans: the Orbit Tower in 19 steps and no flips, Tesseract
  II and III in 36 steps each.
- **Generic.** No part names, coordinates or sequences are built in.
  Everything comes from parts, mates, `fastener` lines and hints.

## Where "easy" falls short

1. **No insertion path.** The planner checks where a part ends up, never
   how it gets there. PLAN says a peg joint "describes the joint seat,
   not a checked collision-free insertion path". A bar that can only drop
   into its hubs from one side is treated like any other part, and a cap
   that would block that drop is not noticed: Tesseract III needed
   `after=` hints so each cap follows the bars it captures.
2. **No joint motion.** Tesseract III's struts, hubs and caps screw
   together. There is no thread joint type, so the plan says "push until
   it seats", counts no turns, and never checks that the part can spin
   while it is turned.
3. **Holding is barely modelled.** "Loose" counts placed but unfastened
   parts at a cost of 1 each per move. Holding three parts at once while
   starting a thread costs almost nothing, and nothing asks whether a
   loose part falls out when the assembly is flipped.
4. **Boxes, not shapes.** Support, contact, balance and mass come from
   bounding boxes. Mass is box volume times a fixed density times a 30%
   fill, although the native meshes are available. A diagonal strut's box
   overstates where it touches and where its weight is.
5. **No hand access.** Room for a screwdriver is checked; room for the
   fingers that push a peg, hold a nut or twist a cap is not.
6. **It needs hand-holding.** Tesseract II needed a temporary jig part
   and `after=` lines. Tesseract III needed generated `fastener peg`
   lines and cap-ordering hints, or every part "would float". The
   physical ordering knowledge lives in the assembly file, not in the
   planner.
7. **Constants decide.** Flip 10, standing rail 6, tilted driver 3,
   shrinking footprint 2, sibling screw 1, loose part 1 are adjustable per
   assembly, but the thresholds behind them are not: 30° for "not
   vertical", 0.3 for "standing", 3:1 for "slender", a 45 mm driver, a
   2 mm driver margin, tau 2.5 in the phase search, at most 4 parts per
   step, the densities and fills. None is derived from the parts.
8. **No calibration.** No weight has been checked against a real build,
   so "cost 92" means nothing in time or effort.
9. **Domain special cases.** 2020 rails (`profile=2020`) and metric screw
   heads are recognised; other hardware is not.

## Proposed work, in order of payoff

1. **Declare how each joint moves.** A `joint` line names the kind of
   motion (`push`, `drop`, `screw`) between two parts. Its axis comes
   from the mate between them, so it adds no new geometry. A screw joint
   takes its pitch.
2. **Check the motion against the meshes.** Sweep the moving part along
   its approach with the kernel's exact gap and intersection queries. A
   blocked path is a hard failure; a tight one costs by its Fitts index
   (engaged length over clearance). The same sweeps tell when a part is
   captured (it can no longer leave in any direction), which is what the
   cap `after=` hints were standing in for. Screw joints also sweep the
   part's rotation about its axis.
3. **Use the real geometry.** Mass and centre of mass from the mesh
   (printed parts as perimeter shell plus infill), bench contact from the
   lowest mesh vertices, and part-on-part contact patches from the
   kernel.
4. **Model holding and hands.** A part is retained by a fastened joint,
   by capture, or by resting stably under gravity; anything else needs a
   hand. Count hands per move (one for the action), charge steeply for a
   third hand, and check finger room around each joint action.
5. **Calibrate.** Make the remaining thresholds plan options with
   documented defaults, report each step's cost terms in the plan JSON,
   and add a command that fits the weights to timed builds.
