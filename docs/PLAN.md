# Build plans: `coscad plan`

`coscad plan foo.assemble` turns an assembly into a numbered build sequence
a person can follow without backtracking: nuts go in before the slot they
need is consumed, every screw is driven with its driver clear, each step
names which face rests on the bench, and every fastener carries a torque
for its host's material. The design rationale is in the assembly
instruction generator design note; this file is the working reference.

## What you write

Everything the planner needs beyond the existing `asm` placement is a few
part hints and one `fastener` line per screw group:

```
rx ← rail200.coscad ×0 profile=2020 material=aluminium
rz ← rail160.coscad ×0 profile=2020 material=printed ends=open,blocked
br ← flat90.coscad ×16

fastener M5x10 br#1 rx#1 bot at=30            // spec clamped host face [options]
fastener M5x10 px  ry#2 rt ×2                 // two screws, spaced along the overlap
fastener M5x10 br1 railA top through=6        // counterbored bracket: 6 mm under the head
plan beam=30 rest=bot,top,fwd,bak             // optional planner settings
```

- `profile=2020` marks a rail: all four long faces are T-slots, both ends
  open unless `ends=open,blocked` (min end, max end along the rail).
- `material=aluminium|printed|steel` sets the torque class; parts printed
  by this assembly (`×n`, n > 0) default to `printed`, premade (`×0`)
  parts to `aluminium`. `torque=` on a part or a fastener overrides.
  `mass=` (grams) overrides the volume-based estimate.
- `fastener spec clamped host face`: `spec` is `M<d>x<len>`; `clamped` and
  `host` are instance names (`br#3` when a part is placed more than once,
  the bare name when once); `face` is the host face the screw enters
  (`top bot lft rt fwd bak`, world frame). Options: `×n` screws spaced
  along the overlap, `at=mm[,mm]` positions from the host's marked end,
  `nut=dropin|slidein|hex`, `head=button|socket`, `through=mm` (material
  under the head when the clamped part is thinner at the hole than its
  bounding box, e.g. counterbored), `torque=Nm`.
- `nut=hex pocket=face`: the host is not a rail; the screw meets a plain
  hex nut captive in a pocket that opens on that host face. Nuts still go
  in before the part that covers them, the pocket face may not be on the
  bench or under another part while they go in, and the plan says
  "hex nut into the pocket on the top face" instead of slot positions.
  `examples/assemble/ball/` (two hemisphere shells bolted to a core disc)
  is the worked example.
- `plan` options: `beam=N` (search width; 30 default, wider is slower
  and better), `rest=faces` to restrict allowed rest faces, cost weights
  `flip vertical driver shrink sibling loose`, `astar=N` to try exact
  search with an expansion cap first.

Accessibility is judged on bounding boxes: a driver is blocked only when
its cylinder really enters another part's box (touching does not count),
so a shell whose box brushes the neighbouring shell's box, as in the ball,
does not block the screw on the other side.

The planner derives the driver axis (the host face's outward normal), the
screw positions (centre of the overlap between clamped part and host
face, or `at=`), the thread reach (length minus material under the head),
and the T-nut preload sheet.

## What it checks

Design errors (reported, and the run exits 1):

- screws that collide inside a host (same spot on one face, or two faces
  meeting at the same station with enough reach to cross);
- too little thread reaching the T-nut (under 3 mm);
- a clamped part that does not sit on the named host face.

Hard predicates on every step: a free part must rest on the bench or on
placed parts and balance on that support (a part about to be screwed may
be held against its host); nothing may reach under the partial assembly
(flip instead); the assembly's centre of mass stays over its bench
contact; a screw's driver cylinder is clear of everything but its own
parts; T-nuts can still enter (drop-in: the slot face is exposed;
slide-in: an end is open and unsealed).

Costs (weights in the `plan` line): flip 10, host rail not lying flat
while its nut is started 6 (also a rail stood on end), driver more than
30° off vertical 3, support polygon shrinking 2, leaving a sibling screw
on the same bracket 1, and 1 per move for each placed part nothing holds
yet.

## Outputs

- `foo_plan.md`: bill of materials, T-nut preload sheet (nuts per rail,
  slot, mm from the marked end), then numbered steps: rest face, parts
  placed, rails preloaded, screws tightened with torque, warnings.
- `foo_plan.json`: the same as data.
- `foo_stepN.scad`: the scene after step N, rest face down, this step's
  parts orange, earlier parts grey, screw heads red, the bench as a
  translucent slab. `--png` also renders `foo_stepN.png` through OpenSCAD.

## Companion site

`coscad site DIR a.assemble b.assemble ...` plans each assembly with
renders and writes a static site: `DIR/index.html` (search across builds
by name, part, material, screw spec) and `DIR/<name>/index.html` (step
cards with the render, rest face, parts, preloads, screws with torque,
warnings; prev/next buttons, arrow keys, a filter box, `#step-N` links).
It is plain HTML with the plan JSON embedded, so it works from a file or
any static host (GitHub Pages, a folder on the router box). Light and
dark themes, safe-area padding for phones.

## How the search works

Nodes are part instances, one T-nut batch per rail, and fasteners; edges
are requires-before relations derived from the model (host and clamped
part before a screw, a rail's nuts before its screws and before any part
covering the slot, slide-in nuts before anything sealing an end). A state
is the placed set plus the rest face. Placing a part settles its
follow-ups (nuts that can now go in, screws whose driver is clear). The
search is a beam over rest-face phases: on one face it greedily takes
every cheap move; it branches on flipping to another face or on accepting
one expensive move. A completion heuristic charges for parts that will
need a flip to reach and for screw groups that cannot be vertical on the
current face.

Search width matters on larger frames. For the 29-part cube fixture the
plan cost was 208 / 183 / 179 / 167 at beam 10 / 30 / 80 / 200, in 3 / 10
/ 25 / 60 seconds; the corner example is instant at any width.

## Limits

- Access and support use bounding boxes, so an L bracket blocks its own
  inner corner and a driver beside a bracket edge may be judged blocked.
- Screw heads and nuts are not solids: contact is between part boxes.
- One torque class per material; the printed default (0.6 Nm) is a guess
  to measure on an offcut.
- Sub-assemblies are not discovered; each recursive `.assemble` is a part.
- Fixtures, clamps, and third hands are not modelled: a warning asks you
  to steady a rail stood on end.
