# Assemblies and the manufacturing stage

## .assemble files (design stage)

Union inside a `.coscad` = one printed solid. A separate reference in a
`.assemble` = a separate physical object. That file boundary is the
decomposition primitive for the whole pipeline.

```
plate 250 250 6                       # bed W D margin (optional)
center ← center3.coscad ×1
larch  ← l_arch3.coscad ×1 seam=rear  # free key=value hints ride to the slicer
screws ← m4x20.coscad ×0              # ×0 = assembly-only (premade part)
sub    ← corner.assemble ×2           # recursive; counts multiply, cycles detected
asm = center ⊕ (larch |> move 55 -13 -7) ⊕ ...
```

- `▽anchor` on a reference declares print orientation (that face on the
  bed). Undeclared parts get the automatic search in `next`.
- The `asm` expression is ordinary CoScad over the part names — poses
  are the one legitimate home for absolute `move`s.
- Running `coscad foo.assemble` emits: `foo_asm.scad` (view),
  `foo_plate.scad` (packed check), `foo_part_<variant>.scad`
  (print-oriented, at origin), and `foo_manifest.json`.

## coscad next (manufacturing stage)

`coscad next foo.assemble` produces what a slicer consumes:

1. **One mesh per variant** — a variant is a unique (part source,
   orientation). Instances of the same variant are the same geometry,
   so the manifest encodes *slice once, stamp many*: per-variant mesh +
   per-instance XY offsets.
2. **Orientation**: declared `▽` always wins. Otherwise the six axis
   faces are scored on the real mesh: + bed-contact area, − overhang
   area (down-facing triangles steeper than ~65°, off the bed — lying
   cylinders' flanks deliberately don't count, they self-support),
   − height, − a slenderness penalty (tall skinny prints wobble).
   Chosen face, mode, and score are recorded in the manifest for audit.
3. **Packing**: largest-footprint-first shelf packing, spilling to as
   many beds as needed; each `foo_bedN.stl` is one watertight multi-body
   solid, every placement inside margins.

`COSCAD_OPENSCAD` overrides the OpenSCAD binary (point it at an
`xvfb-run -a openscad "$@"` wrapper on headless machines).

## Known limitations / roadmap

- Shelf packing is axis-aligned bounding boxes: no rotation (a 246 mm
  limb fits a 220 bed diagonally but won't be placed), no nesting of
  L-shapes.
- Orientation candidates are the six axis faces; tilted optima aren't
  searched.
- Bed Z is currently a constant (250) pending a fourth `plate` arg.
- Fit/tolerance metadata rides as opaque hints; a first-class interface
  schema (mating pairs, fit classes) is deliberately not standardized
  yet.

## Worked example

`examples/assemble/bow3/` — a three-part recurve bow (two chiral
topologically-modelled arches + through-bolted center with captive-nut
pockets). All joints, bolt paths, and nut fits verified at 0.00 mm³
interference; `coscad next` packs all three on one 250×250 bed with
auto-chosen orientations.
