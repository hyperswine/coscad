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
  `foo_plate.scad` (packed check; `foo_plate1.scad`, `foo_plate2.scad`,
  ... when the parts need more than one plate), `foo_part_<variant>.scad`
  (print-oriented, at origin), and `foo_manifest.json` (each placement
  carries its `plate` index). A part whose footprint exceeds the plate
  is an error naming the part and the plate.

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
`xvfb-run -a openscad "$@"` wrapper on headless machines);
`COSCAD_BOSL2` points at a BOSL2 checkout outside the library folders.
`coscad doctor` verifies both. OpenSCAD warnings during a render fail the
stage: a bed built from a part whose include did not resolve is not a
bed you want to print.

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

## Slicing and printing: Bambu Studio CLI

`scripts/bambu-slice.py foo_manifest.json` takes the beds from `coscad
next` through Bambu Studio's command-line slicer (`BambuStudio.app`,
`BAMBU_STUDIO` overrides the path) and writes `foo_bedN.gcode.3mf`, the
file a Bambu printer accepts, plus `foo_print.json` with print time and
filament per bed. Options: `--printer` (machine preset, default
`Bambu Lab A1 0.4 nozzle`), `--process` (default: the printer's default
profile), `--filament` (default: from the parts' `material=` hint, else
the printer's default), `--plate textured|cool|engineering|hightemp`,
`--bed N` for one bed, `--slicer-arg` to pass anything else through.

The script flattens Bambu Studio's own system presets before loading them:
the CLI does not resolve `inherits`, and an unflattened filament preset
silently slices with density 0, flow limit 2 mm³/s and a 200 °C nozzle
(the bracket example took 17 min that way and 11 min with the real
profile). It also merges the printer's `<printer> template <key>.json`
files (start, end, filament-change, layer-change and timelapse gcode),
which the GUI adds and the CLI does not: without them the print runs a
generic placeholder start sequence that never loads filament, so the
head moves and nothing comes out (the first ball attempt).

`--print N` uploads bed N to the printer's SD card over implicit FTPS and
starts it over MQTT (`project_file` command). It needs a printer in LAN
mode with developer mode on, `BAMBU_HOST`, `BAMBU_SERIAL` and
`BAMBU_ACCESS_CODE` in the environment, `paho-mqtt`, and the bambu-lan
scripts (`BAMBU_LAN_DIR`) for the FTPS client with the printer's TLS
quirks. Nothing is sent without `--print`.

`scripts/bambu-print-run.py foo.assemble` does the whole thing unattended:
it photographs the printer (`<timestamp>-a1-printer-capture.png`, from the
printer's own camera), refuses to start if a print is running, runs
`coscad`, `coscad next`, `coscad plan` and `bambu-slice.py --print`,
watches the print over MQTT with a progress line a minute, photographs it
again when it stops, and writes `foo_run.json` with both photos, the
printer's state before and after and an OK/FAILED verdict (state FINISH,
no print error, our file). `--dry-run` stops after slicing. The access
code is read from `~/.config/bambu/a1.env` (`BAMBU_ACCESS_CODE=...`); host
and serial are discovered on the LAN when not given.

Worked example, `examples/assemble/ball/`: a 40 mm ball from two printed
hemisphere shells and a core disc with two captive M5 hex nuts. The whole
chain is

```
coscad ball.assemble                     # views, plate, manifest
coscad check ball.assemble               # 0 overlaps, 0.17 mm gap shell/core
coscad next ball.assemble                # ball_bed1.stl: 3 parts on one bed
coscad plan --png ball.assemble          # 3 steps, 2 flips, hex-nut preload
scripts/bambu-slice.py ball_manifest.json   # ball_bed1.gcode.3mf, 43 min, 17.6 g PLA
```
