# Assemblies and the manufacturing stage

## .assemble files (design stage)

Union inside a `.coscad` = one printed solid. A separate reference in a
`.assemble` = a separate physical object. That file boundary is the
decomposition primitive for the whole pipeline.

```
plate 250 250 6 250                   # bed W D [margin [max height]] [brim=N|auto]
center ← center3.coscad ×1
larch  ← l_arch3.coscad ×1 seam=rear  # free key=value hints ride to the slicer
screws ← m4x20.coscad ×0              # ×0 = assembly-only (premade part)
sub    ← corner.assemble ×2           # recursive; counts multiply, cycles detected
asm = center ⊕ (larch |> move 55 -13 -7) ⊕ ...
```

- `plate W D margin maxZ`: the optional 4th argument is the tallest part
  the printer can build, in mm. It defaults to 250 (A1, P1, X1); set it
  for smaller printers, e.g. `plate 180 180 6 180` for an A1 mini. Give
  the margin to give a height (the margin defaults to 6).
- `▽anchor` on a reference declares print orientation (that face on the
  bed). Undeclared parts get the automatic search in `next`.
- `×n` with n > 0 is the print count. `next` prints n copies, while `plan`
  and `check` see the placements in `asm`. When the two differ, every
  command warns at the declaration (exit status unchanged): printing
  spares while showing one copy is allowed, but no longer silent. `×0`
  parts may be placed any number of times. A subassembly reference counts
  its own placements in the parent.
- `brim=N` on a reference reserves an N mm brim around each of its
  instances when packing (see Packing below); `brim=0` opts out of a
  plate-wide brim.
- The `asm` expression is ordinary CoScad over the part names — poses
  are the one legitimate home for absolute `move`s.
- Running `coscad foo.assemble` emits: `foo_asm.scad` (view),
  `foo_plate.scad` (packed check; `foo_plate1.scad`, `foo_plate2.scad`,
  ... when the parts need more than one plate), `foo_part_<variant>.scad`
  (print-oriented, at origin), and `foo_design.json` (schema
  `coscad.design/1`; each placement carries its `plate` index, and
  `rot_z` when the packer turned it). A part whose footprint exceeds the
  plate either way round is a design error (exit 2) naming the part and
  the plate. The design-stage plate is an approximation: it packs the
  CoScad shape's bounding box in its declared orientation (undeclared
  parts as modelled), while `next` packs the rendered, oriented mesh. A
  part taller than the plate's max height there only warns; `next`
  refuses it.

## Manifests

The two stages write different files with different shapes, and each
carries a `schema` field so a consumer can tell them apart before reading
anything else:

| file                | written by           | `schema`          | shape                                   |
|---------------------|----------------------|-------------------|-----------------------------------------|
| `foo_design.json`   | `coscad foo.assemble`| `coscad.design/1` | `plate`, `variants`, `placements`       |
| `foo_manifest.json` | `coscad next`        | `coscad.next/1`   | `bed`, `variants` (+`printability`), `beds` |

`scripts/bambu-slice.py` accepts only `coscad.next/1` (and manifests with
`beds` but no `schema`, from coscad 1.2 and earlier). It refuses a design
manifest, or a newer `coscad.next/N`, with the command to run instead.

**Migrating from 1.2.** The design stage used to write
`foo_manifest.json` too, so running `coscad foo.assemble` after `coscad
next` silently replaced the print manifest with one the slicer could not
use. Now the design stage writes `foo_design.json` and never touches
`foo_manifest.json`. If it finds an old design manifest under that name it
prints a `Note:` to stderr and leaves the file alone; delete it or run
`coscad next`. Scripts that read the design manifest should read
`foo_design.json`; scripts that read the print manifest keep reading
`foo_manifest.json` and may check `schema == "coscad.next/1"`.

## coscad next (manufacturing stage)

`coscad next foo.assemble` produces what a slicer consumes:

1. **One mesh per variant** — a variant is a unique (part source,
   orientation). Instances of the same variant are the same geometry,
   so the manifest encodes *slice once, stamp many*: per-variant mesh +
   per-instance XY offsets.
2. **Orientation**: declared `▽` always wins. Otherwise the six axis
   faces are scored on the real mesh: + bed-contact area, − overhang
   area (down-facing triangles steeper than ~65°, starting more than
   0.3 mm, the first-layer band, above the bed — lying cylinders' flanks
   deliberately don't count, they self-support), − height, − a slenderness
   penalty (tall skinny prints wobble).
   Faces taller than the plate's max height (`bed.max_z`, the 4th `plate`
   argument) are never chosen. A part that is too tall in its declared
   `▽`, or on every face, is a design error (exit 2): the message gives
   its height, the limit, and the faces it would fit on. No beds or
   manifest are written.
   Chosen face, mode, and score are recorded in the manifest for audit.
   Each variant also records `printability` (analysed once, on the oriented
   mesh): `bed_contact_mm2`, `first_layer_mm2`, `overhang_mm2`,
   `min_width_mm`, `elevated_start_heights_mm` and `warnings`. Warnings are
   printed even for declared orientations, and are the same list as the
   manifest's. They are:
   - **small first layer**: the down-facing area within the 0.3 mm
     first-layer band (projected), below max(25 mm², 0.05·h²). A lying rod
     has no flat contact but a strip of first layer, and does not warn.
   - **tall and narrow**: height over the narrowest width of the
     footprint's convex hull (rotating calipers, so a wall turned 45° is as
     narrow as it is) above 2.5 + 10/h; short parts get more room.
   - **flat overhang**: down faces above the first-layer band (the same
     faces the score penalises), grouped into regions. A region edge is
     supported where the face beyond it goes down. Area is unsupported if it
     is more than 2 mm from a supported edge and no line through it meets
     supported edges on both sides within 30 mm (a bridge). Warns above
     max(25 mm², 2 % of the surface). A region with no supported edge is an
     island and is reported as an elevated start instead.
   - **elevated start**: a downward-facing local minimum above the bed.

   `warning_codes` gives one stable code per warning, in the same order:
   `small_contact`, `tall_narrow`, `flat_overhang`, `elevated_start`.
   `coscad next --strict` exits 2 if any variant has a warning. It writes
   the beds and the manifest first, so the warnings can be reviewed. The
   default stays warn-only.

   The thresholds live in `PrintabilityConfig` (`defaultPrintabilityConfig`).
   **Bed contact** has one definition, used by both the score and
   `printability.bed_contact_mm2`: the area of down-facing triangles (normal
   within ~26° of straight down) whose three vertices lie within the plane
   tolerance of the lowest point. The tolerance is 0.01 mm (1e-5 of the
   largest dimension for parts over a metre): well under a layer, but wide
   enough for STL quantisation and faces put down along a diagonal `▽`.
   These are conservative mesh diagnostics: curved first-layer contact,
   bridges, cooling and complete support coverage still need slicer review.
3. **Packing**: first-fit decreasing shelf packing, spilling to as many
   beds as needed; each `foo_bedN.stl` is one watertight multi-body
   solid, every placement inside margins.
   - **Order and fit:** instances go largest reserved area first (ties in
     declaration order, so packing is deterministic). A bed stays open
     until every remaining instance has been tried; one that fits
     nowhere waits for the next bed, and later, smaller ones still fill
     the gaps. Each instance takes the first of: the open shelf as
     declared, a gap at the end of an earlier shelf, any shelf turned
     90° about Z, a new shelf, a new shelf turned.
   - **Turning:** a turned instance is rotated +90° about Z (and shifted
     back onto its corner) in the bed mesh. The placement records
     `rot_z: 90` and its footprint as placed (`w` and `d` swapped);
     `rot_z` is 0 otherwise. The variant mesh and its `footprint` stay
     as oriented. A part that fits the plate only turned is turned
     instead of refused.
   - **Brim reservation:** an instance with a brim is packed as its
     footprint grown by the brim on every side. The plate margin and the
     gap to its neighbours are kept outside the brim. With margin 6 and
     `brim=8`, a part sits 14 mm from the plate edge and 22 mm from
     another 8 mm-brim part.
   - **Which instances get one:** a part's own `brim=N` hint always applies
     (`brim=0` opts out). Otherwise `plate … brim=N` (`brim=auto` = 5 mm,
     Bambu's default) applies to parts whose printability recommends a
     brim (`small_contact` or `tall_narrow`).
   - **In the manifests:** each placement records `brim_mm` and keeps the
     part's own `x`, `y`, `w`, `d`. `bed.brim_mm` records the plate
     setting.
   - **Design stage:** it reserves only the hints, because it doesn't
     analyse meshes.
   - **Ordering:** by reserved area, ties in declaration order, so packing
     stays deterministic.

`COSCAD_OPENSCAD` overrides the OpenSCAD binary (point it at an
`xvfb-run -a openscad "$@"` wrapper on headless machines);
`COSCAD_BOSL2` points at a BOSL2 checkout outside the library folders.
`coscad doctor` verifies both. OpenSCAD warnings during a render fail the
stage: a bed built from a part whose include did not resolve is not a
bed you want to print.

## Known limitations / roadmap

- Shelf packing is axis-aligned bounding boxes turned by 0° or 90° only:
  no other angles (a 246 mm limb fits a 220 bed diagonally but won't be
  placed), no stacking within a shelf, no nesting of L-shapes.
- Orientation candidates are the six axis faces; tilted optima aren't
  searched.
- The brim is reserved per instance, but Bambu Studio slices each bed as
  one object with one brim width. `bambu-slice.py` reports parts whose
  reservation is narrower than the brim it prints.
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
It warns when the manifest's plate is bigger than the printer: its area
against the machine's `printable_area`, and its `bed.max_z` and any part
on the selected beds against `printable_height` (an A1 mini builds
180 mm; set `plate W D margin 180` and rerun `coscad next`).

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

**Printability gate.** `bambu-slice.py` reads each variant's
`printability` and prints the warnings for the parts on each bed it
slices, with what addresses them.
- **What addresses which warning:**
  - `small_contact` and `tall_narrow` need an explicit brim: `brim_type`
    `outer_only`, `inner_only` or `outer_and_inner` with `brim_width` > 0.
    `auto_brim` doesn't count; it produced no brim on the failed tesseract
    coupon.
  - `flat_overhang` and `elevated_start` need `enable_support`.
  - Warnings without a code (manifests from coscad 1.2) need either.
- **Refusal:** if anything is left unaddressed, the script exits 2 before
  slicing and writes nothing. `--accept-printability` slices anyway.
- **Record:** `<base>_print.json` records the outcome under `printability`
  (`accept_printability`, `unaddressed`, `brim_mm`, `supports`,
  `brim_from_manifest`, `brim_conflicts`, and each warning with its bed,
  variant, instances, code, need and `addressed_by`). Each bed also lists
  its `printability_warnings`.
- **Reserved brim:** when the manifest reserves a brim and the process
  overrides set neither `brim_type` nor `brim_width`, the widest reserved
  width on the sliced beds is printed as an outer brim
  (`brim_from_manifest`). An explicit brim that is wider than a part's
  reservation plus half the margin is reported, since brims may merge.
- **Temporary directories:** the preset and slice directories are removed
  on every exit path, including slicer failures.

`--process-overrides FILE.json` applies explicit settings such as
`{"brim_type":"outer_only","brim_width":"8"}` to the resolved process.
`--filament-overrides FILE.json` handles filament settings such as early-layer
cooling separately. Values use Bambu's string/list-of-strings encoding;
unknown keys are refused. Both sets of overrides are recorded in `_print.json`.
An automatic brim setting does not guarantee brim toolpaths: inspect the
exported G-code. The tesseract adhesion retry includes a repeatable offline
preflight that verifies actual brim and support extrusion.

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
coscad ball.assemble                     # views, plate, ball_design.json
coscad check ball.assemble               # 0 overlaps, 0.17 mm gap shell/core
coscad next ball.assemble                # ball_bed1.stl: 3 parts on one bed, ball_manifest.json
coscad plan --png ball.assemble          # 3 steps, 2 flips, hex-nut preload
scripts/bambu-slice.py ball_manifest.json   # ball_bed1.gcode.3mf, 43 min, 17.6 g PLA
```
