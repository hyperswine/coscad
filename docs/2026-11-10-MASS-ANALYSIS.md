# Shared mass and balance analysis

The first implementation increment from the mass/assembly-analysis roadmap adds shared
mesh moments, a native mass command, and a reusable Bambu sliced-mass reader. It does not
change the geometry backend or add `.assemble` bonded FEA yet.

## Implemented

- `Mesh.meshVolumeCentre` computes closed-mesh volume and uniform-density centre in one
  shared calculation, translated near the mesh before summation to reduce cancellation
  for models far from the origin. `meshVolume` and planner geometry use this calculation.
  Empty/degenerate meshes have no volume centre; the planner retains a bounding-box
  fallback. Mesh orientation/closure must be correct; this function is not a mesh repairer.
- `coscad mass part.coscad` reports native solid volume (mm³), surface area (mm²) and
  centre (mm). Add `--density 1.24` in **g/cm³** for solid mass in grams. Without density,
  `mass_g` is null. The command does not infer shell, infill or filament settings.
- `coscad mass components.json` combines explicit component masses and assembly-frame
  centres, including hardware. Names must be unique; masses positive and finite. Missing
  or invalid components fail instead of silently disappearing from the balance.
- `scripts/slice-mass.py` computes per-object deposited-filament mass and centres from
  Bambu relative-E toolpaths. Declared rigid transforms map plate coordinates into one
  assembly frame. Explicit hardware and optional measured-part masses complete the report.
- Reports use `coscad.mass-report/1`, identify their estimation basis and record source
  hashes. Output files are replaced atomically individually; paired report/component
  exports are not a multi-file transaction. Outputs cannot overwrite their input paths.

## Native command

```sh
coscad mass examples/simple_transform.coscad --density 1.24 -o solid-mass.json
coscad mass examples/mass/components.json -o assembly-mass.json
```

Use an existing `.coscad` model for the first command. Density 1.24 is an explicit example,
not a material default or calibration. Native reports include volume, area, centre, density,
mass, source path/hash and limitations. Components reports include each declared component.
Both report the same total-mass and centre keys; consumers must inspect `basis` before
comparing solid and printed estimates.

Component input:

```json
{
  "schema": "coscad.mass-components/1",
  "components": [
    {"name":"body", "kind":"measured", "mass_g":100, "center_mm":[0,0,0]},
    {"name":"bolt", "kind":"hardware", "mass_g":10, "center_mm":[110,0,0]}
  ]
}
```

This has mass 110 g and centre `[10,0,0]` mm. Supported kinds are `solid`, `sliced`,
`measured`, and `hardware`. A measured mass does not imply a measured centre. All centres
must already be expressed in one common frame. No hardware is generated implicitly
from geometric mates or fastener declarations. Combined screw/nut entries are appropriate
only when their declared centre represents their combined mass moment.

## Slice input and coordinate contract

```sh
python scripts/slice-mass.py examples/mass/slice.json -o sliced-mass.json \
  --components-out sliced-components.json
coscad mass sliced-components.json -o combined-mass.json
python scripts/test_mass.py
```

The included short toolpath is an analytical fixture, **not a printable program**.
Python requires numpy; install the optional dependency with
`python -m pip install -r scripts/requirements-mass.txt`.

A `coscad.slice-mass/1` manifest declares `dialect: "bambu-relative-e"`, a `jobs` array
and an optional `hardware` array. Each job declares:

- `gcode`: path relative to the manifest, read as UTF-8.
- `density_g_cm3` and `filament_diameter_mm`: explicit positive values for that job.
- `extruder_offset_xy_mm`: optional `[x,y]`, default zero, **added to G-code coordinates**
  to recover plate coordinates. Use the machine preset's offset convention.
- `objects`: unique integer object IDs and globally unique names, each with a
  `plate_to_assembly` row-major 4×4 matrix. It multiplies a column `[x,y,z,1]` vector.
  Rotation, reflection and translation are allowed; scale/shear are rejected because
  measured filament mass must not silently change with geometry scaling.
- An object's optional `measured_mass_g` replaces its estimated mass, while keeping its
  sliced centre. The report preserves `deposited_mass_g` and `center_basis` to show this.

Hardware entries have `name`, `mass_g` and `center_mm`, already in assembly coordinates.
Object IDs come from slicer annotations, not guessed naming conventions. For a single
object, the reader can attribute model extrusion after the first layer annotation without
an object label. Multi-object files require labels. Unknown IDs fail.

Linear paths use their midpoint; XY IJ arcs use the analytic circular-arc centroid, including
full circles. Layer centres use `Z_HEIGHT - LAYER_HEIGHT/2`. Retraction, stationary
unretraction, travel, custom purge, support, brim, wipe and skirt are excluded. Modeled
mouse ears are part of the model toolpaths and remain included.

The reader deliberately accepts a bounded dialect: absolute XYZ in mm, relative E in
filament millimetres, G17 XY single-turn IJ arcs, and known Bambu feature annotations.
Absolute-E deposition, relative-XYZ deposition, inch units, volumetric extrusion,
multiple selected material tools, non-100% flow overrides, coordinate resets, unknown features, radius/multi-turn
arcs and unsupported extrusion commands fail. It is not a general G-code interpreter;
new dialects need fixtures and explicit implementation before being accepted.

## Validation and limits

Analytic regression cases cover box volume/centre, translation by 10⁹ mm, reversed winding,
disconnected solids, explicit hardware moments, invalid masses/names, density conversion
and absent-density behavior. Python cases cover line/arc moments, transformations,
measured overrides, object attribution, support exclusion and unsupported modes.

The reusable report was also run on all four existing balanced-glider slices, using their
actual machine offsets, placements and six screws/six nuts. It reproduces the established
218.37507208386234 g total and centre within 10⁻¹⁰ mm; this is an integration parity check,
not an independent physical calibration. G-code and manifest hashes identify those inputs.

The planner retains its existing shell/infill/rail mass approximation and explicit `mass=`
overrides. Sharing the centroid calculation does **not** make that approximation a sliced
mass estimate. The new slice reports are opt-in and do not silently change planner weights.
There is no inertia tensor, automatic uncertainty bound, aerodynamic stability verdict,
strength verdict, or printer operation in either mass-analysis command.

## Next increments

1. Add a shared resolved-instance export for `.assemble` so callers can generate mass
   transforms and component identity without model-specific Python adapters.
2. Use the same identity and region contracts for bounded assembly FEA supports, loads
   and bonds; geometric `mate`/`fixed` must retain their placement meaning.
3. Preview selected analysis regions and per-component mass centres.
4. Consolidate printer preparation/provenance separately from analysis; analysis must not
   dispatch jobs. Compare printed masses and loaded-assembly deflections with measurements
   before promising calibrated accuracy.

## Local validation record

The native build passes with `-Werror`. The full native suite passed **1,676 checks,
zero failures**, with the OpenSCAD oracle explicitly disabled. A final evaluation-
strictness-only mesh accumulator cleanup was followed by **435 focused checks,
zero failures**. The Python analytic/CLI suite passed **9 tests**. The documentation
consistency check and whitespace check pass. CI coverage has been added but hosted CI
has not been run for these uncommitted changes.

[Validation record and final source hashes](evidence/2026-11-10-mass-analysis/validation.json)
include the glider parity result. These checks establish software behavior, not physical
mass accuracy, aerodynamic stability or structural strength.
