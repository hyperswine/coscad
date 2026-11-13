# Running CoScad batch jobs with Sol

Run these commands from the repository root. The ports use the FP-RISC
0.13.1 release bundle (Sol scripting API 1). To select that installed bundle
for these commands, set `SOL` to its executable.

```sh
export SOL="$HOME/.fpr/0.13.1/bin/sol"
"$SOL" scripts/bonded-shelf.sol --output-dir /tmp/shelf --refinement 2
"$SOL" scripts/assembly-region-preview.sol /tmp/regions.json /tmp/regions.svg
"$SOL" models/tesseract-v3-threaded/instructions.sol \
  models/tesseract-v3-threaded/tesseract.assemble /tmp/instructions.assemble
"$SOL" models/tesseract-v3-threaded/export.sol
"$SOL" scripts/render-boats.sol models/beach-outrigger --out /tmp/outrigger
"$SOL" models/egg-box/generate.sol --columns 3 --rows 2
"$SOL" models/tank-model-2020/prepare-panel.sol v3 --output /tmp/tank-fit --render
"$SOL" scripts/slice-prepared.sol models/two-hand-top a1-top
"$SOL" scripts/prepare-projects.sol --python /path/to/mesh-python models/two-hand-top a1-top
"$SOL" scripts/verify-prepared.sol --python /path/to/mesh-python models/two-hand-top a1-top
```

The shelf generator writes symmetric and soft-right explicit-grid problems.
The default refinement is 1 and the supported range is 1 through 3. The SVG
renderer consumes a JSON region report produced by `coscad stress ... --bonded
analysis.json --preview -o regions.json`. Use a separate output assembly
when generating Tesseract instructions.

The tank panel generator accepts `v2` for the single-flange samples or `v3`
for the double-flange samples. Each generates the three clearances and the
35 mm reference rail from the current wall and rail sources. Add `--render`
to export the meshes and SCAD files. All 24 files across both versions matched
the Python output byte for byte with CoScad 1.6.0.0.

The slicer processes already prepared model directories in the order supplied.
With no job arguments it uses the model defaults. Each invocation runs Bambu
Studio locally and writes its stdout and stderr to the job's `slice/slicer.log`.
A failing job ends the batch and leaves its diagnostic log available. Earlier
successful jobs have already published their slicer outputs. Use
`--slicer /path/to/BambuStudio` before the model directory to override the
macOS executable path.

The boat renderer supports `beach-monohull`, `beach-outrigger` and
`twin-boat-trials`. It selects the compiler from `--coscad`, then `COSCAD`,
then `cabal list-bin exe:coscad` from the model's repository. Each part
produces an STL and a compiler log. The three models use one, two and three
concurrent compiler processes respectively.
All 11 native boat STLs matched the Python-rendered files byte for byte in
separate scratch builds.

| Model directory | Default jobs in order |
| --- | --- |
| beach-monohull | p1s-pla, a1-pla, x1c-pla |
| beach-outrigger | p1s-pla, p1s-petg, a1-pla, a1-petg |
| bolted-glider | a1-wing-right, p1s-wing-left, x1c-fuselage, a1-tail |
| twin-boat-trials | monohull, outrigger |
| tt-propellers | x1c-props |
| two-hand-top | a1-top |
| tank-model-2020 | x1c-bed1, x1c-bed2 |

The shared package verifier supports these seven models, the balanced glider,
and Orbit Tower v2. It checks each model's required settings, toolpaths and
filament mass before filling in the 3MF printer identity. For boats it samples
arcs and checks the path bounds against the hull. For the balanced glider and
Orbit Tower it also verifies the recorded geometry and slice hashes. It checks
that the per-bed slice receipt includes every entry from preparation. Removing
the process-preset hash from a copy of an otherwise valid receipt was rejected.

One saved bed from each of the nine models reproduced its original validation
report, including the final package hash. With `SOL` set, the existing
`verify_packages.py` commands select the shared verifier, and the ordinary
`slice.py` and boat `render.py` commands select their shared drivers.
The twin-boat trial records hashes of its Python entry points, so use the Sol
commands directly for that model. The balanced glider and Orbit Tower v2 each provide a model-specific
`slice.sol`, backed by the same hash-checking and staged slicing implementation.
For the tower, the default selects its single `x1c-tower` bed. With a local
slicer stand-in, all 18 archived prepared inputs matched, every one of the
24 new receipt hashes verified, and editing the rotor source stopped slicing.

`prepare-projects.sol` prepares the tank, propellers, two-hand top, both gliders
and the three boat models. The model job tables supply fixed placements, infill and
printer presets. Sol checks part separation, brim and plate clearance for
fixed layouts and selects the hull placement and material profile for boats.
A scratch preparation for one bed per model matched all five 3MF members
and every generated preset and layout file. Both gliders also reproduced
their on-plate meshes. ZIP timestamps vary by build.

The compatibility checks run with `SOL="$SOL" python3 scripts/test-sol-ports.py`.
They compare all six shelf inputs with the Python generator, compare the
Tesseract assembly and all 12 selected part sources byte for byte, inspect SVG geometry
and escaping, and run all seven model batch configurations against a mock slicer. The batch test
covers arguments containing spaces and shell metacharacters, merged logs,
job ordering and child-process failure. The scripts use the host utilities
`realpath`, `test`, `mkdir` and `sh` for filesystem and process operations.

The 0.13.1 release used for these checks identifies its compiler in
`libexec/fprisc/toolchain.json` with SHA-256
`3f8ec8a5e1d039cd10335c0cfe31a33331cd665d00faacbf3a03b3057b0ab974`.
That installed executable prints `Sol 0.14.0-dev (scripting API 1)`.

## Balanced glider

Set `SOL` to use the new builder from the existing `python build.py` entry point.
Use `slice.sol` directly because historical slice receipts hash `slice.py`.
Run from the CoScad root or from the balanced-glider directory. In the model
directory, the commands below use an active Python geometry environment.

```sh
"$SOL" build.sol --coscad /path/to/coscad --python "$(command -v python)"
"$SOL" ../../scripts/prepare-projects.sol --python "$(command -v python)" .
"$SOL" slice.sol a1-tail
python verify_packages.py a1-tail
```

`build.sol` accepts `--tail-incidence` in degrees (default −5.5). It stages
the generated CoScad sources, STL meshes, SCAD exports, design, validation and
preview. A failed geometry check leaves the previous verified build available.
`slice.sol` checks every geometry and preparation hash before invoking Bambu
Studio, rechecks them after the process exits, and publishes the slice receipt
last. The output directory retains the slicer log if a job fails. Each
published file is replaced individually. A slice receipt includes the same
prepared inputs and G-code key used by the Python verifier, plus the hashes of
`slice.sol` and `provenance.sol`. Package verification subsequently stamps
the printer identity into the 3MF and records its final hash in `packages.json`.

On 2026-11-13, separate scratch builds with the same native compiler produced
identical Python and Sol `meshes.json` files (22 entries, SHA-256
`54eab3378461013987fc9b03f71d851c3acc2e6099afd6443669f9bc97c68a6f`).
The geometry validation JSON and all ten SCAD exports also matched. Bambu Studio
02.08.02.61 sliced the four default beds and both additional P1S beds offline.
All 50 prepared-input entries across the six beds and all six G-code hashes
matched the existing receipts. The package verifier accepted all six results.
Rebuilding at −5.4° also passed geometry verification. Slicing a bed prepared
at −5.5° then failed on the changed `meshes.json` hash before Bambu Studio ran.

A second six-bed Sol run matched all 50 prepared inputs and passed package
verification on every bed. Four G-code files matched the recorded bytes in
that run. The P1S left wing and X1C fuselage each reproduced the recorded
hash in a subsequent run and also produced other hashes from the same inputs.
The original Python driver also produced two different fuselage hashes. Compare
the exact inputs and validate each new package independently. The slice receipt
records the hash of the G-code generated in that run. The two extra P1S beds
also require their specific preparation script in the input receipt.
Sol-prepared beds additionally record the shared Sol and Python preparation
files and the model’s Sol provenance helper. Changing a recorded producer
invalidates the bed. A scratch P1S tail preparation
passed slicing and was then rejected after a change to the generator. Removing
the fin mesh entry from the geometry receipt also stopped preparation.

Run `SOL="$SOL" python3 scripts/test-sol-glider.py` for the dependency-light
regression checks. Fixtures compare the Python and Sol build receipts and
exercise modified geometry, changed settings, incomplete receipts, slicer
failure, missing outputs and an input changed while the slicer is running.

For the structural model, run `"$SOL" models/bolted-glider-balanced/structural/wing-gauges.sol REPORT.json`.
It uses `jq` to select gauge nodes from the full report. On the saved
bonded and spring-interface solves, all fields matched `wing_gauges.py`,
including the load-pad displacement, relative rotation and interface wrenches.

## Shelf bracket and repository maintenance

```sh
"$SOL" scripts/shelf-bracket-workflow.sol --coscad /path/to/coscad --output /tmp/new-shelf --images
"$SOL" scripts/check-docs.sol
"$SOL" scripts/package-release.sol --binary /path/to/coscad --output /tmp/release
"$SOL" scripts/update-formula.sol 1.3.0.0
"$SOL" scripts/pipeline-lock.sol --scope slice --check
"$SOL" scripts/slice-mass.sol examples/mass/slice.json --python /path/to/numpy-python -o /tmp/mass.json
```

The shelf workflow requires a new output directory. It records each stage in
`workflow.json` before the child starts, captures its logs, enforces the stage
deadline, and records failures before exiting. Use `--slice` for offline Bambu
Studio slicing with the existing pin checks and presets. `--python` selects
the interpreter for the pin and slicing helpers. The Sol driver uses `jq` to
read the backend from the full stress report and `unzip` to check the exported
process settings and support toolpaths.

On 2026-11-13 the Python and Sol native builds with `--images` produced 19
recorded artifacts apiece. Their contents agreed after substituting the two
output paths. The separate Sol `--slice` run passed the four slice pins,
exported a local A1 package, and verified the settings and support toolpaths.

The documentation checker runs from the repository root. It checks the README,
man page, CLI help and module map. The packager takes a tested executable,
includes the native kernel notices, checks macOS library dependencies, and
writes the tarball and its SHA-256. On macOS arm64, all 554 archive members
matched the Python package in name, type, mode and file contents. Compare
extracted members across packagers because tar/gzip headers may differ.
The formula updater downloads the named tag and hashes the archive before
overwriting the formula. Its regression uses a local download fixture.

Run the workflow and maintenance tests from the repository root with
`PYTHONPATH=scripts SOL="$SOL" python3 -m unittest scripts/test-sol-*.py`.
These include failed stages, rejected slice settings, missing support paths,
a failed formula download, and matching documentation diagnostics on invalid input.

The existing `pipeline-lock.py` and `slice-mass.py` commands also select their Sol
drivers when `SOL` is set. They pass through the calling Python interpreter so
the pin check measures the same environment and the mass reader can import NumPy.

The pipeline lock driver handles all nine scopes, `--check-ci`, `--check`,
`--pin` and `--run`. The offline test captures all 41 pins twice and compares
the complete lock file with the Python command. It also exercises changed
dependencies, conflicting writers, CI drift and passing arguments, stdin and
exit status through to a pinned tool. On the local macOS checkout, both
implementations accepted the four slice pins and reported drift for
`coscad_binary`, `mandoc` and `pandoc` when checking render and manual together.
The complete inventory uses a local stand-in for the deployment host in tests.

The mass driver reads a manifest once and hashes the bytes passed to the NumPy
integrator. It checks for overlapping input and output paths and publishes each
requested report by atomic file replacement. Both the mass report and the
component export for the balanced glider matched the Python output byte for
byte, including hashes for its four G-code files and all 17 component entries.

## Original and threaded globes

The original A1 and threaded P1S builds share source selection, native rendering
and offline slicing. The original build keeps Shapely for continent triangulation.
Run its `build.py --render-only` with `SOL` set to rebuild all six views, the
three component meshes and the assembly. In independent working copies its 13
geometry, design and validation files matched the Python build byte for byte.

With the globe's Python environment active, the following command runs the Sol
generator, renders six native meshes and checks the assembly and thread travel.

```sh
SOL="$HOME/.fpr/0.13.1/bin/sol" python models/print-in-place-globe/v2-threaded/build.py --render-only
```

Omit `--render-only` to prepare and slice the full P1S plate and the fit coupon,
then verify and finalize both packages. Both builds render a reference rotor
from the parent design with the selected compiler. The recorded parent STL
uses 96-segment circles and the native compiler uses 50. The independent
same-compiler reference keeps the original 0.1 mm³ volume tolerance meaningful.
The offline regression also exercises a failed coupon slice and checks that
package finalization stops.

On 2026-11-13 the native Python and Sol builds produced 20 byte-identical
files, including all eight SCAD exports, eight STL meshes, the clearance report
and the preview. The translated original rotor differed by 0.00318 mm³.
Bambu Studio 02.08.02.61 sliced the two Sol-built projects offline. Its
macOS thumbnail exporter needed access to the graphics service. The full
toolpath passed the bearing, socket and support checks. Both final package
checks passed.

One Sol finalizer also serves the original A1 and P1S plates, the threaded P1S
plate and its support-free coupon. The four original `finalize_*.py` commands
select it when `SOL` is set. Copies of all four saved packages reproduced
their recorded package, G-code and report hashes. An intentionally stale
threaded toolpath report stopped finalization.

## FEA comparisons, coupons and model assets

```sh
export SOL="$HOME/.fpr/0.13.1/bin/sol"
python scripts/compare-fea-reference.py --exe /path/to/coscad-experiments
python scripts/compare-cad-fea-reference.py --exe /path/to/coscad-experiments
python scripts/compare-hex20-reference.py --exe /path/to/coscad-experiments --cells 1
python models/two-tier-2020-shelf/vertical-retry/center_bed.py
python models/tesseract-v3-threaded/center_coupon.py
"$SOL" models/twin-boat-trials/design.sol --coscad /path/to/coscad --out /tmp/twin-boats
python scripts/render-editor-icons.py --coscad /path/to/coscad
```

The three FEA commands share a Sol driver for case selection, process deadlines
and verdicts. The independent Python kernels still assemble and solve the
reference elements. Against CoScad 1.6.0.0 the four Hex8 beam cases, four CAD
fixtures and two single-cell-width Hex20 cases produced byte-identical output
to the original commands, including the numeric errors and reference versions. The commands also returned
the same successful exit status.

The two upright shelf-rail variants and the Tesseract thread coupon share
`scripts/center-coupons.sol`. An offset rail and an offset thread-coupon pair
produced identical recentered meshes and placement manifests to Python.
The twin-boat generator reproduced both hulls, their envelope and opening
views, all six SCAD exports and the design manifest. Use it directly to keep
the archived Python source hash reproducible. Editor icon generation now runs
the catalog from Sol. The cube and X-axis cylinder SVGs matched their bundled
assets with the native compiler.

## Repository migration

### Manual builds

With `SOL` exported as above, `scripts/build-manual.sh OUTDIR` builds the
documentation, man page and all eight assembly guides. It clears copied
previews before rendering, checks the plans and step images, and publishes
the complete site in one directory exchange. The destination's parent must
exist. To finish an existing generated tree, run `"$SOL" scripts/finish-manual.sol DIR`.

`scripts/manual-update.sh [--force]` selects the Sol updater when `SOL` is
set. The shell holds the checkout lock across the fetch, build and publication.
The updater is intended for a dedicated deployment checkout and resets it
to `origin/main` before rebuilding. The regression suite exercises this
entry point with local stand-ins for the deployment commands.

Native builds from the shell and Sol commands produced 172 files each. All
133 PNGs matched byte for byte, including 131 step images. The other files
matched after accounting for the build minute and the temporary source path
recorded by each assembly plan.

### Inventory

The goal covers CoScad-owned batch scripts, including model-specific workflows.
Keep historical receipts reproducible while introducing Sol entry points.
At this review the tracked baseline contains 206 Python, shell and CommonJS
files under `scripts/`, `models/`, `examples/` and `experiments/` (30, 172,
2 and 2 respectively). This count includes helper modules and tests. Inventory
new scripts as they enter the tree. The current working tree also has two
P1S-specific glider preparation helpers, five Orbit Tower v2 scripts, six
host helpers and six Python regression drivers outside that tracked baseline.

**60 of the 206 tracked scripts have a Sol entry point.** This counts a
script once when the driver handles its batch workflow, including those
that call a numerical or archive helper. The remaining 146 include
library modules, tests and pending ports. Four additional working-tree
entries are covered outside the baseline: the two P1S glider preparation
variants and Orbit Tower v2 slicing and package verification.

| Tracked entry points with Sol equivalents | Count |
| --- | ---: |
| Repository scripts | 15 |
| Original, P1S and threaded globes | 7 |
| Balanced glider, tank and twin-boat trials | 5 each |
| Beach monohull and beach outrigger | 4 each |
| Original glider, Tesseract, TT propellers and two-hand top | 3 each |
| Two-tier shelf | 2 |
| Egg box | 1 |

| State | Scripts and next verification |
| --- | --- |
| Ported | Balanced-glider build, slice and structural gauge reader, the seven prepared-model slicers above, the three boat render drivers, Tesseract instructions and part export, egg-box generation, the bonded shelf generator, the assembly-region SVG renderer, the shelf-bracket workflow, documentation checks, release packaging and formula updates. |
| Ported | Manual builder, HTML finishing and update driver. The shell entry points select Sol through `SOL`. |
| Ported | Tank panel-fit v2 and v3 preparation and optional rendering use the shared `models/tank-model-2020/prepare-panel.sol` driver. |
| Ported | Pipeline pinning, CI checks and pinned tool execution. The slice-mass CLI generates mass and component reports through the existing NumPy integrator. |
| Ported | Both globe builds share the renderer and offline slicer. The threaded generator and build driver produce the helical threads, eight SCAD views and six native meshes, run clearance checks, and coordinate offline slicing of the full plate and fit coupon. |
| Ported | One prepared-package verifier covers nine model families. It preserves per-job reports and the batch index, including the original models' different support, clearance, layer, infill and mass checks. |
| Ported | One project preparation driver covers eight models. The tank, top and propeller entry points also share the Python mesh placement and 3MF writer used by the existing customized tank-panel and cube workflows. |
| Ported | Hex8 beam and CAD comparisons and the Hex20 cantilever comparison share one Sol batch driver. |
| Ported | Orbit Tower v2 slicing, four globe package finalizers, three coupon-centering entries, the twin-boat generator and the editor icon batch. |
| Queued | Compiler diagnostic probes, including preservation of their seeded corpus and per-case deadlines. |
| Queued | Remaining model generators, preparation, archive verification, analysis drivers and device workflows. For recorded print jobs, exercise adapters offline with captured responses and check deduplication before switching entry points. |
| Keep existing numerical implementations | The balanced-glider assembly and geometry verifier use NumPy transforms, trimesh, Manifold booleans and Matplotlib. The independent FEA comparisons rely on SciPy and scikit-fem. Sol drivers can call these kernels. |
| Keep existing host-specific tests | The manual layout test uses Playwright's Chromium API to measure DOM bounds and exercise keyboard navigation. VS Code extension tests exercise the Node/extension-host API. |

### Next groups within the 206-file baseline

| Paths under the checkout | Remaining work |
| --- | --- |
| `scripts/compare-bonded-reference.py`, `compare-tetra-fea-reference.py`, `compare-native-pipeline.py` | Extend the reference driver to handle bonded refinement summaries, exported tetrahedral fields and the recorded native comparison. Retain the independent solvers and per-case deadlines. |
| `models/bolted-glider*/`, `models/beach-*/`, `models/two-hand-top/`, `models/tt-propellers/` | Port the remaining design and report orchestration. Keep the mesh booleans, hydrostatics and toolpath moments in the numerical libraries. The two-hand top and TT gearmotor geometry generators are candidates for direct Sol implementations. |
| `models/tank-model-2020/panel-*/`, `models/panel-braced-cube/`, `models/orbit-tower-v2/` | Extend the shared project writer to the custom panel, cube and tower recipes. Retain their individual geometry checks, source hash lists, job profiles and existing dispatch-record checks. |
| `models/tesseract-v3-threaded/*retry*/`, `p1s-full/`, `p1s-thread-fit/` | Group the archived packing, preparation, per-bed validation and first-layer plots. The packing kernels depend on Shapely/SciPy and the plots use Pillow or Matplotlib. |
| `models/two-tier-2020-shelf/`, `models/water-tank-2020/` | Port the model generation, packing and report batches. These use SciPy transforms, triangular meshes and an independent scikit-fem bracket solve. |
| `models/print-in-place-globe/` and its revisions | The build and finalization pipelines run in Sol. The model-specific project writer and toolpath analysis still run in Python. |
| `examples/assemble/tesseract/`, `experiments/topological/`, `models/bow-center-reinforced/`, `models/orbit-tower/` | Separate fixture selection and report writing from existing mesh, contact, placement and toolpath checks. |
| Printer context, dispatch, monitoring and calibration scripts under `models/`, including the coin-cylinder fan timer | The device layer uses Python MQTT, TLS or the printer API. Port batch decisions against recorded telemetry before switching an operational entry point. |
| Repository and model test harnesses | Preserve the Python and browser test APIs used to compare the two implementations. Treat verification tools that can be used independently as separate port candidates. |

### Implementations retained for library support

| Implementation | Dependency the workflow still needs |
| --- | --- |
| `scripts/publish-manual.py` | Sol 0.13.1 has no operation to exchange two nonempty directories. This helper stages the verified tree on the destination filesystem and uses the macOS or Linux directory-exchange syscall. |
| `models/water-tank-2020/bracket_fea.py` | scikit-fem constructs the quadratic tetrahedral basis, applies facet traction and solves the constrained elasticity problem. |
| `models/beach-monohull/review-20261101/check_flooded_equilibrium.py` | SciPy solves for heel and trim simultaneously against the existing Python hydrostatics model. |
| `models/beach-outrigger/verify.py` | trimesh separates watertight shells, Manifold measures wall clearances and Matplotlib renders the stability curves. |
| Coastline triangulation in `models/print-in-place-globe/generate.py` | Shapely triangulates each land polygon and checks that retained triangles lie entirely inside its outline. The threaded revision reuses this geometry from the parent source. |
| `models/print-in-place-globe/v2-threaded/verify.py` | trimesh and Manifold measure rotor and thread interference. Matplotlib renders the assembly preview. The Sol build invokes this verifier with a same-compiler reference rotor. |
| `models/print-in-place-globe/v2-threaded/support_roots.py` | SciPy labels connected support components and dilates the previous layers on a NumPy raster to check whether each component can reach the bed. |
| `models/tesseract-v3-threaded/p1s-full/pack.py` | Shapely builds buffered outlines and SciPy performs FFT convolution on the bed-occupancy grid. |
| `scripts/coscad_mass.py`, `scripts/slice_mass_host.py` | The mass reader uses NumPy for arc centroids, rigid transforms and per-object moments. The host helper preserves its double-precision JSON and Python's path resolution and file replacement behavior. |
| `scripts/pipeline_lock_host.py` and the host functions in `scripts/pipeline-lock.py` | These preserve the byte-level tree inventory, file identity checks, OS-owned writer lock, directory fsync, plist and installed-package inspection, and bounded version probes used by the existing pins. |
| `scripts/prepared_package_host.py` | Reads G-code as a stream, samples hull arcs with NumPy, measures STL volumes with trimesh and uses ZipInfo and ElementTree to preserve the original 3MF members and XML. Sol applies the model-specific acceptance criteria before the helper publishes the package. |
| `scripts/prepare_project_host.py` | Loads and centers watertight meshes with trimesh, flattens installed Bambu vendor profiles, and serializes 3MF geometry at the original precision. |
| `scripts/fea_reference_host.py` and the three reference kernels | scikit-fem assembles the independent systems and SciPy solves them. Python preserves the original JSON float encoding and comparison output. |
| Mesh drawing in `scripts/render-editor-icons.py` | NumPy projects and lights the trimesh facets, and the renderer preserves the shipped SVG coordinate formatting. |
