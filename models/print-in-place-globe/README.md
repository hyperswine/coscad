# Captive print-in-place globe

A single print containing a stationary base/meridian/axle and a captive rotating globe. Remove the brim and tree supports after cooling; gently free the globe without forcing the thin meridian. No assembly is required. Physical rotation remains unverified until this first print is cleaned and tested.

- Globe diameter 64 mm, shallow 0.8 mm continent relief; broad 86 mm base; overall height 94 mm.
- Fixed 5 mm axle, 6.2 mm bore: 0.6 mm nominal radial clearance (0.598 mm conservative faceted clearance).
- Flared lower bearing with 0.8 mm nominal axial gap; flattened lower annulus gives supports a practical first-layer landing area.
- Simplified hand-drawn geographic outlines, deliberately illustrative.
- A1, 0.4 mm nozzle, PLA, Textured PEI, 0.16 mm layers, 3 walls, 12% gyroid, 4 mm outer brim, automatic tree supports, 0.2 mm support Z gap.
- Support blocker protects the bore. Support branches may enter its nominal volume above Z87 mm, beyond the rotating sphere; they must be removed too.
- Slicer estimate: 6 h 38 min, 81.89 g / 27.02 m. Actual time and material can differ.

## Artifacts and checks

`globe.coscad` is the editable design; `generate.py` regenerates it. `globe.stl` has two closed connected components. `globe-project.3mf` includes separate fixed/moving parts and the support blocker; prefer this project over importing the STL alone. `slice/globe-a1.gcode.3mf` is the validated A1 job.

`validation.json` records closed/winding-consistent meshes and zero intersection in 72 rotation/axial-offset checks. This is sampled CAD validation, not proof of physical print tolerances. `toolpath-validation.json` checks support exclusion through the moving region and separate shaft/bore paths at mid-height, including arc moves sampled at 0.05 mm. `toolpath-check.png` shows the lower landing and bearing sections. These focused checks do not prove every overhang or layer will print successfully.

`finalize_package.py` fills an empty printer_model_id in BambuStudio CLI output from the installed vendor A1 profile (N2S), verifies the ZIP and preserves G-code byte-for-byte. `package-validation.json` records hashes. CLI slice_info reports an invalid negative first-layer-time estimate; this field is not used as a physical validation or ETA. Slicing reports success and no plate warnings; its log also includes a ZFiller clipping diagnostic. Timelapse is disabled for this LAN job.

## Rebuild (macOS)

Tested with CoScad 1.2.0.0 via Cabal/GHC 9.8.2, OpenSCAD 2025.05.16 (Manifold), BambuStudio 02.08.02.61, Python 3.12, numpy 2.3.5, trimesh 4.11.4, manifold3d 3.4.0, shapely 2.1.2, scipy 1.17.0 and matplotlib 3.10.8. Native apps must run with normal host access. Build the CoScad executable with `cabal build exe:coscad`; `generate.py` currently references this checkout's GHC 9.8.2 Cabal executable path.

Run `python build.py` in a Python environment containing the above packages. It renders component meshes, joins them without altering geometry, validates, slices with absolute paths, checks toolpaths, and finalizes the package. It does not send a print. Presets are flattened from installed vendor profiles; exact settings used here are retained under `presets/`, including the full base project configuration seeded from the prior successful A1 tesseract project.

2026-10-29: `build.py` no longer uses OpenSCAD. It renders each part from `globe.coscad` with the native compiler (`coscad stl`; `--coscad PATH`, `$COSCAD` or `cabal list-bin exe:coscad`), and `--render-only` stops after `verify.py`, before slicing. The recorded meshes, logs, validation and package above came from OpenSCAD at `$fn=96`, which `generate.py` writes into the `.scad` files; the native kernel always uses 50 circle segments, so a rebuild is coarser and not identical. A native rebuild on 2026-10-29 passed every `verify.py` check (zero intersection over the 72-pose rotation sweep) with stand 1730 triangles and 49273.48 mm³ (OpenSCAD 3266, 49367.12), rotor 4718 and 137391.47 mm³ (12596, 138054.04), and `verify.py` now reports the faceted bore clearance for 50 segments (0.5939 mm; 0.5983 mm at 96). Re-slice and re-verify toolpaths before printing a native rebuild.

LAN sending uses the repository's `scripts/bambu-slice.py` and `scripts/bambu-print-run.py`, with paho-mqtt 2.1.0. For this run the A1 reported slot 3 (`ams2`) present with yellow PLA; credentials remain outside this directory. Read printer status and spool presence again before any future dispatch, confirm an empty bed, and never repeat a dispatch merely because acknowledgement is delayed.

## First dispatch

On 2026-10-19 the A1 accepted `globe-a1-20261019.gcode.3mf`, then reported RUNNING, 587 total layers, error 0, with startup heating underway. AMS slot 3 was explicitly selected after presence validation. `print-run.json` records bounded startup telemetry; camera captures are saved alongside it. The camera was too dark to verify adhesion. Completion and physical rotation are not yet established. CoScad checkout at creation: `3616da53a4c501671780ac7112c32d9a6ccc00e5`.

## P1S retry, 2026-10-20

The owner reported repeated tangling on the A1's current filament roll. This is
logged as a feeding issue in the printer app (issue 1); a spool-specific cause
is suspected but unconfirmed. See `p1s/README.md` for the P1S-specific slice,
unchanged captive geometry, tree supports, bearing checks and dispatch evidence.
