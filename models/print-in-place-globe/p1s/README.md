# P1S globe retry, 2026-10-20

Owner reports repeated tangling on the current A1 filament roll. The feeding
issue is recorded in the printer app as issue 1, category Feeding / extrusion,
and in ../a1-filament-issue.json. A spool-specific cause is suspected, not proven;
this is not evidence that the globe geometry cannot print.

This retry uses the P1S, 0.4 mm nozzle, loaded external yellow PLA, Textured PEI,
0.16 mm layers, three walls, 12% gyroid, 4 mm outer brim and automatic tree
supports. Model geometry and bearing support blocker are byte-identical to the
original A1 project (input-geometry-validation.json). Machine start/end code and
filament profile come from the installed P1S vendor presets. Presets are saved
under presets/. Estimated total duration: 6 h 14 min; material: 81.93 g / 27.03 m.

verify_toolpaths.py uses the P1S vendor extruder offset (0,2) to transform the
project center (128,128) to G-code center (128,126). Initial checking with the
A1's zero-offset assumption falsely reported bearing support intrusion. The
corrected check passes: no support footprints inside the protected bearing
region, no shaft/bore gap-crossing model paths, lower rotor begins at Z23.4.
This is sampled toolpath validation, not proof of physical free rotation.

finalize_package.py fills the CLI's omitted printer_model_id from the installed
P1S vendor model profile (C12), preserving G-code bytes. Package SHA-256 and
G-code hash are saved in package-validation.json. CLI reports Success and no
plate warnings; its log retains profile lookup warnings and the same ZFiller
clipping diagnostic observed during A1 slicing. The invalid negative first-layer
time in CLI metadata is not a meaningful physical estimate.

The P1S reported FINISH, no print error, 0.4 mm nozzle and external PLA loaded
before dispatch; the saved camera frame showed an empty bed. LAN dispatch uses
the repository's bambu-slice.py lan_print API and bambu-print-run.py status API.
print-run.json records the single dispatch attempt, acknowledgement and bounded
startup observations. Do not resend merely because acknowledgement is delayed.
Physical completion and free rotation require inspection after cooling and
support removal. No A1 control or stop command was issued for this retry.


## Recheck the saved P1S slice

From this directory, use the Python environment described by the parent project:

```sh
python verify_toolpaths.py
python finalize_package.py
```

To re-slice the unchanged multipart project, use the saved flattened P1S presets
and disable automatic orientation/arrangement so the captive axle stays upright:

```sh
/Applications/BambuStudio.app/Contents/MacOS/BambuStudio --debug 2 --load-settings "$PWD/presets/machine.json;$PWD/presets/process.json" --load-filaments "$PWD/presets/filament.json" --arrange 0 --orient 0 --slice 1 --export-3mf globe-p1s.gcode.3mf --outputdir "$PWD/slice" "$PWD/globe-project.3mf"
```

Run both checks after re-slicing, before dispatch. Reinspect printer status,
external spool and the physical bed each time. Neither check starts a print.

## Accepted dispatch

The P1S acknowledged the single LAN dispatch of `globe-p1s-20261020.gcode.3mf` and
entered RUNNING with 587 total layers, error zero and startup heating underway.
The original A1 job was not resumed or cancelled. `print-run.json` retains
subsequent startup observations. This is startup evidence, not a completed print.

First-layer confirmation: layer 1/587, RUNNING, print error zero; nozzle about
220 C and bed about 55 C. Camera shows yellow support feet lying flat on the
plate. Only part of the first layer was visible; later adhesion and completion
are unverified. The local startup observer was deliberately stopped after this
check; the P1S job continues independently. See startup-observation.json.

## 2026-10-22 physical outcome

Owner reports the globe printed well. Supports were awkward to remove and the
neck joining the gimbal to the base broke during rough handling. Rotation was
not explicitly confirmed. The separate-base redesign is under ../v2-threaded/;
this successful print and its sliced artifacts are retained as the baseline.
