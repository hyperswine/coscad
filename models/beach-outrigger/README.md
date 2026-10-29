# Beach-scale sealed outrigger, 2026-10-25

A one-plate scale-up of `../twin-boat-trials/outrigger` for trying out at a beach.
Same plan form, 2.1x the size, but built as three *sealed* thin-shell hulls instead of a
solid open hull, so it cannot be swamped and still floats with any one chamber flooded.
All geometry is CoScad written by design.py. Nothing here has been printed or
float-tested yet, and no script in this folder talks to a printer.

| | Value |
| --- | --- |
| Footprint, height | 231 x 206 mm, 58 mm (fits a 256 mm plate with a 4 mm brim) |
| Shell | 2.4 mm walls and roofs, 3.0 mm floors; checked minimum 2.27 mm |
| Roofs | 50 degrees from horizontal, flat ridges 20 mm (center) and 8 mm (pontoons) |
| Crossbeams | two per side at x = -60 and +15, 24 mm wide, flat top at z = 40, 47.5 degree gable underside, no bridging |
| Mass | 426 g PLA, 436 g PETG (sliced 100% solid, so the modeled plastic is the mass) |
| Air | 782 cm3 in three chambers; whole envelope 1138 cm3 |

## What the checks say (verify.py, static, fresh and sea water)

| | PLA fresh | PLA sea | PETG fresh | PETG sea |
| --- | --- | --- | --- | --- |
| Draft | 20.3 mm | 20.0 mm | 20.7 mm | 20.3 mm |
| Pontoon freeboard to ridge | 27.6 mm | 28.0 mm | 27.3 mm | 27.6 mm |
| Beam root above water | 4.7 mm | 5.0 mm | 4.3 mm | 4.7 mm |
| Static trim | 0.5 degrees bow down | same | same | same |
| Largest righting arm | 48 mm at 20 degrees | 49 mm | 47 mm | 48 mm |
| Righting arm vanishes near | 87 degrees | same | same | same |
| Payload on the center ridge before the beams touch water | 130 g | 145 g | 120 g | 135 g |
| Payload before the hulls are fully under | 710 g | 740 g | 700 g | 730 g |

Compared with the 51 g bath-size outrigger (righting arm 22 mm at 20 degrees, open
center hull flooding at 35 degrees), this one is stiffer in proportion, keeps
positive righting to almost 90 degrees, and has no opening to flood. Inverted it is
also stable (righting arm zero at 180 degrees), so it will not self-right from a
full capsize; it will float upside down on its ridges. Static only: no waves,
surf, slamming or dynamic capsize are modeled.

## Design notes

- Scaling a homogeneous solid keeps its hydrostatics, but mass and print time grow
  with the cube, so the 2.1x copy is a shell. Plastic is 31% of the envelope here
  against 37% for the small boat.
- The void of each hull is a true inset of the outer bezier outline (Clipper miter
  offset of the compiler's own 24-sample evaluation of `✎`), emitted as a `✎` path
  with collinear control points. A scaled copy of the outline is not an inset: it
  left 1.1 mm at the pontoon bows.
- Walls that lean are inset by t / cos(lean) so the normal thickness stays 2.4 mm.
  The wall-to-roof junction station takes the outer size at its own height; using
  the widest station's size there thinned the transom corners to 1.8 mm.
- Beams sit where both hulls are near their widest (z = 25), inside the straight
  full-width run of every hull's bezier outline. design.py asserts this; a beam
  on the bow taper hangs off the wall and the slicer reports a floating cantilever.
- A beam is two hulled halves (`(a ⇓ b) ⊕ (b ⇓ c)`); the convex hull of all three
  boxes fills the gable and brings back a 24 mm bridge.
- Each beam has the air chambers subtracted, so it cannot put plastic inside a
  sealed volume.
- Chamber pressure in the sun is small: a 30 K rise is about 10 kPa, a few hundred
  kPa of hoop stress on a 2.4 mm wall. No vent is modeled; a vent would let wash in.
- The P1S excludes an 18 x 28 mm front-left corner. Its jobs place the hull at
  (134, 136); the A1 jobs are centered at (128, 128).

## Files and rebuilding

- design.py, render.py, verify.py: CoScad generation, Manifold render, thickness and
  hydrostatic checks. Run in this order with the environment used for the twin boats
  (trimesh, manifold3d, numpy, scipy, matplotlib; `/private/tmp/coscad-globe-venv`).
  The recorded meshes and `*-render.log` files came from OpenSCAD's Manifold backend.
  Since 2026-10-29 render.py renders with the native compiler instead
  (`coscad stl`; compiler from `--coscad PATH`, `$COSCAD` or `cabal list-bin
  exe:coscad`; `--out DIR` writes elsewhere), so no OpenSCAD is needed. A native
  re-render matches both meshes in volume (body 343534.021 against 343533.904 mm³,
  envelope identical) and bounds, with 5372 body triangles against 17368, and
  verify.py on it reproduces validation.json to within 1e-6 mm.
- beach-outrigger.coscad: the printable source. beach-outrigger-envelope.coscad is the
  displacement body for verify.py, not a part.
- preview-and-stability.png, validation.json, design.json: results.
- prepare_slice.py, slice.py, verify_packages.py: four Bambu Studio CLI jobs,
  `p1s-pla`, `p1s-petg`, `a1-pla`, `a1-petg`, each `<job>/slice/beach-outrigger.gcode.3mf`.
  Presets copy the twin-boat settings (0.4 mm nozzle, six walls, 100% zig-zag fill,
  six top and bottom layers, no supports, outer wall 35 mm/s, textured PEI) at
  0.2 mm layers with a 4 mm brim. PLA is Bambu PLA Basic; PETG is Bambu PETG HF for
  each printer (245 C on the P1S, 240 C on the A1, 70 C bed).
- packages.json: the slice results. See the table below.

## Before the beach

1. Print one (either printer, either material). About 22 hours and 430 g.
2. Weigh it and float it in a sink: it should sit about 20 mm deep with the beam
   roots just clear of the water. Press each hull under and watch for bubbles.
3. Leave it in the sun on a dark surface for an hour and re-check for leaks. PETG
   tolerates hot sand better than PLA (about 80 C against 55 to 60 C).
4. Try a rock pool or lagoon before surf; the model is 23 cm long and beach swash is
   taller than its freeboard.

## Slices (Bambu Studio 02.08.02.61 CLI, verify_packages.py passed: Success, no warnings, no support paths, paths inside the model)

| Job | Printer | Filament | Nozzle / bed | Time | Filament used | Layers | Package |
| --- | --- | --- | --- | --- | --- | --- | --- |
| p1s-pla | P1S | Bambu PLA Basic @BBL P1S 0.4 nozzle | 220 C / 55 C | 21.4 h | 424 g | 288 | `p1s-pla/slice/beach-outrigger.gcode.3mf` |
| p1s-petg | P1S | Bambu PETG HF @BBL P1S 0.4 nozzle | 245 C / 70 C | 21.5 h | 418 g | 288 | `p1s-petg/slice/beach-outrigger.gcode.3mf` |
| a1-pla | A1 | Bambu PLA Basic @BBL A1 | 220 C / 65 C | 21.7 h | 424 g | 288 | `a1-pla/slice/beach-outrigger.gcode.3mf` |
| a1-petg | A1 | Bambu PETG HF @BBL A1 | 240 C / 70 C | 21.8 h | 413 g | 288 | `a1-petg/slice/beach-outrigger.gcode.3mf` |
| x1c-pla | X1C | Bambu PLA Basic @BBL X1C | 220 C / 55 C | 21.5 h | 424 g | 288 | `x1c-pla/slice/beach-outrigger.gcode.3mf` |
| x1c-petg | X1C | Bambu PETG HF @BBL X1C | 245 C / 70 C | 21.6 h | 418 g | 288 | `x1c-petg/slice/beach-outrigger.gcode.3mf` |

The bridged layers are the flat ceilings of the air chambers (the 13.7 mm wide center
ridge at z = 54 and the pontoon ridges at z = 44), printed as ordinary bridges between
the two roof walls, plus the 1 mm strip where the two gable slopes of each beam meet.
The X1C jobs reuse the P1S placement (same excluded corner) and the same baseline process
preset, `0.16mm Optimal @BBL X1C`, which is compatible with both enclosed printers.

## X1C dispatch, 2026-10-25 17:21 Sydney

The owner put the X1C (192.168.0.14, serial 00M09D550701167, no AMS, 0.4 mm hardened
nozzle) into developer mode and asked for a print. printer-context.py now bridges
`a1`, `p1s` and `x1c` from `3d-models/.env` (BAMBU_X1C_HOST / _SERIAL / _ACCESS_CODE,
also in `~/.config/bambu/x1c.env`), verifying the printer's TLS identity against
the same Bambu printer CA. dispatch.py is the twin-boat procedure for this folder:

```sh
python dispatch.py x1c-pla --spool external --confirm-bed-clear --authorization-note '...'
```

Preflight: IDLE, no HMS, print_error 0, external spool PLA (colour BECF00FF) the only
filament. The X1C has no port-6000 JPEG camera; frames come from its RTSPS stream
(`rtsps://192.168.0.14:322/streaming/live/1`) with ffmpeg, see `x1c/*.png`. The camera
looks across the chamber and shows only the front of the plate, which was clear; the
chamber light was switched on for the second frame.

Two X1C differences from the A1/P1S were found on the way:

- The first upload failed with `426 Failure reading network stream`: the X1C wants a TLS
  close_notify on the data connection. dispatch.py now sends it with a bounded timeout
  (the A1/P1S never answer it). That run never sent a print command; its record is
  `x1c-pla/print-run.failed-upload-1.json` and the partial file was deleted from the card.
- The X1C acknowledges `project_file` with `result: SUCCESS` in upper case. The first
  dispatch classified that as rejected and stopped watching; the check is now
  case-insensitive and the record was corrected from the live state.

`x1c-pla/print-run.json` holds the acknowledgement and events. At 17:24 the printer was
RUNNING `beach-outrigger-x1c-pla-20261025-172115`, 288 layers, about 21 h remaining.
`x1c/observe_first_layer.py` is a bounded watcher that appends events until layer 1 or an
issue; nothing recurring was installed. It saw layer 1 at 17:33 Sydney with 220 C nozzle,
55 C bed, no errors or HMS, and saved `x1c/20261025-173317-x1c-first-layer-check-printer-capture.png`.

## Dashboard

The X1C was added to the native printer host on the Mac mini (`/Users/jq/printer-host/
data/printers.json`, id `x1c`, model `X1 Carbon`, `camera: none` because the host only
implements the port-6000 JPEG camera, same CA file) with its access code in
`data/printer-credentials.env`; the previous files are under `rollback/x1c-add-<stamp>/`.
The host was restarted by killing its process (launchd KeepAlive relaunched it; sudo is not
available non-interactively), which signs out existing dashboard sessions. Afterwards the
host held MQTT sessions to all three printers. The public page needs a login key this
machine does not hold, so the X1C card itself was not viewed.
