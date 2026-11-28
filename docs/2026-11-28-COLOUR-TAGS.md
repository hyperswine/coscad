# Colour tags: from model parts to printer spools, in Sol

2026-11-28. This replaces the hand-wired colour mapping used for the first two-colour AGVN sailboat, and the last Python scripts in the repository.

## What was wrong

Before this change, a part's colour reached a spool through four places that agreed only by hand:

1. **Model.** The `.coscad` file had one solid per colour, rendered by swapping `main`.
2. **Template project.** An old Bambu Studio project, `AGVN-sail.3mf`, gave part 1 extruder 1 and part 2 extruder 2. `build.py` copied it and swapped in the new meshes.
3. **Slicer.** The order of `--load-filaments` decided each filament's material, and a string replacement set `filament_maps "1 2"`.
4. **Printer.** `dispatch_x2d.py` had `ams_mapping [3, 255]` and the expected spools written into its code.

A new multi-colour model needed a hand-made template. A moved spool meant editing the dispatcher. Nothing checked that the nozzle map in the package matched the spools.

## Now

**Tags.** Each model has a `parts.json` beside its source. For the sailboat it is [models/twin-boat-trials/agvn/sail/parts.json](../models/twin-boat-trials/agvn/sail/parts.json):

```json
{"model": "boat.coscad", "center": [138.25, 128],
 "filaments": [{"key": "black", "material": "PLA", "colour": "#000000", "idx": "GFSNL04", "preset": "../filament.json"},
               {"key": "green", "material": "PLA", "colour": "#00AE42", "idx": "GFA00", "preset": "green.json"}],
 "parts": [{"name": "blackBody", "filament": "black"}, {"name": "greenLetters", "filament": "green"}],
 "supports": "black"}
```

**Slicing.** `build.sol` renders each tagged part. [models/lib/bambu.sol](../models/lib/bambu.sol) `slice` then writes a Bambu Studio assemble list (`--load-assemble-list`):
- every part goes at the same plate position, `pos_x`/`pos_y`, with its own `filaments` entry
- all parts share one `assemble_index`, so they merge into one object
- the plate gets `need_arrange: false`
- the nozzle map goes in `plate_params` as `filament_map_mode: Manual` and `filament_map: "1,2"`

Three things we found about this format:
- `need_arrange` must be a boolean.
- `subtype` must be a string (`normal_part`).
- The slicer takes the manual map from the comma-separated `filament_map`. With only `filament_maps` it falls back to auto and puts black on the wrong nozzle.

No template project is needed.

**Spools.** `plan` reads the printer's full report (the `pushall` reply) and lists the loaded feeds:
- AMS trays
- the X2D's `vir_slot` entries
- the older printers' `vt_tray`

For each tagged filament, it picks an unused feed with the same material and the nearest colour, within a distance of 80 in RGB. A feed whose `tray_info_idx` matches the tag wins ties. The feed then gives:
- **`ams_mapping`:** `unit * 4 + slot` for AMS trays, or 255 or 254 for the external spool
- **`ams_mapping2`:** on the X2D only
- **slicer extruder:** the physical nozzle whose `device.extruder.info[i].snow` names the feed, through the X2D's `physical_extruder_map = 1,0`

**Dispatch.** Every `dispatch.sol` calls `Bambu.main` with a short job record. It checks the following in order, and stops at the first failure:
1. There is no `print-run.json` yet.
2. The printer proves its identity over FTPS: the certificate chains to `models/lib/bbl-ca.pem` and names the serial.
3. The printer is idle and fault-free.
4. The spools match the tags.
5. The package was sliced for the same number of filaments.
6. On the X2D, the package was sliced with the same `filament_map`.

It then:
1. writes the record
2. uploads with curl
3. reads the file back and compares SHA-256
4. checks the printer is still idle
5. publishes one `project_file`
6. records the acknowledgement
7. polls until the first layer

`--dry-run` stops after the checks and prints the start command it would send.

**Secrets.** The access code comes from `BAMBU_ACCESS_CODE_<PRINTER>`, the printer's keys in `coscad/.env`, `BAMBU_ACCESS_CODE` in `~/.config/bambu/<printer>.env`, or `BAMBU_ACCESS_CODE`. It never appears on a command line or on disk:
- mosquitto reads its options through a bash process substitution fed on stdin
- curl reads its `user` line from stdin

The X2D pretty-prints its MQTT JSON across lines, so `mosquitto_sub -F %x` prints each payload hex-encoded on one line.

## Checks (2026-11-28)

**Plan.** Against the live X2D, while it was printing the flush-lettered boat, `plan` gave:
- `filament_map "1 2"`
- `ams_mapping [3, 255]`
- `ams_mapping2 [{0, 3}, {255, 0}]`

This is the mapping verified by hand earlier that day. The `--dry-run` start command is the same as the one the Python dispatcher sent.

**Slice.** `build.sol --tree --no-tower --cooldown` gives the same result as the template project:

| | Assemble list | Template project |
|---|---|---|
| Time | 1.46 h | 1.45 h |
| Black / green | 21.0 / 0.3 g | 20.7 / 0.3 g |
| Tool changes | 80 | 80 |
| Prime tower | none | none |
| Cool-down | present | present |

Per-nozzle toolpath bounding boxes match the template slice to 0.01 mm, so the letters stay registered in the hull.

**Upload read-back.** The remote SHA-256 of the uploaded package equals the local one.

**Letters.** `letters.sol` draws strokes with mitred bends instead of shapely buffers. The rendered green volume is 274.920 mm³, against 274.918 mm³ for the shapely outlines.

**Older printers.** The nine X1C/P1S/A1 job files compile and refuse to resend because their records exist. Their access codes are in `~/.config/bambu/<printer>.env` (mode 600), which `bambu.sol` reads. Read-only dry runs passed against the live X1C (FAILED state from an earlier job, external PLA, `ams_mapping []`), P1S (FINISH, external PLA, `[254]`) and A1 (IDLE): the belt parts got white on AMS slot 1 (`[0]`), and the three-colour test got white, black and yellow on slots 1-3 (`[0, 1, 2]`), the same mapping as the original dispatch.

## Not done

- CoScad itself has no colour attribute, so the tags live beside the source rather than in it.
- Only the X2D is wired with a nozzle path fallback, used when a spool is not loaded in either nozzle.
- The `withCooldown` fan list for the X1C and P1S (`P2`, `P3`) comes from their stock G-code but has not run on those printers.
