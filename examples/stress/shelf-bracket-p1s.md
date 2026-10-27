# Shelf bracket on the P1S with SUNLU PLA+ 2.0

Owner reported on 2026-10-27 that the P1S has just been loaded with SUNLU
PLA+ 2.0. Colour, AMS slot/external feed and spool calibration are unspecified.
This report is also in `models/printer-filament-log.jsonl`.

Use `shelf-bracket-p1s.assemble`, then `coscad next --backend=native`.
The P1S machine preset allows 250 mm Z, so this assembly declares that limit.
The bracket is 60 x 40 x 50 mm and its model/print Z orientation is unchanged.

Slice its manifest using:

```sh
python3 scripts/bambu-slice.py PATH_TO_MANIFEST \
  --printer 'Bambu Lab P1S 0.4 nozzle' \
  --filament 'SUNLU PLA+ 2.0 @BBL X1C' \
  --process-overrides examples/stress/shelf-bracket-process.json \
  --filament-overrides examples/stress/shelf-bracket-sunlu-filament.json
```

The installed SUNLU X1C preset explicitly supports the P1S 0.4 mm nozzle.
Vendor presets in subdirectories now resolve by unique name, including their
inheritance chain. Ambiguous names and inheritance cycles are refused.

The native profile uses 220 C nozzle and 55 C textured PEI bed. Max volumetric
flow is reduced from the profile's 22 to 12 mm3/s as an uncalibrated starting
point for this spool. Keep 4 walls, 100% rectilinear infill, normal supports
including on the base, and the reserved 5 mm brim.
[SUNLU's product page](https://uk.store.sunlu.com/products/moq-6kg-pla-2-0-upgraded-pla-pla-plus-3d-printer-filament-1kg)
lists speed-dependent nozzle temperatures and a 50-60 C bed range.

The local P1S slice in `output/shelf-bracket-p1s-sunlu/` passed exported-setting
and support-toolpath checks. Estimate: 1 h 40 m 40 s total, 64.34 g.
`verification.json` records settings, hashes and the unchanged geometry link.
No printer was contacted or print dispatched. Before dispatch, verify the bed,
printer status and which feed contains this spool.

The earlier FEA uses generic homogeneous PLA properties. It has not been
recalibrated to this spool; PLA+ is not proof of a higher stiffness or allowable
load, and the earlier numerical safety factors are not SUNLU-specific ratings.
