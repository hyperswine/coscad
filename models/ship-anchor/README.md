# Ship anchor

Decorative anchor designed entirely in CoScad. Approximately 100 mm long,
74 mm wide and 6 mm thick, with an eye, shank, crossbar, curved arms and
rounded triangular flukes. Prints flat without supports.

- Source: `ship-anchor.coscad`
- Validated mesh: `ship-anchor.stl` (one connected closed manifold)
- Preview: `ship-anchor.png`
- Slice: `ship-anchor_bed1.gcode.3mf`
- Profile: Bambu Lab A1, 0.4 mm nozzle, 0.20 mm Standard, PLA Basic,
  textured PEI, 30 layers, supports off.
- Estimate: 24m39s including preparation, 9.06 g PLA.
- Selected filament: yellow PLA, AMS slot 3 (`ams2`).

Rebuild from the repository root:

```sh
stack exec coscad -- stl models/ship-anchor/ship-anchor.coscad
python3 scripts/bambu-slice.py models/ship-anchor/ship-anchor_manifest.json --printer 'Bambu Lab A1 0.4 nozzle'
```

`print-run.json` records the printer's acceptance and startup state.
