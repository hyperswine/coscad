# Egg box

One open-top box with a 2 x 2 grid of compartments for four XL eggs.
Each compartment has a 60 x 60 mm footprint (6 mm rounded corners) and
75 mm usable depth. Check unusually large eggs against these dimensions;
XL is not a fixed dimensional specification.

Outside dimensions: approximately 127.2 x 127.2 x 78 mm.
Walls/dividers: 2.4 mm. Floor: 3 mm. Exterior corner radius: 4 mm.
All four compartments share a single continuous floor. Print upright.

The geometry is pure CoScad. `generate.py` emits the repeated cells because
the current CoScad language does not provide a grid loop. For another grid:

```sh
python3 models/egg-box/generate.py --columns 2 --rows 2
stack exec coscad -- stl models/egg-box/egg-box.coscad
python3 scripts/bambu-slice.py models/egg-box/egg-box_manifest.json --printer 'Bambu Lab A1 0.4 nozzle'
```

Run from the CoScad repository root. Regeneration overwrites this model's
source and manifest; recompile and re-slice afterward. Larger grids must
fit the target build plate (the supplied manifest is for a 256 mm A1 bed).

Validation: closed manifold, one connected component, 1036 triangles;
see mesh-check.json. Preview: egg-box.png.

Slice: Bambu Lab A1 0.4 mm nozzle, 0.20 mm Standard, Bambu PLA Basic,
textured PEI plate, supports disabled, 390 layers. Estimated material
173.69 g; print time 4h22m10s, approximately 4h28m including preparation.
Prepared for yellow PLA in AMS slot 3 (ams2).

Print sent after the user confirmed the plate clear on 2026-10-06. The A1
accepted the job and reported PREPARE for egg-box_bed1.gcode with no print
error. See print-run.json. Completion has not yet been verified.
