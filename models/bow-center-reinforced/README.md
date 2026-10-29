# Elastic-cord toy bow replacement centre

Source example: ../../examples/assemble/bow3/center3.coscad.
User confirmed this is the elastic-cord toy and that the P1S plate is clear.

Changes:
- Widened the body from 30 to 36 mm; each side of the 28.5 mm arm
  pocket is now 3.75 mm thick instead of 0.75 mm.
- Added a capsule-shaped 40 x 14 mm recess, 1.2 mm deep, on the +Z face.
- Existing 110 mm length, 14 mm height, arm pockets, four M4 clearance
  holes and underside captive-nut seats are preserved.

Checks:
- Closed manifold, one connected component, 1156 triangles.
- Assembled bow: zero overlaps and zero clearance warnings.
- Boolean probes for M4 bolts, AF7 nuts, arm pockets and face recess:
  zero interference; see validation.json and verify.py.
  validation.json was recorded with OpenSCAD boolean probes. Since
  2026-10-29 verify.py runs the same probes with manifold3d (numpy and
  manifold3d, no OpenSCAD) and gives the same report for bow-center.stl.
  A fresh native `coscad stl bow-center.coscad` render has 1158 triangles
  and passes all four probes (`verify.py --mesh PATH --output PATH`).
- 190 non-rendering compiler tests passed. Full geometry suite not run;
  this part and its assembly were rendered and checked directly.
- Example's SCAD and geometry regression expectations updated.

Print: P1S, 0.4 mm nozzle, PLA, 0.20 mm layers, four perimeter walls,
70 layers, no supports, textured PEI. Broad underside on bed, recess up.
Estimate: 30m49s printing, about 37m including preparation; 24.39 g.

Re-slice from the repository root:

```sh
python3 scripts/bambu-slice.py models/bow-center-reinforced/bow-center_manifest.json --printer 'Bambu Lab P1S 0.4 nozzle' --slicer-arg=--wall-loops --slicer-arg=4
```

print-run.json records the printer's response. Startup confirmation is
not a completed print or a physical durability test.

## 2026-10-07: retry on A1

P1S job reported FAILED at layer 3/70; user reports a downward Z-axis
squeak. Maintenance note: ../../../3d-models/maintenance/P1S.md.
Re-sliced the same part under a1/ for the A1 with four perimeter walls.
The user confirmed the A1 plate is clear. Its previous egg-box job reported
FAILED at layer 256/390; the reason was not established.
