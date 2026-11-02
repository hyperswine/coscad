# Enlarged monohull review, 2026-11-01

The 236 x 124 x 64.02 mm design looks coherent as a printable flotation prototype.
This review did not dispatch it or change its geometry, presets, or sliced files.

## Fresh checks

- All three CoScad sources re-rendered with the native compiler to byte-identical
  STL files. The printable body is closed, consistently wound, and has one
  positive shell with ten enclosed chamber surfaces. No downward faces above the
  base exceed 45 degrees from vertical.
- The model contains 249.33 cm3 of plastic and 191.06 cm3 of enclosed air.
  Estimated homogeneous PLA mass is 309.17 g; existing slicer estimates are about
  306 g and 16.1-16.4 hours.
- All three exported printer packages retain a closed, eleven-surface-shell mesh
  with relative volume difference below 0.000013% from the original. Ten negative
  chamber shells are intentional voids, not ten loose printed parts.
- Fresh thermal preflight checked actual deposition on every one of 320 layers.
  All are Textured PEI, 225 C nozzle. P1S/X1C beds are 55 C; **A1 is 65 C**,
  consistent with its filament preset but different from the README's blanket
  55 C statement. Package hashes match the previously saved reports.
- The existing geometry checks give minimum chamber-to-exterior thickness
  2.79 mm and chamber-to-cockpit thickness 2.27 mm. These were reproduced by
  calling the existing geometry functions in an isolated analysis environment.

## Flotation qualifications

The existing static draft results reproduce: about 25.4 mm dry in fresh water,
38.6 mm dry freeboard, and 24.8 mm rim clearance when swamped with intact chambers.
This assumes sealed chambers, homogeneous PLA density 1.24 g/cm3, no payload,
and water freely communicating with the swamped cockpit.

The existing damaged-chamber comparison holds heel and pitch at zero. Extending
those same hydrostatic functions to solve both equilibrium moments for each of
the ten possible flooded chambers changes the worst result substantially:
flooding one stern chamber gives about **4.7 degrees heel, 3.9 degrees bow-up
trim, and only 9.1 mm minimum rim clearance** in fresh water, rather than the
21.4 mm level-only clearance. All ten near-upright equilibrium solves converged.
This extension is not an independent fluid solver or a dynamic stability test.

The dry righting curve beyond downflooding is also conditional: the current
samples put the first submerged rim between 35 and 40 degrees. Its larger-angle
dry results continue to assume an empty cockpit, so they cannot establish how an
open boat actually recovers once water enters. Partially retained water, wave
impacts, progressive leaks, and local stability about the damaged equilibrium
have not been tested here. It is not demonstrated to self-right after inversion.

“Cannot sink” is therefore too strong. The chamber geometry offers useful reserve
buoyancy, including one idealized chamber failure, but real wall/seam leaks and
multiple chamber flooding are still physical test conditions. A mesh being closed
does not establish that an FDM print is watertight.

Before a surf trial: weigh the print, test dry flotation, deliberately fill the
cockpit, heel it both ways, and check for leakage over time. Compare observed
waterlines with the calculations. No geometry change is clearly required by this
review for that controlled test; damaged-chamber wave margin is the main design
limitation identified.

## Evidence

- `native-rerender.json`: fresh source-to-STL reproducibility.
- `geometry-and-thermal.json`: independent mesh inspection and actual thermal gate.
- `exported-mesh.json`: actual mesh retained inside each sliced package.
- `flooded-equilibrium.json`: reproduced drafts and all ten extended equilibrium cases.
- `check_flooded_equilibrium.py`: review extension; needs the same analysis
  dependencies as `../verify.py`. It writes only its adjacent review JSON file.

Original design and package files were left unchanged.
