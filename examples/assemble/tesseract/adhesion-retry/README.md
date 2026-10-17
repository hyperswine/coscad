# A1 tesseract adhesion retry

Prepared offline on 17 October 2026. Printing is pending restoration of the
home network. No printer connection, upload or print is needed to rebuild
or verify this package.

## Print file

Open `fit-test_bed1.gcode.3mf` in Bambu Studio and select the A1 with its
0.4 mm nozzle, textured PEI plate and PLA. The saved file already contains
the slice; sending it does not require re-slicing. Check that the bed is
clear again when resuming and select the actual PLA tray. Print this trial
before the complete tesseract.

The plate contains one corner base, one cap, one short upright bar and one
full-length bar. The slicer estimates **39m15s of printing, about 46 minutes
including preparation, and 5.29 g PLA**. These are estimates, not measured
physical results.

## Changes from the failed coupon

- The cap's outer diagonal tip is clipped at x+y+z=9 instead of 3. This
  increases measured bed contact from 7.38 to **69.73 mm²** while keeping
  the pin vertical and the mating surfaces unchanged.
- An explicit **8 mm outer brim** with a **0.05 mm separation** replaces
  auto brim, which produced no brim on the failed slice.
- First-layer walls run at **25 mm/s**, infill at **35 mm/s**, with reduced
  acceleration. Later walls are limited to 45/60 mm/s instead of 200/300.
- Cooling is disabled for the first three layers and ramps up afterwards.
- Snug supports are enabled beneath the full bar's raised collars and necks.
  The rendered collar low points start about **0.805 mm** above the bed;
  the nominal neck bottom is **1.4 mm** above the bed. The upright short
  bar alone would not test these supports.
- Plate margins are 12 mm to leave room for the brim.

Nozzle and textured-plate temperatures remain **220°C / 65°C**.
The nominal cap pin fit remains 0.03 mm radial interference.

## Offline verification

`verification.json` records the print-file checksum, embedded mesh check,
four closed components, measured contact areas, settings, actual first-layer
brim/support extrusion, and early cooling. Its assembly section records
38 closed parts, no unintended overlap, 4,629 sampled motion positions and
unchanged collar capture and nominal pin interference.

This does not establish physical adhesion, support removal quality, cap
retention or long-term durability. After printing, inspect all four parts;
remove the bar supports and brim, check that both bar collars are complete,
then trial the short bar in the corner and close the cap. Stop if it needs
excessive force. Record the outcome before preparing the full set.

## Rebuild from the CoScad repository root

```sh
cabal run coscad -- next examples/assemble/tesseract/adhesion-retry/fit-test.assemble
python3 scripts/bambu-slice.py \
  examples/assemble/tesseract/adhesion-retry/fit-test_manifest.json \
  --printer 'Bambu Lab A1 0.4 nozzle' \
  --process '0.16mm Optimal @BBL A1' \
  --process-overrides examples/assemble/tesseract/adhesion-retry/process-overrides.json \
  --filament-overrides examples/assemble/tesseract/adhesion-retry/filament-overrides.json
python3 examples/assemble/tesseract/adhesion-retry/verify.py
```

The verifier uses the dependencies in `../requirements.txt`. Rendering also
needs OpenSCAD and BOSL2. Both override files are kept separately so
filament cooling cannot be accidentally overwritten by loading its preset.
Do not add `--print` while preparing offline.
