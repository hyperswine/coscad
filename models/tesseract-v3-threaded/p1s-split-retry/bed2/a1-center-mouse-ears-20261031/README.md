# Center-only A1 support retry - 31 October 2026

The owner reported poor lower diagonal female-thread sockets on the previous
inner frame despite the supports printing properly; upper diagonals printed
well. This experiment prints **only the unchanged center frame** on A1, with
reinforced bed-only tree supports and four removable mouse-ear pads. The owner
explicitly confirmed the A1 plate clear. Physical AMS slot 1 is retained from
the owner's prior preference, and live white PLA is required before start.

## Changes from the earlier center plate

| Setting | Previous X1C Bed 2 | A1 center-only retry |
|---|---|---|
| Model contents | Center + four hubs | Center only |
| Tree walls | 1 | 2 |
| Tree branch diameter | 2 mm | 3 mm |
| Tree branch angle | 45 degrees | 35 degrees |
| Top/bottom support Z gap | 0.24 mm | 0.16 mm |
| Support XY gap | 0.4 mm | 0.3 mm |
| Top interface layers | 2 | 3 |
| Interface spacing | 0.35 mm | 0.25 mm |
| Support/interface speed | 150 / 80 mm/s | 60 / 35 mm/s |
| Support-base first-layer expansion | 8 mm | 1 mm |
| Adhesion | Full brim/expanded tree bases | Four 7.5 mm radius, 0.2 mm high removable pads |

Six walls, 20% gyroid, 0.2 mm layers, slow initial walls/fill, Textured PEI
55 C bed / 220 C nozzle remain. A 12-second layer cooling target with 5 mm/s
minimum, wall crossing avoidance and inclusion of supports in crossing
avoidance are enabled. Existing thread-bore support blockers are retained.

Closer, denser PLA support interfaces may be harder to remove. Trim the thin
mouse-ear pads carefully from the four bottom contact areas after printing;
they are sacrificial additions, not changes to the original frame/thread mesh.
The original orientation remains, so this trial tests support/adhesion changes
and A1 printing together rather than isolating one cause of the previous defect.

## Mouse ears and verification

Bambu's source defines [native painted ears](https://github.com/bambulab/BambuStudio/blob/master/src/libslic3r/Brim.cpp)
and [3MF annotation storage](https://github.com/bambulab/BambuStudio/blob/master/src/libslic3r/Format/bbs_3mf.cpp).
The local CLI retained the annotations/settings but repeatedly emitted no
actual brim paths. Those attempts were rejected before dispatch. Their exact
cause was not established; coordinate changes/normalization did not resolve
it. `native-painted-ear-rejection.json` records the limitation.

The delivery file instead uses four **explicit CoScad-generated first-layer
mouse-ear pads**, added as normal volumes in the same object. These are not
native painted-brim annotations. Full brims and rafts are disabled. Every pad
covers exactly one of the original four tiny first-layer contact patches.
`mouse-ear.coscad` is the pad source; original `1-inner.stl` remains byte-identical
to the prior center model. `prepare.py` refuses an existing print record.

The exported A1 package passed single-object/four-pad geometry, actual pad
first-layer paths, bed/keep-out, no warnings, sampled bed-connected support
roots, zero support-thread intrusions and full-deposition thermal checks.
Estimate: **3 h 39 min, 38.94 g PLA**. `package-validation.json` and
`experiment-validation.json` preserve hashes and validation evidence. These
checks do not prove improved female-thread surface quality before printing.

## Reproduce and dispatch

From repository root with the modelling Python environment:

```sh
python3 models/tesseract-v3-threaded/p1s-split-retry/bed2/a1-center-mouse-ears-20261031/prepare.py
# Slice full-project.3mf using the generated native A1 presets, --arrange 0 --orient 0.
# Exact selected arguments are saved in slice/slice-command.json.
python3 models/tesseract-v3-threaded/p1s-split-retry/bed2/a1-center-mouse-ears-20261031/verify.py
```

`../../print_center_a1.py` without flags only reads status. Its one-shot
`--start --bed-clear` mode repeats actual package and thermal validation,
checks live A1/nozzle/AMS-slot readiness, freezes the upload bytes, verifies
full FTPS read-back SHA-256, saves exclusive intent and sends one print start.
An existing `print-run.json` prevents repeats; an uncertain start requires
read-only reconciliation. Credentials stay in the existing local runtime.

## Accepted start

The checked upload was read back with an identical SHA-256 and one start was
acknowledged successfully at 22:43 Sydney time. Fresh telemetry identified the
new 282-layer job RUNNING in preparation, with the bed targeting 55 C and no
active print error or HMS. AMS slot 1 was requested; actual selected tray and
first-layer progress remain to be checked. `print-run.json` and
`dispatch-summary.json` preserve the bounded startup observations.

[Actual first-layer support and mouse-ear toolpaths](first-layer-supports-and-ears.png)
show the four sacrificial pads and the tree bases.

## First-layer startup verified

At **22:51:22 Sydney time on 31 October**, the unique job reported
**RUNNING, layer 1/282**, about **211 minutes remaining**, measured nozzle
**220.1 C** / bed **55.4 C**, with targets **220/55 C**, no active print error
or HMS, and **AMS tray 0 (physical slot 1)** selected. This confirms startup
and selected filament; adhesion and lower female-thread quality still require
physical inspection. The bounded observer ended after this checkpoint; no
recurring monitor or additional start command was created.
