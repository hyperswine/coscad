# X1C Bed 2 startup failure investigation - 29 October 2026

The confirmed reason is **automatic bed levelling failed**, code **0300-4002**.
At 14:21:46 Sydney time, the matching retry was FAILED at layer 0/282. Active
`print_error` was 0 and HMS was empty, but **`fail_reason` retained `50348034`**
(hex `03004002`). The owner independently read **`0300-4002 100529`** on the
printer screen. The installed and upstream Bambu Studio error catalog says:
“Auto Bed Leveling failed; the task has been stopped.”

Vendor source: [Bambu Studio catalog](https://github.com/bambulab/BambuStudio/blob/master/resources/hms/hms_en_093.json).
The exact locally inspected catalog hash and matching description are saved in
`vendor-error-descriptions.json`. Raw selected fields are in `fresh-status.json`.

## What the sliced file shows

The failed file read back from the printer has SHA-256
`b528a1a89dd6850d5675c416a6da22e17814326009e2c85517088f568289a88f`, identical
to the geometry-validated local package. There is no evidence of upload damage.
A fresh thermal check covered 282 layers and 539,272 deposition moves: textured
PEI, bed 55 C and nozzle 220 C for initial and later printing layers. Temporary
startup temperatures (250 C for purging and 140 C for probing) come from the
same vendor template used in earlier working shelf prints.

The embedded machine-start template is identical to the currently installed
X1 Carbon 0.4 mm template and to the successful upright loose-fit shelf test.
After removing the slicer's `M73` progress/time annotations, there is exactly
one executable startup difference:

```gcode
; Successful small upright rail: roughly 29 x 29 mm central probing area
G29 A X113.329 Y113.329 I29.3414 J29.3414
; Bed 2: roughly 195 x 195 mm probing area
G29 A X30.5662 Y30.5662 I194.868 J194.868
```

Both rectangles are inside the nominal 256 mm bed. The larger rectangle follows
Bambu's template placeholders for first-layer bounds; it is not a custom probing
command introduced by this model. See `startup-comparison.diff` and the extracted
`failed-startup.gcode.txt`. No unresolved placeholders or explicit stop,
emergency-stop or user-pause commands were found in the executable startup.

The *same* Bed 2 package had previously reached layer 2 before the owner-reported
spool problem and intentional cancellation. That further argues against a
startup file that fails deterministically on every attempt. It does not prove
all geometry, adhesion or printer conditions are correct.

## What remains unknown

The larger probing footprint could encounter a bed/plate/nozzle/probing issue
outside the small central rail region. This is an inference, not a diagnosis of
plate warping, dirty nozzle, tramming, sensors or mechanics. The exact failing
probe point is unavailable. The current FTPS service lists print files and
camera/timelapse folders; attempts to access `/log`, `/logs` and `/logger` were
rejected. No detailed sensor/probing firmware log was obtained. Selected SD
metadata and the readback hash are recorded in `sd-listing.json`.

Before another print, inspect that the plate is seated flat, with no debris
underneath or nozzle-tip residue, then run the printer's normal full-bed
calibration. A repeatable failure should be investigated as a levelling fault;
do not bypass it by disabling levelling. No calibration, movement, alert-clear,
resume, restart or new print command was sent during this investigation.

## Why earlier status checks missed the cause

CoScad's `Printer.summary()` kept `print_error` but omitted `fail_reason`.
The former had cleared to zero after the job stopped; the latter retained the
actual cause. `scripts/bambu-print-run.py` now includes `fail_reason`,
`mc_print_error_code` and `stg_cur` in saved summaries. A focused reproduction
verifies that the retained reason survives when the active error is zero;
25 preflight and 33 slicer tests also pass. Historical failure reasons remain
separate from active errors and do not independently block new jobs.

The FP-RISC dashboard's current `std/printertelemetry.fpr` also does not retain
`fail_reason`, so its absence can similarly leave a FAILED job without a visible
reason. That dashboard change/deployment was not part of this investigation.

The original retry's two-minute startup log ended before levelling; it did not
capture the later transition to failure. This report does not infer a precise
failure time from that gap or parse the screen's final six digits as a timestamp.
