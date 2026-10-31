# More sequential parts and compact struts - 31 October 2026

The owner asked whether more struts/bars could print per object, and whether a
close cluster of four struts could reduce nozzle travel. These were initially
staged X1C experiments; preparation did not modify the preceding mixed trial.
The owner subsequently authorized the X1C staggered and P1S compact prints,
as recorded below. Existing threaded geometry is translated only.

## What fits

| Candidate | Actual mode | Contents/order | Estimated total | PLA |
|---|---|---|---:|---:|
| [Four sequential struts](four-struts-sequential/slice/tesseract-bed4-x1c.gcode.3mf) | By object | Four complete struts, front to back in staggered rows | 4 h 10 min | 14.77 g |
| [Three struts then a bar](three-struts-bar-sequential/slice/tesseract-bed4-x1c.gcode.3mf) | By object | Three complete struts, then the tall bar | 4 h 34 min | 13.65 g |
| [Four compact struts](four-struts-compact/slice/tesseract-bed4-x1c.gcode.3mf) | By layer | Four struts together in a 2 x 2 grid, 28 mm centre spacing | 1 h 30 min | 14.77 g |

The previous two-part sequential layout was conservative rather than maximum
strut capacity. Struts are 58.28 mm high, below the installed X1C's 90 mm
non-last-object lid limit. Four fit in staggered rows with disjoint Y bands,
so the gantry can clear them. The machine retains its 68 mm maximum head radius,
34 mm rod height and 33 mm rod-distance settings; no physical limit is reduced.

Bars remain 99.2 mm high and therefore must be last in strict per-object mode.
More struts per plate do not remove the one-bar-per-run constraint: the complete
12-bar set still needs 12 bar-containing sequential runs if every bar is printed
individually. Fewer total runs would require batching bars by layer, a grouped
layout, or changing orientation/design and revalidating it.

The slicer's rod/lid/order logic is documented in its
[primary implementation](https://github.com/bambulab/BambuStudio/blob/master/src/libslic3r/Print.cpp).
All three actual slices returned success with no warnings or collision errors.

## The close-packed alternative

The compact file is deliberately **by layer**, not four close sequential objects.
A temporary offline probe changed it to by-object mode with the same manufacturer
limits: Studio rejected it with object conflicts (return code -63). Only the
rejection report is retained in `compact-sequential-rejection.json`; the unsafe
temporary project/output was deleted.

The compact file keeps 8 mm brims. Its conservative projected envelope masks
partially overlap, but the actual deposited brim footprints remain about 5.65 mm
apart. All four objects have deposition in every one of 291 layers. Wall-crossing
avoidance is enabled; native automatic Z-hop settings are unchanged. Full-height
geometry/keep-out and exact 55 C Textured PEI / 220 C thermal checks passed.
The layer-time cooling target remains 12 seconds with a 5 mm/s minimum speed.

Measured within-layer inter-object XY travel includes retract/wipe/helix motion:
mean about 30.1 mm, maximum 42.3 mm, 873 transitions and 26.3 m total. The prior
P1S eight-strut file had mean 42.0 mm, maximum 55.6 mm, 2,037 transitions and
85.5 m total. `travel-comparison.json` records that comparison. It changes part
count, layout, crossing avoidance and machine profile; it does not isolate the
causal effect of spacing alone.

Shorter moves reduce travel but do not prove less physical contact. A nozzle can
still hit curling or excess material on the part it is printing. The strut's
roughly 1.62 mm upper-collar overhang and any dynamic wobble remain uncorrected.
This is an experiment in packing/travel, not a diagnosed fix.

## Sequential motion audit and end lift

The actual per-object files were checked for complete object order, per-object
layer progression/full heights, exported manufacturer clearance, every deposition
layer's temperatures, and sampled head/rod return paths including the end moves.
The struts-plus-bar layout passed unchanged. The four-strut layout initially
missed the audit's additional 0.5 mm end-parking margin: native parking was at
Z58.7, about 0.418 mm above the actual mesh top. This was a conservative-margin
finding, not a diagnosed physical collision. Its native end-lift template is
now opt-in 5 mm instead of 0.5 mm, with a full re-slice and verification.
All physical clearance settings remain unchanged. The bounded helper refuses
non-finite, negative, excessive or out-of-height-range end lifts; see
`end-lift-bound-checks.json`. Other jobs retain the original default behavior.

`validation-summary.json` contains positive geometry/order/thermal/clearance
results. Each sequential folder has `sequential-validation.json`; the compact
folder has `compact-validation.json`. Checks are static/sampled, not measured
hardware clearance or physical print-quality acceptance. Completed physical
acceptance of the four-strut candidates is pending. The original three files
are X1C-specific; the separately sliced P1S compact variant is documented below.
A1 use would still require its own machine slice and verification.

## Reproduce

From repository root with the modelling Python environment:

```sh
python3 models/tesseract-v3-threaded/p1s-split-retry/layout-experiments-20261031/prepare.py
```

Slice each `full-project.3mf` with its generated presets and `--arrange 0
--orient 0`. Exact local arguments are in its `slice/slice-command.json`.
Regenerate each shared `package-validation.json` using
`p1s-split-retry/verify.py --bed 4 --printer x1c --output-dir <candidate-folder>`,
then run:

```sh
python3 models/tesseract-v3-threaded/p1s-split-retry/layout-experiments-20261031/verify.py
```

Preparation refuses to overwrite dispatched candidates. New retries require
separate run records and fresh owner authorization/bed-clear confirmation.

## Requested printer-specific trials - 31 October

The preceding mixed X1C bed now has owner-reported physical success. The owner
requested the four-staggered-strut X1C file and the compact four-strut layout
on P1S. The latter has a freshly sliced and verified
[P1S-specific variant](four-struts-compact/p1s/README.md), estimated at 1 h 28 min.
The original compact X1C file is preserved. Both printers reported idle/FINISH,
loaded external PLA and no active faults on fresh reads. The owner subsequently confirmed both plates clear; both checked uploads
were read back with matching hashes and one start accepted on each printer.
See each variant's dispatch-summary.json and print-run.json for current startup evidence.

`../print_strut_trial.py` accepts `x1c-staggered` or `p1s-compact`; without
`--start` it only reads status. Starts require `--bed-clear` and refuse any
existing trial print-run record. Geometry, sequence (where applicable), actual
plate/temperatures and machine identity are reverified immediately before
upload; read-back hashes must match and one start intent is saved before send.
`dispatch-guard-checks.json` records offline readiness-rejection checks.

### Four-strut startup acceptance

The P1S compact job reached layer 1/291 at 12:52:45 Sydney time, with 81
minutes remaining, 55 C bed and about 220 C nozzle. Its recurring startup
fan-speed warning cleared on fresh status without control/alert clearing.
The X1C staggered sequential job reached layer 1/1164 at 12:55:10, with 242
minutes remaining and 55/220 C. Both had no active errors/HMS at these
observations. Each uploaded package was read back with a matching hash and
one start accepted. Completed print quality is still pending. Per-variant
dispatch-summary.json files preserve the observations; P1S startup-followup.json
retains the fan-warning follow-up without overwriting the original warning.

## Physical results - 31 October evening

| Trial | Owner-observed result | Fresh camera / telemetry evidence |
|---|---|---|
| X1C, four staggered struts, per object | Each strut printed perfectly | FINISH/1164, no faults; three saved JPEGs have lower-image corruption and cannot verify quality |
| P1S, four compact struts, by layer | Almost perfect below the threads, then failed around the threaded ends; possible contact during travel between threads | FINISH/291, no faults despite failed parts; three photos document spaghetti/displaced material |

The [comparison record](physical-comparison-20261031.json),
[P1S outcome/photos](four-struts-compact/p1s/outcome-20261031-214805/outcome.json),
and [X1C outcome/photos](four-struts-sequential/outcome-20261031-214805/outcome.json)
keep owner observations separate from camera review. No initiating collision or
exact first failing layer was recorded. Different printers, loaded filament,
spacing and scheduling prevent isolating the cause. Shorter inter-object travel
was insufficient on this P1S trial; the X1C per-object layout is the demonstrated
working baseline for the same strut geometry. No new print or corrective
controls were sent during this review.

The X1C camera returned corrupt-looking lower images through the authenticated
dashboard; direct/alternate decoder attempts produced no usable replacement.
That camera issue remains separate from the owner's successful print inspection.
