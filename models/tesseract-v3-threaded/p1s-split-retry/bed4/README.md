# Bed 4 P1S comparison after poor A1 print - 30 October 2026

The owner left the failed-quality A1 result on its plate for photographs and
requested G-code review followed by a fresh P1S comparison if no clear defect
was found. The owner separately confirmed the P1S bed clear and print when ready.
The A1 remains untouched. Its [outcome and review](a1/outcome-20261030-171001/outcome.json)
and three photographs show substantial spaghetti/displaced struts despite
printer FINISH/291 and no active faults. Earlier layer-96 photos only established
that the lower portions were then intact.

## Review conclusion and comparison limits

The exact A1 package still matches its dispatched hash. Temperature checks pass
through all 291 layers; there are no missing deposition layers, unexpected layer
height steps or obvious mid-print cooling-off commands. Actual brim paths are
present on all eight objects. This does not establish that the design is easy
to print or that every toolpath is optimal.

The upper shoulder begins at about 47.3 mm mid-layer (layer 237), growing from a
3.5 mm shaft radius to about 5.12 mm. This roughly 1.62 mm unsupported annular
growth is a real printability risk. Its overhang walls are slowed to 10 mm/s,
but curling, sagging or nozzle contact could still disturb a slender part.
Long inter-object travel and fast vendor infill defaults are further risks.
None is proved to be the initiating cause by the end-state photo or telemetry.
A possible later design change would add a gradual underside transition; this
comparison deliberately retains the original geometry to test it on the P1S.

All eight STL hashes, packing JSON and selected process overrides match the A1
attempt. The P1S has a different loaded external PLA spool and native machine,
motion and cooling profile, so results cannot isolate hardware alone.

## Fresh P1S slice

- P1S model C12, 0.4 mm nozzle; native P1S startup/end G-code.
- Eight upright struts, 8 mm outer brims, no support paths.
- Six walls, 20% gyroid, 0.2 mm layers; slow first-layer speeds.
- Textured PEI 55 C, nozzle 220 C throughout deposition.
- Estimate: 2 h 26 min total, 29.0 g PLA, 291 layers.

`package-validation.json` checks actual exported geometry/identity, settings,
placement, bed keep-out and brim paths, with no slicer warnings.
`thermal-preflight.json` independently checks every deposition layer.
`print_bed4_p1s.py --start --bed-clear` repeats thermal checks against a frozen
snapshot, verifies fresh readiness/filament, uploads over verified FTPS, reads
back matching SHA-256 and sends one start after saving exclusive intent.
`print-run.json` is the actual dispatch/startup record and blocks another start.
Without flags the helper only reads status. No automatic retry is permitted
after an uncertain start. Completed printing and fit remain unverified.

From repository root, the preparation/verification commands are:

```sh
python3 models/tesseract-v3-threaded/p1s-split-retry/prepare.py --bed 4 --printer p1s
# Use the generated presets and slice/slice-command.json, --arrange 0 --orient 0.
python3 models/tesseract-v3-threaded/p1s-split-retry/verify.py --bed 4 --printer p1s
python3 scripts/bambu_preflight.py models/tesseract-v3-threaded/p1s-split-retry/bed4/slice/tesseract-bed4-p1s.gcode.3mf --presets models/tesseract-v3-threaded/p1s-split-retry/bed4/presets --plate 'Textured PEI Plate'
```

## Actual startup acceptance

Verified upload read-back matched the validated package; one project-file start
received success at 17:17:16 Sydney time. Fresh status confirmed the unique
291-layer job RUNNING. The recurring part-cooling fan warning appeared during
startup and cleared at 17:21:27 on fresh status without reset/alert clearing.
At 17:22:48 the job reached layer 1/291, RUNNING with zero print error and no HMS,
55/220 C targets, measured about 55.0/220.3 C and 139 minutes remaining.
`dispatch-summary.json`, `startup-followup.json` and `startup-fan-alert.json`
preserve that evidence. This proves startup only, not full-height print quality.

## Physical outcome and next experiment - owner report, 30 October

The owner reports that this P1S attempt was going okay until after roughly the
halfway mark, then appeared to sabotage itself and collapsed into spaghetti.
Momentary wobble or uneven extrusion causing a nozzle strike are the owner's
hypotheses, not a diagnosed mechanism. The exact first failing layer/height is
unknown; halfway could refer to time or displayed progress rather than height.
Fresh photographs in `outcome-20261030-202153/` confirm substantial spaghetti
and displaced/collapsed material. The printer nevertheless reported FINISH/291
with no active fault. No printer controls were sent while recording this result.

Both the A1 and P1S versions have now failed physically. This does not isolate
printer hardware or prove the earlier upper-collar hypothesis. The requested
next experiment is staged under [sequential-retry](../sequential-retry/README.md):
more spacing, actual per-object sequencing, manufacturer collision limits
retained, and additional small-layer cooling time. Geometry remains unchanged.
No new print is dispatched as part of this preparation.
