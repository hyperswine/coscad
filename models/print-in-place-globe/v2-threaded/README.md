# Threaded globe revision 2, 2026-10-22

The owner reports the original P1S globe printed well, but supports were awkward
to remove and rough handling broke the neck between gimbal and base. This
revision keeps the successful 64 mm globe, relief and captive bearing geometry.
The globe and gimbal print together as a free-moving captive assembly; a separate
base screws into the bottom of the gimbal foot after support removal.

## Changes

- 90 mm base; assembled height 112.5 mm. Both pieces fit on one P1S plate.
- Gimbal section grows from 3.5 x 6 mm to 5.5 x 10 mm. A broad 36 mm foot blends
  into the lower ring and houses the female thread; no thin connector joins it
  to a permanently attached base.
- Coarse right-hand thread: 24 mm major diameter, 20 mm nominal core, 6 mm pitch,
  12 mm engagement. A 3 mm male lead-in and chamfered socket mouth ease starting.
  The socket cutter is enlarged 0.4 mm radially and 0.25 mm along each flank.
  The owner confirmed the matching P1S thread-fit coupon worked on 2026-10-24.
  Full-assembly strength and support removal still need physical inspection.
- Sloped thread flanks and a conical socket roof avoid internal support/bridging.
- Six walls and 20% gyroid for the gimbal/foot and base. The globe retains its
  proven three walls and 12% infill to avoid unnecessary weight and print time.
- Automatic tree supports **from the build plate only**. Bearing and socket
  support blockers are included as modifiers. One tree wall, two interface
  layers, 0.24 mm top gap and 0.4 mm XY gap aim to make removal easier.

## Prepared files

- `globe-project.3mf`: editable multipart P1S project, including support blockers.
- `slice/globe-v2-p1s.gcode.3mf`: verified P1S 0.4 mm / PLA / Textured PEI job.
  Estimate: **8 h 24 min, 115.25 g**, 659 layers at 0.16 mm.
- `thread-coupon-project.3mf`: editable support-free thread fit test.
- `coupon-slice/thread-coupon-p1s.gcode.3mf`: P1S fit test, about **1 h 2 min,
  14.45 g**. The two coupons use the actual male/female thread geometry.
- `captive-assembly.stl` and `base.stl`: printable geometry. If using STLs,
  import the bearing/socket blocker separately and mark it as a support blocker;
  the multipart project already does this correctly.
- `assembly.coscad`: generated CoScad source, including the helical threads.
- `preview.png`: assembled/exploded illustration; colours distinguish geometry
  and do not imply a multicolour slice.

The owner approved the thread-fit coupon on 2026-10-23 and confirmed a clear
P1S bed. The LAN API accepted one dispatch; `coupon-print-run.json` records its
acknowledgement and startup observations. The full globe was subsequently dispatched on 2026-10-24; see the latest section below.
P1S is retained from the successful previous print. Recheck physical bed, spool, printer faults and
cooling before each dispatch. The 2026-10-23 coupon preflight reported external
PLA, a 0.4 mm nozzle, print error zero and an empty HMS alert list. The earlier
part-cooling warning was absent at this preflight.

## Verification and limits

`validation.json` records closed, single-body meshes for each rigid part;
72 globe rotation/axial-offset poses have zero detected interference. A 49-pose
screw sweep covers two turns and 12 mm travel with zero interference. Deliberate
wrong-phase and lateral-offset controls do collide. The sphere/relief matches
the successful mesh after translating it upward 10 mm, within mesh arithmetic
tolerance. This validates geometric clearance, not printed fit or break strength.
`project-geometry-validation.json` also checks the exported sliced package:
each mesh and support blocker retains its intended placement within 0.001 mm.

`toolpath-validation.json` checks the actual G-code, including sampled G2/G3 arcs
and the P1S (0,2) extruder offset. It finds no support in the socket, male thread
or moving bearing region and no model path bridging the shaft/bore gap. A 0.5 mm
support raster checks 3,320 support components across 606 support layers, with
six components at the bed and no later component lacking prior support within
the stated sampling tolerance. This supplements the effective bed-only flag;
it is not exact extrusion topology or a physical support-strength test.

Both slicer result files report Success and no plate warning. The full slicer
log retains a tree influence-area diagnostic near layer 545; it is not hidden.
The toolpath checks pass, but physical support behaviour remains untested.
The CLI's omitted P1S model ID is filled from its installed C12 vendor profile;
the G-code is unchanged. ZIP integrity and hashes are recorded in
`package-validation.json` and `coupon-package-validation.json`.

## First physical test and assembly

Print the small thread test first with the intended PLA. Let it cool, remove
brims, and screw the female ring onto the male piece. It should start by hand
and seat after roughly two turns without forcing. Adjust the shared clearance
in `generate.py` and rebuild both mating pieces if fit is too tight or loose.

For the full print, let the bed and parts cool. Hold the broad gimbal foot while
snipping support branches in short sections. Remove support contacts before
trying to spin the globe. Avoid levering the sphere or pulling whole trees off
against the ring. Clear the brim from the socket mouth, then screw the gimbal
onto the base clockwise viewed from above, holding the foot close to the joint.
Stop finger-tight when the broad foot seats against the base.

## Rebuild locally

Use the Python environment from the parent project's README (trimesh, manifold3d,
numpy, scipy, matplotlib). OpenSCAD 2025.05.16 with Manifold and Bambu Studio
02.08.02.61 were used for the recorded meshes and packages. Installed Bambu vendor machine/templates are refreshed;
the saved successful P1S project supplies the baseline print settings.

From the CoScad repository root:

```sh
cabal build exe:coscad
MPLCONFIGDIR=/private/tmp/globe-matplotlib /private/tmp/coscad-globe-venv/bin/python models/print-in-place-globe/v2-threaded/build.py
```

The generator locates CoScad through `cabal list-bin exe:coscad`. It emits all
geometry in CoScad without a third-party thread library or external map data.
`build.py` renders, verifies, prepares both multipart projects, slices both jobs
and verifies the packages. Native macOS apps may need host access outside a
sandbox. This build never dispatches a print. Saved presets, logs and checks
remain alongside their respective artifacts.

2026-10-29: `build.py` no longer uses OpenSCAD. It renders each part from
`assembly.coscad` with the native compiler (`coscad stl`; `--coscad PATH`,
`$COSCAD` or `cabal list-bin exe:coscad`), and `--render-only` stops after
`verify.py`, before slicing. The recorded meshes were rendered by OpenSCAD at
`$fn=96`, which `generate.py` writes into the `.scad` files; the native kernel
always uses 50 circle segments (thread helices keep their own segments), so a
rebuild is coarser and not identical. A native rebuild on 2026-10-29 passed
every `verify.py` check. Volumes moved by up to 0.5% (rotor 137391 against
138054 mm³, base 47048 against 47133, coupon-female 7173 against 7193), and the
wrong-phase thread probe intersects 225.85 mm³ (224.83 at 96). The physically
tested thread fit was printed from the 96-segment meshes; print a coupon again
before relying on a native rebuild.

## Thread coupon dispatch, 2026-10-23

The owner confirmed a clear P1S bed and explicitly requested the small fit test.
One LAN upload/dispatch was acknowledged successfully; the printer reported the
matching 87-layer job RUNNING. Preflight had no HMS alerts, but HMS
`0300310000010001` (part-cooling fan slow/stopped) returned during nozzle-cleaning
startup. Print error remained zero. See `coupon-print-run.json` for the latest
observed state and frames. Do not equate accepted/running startup with physical
completion or a proven screw fit. The full globe was not queued.

First-layer confirmation at 18:03:57 Sydney time: RUNNING, layer 1/87, about
54 minutes remaining, nozzle 220 C, bed 55 C, print error zero and no HMS alerts.
The temporary fan alert cleared by 18:00:42; this does not establish a repaired
fan. The bounded startup observer has stopped; the printer continues on its own.
Completion, cooling and hand-tested screw fit remain unverified.

## Full globe dispatch and runout observation, 2026-10-24

The owner reports the thread-fit test worked, approved the full print, and
confirmed the plate is clear. Visible test pieces in the camera are off the
plate, as explicitly confirmed by the owner. The first upload was deliberately
interrupted before any MQTT print command while checking that image; its record
is `full-print-upload-interrupted-20261024.json`. A fresh query confirmed the
prior coupon was still FINISH, so this was not a duplicate print dispatch.

The second upload completed with remote file-size validation. Exactly one print
command was acknowledged for `globe-v2-p1s-20261024-182744.gcode.3mf`; the P1S
reported RUNNING with 659 layers and about 504 minutes remaining. The unchanged
package uses external PLA and enables firmware filament-runout detection via
M412 S1. The owner notes that the spool may run out. The authenticated public
dashboard independently showed the matching RUNNING job with fresh fault data.

The focused FP-RISC telemetry regression suite passed, including runout codes,
pause/fault projection, clearing and omission handling. This is synthetic
evidence, not a real filament-runout result. No fault was injected into either
printer or the production dashboard.

At the owner-selected 30-minute interval, the thread follow-up compares direct
printer faults with the live dashboard. The read-only `check-full-print.py`
records observations in `full-monitor-events.jsonl` and `full-monitor-state.json`.
Automation identity is in `full-monitor-automation.json`. It should notify only
for filament trouble, pause/failure, a missing dashboard fault, telemetry loss,
or completion, then stop when this job ends or is replaced. Dashboard card
alerts are live; these separate periodic Codex checks are not browser push
notifications and can miss a transient event between checks. The real runout
result remains unverified until one actually occurs.

At 18:32 Sydney time, the real P1S reported HMS `0300310000010001` during
startup. The public dashboard independently showed that exact code, categorized
as Fan needs attention, with fresh matching job telemetry and no missing codes.
This demonstrates live health-alert delivery for the fan warning; it does not
prove a filament-runout event has been detected. See `full-monitor-events.jsonl`.

First-layer confirmation: 2026-10-24 18:36:07 Australia/Sydney, RUNNING, layer
1/659, 497 minutes remaining, nozzle 220 C, bed 55 C, print error zero and no
HMS alerts. Camera confirms yellow filament deposition on the cleared plate.
The bounded startup observer ended; the P1S continues independently and the
30-minute dashboard comparison remains enabled. Completion, support removal,
and full-assembly operation remain unverified.

## Actual filament runout, 2026-10-24 20:01 Sydney

The P1S paused at layer 29/659 with print_error 50364420 (`03008004`).
The installed Bambu English catalogs explicitly identify this as filament runout.
The live dashboard correctly displayed PAUSE and the exact code, with fresh
telemetry and no missing alerts, but used the generic label Printer reports an
error rather than a runout label. The monitor classification also returned
runout_seen=false because its category list lacks this exact short code; the
verified catalog interpretation is retained in `verified-runout-20261024.json`.
No printer control commands were issued. User action is required to load PLA
and resume. The follow-up remains enabled while paused.

## Full revision physical outcome, 2026-10-25

The owner reports the full two-piece globe worked quite well: both main parts
printed on one bed; increased structural wall count and bed-only tree supports
worked well; the male stand thread and female globe/gimbal interface fit well;
and the globe-to-gimbal tolerances worked well. Retain this as the successful
physical baseline for future moving fits and threaded interfaces. The structural
parts used six walls, the rotor three walls; the shared thread enlargement was
0.4 mm radial and 0.25 mm along each flank. See physical-outcome-20261025.json.
This is owner-reported success, not a measured strength/lifetime test.

The owner stopped the periodic print follow-up on 2026-10-24. It is deleted and
remains stopped; earlier chronology describing it as enabled is historical.
