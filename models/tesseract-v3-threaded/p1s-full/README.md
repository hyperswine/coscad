# Full tesseract on one P1S plate — 28 October 2026

The owner reported that the P1S screw/socket fit worked and authorized the full
print if it fits one plate. Both printer beds were confirmed clear. The packed
project contains all **37 normal parts**: 8 hubs, 8 caps, 8 diagonal struts,
12 bars and the inner frame. No purchased fasteners are needed.

`pack.py` uses actual projected mesh outlines. Broad hubs and caps have 1.5 mm
brims, struts 5 mm and tall bars 6 mm; the inner frame reserves a 6 mm support
region beyond its projection and uses a 5 mm brim. The bed exclusion and margins
are retained. The minimum gap between the reserved brim/support envelopes is
1.279 mm. This is a packed one-plate project, not the conservative earlier
bounding-box layout.

Bar tips have 0.8 mm flats for upright printing. The head centres and retained
spherical surfaces remain in place; the printable bar is a subset of the checked
nominal bar. `manufacturing-geometry.json` records this check. All thread shapes
and their physically successful clearances are unchanged.

`prepare.py` combines only CoScad-compiled meshes. Per-object settings enable
bed-only tree supports on the inner frame and disable supports on the other
objects. Thread-bore exclusion meshes prevent support plugs in the female
threads. These are slicer modifiers, not extra printed parts. Angled thread
surfaces and the small blind-bore roofs still rely on the PLA bridging behaviour;
the successful vertical coupon alone does not physically validate those details.

The slicer produced one plate with 37 objects and no warnings, estimated at
**19 h 49 min and 309.4 g Generic PLA**, on a P1S 0.4 mm nozzle and textured PEI.
Settings use 0.2 mm layers, six walls, 20% gyroid and conservative speeds.
`verify_full.py` checks the exported geometry/placement, actual toolpaths and
support regions before the separate dispatch helper may start a print.

The authoritative package is
`slice-onebed/tesseract-full-p1s.gcode.3mf`. The earlier `slice/` attempt was
rearranged by the slicer and was never dispatched. The final slice explicitly
uses `--arrange 0 --orient 0` and absolute output paths. Do not use an earlier
G-code file or infer one-plate validity from an STL bounding box alone.

`print_full.py` requires `--start --bed-clear`, refuses an existing
`full-print-run.json`, verifies the package hash and fresh idle/fault/nozzle/spool
state, then uploads and verifies the remote bytes before sending one start.
Read status without flags. Startup is not physical assembly or strength proof.

After cooling, peel brims and clip the inner-frame trees in short sections.
Clear the thread entries before trying to screw anything together. Start each
joint by hand, seat the strut shoulders, align the empty hubs, install every bar,
then close the individual caps. Use the main model README for assembly order.

The verified upload was acknowledged by the P1S on 28 October 2026. Fresh
telemetry reported the expected 496-layer job RUNNING during heating, with
print error zero and no HMS faults. `dispatch-summary.json` records that
observation; `full-print-run.json` is the live local startup record.

## Owner-reported early failure

On 28 October the owner reported substantial loss of adhesion and spaghetti
in several places relatively early in the P1S print. The supplied photograph is
[failure-photo-20261028.jpg](failure-photo-20261028.jpg); structured observations
are in `physical-failure-20261028.json`. The first failed part/layer and root
cause are unknown. Bed contamination, Z-belt friction/tension and lead-screw
friction are hypotheses only. The lead screws were reportedly cleaned and
lubricated recently. No mechanical diagnosis is established by the photo.

The follow-up is an A1 retry on physical AMS slot 1. Changing printer and
filament together can show whether the problem recurs, but cannot isolate
a single cause. The successful thread-fit coupon does not override this
failed full-print outcome.
