# X1C four staggered struts - 31 October 2026

Four struts print in order, one complete part at a time. Native X1C limits
are unchanged; the verified 5 mm end lift is retained. Estimate 4 h 10 min.

## Dispatch and first-layer acceptance

The owner explicitly confirmed both build plates clear on 31 October. The
checked X1C upload was read back with a matching SHA-256 and one start was
acknowledged successfully. At 12:55:10 Sydney time the unique job reported
RUNNING at layer 1/1164, with 242 minutes remaining, measured bed 55 C and
nozzle 220 C, targets 55/220 C, no print error and no HMS faults.

Firmware counts all four sequential objects globally: 4 x 291 = 1164 layers.
See dispatch-summary.json and print-run.json for exact identity and evidence.
Completed quality and physical clearance remain unverified. No repeated start
or alert clearing was sent.

## Owner-confirmed physical success - 31 October evening

The owner reports **each of the four struts printed perfectly in per-object
mode**. This supersedes the startup-only physical-quality uncertainty above.
The correct job reports FINISH/1164, 100%, with no active print error or HMS.
[Outcome record and three saved camera images](outcome-20261031-214805/outcome.json)
retain both observations. The X1C JPEGs are visibly corrupted across the lower
image/bed, so they do not independently verify strut or thread quality. Physical
acceptance comes from the owner's inspection. Direct RTSPS and longer/filtered
read-only decoder captures produced no usable replacement. No service changes,
printer movement, fault clearing or new print command was sent.
