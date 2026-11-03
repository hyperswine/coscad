# Tesseract III per-object batch - 3 November 2026

Owner requested four diagonal struts on each printer and explicitly confirmed
all three build plates clear. After reviewing the A1 gantry clearance, owner
changed A1 to long bars: three bars per object, using white PLA physical AMS
slot 1. Final request is **X1C four struts; P1S four struts; A1 three bars**.

All geometry is translated only from the original printed Tesseract III parts.
X1C/P1S use the demonstrated staggered four-strut arrangement. A1 uses the
previously checked three-bar arrangement. All use native printer-specific
presets with manufacturer head/rod/lid limits unchanged, 8 mm brims, six walls,
20% gyroid infill, 0.2 mm layers, no supports, 55 C Textured PEI, 220 C nozzle,
12-second layer cooling target and 5 mm/s minimum speed. The first native end
lift is 5 mm; full actual return-motion audits remain required.

Slices estimate X1C 4h10 / 14.77g, P1S 4h08 / 14.54g and A1 4h21 / 7.52g.
No physical fit or successful completion is claimed by a slice or first layer.

`prepare.py x1c p1s a1` prepares without dispatch and refuses run records.
It needs numpy, trimesh, shapely and the existing original meshes. Exact slicer
commands are saved per printer in `slice/slice-command.json`. Shared geometry,
thermal and actual sequential-motion verifiers produce per-printer reports.
`../print_parts_20261103.py` is read-only without `--start`; an authorized start
requires `--bed-clear`, frozen verified upload bytes, complete FTPS read-back
and one durable intent before one start. It refuses optimized Python.
Each printer has its own exclusive `print-run.json`; never delete one to retry.

Initial read-only readiness: all three FINISH, no active print error/HMS,
SD available, 0.4 mm nozzle; X1C/P1S active external PLA and A1 white PLA in
physical slot 1. Exact live readiness is checked again before upload/start.
Existing printer credentials stay in the existing local credential bridge;
no private values are included here. No recurring monitor is created.

The initial four-strut A1 request was not dispatched: its 56.5 mm rod-distance
limit plus 5 mm margin needs about 236.4 mm of four-part Y span before brims,
exceeding the existing 216 mm region with 20 mm edge margins. The requested
three-bar substitution retains those safety margins. Nothing is queued for a
fourth A1 strut after this change.

## First-layer startup observations

- X1C: 2026-11-03T17:06:40+11:00, RUNNING layer 1/1164, 241 minutes remaining; measured nozzle 220.0 C / bed 55.0 C; no active print error/HMS.
- P1S: 2026-11-03T17:05:19+11:00, RUNNING layer 1/1164, 241 minutes remaining; measured nozzle 220.3125 C / bed 54.96875 C; no active print error/HMS.
- A1: 2026-11-03T17:06:11+11:00, RUNNING layer 1/1488, 253 minutes remaining; measured nozzle 220.65625 C / bed 55.15625 C; no active print error/HMS.

A1 physical AMS slot 1 was verified. The P1S briefly reported its recurring
fan-speed warning during warmup; fresh read-only follow-up showed it clear and
then reached first layer. The original warning record is preserved. No alert
clearing, restart, or repeated start was sent. Each uploaded file matched the
frozen validated file on FTPS read-back, and one start was accepted per printer.
These are startup observations, not completed physical-quality acceptance.
All startup observers have ended; no recurring monitoring was created.
