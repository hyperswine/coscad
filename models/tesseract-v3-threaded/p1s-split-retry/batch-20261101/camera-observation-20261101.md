# Read-only camera check and owner-approved P1S pause

Fresh captures on 2026-11-01 around 16:27 Australia/Sydney:

| Printer | Telemetry at capture | Visual observation |
| --- | --- | --- |
| A1 | RUNNING, 78%, layer 1166/1488, about 54 min left; no active print error/HMS | Three fresh frames; the visible bars remain upright, with no obvious spaghetti or displaced pieces. Some detail is obscured by the active toolhead. Not a completed physical acceptance. |
| P1S | RUNNING, 75%, layer 983/1369, about 65 min left; no active print error/HMS | Three fresh frames show substantial spaghetti under the toolhead and at least one part lying on its side. Another threaded component remains standing. This by-object batch has visibly failed; telemetry did not detect it. |
| X1C | RUNNING, 95%, layer 256/282, about 8 min left; no active print error/HMS | Three direct RTSPS captures failed; authenticated dashboard fallback and three Mini settled-stream attempts also failed. No usable fresh photo, so visual quality is unverified. |

Photos and sanitized status records:

- `camera-check-20261101-162705/a1/`
- `camera-check-20261101-162706/p1s/`
- `camera-check-20261101-162705/x1c/`

The first and third frames of both working cameras were inspected. They show the
same overall state. The second frames are preserved as additional evidence.
No initiating cause or exact first-failure layer can be inferred from the P1S
end-state photos. In particular this does not prove a nozzle collision, adhesion
problem, or thread defect. The observations should not be generalized to all
per-object layouts or printers.

After seeing the failure, the owner explicitly requested **“Pause the P1S now.”**
One pause was sent to the exact expected job at 16:30:48, with the printer at
77% / layer 1004. The resulting PAUSE state was verified. No cancellation,
restart, resume, alert clearing, or new print dispatch was sent. See
`three-struts-one-bar/pause-request-20261101.json`.
