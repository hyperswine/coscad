# X1C full-bed calibration - 29 October 2026

**Result: passed.** The built-in full-bed levelling routine ran from
14:31:22 to 14:35:47 Australia/Sydney and ended `FINISH`, 100%, error 0,
`fail_reason=0`, HMS empty. It did not reproduce the print's `0300-4002`
failure, so there is no failing probe location from this run.

The owner explicitly requested this calibration. The printer was inactive
with the expected failed tesseract job and no current error/HMS before starting.
One native `print.command=calibration` request with `option=2` was acknowledged
successfully. Bambu Studio maps bit 1 to bed levelling; vibration, camera,
motor-noise and other calibrations were not selected. The printer ran
`/usr/etc/print/auto_cali_for_user_param.gcode` and reported `stg=[1]`.
[Command definition](https://github.com/bambulab/BambuStudio/blob/master/src/slic3r/GUI/DeviceManager.cpp#L1757-L1777).

## Important comparison limit

The built-in routine used an unheated bed, around 26-27 C, and a nozzle around
38 C. The failed print used a 55 C bed, with the vendor startup nozzle-cleaning
and homing sequence. Passing this room-temperature routine does not show that
levelling will pass at print temperature, nor does it establish a mechanical
cause or prove the earlier problem is fixed. A controlled warm-bed comparison
is the next diagnostic; it was not performed automatically.

The result shows that the printer can complete its full-bed routine under
these conditions, independent of the tesseract slice. The exact probe grid and
per-point height readings were not exposed, so no numbered probe points are
claimed. Telemetry contained no X/Y/Z probe coordinates. The camera paths did
not return frames; the owner offered to watch the routine directly. The owner subsequently reported that nothing appeared wrong during calibration.

## Records

- `run_calibration.py`: one-shot runner, refusing an existing `run.json` so an
  uncertain operation is never resent automatically.
- `run.json`: owner authorization, before state, native command, matched success
  acknowledgement, progress observations and terminal result.
- `telemetry.jsonl`: selected raw status deltas, active/retained error fields,
  HMS messages and reported field names. Credential-bearing camera fields are
  not recorded.
- `status-latest.json`, `summary.json`: terminal snapshot and compact findings.
- `camera-status.json`: camera unavailable, zero saved frames.

The bounded observers have ended. No print, manual move, error-clear, service
restart or repeated calibration was sent. Existing print/cancellation records
remain separate. No private key, access code or camera authentication header
is recorded in these files.
