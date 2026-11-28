# Operating the X2D with AMS 2 Pro

## Connect and check status

On the X2D touchscreen, join the printer LAN and enable **Settings → LAN Only → Developer Mode (only for 3D printing)**. The workstation and the dashboard Mac mini can reach the printer over this LAN. LAN Only disables Bambu cloud features, including Handy access. [X2D user manual, §7.3](https://csm.bblcdn.com/hub/7c58718aaa2e40edab56efb87419a96a.pdf)

| Location | Configuration |
| --- | --- |
| Slicing workstation | `/Users/jasonqin/Documents/GitHub/coscad/.env` contains `IP_ADDR` and `ACCESS_CODE`. The file is ignored by Git. |
| Production dashboard Mac mini | `~/printer-host/data/printers.json` defines printer ID `x2d`, the MQTT host and TLS identity. Its `access_code_env` refers to `BAMBU_X2D_ACCESS_CODE` in `~/printer-host/data/printer-credentials.env`, owned by the service user with mode 0600. |
| Operator view | Sign in to [3dprint.cswine.cloud](https://3dprint.cswine.cloud) and open **Bambu X2D** for live job status and reported filament labels. Camera streaming is disabled for this printer. |

If the LAN access code changes, update both private credential files and restart the dashboard service so it reloads its binding. Keep the value out of commands, screenshots, logs and commits.

## Prepare the plate and map the filament

1. Insert a writable USB drive. The verified setup uses one exFAT data partition. A successful MQTT connection or a USB-present indicator alone does not prove the printer can store the full job.
2. In Bambu Studio select **Bambu Lab X2D 0.4 nozzle** and **Textured PEI Plate** for the installed plate. The original boat slice selected Cool Plate and triggered a mismatch. The corrected PLA job used a 55 °C bed.
3. Check **AMS Setup** on the touchscreen. For this setup the AMS feeds the main nozzle through the upper buffer inlet. The external feed has a separate path. If the printer asks to unload external filament, follow the touchscreen prompt before resuming.
4. Assign the material for every model part and support interface. The verified sailboat used slicer filament 1 for black SUNLU PLA+ 2.0 (hull and supports) and filament 2 for green Bambu PLA Basic (letters). Both used the main nozzle. In the installed AMS these were physical slots **4** and **3**, respectively. Verify the spools again after a reload.
5. Slice, then inspect the plate preview, nozzle assignments and `Metadata/slice_info.config`. For black bed-only tree supports the toolpath should report `support_type = tree(auto)`, `support_on_build_plate_only = 1` and both support filament selectors set to 1. [Prepared two-color project](../models/twin-boat-trials/agvn/sail/AGVN-sail.3mf) · [Tree-supported slice](../models/twin-boat-trials/agvn/sail/tree/AGVN-Sail-Tree.gcode.3mf)

## Send from the LAN pipeline

- Authenticate to MQTT over TLS on port 8883 as `bblp` using the access code. Subscribe to `device/<serial>/report` and request a full status with `pushing.pushall` on `device/<serial>/request`. Check job state, active errors and AMS occupancy, type and color. Confirm the physical bed is clear.
- Upload the sliced package to the printer’s root over implicit FTPS on port 990. Verify its size and SHA-256 by reading it back. A partial upload can still appear in the drive listing.
- Publish one `print.project_file` request using `ftp:///FILENAME.gcode.3mf` and `Metadata/plate_1.gcode`. The SD-card-style URL used on older printers was rejected by this X2D. For the two-color boat the verified payload used `bed_type="textured_plate"`, `use_ams=true`, `ams_mapping=[3,2]` and the corresponding AMS unit 0 / slot IDs 3 and 2 in `ams_mapping2`. Physical tray numbers are one-based. API slot IDs are zero-based. [Recorded dispatch payload](../models/twin-boat-trials/agvn/sail/tree/dispatch-textured.json)
- Match the reply and live subtask name to the request. If an acknowledgement is missing, query status before another dispatch. A job can report RUNNING during preparation and later pause at layer zero. Check the error and touchscreen prompt before treating startup as a successful first layer.

The original single-color boat physically printed well according to the operator. The two-color tree-supported job was verified through upload, acknowledgement and startup. [Operator outcome](../models/twin-boat-trials/agvn/print-outcome.md) · [Material routing and integration notes](2026-11-25-X2D-AMS2-INTEGRATION.md)
