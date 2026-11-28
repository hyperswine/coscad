#!/usr/bin/env python3
"""Upload and start the three-colour AMS Lite test on the A1, once.

Slicer filament 1 -> AMS tray 0 (white), 2 -> tray 1 (black), 3 -> tray 2
(yellow), via the project_file command's use_ams + ams_mapping. Refuses to run
if print-run.json exists, if the printer is busy, or if the trays no longer
hold PLA in those colours. Never retries the start.

Run with /opt/homebrew/bin/python3.14 (paho-mqtt) and BAMBU_HOST,
BAMBU_SERIAL, BAMBU_ACCESS_CODE set for the A1.
"""
import hashlib, json, os, ssl, sys, time
from datetime import datetime

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "../../../../3d-models/bambu-lan"))
import ftp  # noqa: E402
import paho.mqtt.client as mqtt  # noqa: E402

PKG = os.path.join(HERE, "slice/ams-tricolor-a1.gcode.3mf")
RECORD = os.path.join(HERE, "print-run.json")
HOST, SERIAL, CODE = (os.environ[k] for k in ("BAMBU_HOST", "BAMBU_SERIAL", "BAMBU_ACCESS_CODE"))
# slicer filament index -> (AMS tray, expected colour RGBA)
MAPPING = [(0, "FFFFFFFF"), (1, "000000FF"), (2, "F4EE2AFF")]

if os.path.exists(RECORD):
    sys.exit("print-run.json exists: this package was already dispatched")

data = open(PKG, "rb").read()
stamp = datetime.now().strftime("%Y%m%d-%H%M%S")
remote = f"ams-tricolor-a1-{stamp}.gcode.3mf"
record = {"job": "ams-tricolor-a1", "printer": "a1", "material": "pla",
          "ams_mapping": [t for t, _ in MAPPING], "package_sha256": hashlib.sha256(data).hexdigest(),
          "remote_name": remote, "bed_clear_owner_confirmed": True,
          "created_at": datetime.now().astimezone().isoformat(timespec="seconds"), "events": []}

def save():
    json.dump(record, open(RECORD, "w"), indent=1)

state, replies = {}, []
def on_message(c, u, msg):
    p = json.loads(msg.payload).get("print", {})
    if p.get("command") == "project_file":
        replies.append(p)
    state.update(p)

c = mqtt.Client(mqtt.CallbackAPIVersion.VERSION2, client_id=f"ams-test-{os.getpid()}")
c.username_pw_set("bblp", CODE)
ctx = ssl.create_default_context(); ctx.check_hostname = False; ctx.verify_mode = ssl.CERT_NONE
c.tls_set_context(ctx)
c.on_message = on_message
c.connect(HOST, 8883)
c.subscribe(f"device/{SERIAL}/report")
c.loop_start()
c.publish(f"device/{SERIAL}/request", json.dumps({"pushing": {"sequence_id": "1", "command": "pushall"}}))
time.sleep(5)

trays = {int(t["id"]): t for u in (state.get("ams") or {}).get("ams") or [] for t in u.get("tray") or []}
record["before"] = {"gcode_state": state.get("gcode_state"), "subtask_name": state.get("subtask_name"),
                    "trays": {i: [t.get("tray_type"), t.get("tray_color")] for i, t in trays.items()}}
if state.get("gcode_state") not in ("FINISH", "IDLE", "FAILED"):
    save(); sys.exit(f"printer busy: {state.get('gcode_state')}")
for tray, colour in MAPPING:
    t = trays.get(tray) or {}
    if t.get("tray_type") != "PLA" or (t.get("tray_color") or "").upper() != colour:
        save(); sys.exit(f"AMS tray {tray} holds {t.get('tray_type')} {t.get('tray_color')}, expected PLA {colour}")

f = ftp.connect()
f.storbinary(f"STOR {remote}", open(PKG, "rb"))
size = f.size(remote)
if size != len(data):
    save(); sys.exit(f"remote size {size} != {len(data)}")
record["phase"] = "uploaded"; save()

seq = str(int(time.time()))
cmd = {"print": {"sequence_id": seq, "command": "project_file", "param": "Metadata/plate_1.gcode",
                 "url": f"file:///sdcard/{remote}", "subtask_name": remote.removesuffix(".3mf"),
                 "project_id": "0", "profile_id": "0", "task_id": "0", "subtask_id": "0",
                 "md5": hashlib.md5(data).hexdigest(), "timelapse": False, "bed_type": "auto",
                 "bed_levelling": True, "flow_cali": False, "vibration_cali": True, "layer_inspect": True,
                 "use_ams": True, "ams_mapping": [t for t, _ in MAPPING]}}
record["sequence_id"] = seq; record["dispatch_attempted"] = True; save()
c.publish(f"device/{SERIAL}/request", json.dumps(cmd))

deadline = time.time() + 30
while not replies and time.time() < deadline:
    time.sleep(0.5)
record["reply"] = replies[0] if replies else None
record["phase"] = "acknowledged" if replies else "no-acknowledgement"; save()

# Short read-only startup observation.
for _ in range(8):
    time.sleep(15)
    c.publish(f"device/{SERIAL}/request", json.dumps({"pushing": {"sequence_id": "2", "command": "pushall"}}))
    time.sleep(2)
    snap = {k: state.get(k) for k in ("gcode_state", "mc_percent", "layer_num", "total_layer_num", "mc_remaining_time",
                                      "nozzle_temper", "bed_temper", "print_error", "hms")}
    snap["tray_now"] = (state.get("ams") or {}).get("tray_now")
    record["events"].append({"at": datetime.now().astimezone().isoformat(timespec="seconds"), "status": snap})
    save(); print(json.dumps(snap))
c.loop_stop()
print("reply:", record["reply"])
