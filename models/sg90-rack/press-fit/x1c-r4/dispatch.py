#!/usr/bin/env python3
"""Upload and start the SG90 press-fit revision 4 package (base + tall pinion) on the X1C, once.

Needs paho-mqtt and the sibling 3d-models/bambu-lan checkout, with
BAMBU_HOST, BAMBU_SERIAL and BAMBU_ACCESS_CODE set for the X1C.
Refuses to run if print-run.json exists, and never retries the start.
"""
import hashlib, json, os, ssl, sys, time
from datetime import datetime

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "../../../../../3d-models/bambu-lan"))
import ftp  # noqa: E402
import paho.mqtt.client as mqtt  # noqa: E402

PKG = os.path.join(HERE, "slice/sg90-press-fit-r4.gcode.3mf")
RECORD = os.path.join(HERE, "print-run.json")
HOST, SERIAL, CODE = (os.environ[k] for k in ("BAMBU_HOST", "BAMBU_SERIAL", "BAMBU_ACCESS_CODE"))

if os.path.exists(RECORD):
    sys.exit("print-run.json exists: this package was already dispatched")

data = open(PKG, "rb").read()
sha = hashlib.sha256(data).hexdigest()
stamp = datetime.now().strftime("%Y%m%d-%H%M%S")
remote = f"sg90-press-fit-r4-x1c-{stamp}.gcode.3mf"
record = {"job": "x1c-r4", "printer": "x1c", "material": "pla", "spool": "external",
          "package_sha256": sha, "remote_name": remote, "bed_clear_owner_confirmed": True,
          "created_at": datetime.now().astimezone().isoformat(timespec="seconds"), "events": []}

def save():
    json.dump(record, open(RECORD, "w"), indent=1)

state, replies = {}, []
def on_message(c, u, msg):
    p = json.loads(msg.payload).get("print", {})
    if p.get("command") == "project_file":
        replies.append(p)
    state.update({k: v for k, v in p.items() if not isinstance(v, (dict, list)) or k == "hms"})

c = mqtt.Client(mqtt.CallbackAPIVersion.VERSION2, client_id=f"sg90-{os.getpid()}")
c.username_pw_set("bblp", CODE)
ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE  # printer uses a self-signed cert
c.tls_set_context(ctx)
c.on_message = on_message
c.connect(HOST, 8883)
c.subscribe(f"device/{SERIAL}/report")
c.loop_start()
c.publish(f"device/{SERIAL}/request", json.dumps({"pushing": {"sequence_id": "1", "command": "pushall"}}))
time.sleep(4)
record["before"] = {k: state.get(k) for k in ("gcode_state", "subtask_name", "print_error", "hms")}
if state.get("gcode_state") not in ("FINISH", "IDLE", "FAILED"):
    save(); sys.exit(f"printer busy: {state.get('gcode_state')}")

f = ftp.connect()
# The X1C needs a clean TLS shutdown on the data connection (the A1 does not
# answer close_notify, which is why ftp.py skips it). Try unwrap with a timeout.
f.voidcmd("TYPE I")
conn = f.transfercmd(f"STOR {remote}")
conn.sendall(data)
try:
    conn.settimeout(10)
    conn = conn.unwrap()
except Exception as e:
    record["unwrap_note"] = repr(e)
conn.close()
f.voidresp()
size = f.size(remote)
if size != len(data):
    save(); sys.exit(f"remote size {size} != {len(data)}")
record["uploaded"] = True
record["phase"] = "uploaded"
save()

seq = str(int(time.time()))
record["sequence_id"] = seq
cmd = {"print": {"sequence_id": seq, "command": "project_file", "param": "Metadata/plate_1.gcode",
                 "url": f"file:///sdcard/{remote}", "subtask_name": remote.removesuffix(".3mf"),
                 "project_id": "0", "profile_id": "0", "task_id": "0", "subtask_id": "0",
                 "md5": hashlib.md5(data).hexdigest(), "timelapse": False, "bed_type": "auto",
                 "bed_levelling": True, "flow_cali": True, "vibration_cali": True,
                 "layer_inspect": True, "use_ams": False, "ams_mapping": []}}
record["dispatch_attempted"] = True
save()
c.publish(f"device/{SERIAL}/request", json.dumps(cmd))

deadline = time.time() + 30
while not replies and time.time() < deadline:
    time.sleep(0.5)
record["reply"] = replies[0] if replies else None
record["phase"] = "acknowledged" if replies else "no-acknowledgement"
save()

# Short startup observation, read-only.
for _ in range(8):
    time.sleep(15)
    c.publish(f"device/{SERIAL}/request", json.dumps({"pushing": {"sequence_id": "2", "command": "pushall"}}))
    time.sleep(2)
    snap = {k: state.get(k) for k in ("gcode_state", "mc_percent", "layer_num", "total_layer_num",
            "mc_remaining_time", "subtask_name", "nozzle_temper", "bed_temper", "print_error", "hms")}
    record["events"].append({"at": datetime.now().astimezone().isoformat(timespec="seconds"), "status": snap})
    save()
    print(json.dumps(snap))
c.loop_stop()
print("reply:", record["reply"])
