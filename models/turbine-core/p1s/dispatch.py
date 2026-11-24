"""Send the sliced turbine core to the P1S and start it (PLA, external spool).

Needs BAMBU_ACCESS_CODE in the environment (never written to disk or printed).
Checks the printer is idle and fault-free first; a durable record stops a second send.
Usage: BAMBU_ACCESS_CODE=... python dispatch.py [--dry-run]
"""
import hashlib, json, os, ssl, sys, time, threading
from pathlib import Path
import paho.mqtt.client as mqtt

sys.path.insert(0, '/Users/jasonqin/Documents/GitHub/3d-models/bambu-lan')
HOST, SERIAL = '192.168.0.27', '01P00A442400974'
CODE = os.environ['BAMBU_ACCESS_CODE']
HERE = Path(__file__).resolve().parent
PKG = HERE.parent / 'v3' / 'slice' / 'turbine-v3.gcode.3mf'
RECORD = HERE / 'print-run.json'
CA = '/private/tmp/boat-printer-ca.pem'
dry = '--dry-run' in sys.argv

state = {}
def merge(d, s):
    for k, v in s.items():
        if isinstance(v, dict) and isinstance(d.get(k), dict): merge(d[k], v)
        else: d[k] = v

c = mqtt.Client(mqtt.CallbackAPIVersion.VERSION2, client_id=f'turbine-{os.getpid()}')
c.username_pw_set('bblp', CODE)
ctx = ssl.create_default_context(); ctx.check_hostname = False; ctx.verify_mode = ssl.CERT_NONE
c.tls_set_context(ctx)
got = threading.Event()
def on_connect(cl, ud, fl, rc, pr):
    if rc != 0: sys.exit(f'MQTT connect failed: {rc} (wrong access code?)')
    cl.subscribe(f'device/{SERIAL}/report')
    cl.publish(f'device/{SERIAL}/request', json.dumps({'pushing': {'sequence_id': '1', 'command': 'pushall', 'version': 1, 'push_target': 1}}))
def on_message(cl, ud, m):
    d = json.loads(m.payload); merge(state, d)
    if d.get('print', {}).get('command') == 'push_status' and 'gcode_state' in d['print']: got.set()
c.on_connect, c.on_message = on_connect, on_message
c.connect(HOST, 8883, keepalive=60); c.loop_start()
if not got.wait(15): sys.exit('No status from printer')
p = state['print']

def ready():
    assert p.get('gcode_state') in ('FINISH', 'IDLE'), f"Printer busy: {p.get('gcode_state')}"
    assert p.get('print_error') == 0 and not p.get('hms'), 'Active printer fault'
    assert str(p.get('nozzle_diameter')) == '0.4', 'Wrong nozzle'
    ext = p.get('vt_tray') or {}
    print('external spool:', ext.get('tray_type'), ext.get('tray_color'))
    assert ext.get('tray_type') == 'PLA', 'External PLA is not loaded'
print('state:', p.get('gcode_state'), '| nozzle', p.get('nozzle_diameter'), '| bed', p.get('bed_temper'), 'C')
ready()
digest = hashlib.sha256(PKG.read_bytes()).hexdigest()
print('package', PKG.name, PKG.stat().st_size, 'bytes sha256', digest[:16])
if dry: print('dry run OK, nothing sent'); sys.exit(0)

assert not RECORD.exists(), 'Dispatch record exists; check printer status instead of resending'
remote = f'turbine-core-v3-{time.strftime("%Y%m%d-%H%M%S")}.gcode.3mf'
RECORD.write_text(json.dumps({'printer': 'P1S', 'material': 'PLA', 'spool': 'external', 'sha256': digest,
    'remote': remote, 'owner_confirmed_bed_clear': True, 'phase': 'preflight',
    'at': time.strftime('%Y-%m-%dT%H:%M:%S%z')}, indent=2) + '\n')

os.environ['BAMBU_HOST'] = HOST
import ftp as lanftp   # ImplicitFTPS helper (module reads BAMBU_HOST and the code at import)
tls = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT); tls.load_verify_locations(CA)
tls.check_hostname = False   # the cert names the serial in CN only (no SAN); chain is verified, CN checked below
_wrap = tls.wrap_socket
class _Ctx(ssl.SSLContext): pass
def _checked(*a, **k):
    ss = _wrap(*a, **k)
    cn = dict(x[0] for x in ss.getpeercert()['subject'])['commonName']
    assert cn == SERIAL, f'Printer cert is for {cn}, expected {SERIAL}'
    return ss
tls.wrap_socket = _checked
f = lanftp.ImplicitFTPS(context=tls, timeout=30)
try:
    f.connect(HOST, 990); f.login('bblp', CODE); f.prot_p()
    with PKG.open('rb') as fh: f.storbinary('STOR ' + remote, fh)
    assert f.size(remote) == PKG.stat().st_size, 'Uploaded size mismatch'
finally:
    f.close()
print('uploaded', remote)
ready()
payload = {'print': {'sequence_id': str(int(time.time() * 1000) % 1000000000), 'command': 'project_file',
    'param': 'Metadata/plate_1.gcode', 'project_id': '0', 'profile_id': '0', 'task_id': '0', 'subtask_id': '0',
    'subtask_name': remote, 'file': '', 'url': 'file:///sdcard/' + remote, 'md5': '', 'timelapse': False,
    'bed_type': 'auto', 'bed_levelling': True, 'flow_cali': True, 'vibration_cali': True,
    'layer_inspect': True, 'use_ams': False, 'ams_mapping': [254]}}
r = json.loads(RECORD.read_text()); r['phase'] = 'start-sent'; RECORD.write_text(json.dumps(r, indent=2) + '\n')
c.publish(f'device/{SERIAL}/request', json.dumps(payload))
for _ in range(60):
    time.sleep(1)
    s = state['print'].get('gcode_state')
    if s in ('PREPARE', 'RUNNING'):
        print('started:', s, state['print'].get('gcode_file')); break
else:
    print('No start seen in 60 s; check the printer')
c.loop_stop()
