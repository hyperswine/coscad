"""One owner-confirmed dispatch of the two-nozzle AGVN sailboat package to the X2D, following
docs/X2D-OPERATIONS.md: TLS MQTT status check, implicit-FTPS upload with read-back, one
print.project_file, then status up to the first layer. Black (filament 1) comes from AMS
unit 0 slot 3 (physical slot 4) on physical extruder 1; green (filament 2) from the external
feed, virtual slot 255, on physical extruder 0 -- as the printer reported them before slicing.

  python dispatch_x2d.py --confirm-bed-clear --authorization-note '...' [--job tree-two-nozzles-notower-cool10]

--job names the build.py output folder (default tree-two-nozzles, the first print); the record
goes to <job>/print-run.json and the remote file is named after the job.

Reads IP_ADDR / ACCESS_CODE from coscad/.env and never prints or records the code. An existing
dispatch record blocks a second send; a missing acknowledgement is recorded, never resent.
"""
from pathlib import Path
import argparse, hashlib, json, os, socket, ssl, sys, time
import paho.mqtt.client as mqtt
H = Path(__file__).resolve().parent
ROOT = Path('/Users/jasonqin/Documents/GitHub')
SERIAL = '20P8BJ692700673'
CA = os.environ.get('BBL_CA', str(Path(__file__).resolve().parent/'bbl-printer-ca.pem'))
EXPECT = {'ams': ('0', '3', 'PLA', 'GFSNL04'), 'virtual': ('255', 'PLA', 'GFA00')}

ap = argparse.ArgumentParser(); ap.add_argument('--confirm-bed-clear', action='store_true', required=True)
ap.add_argument('--authorization-note', required=True); ap.add_argument('--ca', default=CA)
ap.add_argument('--job', default='tree-two-nozzles'); args = ap.parse_args()
PACKAGE = H/args.job/'slice/AGVN-sail-light.gcode.3mf'
REMOTE = 'AGVN-sail-light-2n.gcode.3mf' if args.job == 'tree-two-nozzles' else f'AGVN-{args.job}.gcode.3mf'
RECORD = H/args.job/'print-run.json'
assert not RECORD.exists(), 'A dispatch record exists: inspect it and the printer instead of resending'
env = {}
for line in (ROOT/'coscad/.env').read_text().splitlines():
    if '=' in line and not line.lstrip().startswith('#'):
        k, v = line.split('=', 1); env[k.strip()] = v.strip().strip('"').strip("'")
HOST, CODE = env['IP_ADDR'], env['ACCESS_CODE']
data = PACKAGE.read_bytes(); sha = hashlib.sha256(data).hexdigest(); md5 = hashlib.md5(data).hexdigest()
def tls_ctx():
    ctx = ssl.create_default_context(cafile=args.ca); ctx.check_hostname = False; return ctx
def write(rec): tmp = RECORD.with_suffix('.tmp'); tmp.write_text(json.dumps(rec, indent=2, default=str) + '\n'); tmp.replace(RECORD)

state, replies = {}, []
def on_connect(c, u, f, rc, props=None):
    c.subscribe(f'device/{SERIAL}/report')
    c.publish(f'device/{SERIAL}/request', json.dumps({'pushing': {'sequence_id': '1', 'command': 'pushall'}}))
def on_message(c, u, msg):
    try: d = json.loads(msg.payload)
    except Exception: return
    p = d.get('print', {})
    if p.get('command') == 'project_file': replies.append(p)
    state.update(p)
c = mqtt.Client(mqtt.CallbackAPIVersion.VERSION2); c.tls_set_context(tls_ctx()); c.username_pw_set('bblp', CODE)
c.on_connect = on_connect; c.on_message = on_message; c.connect(HOST, 8883, 30); c.loop_start()
t0 = time.time()
while time.time() - t0 < 20 and 'ams' not in state: time.sleep(0.5)
cn = dict(x[0] for x in c._sock.getpeercert()['subject']).get('commonName'); assert cn == SERIAL, ('printer identity', cn)

def summary(): return {k: state.get(k) for k in ('gcode_state', 'mc_percent', 'layer_num', 'total_layer_num', 'mc_remaining_time',
                                                    'subtask_name', 'print_error', 'fail_reason', 'stg_cur')} | {'hms': state.get('hms')}
trays = {(u.get('id'), t.get('id')): t for u in state['ams'].get('ams', []) for t in u.get('tray', [])}
vslots = {s.get('id'): s for s in state.get('vir_slot', [])}
u_, s_, typ, idx = EXPECT['ams']; t = trays.get((u_, s_), {})
assert t.get('tray_type') == typ and t.get('tray_info_idx') == idx and str(t.get('tray_color', '')).upper().startswith('000000'), ('black tray', t)
vid, vtyp, vidx = EXPECT['virtual']; v = vslots.get(vid, {})
assert v.get('tray_type') == vtyp and v.get('tray_info_idx') == vidx, ('green external slot', v)
assert state.get('gcode_state') in ('FINISH', 'IDLE', 'FAILED'), ('printer busy', state.get('gcode_state'))
assert not state.get('print_error') and not state.get('hms'), ('active fault', state.get('print_error'), state.get('hms'))
assert state.get('sdcard') in (True, 1), 'no storage reported'
rec = {'printer': 'x2d', 'serial': SERIAL, 'package': str(PACKAGE.relative_to(ROOT)), 'package_sha256': sha, 'package_md5': md5,
       'remote_name': REMOTE, 'bed_clear_owner_confirmed': True, 'authorization_note': args.authorization_note,
       'accepted_failed_state': state.get('gcode_state') == 'FAILED', 'before': summary(),
       'spools_before': {'ams0-slot3': {k: t.get(k) for k in ('tray_type', 'tray_color', 'tray_info_idx')},
                         'virtual-255': {k: v.get(k) for k in ('tray_type', 'tray_color', 'tray_info_idx')}},
       'phase': 'preflight', 'dispatch_attempted': False, 'events': [], 'created_at': time.strftime('%Y-%m-%dT%H:%M:%S%z')}
with RECORD.open('x') as f: f.write(json.dumps(rec, indent=2, default=str) + '\n')

os.environ.update(BAMBU_HOST=HOST, BAMBU_SERIAL=SERIAL, BAMBU_ACCESS_CODE=CODE)   # the helper reads these at import; this process only
sys.path.insert(0, str(ROOT/'3d-models/bambu-lan')); import ftp as lanftp
class FTPS(lanftp.ImplicitFTPS):
    def storbinary(self, cmd, fp, blocksize=32768):
        self.voidcmd('TYPE I')
        with self.transfercmd(cmd) as conn:
            while buf := fp.read(blocksize): conn.sendall(buf)
            try: conn.settimeout(5); conn.unwrap()
            except Exception: pass
        return self.voidresp()
ftp = FTPS(context=tls_ctx(), timeout=60)
try:
    ftp.connect(HOST, 990); ftp.login('bblp', CODE); ftp.prot_p()
    with PACKAGE.open('rb') as fp: ftp.storbinary('STOR ' + REMOTE, fp)
    assert ftp.size(REMOTE) == len(data), 'remote size mismatch'
    ftp.voidcmd('TYPE I'); h = hashlib.sha256(); n = 0
    with ftp.transfercmd('RETR ' + REMOTE) as s:
        while chunk := s.recv(32768): h.update(chunk); n += len(chunk)
    ftp.voidresp()
    assert h.hexdigest() == sha and n == len(data), 'remote content mismatch'
finally:
    try: ftp.close()
    except Exception: pass
rec.update(phase='uploaded-verified', remote_content_sha256=sha); write(rec)

assert state.get('gcode_state') in ('FINISH', 'IDLE', 'FAILED') and not state.get('print_error') and not state.get('hms'), 'printer changed during upload'
seq = str(int(time.time()))
cmd = {'print': {'sequence_id': seq, 'command': 'project_file', 'param': 'Metadata/plate_1.gcode', 'url': 'ftp:///' + REMOTE,
                 'subtask_name': REMOTE.removesuffix('.gcode.3mf'), 'project_id': '0', 'profile_id': '0', 'task_id': '0', 'subtask_id': '0',
                 'md5': md5, 'bed_type': 'textured_plate', 'bed_levelling': True, 'timelapse': False, 'use_ams': True,
                 'ams_mapping': [3, 255], 'ams_mapping2': [{'ams_id': 0, 'slot_id': 3}, {'ams_id': 255, 'slot_id': 0}]}}
rec.update(phase='start-intent', dispatch_attempted=True, command=cmd['print']); write(rec)
replies.clear(); c.publish(f'device/{SERIAL}/request', json.dumps(cmd))
t1 = time.time()
while time.time() - t1 < 35 and not replies: time.sleep(0.5)
rec.update(phase='acknowledged' if replies else 'no-acknowledgement', reply=replies[0] if replies else None); write(rec)
print('reply:', json.dumps(replies[0] if replies else None)[:400], flush=True)

started = False; until = time.time() + 900
while time.time() < until:
    c.publish(f'device/{SERIAL}/request', json.dumps({'pushing': {'sequence_id': str(int(time.time())), 'command': 'pushall'}}))
    time.sleep(15)
    sm = summary(); ours = sm.get('subtask_name') == cmd['print']['subtask_name']
    started = started or (ours and sm.get('gcode_state') == 'RUNNING')
    rec['events'].append({'at': time.strftime('%Y-%m-%dT%H:%M:%S%z'), 'status': sm}); write(rec)
    print(json.dumps(rec['events'][-1]), flush=True)
    if sm.get('print_error') or sm.get('hms') or (started and sm.get('gcode_state') in ('FAILED', 'PAUSE')):
        rec['phase'] = 'attention'; write(rec); break
    if ours and (sm.get('layer_num') or 0) >= 1:
        rec['phase'] = 'first-layer'; write(rec); break
c.loop_stop(); c.disconnect()
print('phase:', rec['phase'])
