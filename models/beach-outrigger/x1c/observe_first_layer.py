"""Bounded startup observation for the dispatched X1C job: polls until layer 1 or an issue,
appends events to x1c-pla/print-run.json, grabs one camera frame, then exits. No retries, no resend."""
import sys, importlib.util, json, time, subprocess, os
D = sys.argv[1]; spec = importlib.util.spec_from_file_location('pc', f'{D}/printer-context.py'); pc = importlib.util.module_from_spec(spec); spec.loader.exec_module(pc)
run, p, tls = pc.connect('x1c'); rec_path = f'{D}/x1c-pla/print-run.json'; rec = json.load(open(rec_path)); verified = json.load(open(f'{D}/x1c-pla/package-validation.json'))
remote = rec['remote_name']; deadline = time.monotonic() + 40*60
try:
    while time.monotonic() < deadline:
        s = p.summary(); s['stg_cur'] = p.state.get('stg_cur'); ev = {'at': time.strftime('%Y-%m-%dT%H:%M:%S%z'), 'status': s, 'hms': p.state.get('hms')}
        rec['events'].append(ev); json.dump(rec, open(rec_path, 'w'), indent=2, default=str); print(json.dumps(ev), flush=True)
        is_job = str(s.get('subtask_name', '')).startswith(remote.removesuffix('.3mf'))
        if is_job and s.get('gcode_state') == 'RUNNING' and s.get('total_layer_num') == verified['layers'] and 1 <= (s.get('layer_num') or 0):
            rec['phase'] = 'first-layer'; rec['first_layer_reached'] = True; break
        if s.get('print_error') or s.get('gcode_state') in ['FAILED', 'PAUSE', 'IDLE', 'FINISH']:
            rec['needs_attention'] = True; break
        time.sleep(20)
finally:
    p.close()
    code = os.environ['BAMBU_ACCESS_CODE']; out = f"{D}/x1c/{time.strftime('%Y%m%d-%H%M%S')}-x1c-first-layer-check-printer-capture.png"
    r = subprocess.run(['ffmpeg', '-hide_banner', '-loglevel', 'error', '-rtsp_transport', 'tcp', '-tls_verify', '0', '-i', f'rtsps://bblp:{code}@192.168.0.14:322/streaming/live/1', '-frames:v', '1', '-y', out], capture_output=True)
    rec['startup_camera'] = out if r.returncode == 0 else None
    json.dump(rec, open(rec_path, 'w'), indent=2, default=str); print('observer exit; phase', rec.get('phase'), 'camera', rec.get('startup_camera'))
