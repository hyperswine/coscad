"""One explicit, deduplicated dispatch of a sliced beach-outrigger package to a printer.

  python dispatch.py x1c-pla --spool external --confirm-bed-clear --authorization-note '...'

Requires the owner's bed-clear confirmation and the exact spool. Uploads over verified
FTPS, checks the remote size and SHA-256, then sends exactly one project_file command.
A missing acknowledgement is recorded, never resent. An existing print-run.json blocks
another invocation for that job.
"""
from pathlib import Path
import argparse, hashlib, importlib, json, os, sys, time

# Every dispatch rechecks the actual package against independent thermal intent.
sys.path.insert(0, str(Path(__file__).resolve().parents[2]/'scripts'))
from bambu_preflight import preset_expectations, verify_package

B = Path(__file__).resolve().parent; NAME = 'beach-outrigger'
sys.path.insert(0, str(B)); context = importlib.import_module('printer-context')
def write(path, record):
    tmp = path.with_suffix('.tmp'); tmp.write_text(json.dumps(record, indent=2, default=str)+'\n'); tmp.replace(path)
def main():
    ap = argparse.ArgumentParser(description=__doc__); ap.add_argument('job'); ap.add_argument('--spool', required=True)
    ap.add_argument('--confirm-bed-clear', action='store_true', required=True); ap.add_argument('--authorization-note', required=True); args = ap.parse_args()
    device, material = args.job.split('-'); folder = B/args.job; record_path = folder/'print-run.json'
    assert not record_path.exists(), 'Existing dispatch record: inspect it and the printer instead of resending'
    package = folder/'slice'/f'{NAME}.gcode.3mf'; verified = json.loads((folder/'package-validation.json').read_text()); assert verified['passed']
    digest = hashlib.sha256(package.read_bytes()).hexdigest(); assert digest == verified['package_sha256'], 'Package changed since verification'
    thermal_preflight = verify_package(package, preset_expectations(folder/'presets', plate='Textured PEI Plate'))
    if thermal_preflight['package_sha256'] != digest:
        raise ValueError('Package changed during preflight; refusing to print')
    run, p, tls = context.connect(device)
    try:
        assert p.state.get('gcode_state') in ['FINISH', 'IDLE'], 'Printer is busy'
        assert p.state.get('print_error') == 0 and not p.state.get('hms'), 'Printer has an active fault'
        assert str(p.state.get('nozzle_diameter')) == '0.4'
        spools = p.spools()
        assert any(name == args.spool and mat.upper() == material.upper() for name, mat, colour, loaded in spools), f'Selected {material} spool is not reported: {spools}'
        if args.spool == 'external': assert any(name == 'external' and loaded for name, mat, colour, loaded in spools), 'External spool is not the active tray'
        remote = f'{NAME}-{args.job}-{time.strftime("%Y%m%d-%H%M%S")}.gcode.3mf'
        record = {'job': args.job, 'printer': device, 'material': material, 'package_sha256': digest, 'thermal_preflight': thermal_preflight, 'remote_name': remote, 'spool': args.spool,
                  'bed_clear_owner_confirmed': True, 'authorization_note': args.authorization_note, 'before': p.summary(), 'spools_before': spools,
                  'hms_before': p.state.get('hms'), 'phase': 'preflight', 'dispatch_attempted': False, 'events': [], 'created_at': time.strftime('%Y-%m-%dT%H:%M:%S%z')}
        with record_path.open('x') as f: f.write(json.dumps(record, indent=2, default=str)+'\n')
        sys.path.insert(0, str(context.ROOT/'3d-models/bambu-lan')); import ftp as lanftp
        class FTPS(lanftp.ImplicitFTPS):
            def storbinary(self, cmd, fp, blocksize=32768):
                # The X1C answers 426 when the data connection is dropped without close_notify;
                # send it with a bounded timeout (the A1/P1S never answer it, so ignore failures).
                self.voidcmd('TYPE I')
                with self.transfercmd(cmd) as conn:
                    while buf := fp.read(blocksize): conn.sendall(buf)
                    try: conn.settimeout(5); conn.unwrap()
                    except Exception: pass
                return self.voidresp()
        ftp = FTPS(context=tls(), timeout=60)
        try:
            ftp.connect(os.environ['BAMBU_HOST'], 990); ftp.login('bblp', os.environ['BAMBU_ACCESS_CODE']); ftp.prot_p()
            with package.open('rb') as f: ftp.storbinary('STOR '+remote, f)
            assert ftp.size(remote) == package.stat().st_size, 'Remote size mismatch'
            ftp.voidcmd('TYPE I'); remote_hash = hashlib.sha256(); received = 0
            with ftp.transfercmd('RETR '+remote) as stream:
                while True:
                    data = stream.recv(32768)
                    if not data: break
                    remote_hash.update(data); received += len(data)
            ftp.voidresp()
            if remote_hash.hexdigest() != digest or received != package.stat().st_size:
                raise ValueError('Remote content mismatch')
        finally: ftp.close()
        record.update(phase='uploaded-verified', remote_content_sha256=digest); write(record_path, record)
        assert p.state.get('gcode_state') in ['FINISH', 'IDLE'] and p.state.get('print_error') == 0 and not p.state.get('hms'), 'Printer changed during upload'
        seq = str(int(time.time()*1000) % 1000000000); use_ams = args.spool != 'external'
        command = {'print': {'sequence_id': seq, 'command': 'project_file', 'param': 'Metadata/plate_1.gcode', 'project_id': '0', 'profile_id': '0', 'task_id': '0', 'subtask_id': '0',
                             'subtask_name': remote.removesuffix('.3mf'), 'url': 'file:///sdcard/'+remote, 'md5': '', 'timelapse': False, 'bed_type': 'auto', 'bed_levelling': True,
                             'flow_cali': True, 'vibration_cali': True, 'layer_inspect': True, 'use_ams': use_ams, 'ams_mapping': [int(args.spool[3:])] if use_ams else [254]}}
        p.replies.clear(); record.update(phase='start-intent', dispatch_attempted=True, sequence_id=seq); write(record_path, record); p.send(command)
        until = time.monotonic() + 35; reply = None
        while time.monotonic() < until:
            reply = next((r for r in p.replies if r.get('command') == 'project_file' and str(r.get('sequence_id')) == seq), None)
            if reply: break
            time.sleep(.2)
        if reply:
            record['reply'] = {k: reply.get(k) for k in ['result', 'reason', 'command', 'sequence_id']}; record['phase'] = 'acknowledged' if str(reply.get('result', '')).lower() == 'success' else 'rejected'   # the X1C answers SUCCESS in upper case
        else: record['phase'] = 'start-unknown'; record['acknowledgement_uncertain'] = True
        write(record_path, record); print(json.dumps({'printer': device, 'job': args.job, 'phase': record['phase'], 'spool': args.spool, 'remote': remote}), flush=True)
        if record['phase'] == 'rejected': return
        until = time.monotonic() + 900
        while time.monotonic() < until:
            state = p.summary(); event = {'at': time.strftime('%Y-%m-%dT%H:%M:%S%z'), 'status': state, 'hms': p.state.get('hms')}; record['events'].append(event); write(record_path, record); print(json.dumps(event), flush=True)
            is_job = state.get('subtask_name') in [remote, remote.removesuffix('.3mf')] or state.get('gcode_file') == remote
            if is_job and state.get('gcode_state') == 'RUNNING' and state.get('total_layer_num') == verified['layers'] and 1 <= (state.get('layer_num') or 0) <= verified['layers']:
                record['phase'] = 'first-layer'; record['first_layer_reached'] = True; write(record_path, record); break
            if state.get('print_error') or state.get('gcode_state') in ['FAILED', 'PAUSE']:
                record['needs_attention'] = True; write(record_path, record); break
            time.sleep(15)
    finally: p.close()
if __name__ == '__main__': main()
