"""One read-only comparison of this P1S job with the public dashboard.

Keeps credentials and login proof/session in-process. Emits only job/fault
observations. Does not send printer control commands or alter dashboard data.
"""
from datetime import datetime
import importlib.util
import json
import os
from pathlib import Path
import re
import socket
import sys
import time
from zoneinfo import ZoneInfo

B = Path(__file__).resolve().parent
ROOT = B.parents[2].parent
socket.setdefaulttimeout(15)


def main():
    record = json.loads((B/'full-print-run.json').read_text())
    expected = record['remote_name']
    values = {}
    for line in (ROOT/'3d-models/.env').read_text().splitlines():
        if '=' in line and not line.lstrip().startswith('#'):
            k, v = line.split('=', 1)
            values[k.strip()] = v.strip().strip('"').strip("'")
    assert values['BAMBU_P1S_SERIAL'] == '01P00A442400974'
    os.environ.update(BAMBU_HOST=record.get('printer_host', '192.168.0.27'),
                      BAMBU_SERIAL=values['BAMBU_P1S_SERIAL'],
                      BAMBU_ACCESS_CODE=values['BAMBU_P1S_ACCESS_CODE'])
    spec = importlib.util.spec_from_file_location('bambu_run', ROOT/'coscad/scripts/bambu-print-run.py')
    run = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(run)
    p = run.Printer()
    try:
        direct = p.summary()
        hms = [{'attr': v['attr'], 'code': v['code']} for v in p.state.get('hms', [])]
    finally:
        p.close()
    sys.path.insert(0, str(ROOT/'fprisc/tests'))
    from check_printer_public import Login
    from cryptography.hazmat.primitives.serialization import load_pem_private_key
    key = load_pem_private_key(Path('/Users/jasonqin/.config/fprisc/printer-dashboard/mac-ed25519.pem').read_bytes(), password=None)
    session = Login('https://3dprint.cswine.cloud', key)
    direct_codes = {f"{int(v['attr']):08X}{int(v['code']):08X}" for v in hms}
    if direct.get('print_error'):
        direct_codes.add(f"{int(direct['print_error']):08X}")
    try:
        for attempt in range(3):
            row = session.snapshot()['p1s']
            site_codes = {a['code'] for a in row['alerts']}
            missing = sorted(direct_codes-site_codes)
            if not missing or attempt == 2:
                break
            time.sleep(2)
    finally:
        session.request('/auth/logout', {})
    labels = (ROOT/'fprisc/std/printeralertcodes.fpr').read_text()
    tokens = re.search(r'\("Filament runout reported", "([^"]+)"\)', labels)
    runout_codes = set(tokens.group(1).split('|')) if tokens else set()
    source_job = direct.get('gcode_file') or direct.get('subtask_name') or ''
    is_job = source_job in (expected, expected.removesuffix('.3mf'))
    site_job = row.get('job') or ''
    site_is_job = site_job in (expected, expected.removesuffix('.3mf'))
    terminal = is_job and direct.get('gcode_state') in ('FINISH', 'FAILED')
    now = datetime.now(ZoneInfo('Australia/Sydney')).isoformat(timespec='seconds')
    observation = {'checked_at': now, 'expected_job': expected, 'direct': direct,
                   'direct_alert_codes': sorted(direct_codes), 'this_job_on_printer': is_job,
                   'dashboard': {k: row.get(k) for k in ['state', 'job', 'progress', 'layers',
                       'remaining', 'connection', 'last_report_iso', 'report_hz', 'error', 'alerts',
                       'filament_source', 'fault_data_problem', 'hms_reported', 'print_error_reported']},
                   'this_job_on_dashboard': site_is_job, 'missing_dashboard_codes': missing,
                   'runout_seen': bool((direct_codes | site_codes) & runout_codes),
                   'terminal': terminal}
    signature = {'job': source_job, 'state': direct.get('gcode_state'),
                 'codes': sorted(direct_codes), 'missing': missing,
                 'site_state': row.get('state'), 'site_job': site_job,
                 'fault_data_problem': row.get('fault_data_problem'), 'connection': row.get('connection')}
    previous_file = B/'full-monitor-state.json'
    previous = json.loads(previous_file.read_text()) if previous_file.exists() else {}
    observation['meaningful_change'] = signature != previous.get('signature')
    with (B/'full-monitor-events.jsonl').open('a') as out:
        out.write(json.dumps(observation)+'\n')
    previous_file.write_text(json.dumps({'signature': signature, 'last': observation}, indent=2)+'\n')
    print(json.dumps(observation, indent=2))


if __name__ == '__main__':
    main()
