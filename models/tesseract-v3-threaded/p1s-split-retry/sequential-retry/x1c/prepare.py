#!/usr/bin/env python3
"""Create the owner-approved mixed sequential trial with native X1C presets."""
from pathlib import Path
import hashlib
import importlib.util
import json
import shutil
import subprocess
import sys

HERE = Path(__file__).resolve().parent
SEQUENTIAL = HERE.parent
SPLIT = SEQUENTIAL.parent
ROOT = HERE.parents[4]


def main():
    folder = HERE/'mixed'
    folder.mkdir(exist_ok=True)
    if (folder/'print-run.json').exists():
        raise ValueError('Dispatch record exists; preserve this attempt')
    source = SEQUENTIAL/'mixed'
    packing = json.loads((source/'packing.json').read_text())
    assert [i['name'] for i in packing['items']] == ['strut', 'bar-print']
    for name in ['packing.json', 'sequence-process.json', 'sequence-filament.json'] + [i['file'] for i in packing['items']]:
        shutil.copy2(source/name, folder/name)
    packing['label'] = 'Tesseract sequential mixed X1C trial'
    (folder/'packing.json').write_text(json.dumps(packing, indent=2)+'\n')
    for item in packing['items']:
        assert hashlib.sha256((source/item['file']).read_bytes()).digest() == hashlib.sha256((folder/item['file']).read_bytes()).digest()
    spec = importlib.util.spec_from_file_location('slicer', ROOT/'scripts/bambu-slice.py')
    slicer = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(slicer)
    machine = slicer.machine_with_templates('Bambu Lab X1 Carbon 0.4 nozzle')
    keys = ['extruder_clearance_height_to_lid', 'extruder_clearance_height_to_rod',
            'extruder_clearance_max_radius', 'extruder_clearance_dist_to_rod',
            'extruder_clearance_radius', 'printable_height']
    clearance = {k: float(machine[k]) for k in keys}
    first, last = [i['bounds'] for i in packing['items']]
    gap = last[0][1]-first[1][1]
    assert first[1][2]+5 < clearance['extruder_clearance_height_to_lid']
    assert gap > max(clearance['extruder_clearance_max_radius'], clearance['extruder_clearance_dist_to_rod'])+5
    assert last[1][2] < clearance['printable_height']
    plan = dict(printer='x1c', layout='mixed', trial_components={'strut': 1, 'bar-print': 1},
                order=['strut', 'bar-print'], unmodified_machine_clearance=clearance,
                owner_authorization='Owner cleaned all three printers and requested printing the new sequential trial on X1C on 2026-10-31.',
                verified=False, dispatched=False)
    (HERE/'plan.json').write_text(json.dumps(plan, indent=2)+'\n')
    subprocess.run([sys.executable, str(SPLIT/'prepare.py'), '--bed', '4', '--printer', 'x1c',
                    '--output-dir', str(folder), '--process-overrides', str(folder/'sequence-process.json'),
                    '--filament-overrides', str(folder/'sequence-filament.json')], check=True)


if __name__ == '__main__':
    main()
