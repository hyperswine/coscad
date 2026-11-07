#!/usr/bin/env python3
"""Read X1C status, or dispatch the owner-approved three-sample print once.

Uses the existing tank LAN dispatcher, verified printer TLS, actual-package
preflight and upload read-back SHA-256. An existing run record blocks retries.
"""
import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import sys
import time

from prepare_print import HERE, JOB, FOLDER
from verify_print import verify


def main(here=HERE, job=JOB, folder=FOLDER, verify_print=verify,
         authorization='Owner requested exactly three panel fit samples on X1C and confirmed its bed clear, 2026-11-07. Use the currently loaded external PLA.'):
    HERE, JOB, FOLDER = Path(here), job, Path(folder)
    if not __debug__:
        raise RuntimeError('Run without Python optimization; safety assertions are required')
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--start', action='store_true')
    parser.add_argument('--bed-clear', action='store_true')
    args = parser.parse_args()
    if args.start:
        if not args.bed_clear:
            parser.error('Owner bed-clear confirmation is required')
        if (FOLDER / 'print-run.json').exists():
            raise RuntimeError('Dispatch record exists; inspect status instead of resending')
        verify_print()
    spec = importlib.util.spec_from_file_location('tank_dispatch', HERE.parent / 'dispatch.py')
    dispatch = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(dispatch)
    dispatch.B = HERE
    ca = HERE.parents[3] / 'fprisc/build/printer-native-runtime/bbl-printer-ca.pem'
    dispatch.context.CA = str(ca)
    _, printer, _ = dispatch.context.connect('x1c')
    try:
        snapshot = {'status': printer.summary(), 'hms': printer.state.get('hms'),
                    'spools': printer.spools(), 'nozzle_diameter': printer.state.get('nozzle_diameter'),
                    'sdcard': printer.state.get('sdcard')}
        print(json.dumps(snapshot), flush=True)
        if not args.start:
            return
        if printer.state.get('sdcard') is not True:
            raise RuntimeError('Printer SD card is unavailable')
        if not any(name == 'external' and material.upper() == 'PLA' and loaded
                   for name, material, colour, loaded in printer.spools()):
            raise RuntimeError('Loaded external PLA was not reported')
    finally:
        printer.close()
    config = {'job': JOB, 'printer': 'x1c', 'spool': 'external',
              'bed_levelling': True, 'flow_cali': True, 'vibration_cali': True, 'layer_inspect': True,
              'dispatcher_sha256': hashlib.sha256((HERE.parent / 'dispatch.py').read_bytes()).hexdigest(),
              'printer_ca_sha256': hashlib.sha256(ca.read_bytes()).hexdigest()}
    (FOLDER / 'dispatch-config.json').write_text(json.dumps(config, indent=2) + '\n')
    os.environ['TZ'] = 'Australia/Sydney'
    time.tzset()
    sys.argv = [str(HERE.parent / 'dispatch.py'), JOB, '--spool', 'external',
                '--confirm-bed-clear', '--authorization-note',
                authorization]
    dispatch.main()  # Existing durable intent, exact upload verification and one start.


if __name__ == '__main__':
    main()
