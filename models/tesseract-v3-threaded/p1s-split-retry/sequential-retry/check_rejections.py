#!/usr/bin/env python3
"""Offline fault injection: no printer, credentials or print commands."""
from pathlib import Path
import contextlib
import hashlib
import importlib.util
import io
import json
import shutil
import tempfile
import zipfile

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('sequential_verify', HERE/'verify.py')
checker = importlib.util.module_from_spec(spec)
spec.loader.exec_module(checker)


def check(name, mutate, expected_message):
    with tempfile.TemporaryDirectory(prefix='tesseract-sequence-negative-') as tmp:
        folder = Path(tmp)/name
        shutil.copytree(HERE/'mixed', folder)
        package = folder/'slice/tesseract-bed4-p1s.gcode.3mf'
        with zipfile.ZipFile(package) as archive:
            entries = {i.filename: (i, archive.read(i)) for i in archive.infolist()}
        mutate(entries)
        with zipfile.ZipFile(package, 'w', zipfile.ZIP_DEFLATED) as archive:
            for info, data in entries.values(): archive.writestr(info, data)
        # Deliberately refresh the ordinary hash so the dedicated order/motion
        # checks, rather than a stale-artifact guard, have to catch the fault.
        p = folder/'package-validation.json'
        geometry = json.loads(p.read_text())
        geometry['package_sha256'] = hashlib.sha256(package.read_bytes()).hexdigest()
        p.write_text(json.dumps(geometry))
        try:
            with contextlib.redirect_stdout(io.StringIO()): checker.verify(folder)
        except AssertionError as error:
            assert expected_message in str(error), (name, str(error))
        else:
            raise AssertionError(name+' was accepted')
        print('PASS rejects '+name)


def setting(entries, key, value):
    name = 'Metadata/project_settings.config'
    info, raw = entries[name]
    data = json.loads(raw)
    data[key] = value
    entries[name] = (info, json.dumps(data).encode())


def interleave(entries):
    name = 'Metadata/plate_1.gcode'
    info, raw = entries[name]
    marker = b'; start printing object, unique label id: 4'
    assert marker in raw
    raw = raw.replace(marker, marker+b'\n; start printing object, unique label id: 2\n'+marker, 1)
    entries[name] = (info, raw)


def return_collision(entries):
    name = 'Metadata/plate_1.gcode'
    info, raw = entries[name]
    marker = b'; MACHINE_END_GCODE_START'
    assert raw.count(marker) == 1
    # After completing the rear bar, lower and drive back into the tall front
    # strut. These are non-extruding moves; temperature checks alone allow them.
    raw = raw.replace(marker, b'G90\nG1 Z0.2 F600\nG1 X128 Y64 F6000\n'+marker)
    entries[name] = (info, raw)


if __name__ == '__main__':
    check('wrong-sequence-setting', lambda e: setting(e, 'print_sequence', 'by layer'), 'Per-object mode not exported')
    check('lowered-head-clearance', lambda e: setting(e, 'extruder_clearance_max_radius', '1'), 'Changed exported clearance')
    check('interleaved-object-order', interleave, 'Objects must complete once')
    check('return-through-finished-object', return_collision, 'head')
