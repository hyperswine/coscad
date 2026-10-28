#!/usr/bin/env python3
"""Reproduce the shelf bracket design-to-slice integration example. Never prints.

Requires a built coscad binary; --slice also needs local Bambu Studio presets.
Uses a fresh output directory so stale artifacts cannot be mistaken for results.
"""
import argparse
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys
import time
import zipfile

ROOT = Path(__file__).resolve().parent.parent


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def verify_slice(archive, expected):
    """Read the exported settings, not merely the requested preset overrides."""
    with zipfile.ZipFile(archive) as package:
        settings = json.loads(package.read('Metadata/project_settings.config'))
        for key, value in expected.items():
            if settings.get(key) != value:
                raise ValueError(f'slicer changed {key}: requested {value!r}, exported {settings.get(key)!r}')
        gcode = package.read('Metadata/plate_1.gcode').decode('utf-8')
        if '; FEATURE: Support' not in gcode or 'G1 ' not in gcode:
            raise ValueError('sliced toolpaths do not contain the expected supports and movement')
        return {key: settings[key] for key in expected}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--coscad', default='coscad', help='executable path, not a shell command')
    parser.add_argument('--output', type=Path, required=True, help='new output directory')
    parser.add_argument('--backend', choices=['native-experimental', 'openscad'], default='native-experimental')
    parser.add_argument('--slice', action='store_true', help='prepare A1 G-code with Bambu Studio; never dispatch')
    parser.add_argument('--images', action='store_true', help='render instruction images with OpenSCAD')
    args = parser.parse_args()
    binary = shutil.which(args.coscad)
    if not binary:
        parser.error('coscad executable not found')
    binary = str(Path(binary).resolve())
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    for name in ['shelf-bracket.coscad', 'shelf-bracket.assemble', 'shelf-bracket-process.json']:
        shutil.copy2(ROOT / 'examples/stress' / name, output / name)
    part = output / 'shelf-bracket.coscad'
    assembly = output / 'shelf-bracket.assemble'
    record = {'schema': 'coscad.workflow/1', 'example': 'shelf-bracket', 'backend': args.backend,
              'compiler': {'path': binary, 'sha256': digest(Path(binary))},
              'sources': {p.name: digest(p) for p in output.iterdir()}, 'stages': [],
              'tools': {name: digest(ROOT / name) for name in ['scripts/shelf-bracket-workflow.py', 'scripts/bambu-slice.py', 'pipeline.lock.json']},
              'status': 'running', 'dispatched': False}

    def save():
        (output / 'workflow.json').write_text(json.dumps(record, indent=2)+'\n')

    def stage(name, command, timeout=300):
        print(name, flush=True)
        started = time.monotonic()
        entry = {'name': name, 'command': [str(x) for x in command], 'status': 'running'}
        record['stages'].append(entry)
        save()
        try:
            result = subprocess.run(command, cwd=output, capture_output=True, text=True, timeout=timeout)
            (output / (name+'.log')).write_text(result.stdout+result.stderr)
            entry.update(exit=result.returncode, seconds=round(time.monotonic()-started, 2),
                         status='passed' if result.returncode == 0 else 'failed', log=name+'.log')
            save()
            if result.returncode:
                raise RuntimeError(f'{name} failed (exit {result.returncode}); see {output / (name+".log")}')
            return result.stdout
        except (OSError, subprocess.TimeoutExpired) as exc:
            entry.update(status='failed', error=str(exc))
            save()
            raise

    try:
        if args.slice:
            stage('slice-pins', [sys.executable, ROOT / 'scripts/pipeline-lock.py', '--scope', 'slice', '--check'])
        record['capabilities'] = json.loads(stage('capabilities', [binary, 'capabilities', '--json']))
        if args.backend not in record['capabilities']['backends']:
            raise RuntimeError('selected backend unavailable in this compiler')
        for name, source in [('validate-part', part), ('validate-assembly', assembly)]:
            stage(name, [binary, 'validate', '--json', source])
        backend = '--backend='+args.backend
        stage('check', [binary, 'check', assembly, backend])
        stage('stress', [binary, 'stress', part, '--full', backend], timeout=900)
        stress = json.loads((output / 'shelf-bracket_stress.json').read_text())
        stage('next', [binary, 'next', assembly, backend])
        manifest = json.loads((output / 'shelf-bracket_manifest.json').read_text())
        variants = manifest['variants']
        if len(variants) != 1 or variants[0]['down'] != 'bot' or variants[0]['hints'].get('material') != 'pla':
            raise RuntimeError('manufacturing orientation/material differs from this analysis workflow')
        if manifest['geometry_backend'] != stress['geometry_backend']:
            raise RuntimeError('stress and manufacturing backend mismatch')
        stage('plan', [binary, 'plan', assembly, backend] + (['--png'] if args.images else []))
        if args.slice:
            stage('slice', [sys.executable, ROOT / 'scripts/bambu-slice.py', output / 'shelf-bracket_manifest.json',
                           '--printer', 'Bambu Lab A1 0.4 nozzle', '--process-overrides', output / 'shelf-bracket-process.json'], timeout=600)
            expected = json.loads((output / 'shelf-bracket-process.json').read_text())
            expected.update(brim_type='outer_only', brim_width='5', printer_settings_id='Bambu Lab A1 0.4 nozzle')
            record['verified_slice_settings'] = verify_slice(output / 'shelf-bracket_bed1.gcode.3mf', expected)
        record['status'] = 'sliced' if args.slice else 'planned'
        record['artifacts'] = {p.name: digest(p) for p in sorted(output.iterdir()) if p.is_file() and p.name != 'workflow.json'}
        save()
        (output / 'README.md').write_text(
            '# Shelf bracket workflow\n\n'
            f'Status: {record["status"]}. No printer command was sent.\n\n'
            '- [Stage results and artifact hashes](workflow.json)\n'
            '- [Stress summary](stress.log) and [full stress field](shelf-bracket_stress.json)\n'
            '- [Assembly instructions](shelf-bracket_plan.md)\n'
            '- [Manufacturing manifest](shelf-bracket_manifest.json)\n\n'
            'The 20 N calculation uses homogeneous PLA and fully clamped supports. '
            'It does not certify the printed bracket, mounting hardware, infill, layer bonding or creep. '
            'The print keeps model Z vertical, uses 100% infill and supports under the arm; '
            'supports may grow on the base. Review the sliced toolpaths before any dispatch.\n')
        print(f'Completed: {output / "README.md"}')
    except (OSError, ValueError, KeyError, RuntimeError, zipfile.BadZipFile, subprocess.TimeoutExpired) as exc:
        record.update(status='failed', error=str(exc))
        save()
        print(str(exc), file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
