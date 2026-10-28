"""Offline, fail-closed thermal checks for the exact Bambu package to be sent.

Intent comes from the input presets (or an explicit reviewed contract), never
from the exported package. Only single-plate, homogeneous-temperature Bambu
G-code is supported. Unsupported or ambiguous output requires a new validator,
not an acceptance flag. No printer connections or credentials are used here.
"""
import argparse
from contextlib import contextmanager
import hashlib
import json
import math
from pathlib import Path
import re
import tempfile
import zipfile

PLATE_KEYS = {
    'Textured PEI Plate': 'textured_plate_temp',
    'Cool Plate': 'cool_plate_temp',
    'Engineering Plate': 'eng_plate_temp',
    'High Temp Plate': 'hot_plate_temp',
    'Supertack Plate': 'supertack_plate_temp',
}
NUMBER = r'[-+]?(?:\d+(?:\.\d*)?|\.\d+)'
WORD = re.compile(r'([A-Z])(' + NUMBER + r')(?![\d.])', re.I)


class PreflightError(ValueError):
    """A package cannot be established to match the intended print settings."""


def temperature(value, label):
    values = value if isinstance(value, list) else [value]
    try:
        if not values or any(isinstance(v, bool) for v in values):
            raise ValueError()
        numbers = [float(v) for v in values]
        if any(not math.isfinite(v) or v <= 0 for v in numbers):
            raise ValueError()
    except (ValueError, TypeError):
        raise PreflightError(f'{label}: missing or invalid positive temperature') from None
    if len(set(numbers)) != 1:
        raise PreflightError(f'{label}: different filament temperatures are not supported by this preflight')
    return numbers[0]


def expectations(process, filament, *, plate=None):
    """Capture independent intent before slicing, including filament overrides."""
    plate = plate or process.get('curr_bed_type')
    if plate not in PLATE_KEYS:
        raise PreflightError(f'Unsupported or missing intended plate type: {plate!r}')
    if process.get('curr_bed_type') != plate:
        raise PreflightError(f'Process plate {process.get("curr_bed_type")!r} differs from intended {plate!r}')
    key = PLATE_KEYS[plate]
    return dict(plate=plate,
                bed_initial_c=temperature(filament.get(key+'_initial_layer'), key+'_initial_layer'),
                bed_c=temperature(filament.get(key), key),
                nozzle_initial_c=temperature(filament.get('nozzle_temperature_initial_layer'), 'nozzle_temperature_initial_layer'),
                nozzle_c=temperature(filament.get('nozzle_temperature'), 'nozzle_temperature'))


def preset_expectations(directory, *, plate):
    directory = Path(directory)
    return expectations(json.loads((directory/'process.json').read_text()),
                        json.loads((directory/'filament.json').read_text()), plate=plate)


def _matches(actual, expected, label):
    if actual != {expected}:
        raise PreflightError(f'{label}: expected {expected:g} C, actual possible targets {sorted(str(v) for v in actual)}')


def _gcode_check(stream, expected):
    # Sets retain both possible results of Bambu M622/M623 conditional blocks.
    # A final unconditional command resolves earlier calibration temperatures.
    targets = {'bed': {None}, 'nozzle': {None}, 'bed_wait': {None}}
    branches = []
    relative_e, e_position = False, 0.0
    layer, deposited, checked_layers = 0, 0, set()
    ended, started = False, False
    for line_number, raw in enumerate(stream, 1):
        try:
            line = raw.decode('utf-8').strip()
        except UnicodeDecodeError:
            raise PreflightError('G-code is not UTF-8 text') from None
        if line == '; CHANGE_LAYER':
            if ended:
                raise PreflightError('Layer after machine end G-code')
            if not started:
                _matches(targets['bed'], expected['bed_initial_c'], 'Startup bed target')
                _matches(targets['bed_wait'], expected['bed_initial_c'], 'Startup bed wait (M190)')
                _matches(targets['nozzle'], expected['nozzle_initial_c'], 'Startup nozzle target')
            started = True
            layer += 1
        if line == '; MACHINE_END_GCODE_START':
            ended = True
        command = line.split(';', 1)[0].strip().upper()
        if not command:
            continue
        if re.match(r'^N\d', command) or '*' in command:
            raise PreflightError(f'Numbered/checksummed G-code is unsupported at line {line_number}')
        match = re.match(r'^([GMT]\d+)(?=\s|[A-Z]|$)', command)
        if not match:
            continue
        token = match[1]
        op = token[0]+str(int(token[1:]))
        words = dict((k.upper(), float(v)) for k, v in WORD.findall(command[len(token):]))
        if op == 'M622':
            branches.append({k: set(v) for k, v in targets.items()})
        elif op == 'M623':
            if not branches:
                raise PreflightError(f'Unmatched M623 at line {line_number}')
            before = branches.pop()
            targets = {k: v | before[k] for k, v in targets.items()}
        elif op in ('M140', 'M190', 'M104', 'M109'):
            # R is a supported wait target too. Unresolved placeholders, missing
            # targets and extra malformed S/R words must never pass as numbers.
            params = command[len(token):].strip()
            target_words = re.findall(r'[SR]([^\s]*)', params)
            if len(target_words) != 1 or not re.fullmatch(NUMBER, target_words[0]):
                raise PreflightError(f'Unresolved or ambiguous {op} target at line {line_number}')
            value = float(target_words[0])
            which = 'bed' if op in ('M140', 'M190') else 'nozzle'
            targets[which] = {value}
            if op == 'M190':
                targets['bed_wait'] = {value}
        elif op in ('M82', 'M83'):
            relative_e = op == 'M83'
        elif op == 'G92' and 'E' in words:
            e_position = words['E']
        elif op in ('G0', 'G1', 'G2', 'G3') and 'E' in words:
            delta = words['E'] if relative_e else words['E'] - e_position
            e_position = words['E'] if not relative_e else e_position + words['E']
            if started and not ended and delta > 0 and any(k in words for k in ('X', 'Y', 'I', 'J')):
                phase = 'initial_c' if layer == 1 else 'c'
                _matches(targets['bed'], expected['bed_'+phase], f'Layer {layer} bed target, line {line_number}')
                _matches(targets['nozzle'], expected['nozzle_'+phase], f'Layer {layer} nozzle target, line {line_number}')
                deposited += 1
                checked_layers.add(layer)
    if branches:
        raise PreflightError('Unclosed conditional G-code block')
    if not started or not ended or not deposited or checked_layers != set(range(1, layer+1)):
        raise PreflightError('Missing Bambu layer/end markers or deposition evidence for every layer')
    return dict(layers_checked=layer, deposition_moves_checked=deposited)


def _verify(stream, expected):
    # Revalidate callers' explicit contracts too; no NaN or missing-field bypass.
    if expected.get('plate') not in PLATE_KEYS:
        raise PreflightError('Missing or unsupported intended plate')
    expected = dict(plate=expected['plate'], **{
        k: temperature(expected.get(k), k)
        for k in ('bed_initial_c', 'bed_c', 'nozzle_initial_c', 'nozzle_c')})
    try:
        with zipfile.ZipFile(stream) as archive:
            names = archive.namelist()
            if len(names) != len(set(names)):
                raise PreflightError('Duplicate archive members are ambiguous')
            if [n for n in names if re.fullmatch(r'Metadata/plate_\d+\.gcode', n)] != ['Metadata/plate_1.gcode']:
                raise PreflightError('Expected exactly one sliced plate (Metadata/plate_1.gcode)')
            settings = json.loads(archive.read('Metadata/project_settings.config'))
            if not isinstance(settings, dict):
                raise PreflightError('Exported settings must be a JSON object')
            if settings.get('curr_bed_type') != expected['plate']:
                raise PreflightError(f'Exported plate mismatch: expected {expected["plate"]!r}, got {settings.get("curr_bed_type")!r}')
            exported = expectations({'curr_bed_type': expected['plate']}, settings)
            if exported != expected:
                raise PreflightError(f'Exported temperatures differ from intended settings: expected {expected}, got {exported}')
            with archive.open('Metadata/plate_1.gcode') as code:
                evidence = _gcode_check(code, expected)
    except (zipfile.BadZipFile, KeyError, json.JSONDecodeError, UnicodeError) as exc:
        raise PreflightError('Missing or malformed sliced package: '+str(exc)) from None
    return dict(schema='coscad.thermal-preflight/1', checked=True, expected=expected, **evidence)


@contextmanager
def checked_package(path, expected):
    """Freeze the bytes, verify them, then give the uploader that same snapshot.

    The temporary file is private and deleted on every exit. Saved JSON reports
    are evidence only; they are never used instead of a fresh package check.
    """
    with tempfile.TemporaryFile() as snapshot:
        digest = hashlib.sha256()
        with open(path, 'rb') as source:
            while chunk := source.read(1024*1024):
                digest.update(chunk)
                snapshot.write(chunk)
        snapshot.seek(0)
        report = _verify(snapshot, expected)
        report['package_sha256'] = digest.hexdigest()
        snapshot.seek(0)
        yield snapshot, report


def verify_package(path, expected):
    with checked_package(path, expected) as (_, report):
        return report


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('package', type=Path)
    parser.add_argument('--presets', required=True, type=Path)
    parser.add_argument('--plate', required=True, choices=PLATE_KEYS)
    args = parser.parse_args()
    try:
        print(json.dumps(verify_package(args.package, preset_expectations(args.presets, plate=args.plate)), indent=2))
    except (OSError, ValueError) as exc:
        parser.exit(2, f'Print preflight refused: {exc}\n')
