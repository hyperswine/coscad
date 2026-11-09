#!/usr/bin/env python3
"""Recheck the four-part package, exported geometry, paths, saved settings and actual temperatures."""
from collections import Counter, defaultdict
import importlib.util
import json
from prepare_print import HERE, FOLDER, WALL, FLOOR, REFERENCE, digest


def verify():
    intent = json.loads((FOLDER / 'print-intent.json').read_text())
    if Counter(p['name'] for p in intent['parts']) != Counter({WALL: 3, FLOOR: 1}):
        raise ValueError('Expected exactly three walls and one notched floor')
    geometry = json.loads((HERE / 'geometry-validation.json').read_text())
    if not geometry['passed']:
        raise ValueError('Geometry validation failed')
    for part in intent['parts']:
        for key in ('source_sha256', 'stl_sha256'):
            if part[key] != geometry['parts'][part['name']][key]:
                raise ValueError('Print intent differs from geometric validation')
    for name, expected in intent['reference_presets_sha256'].items():
        if digest(FOLDER / 'presets' / name) != expected or digest(REFERENCE / name) != expected:
            raise ValueError('Presets differ from the successful wall print')
    # Check each copy, not only the plate's highest point: no absent wall or
    # dropped thin floor tips can hide behind another object's layer count.
    object_layers = defaultdict(set)
    z = None
    with (FOLDER / 'slice/plate_1.gcode').open() as code:
        for line in code:
            if line.startswith('; Z_HEIGHT:'):
                z = round(float(line.split(':')[1]), 3)
            if line.startswith('; OBJECT_ID:') and z is not None:
                object_layers[int(line.split(':')[1])].add(z)
    coverage = {}
    if set(object_layers) != {part['object_id'] for part in intent['parts']}:
        raise ValueError('Unexpected or missing object in sliced paths')
    for part in intent['parts']:
        height = 70 if part['name'] == WALL else 91
        count = round((height - .2) / .1) + 1
        levels = object_layers[part['object_id']]
        if levels != {round(.2 + .1 * index, 3) for index in range(count)}:
            raise ValueError(f'Missing layers for {part["name"]} copy {part["object_id"]}')
        coverage[str(part['object_id'])] = {'part': part['name'], 'layers': len(levels), 'top_mm': max(levels)}
    spec = importlib.util.spec_from_file_location('fit_verify', HERE.parent / 'panel-fit-v2/verify_print.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    result = module.verify(HERE, FOLDER, expected_parts=4)
    result['object_layer_coverage'] = coverage
    (FOLDER / 'package-validation.json').write_text(json.dumps(result, indent=2) + '\n')
    return result


if __name__ == '__main__':
    verify()
