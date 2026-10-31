#!/usr/bin/env python3
"""Audit the staged alternatives; never dispatch or modify a printer job."""
from pathlib import Path
import importlib.util
import itertools
import json
import math
import re
import sys
import zipfile

import numpy as np
from shapely.geometry import MultiPoint

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
sys.path.insert(0, str(ROOT/'scripts'))
from bambu_preflight import preset_expectations, verify_package


def layer_review(code, centers, offset):
    x = y = z = e = 0.
    absolute, relative_e, active = True, False, False
    layer, owner, feature = 0, None, ''
    width, max_brim_width = .5, 0.
    seen = {}
    brims = [[] for _ in centers]
    travel, distances = 0., []
    last_owner, last_layer = None, None
    for line in code.splitlines():
        if line == '; CHANGE_LAYER': active = True; layer += 1
        if line == '; MACHINE_END_GCODE_START': active = False
        if line.startswith('; FEATURE:'): feature = line.split(':', 1)[1].strip()
        if line.startswith('; LINE_WIDTH:'): width = float(line.split(':', 1)[1])
        match = re.match(r'; start printing object, unique label id: (\d+)', line)
        if match: owner = match[1]
        if line.startswith('; stop printing object'): owner = None
        raw = line.split(';', 1)[0].strip()
        if not raw: continue
        op = raw.split()[0]
        v = {k: float(n) for k, n in re.findall(r'([XYZIJEP])(-?\d*\.?\d+)', raw)}
        if op == 'G90': absolute = True
        if op == 'G91': absolute = False
        if op == 'M82': relative_e = False
        if op == 'M83': relative_e = True
        if op == 'G92':
            assert not any(k in v for k in ('X', 'Y', 'Z'))
            if 'E' in v: e = v['E']
            continue
        if op not in ('G0', 'G1', 'G2', 'G3'): continue
        nx = v.get('X', x) if absolute else x+v.get('X', 0)
        ny = v.get('Y', y) if absolute else y+v.get('Y', 0)
        nz = v.get('Z', z) if absolute else z+v.get('Z', 0)
        de = v.get('E', 0) if relative_e else v.get('E', e)-e
        if 'E' in v: e += de
        distance = math.hypot(nx-x, ny-y)
        pts = np.array([[x, y], [nx, ny]])
        if op in ('G2', 'G3'):
            assert 'I' in v or 'J' in v
            assert v.get('P', 1) == 1
            cx, cy = x+v.get('I', 0), y+v.get('J', 0)
            radius = math.hypot(x-cx, y-cy)
            a, b = math.atan2(y-cy, x-cx), math.atan2(ny-cy, nx-cx)
            sweep = (b-a) % (2*math.pi) if op == 'G3' else -((a-b) % (2*math.pi))
            if abs(sweep) < 1e-8: sweep = 2*math.pi if op == 'G3' else -2*math.pi
            distance = abs(sweep)*radius
            # Fine sampling is required for first-layer brim envelopes only.
            if active and layer == 1 and feature == 'Brim' and de > 0:
                angles = np.linspace(a, a+sweep, max(2, math.ceil(distance/.25)+1))
                pts = np.column_stack((cx+radius*np.cos(angles), cy+radius*np.sin(angles)))
        if active:
            if de <= 0: travel += distance
            if de > 0 and distance > 1e-6 and feature == 'Brim' and layer == 1:
                pts = pts+offset
                ids = np.argmin(np.linalg.norm(pts[:, None, :]-centers[None, :, :], axis=2), axis=1)
                assert len(set(ids)) == 1, 'Brims merge across parts; review separately'
                brims[int(ids[0])].append(pts)
                max_brim_width = max(max_brim_width, width)
            if de > 0 and distance > 1e-6 and owner and feature not in ('', 'Custom', 'Brim', 'Skirt'):
                seen.setdefault(owner, set()).add(layer)
                if owner != last_owner and last_owner is not None and layer == last_layer:
                    distances.append(travel)
                last_owner, last_layer, travel = owner, layer, 0.
        x, y, z = nx, ny, nz
    envelopes = [MultiPoint(np.concatenate(points)).convex_hull.buffer(max_brim_width/2) for points in brims]
    gap = min(a.distance(b) for a, b in itertools.combinations(envelopes, 2))
    return dict(layers=layer, per_object_layers={k: len(v) for k, v in seen.items()},
                minimum_actual_brim_gap_mm=gap,
                actual_brim_bounds=[list(p.bounds) for p in envelopes],
                within_layer_interobject_travel=dict(transitions=len(distances),
                    mean_mm=float(np.mean(distances)), max_mm=max(distances),
                    total_mm=sum(distances)))


def verify_compact(folder, printer='x1c'):
    package = folder/('slice/tesseract-bed4-'+printer+'.gcode.3mf')
    geometry = json.loads((folder/'package-validation.json').read_text())
    thermal = verify_package(package, preset_expectations(folder/'presets', plate='Textured PEI Plate'))
    assert thermal['package_sha256'] == geometry['package_sha256']
    with zipfile.ZipFile(package) as z:
        settings = json.loads(z.read('Metadata/project_settings.config'))
        code = z.read('Metadata/plate_1.gcode').decode()
    preset, model_id = {'x1c': ('Bambu Lab X1 Carbon 0.4 nozzle', 'BL-P001'),
                        'p1s': ('Bambu Lab P1S 0.4 nozzle', 'C12')}[printer]
    assert geometry['checked'] and geometry['single_plate'] and geometry['normal_objects'] == 4
    assert settings['printer_settings_id'] == preset and geometry['printer_model_id'] == model_id
    assert settings['print_sequence'] == 'by layer' and settings['reduce_crossing_wall'] == '1'
    packing = json.loads((folder/'packing.json').read_text())
    centers = np.array([i['center_xy_mm'] for i in packing['items']])
    offset = np.array([float(x) for x in settings['extruder_offset'][0].split('x')])
    audit = layer_review(code, centers, offset)
    assert audit['layers'] == thermal['layers_checked'] == 291
    assert len(audit['per_object_layers']) == 4 and set(audit['per_object_layers'].values()) == {291}
    assert audit['minimum_actual_brim_gap_mm'] > 1
    audit.update(checked=True, package_sha256=thermal['package_sha256'], thermal=thermal,
                 actual_print_sequence='by layer', wall_crossing_avoidance_enabled=True,
                 estimated_seconds=geometry['total_estimated_seconds'], filaments=geometry['filaments'],
                 limitation='Actual sliced paths/preset checks, not proof against curling, dynamic wobble or physical nozzle contact.')
    (folder/'compact-validation.json').write_text(json.dumps(audit, indent=2)+'\n')
    return audit


def main():
    spec = importlib.util.spec_from_file_location('sequential', HERE.parent/'sequential-retry/verify.py')
    checker = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(checker)
    summary = {}
    for name in ('four-struts-sequential', 'three-struts-bar-sequential'):
        folder = HERE/name
        checker.verify(folder, plan_path=folder/'plan.json', printer='x1c')
        summary[name] = json.loads((folder/'sequential-validation.json').read_text())
    audit = verify_compact(HERE/'four-struts-compact')
    summary['four-struts-compact'] = audit
    (HERE/'validation-summary.json').write_text(json.dumps(dict(layouts=summary, dispatched=False), indent=2)+'\n')
    print(json.dumps(dict(compact_checked=True, brim_gap_mm=audit['minimum_actual_brim_gap_mm'], travel=audit['within_layer_interobject_travel'])))


if __name__ == '__main__':
    main()
