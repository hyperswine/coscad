#!/usr/bin/env python3
"""Verify actual object order, height resets and conservative return clearances."""
from pathlib import Path
import json
import math
import re
import sys
import zipfile

import numpy as np
import trimesh
from shapely.geometry import MultiPoint, Point

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
sys.path.insert(0, str(ROOT/'scripts'))
from bambu_preflight import preset_expectations, verify_package


def verify(folder, *, plan_path=None, printer='p1s'):
    packing = json.loads((folder/'packing.json').read_text())
    parts = json.loads((folder/'project-parts.json').read_text())['parts']
    plan = json.loads((plan_path if plan_path else HERE/'plan.json').read_text())
    clearance = plan['unmodified_machine_clearance']
    package = folder/('slice/tesseract-bed4-'+printer+'.gcode.3mf')
    geometry = json.loads((folder/'package-validation.json').read_text())
    intent = preset_expectations(folder/'presets', plate='Textured PEI Plate')
    # The shared checker intentionally supports homogeneous temperatures only.
    assert intent['bed_initial_c'] == intent['bed_c'] == 55
    assert intent['nozzle_initial_c'] == intent['nozzle_c'] == 220
    thermal = verify_package(package, intent)
    assert thermal['package_sha256'] == geometry['package_sha256']
    with zipfile.ZipFile(package) as z:
        settings = json.loads(z.read('Metadata/project_settings.config'))
        code = z.read('Metadata/plate_1.gcode').decode()
    assert settings['print_sequence'] == 'by object', 'Per-object mode not exported'
    preset, model_id = {'p1s': ('Bambu Lab P1S 0.4 nozzle', 'C12'),
                        'x1c': ('Bambu Lab X1 Carbon 0.4 nozzle', 'BL-P001')}[printer]
    assert settings['printer_settings_id'] == preset and geometry['printer_model_id'] == model_id
    machine = json.loads((folder/'presets/machine.json').read_text())
    nozzle_offset = np.array([float(v) for v in machine['extruder_offset'][0].split('x')])
    assert nozzle_offset.shape == (2,) and np.all(np.isfinite(nozzle_offset))
    input_only = []
    for key, value in clearance.items():
        assert float(machine[key]) == value, ('Changed input machine clearance', key)
        # Current Studio omits this legacy radius from the exported config;
        # all active head/rod/lid limits must still survive the export.
        if key == 'extruder_clearance_radius' and key not in settings:
            input_only.append(key)
        else:
            assert float(settings[key]) == value, ('Changed exported clearance', key)
    for key, value in [('slow_down_layer_time', 12), ('slow_down_min_speed', 5),
                       ('slow_down_for_layer_cooling', 1)]:
        actual = settings[key]
        assert float(actual[0] if isinstance(actual, list) else actual) == value

    expected = [str(p['parent']) for p in parts]
    bounds = {str(p['parent']): np.array(p['bounds']) for p in parts}
    footprints = {}
    for item, part in zip(packing['items'], parts):
        mesh = trimesh.load_mesh(folder/item['file'])
        footprints[str(part['parent'])] = MultiPoint(mesh.vertices[:, :2]).convex_hull

    # Keep enough actual modal motion state to inspect the full transition,
    # including custom travel, wipes and end G-code, not just extrusion endpoints.
    x = y = z = e = 0.
    absolute, relative_e, active = True, False, False
    owner = expected[0] if len(expected) == 1 else None
    feature = ''
    groups, motion = [], []
    observed = {k: dict(layer_z=[], last_line=-1, segments=0) for k in expected}
    layer_number = 0
    global_z = None
    seen_layer = set()
    for line_number, line in enumerate(code.splitlines(), 1):
        if line == '; CHANGE_LAYER':
            active = True
            layer_number += 1
        if line.startswith('; Z_HEIGHT:'):
            global_z = float(line.split(':', 1)[1])
        if line.startswith('; FEATURE:'):
            feature = line.split(':', 1)[1].strip()
        match = re.match(r'; start printing object, unique label id: (\d+)', line)
        if match:
            owner = match[1]
            assert owner in observed
            if not groups or groups[-1] != owner:
                groups.append(owner)
        if line.startswith('; stop printing object') and len(expected) > 1:
            owner = None
        if line == '; MACHINE_END_GCODE_START':
            active = False
        raw = line.split(';', 1)[0].strip()
        if not raw:
            continue
        op = raw.split()[0]
        words = {k: float(v) for k, v in re.findall(r'([XYZIJERP])(-?\d*\.?\d+)', raw)}
        assert op not in ('G18', 'G19'), 'Unsupported arc plane'
        if op == 'G90': absolute = True
        if op == 'G91': absolute = False
        if op == 'M82': relative_e = False
        if op == 'M83': relative_e = True
        if op == 'G92':
            assert not any(k in words for k in ('X', 'Y', 'Z')), 'Unsupported coordinate reset'
            if 'E' in words: e = words['E']
            continue
        if op not in ('G0', 'G1', 'G2', 'G3'):
            continue
        if op in ('G2', 'G3'):
            assert 'R' not in words and ('I' in words or 'J' in words), 'Unsupported arc center'
            assert words.get('P', 1) == 1, 'Unsupported multi-turn arc'
        nx = words.get('X', x) if absolute else x+words.get('X', 0)
        ny = words.get('Y', y) if absolute else y+words.get('Y', 0)
        nz = words.get('Z', z) if absolute else z+words.get('Z', 0)
        de = words.get('E', 0) if relative_e else words.get('E', e)-e
        if 'E' in words: e += de
        motion.append((line_number, op, (x, y, z), (nx, ny, nz), words.get('I', 0), words.get('J', 0)))
        if active and owner and de > 0 and feature not in ('', 'Custom') and (
                math.hypot(nx-x, ny-y) > 1e-6 or op in ('G2', 'G3')):
            row = observed[owner]
            row['last_line'] = line_number
            row['segments'] += 1
            key = (owner, layer_number)
            if key not in seen_layer:
                assert global_z is not None
                row['layer_z'].append(global_z)
                seen_layer.add(key)
        x, y, z = nx, ny, nz
    if len(expected) == 1 and not groups:
        groups = expected.copy()
    assert groups == expected, ('Objects must complete once in the expected order', groups, expected)
    for owner, row in observed.items():
        zs = row['layer_z']
        assert row['segments'] > 0 and zs and abs(zs[0]-.2) < 1e-5
        assert all(abs(b-a-.2) < 1e-5 for a, b in zip(zs, zs[1:]))
        assert abs(zs[-1]-bounds[owner][1, 2]) <= .201
    assert sum(len(r['layer_z']) for r in observed.values()) == thermal['layers_checked']

    checks, violations = 0, []
    for owner in expected[:-1]:
        previous_height = bounds[owner][1, 2]
        assert previous_height+5 < clearance['extruder_clearance_height_to_lid']
        head_zone = footprints[owner].buffer(clearance['extruder_clearance_max_radius']+5)
        rod_min = bounds[owner][0, 1]-clearance['extruder_clearance_dist_to_rod']-5
        rod_max = bounds[owner][1, 1]+clearance['extruder_clearance_dist_to_rod']+5
        released = False
        for line, op, a, b, di, dj in motion:
            if line <= observed[owner]['last_line']:
                continue
            a, b = np.array(a), np.array(b)
            if op in ('G2', 'G3') and (di or dj):
                center = a[:2]+[di, dj]
                radius = math.hypot(di, dj)
                assert abs(np.linalg.norm(b[:2]-center)-radius) < .05, 'Inconsistent arc radius'
                start = math.atan2(a[1]-center[1], a[0]-center[0])
                end = math.atan2(b[1]-center[1], b[0]-center[0])
                sweep = (end-start) % (2*math.pi) if op == 'G3' else -((start-end) % (2*math.pi))
                if abs(sweep) < 1e-8: sweep = 2*math.pi if op == 'G3' else -2*math.pi
                count = max(2, math.ceil(math.hypot(abs(sweep)*radius, b[2]-a[2])/.5)+1)
                angles = np.linspace(start, start+sweep, count)
                pts = np.column_stack((center[0]+radius*np.cos(angles), center[1]+radius*np.sin(angles), np.linspace(a[2], b[2], count)))
            else:
                count = max(2, math.ceil(np.linalg.norm(b-a)/.5)+1)
                pts = np.linspace(a, b, count)
            for px, py, pz in pts:
                px += nozzle_offset[0]
                py += nozzle_offset[1]
                checks += 1
                near = head_zone.covers(Point(px, py))
                if not released and (not near or pz > previous_height+5):
                    released = True
                if not released:
                    continue  # Intended wipe/departure at the just-finished top.
                head_risk = near and pz < previous_height+.5
                rod_risk = rod_min <= py <= rod_max and pz+clearance['extruder_clearance_height_to_rod'] < previous_height+.5
                if head_risk or rod_risk:
                    violations.append(dict(line=line, xyz=[float(px), float(py), float(pz)], head=head_risk, rod=rod_risk))
                    break
        assert released
    assert not violations, violations[:5]
    report = dict(checked=True, package_sha256=thermal['package_sha256'],
                  actual_print_sequence=settings['print_sequence'],
                  object_order=[p['label'] for p in parts],
                  per_object={p['label']: dict(layers=len(observed[str(p['parent'])]['layer_z']),
                                              last_z_mm=observed[str(p['parent'])]['layer_z'][-1]) for p in parts},
                  thermal=thermal, transition_samples_0_5mm=checks,
                  conservative_return_collisions=violations,
                  legacy_input_only_clearance_keys=input_only,
                  slicer_warnings=geometry['slicer_warnings'],
                  estimated_seconds=geometry['total_estimated_seconds'],
                  filaments=geometry['filaments'],
                  limitation='Preset limits, slicer validation and sampled conservative motion envelopes; not measured hardware clearance or a physical print-quality guarantee.')
    (folder/'sequential-validation.json').write_text(json.dumps(report, indent=2)+'\n')
    print(json.dumps(dict(layout=folder.name, checked=True, order=report['object_order'],
                          samples=checks, estimated_seconds=report['estimated_seconds'])), flush=True)


if __name__ == '__main__':
    for name in ('mixed', 'bar-only'):
        verify(HERE/name)
    path = HERE/'plan.json'
    plan = json.loads(path.read_text())
    plan['verified'] = True
    plan['validation_reports'] = [name+'/sequential-validation.json' for name in ('mixed', 'bar-only')]
    plan['limitation'] = 'Slicer and sampled G-code clearance checks passed; physical clearance, adhesion and print quality remain untested.'
    path.write_text(json.dumps(plan, indent=2)+'\n')
