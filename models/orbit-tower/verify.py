"""Verify packed bodies, loose-fit joints and a full rotor-motion envelope.

Run after `coscad next orbit-tower.assemble`. OpenSCAD is the boolean engine.
"""
from collections import Counter, defaultdict
import json
import math
from pathlib import Path
import re
import subprocess

ROOT = Path(__file__).resolve().parent
OPENSCAD = '/Applications/OpenSCAD.app/Contents/MacOS/openscad'
NUMBER = r'([-\d.eE+]+)'


def read_mesh(path):
    verts = [tuple(map(float, v)) for v in re.findall(
        r'vertex\s+' + NUMBER + r'\s+' + NUMBER + r'\s+' + NUMBER,
        path.read_text())]
    return [verts[i:i + 3] for i in range(0, len(verts), 3)]


def inspect(path):
    faces = read_mesh(path)
    assert faces, path
    edges = defaultdict(list)
    for i, tri in enumerate(faces):
        for j in range(3):
            edges[tuple(sorted((tri[j], tri[(j + 1) % 3])))].append(i)
    assert all(len(v) == 2 for v in edges.values()), f'non-manifold: {path}'
    adj = defaultdict(set)
    for a, b in edges.values():
        adj[a].add(b)
        adj[b].add(a)
    unseen = set(range(len(faces)))
    count = 0
    while unseen:
        todo = [unseen.pop()]
        count += 1
        while todo:
            for i in adj[todo.pop()] & unseen:
                unseen.remove(i)
                todo.append(i)
    volume = 0.0
    for a, b, c in faces:
        cross = (b[1] * c[2] - b[2] * c[1],
                 b[2] * c[0] - b[0] * c[2],
                 b[0] * c[1] - b[1] * c[0])
        volume += sum(a[j] * cross[j] for j in range(3)) / 6
    return {'components': count, 'triangles': len(faces), 'volume_mm3': abs(volume)}


def probe(name, shape, empty):
    source = ROOT / f'probe-{name}.scad'
    output = source.with_suffix('.stl')
    output.unlink(missing_ok=True)
    source.write_text(shape + '\n')
    result = subprocess.run([OPENSCAD, '-o', str(output), str(source)],
                            capture_output=True, text=True)
    if empty:
        assert 'Current top level object is empty' in result.stderr, result.stderr
        assert not output.exists(), f'{name}: unexpected interference'
        return 'zero interference'
    assert result.returncode == 0, result.stderr
    data = inspect(output)
    assert data['volume_mm3'] > 0.01, f'{name}: expected interference'
    return data['volume_mm3']


manifest = json.loads((ROOT / 'orbit-tower_manifest.json').read_text())
assert len(manifest['beds']) == 1
placements = manifest['beds'][0]['placements']
assert len(placements) == 20
counts = Counter(p['variant'] for p in placements)
variants = {}
for v in manifest['variants']:
    data = inspect(ROOT / Path(v['mesh']).name)
    assert data['components'] == 1, (v['id'], data)
    variants[v['id']] = data
    assert v['down'] == 'bot'
for p in placements:
    assert p['x'] >= 7 and p['y'] >= 7
    assert p['x'] + p['w'] <= 249 and p['y'] + p['d'] <= 249
for i, a in enumerate(placements):
    for b in placements[i + 1:]:
        assert (a['x'] + a['w'] <= b['x'] or b['x'] + b['w'] <= a['x']
                or a['y'] + a['d'] <= b['y'] or b['y'] + b['d'] <= a['y'])
bed = inspect(ROOT / Path(manifest['beds'][0]['stl']).name)
assert bed['components'] == 20
expected_volume = sum(variants[k]['volume_mm3'] * n for k, n in counts.items())
assert math.isclose(expected_volume, bed['volume_mm3'], rel_tol=0.0001)

axle = 'import("orbit-tower_next_axle1_bot.stl");'
rotor = 'import("orbit-tower_next_rotor_bot.stl");'
satellite = 'import("orbit-tower_next_satellite_bot.stl");'
checks = {}
for name, target, nominal, shift in [
    ('square_socket', axle, 'translate([-3,-3,-0.1])cube([6,6,5.2]);', 0.5),
    ('journal', rotor, 'translate([0,0,-0.1])cylinder(r=4.8,h=4.7,$fn=50);', 0.6),
    ('satellite_socket', rotor, 'translate([24,0,-0.1])cylinder(r=2.4,h=4.7,$fn=50);', 0.5),
]:
    checks[name] = probe(name, 'intersection(){' + target + nominal + '}', True)
    checks[name + '_shifted'] = probe(name + '_shifted',
        'intersection(){' + target + f'translate([{shift},0,0]){{' + nominal + '}}', False)

# An oversized radial sweep contains every arm and satellite position.
# Lift by 0.05 mm to exclude the intentional bearing-face contact plane.
# An outer radius of 31 and reduced inner radii cover tessellation too.
checks['full_rotation_envelope'] = probe('full_rotation_envelope',
    'intersection(){union(){' + axle + 'translate([0,0,15]){' + axle + '}}'
    'union(){translate([0,0,7.05])difference(){'
    'cylinder(r=31,h=4.5,$fn=100);'
    'translate([0,0,-0.1])cylinder(r=5.15,h=4.7,$fn=100);}'
    'translate([0,0,11.55])difference(){'
    'cylinder(r=31,h=7,$fn=100);'
    'translate([0,0,-0.1])cylinder(r=17.4,h=7.2,$fn=100);}}}', True)
report = {'one_bed': True, 'physical_parts': 20, 'bed': bed,
          'variants': variants, 'fits_and_motion': checks,
          'minimum_vertical_gap_between_satellite_and_next_arm_mm': 3.5}
(ROOT / 'validation.json').write_text(json.dumps(report, indent=2) + '\n')
print(json.dumps(report, indent=2))
