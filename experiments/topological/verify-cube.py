#!/usr/bin/env python3
"""Audit cube checker meshes and actual planned screw coordinates.

First run `coscad check examples/assemble/plan/cube.assemble --keep-temp`
and `coscad plan examples/assemble/plan/cube.assemble`. No printer or network
access. World-space expectations are independent of the mating declarations.
"""
import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
import subprocess
import tempfile

from verify import VERTEX, mesh

ROOT = Path(__file__).resolve().parents[2]
EXAMPLE = ROOT / 'examples/assemble/plan'


def expected_parts():
    boxes = {}
    for ring, z in [('bottom', 0), ('top', 180)]:
        for side, lo, hi in [
            ('front', (0, 0, z), (200, 20, z+20)),
            ('back', (0, 180, z), (200, 200, z+20)),
            ('left', (0, 20, z), (20, 180, z+20)),
            ('right', (180, 20, z), (200, 180, z+20))]:
            boxes[f'{ring}_{side}'] = [lo, hi]
    for corner, x, y in [('fl', 0, 0), ('fr', 180, 0), ('bl', 0, 180), ('br', 180, 180)]:
        boxes[f'upright_{corner}'] = [(x, y, 20), (x+20, y+20, 180)]
    for ring, z in [('bottom', -3), ('top', 203)]:
        for corner, x, y in [('fl', 20, 20), ('fr', 180, 20), ('bl', 20, 180), ('br', 180, 180)]:
            boxes[f'br_{ring}_{corner}'] = [(x-20, y-20, z-3), (x+20, y+20, z+3)]
    for face, y in [('front', -3), ('back', 203)]:
        for height, z in [('low', 20), ('high', 180)]:
            for side, x in [('left', 20), ('right', 180)]:
                boxes[f'br_{face}_{height}_{side}'] = [(x-20, y-3, z-20), (x+20, y+3, z+20)]
    boxes['panel'] = [(200, 5, 5), (203, 195, 195)]
    return boxes


def expected_screws():
    rows = []
    inward = lambda n: 30 if n == 10 else 170
    for ring, z, face in [('bottom', 0, 'bot'), ('top', 200, 'top')]:
        for corner, x, y in [('fl', 10, 10), ('fr', 190, 10), ('bl', 10, 190), ('br', 190, 190)]:
            br = f'br_{ring}_{corner}'
            rows += [(br, f'{ring}_{"front" if y == 10 else "back"}', face, (inward(x), y, z)),
                     (br, f'{ring}_{"left" if x == 10 else "right"}', face, (x, inward(y), z))]
    for side, y, face in [('front', 0, 'fwd'), ('back', 200, 'bak')]:
        for height, z, ring in [('low', 10, 'bottom'), ('high', 190, 'top')]:
            for end, x in [('left', 10), ('right', 190)]:
                br = f'br_{side}_{height}_{end}'
                corner = ('f' if side == 'front' else 'b') + ('l' if end == 'left' else 'r')
                rows += [(br, f'{ring}_{side}', face, (inward(x), y, z)),
                         (br, f'upright_{corner}', face, (x, y, inward(z)))]
    for host, points in [
        ('bottom_right', [(200, 60, 10), (200, 140, 10)]),
        ('top_right', [(200, 60, 190), (200, 140, 190)]),
        ('upright_fr', [(200, 10, 100)]),
        ('upright_br', [(200, 190, 100)])]:
        rows += [('panel', host, 'rt', p) for p in points]
    return rows


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--check-dir', type=Path, default=Path(tempfile.gettempdir()) / 'coscad-check/cube')
    parser.add_argument('--check-log', type=Path, required=True)
    parser.add_argument('--output', type=Path, default=Path(__file__).with_name('cube-results.json'))
    args = parser.parse_args()
    plan = json.loads((EXAMPLE / 'cube_plan.json').read_text())
    parts = expected_parts()
    assert set(p['name'] for p in plan['parts']) == set(parts)
    assert not plan['design_errors'], plan['design_errors']
    placed = [n for s in plan['steps'] for n in s['parts']]
    assert Counter(placed) == Counter(parts.keys()), 'missing or repeated instruction placements'
    physical_log = args.check_log.read_text()
    assert '29 placed instances:' in physical_log
    assert 'check: 0 overlaps, 0 clearance warnings' in physical_log
    measurements = []
    with tempfile.TemporaryDirectory(prefix='coscad-cube-audit-') as tmp:
        for name, expected in parts.items():
            vertices = [tuple(map(float, v)) for v in VERTEX.findall((args.check_dir / f'chk_{name}.stl').read_text())]
            assert len(vertices) % 3 == 0
            triangles = [vertices[i:i+3] for i in range(0, len(vertices), 3)]
            real = [tri for tri in triangles if min(v[0] for v in tri) < 90000]
            path = Path(tmp) / f'{name}.stl'
            path.write_text('\n'.join('vertex ' + ' '.join(map(str, v)) for tri in real for v in tri))
            measured = mesh(path)
            assert measured['components'] == 1, (name, measured)
            assert max(abs(a-b) for row, want in zip(measured['bounds'], expected)
                       for a, b in zip(row, want)) < 1e-6, (name, measured['bounds'], expected)
            measurements.append({'name': name, 'expected_bounds': expected, **measured})
    actual = [f for s in plan['steps'] for f in s['fasteners']]
    expected = expected_screws()
    assert len(actual) == len(expected) == 38
    unmatched = list(expected)
    for f in actual:
        matches = [i for i, (clamped, host, face, point) in enumerate(unmatched)
                   if (f['clamped'], f['host'], f['face']) == (clamped, host, face)
                   and max(abs(a-b) for a, b in zip(f['position_mm'], point)) < 1e-6]
        assert len(matches) == 1, f
        unmatched.pop(matches[0])
    assert not unmatched
    source_files = [EXAMPLE / f for f in ['cube.assemble', 'flat90.coscad', 'rail200.coscad', 'rail160.coscad', 'panel190.coscad']]
    source_files += [ROOT / 'src/Coscad/Plan.hs', ROOT / 'test/Spec.hs']
    result = {
        'git_head': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
        'dirty_worktree': bool(subprocess.check_output(['git', 'status', '--porcelain'], cwd=ROOT, text=True).strip()),
        'source_sha256': {str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest() for p in source_files},
        'summary': {'parts': 29, 'brackets': 16, 'rails': 12, 'panel': 1,
                    'manifold_single_component_parts': len(measurements),
                    'verified_world_screw_positions': len(actual), 'bracket_screws': 32, 'panel_screws': 6,
                    'overlaps': 0, 'clearance_warnings': 0, 'plan_design_errors': 0,
                    'instruction_steps': len(plan['steps']), 'flips': sum(s['flip'] for s in plan['steps'])},
        'parts': measurements, 'screws': actual,
        'scope': 'Nominal 200 mm cube; mesh bounds, manifold bodies, exact overlap checker and planned host-face screw coordinates. Bore material and rotation equivalence are tested separately by stack test.'
    }
    args.output.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result['summary'], indent=2))
    print('Wrote', args.output)


if __name__ == '__main__':
    main()
