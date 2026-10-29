"""Compare pilot solids to originals, then test anchor behaviour after resizing.

Meshes come from the native compiler (`coscad stl`); booleans, containment and
material probes use manifold3d (the Manifold kernel the compiler links). No
OpenSCAD or BOSL2 is needed. Requires numpy and manifold3d.
No printer, slicer, network, or original-source writes. Only the report persists.
"""
import argparse
from collections import Counter, defaultdict
from contextlib import contextmanager
import hashlib
import importlib.metadata
import json
import math
from pathlib import Path
import re
import shutil
import subprocess
import tempfile

import manifold3d as md
import numpy as np

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
TRANSLATE = re.compile(r'[χψζ]|\|>\s*(?:move|x|y|z)\b|\bTranslate\.[xyz]\b')
RELATE = re.compile(r'[⌖⋈⚓]|\|>\s*(?:at|on|cutat|anchor|datum|place)\b')
VERTEX = re.compile(r'vertex\s+([-\d.eE+]+)\s+([-\d.eE+]+)\s+([-\d.eE+]+)')


@contextmanager
def scratch_directory():
    path = Path(tempfile.mkdtemp(prefix='coscad-topology-'))
    try:
        yield path
    except BaseException:
        print('Failure artifacts retained in', path, flush=True)
        raise
    else:
        shutil.rmtree(path)


def code(source):
    return '\n'.join(line.split('//', 1)[0] for line in source.splitlines())


def inventory():
    rows = []
    for directory in ('examples', 'examples-next', 'models'):
        for path in sorted((ROOT / directory).rglob('*.coscad')):
            if 'archive' in path.parts:
                continue
            source = code(path.read_text())
            translations = len(TRANSLATE.findall(source))
            relations = len(RELATE.findall(source))
            rows.append({'file': str(path.relative_to(ROOT)),
                         'coordinate_placements': translations,
                         'anchor_operations': relations,
                         'style': 'mixed' if translations and relations else
                                  'coordinates' if translations else
                                  'anchors' if relations else 'no placement'})
    return {'method': 'Lexical inventory; stable datums require semantic review.',
            'counts': dict(Counter(row['style'] for row in rows)), 'files': rows}


def mesh(path):
    vertices = [tuple(map(float, v)) for v in VERTEX.findall(path.read_text())]
    assert vertices and len(vertices) % 3 == 0, f'empty or malformed mesh: {path.name}'
    triangles = [vertices[i:i + 3] for i in range(0, len(vertices), 3)]
    edges = defaultdict(list)
    volume = 0.0
    for i, (a, b, c) in enumerate(triangles):
        for p, q in ((a, b), (b, c), (c, a)):
            edges[tuple(sorted((p, q)))].append(i)
        cross = (b[1]*c[2]-b[2]*c[1], b[2]*c[0]-b[0]*c[2], b[0]*c[1]-b[1]*c[0])
        volume += sum(a[j]*cross[j] for j in range(3)) / 6
    assert all(len(ids) == 2 for ids in edges.values()), f'non-manifold: {path.name}'
    adjacent = defaultdict(set)
    for a, b in edges.values():
        adjacent[a].add(b)
        adjacent[b].add(a)
    remaining, components = set(range(len(triangles))), 0
    while remaining:
        todo = [remaining.pop()]
        components += 1
        while todo:
            for i in adjacent[todo.pop()] & remaining:
                remaining.remove(i)
                todo.append(i)
    return {'triangles': len(triangles), 'closed_manifold': True,
            'components': components, 'volume_mm3': round(abs(volume), 6),
            'bounds': [[min(v[i] for v in vertices) for i in range(3)],
                       [max(v[i] for v in vertices) for i in range(3)]]}


def solid(path):
    """Load an STL as a Manifold, welding exactly coincident vertices."""
    index, triangles = {}, []
    vertices = [tuple(map(float, v)) for v in VERTEX.findall(path.read_text())]
    for i in range(0, len(vertices), 3):
        triangles.append([index.setdefault(v, len(index)) for v in vertices[i:i + 3]])
    result = md.Manifold(md.Mesh64(vert_properties=np.array(list(index), dtype=np.float64),
                                   tri_verts=np.array(triangles, dtype=np.uint32)))
    assert result.status() == md.Error.NoError, (path.name, result.status())
    return result


def extent(shape):
    lo_hi = shape.bounding_box()
    return min(lo_hi[i + 3] - lo_hi[i] for i in range(3))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--coscad', type=Path, required=True)
    # results.json and native-results.json are the OpenSCAD-era records; keep them.
    parser.add_argument('--output', type=Path, default=HERE / 'manifold-results.json')
    parser.add_argument('--case', action='append', help='Run only these pilot names')
    args = parser.parse_args()
    binary = str(args.coscad.resolve(strict=True))

    def render(source, output):
        """Render a .coscad source natively; return its mesh summary and solid."""
        result = subprocess.run([binary, 'stl', str(source), '-o', str(output), '+RTS', '-N1', '-RTS'],
                                capture_output=True, text=True, timeout=90)
        assert result.returncode == 0 and output.exists(), result.stdout + result.stderr
        assert not re.search(r'^(WARNING|ERROR|Warning|Error)', result.stderr, re.M), result.stderr
        return mesh(output), solid(output)

    def residual(shape, numeric_residual=False):
        if shape.is_empty():
            return {'empty': True, 'residual_volume_mm3': 0}
        # Anchor placement can change the numerical evaluation order of a
        # hull. A residual is accepted only if both volume and thickness
        # are negligible. Actual cavity probes still require empty output.
        # Thickness is the bounding-box minimum extent or, for slanted
        # slivers, the mean thickness 2 * volume / surface area.
        volume, thickness = abs(shape.volume()), extent(shape)
        mean = 2 * volume / shape.surface_area()
        assert numeric_residual and volume <= 0.001 and min(thickness, mean) <= 0.00001, (volume, thickness, mean)
        return {'empty': False, 'residual_volume_mm3': round(volume, 6),
                'minimum_extent_mm': thickness, 'mean_thickness_mm': mean,
                'within_numeric_tolerance': True}

    report = {'source_head': subprocess.check_output(['git', 'rev-parse', 'HEAD'],
               cwd=ROOT, text=True).strip(),
              'source_worktree_dirty': bool(subprocess.check_output(['git', 'status', '--porcelain'], cwd=ROOT)),
              'coscad_binary_sha256': hashlib.sha256(Path(binary).read_bytes()).hexdigest(),
              'geometry': {'meshes': 'coscad stl (native Manifold kernel)',
                           'booleans': 'manifold3d ' + importlib.metadata.version('manifold3d')},
              'inventory': inventory(), 'equivalence': [], 'resizing': []}
    with scratch_directory() as scratch:
        for case in json.loads((HERE / 'cases.json').read_text()):
            name = case['name']
            if args.case and name not in args.case:
                continue
            pilot = HERE / (name + '.coscad')
            assert not TRANSLATE.search(code(pilot.read_text())), name
            (before, old), (after, new) = (render(ROOT / case['original'], scratch / (name + '-old.stl')),
                                           render(pilot, scratch / (name + '-new.stl')))
            assert before['components'] == after['components'] == 1, name
            assert all(math.isclose(a, b, abs_tol=1e-5) for x, y in
                       zip(before['bounds'], after['bounds']) for a, b in zip(x, y)), name
            if 'containment_tolerance_mm' in case:
                # Dispersed roundoff can make a raw symmetric difference
                # non-manifold. Prove that each complete solid is contained
                # in the other's epsilon expansion instead. These residuals
                # must be genuinely empty and warning-free in BOTH directions.
                epsilon = case['containment_tolerance_mm']
                assert 0 < epsilon <= 0.00001
                grow = md.Manifold.cube([2 * epsilon] * 3, True)
                for a, b in [(old, new), (new, old)]:
                    assert (a - b.minkowski_sum(grow)).is_empty(), name
                report['equivalence'].append({**case, 'original_mesh': before,
                    'pilot_mesh': after, 'bidirectional_containment': {
                        'tolerance_per_axis_mm': epsilon, 'both_residuals_empty': True}})
                print(name + ': closed, one component, same bounds; bidirectional containment at ' +
                      str(epsilon) + ' mm', flush=True)
                continue
            diff = residual((old - new) + (new - old), numeric_residual=True)
            report['equivalence'].append({**case, 'original_mesh': before,
                                          'pilot_mesh': after, 'symmetric_difference': diff})
            print(name + ': closed, one component, same bounds; residual ' +
                  str(diff['residual_volume_mm3']) + ' mm3', flush=True)

        # Small point cubes check actual material on both sides of each intended
        # floor, or at a moved hole. Bounds alone cannot prove those relationships.
        # $fn=50 has vertices on +/-X, but its +/-Y support is r*cos(pi/50).
        circle_y = math.cos(math.pi / 50)
        variants = [
            ('coin-weight-cylinder', {'height': 70}, [(2, 0, 70)],
             [((0, 0, 44.1), False), ((0, 0, 43.9), True)]),
            ('bow-center', {'body_height': 18, 'cheek': 6.75}, [(1, -21, 21), (2, -9, 9)],
             [((0, 0, 7.9), False), ((0, 0, 7.7), True),
              ((42, 0, 1.9), False), ((42, 0, 1.7), True),
              ((42, 8, 0), False), ((-42, -8, 0), False)]),
            ('flat90', {'span': 50}, [(0, -25, 25), (1, -25, 25)],
             [((15, -15, 0), False), ((-15, 15, 0), False), ((10, -10, 0), True)]),
            ('panel_corner', {'width': 250, 'depth': 230}, [(0, -125, 125), (1, -115, 115)],
             [((-105, 0, 0), False), ((95, 0, 0), False),
              ((0, -95, 0), False), ((0, 85, 0), False), ((-85, 0, 0), True)]),
            ('orbit-axle', {'foot_height': 10}, [(2, 0, 23)],
             [((0, 0, 5.5), False), ((0, 0, 5.7), True)]),
            ('orbit-satellite', {'disc_height': 6}, [(2, 0, 12.8)], []),
            ('native-orbit-rotor', {'half_span': 30, 'thickness': 6, 'counterweight_radius': 9},
             [(0, -39, 35), (1, -10.5 * circle_y, 10.5 * circle_y), (2, 0, 6)],
             [((0, 0, 3), False), ((30, 0, 3), False), ((24, 0, 3), True), ((-30, 7, 3), True)]),
            ('native-egg-box', {'columns': 3, 'rows': 1},
             [(0, 0, 189.6), (1, 4 * (1 - circle_y), 64.8 - 4 * (1 - circle_y)), (2, 0, 78)],
             [((32.4, 32.4, 3.1), False), ((32.4, 32.4, 2.9), True),
              ((94.8, 32.4, 40), False), ((157.2, 32.4, 40), False),
              ((63.6, 32.4, 40), True), ((1.2, 32.4, 40), True)]),
        ]
        for name, replacements, bounds, points in variants:
            if args.case and name not in args.case:
                continue
            source = (HERE / (name + '.coscad')).read_text()
            for key, value in replacements.items():
                source, n = re.subn(r'^' + re.escape(key) + r' = [^\n]+$',
                                    key + ' = ' + str(value), source, flags=re.M)
                assert n == 1, (name, key)
            changed = scratch / (name + '-resized.coscad')
            changed.write_text(source)
            data, part = render(changed, changed.with_suffix('.stl'))
            assert data['components'] == 1, name
            for axis, lo, hi in bounds:
                assert math.isclose(data['bounds'][0][axis], lo, abs_tol=1e-5), name
                assert math.isclose(data['bounds'][1][axis], hi, abs_tol=1e-5), name
            for point, material in points:
                common = part ^ md.Manifold.cube([0.1] * 3, True).translate(list(point))
                if material:
                    assert common.volume() >= 0.0009, (name, point, common.volume())
                else:
                    assert common.is_empty(), (name, point, common.volume())
            report['resizing'].append({'name': name, 'bindings': replacements,
                'mesh': data, 'material_probes_passed': len(points)})
            print(name + ': resizing bounds and ' + str(len(points)) + ' material probes pass', flush=True)

        if not args.case:
            plans = []
            for kind in ('original', 'topological', 'native'):
                directory = scratch / kind
                directory.mkdir()
                assembly = directory / 'orbit-tower.assemble'
                if kind == 'original':
                    shutil.copyfile(ROOT / 'models/orbit-tower/orbit-tower.assemble', assembly)
                    for part in ('base', 'axle', 'cap', 'satellite', 'rotor'):
                        shutil.copyfile(ROOT / f'models/orbit-tower/{part}.coscad', directory / (part + '.coscad'))
                else:
                    shutil.copyfile(HERE / 'orbit-tower.assemble', assembly)
                    for part in ('base', 'axle', 'cap', 'satellite', 'rotor'):
                        shutil.copyfile(HERE / f'orbit-{part}.coscad', directory / f'orbit-{part}.coscad')
                    if kind == 'native':
                        shutil.copyfile(HERE / 'native-orbit-rotor.coscad', directory / 'orbit-rotor.coscad')
                result = subprocess.run([binary, 'plan', str(assembly), '+RTS', '-N1', '-RTS'],
                                        capture_output=True, text=True, timeout=60)
                assert result.returncode == 0, result.stdout + result.stderr
                plan = json.loads(assembly.with_name('orbit-tower_plan.json').read_text())
                plan.pop('source')
                plans.append(plan)
            assert plans[0] == plans[1] == plans[2], 'Orbit Tower plan changed after replacing its parts'
            plan = plans[0]
            report['assembly'] = {'name': 'Orbit Tower', 'plan_equal_except_source_path': True,
                'parts': sum(len(s['parts']) for s in plan['steps']),
                'fasteners': sum(len(s['fasteners']) for s in plan['steps']),
                'steps': len(plan['steps']), 'design_errors': plan['design_errors']}
            print('Orbit Tower: instruction plan unchanged for both pilot sets', flush=True)
    args.output.write_text(json.dumps(report, indent=2) + '\n')
    print('Saved', args.output)


if __name__ == '__main__':
    main()
