"""Verify packed bodies, loose-fit joints and a full rotor-motion envelope.

Run after `coscad next orbit-tower.assemble`. manifold3d (the Manifold kernel
the native compiler links) is the boolean engine; no OpenSCAD is needed.
Requires numpy and manifold3d. `--output` writes the report elsewhere
(default validation.json); probe meshes are written beside the report.
"""
import argparse
from collections import Counter, defaultdict
import json
import math
from pathlib import Path
import re

import manifold3d as md
import numpy as np

ROOT = Path(__file__).resolve().parent
NUMBER = r'([-\d.eE+]+)'
SEGMENTS = 50


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


def solid(path):
    """Load an STL as a Manifold, welding exactly coincident vertices."""
    index, tris = {}, []
    for tri in read_mesh(path):
        tris.append([index.setdefault(v, len(index)) for v in tri])
    mesh = md.Mesh64(vert_properties=np.array(list(index), dtype=np.float64),
                     tri_verts=np.array(tris, dtype=np.uint32))
    result = md.Manifold(mesh)
    assert result.status() == md.Error.NoError, (path, result.status())
    return result


def write_stl(shape, path):
    mesh = shape.to_mesh64()
    verts = np.asarray(mesh.vert_properties)[:, :3]
    lines = ['solid probe']
    for tri in np.asarray(mesh.tri_verts):
        a, b, c = verts[tri]
        n = np.cross(b - a, c - a)
        n = n / (np.linalg.norm(n) or 1)
        lines += [f'  facet normal {n[0]} {n[1]} {n[2]}', '    outer loop']
        lines += [f'      vertex {p[0]} {p[1]} {p[2]}' for p in (a, b, c)]
        lines += ['    endloop', '  endfacet']
    path.write_text('\n'.join(lines + ['endsolid probe']) + '\n')


def cylinder(r, h, n=SEGMENTS):
    return md.Manifold.cylinder(h, r, r, n)


def probe(out_dir, name, shape, empty):
    output = out_dir / f'probe-{name}.stl'
    output.unlink(missing_ok=True)
    if empty:
        assert shape.is_empty(), f'{name}: unexpected interference {shape.volume()} mm^3'
        return 'zero interference'
    volume = shape.volume()
    assert volume > 0.01, f'{name}: expected interference'
    write_stl(shape, output)
    return volume


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--output', type=Path, default=ROOT / 'validation.json')
    args = ap.parse_args()
    out_dir = args.output.resolve().parent
    out_dir.mkdir(parents=True, exist_ok=True)

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

    def variant_solid(part):
        # Variant ids follow instance names (axle1_bot, rotor_bot or rotor1_bot).
        mesh = next(v['mesh'] for v in manifest['variants'] if v['id'].startswith(part))
        return solid(ROOT / Path(mesh).name)

    axle = variant_solid('axle')
    rotor = variant_solid('rotor')
    checks = {}
    for name, target, nominal, shift in [
        ('square_socket', axle, md.Manifold.cube([6, 6, 5.2]).translate([-3, -3, -0.1]), 0.5),
        ('journal', rotor, cylinder(4.8, 4.7).translate([0, 0, -0.1]), 0.6),
        ('satellite_socket', rotor, cylinder(2.4, 4.7).translate([24, 0, -0.1]), 0.5),
    ]:
        checks[name] = probe(out_dir, name, target ^ nominal, True)
        checks[name + '_shifted'] = probe(out_dir, name + '_shifted',
                                          target ^ nominal.translate([shift, 0, 0]), False)

    # An oversized radial sweep contains every arm and satellite position.
    # Lift by 0.05 mm to exclude the intentional bearing-face contact plane.
    # An outer radius of 31 and reduced inner radii cover tessellation too.
    axles = axle + axle.translate([0, 0, 15])
    sweep = ((cylinder(31, 4.5, 100) - cylinder(5.15, 4.7, 100).translate([0, 0, -0.1]))
             .translate([0, 0, 7.05])
             + (cylinder(31, 7, 100) - cylinder(17.4, 7.2, 100).translate([0, 0, -0.1]))
             .translate([0, 0, 11.55]))
    checks['full_rotation_envelope'] = probe(out_dir, 'full_rotation_envelope', axles ^ sweep, True)
    report = {'one_bed': True, 'physical_parts': 20, 'bed': bed,
              'variants': variants, 'fits_and_motion': checks,
              'minimum_vertical_gap_between_satellite_and_next_arm_mm': 3.5}
    args.output.write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
