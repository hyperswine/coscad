#!/usr/bin/env python3
"""Check real panel meshes, slot capture, insertion paths and assembly order.

Temporary analysis dependencies: numpy==2.5.4 trimesh==5.2.0
scipy==1.18.1 manifold3d==3.5.4. CoScad itself uses its pinned kernel.
No printer access; generated meshes stay in a temporary directory.
"""
import argparse
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
import trimesh
from prepare import prepare

BASE = Path(__file__).resolve().parent.parent
EPS = 1e-5


def require(condition, message):
    if not condition:
        raise ValueError(message)


def pose(translation=(0, 0, 0), rotation=None):
    matrix = np.eye(4)
    if rotation is not None:
        matrix[:3, :3] = rotation
    matrix[:3, 3] = translation
    return matrix[:3]


RX = np.array([[1, 0, 0], [0, 0, -1], [0, 1, 0]])
RY = np.array([[0, 0, 1], [0, 1, 0], [-1, 0, 0]])


def solid(mesh):
    result = md.Manifold(md.Mesh(np.asarray(mesh.vertices, dtype=np.float32),
                                np.asarray(mesh.faces, dtype=np.uint32)))
    require(result.status() == md.Error.NoError, f'Invalid manifold: {result.status()}')
    return result


def intersection(a, b):
    return abs((a ^ b).volume())


def run(coscad, output):
    configured = []
    for name in ('panel_wall', 'panel_floor'):
        matches = re.findall(r'^clearance = ([0-9.eE+-]+)\s*$',
                             (BASE / f'{name}.coscad').read_text(), flags=re.M)
        require(len(matches) == 1, f'{name}: expected one numeric clearance binding')
        configured.append(float(matches[0]))
    require(configured[0] == configured[1], 'Wall and floor clearances differ')
    require(math.isfinite(configured[0]) and 0 < configured[0] < .65, 'Invalid sliding clearance')
    report = {'physical_fit_tested': False, 'clearance_per_face_mm': configured[0],
              'analysis_packages': {p: importlib.metadata.version(p) for p in
                                    ('numpy', 'trimesh', 'scipy', 'manifold3d')},
              'source_sha256': {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
                                for p in [*BASE.glob('*.coscad'), BASE / 'tank.assemble']}}
    with tempfile.TemporaryDirectory(prefix='coscad-tank-fit-') as temporary:
        work = Path(temporary)
        for source in [*BASE.glob('*.coscad'), BASE / 'tank.assemble']:
            shutil.copy(source, work / source.name)

        def command(*args):
            result = subprocess.run([coscad, *map(str, args)], check=True,
                                    text=True, capture_output=True)
            return result.stdout

        report['compiler'] = command('--version').strip()
        report['compiler_sha256'] = hashlib.sha256(Path(coscad).read_bytes()).hexdigest()
        command('validate', work / 'tank.assemble')
        check = command('check', work / 'tank.assemble')
        require('0 overlaps, 0 clearance warnings' in check, check)
        report['assembly_check'] = check.splitlines()[-1]
        command('plan', work / 'tank.assemble')
        plan = json.loads((work / 'tank_plan.json').read_text())
        require(not plan['design_errors'], plan['design_errors'])
        order = {p: i for i, step in enumerate(plan['steps']) for p in step['parts']}
        prerequisites = [('floor', 'bottom_left'), ('floor', 'bottom_right'),
                         ('bottom_left', 'up_bl'), ('bottom_right', 'up_br')]
        prerequisites += [(f'wall_{side}', f'top_{side}') for side in ('front', 'back', 'left', 'right')]
        for before, after in prerequisites:
            require(order[before] < order[after], f'Wrong order: {before} before {after}')
        report['plan'] = {'steps': len(plan['steps']), 'flips': sum(s['flip'] for s in plan['steps']),
                          'design_errors': plan['design_errors'], 'prerequisites_checked': prerequisites}

        def render(name, source=None):
            path = work / f'{name}.coscad'
            if source is not None:
                path.write_text(source)
            command('stl', path, '-o', path.with_suffix('.stl'))
            mesh = trimesh.load_mesh(path.with_suffix('.stl'), process=True)
            require(mesh.is_watertight and mesh.is_winding_consistent and mesh.volume > 0, name)
            require(len(mesh.split()) == 1, f'{name} has disconnected pieces')
            return mesh, solid(mesh)

        _, upright = render('rail100')
        _, rail = render('rail60')
        front = rail.transform(pose((50, 10, 10), RY))
        back = rail.transform(pose((50, 90, 10), RY))
        left = rail.transform(pose((10, 50, 10), RX))
        right = rail.transform(pose((90, 50, 10), RX))
        uprights = [upright.transform(pose((x, 10, 50))) for x in (10, 90)]
        cap = rail.transform(pose((50, 10, 90), RY))
        report['variants'] = []
        for clearance in (0.20, 0.25, 0.30):
            meshes, parts = {}, {}
            for part in ('panel_wall', 'panel_floor'):
                source = re.sub(r'^clearance = .*$', f'clearance = {clearance}',
                                (BASE / f'{part}.coscad').read_text(), flags=re.M)
                mesh, item = render(f'{part}_{round(clearance * 100)}', source)
                hanging = mesh.face_normals[:, 2] < -0.01
                heights = mesh.triangles[:, :, 2].max(axis=1)
                hanging &= heights > mesh.bounds[0, 2] + 0.001
                require(not hanging.any(), f'{part}: downward faces above the flat print face')
                meshes[part], parts[part] = mesh, item
            wall, floor = parts['panel_wall'], parts['panel_floor']
            wall_here = wall.transform(pose((50, 10, 50), RX))
            floor_here = floor.transform(pose((50, 50, 10)))
            volumes = [intersection(wall_here, fixed) for fixed in (*uprights, front, cap)]
            volumes += [intersection(floor_here, fixed) for fixed in (front, back, left, right)]
            require(max(volumes) < EPS, f'Seated interference at {clearance}: {volumes}')
            maximum, samples = 0., 0
            # Sweep the complete wall through the upright slots and bottom mouth.
            for offset in np.arange(0, 120.001, 0.5):
                moving = wall.transform(pose((50, 10, 50 + offset), RX))
                for fixed in (*uprights, front):
                    maximum = max(maximum, intersection(moving, fixed)); samples += 1
            # Floor enters the front rail, then each side rail slides over its key.
            for offset in np.arange(0, 100.001, 0.5):
                maximum = max(maximum, intersection(floor.transform(pose((50, 50 + offset, 10))), front)); samples += 1
                for x in (10, 90):
                    moving = rail.transform(pose((x, 50 + offset, 10), RX))
                    maximum = max(maximum, intersection(moving, floor_here)); samples += 1
            require(maximum < EPS, f'Blocked insertion at {clearance}: {maximum}')
            # A normal shift beyond the available gap and radial pullout must collide.
            normal = intersection(wall.transform(pose((50, 10 + clearance + .10, 50), RX)), uprights[0])
            radial = intersection(wall.transform(pose((52, 10, 50), RX)), uprights[0])
            require(normal > .1 and radial > .1, f'Missing fit/capture: {normal}, {radial}')
            report['variants'].append({'clearance_per_face_mm': clearance,
                                      'neck_mm': round(6.2 - 2 * clearance, 3),
                                      'seated_intersection_max_mm3': max(volumes),
                                      'sweep_step_mm': .5, 'swept_pair_checks': samples,
                                      'swept_intersection_max_mm3': maximum,
                                      'normal_overtravel_collision_mm3': normal,
                                      'radial_pullout_collision_mm3': radial,
                                      'wall_volume_mm3': meshes['panel_wall'].volume,
                                      'floor_volume_mm3': meshes['panel_floor'].volume,
                                      'non_bed_downward_faces': 0})
        oversize = re.sub(r'^clearance = .*$', 'clearance = -0.10',
                          (BASE / 'panel_wall.coscad').read_text(), flags=re.M)
        _, bad = render('oversize_negative_control', oversize)
        collision = intersection(bad.transform(pose((50, 10, 50), RX)), uprights[0])
        require(collision > .1, 'Oversized negative control did not collide')
        report['oversize_negative_control_collision_mm3'] = collision
        samples = work / 'samples'
        prepare(samples, coscad)
        report['fit_samples'] = []
        for source in sorted(samples.glob('*.coscad')):
            checked_in = Path(__file__).with_name(source.name)
            require(checked_in.read_text() == source.read_text(), f'Regenerate {checked_in.name}')
            mesh = trimesh.load_mesh(source.with_suffix('.stl'), process=True)
            require(mesh.is_watertight and mesh.is_winding_consistent and mesh.volume > 0, source.name)
            require(len(mesh.split()) == 1, f'Disconnected sample: {source.name}')
            report['fit_samples'].append({'source': source.name,
                                          'sha256': hashlib.sha256(source.read_bytes()).hexdigest(),
                                          'watertight_single_component': True,
                                          'bounds_mm': mesh.bounds.tolist(),
                                          'volume_mm3': mesh.volume})
        report['passed'] = True
    output.write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--coscad', default='coscad')
    parser.add_argument('--output', type=Path, default=Path(__file__).with_name('validation.json'))
    args = parser.parse_args()
    executable = shutil.which(args.coscad)
    if executable is None:
        parser.error(f'CoScad executable not found: {args.coscad}')
    run(executable, args.output)
