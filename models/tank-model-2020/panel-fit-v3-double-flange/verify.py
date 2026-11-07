#!/usr/bin/env python3
"""Check both flanges, axial fit, capture and support angles of V3 samples.

Uses the same temporary analysis dependencies as panel-fit-v2/verify.py.
No printer access. CoScad renders the actual sources with its pinned kernel.
"""
import argparse
import hashlib
import importlib.metadata
import json
from pathlib import Path
import shutil
import subprocess
import tempfile

import manifold3d as md
import numpy as np
import trimesh
from prepare import HERE, BASE, prepare, coupon_source


def require(condition, message):
    if not condition:
        raise ValueError(message)


def solid(path):
    mesh = trimesh.load_mesh(path, process=True)
    require(mesh.is_watertight and mesh.is_winding_consistent and mesh.volume > 0, str(path))
    require(len(mesh.split()) == 1, f'Disconnected sample: {path.name}')
    result = md.Manifold(md.Mesh(np.asarray(mesh.vertices, dtype=np.float32),
                                np.asarray(mesh.faces, dtype=np.uint32)))
    require(result.status() == md.Error.NoError, str(result.status()))
    return mesh, result


def run(coscad, output):
    report = {'revision': 'v3-double-flange', 'physical_fit_tested': False,
              'compiler': subprocess.check_output([coscad, '--version'], text=True).strip(),
              'compiler_sha256': hashlib.sha256(Path(coscad).read_bytes()).hexdigest(),
              'source_wall_sha256': hashlib.sha256((BASE / 'panel_wall.coscad').read_bytes()).hexdigest(),
              'source_rail_sha256': hashlib.sha256((BASE / 'rail100.coscad').read_bytes()).hexdigest(),
              'analysis_packages': {name: importlib.metadata.version(name) for name in ('numpy', 'trimesh', 'scipy', 'manifold3d')},
              'samples': []}
    with tempfile.TemporaryDirectory(prefix='coscad-double-flange-') as temporary:
        work = Path(temporary)
        prepare(work, coscad)
        for source in work.glob('*.coscad'):
            require(source.read_bytes() == (HERE / source.name).read_bytes(), f'Regenerate {source.name}')
        _, rail = solid(work / 'rail35.stl')
        # The coupon's rail face is X=-1.5; rail centre X=8.5, bottom Z=0.
        rail = rail.translate((8.5, 0, 0))
        for holes, clearance in enumerate((.20, .25, .30), 1):
            name = f'double-c{round(clearance * 100):03d}-{holes}holes'
            mesh, coupon = solid(work / (name + '.stl'))
            require(mesh.euler_number == 2 - 2 * holes, 'Wrong number of identification holes')
            require(abs(mesh.bounds[0, 2]) < 1e-5 and abs(mesh.bounds[1, 2] - 30) < 1e-5, 'Not upright/on bed')
            # Above the bed, downward faces must remain within 45 degrees of vertical.
            hanging = (mesh.face_normals[:, 2] < -np.sqrt(.5) - 1e-4)
            hanging &= mesh.triangles[:, :, 2].max(axis=1) > 1e-4
            require(not hanging.any(), 'Unsupported underside on a fitting surface')
            section = coupon ^ md.Manifold.cube((20, 20, .2), center=True).translate((-2, 0, 6))
            mirrored = section.mirror((0, 1, 0))
            symmetry_error = abs((section - mirrored).volume()) + abs((mirrored - section).volume())
            require(symmetry_error < 1e-5, 'Cross-section does not have matching bilateral flanges')
            # Confirm material behind both lips and in both narrower shoulder pockets.
            flange_probes = []
            for sign in (-1, 1):
                for size, centre in [((1, .5, .2), (1.2, sign * 4.5, 6)),
                                     ((.5, .4, .2), (2.75, sign * 3.6, 6))]:
                    probe = md.Manifold.cube(size, center=True).translate(centre)
                    volume = abs((coupon ^ probe).volume())
                    require(volume > .035, 'Missing flange/step')
                    flange_probes.append(volume)
            maximum = 0.
            offsets = np.arange(0, 36.001, .25)
            for offset in offsets:
                maximum = max(maximum, abs((coupon.translate((0, 0, float(offset))) ^ rail).volume()))
            require(maximum < 1e-5, 'Axial sliding path intersects the rail')
            normal_controls = [abs((coupon.translate((0, sign * (clearance + .1), 0)) ^ rail).volume()) for sign in (-1, 1)]
            pullout = abs((coupon.translate((-2, 0, 0)) ^ rail).volume())
            bottoming = abs((coupon.translate((1.2, 0, 0)) ^ rail).volume())
            require(min(normal_controls) > .1 and pullout > .1 and bottoming > .1, 'Missing capture or depth constraint')
            report['samples'].append({'source': name + '.coscad',
                                      'source_sha256': hashlib.sha256((work / (name + '.coscad')).read_bytes()).hexdigest(),
                                      'clearance_per_face_mm': clearance, 'holes': holes,
                                      'neck_mm': round(6.2 - 2 * clearance, 3),
                                      'full_head_width_mm': round(11 - 2 * clearance, 3),
                                      'bounds_mm': mesh.bounds.tolist(), 'volume_mm3': mesh.volume,
                                      'watertight_single_component': True,
                                      'unsupported_downward_faces_over_45deg': int(hanging.sum()),
                                      'bilateral_symmetry_error_mm3': symmetry_error,
                                      'flange_probe_volumes_mm3': flange_probes,
                                      'sweep_step_mm': .25, 'sweep_samples': len(offsets),
                                      'swept_intersection_max_mm3': maximum,
                                      'normal_overtravel_collision_mm3': normal_controls,
                                      'radial_pullout_collision_mm3': pullout,
                                      'slot_bottoming_collision_mm3': bottoming})
        bad = work / 'oversize.coscad'
        bad.write_text(coupon_source(-.10, 1))
        subprocess.run([coscad, 'stl', str(bad), '-o', str(bad.with_suffix('.stl'))], check=True, capture_output=True)
        _, oversized = solid(bad.with_suffix('.stl'))
        collision = abs((oversized ^ rail).volume())
        require(collision > .1, 'Oversized negative control did not collide')
        report.update(oversized_negative_control_collision_mm3=collision, passed=True)
    output.write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--coscad', default='coscad')
    parser.add_argument('--output', type=Path, default=HERE / 'validation.json')
    args = parser.parse_args()
    executable = shutil.which(args.coscad)
    if executable is None:
        parser.error(f'CoScad executable not found: {args.coscad}')
    run(executable, args.output)
