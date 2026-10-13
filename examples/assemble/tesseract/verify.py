#!/usr/bin/env python3
"""Real-mesh regression for the fastener-free tesseract (see requirements.txt).

Runs the compiler checker with a clearance pin, then independently checks
connected solids, sampled assembly paths, captured shoulders and the nominal
press fit with Manifold. Sampling is not a continuous swept-volume proof.
"""
import argparse
import itertools
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile

import numpy as np
import trimesh

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[2]

def run(args, env):
    result = subprocess.run([str(a) for a in args], capture_output=True, text=True, env=env)
    if result.returncode:
        raise RuntimeError(result.stdout + result.stderr)
    return result.stdout + result.stderr

def overlap(a, b):
    if np.any(a.bounds[1] < b.bounds[0]) or np.any(b.bounds[1] < a.bounds[0]):
        return 0.0
    with np.errstate(invalid='ignore', divide='ignore'):
        result = trimesh.boolean.intersection([a, b], engine='manifold')
        volume = abs(float(result.volume)) if len(result.faces) else 0.0
    assert np.isfinite(volume), 'invalid intersection volume'
    return volume

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--compiler', default=str(REPO / 'bin/coscad'))
    parser.add_argument('--report', type=Path)
    args = parser.parse_args()
    env = dict(os.environ, GHCRTS='-N1')
    if (REPO.parent / 'BOSL2/std.scad').is_file():
        env.setdefault('COSCAD_BOSL2', str(REPO.parent / 'BOSL2'))
    checker_dir = None
    try:
        with tempfile.TemporaryDirectory(prefix='tesseract-verify-') as temp:
            work = Path(temp)
            for source in itertools.chain(HERE.glob('*.coscad'), HERE.glob('*.assemble')):
                shutil.copy(source, work / source.name)
            cap = work / 'cap.coscad'
            original_cap = cap.read_text()
            assert original_cap.count('fit = 0.03') == 1
            cap.write_text(original_cap.replace('fit = 0.03', 'fit = -0.05'))
            output = run([args.compiler, 'check', work / 'tesseract.assemble', '--keep-temp'], env)
            match = re.search(r'scratch renders kept in (.+)', output)
            assert match, output
            checker_dir = Path(match[1].strip())
            meshes = {p.stem[4:]: trimesh.load_mesh(p) for p in checker_dir.glob('chk_*.stl') if not p.stem.startswith('chk_pair')}
            assert len(meshes) == 38, sorted(meshes)
            for name, mesh in meshes.items():
                assert mesh.is_volume and len(mesh.split()) == 1, name
            print('38 watertight, connected part meshes', flush=True)
            worst = max((overlap(a, b), na, nb) for (na, a), (nb, b)
                        in itertools.combinations(meshes.items(), 2))
            assert worst[0] < 0.01, worst
            print('All assembly pairs: no unintended interference', flush=True)
            checks = 0
            def path(name, direction, others, distance=15):
                nonlocal checks
                for d in np.linspace(0, distance, int(distance * 10) + 1):
                    moved = meshes[name].copy()
                    moved.apply_translation(d * direction)
                    for target in others:
                        volume = overlap(moved, meshes[target])
                        assert volume < 0.01, (name, target, float(d), volume)
                    checks += 1
            core = [n for n in meshes if n.startswith(('strut', 'corner')) or n in ['inner', 'jig']]
            no_caps = [n for n in meshes if not n.startswith('cap')]
            for i in range(1, 9):
                name = f'corner{i}'
                outward = np.sign(meshes[name].bounds.mean(axis=0) - 50) / np.sqrt(3)
                # Bases approach the exposed strut ends before any bars exist.
                path(name, outward, [n for n in core if n != name])
                # Caps close last, with every other part already in its final pose.
                cap_name = f'cap{i}'
                path(cap_name, outward, [n for n in meshes if n != cap_name])
            for i in range(1, 13):
                name = f'bar{i}'
                axis = int(np.argmax(meshes[name].extents))
                outward = np.sign(meshes[name].bounds.mean(axis=0) - 50)
                outward[axis] = 0
                outward /= np.linalg.norm(outward)
                path(name, outward, [n for n in no_caps if n != name])
            path('jig', np.array([0, 0, -1]), [n for n in meshes if n != 'jig'], 40)
            print(f'{checks} assembly/removal positions sampled at 0.1 mm intervals', flush=True)
            retained = {}
            for axis, name in enumerate(['bar1', 'bar5', 'bar9']):
                moved = meshes[name].copy()
                direction = np.zeros(3); direction[axis] = 1
                moved.apply_translation(direction)
                retained[name] = sum(overlap(moved, meshes[n]) for n in ['corner1', 'cap1'])
                assert retained[name] > 0.1, retained
            cap.write_text(original_cap)
            run([args.compiler, 'stl', cap, '-o', work / 'nominal-cap.stl'], env)
            nominal = trimesh.load_mesh(work / 'nominal-cap.stl')
            assert nominal.is_volume and len(nominal.split()) == 1
            press_volume = overlap(nominal, meshes['corner1'])
            assert 0.01 < press_volume < 2, press_volume
            assert all(overlap(nominal, meshes[n]) < 0.01 for n in ['bar1', 'bar5', 'bar9', 'strut1'])
            print(f'Nominal cap/base interference: {press_volume:.3f} mm^3; confined to split pin', flush=True)
            for name, count in [('tesseract', 38), ('fit-test', 3)]:
                run([args.compiler, 'next', work / (name + '.assemble')], env)
                manifest = json.loads((work / (name + '_manifest.json')).read_text())
                bed = trimesh.load_mesh(work / (name + '_bed1.stl'))
                assert len(manifest['beds']) == 1
                assert bed.is_volume and len(bed.split()) == count, name
                assert np.all(bed.bounds[0][:2] >= 5.99) and np.all(bed.bounds[1][:2] <= 250.01)
                assert abs(bed.bounds[0][2]) < 1e-6
                print(f'{name}: {count} connected pieces on one bed, within margins', flush=True)
            report = dict(parts=38, unintended_overlap_mm3=worst[0],
                          sampled_positions=checks, spacing_mm=0.1,
                          nominal_press_interference_mm3=press_volume,
                          retained_bar_interference_mm3=retained,
                          physical_fit_tested=False)
            if args.report:
                args.report.write_text(json.dumps(report, indent=2) + '\n')
    finally:
        if checker_dir:
            shutil.rmtree(checker_dir)

if __name__ == '__main__':
    main()
