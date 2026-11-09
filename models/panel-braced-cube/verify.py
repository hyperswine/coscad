#!/usr/bin/env python3
"""Verify the three actual assemblies, print poses and panel insertion paths. No printer access."""
from collections import Counter
import json
from pathlib import Path
import re
import subprocess

import manifold3d as md
import numpy as np
import trimesh
from prepare import HERE, ROOT, COSCAD, CONFIGS, digest


def require(condition, message):
    if not condition:
        raise ValueError(message)


def solid(mesh):
    value = md.Manifold(md.Mesh(np.asarray(mesh.vertices, np.float32), np.asarray(mesh.faces, np.uint32)))
    require(value.status() == md.Error.NoError, str(value.status()))
    return value


def placements(path):
    text = path.read_text()
    references = dict(re.findall(r'^(\w+) ← (\S+)', text, flags=re.M))
    expression = text.split('asm =', 1)[1].split('\n\n', 1)[0]
    found, counts = {}, Counter()
    for label, steps in re.findall(r'\((\w+)([^()]*)\)', expression):
        counts[label] += 1
        ident = f'{label}#{counts[label]}' if label == 'br' else label
        mesh = trimesh.load_mesh(HERE / Path(references[label]).with_suffix('.stl'))
        for step in steps.split('|>'):
            if not step.strip():
                continue
            op, *args = step.split()
            values = list(map(float, args))
            if op == 'move' and len(values) == 3:
                mesh.apply_translation(values)
            elif op in ('rotx', 'roty', 'rotz') and len(values) == 1:
                axis = np.eye(3)['xyz'.index(op[-1])]
                mesh.apply_transform(trimesh.transformations.rotation_matrix(np.deg2rad(values[0]), axis))
            else:
                raise ValueError(f'Unexpected pose syntax: {step}')
        found[ident] = mesh
    require(len(found) == 28 + len(CONFIGS[path.stem]), 'Unexpected actual placement count')
    return found


def main():
    report = {'physical_tested': False, 'sliced': False, 'dispatched': False,
              'compiler': subprocess.check_output([str(COSCAD), '--version'], text=True).strip(),
              'compiler_sha256': digest(COSCAD), 'configurations': {}, 'print_parts': {}, 'panel_paths': {}}
    baseline = placements(HERE / 'bare.assemble')
    for name, included in CONFIGS.items():
        path = HERE / (name + '.assemble')
        validation = json.loads(subprocess.check_output([str(COSCAD), 'validate', '--json', str(path)], text=True))
        require(validation['valid'], f'Invalid {name}')
        check = subprocess.check_output([str(COSCAD), 'check', str(path)], text=True)
        require('0 overlaps, 0 clearance warnings' in check, check)
        posed = placements(path)
        for ident, mesh in baseline.items():
            require(np.allclose(mesh.vertices, posed[ident].vertices, atol=1e-7), f'Frame changed: {name}/{ident}')
        bounds = np.stack([m.bounds for m in posed.values()])
        extent = [bounds[:, 0].min(axis=0).tolist(), bounds[:, 1].max(axis=0).tolist()]
        require(np.allclose(extent, [[-4, -4, 0], [154, 154, 150]], atol=1e-5), 'Wrong assembled bounds')
        require(len(re.findall(r'^fastener M5x8 ', path.read_text(), flags=re.M)) == 32, 'Hardware changed')
        report['configurations'][name] = {'parts': len(posed), 'fasteners': 32, 'bounds_mm': extent,
                                          'overlaps': 0, 'clearance_warnings': 0, 'frame_matches_bare': True,
                                          'source_sha256': digest(path)}
        plan = json.loads((HERE / (name + '_plan.json')).read_text())
        require(not plan['design_errors'], f'Planner errors: {name}')
        ordered = [p for step in plan['steps'] for p in step['parts']]
        require(Counter(ordered) == Counter(posed.keys()), f'Missing/duplicate planned part: {name}')
        joints = [joint for step in plan['steps'] for joint in step['fasteners']]
        require(sum(joint['joint'] == 'bolt' and joint['spec'] == 'M5x8' for joint in joints) == 32, f'Missing planned bolts: {name}')
        require(sum(joint['joint'] == 'drop' for joint in joints) == len(included), f'Missing panel drop joints: {name}')
        order = {p: i for i, step in enumerate(plan['steps']) for p in step['parts']}
        for panel, cap in [('wall_back', 'top_back'), ('wall_right', 'top_right')]:
            if panel in included:
                require(order[panel] < order[cap], f'Cap fitted before panel: {name}/{panel}')
        report['configurations'][name]['plan'] = {'steps': len(plan['steps']),
                                                 'flips': sum(step['flip'] for step in plan['steps']),
                                                 'fasteners_driven': 32, 'all_parts_placed_once': True,
                                                 'panel_before_cap': True}
    both = placements(HERE / 'back-side.assemble')
    solids = {name: solid(mesh) for name, mesh in both.items()}
    for panel, cap, normal in [('wall_back', 'top_back', (0, 1, 0)), ('wall_right', 'top_right', (1, 0, 0))]:
        fixed = md.Manifold.batch_boolean([s for n, s in solids.items() if n not in (panel, cap)], md.OpType.Add)
        offsets = np.arange(0, 160.001, .5)
        maximum = max(abs((solids[panel].translate((0, 0, float(d))) ^ fixed).volume()) for d in offsets)
        require(maximum < 1e-5, f'Panel insertion blocked: {panel}')
        rail_union = md.Manifold.batch_boolean([s for n, s in solids.items() if n.startswith('up_')], md.OpType.Add)
        controls = [abs((solids[panel].translate(np.asarray(normal) * sign * .35) ^ rail_union).volume()) for sign in (-1, 1)]
        require(min(controls) > .1, f'Missing capture: {panel}')
        report['panel_paths'][panel] = {'samples': len(offsets), 'step_mm': .5,
                                         'maximum_overlap_mm3': maximum, 'overtravel_collision_mm3': controls,
                                         'removed_cap_only': cap, 'other_panel_present': True}
    for name in ('rail-print-150', 'rail-print-110', 'panel-print', 'bracket'):
        mesh = trimesh.load_mesh(HERE / 'parts' / (name + '.stl'))
        require(mesh.is_watertight and mesh.is_winding_consistent and len(mesh.split()) == 1, name)
        hanging = (mesh.face_normals[:, 2] < -np.sqrt(.5) - 1e-4)
        hanging &= mesh.triangles[:, :, 2].max(axis=1) > mesh.bounds[0, 2] + .001
        require(not hanging.any(), f'Unsupported print underside: {name}')
        require(mesh.extents[2] <= 250 and max(mesh.extents[:2]) < 244, 'Part does not fit the nominal bed')
        report['print_parts'][name] = {'watertight_single_component': True, 'extents_mm': mesh.extents.tolist(),
                                        'unsupported_faces_over_45deg': 0, 'volume_mm3': mesh.volume,
                                        'source_sha256': digest(HERE / 'parts' / (name + '.coscad')),
                                        'stl_sha256': digest(HERE / 'parts' / (name + '.stl'))}
    manifest = json.loads((HERE / 'manufacture_manifest.json').read_text())
    require(sum(len(b['placements']) for b in manifest['beds']) == 30, 'Wrong packed inventory')
    packed = Counter(p['variant'] for b in manifest['beds'] for p in b['placements'])
    require(packed == Counter({'rail150_bot': 4, 'rail110_bot': 8, 'bracket_bot': 16, 'panel_bot': 2}), 'Wrong packed quantities')
    report['manufacturing'] = {'generic_beds': len(manifest['beds']), 'parts': 30,
                                'printer_specific_preflight_passed': False,
                                'warnings': {v['id']: v['printability']['warnings'] for v in manifest['variants']}}
    report['passed'] = True
    (HERE / 'validation.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
