#!/usr/bin/env python3
"""Independent mesh, hardware-path and packing checks; make assembly previews.

Requires numpy/scipy/trimesh/manifold3d. This does not operate printers.
"""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import numpy as np
from scipy.spatial.transform import Rotation
import trimesh

HERE = Path(__file__).resolve().parent
EVIDENCE = HERE / 'evidence'


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--coscad', required=True)
    args = ap.parse_args()
    EVIDENCE.mkdir(exist_ok=True)
    design = json.loads((HERE/'model.json').read_text())
    meshes, report = {}, {'parts': {}, 'hardware_paths': 0, 'physical_fit_verified': False,
                          'physical_load_tested': False, 'dispatched': False}
    for name in design['parts']:
        source = HERE/'parts'/f'{name}.coscad'
        result = subprocess.run([args.coscad, 'stl', str(source), '--backend=native', '-o', str(EVIDENCE/f'{name}.stl')], capture_output=True, text=True)
        if result.returncode:
            raise RuntimeError(result.stdout+result.stderr)
        mesh = trimesh.load_mesh(EVIDENCE/f'{name}.stl')
        components = mesh.split()
        assert mesh.is_watertight and mesh.is_winding_consistent and mesh.volume > 0 and len(components) == 1, name
        meshes[name] = mesh
        report['parts'][name] = dict(watertight=True, components=1, volume_mm3=mesh.volume,
                                     bounds_mm=mesh.bounds.tolist(), source_sha256=hashlib.sha256(source.read_bytes()).hexdigest())
    # Canonical assembled rails and print sources must represent the same solid.
    for length in (50, 160, 170, 230):
        a = meshes[f'rail_{length}'].copy()
        b = meshes[f'rail_print_{length}']
        common = trimesh.boolean.intersection([a,b], engine='manifold').volume
        assert abs(a.volume+b.volume-2*common) < 0.01
    world = {}
    for item in design['instances']:
        transform = np.eye(4)
        transform[:3,:3] = Rotation.from_euler('xyz', item['rotation_xyz_deg'], degrees=True).as_matrix()
        transform[:3,3] = item['position_mm']
        world[item['id']] = meshes[item['part']].copy().apply_transform(transform)
    normals = {'top':[0,0,1], 'bot':[0,0,-1], 'fwd':[0,-1,0], 'bak':[0,1,0], 'lft':[-1,0,0], 'rt':[1,0,0]}
    for bolt in design['hardware']:
        normal = np.array(normals[bolt['face']])
        origin = np.array(bolt['hole_world_mm'])
        is_panel = bolt['part'].startswith('panel_')
        bearing = origin if is_panel else origin+3*normal
        shank = trimesh.creation.cylinder(radius=2.5, height=12, sections=32)
        transform = trimesh.geometry.align_vectors([0,0,1], normal)
        transform[:3,3] = bearing-6*normal
        shank.apply_transform(transform)
        for owner in (bolt['part'], bolt['host']):
            common = trimesh.boolean.intersection([shank,world[owner]], engine='manifold')
            assert common.volume < 0.01, (owner, bolt, common.volume)
        report['hardware_paths'] += 1
    # Provisional rectangular sliding-nut envelope; actual nut dimensions unknown.
    nut = trimesh.creation.box([10,9.6,2.6])
    nut.apply_translation([0,0,7])
    common = trimesh.boolean.intersection([nut, meshes['rail_50']], engine='manifold')
    assert common.volume < 0.001
    report['nominal_nut_envelope_mm'] = [10,9.6,2.6]
    report['nut_envelope_interference_mm3'] = common.volume
    # Independently check transformed assembly rather than relying on bounding boxes.
    overlaps = []
    items = list(world.items())
    for j, (an, a) in enumerate(items):
        for bn, b in items[j+1:]:
            extent = np.minimum(a.bounds[1],b.bounds[1])-np.maximum(a.bounds[0],b.bounds[0])
            if np.any(extent < 1e-5):
                continue
            volume = trimesh.boolean.intersection([a,b], engine='manifold').volume
            if volume > 0.02:
                overlaps.append([an,bn,volume])
    assert not overlaps, overlaps
    report['assembly_overlaps'] = overlaps
    for stem in ('manufacture','fit'):
        manifest = json.loads((HERE/f'{stem}_manifest.json').read_text())
        variants = {v['id']:v for v in manifest['variants']}
        total = 0
        for bed in manifest['beds']:
            actual = trimesh.load_mesh(HERE/Path(bed['stl']).name)
            assert actual.is_watertight and len(actual.split()) == len(bed['placements'])
            # A ribbed panel must start on its broad flat skin, not its rib grid.
            if all(p['variant'].startswith('panel_') for p in bed['placements']):
                on_bed = np.all(np.abs(actual.triangles[:,:,2]) < 1e-4,axis=1)
                downward = actual.face_normals[:,2] < -.99
                assert actual.area_faces[on_bed & downward].sum() > 35000
            occupied = []
            for placement in bed['placements']:
                b = placement['brim_mm']
                x,y,w,d = (placement[k] for k in ('x','y','w','d'))
                bounds = [x-b,y-b,x+w+b,y+d+b]
                assert min(bounds[:2]) >= 6-0.001 and max(bounds[2:]) <= 250.001, bounds
                assert variants[placement['variant']]['footprint'][2] <= 250
                for other in occupied:
                    assert bounds[2] <= other[0]+0.001 or other[2] <= bounds[0]+0.001 or bounds[3] <= other[1]+0.001 or other[3] <= bounds[1]+0.001
                occupied.append(bounds)
                total += 1
        assert total == (48 if stem == 'manufacture' else 3), total
        report[stem] = dict(beds=len(manifest['beds']), parts=total, brim_included=True)
    render(design, world, meshes)
    (EVIDENCE/'geometry.json').write_text(json.dumps(report, indent=2)+'\n')
    print(json.dumps({k:v for k,v in report.items() if k!='parts'}, indent=2))


def render(design, world, meshes):
    # Export actual verified meshes for a depth-buffered OpenSCAD render.
    # Matplotlib's painter ordering is unreliable for slotted, perforated solids.
    colors = {'rail':[.23,.34,.43], 'post':[.23,.34,.43], 'connector':[.28,.55,.51], 'panel':[.88,.71,.40]}
    lines = ['// Verified native meshes at their assembly poses. Hardware omitted.']
    for item in design['instances']:
        lines.append(f"color({colors[item['kind']]}) translate({item['position_mm']}) rotate({item['rotation_xyz_deg']}) import(\"evidence/{item['part']}.stl\");")
    (HERE/'preview.scad').write_text('\n'.join(lines)+'\n')
    (HERE/'details.scad').write_text('''// L plate, centre T plate and panel underside.
color([.28,.55,.51]) translate([-240,0,0]) import("evidence/gusset_l.stl");
color([.28,.55,.51]) translate([-110,0,0]) import("evidence/gusset_t.stl");
color([.88,.71,.40]) translate([95,0,11]) rotate([180,0,0]) import("evidence/panel_left.stl");
''')


if __name__ == '__main__':
    main()
