#!/usr/bin/env python3
"""Stage unchanged tall parts for two conservative P1S sequential layouts."""
from pathlib import Path
import hashlib
import importlib.util
import json
import subprocess
import sys

import numpy as np
import trimesh
from shapely.geometry import MultiPoint, box

HERE = Path(__file__).resolve().parent
SPLIT = HERE.parent
ROOT = HERE.parents[3]


def save(path, value):
    path.write_text(json.dumps(value, indent=2) + '\n')


def main():
    spec = importlib.util.spec_from_file_location('slicer', ROOT/'scripts/bambu-slice.py')
    slicer = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(slicer)
    machine = slicer.machine_with_templates('Bambu Lab P1S 0.4 nozzle')
    keys = ['extruder_clearance_height_to_lid', 'extruder_clearance_height_to_rod',
            'extruder_clearance_max_radius', 'extruder_clearance_dist_to_rod',
            'extruder_clearance_radius', 'printable_height']
    clearance = {k: float(machine[k]) for k in keys}
    sources = {'strut': SPLIT/'bed4/1-strut.stl',
               'bar-print': SPLIT/'bed5/1-bar-print.stl'}
    layouts = {'mixed': [('strut', 128, 64), ('bar-print', 128, 192)],
               'bar-only': [('bar-print', 128, 128)]}
    reports = {}
    for name, layout in layouts.items():
        folder = HERE/name
        folder.mkdir(exist_ok=True)
        if (folder/'print-run.json').exists():
            raise ValueError('A dispatch exists; preserve this layout and make a new variant')
        items, footprints, bounds = [], [], []
        for ident, (kind, cx, cy) in enumerate(layout, 1):
            source = sources[kind]
            mesh = trimesh.load_mesh(source)
            old_volume = mesh.volume
            old_extents = mesh.extents.copy()
            assert mesh.is_watertight and mesh.is_winding_consistent
            shift = np.array([cx, cy]) - mesh.bounds[:, :2].mean(axis=0)
            mesh.apply_translation([*shift, 0])
            assert np.max(abs(mesh.extents-old_extents)) < 1e-5
            assert abs(mesh.volume-old_volume) < 1e-4
            hull = MultiPoint(mesh.vertices[:, :2]).convex_hull
            envelope = hull.buffer(8.5)
            assert box(20, 20, 236, 236).contains(envelope)
            assert not envelope.intersects(box(0, 0, 18, 28))
            file = f'{ident}-{kind}.stl'
            mesh.export(folder/file)
            items.append(dict(id=ident, name=kind, file=file, angle=0,
                              shift=shift.tolist(), brim_mm=8,
                              center_xy_mm=[cx, cy], bounds=mesh.bounds.tolist(),
                              envelope_wkt=envelope.wkt,
                              source=str(source.relative_to(ROOT)),
                              source_sha256=hashlib.sha256(source.read_bytes()).hexdigest()))
            footprints.append(envelope)
            bounds.append(mesh.bounds.copy())
        # Retain the manufacturer's physical limits. Only the last item may
        # exceed lid clearance; disjoint Y bands avoid the X-rod sweep.
        for i, a in enumerate(bounds[:-1]):
            assert a[1, 2] + 5 < clearance['extruder_clearance_height_to_lid']
            for b in bounds[i+1:]:
                assert b[0, 1] > a[1, 1]
                y_gap = b[0, 1] - a[1, 1]
                assert y_gap > clearance['extruder_clearance_dist_to_rod'] + 5
                assert y_gap > clearance['extruder_clearance_max_radius'] + 5
        assert max(b[1, 2] for b in bounds) < clearance['printable_height']
        gap = footprints[0].distance(footprints[1]) if len(footprints) == 2 else None
        packing = dict(bed_index=4, label='Tesseract sequential '+name,
                       items=items, count=len(items), minimum_edge_inset_mm=20,
                       brim_mm=8, minimum_reserved_envelope_gap_mm=gap,
                       normal_objects_by_type={k: sum(i['name'] == k for i in items)
                                               for k, _, _ in layout})
        save(folder/'packing.json', packing)
        save(folder/'sequence-process.json', {'print_sequence': 'by object'})
        # A single thin shaft no longer has seven other objects for cooling.
        save(folder/'sequence-filament.json', {'slow_down_layer_time': ['12'],
                                             'slow_down_min_speed': ['5'],
                                             'slow_down_for_layer_cooling': ['1']})
        subprocess.run([sys.executable, str(SPLIT/'prepare.py'), '--bed', '4',
                        '--printer', 'p1s', '--output-dir', str(folder),
                        '--process-overrides', str(folder/'sequence-process.json'),
                        '--filament-overrides', str(folder/'sequence-filament.json')], check=True)
        reports[name] = dict(objects=[i['name'] for i in items],
                             heights_mm=[float(b[1, 2]) for b in bounds],
                             brim_envelope_gap_mm=gap,
                             centers_xy_mm=[i['center_xy_mm'] for i in items])
    runs = [dict(run=i+1, layout='mixed', components=[f'strut-{i+1:02}', f'bar-{i+1:02}'])
            for i in range(8)]
    runs += [dict(run=i+9, layout='bar-only', components=[f'bar-{i+9:02}']) for i in range(4)]
    save(HERE/'plan.json', dict(printer='p1s', print_sequence='by object',
                              components={'strut': 8, 'bar-print': 12},
                              physical_plate_runs=12, layouts=reports, runs=runs,
                              unmodified_machine_clearance=clearance,
                              dispatched=False,
                              limitation='Static layout checks only until slicer and actual G-code verification pass.'))
    print('Prepared two unique layouts for 12 manual plate runs; no print dispatched')


if __name__ == '__main__':
    main()
