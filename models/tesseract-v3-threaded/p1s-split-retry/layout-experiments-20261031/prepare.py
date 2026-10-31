#!/usr/bin/env python3
"""Prepare X1C capacity and compact by-layer experiments without dispatch."""
from pathlib import Path
from collections import Counter
import hashlib
import itertools
import json
import subprocess
import sys

import numpy as np
import trimesh
from shapely.geometry import MultiPoint, box

HERE = Path(__file__).resolve().parent
SPLIT = HERE.parent
ROOT = HERE.parents[3]


def main():
    stagger = [(55, 40), (201, 97.5), (55, 155), (201, 212.5)]
    compact = [(x, y) for y in (114, 142) for x in (114, 142)]
    layouts = {
        'four-struts-sequential': ('by object', [('strut', *p) for p in stagger]),
        'three-struts-bar-sequential': ('by object', [('strut', *p) for p in stagger[:3]] + [('bar-print', *stagger[3])]),
        'four-struts-compact': ('by layer', [('strut', *p) for p in compact]),
    }
    machine = json.loads((SPLIT/'sequential-retry/x1c/mixed/presets/machine.json').read_text())
    keys = ['extruder_clearance_height_to_lid', 'extruder_clearance_height_to_rod',
            'extruder_clearance_max_radius', 'extruder_clearance_dist_to_rod',
            'extruder_clearance_radius', 'printable_height']
    limits = {k: float(machine[k]) for k in keys}
    sources = {'strut': SPLIT/'bed4/1-strut.stl', 'bar-print': SPLIT/'bed5/1-bar-print.stl'}
    summary = {}
    for name, (mode, layout) in layouts.items():
        folder = HERE/name
        folder.mkdir(exist_ok=True)
        if (folder/'print-run.json').exists():
            raise ValueError('Preserve any already-dispatched experiment')
        items, bodies, envelopes = [], [], []
        for ident, (kind, x, y) in enumerate(layout, 1):
            source = sources[kind]
            mesh = trimesh.load_mesh(source)
            shift = np.array([x, y])-mesh.bounds[:, :2].mean(axis=0)
            mesh.apply_translation([*shift, 0])
            hull = MultiPoint(mesh.vertices[:, :2]).convex_hull
            envelope = hull.buffer(8.5)
            assert box(20, 20, 236, 236).contains(envelope)
            assert not envelope.intersects(box(0, 0, 18, 28))
            file = f'{ident}-{kind}.stl'
            mesh.export(folder/file)
            items.append(dict(id=ident, name=kind, file=file, angle=0, shift=shift.tolist(),
                              brim_mm=8, center_xy_mm=[x, y], bounds=mesh.bounds.tolist(),
                              envelope_wkt=envelope.wkt,
                              source=str(source.relative_to(ROOT)),
                              source_sha256=hashlib.sha256(source.read_bytes()).hexdigest()))
            bodies.append(hull)
            envelopes.append(envelope)
        assert min(a.distance(b) for a, b in itertools.combinations(bodies, 2)) > 1
        if mode == 'by object':
            # Head clearance needs full XY separation, not just a large Y gap;
            # the rod spans the X direction and separately needs Y clearance.
            for i, a in enumerate(items[:-1]):
                assert a['bounds'][1][2]+5 < limits['extruder_clearance_height_to_lid']
                for j, b in enumerate(items[i+1:], i+1):
                    assert bodies[i].distance(bodies[j]) > limits['extruder_clearance_max_radius']+5
                    assert b['bounds'][0][1]-a['bounds'][1][1] > limits['extruder_clearance_dist_to_rod']+5
        assert max(i['bounds'][1][2] for i in items) < limits['printable_height']
        packing = dict(bed_index=4, label='Tesseract '+name, items=items, count=len(items),
                       minimum_reserved_envelope_gap_mm=min(a.distance(b) for a, b in itertools.combinations(envelopes, 2)),
                       minimum_edge_inset_mm=20, brim_mm=8,
                       normal_objects_by_type=dict(Counter(i['name'] for i in items)))
        (folder/'packing.json').write_text(json.dumps(packing, indent=2)+'\n')
        process = {'print_sequence': mode}
        if mode == 'by layer': process['reduce_crossing_wall'] = '1'
        (folder/'experiment-process.json').write_text(json.dumps(process, indent=2)+'\n')
        (folder/'experiment-filament.json').write_text(json.dumps({
            'slow_down_layer_time': ['12'], 'slow_down_min_speed': ['5'],
            'slow_down_for_layer_cooling': ['1']}, indent=2)+'\n')
        plan = dict(printer='x1c', print_sequence=mode, objects=[i['name'] for i in items],
                    unmodified_machine_clearance=limits, dispatched=False,
                    centers_xy_mm=[i['center_xy_mm'] for i in items],
                    note='Projected brim envelope overlap is permitted only in the compact by-layer experiment; actual sliced brim paths must be checked.')
        (folder/'plan.json').write_text(json.dumps(plan, indent=2)+'\n')
        subprocess.run([sys.executable, str(SPLIT/'prepare.py'), '--bed', '4', '--printer', 'x1c',
                        '--output-dir', str(folder), '--process-overrides', str(folder/'experiment-process.json'),
                        '--filament-overrides', str(folder/'experiment-filament.json')] +
                       (['--end-clearance-mm', '5'] if name == 'four-struts-sequential' else []), check=True)
        summary[name] = plan
    (HERE/'plans.json').write_text(json.dumps(summary, indent=2)+'\n')


if __name__ == '__main__':
    main()
