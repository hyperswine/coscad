#!/usr/bin/env python3
"""Deterministic row packing for this shelf; includes brims and X1C keep-out.

Run AFTER coscad next manufacture.assemble. No geometry/hints are changed.
The generic packer rotates long rails into sparse layouts; this grouped layout
uses parallel rails and reserves the X1C's front-left 18 x 28 mm exclusion.
"""
import hashlib
import json
from pathlib import Path
from collections import Counter
import numpy as np
import trimesh

HERE = Path(__file__).resolve().parent


def main():
    path = HERE/'manufacture_manifest.json'
    manifest = json.loads(path.read_text())
    variants = {v['id']:v for v in manifest['variants']}
    counts = Counter(p['variant'] for b in manifest['beds'] for p in b['placements'])
    used = Counter()
    layouts = [[['panel_left_top']], [['panel_left_top']], [['panel_right_top']], [['panel_right_top']],
               [['rail_230_bot'] for _ in range(6)], [['rail_170_bot'] for _ in range(7)],
               [['rail_170_bot']]+[['rail_160_bot'] for _ in range(6)]]
    layouts += [[['gusset_l_bot']*2 for _ in range(3)] for _ in range(3)]
    layouts += [[['gusset_l_bot']*2, ['gusset_t_bot']*2, ['gusset_t_bot']*2]]
    beds = []
    for index, rows in enumerate(layouts, 1):
        heights = [max(variants[v]['footprint'][1] for v in row) for row in rows]
        depth = sum(heights)+12*(len(rows)-1)
        y = (256-depth)/2
        placements, meshes = [], []
        for row,height in zip(rows,heights):
            width = sum(variants[v]['footprint'][0] for v in row)+12*(len(row)-1)
            x = (256-width)/2
            for vid in row:
                v = variants[vid]; w,d,h = v['footprint']
                assert x-5 >= 6-1e-4 and y-5 >= 6-1e-4 and x+w+5 <= 250.0001 and y+d+5 <= 250.0001
                assert x-5 >= 18 or y-5 >= 28, (index,vid,x,y)
                used[vid] += 1
                placements.append(dict(instance=f"{v['part']}_{used[vid]}", variant=vid,x=x,y=y,w=w,d=d,brim_mm=5,rot_z=0))
                mesh = trimesh.load_mesh(HERE/Path(v['mesh']).name)
                # next's variant STL is the source mesh; only its bed STL has
                # the declared down-face rotation baked in. Do not confuse them.
                assert v['down'] in ('bot','top')
                if v['down'] == 'top':
                    mesh.apply_transform(trimesh.transformations.rotation_matrix(np.pi,[1,0,0]))
                mesh.apply_translation(np.array([x,y,0])-mesh.bounds[0])
                meshes.append(mesh)
                x += w+12
            y += height+12
        stl = HERE/f'manufacture_bed{index}.stl'
        trimesh.util.concatenate(meshes).export(stl)
        beds.append(dict(index=index,stl=str(stl.relative_to(HERE.parents[1])),placements=placements))
    assert used == counts, (used,counts)
    manifest['beds'] = beds
    manifest['packing'] = dict(method='explicit shelf rows', clearance_between_brims_mm=2,
        front_left_keepout_mm=[0,0,18,28], script_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest())
    path.write_text(json.dumps(manifest,indent=2)+'\n')
    print(f'{len(beds)} X1C beds; {sum(used.values())} parts; brims and exclusion checked')


if __name__ == '__main__':
    main()
