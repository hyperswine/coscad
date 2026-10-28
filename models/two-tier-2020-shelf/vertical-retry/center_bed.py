#!/usr/bin/env python3
"""Center the regenerated single upright coupon, including its 5 mm brim."""
from pathlib import Path
import json
import trimesh
HERE=Path(__file__).resolve().parent
manifest=HERE/'fit_manifest.json'
data=json.loads(manifest.read_text())
assert len(data['beds'])==len(data['variants'])==1
v=data['variants'][0]
assert v['down']=='lft' and v['footprint']==[20,20,50] and not v['printability']['warnings']
mesh=trimesh.load_mesh(HERE/'fit_bed1.stl')
assert mesh.is_watertight and len(mesh.split())==1
mesh.apply_translation([128-mesh.bounds[:,0].mean(),128-mesh.bounds[:,1].mean(),-mesh.bounds[0,2]])
mesh.export(HERE/'fit_bed1.stl')
data['beds'][0]['placements'][0].update(x=118,y=118)
data['packing']={'method':'centered single upright rail','front_left_keepout_mm':[0,0,18,28]}
manifest.write_text(json.dumps(data,indent=2)+'\n')
print('Centered upright 20 x 20 x 50 mm rail; re-slice before using its toolpath')
