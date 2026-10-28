#!/usr/bin/env python3
"""Center the two compiler-generated thread coupons; never dispatch."""
from pathlib import Path
import json
import trimesh
HERE=Path(__file__).resolve().parent
file=HERE/'thread-fit_manifest.json';m=json.loads(file.read_text())
assert len(m['beds'])==1 and len(m['beds'][0]['placements'])==2
assert all(not v['printability']['warnings'] for v in m['variants'])
mesh=trimesh.load_mesh(HERE/'thread-fit_bed1.stl')
assert mesh.is_watertight and len(mesh.split())==2
delta=[128-mesh.bounds[:,0].mean(),128-mesh.bounds[:,1].mean(),-mesh.bounds[0,2]]
mesh.apply_translation(delta);mesh.export(HERE/'thread-fit_bed1.stl')
for v in m['beds'][0]['placements']:
 v['x']+=delta[0];v['y']+=delta[1]
 assert v['x']-5>=18 and v['y']-5>=28
 assert v['x']+v['w']+5<250 and v['y']+v['d']+5<250
m['packing']={'method':'centered two-part coupon','front_left_keepout_mm':[0,0,18,28]}
file.write_text(json.dumps(m,indent=2)+'\n')
print('Centered two thread coupons; re-slice before using the toolpath')
