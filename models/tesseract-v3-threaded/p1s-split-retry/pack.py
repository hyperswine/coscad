#!/usr/bin/env python3
"""Separate geometry classes; all parts get 8 mm brims and >=20 mm edge inset."""
from pathlib import Path
from collections import Counter
import itertools,json
import numpy as np
import trimesh
from shapely.geometry import MultiPoint,box
from shapely.affinity import translate
HERE=Path(__file__).resolve().parent;SOURCE=HERE.parent
ring=[(x,y) for y in [64,128,192] for x in [64,128,192] if (x,y)!=(128,128)]
layouts={1:[('cap',x,y) for x,y in ring],2:[('inner',128,128)]+[('hub',x,y) for x,y in [(60,60),(196,60),(60,196),(196,196)]],3:[('hub',x,y) for x,y in [(88,88),(168,88),(88,168),(168,168)]],4:[('strut',x,y) for y in [88,128,168] for x in [88,128,168] if (x,y)!=(128,128)],5:[('bar-print',x,y) for y in [88,128,168] for x in [68,108,148,188]]}
downs={'cap':[0,0,1],'hub':[-1,-1,-1],'inner':[0,0,-1],'strut':[0,0,-1],'bar-print':[-1,0,0]}
counts=Counter();reports={}
for bed,layout in layouts.items():
 folder=HERE/('bed'+str(bed));folder.mkdir(exist_ok=True);items=[];envelopes=[];meshes=[]
 for ident,(name,cx,cy) in enumerate(layout,1):
  mesh=trimesh.load_mesh(SOURCE/(name+'.stl'));orient=trimesh.geometry.align_vectors(downs[name],[0,0,-1]);mesh.apply_transform(orient);mesh.apply_translation(-mesh.bounds[0])
  shift=np.array([cx,cy])-mesh.bounds[:,:2].mean(axis=0);mesh.apply_translation([*shift,0])
  assert mesh.is_watertight and len(mesh.split())==1 and mesh.bounds[0,2]>=-1e-6
  envelope=MultiPoint(mesh.vertices[:,:2]).convex_hull.buffer(8.5)
  assert box(20,20,236,236).covers(envelope),(bed,name,envelope.bounds)
  assert not envelope.intersects(box(0,0,18,28))
  file=f'{ident}-{name}.stl';mesh.export(folder/file);meshes.append(mesh);envelopes.append(envelope);counts[name]+=1
  items.append({'id':ident,'name':name,'file':file,'angle':0,'shift':shift.tolist(),'brim_mm':8,'center_xy_mm':[cx,cy],'bounds':mesh.bounds.tolist(),'envelope_wkt':envelope.wkt})
 min_gap=min((a.distance(b) for a,b in itertools.combinations(envelopes,2)),default=None)
 assert min_gap is None or min_gap>2,(bed,min_gap)
 trimesh.util.concatenate(meshes).export(folder/'bed.stl')
 data={'bed_index':bed,'items':items,'count':len(items),'minimum_reserved_envelope_gap_mm':min_gap,'minimum_edge_inset_mm':20,'brim_mm':8,'normal_objects_by_type':dict(Counter(i['name'] for i in items))}
 (folder/'packing.json').write_text(json.dumps(data,indent=2)+'\n');reports[bed]={k:v for k,v in data.items() if k!='items'}
 print('bed',bed,data['normal_objects_by_type'],'gap',round(min_gap,2),flush=True)
assert counts=={'hub':8,'cap':8,'inner':1,'strut':8,'bar-print':12},counts
(HERE/'layout-summary.json').write_text(json.dumps({'parts':dict(counts),'total_parts':sum(counts.values()),'beds':reports,'dispatch_authorized_beds':[1]},indent=2)+'\n')
