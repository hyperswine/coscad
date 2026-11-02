#!/usr/bin/env python3
"""Prepare this batch without dispatch; refuse existing run records."""
from pathlib import Path
import hashlib,importlib.util,json,subprocess,sys
import numpy as np
import trimesh
from shapely.geometry import MultiPoint,box
root=Path(__file__).resolve().parents[4]
split=root/"models/tesseract-v3-threaded/p1s-split-retry"
# Other two plates use unmodified existing tall-part meshes.
batch=split/'batch-20261101';batch.mkdir(exist_ok=True)
spec=importlib.util.spec_from_file_location('slicer',root/'scripts/bambu-slice.py');slicer=importlib.util.module_from_spec(spec);spec.loader.exec_module(slicer)
for printer,name,layout in [('p1s','three-struts-one-bar',[('strut',55,40),('strut',201,97.5),('strut',55,155),('bar-print',201,212.5)]),('a1','three-bars',[('bar-print',55,50),('bar-print',201,128),('bar-print',55,206)])]:
 f=batch/name;f.mkdir(exist_ok=True);assert not (f/'print-run.json').exists()
 machine=slicer.machine_with_templates({'p1s':'Bambu Lab P1S 0.4 nozzle','a1':'Bambu Lab A1 0.4 nozzle'}[printer])
 limits={k:float(machine[k]) for k in ['extruder_clearance_height_to_lid','extruder_clearance_height_to_rod','extruder_clearance_max_radius','extruder_clearance_dist_to_rod','extruder_clearance_radius','printable_height']}
 items=[];bodies=[]
 for ident,(kind,x,y) in enumerate(layout,1):
  source=split/('bed4/1-strut.stl' if kind=='strut' else 'bed5/1-bar-print.stl');mesh=trimesh.load_mesh(source);shift=np.array([x,y])-mesh.bounds[:,:2].mean(axis=0);mesh.apply_translation([*shift,0]);hull=MultiPoint(mesh.vertices[:,:2]).convex_hull;env=hull.buffer(8.5)
  assert box(20,20,236,236).contains(env) and not env.intersects(box(0,0,18,28))
  file=f'{ident}-{kind}.stl';mesh.export(f/file);items.append({'id':ident,'name':kind,'file':file,'angle':0,'shift':shift.tolist(),'brim_mm':8,'center_xy_mm':[x,y],'bounds':mesh.bounds.tolist(),'envelope_wkt':env.wkt,'source':str(source.relative_to(root)),'source_sha256':hashlib.sha256(source.read_bytes()).hexdigest()});bodies.append(hull)
 for i,a in enumerate(items[:-1]):
  assert a['bounds'][1][2]+5<limits['extruder_clearance_height_to_lid']
  for j,b in enumerate(items[i+1:],i+1):
   assert bodies[i].distance(bodies[j])>limits['extruder_clearance_max_radius']+5
   assert b['bounds'][0][1]-a['bounds'][1][1]>limits['extruder_clearance_dist_to_rod']+5
 packing={'bed_index':4,'label':f'Tesseract {printer} sequential {name}','count':len(items),'items':items,'brim_mm':8,'normal_objects_by_type':{k:sum(i['name']==k for i in items) for k in set(i['name'] for i in items)}}
 (f/'packing.json').write_text(json.dumps(packing,indent=2)+'\n')
 (f/'sequence-process.json').write_text(json.dumps({'print_sequence':'by object'},indent=2)+'\n')
 (f/'sequence-filament.json').write_text(json.dumps({'slow_down_layer_time':['12'],'slow_down_min_speed':['5'],'slow_down_for_layer_cooling':['1']},indent=2)+'\n')
 (f/'plan.json').write_text(json.dumps({'printer':printer,'print_sequence':'by object','objects':[i['name'] for i in items],'unmodified_machine_clearance':limits,'bed_clear_owner_confirmed':True,'dispatched':False},indent=2)+'\n')
 subprocess.run([sys.executable,str(split/'prepare.py'),'--bed','4','--printer',printer,'--output-dir',str(f),'--process-overrides',str(f/'sequence-process.json'),'--filament-overrides',str(f/'sequence-filament.json')]+(['--end-clearance-mm','5'] if printer=='a1' else []),check=True)
print('Prepared two sequential plate recipes; no printer commands sent')
