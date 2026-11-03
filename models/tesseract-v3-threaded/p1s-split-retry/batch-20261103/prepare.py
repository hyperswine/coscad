from pathlib import Path
import hashlib,importlib.util,json,subprocess,sys,itertools
import numpy as np,trimesh
from shapely.geometry import MultiPoint,box
ROOT=Path(__file__).resolve().parents[4];SPLIT=ROOT/'models/tesseract-v3-threaded/p1s-split-retry';B=SPLIT/'batch-20261103';B.mkdir(exist_ok=True)
s=importlib.util.spec_from_file_location('slicer',ROOT/'scripts/bambu-slice.py');slicer=importlib.util.module_from_spec(s);s.loader.exec_module(slicer)
for device in sys.argv[1:]:
 centers=[(55,40),(201,97.5),(55,155),(201,212.5)] if device!='a1' else [(55,50),(201,128),(55,206)]
 f=B/device;f.mkdir(exist_ok=True);assert not (f/'print-run.json').exists(),'Do not overwrite a dispatched job'
 machine=slicer.machine_with_templates({'x1c':'Bambu Lab X1 Carbon 0.4 nozzle','p1s':'Bambu Lab P1S 0.4 nozzle','a1':'Bambu Lab A1 0.4 nozzle'}[device]);keys=['extruder_clearance_height_to_lid','extruder_clearance_height_to_rod','extruder_clearance_max_radius','extruder_clearance_dist_to_rod','extruder_clearance_radius','printable_height'];limits={k:float(machine[k]) for k in keys}
 items=[];hulls=[];kind='bar-print' if device=='a1' else 'strut';source=SPLIT/('bed5/1-bar-print.stl' if device=='a1' else 'bed4/1-strut.stl')
 for i,(x,y) in enumerate(centers,1):
  mesh=trimesh.load_mesh(source);shift=np.array([x,y])-mesh.bounds[:,:2].mean(axis=0);mesh.apply_translation([*shift,0]);hull=MultiPoint(mesh.vertices[:,:2]).convex_hull;env=hull.buffer(8.5)
  assert mesh.is_watertight and len(mesh.split())==1
  assert box(20,20,236,236).contains(env) and not env.intersects(box(0,0,18,28))
  name=f'{i}-{kind}.stl';mesh.export(f/name);items.append(dict(id=i,name=kind,file=name,angle=0,shift=shift.tolist(),center_xy_mm=[x,y],brim_mm=8,bounds=mesh.bounds.tolist(),envelope_wkt=env.wkt,source=str(source.relative_to(ROOT)),source_sha256=hashlib.sha256(source.read_bytes()).hexdigest()));hulls.append(hull)
 for i,a in enumerate(items[:-1]):
  assert a['bounds'][1][2]+5<limits['extruder_clearance_height_to_lid']
  for j,b in enumerate(items[i+1:],i+1):
   assert hulls[i].distance(hulls[j])>limits['extruder_clearance_max_radius']+5
   assert b['bounds'][0][1]-a['bounds'][1][1]>limits['extruder_clearance_dist_to_rod']+5
 (f/'packing.json').write_text(json.dumps(dict(bed_index=4,label=f'Tesseract III {device} sequential diagonal struts 20261103',items=items,count=len(items),brim_mm=8,normal_objects_by_type={kind:len(items)}),indent=2)+'\n')
 (f/'sequence-process.json').write_text(json.dumps({'print_sequence':'by object'})+'\n');(f/'sequence-filament.json').write_text(json.dumps({'slow_down_layer_time':['12'],'slow_down_min_speed':['5'],'slow_down_for_layer_cooling':['1']})+'\n')
 (f/'plan.json').write_text(json.dumps(dict(printer=device,print_sequence='by object',objects=[kind]*len(items),centers_xy_mm=centers,unmodified_machine_clearance=limits,dispatched=False),indent=2)+'\n')
 subprocess.run([sys.executable,str(SPLIT/'prepare.py'),'--bed','4','--printer',device,'--output-dir',str(f),'--process-overrides',str(f/'sequence-process.json'),'--filament-overrides',str(f/'sequence-filament.json'),'--end-clearance-mm','5'],check=True)
print('Prepared per-printer files; no printer controls sent')
