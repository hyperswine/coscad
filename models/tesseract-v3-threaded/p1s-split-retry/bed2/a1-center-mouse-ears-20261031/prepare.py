#!/usr/bin/env python3
"""Single unchanged inner frame, reinforced trees and removable CoScad mouse-ear pads."""
from pathlib import Path
import hashlib,json,shutil,subprocess,sys,zipfile
import numpy as np
import trimesh
from shapely.geometry import LineString
from shapely.ops import polygonize
HERE=Path(__file__).resolve().parent
SPLIT=HERE.parents[1]

def main():
 if (HERE/'print-run.json').exists():raise RuntimeError('Dispatched variant is immutable; prepare a new retry')
 source=HERE.parent/'1-inner.stl'
 packing=json.loads((HERE.parent/'packing.json').read_text())
 item=next(i for i in packing['items'] if i['name']=='inner')
 item['brim_type']='no_brim'
 packing.update(items=[item],count=1,normal_objects_by_type={'inner':1},label='Tesseract center only - A1 reinforced trees and painted ears')
 (HERE/'packing.json').write_text(json.dumps(packing,indent=2)+'\n');shutil.copyfile(source,HERE/item['file'])
 process={'brim_type':'no_brim','brim_width':'8','brim_object_gap':'0.1',
  'tree_support_wall_count':'2','tree_support_branch_diameter':'3','tree_support_branch_angle':'35',
  'support_top_z_distance':'0.16','support_bottom_z_distance':'0.16',
  'support_object_xy_distance':'0.3','support_interface_top_layers':'3','support_interface_bottom_layers':'3',
  'support_interface_spacing':'0.25','support_speed':['60','60'],'support_interface_speed':['35','35'],
  'raft_first_layer_expansion':'1','reduce_crossing_wall':'1','avoid_crossing_wall_includes_support':'1'}
 (HERE/'support-process.json').write_text(json.dumps(process,indent=2)+'\n')
 filament={'slow_down_layer_time':['12'],'slow_down_min_speed':['5'],'slow_down_for_layer_cooling':['1']}
 (HERE/'cooling-filament.json').write_text(json.dumps(filament,indent=2)+'\n')
 subprocess.run([sys.executable,str(SPLIT/'prepare.py'),'--bed','2','--printer','a1','--output-dir',str(HERE),'--process-overrides',str(HERE/'support-process.json'),'--filament-overrides',str(HERE/'cooling-filament.json')],check=True)
 mesh=trimesh.load_mesh(source)
 seg=trimesh.intersections.mesh_plane(mesh,[0,0,1],[0,0,.1])
 patches=sorted([p for p in polygonize([LineString(x[:,:2]) for x in np.round(seg,5)]) if p.area>.1],key=lambda p:(round(p.centroid.x,2),round(p.centroid.y,2)))
 assert len(patches)==4
 points=[dict(x=p.centroid.x,y=p.centroid.y,z=0,radius_mm=7.5,volume_idx=0) for p in patches]
 # The native CLI retained painted annotations without emitting ears. Use
 # explicit CoScad first-layer pads as four additional normal volumes instead.
 import xml.etree.ElementTree as E
 subprocess.run([str(SPLIT.parents[2]/'bin/coscad'),'stl',str(HERE/'mouse-ear.coscad')],check=True)
 pad=trimesh.load_mesh(HERE/'mouse-ear.stl')
 assert pad.is_watertight and abs(pad.bounds[0,2])<1e-6 and abs(pad.bounds[1,2]-.2)<1e-6
 project=HERE/'full-project.3mf'
 with zipfile.ZipFile(project) as z:entries={i.filename:(i,z.read(i)) for i in z.infolist()}
 ns='http://schemas.microsoft.com/3dmanufacturing/core/2015/02'
 E.register_namespace('',ns)
 model=E.fromstring(entries['3D/3dmodel.model'][1]);resources=model.find('{'+ns+'}resources')
 parent=model.find('{'+ns+'}resources/{'+ns+'}object[@id="3"]/{'+ns+'}components')
 config=E.fromstring(entries['Metadata/model_settings.config'][1]);cfg=config.find('object')
 extra=[]
 for index,point in enumerate(points,1):
  shape=pad.copy();shape.apply_translation([point['x'],point['y'],0]);file=f'mouse-ear-{index}.stl';shape.export(HERE/file)
  ident=str(3+index);name=f'mouse-ear-{index}'
  obj=E.SubElement(resources,'{'+ns+'}object',{'id':ident,'type':'model','name':name});m=E.SubElement(obj,'{'+ns+'}mesh');vs=E.SubElement(m,'{'+ns+'}vertices');ts=E.SubElement(m,'{'+ns+'}triangles')
  for xyz in shape.vertices:E.SubElement(vs,'{'+ns+'}vertex',dict(zip(('x','y','z'),(f'{x:.7f}' for x in xyz))))
  for tri in shape.faces:E.SubElement(ts,'{'+ns+'}triangle',dict(zip(('v1','v2','v3'),map(str,tri))))
  E.SubElement(parent,'{'+ns+'}component',{'objectid':ident})
  part=E.SubElement(cfg,'part',{'id':ident,'subtype':'normal_part'})
  E.SubElement(part,'metadata',{'key':'name','value':name});E.SubElement(part,'metadata',{'key':'matrix','value':'1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1'})
  extra.append({'name':name,'file':file,'center_xy_mm':[point['x'],point['y']],'radius_mm':7.5,'height_mm':.2,'bounds':shape.bounds.tolist(),'volume_mm3':shape.volume})
 entries['3D/3dmodel.model']=(entries['3D/3dmodel.model'][0],E.tostring(model,encoding='utf-8',xml_declaration=True))
 entries['Metadata/model_settings.config']=(entries['Metadata/model_settings.config'][0],E.tostring(config,encoding='utf-8',xml_declaration=True))
 with zipfile.ZipFile(project,'w',zipfile.ZIP_DEFLATED) as z:
  for info,data in entries.values():z.writestr(info,data)
 parts=json.loads((HERE/'project-parts.json').read_text());parts['adhesion_pads']=extra;(HERE/'project-parts.json').write_text(json.dumps(parts,indent=2)+'\n')
 (HERE/'plan.json').write_text(json.dumps({'printer':'a1','parts':['inner'],'source_mesh':str(source),'source_sha256':hashlib.sha256(source.read_bytes()).hexdigest(),'geometry_changed':False,'source_orientation_retained':True,'mouse_ear_pads':extra,'adhesion_method':'four explicit CoScad 0.2 mm removable mouse-ear pads; native painted-ear CLI produced no paths','contact_patch_areas_at_z_0_1_mm':[p.area for p in patches],'process_overrides':process,'thread_bore_blockers_retained':True,'bed_clear_owner_confirmed':True,'physical_ams_slot':1,'support_removal_note':'Closer and denser PLA interfaces may require more care to remove; lower diagonal quality is unproven until printed.'},indent=2)+'\n')
 print('Prepared unchanged center with four removable first-layer mouse-ear pads and reinforced trees')
if __name__=='__main__':main()
