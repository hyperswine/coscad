#!/usr/bin/env python3
"""Compose the packed CoScad meshes and thread exclusions for a selected printer."""
from pathlib import Path
import argparse,importlib.util,json,zipfile,xml.etree.ElementTree as E
import numpy as np
import trimesh
PROJECT=Path(__file__).resolve().parent;ROOT=PROJECT.parents[2];SOURCE=PROJECT.parent
ap=argparse.ArgumentParser();ap.add_argument('--bed',type=int,choices=range(1,6),required=True);ap.add_argument('--printer',choices=('p1s','x1c'),default='p1s');ap.add_argument('--variant',default='');ap.add_argument('--support-base-brim',type=float);args=ap.parse_args();HERE=PROJECT/('bed'+str(args.bed))/args.variant
printer={'p1s':'Bambu Lab P1S 0.4 nozzle','x1c':'Bambu Lab X1 Carbon 0.4 nozzle'}[args.printer]
spec=importlib.util.spec_from_file_location('slicer',ROOT/'scripts/bambu-slice.py');s=importlib.util.module_from_spec(spec);spec.loader.exec_module(s)
packing=json.loads((HERE/'packing.json').read_text());assert packing['count']==len(packing['items'])
machine=s.machine_with_templates(printer)
process=s.flatten('process',machine['default_print_profile']);filament=s.flatten('filament','Generic PLA')
overrides={'wall_loops':'6','sparse_infill_density':'20%','sparse_infill_pattern':'gyroid','top_shell_layers':'5','bottom_shell_layers':'5','layer_height':'0.2','outer_wall_speed':'40','inner_wall_speed':'70','default_acceleration':'2000','outer_wall_acceleration':'1000','inner_wall_acceleration':'2000','travel_acceleration':'3000','enable_support':'1','support_type':'tree(auto)','support_on_build_plate_only':'1','support_top_z_distance':'0.24','support_bottom_z_distance':'0.24','support_object_xy_distance':'0.4','support_interface_top_layers':'2','support_interface_spacing':'0.35','tree_support_wall_count':'1','brim_type':'outer_only','brim_width':'8','brim_object_gap':'0.1'}
overrides.update(enable_support='1' if any(i['name']=='inner' for i in packing['items']) else '0',initial_layer_speed='20',initial_layer_infill_speed='25',initial_layer_acceleration='500')
if args.support_base_brim is not None:
 if not 0<=args.support_base_brim<=8:raise ValueError('Support base expansion must fit the reserved 8 mm envelope')
 overrides['raft_first_layer_expansion']=f'{args.support_base_brim:g}'
assert set(overrides)<=set(process)|{'brim_type','raft_first_layer_expansion'}
process.update(overrides)
# The CLI --load-settings process preset overrides project metadata.
process['curr_bed_type']='Textured PEI Plate'
full={}
for preset in [machine,process,filament]:full.update({k:v for k,v in preset.items() if k not in {'type','name','inherits','from','instantiation','setting_id'}})
full.update(curr_bed_type='Textured PEI Plate',printer_settings_id=printer,filament_settings_id=['Generic PLA'])
(HERE/'process.json').write_text(json.dumps(overrides,indent=2)+'\n')
for name,preset in [('machine',machine),('process',process),('filament',filament),('project',full)]:
 (HERE/'presets').mkdir(exist_ok=True);(HERE/'presets'/(name+'.json')).write_text(json.dumps(preset,indent=2)+'\n')
ns='http://schemas.microsoft.com/3dmanufacturing/core/2015/02';E.register_namespace('',ns)
def elem(parent,tag,attrs=None,text=None):
 x=E.SubElement(parent,'{'+ns+'}'+tag,attrs or {})
 if text is not None:x.text=text
 return x
root=E.Element('{'+ns+'}model',{'unit':'millimeter'});elem(root,'metadata',{'name':'Application'},'BambuStudio');elem(root,'metadata',{'name':'BambuStudio:3mfVersion'},'1')
res=elem(root,'resources');build=elem(root,'build');config=E.Element('config');ident=0;exports=[];exclusions=[]
def resource(mesh,label):
 global ident
 ident+=1;obj=elem(res,'object',{'id':str(ident),'type':'model','name':label});m=elem(obj,'mesh');vs=elem(m,'vertices');ts=elem(m,'triangles')
 for xyz in mesh.vertices:elem(vs,'vertex',dict(zip(('x','y','z'),(f'{v:.7f}' for v in xyz))))
 for tri in mesh.faces:elem(ts,'triangle',dict(zip(('v1','v2','v3'),map(str,tri))))
 return ident
for entry in packing['items']:
 name=entry['name'];mesh=trimesh.load_mesh(HERE/entry['file']);partid=resource(mesh,name);parts=[(partid,name,'normal_part')]
 if name in ('hub','inner'):
  original=trimesh.load_mesh(SOURCE/(name+'.stl'));down=[-1,-1,-1] if name=='hub' else [0,0,-1]
  orient=trimesh.geometry.align_vectors(down,[0,0,-1]);original.apply_transform(orient);zero=np.eye(4);zero[:3,3]=-original.bounds[0]
  place=trimesh.transformations.rotation_matrix(np.deg2rad(entry['angle']),[0,0,1]);place[:2,3]=entry['shift'];transform=place@zero@orient
  blocker=trimesh.load_mesh(SOURCE/(name+'-blockers.stl'));blocker.apply_transform(transform)
  parts.append((resource(blocker,name+'-thread-exclusion'),name+'-thread-exclusion','support_blocker'))
  for component in blocker.split():
   component.export(HERE/('thread-exclusion-'+str(len(exclusions))+'.stl'));exclusions.append('thread-exclusion-'+str(len(exclusions))+'.stl')
 ident+=1;parent=ident;obj=elem(res,'object',{'id':str(parent),'type':'model'});cs=elem(obj,'components')
 for child,label,kind in parts:elem(cs,'component',{'objectid':str(child)})
 elem(build,'item',{'objectid':str(parent),'transform':'1 0 0 0 1 0 0 0 1 0 0 0','printable':'1'})
 cfg=E.SubElement(config,'object',{'id':str(parent)})
 settings={'name':str(entry['id'])+'-'+name,'extruder':'1','brim_type':'outer_only','brim_width':str(entry['brim_mm']),'enable_support':'1' if name=='inner' else '0'}
 for k,v in settings.items():E.SubElement(cfg,'metadata',{'key':k,'value':v})
 for child,label,kind in parts:
  part=E.SubElement(cfg,'part',{'id':str(child),'subtype':kind})
  E.SubElement(part,'metadata',{'key':'name','value':label});E.SubElement(part,'metadata',{'key':'matrix','value':'1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1'})
 exports.append({'parent':parent,'normal_part':partid,'label':settings['name'],'brim_mm':entry['brim_mm'],'enable_support':name=='inner','bounds':mesh.bounds.tolist()})
plate=E.SubElement(config,'plate')
for k,v in [('plater_id','1'),('plater_name','Tesseract split retry bed '+str(args.bed)),('locked','false')]:E.SubElement(plate,'metadata',{'key':k,'value':v})
for e in exports:
 inst=E.SubElement(plate,'model_instance')
 for k,v in [('object_id',str(e['parent'])),('instance_id','0'),('identify_id',str(e['parent']))]:E.SubElement(inst,'metadata',{'key':k,'value':v})
with zipfile.ZipFile(HERE/'full-project.3mf','w',zipfile.ZIP_DEFLATED) as z:
 z.writestr('3D/3dmodel.model',E.tostring(root,encoding='utf-8',xml_declaration=True));z.writestr('Metadata/model_settings.config',E.tostring(config,encoding='utf-8',xml_declaration=True));z.writestr('Metadata/project_settings.config',json.dumps(full))
 with zipfile.ZipFile(SOURCE/'p1s-thread-fit/thread-fit_bed1.gcode.3mf') as template:
  for name in ['[Content_Types].xml','_rels/.rels']:z.writestr(name,template.read(name))
(HERE/'project-parts.json').write_text(json.dumps({'parts':exports,'thread_exclusions':exclusions},indent=2)+'\n')
print('Prepared bed '+str(args.bed)+': '+str(packing['count'])+' objects, all 8 mm brims',flush=True)
