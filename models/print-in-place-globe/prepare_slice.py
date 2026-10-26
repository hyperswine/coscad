"""Build a Bambu multipart 3MF with a bearing support blocker and fixed orientation."""
from pathlib import Path
import importlib.util,json,zipfile,xml.etree.ElementTree as E
import trimesh
B=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('bambu_slice',str(B.parents[1]/'scripts/bambu-slice.py'));s=importlib.util.module_from_spec(spec);spec.loader.exec_module(s)
process={
 'wall_loops':'3','sparse_infill_density':'12%','sparse_infill_pattern':'gyroid',
 'top_shell_layers':'5','bottom_shell_layers':'4',
 'brim_type':'outer_only','brim_width':'4','brim_object_gap':'0.1',
 'enable_support':'1','support_type':'tree(auto)','support_style':'default',
 'support_on_build_plate_only':'0','support_threshold_angle':'35',
 'support_top_z_distance':'0.2','support_bottom_z_distance':'0.2',
 'support_object_xy_distance':'0.35','support_interface_top_layers':'3',
 'support_interface_spacing':'0.25','tree_support_wall_count':'2',
 'enable_prime_tower':'0','initial_layer_speed':['25'],'initial_layer_infill_speed':['35'],
 'initial_layer_acceleration':['300'],'initial_layer_travel_acceleration':['1500'],
 'outer_wall_speed':['55'],'inner_wall_speed':['80'],'sparse_infill_speed':['100'],
 'internal_solid_infill_speed':['80'],'top_surface_speed':['45'],'gap_infill_speed':['45'],
 'default_acceleration':['2000'],'outer_wall_acceleration':['1000'],'inner_wall_acceleration':['2000'],'travel_acceleration':['3000']}
filament={'close_fan_the_first_x_layers':['3'],'full_fan_speed_layer':['5']}
(B/'process-overrides.json').write_text(json.dumps(process,indent=2)+'\n');(B/'filament-overrides.json').write_text(json.dumps(filament,indent=2)+'\n')
presets=s.write_presets('Bambu Lab A1 0.4 nozzle','0.16mm Optimal @BBL A1','Bambu PLA Basic @BBL A1','Textured PEI Plate',str(B/'process-overrides.json'),str(B/'filament-overrides.json'))
P=B/'presets';P.mkdir(exist_ok=True)
for k,v in presets.items():(P/(k+'.json')).write_bytes(Path(v).read_bytes())
ns='http://schemas.microsoft.com/3dmanufacturing/core/2015/02';E.register_namespace('',ns)
def elem(parent,tag,attrs=None,text=None):
 x=E.SubElement(parent,'{'+ns+'}'+tag,attrs or {})
 if text is not None:x.text=text
 return x
root=E.Element('{'+ns+'}model',{'unit':'millimeter','{http://www.w3.org/XML/1998/namespace}lang':'en-US'})
elem(root,'metadata',{'name':'Application'},'BambuStudio-02.08.02.61');elem(root,'metadata',{'name':'BambuStudio:3mfVersion'},'1')
resources=elem(root,'resources')
for i,name in enumerate(['stand','rotor','bearing-blocker'],1):
 m=trimesh.load_mesh(B/(name+'.stl'))
 ob=elem(resources,'object',{'id':str(i),'type':'model','name':name});me=elem(ob,'mesh');vs=elem(me,'vertices');ts=elem(me,'triangles')
 for xyz in m.vertices:elem(vs,'vertex',dict(zip(('x','y','z'),(f'{x:.7f}' for x in xyz))))
 for f in m.faces:elem(ts,'triangle',dict(zip(('v1','v2','v3'),map(str,f))))
a=elem(resources,'object',{'id':'4','type':'model','name':'Print-in-place globe'});cs=elem(a,'components')
for i in range(1,4):elem(cs,'component',{'objectid':str(i)})
elem(elem(root,'build'),'item',{'objectid':'4','transform':'1 0 0 0 1 0 0 0 1 128 128 0','printable':'1'})
config=E.Element('config');obj=E.SubElement(config,'object',{'id':'4'})
E.SubElement(obj,'metadata',{'key':'name','value':'Print-in-place globe'});E.SubElement(obj,'metadata',{'key':'extruder','value':'1'})
for i,name in enumerate(['stand','rotor','bearing-blocker'],1):
 part=E.SubElement(obj,'part',{'id':str(i),'subtype':'support_blocker' if i==3 else 'normal_part'})
 E.SubElement(part,'metadata',{'key':'name','value':name})
 E.SubElement(part,'metadata',{'key':'matrix','value':'1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1'})
plate=E.SubElement(config,'plate')
for k,v in [('plater_id','1'),('plater_name','Captive globe'),('locked','false')]:E.SubElement(plate,'metadata',{'key':k,'value':v})
inst=E.SubElement(plate,'model_instance')
for k,v in [('object_id','4'),('instance_id','0'),('identify_id','4')]:E.SubElement(inst,'metadata',{'key':k,'value':v})
with zipfile.ZipFile(B/'globe-project.3mf','w',zipfile.ZIP_DEFLATED) as z:
 z.writestr('3D/3dmodel.model',E.tostring(root,encoding='utf-8',xml_declaration=True))
 z.writestr('Metadata/model_settings.config',E.tostring(config,encoding='utf-8',xml_declaration=True))
 full=json.loads((P/'base-project.json').read_text())
 full.update(process);full.update(filament);full['filament_colour']=['#F4EE2A']
 z.writestr('Metadata/project_settings.config',json.dumps(full))
 (P/'project.json').write_text(json.dumps(full,indent=2)+'\n')
 z.writestr('[Content_Types].xml','<?xml version="1.0"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="model" ContentType="application/vnd.ms-package.3dmanufacturing-3dmodel+xml"/></Types>')
 z.writestr('_rels/.rels','<?xml version="1.0"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Target="/3D/3dmodel.model" Id="rel0" Type="http://schemas.microsoft.com/3dmanufacturing/2013/01/3dmodel"/></Relationships>')
print('Prepared globe-project.3mf with two moving/standing parts and one SupportBlocker')
