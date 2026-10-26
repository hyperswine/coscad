"""Prepare one support-free single-object 3MF for each specific printer."""
from pathlib import Path
import importlib.util,json,zipfile,xml.etree.ElementTree as E
import trimesh
B=Path(__file__).resolve().parent;ROOT=B.parents[1]
spec=importlib.util.spec_from_file_location('bambu_slice',ROOT/'scripts/bambu-slice.py');s=importlib.util.module_from_spec(spec);spec.loader.exec_module(s)
ns='http://schemas.microsoft.com/3dmanufacturing/core/2015/02';E.register_namespace('',ns)
def q(t):return '{'+ns+'}'+t
overrides={'wall_loops':'6','sparse_infill_density':'100%','sparse_infill_pattern':'zig-zag','top_shell_layers':'6','bottom_shell_layers':'6','enable_support':'0','support_on_build_plate_only':'1','brim_type':'outer_only','brim_width':'3','brim_object_gap':'0.1','only_one_wall_top':'0','outer_wall_speed':['35']}
for name,printer,baseline in [('monohull','P1S',B.parent/'print-in-place-globe/p1s'),('outrigger','A1',B.parent/'print-in-place-globe')]:
 folder=B/name;folder.mkdir(exist_ok=True);presets=folder/'presets';presets.mkdir(exist_ok=True)
 machine=s.machine_with_templates(f'Bambu Lab {printer} 0.4 nozzle')
 process=json.loads((baseline/'presets/process.json').read_text());filament=json.loads((baseline/'presets/filament.json').read_text());project=json.loads((baseline/'presets/project.json').read_text())
 assert all(k in process or k=='brim_type' for k in overrides)
 process.update(overrides);process['layer_height']='0.16';process['initial_layer_print_height']='0.2'
 for kind,data in [('machine',machine),('process',process),('filament',filament)]:
  (presets/(kind+'.json')).write_text(json.dumps(data,indent=2)+'\n')
  project.update({k:v for k,v in data.items() if k not in {'type','name','inherits','from','instantiation','setting_id'}})
 project.update(overrides);assert project['printer_model']==f'Bambu Lab {printer}'
 (presets/'project.json').write_text(json.dumps(project,indent=2)+'\n')
 model=E.Element(q('model'),{'unit':'millimeter'});E.SubElement(model,q('metadata'),{'name':'Application'}).text='BambuStudio-02.08.02.61';E.SubElement(model,q('metadata'),{'name':'BambuStudio:3mfVersion'}).text='1'
 resources=E.SubElement(model,q('resources'));obj=E.SubElement(resources,q('object'),{'id':'1','type':'model','name':name});mesh=E.SubElement(obj,q('mesh'));vs=E.SubElement(mesh,q('vertices'));ts=E.SubElement(mesh,q('triangles'))
 geometry=trimesh.load_mesh(B/(name+'.stl'));assert geometry.is_watertight
 for xyz in geometry.vertices:E.SubElement(vs,q('vertex'),dict(zip(['x','y','z'],map(lambda v:f'{v:.7f}',xyz))))
 for face in geometry.faces:E.SubElement(ts,q('triangle'),dict(zip(['v1','v2','v3'],map(str,face))))
 parent=E.SubElement(resources,q('object'),{'id':'2','type':'model','name':name});cs=E.SubElement(parent,q('components'));E.SubElement(cs,q('component'),{'objectid':'1'})
 build=E.SubElement(model,q('build'));E.SubElement(build,q('item'),{'objectid':'2','transform':'1 0 0 0 1 0 0 0 1 128 128 0','printable':'1'})
 config=E.Element('config');obj=E.SubElement(config,'object',{'id':'2'});E.SubElement(obj,'metadata',{'key':'name','value':name});E.SubElement(obj,'metadata',{'key':'extruder','value':'1'});part=E.SubElement(obj,'part',{'id':'1','subtype':'normal_part'});E.SubElement(part,'metadata',{'key':'name','value':name});E.SubElement(part,'metadata',{'key':'matrix','value':'1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1'})
 plate=E.SubElement(config,'plate');E.SubElement(plate,'metadata',{'key':'plater_id','value':'1'});E.SubElement(plate,'metadata',{'key':'plater_name','value':name+' buoyancy trial'});inst=E.SubElement(plate,'model_instance')
 for key,value in [('object_id','2'),('instance_id','0'),('identify_id','2')]:E.SubElement(inst,'metadata',{'key':key,'value':value})
 with zipfile.ZipFile(folder/(name+'-project.3mf'),'w',zipfile.ZIP_DEFLATED) as out:
  out.writestr('3D/3dmodel.model',E.tostring(model,encoding='utf-8',xml_declaration=True));out.writestr('Metadata/model_settings.config',E.tostring(config,encoding='utf-8',xml_declaration=True));out.writestr('Metadata/project_settings.config',json.dumps(project))
  with zipfile.ZipFile(baseline/'globe-project.3mf') as old:
   for filename in ['[Content_Types].xml','_rels/.rels']:out.writestr(filename,old.read(filename))
 print('Prepared',name,'for',printer)
