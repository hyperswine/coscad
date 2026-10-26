"""Four support-free single-object 3MF projects: P1S and A1, each in PLA and PETG.
Presets follow twin-boat-trials (proven walls/brim/speeds); PETG uses Bambu's
flattened system 'Bambu PETG HF' preset for each printer."""
from pathlib import Path
import importlib.util, json, zipfile, xml.etree.ElementTree as E
import trimesh
B = Path(__file__).resolve().parent; ROOT = B.parents[1]
spec = importlib.util.spec_from_file_location('bambu_slice', ROOT/'scripts/bambu-slice.py'); s = importlib.util.module_from_spec(spec); spec.loader.exec_module(s)
ns = 'http://schemas.microsoft.com/3dmanufacturing/core/2015/02'; E.register_namespace('', ns)
def q(t): return '{'+ns+'}'+t
NAME = 'beach-outrigger'
# The P1S excludes an 18 x 28 mm front-left corner; the hull's bounding box clears it when shifted.
PLACEMENT = {'P1S': (134, 136), 'X1C': (134, 136), 'A1': (128, 128)}
MACHINE = {'P1S': 'Bambu Lab P1S 0.4 nozzle', 'A1': 'Bambu Lab A1 0.4 nozzle', 'X1C': 'Bambu Lab X1 Carbon 0.4 nozzle'}
MODEL = {'P1S': 'Bambu Lab P1S', 'A1': 'Bambu Lab A1', 'X1C': 'Bambu Lab X1 Carbon'}
overrides = {'wall_loops':'6','sparse_infill_density':'100%','sparse_infill_pattern':'zig-zag','top_shell_layers':'6','bottom_shell_layers':'6',
             'enable_support':'0','support_on_build_plate_only':'1','brim_type':'outer_only','brim_width':'4','brim_object_gap':'0.1','only_one_wall_top':'0','outer_wall_speed':['35']}
JOBS = {'p1s-pla': ('P1S', 'Bambu PLA Basic @BBL P1S 0.4 nozzle'), 'p1s-petg': ('P1S', 'Bambu PETG HF @BBL P1S 0.4 nozzle'),
        'a1-pla': ('A1', 'Bambu PLA Basic @BBL A1'), 'a1-petg': ('A1', 'Bambu PETG HF @BBL A1'),
        'x1c-pla': ('X1C', 'Bambu PLA Basic @BBL X1C'), 'x1c-petg': ('X1C', 'Bambu PETG HF @BBL X1C')}
geometry = trimesh.load_mesh(B/(NAME+'.stl')); assert geometry.is_watertight
import sys
for job, (printer, filament_name) in JOBS.items():
    if sys.argv[1:] and job not in sys.argv[1:]: continue
    folder = B/job; folder.mkdir(exist_ok=True); presets = folder/'presets'; presets.mkdir(exist_ok=True)
    baseline = B.parent/'print-in-place-globe'/('.' if printer == 'A1' else 'p1s')   # the P1S baseline process is 0.16mm Optimal @BBL X1C, valid for both enclosed printers
    machine = s.machine_with_templates(MACHINE[printer])
    process = json.loads((baseline/'presets/process.json').read_text()); project = json.loads((baseline/'presets/project.json').read_text())
    filament = s.flatten('filament', filament_name)
    assert all(k in process or k == 'brim_type' for k in overrides)
    process.update(overrides); process['layer_height'] = '0.2'; process['initial_layer_print_height'] = '0.2'
    for kind, data in [('machine', machine), ('process', process), ('filament', filament)]:
        (presets/(kind+'.json')).write_text(json.dumps(data, indent=2)+'\n')
        project.update({k: v for k, v in data.items() if k not in {'type','name','inherits','from','instantiation','setting_id'}})
    project.update(overrides); project['filament_settings_id'] = [filament_name]; project['printer_settings_id'] = MACHINE[printer]; assert project['printer_model'] == MODEL[printer], project['printer_model']
    (presets/'project.json').write_text(json.dumps(project, indent=2)+'\n')
    model = E.Element(q('model'), {'unit':'millimeter'}); E.SubElement(model, q('metadata'), {'name':'Application'}).text = 'BambuStudio-02.08.02.61'; E.SubElement(model, q('metadata'), {'name':'BambuStudio:3mfVersion'}).text = '1'
    resources = E.SubElement(model, q('resources')); obj = E.SubElement(resources, q('object'), {'id':'1','type':'model','name':NAME}); mesh = E.SubElement(obj, q('mesh')); vs = E.SubElement(mesh, q('vertices')); ts = E.SubElement(mesh, q('triangles'))
    for xyz in geometry.vertices: E.SubElement(vs, q('vertex'), dict(zip(['x','y','z'], map(lambda v: f'{v:.7f}', xyz))))
    for face in geometry.faces: E.SubElement(ts, q('triangle'), dict(zip(['v1','v2','v3'], map(str, face))))
    parent = E.SubElement(resources, q('object'), {'id':'2','type':'model','name':NAME}); cs = E.SubElement(parent, q('components')); E.SubElement(cs, q('component'), {'objectid':'1'})
    build = E.SubElement(model, q('build')); E.SubElement(build, q('item'), {'objectid':'2','transform':f'1 0 0 0 1 0 0 0 1 {PLACEMENT[printer][0]} {PLACEMENT[printer][1]} 0','printable':'1'})
    config = E.Element('config'); obj = E.SubElement(config, 'object', {'id':'2'}); E.SubElement(obj, 'metadata', {'key':'name','value':NAME}); E.SubElement(obj, 'metadata', {'key':'extruder','value':'1'}); part = E.SubElement(obj, 'part', {'id':'1','subtype':'normal_part'}); E.SubElement(part, 'metadata', {'key':'name','value':NAME}); E.SubElement(part, 'metadata', {'key':'matrix','value':'1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1'})
    plate = E.SubElement(config, 'plate'); E.SubElement(plate, 'metadata', {'key':'plater_id','value':'1'}); E.SubElement(plate, 'metadata', {'key':'plater_name','value':f'{NAME} {job}'}); inst = E.SubElement(plate, 'model_instance')
    for key, value in [('object_id','2'),('instance_id','0'),('identify_id','2')]: E.SubElement(inst, 'metadata', {'key':key,'value':value})
    with zipfile.ZipFile(folder/(NAME+'-project.3mf'), 'w', zipfile.ZIP_DEFLATED) as out:
        out.writestr('3D/3dmodel.model', E.tostring(model, encoding='utf-8', xml_declaration=True)); out.writestr('Metadata/model_settings.config', E.tostring(config, encoding='utf-8', xml_declaration=True)); out.writestr('Metadata/project_settings.config', json.dumps(project))
        with zipfile.ZipFile(B.parent/'print-in-place-globe/globe-project.3mf') as old:
            for filename in ['[Content_Types].xml', '_rels/.rels']: out.writestr(filename, old.read(filename))
    print('Prepared', job, printer, filament_name)
