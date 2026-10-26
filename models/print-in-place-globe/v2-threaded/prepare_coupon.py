"""Make a small, support-free P1S fit test using the identical mating threads."""
from pathlib import Path
import json
import zipfile
import xml.etree.ElementTree as E
import trimesh

B = Path(__file__).resolve().parent
ns = 'http://schemas.microsoft.com/3dmanufacturing/core/2015/02'
E.register_namespace('', ns)
q = lambda name: '{'+ns+'}'+name
with zipfile.ZipFile(B/'globe-project.3mf') as z:
    entries = {i: z.read(i) for i in z.namelist()}
root = E.fromstring(entries['3D/3dmodel.model'])
resources, build = root.find(q('resources')), root.find(q('build'))
resources.clear()
build.clear()
config = E.Element('config')
for i, name, cx in [(1, 'coupon-female', 100), (2, 'coupon-male', 156)]:
    m = trimesh.load_mesh(B/(name+'.stl'))
    ob = E.SubElement(resources, q('object'), {'id': str(i), 'type': 'model', 'name': name})
    mesh = E.SubElement(ob, q('mesh'))
    vs, ts = E.SubElement(mesh, q('vertices')), E.SubElement(mesh, q('triangles'))
    for xyz in m.vertices:
        E.SubElement(vs, q('vertex'), dict(zip(('x', 'y', 'z'), (f'{v:.7f}' for v in xyz))))
    for face in m.faces:
        E.SubElement(ts, q('triangle'), dict(zip(('v1', 'v2', 'v3'), map(str, face))))
    parent = E.SubElement(resources, q('object'), {'id': str(i+2), 'type': 'model', 'name': name})
    cs = E.SubElement(parent, q('components'))
    E.SubElement(cs, q('component'), {'objectid': str(i)})
    E.SubElement(build, q('item'), {'objectid': str(i+2), 'transform': f'1 0 0 0 1 0 0 0 1 {cx} 128 0', 'printable': '1'})
    obj = E.SubElement(config, 'object', {'id': str(i+2)})
    E.SubElement(obj, 'metadata', {'key': 'name', 'value': name})
    E.SubElement(obj, 'metadata', {'key': 'extruder', 'value': '1'})
    part = E.SubElement(obj, 'part', {'id': str(i), 'subtype': 'normal_part'})
    E.SubElement(part, 'metadata', {'key': 'name', 'value': name})
    E.SubElement(part, 'metadata', {'key': 'matrix', 'value': '1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1'})
plate = E.SubElement(config, 'plate')
for k, v in [('plater_id', '1'), ('plater_name', 'Thread fit test'), ('locked', 'false')]:
    E.SubElement(plate, 'metadata', {'key': k, 'value': v})
for i in [3, 4]:
    instance = E.SubElement(plate, 'model_instance')
    for k, v in [('object_id', str(i)), ('instance_id', '0'), ('identify_id', str(i))]:
        E.SubElement(instance, 'metadata', {'key': k, 'value': v})
settings = json.loads(entries['Metadata/project_settings.config'])
settings['enable_support'] = '0'
entries['Metadata/project_settings.config'] = json.dumps(settings).encode()
entries['3D/3dmodel.model'] = E.tostring(root, encoding='utf-8', xml_declaration=True)
entries['Metadata/model_settings.config'] = E.tostring(config, encoding='utf-8', xml_declaration=True)
with zipfile.ZipFile(B/'thread-coupon-project.3mf', 'w', zipfile.ZIP_DEFLATED) as z:
    for name, data in entries.items():
        z.writestr(name, data)
process = json.loads((B/'presets/process.json').read_text())
process['enable_support'] = '0'
(B/'presets/process-coupon.json').write_text(json.dumps(process, indent=2)+'\n')
print('Prepared support-free pair of identical thread test pieces')
