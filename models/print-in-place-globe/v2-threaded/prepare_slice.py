"""Two objects on one P1S plate; modifiers exclude both bearing and socket."""
from pathlib import Path
import importlib.util
import json
import zipfile
import xml.etree.ElementTree as E
import trimesh

B = Path(__file__).resolve().parent
BASELINE = B.parent/'p1s'
spec = importlib.util.spec_from_file_location('bambu_slice', B.parents[2]/'scripts/bambu-slice.py')
s = importlib.util.module_from_spec(spec)
spec.loader.exec_module(s)
overrides = {
    'wall_loops': '6', 'sparse_infill_density': '20%', 'sparse_infill_pattern': 'gyroid',
    'top_shell_layers': '6', 'bottom_shell_layers': '6',
    'support_on_build_plate_only': '1', 'support_type': 'tree(auto)',
    'support_top_z_distance': '0.24', 'support_bottom_z_distance': '0.24',
    'support_object_xy_distance': '0.4', 'support_interface_top_layers': '2',
    'support_interface_spacing': '0.35', 'tree_support_wall_count': '1',
    'brim_type': 'outer_only', 'brim_width': '5', 'brim_object_gap': '0.1',
    'only_one_wall_top': '0', 'outer_wall_speed': ['40'],
}
# Start from the exact fully populated project used for the successful print.
# Refresh the installed machine/template and filament presets, retaining its
# deliberately conservative speed/cooling settings before applying this delta.
machine = s.machine_with_templates('Bambu Lab P1S 0.4 nozzle')
process = json.loads((BASELINE/'presets/process.json').read_text())
assert set(overrides) <= set(process)
process.update(overrides)
filament = json.loads((BASELINE/'presets/filament.json').read_text())
P = B/'presets'
P.mkdir(exist_ok=True)
full = json.loads((BASELINE/'presets/project.json').read_text())
for kind, value in [('machine', machine), ('process', process), ('filament', filament)]:
    (P/(kind+'.json')).write_text(json.dumps(value, indent=2)+'\n')
    full.update({k: v for k, v in value.items() if k not in
                 {'type', 'name', 'inherits', 'from', 'instantiation', 'setting_id'}})
full.update(overrides)
assert full['printer_model'] == 'Bambu Lab P1S'
(P/'project.json').write_text(json.dumps(full, indent=2)+'\n')
(B/'process-overrides.json').write_text(json.dumps(overrides, indent=2)+'\n')

ns = 'http://schemas.microsoft.com/3dmanufacturing/core/2015/02'
E.register_namespace('', ns)


def elem(parent, tag, attrs=None, text=None):
    x = E.SubElement(parent, '{'+ns+'}'+tag, attrs or {})
    if text is not None:
        x.text = text
    return x


root = E.Element('{'+ns+'}model', {'unit': 'millimeter', '{http://www.w3.org/XML/1998/namespace}lang': 'en-US'})
elem(root, 'metadata', {'name': 'Application'}, 'BambuStudio-02.08.02.61')
elem(root, 'metadata', {'name': 'BambuStudio:3mfVersion'}, '1')
resources = elem(root, 'resources')
for i, name in enumerate(['gimbal', 'rotor', 'support-blocker', 'base'], 1):
    m = trimesh.load_mesh(B/(name+'.stl'))
    ob = elem(resources, 'object', {'id': str(i), 'type': 'model', 'name': name})
    mesh = elem(ob, 'mesh')
    vs, ts = elem(mesh, 'vertices'), elem(mesh, 'triangles')
    for xyz in m.vertices:
        elem(vs, 'vertex', dict(zip(('x', 'y', 'z'), (f'{x:.7f}' for x in xyz))))
    for f in m.faces:
        elem(ts, 'triangle', dict(zip(('v1', 'v2', 'v3'), map(str, f))))
for ident, names in [(5, [1, 2, 3]), (6, [4])]:
    ob = elem(resources, 'object', {'id': str(ident), 'type': 'model'})
    cs = elem(ob, 'components')
    for i in names:
        elem(cs, 'component', {'objectid': str(i)})
build = elem(root, 'build')
for ident, cx in [(5, 78), (6, 178)]:
    elem(build, 'item', {'objectid': str(ident), 'transform': f'1 0 0 0 1 0 0 0 1 {cx} 128 0', 'printable': '1'})

config = E.Element('config')
for ident, names, label in [(5, [(1, 'gimbal'), (2, 'rotor'), (3, 'support-blocker')], 'Captive globe and gimbal'),
                            (6, [(4, 'base')], 'Screw-on base')]:
    obj = E.SubElement(config, 'object', {'id': str(ident)})
    for k, v in [('name', label), ('extruder', '1')]:
        E.SubElement(obj, 'metadata', {'key': k, 'value': v})
    for i, name in names:
        part = E.SubElement(obj, 'part', {'id': str(i), 'subtype': 'support_blocker' if i == 3 else 'normal_part'})
        E.SubElement(part, 'metadata', {'key': 'name', 'value': name})
        E.SubElement(part, 'metadata', {'key': 'matrix', 'value': '1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1'})
        if name == 'rotor':
            # Keep the proven sphere shell/infill. Six walls are useful at the
            # threaded foot/ring but add hours and high-up weight to the globe.
            for k, v in [('wall_loops', '3'), ('sparse_infill_density', '12%'),
                         ('top_shell_layers', '5'), ('bottom_shell_layers', '4')]:
                E.SubElement(part, 'metadata', {'key': k, 'value': v})
plate = E.SubElement(config, 'plate')
for k, v in [('plater_id', '1'), ('plater_name', 'Threaded globe revision 2'), ('locked', 'false')]:
    E.SubElement(plate, 'metadata', {'key': k, 'value': v})
for ident in [5, 6]:
    inst = E.SubElement(plate, 'model_instance')
    for k, v in [('object_id', str(ident)), ('instance_id', '0'), ('identify_id', str(ident))]:
        E.SubElement(inst, 'metadata', {'key': k, 'value': v})
with zipfile.ZipFile(B/'globe-project.3mf', 'w', zipfile.ZIP_DEFLATED) as z:
    z.writestr('3D/3dmodel.model', E.tostring(root, encoding='utf-8', xml_declaration=True))
    z.writestr('Metadata/model_settings.config', E.tostring(config, encoding='utf-8', xml_declaration=True))
    z.writestr('Metadata/project_settings.config', json.dumps(full))
    # Reuse standard OPC relationship/content descriptors, not any old geometry.
    with zipfile.ZipFile(BASELINE/'globe-project.3mf') as old:
        for name in ['[Content_Types].xml', '_rels/.rels']:
            z.writestr(name, old.read(name))
print('Prepared two-piece P1S plate: six walls, bed-only trees, blocked socket/bearings', flush=True)
