"""PLA projects for the 2020 tank model (X1C beds 1 and 2, P1S bed 2), every part a separate object.

Positions are fixed here (not Bambu's arrange) and keep clear of the X1C's 18 x 28 mm
front-left exclusion (the P1S has the same one). Bed 1 holds the five panels and twelve brackets, printed solid so
the panels do not seep. Bed 2 holds the twelve rails standing on end, with a 5 mm brim,
and the last four brackets. The process is Bambu's stock 0.20mm Standard for the X1C;
the project file template comes from the print-in-place globe."""
from pathlib import Path
import importlib.util, json, sys, zipfile, xml.etree.ElementTree as E
import trimesh
B = Path(__file__).resolve().parent; ROOT = B.parents[1]
spec = importlib.util.spec_from_file_location('bambu_slice', ROOT/'scripts/bambu-slice.py'); s = importlib.util.module_from_spec(spec); spec.loader.exec_module(s)
ns = 'http://schemas.microsoft.com/3dmanufacturing/core/2015/02'; E.register_namespace('', ns)
def q(t): return '{'+ns+'}'+t
PROCESS = '0.20mm Standard @BBL X1C'   # Bambu's stock process for both the X1C and the P1S
PRINTER = {'x1c': ('Bambu Lab X1 Carbon 0.4 nozzle', 'Bambu PLA Basic @BBL X1C', 'Bambu Lab X1 Carbon'),
           'p1s': ('Bambu Lab P1S 0.4 nozzle', 'Bambu PLA Basic @BBL P1S 0.4 nozzle', 'Bambu Lab P1S')}

def grid(xs, ys): return [(x, y) for y in ys for x in xs]
# Part centres on the plate (mm). Panels 70 x 70, brackets 40 x 40, rails 20 x 20.
BED1 = ([('panel_wall', p) for p in [(45, 85), (125, 85), (205, 85), (45, 165)]] + [('panel_floor', (125, 165))] +
        [('bracket', p) for p in grid([45, 90, 135, 180, 225], [27]) + grid([185, 230], [150]) + grid([30, 75, 120, 165, 210], [225])])
BED2 = ([('rail100', p) for p in grid([70, 105, 140, 175], [90])] +
        [('rail60', p) for p in grid([70, 105, 140, 175], [125, 160])] +
        [('bracket', p) for p in grid([40, 95, 150, 205], [215])])
COMMON = {'enable_support': '0', 'support_on_build_plate_only': '1', 'only_one_wall_top': '0',
          'sparse_infill_pattern': 'zig-zag', 'brim_object_gap': '0.1'}
JOBS = {'x1c-bed1': (BED1, {'wall_loops': '4', 'sparse_infill_density': '100%', 'top_shell_layers': '5', 'bottom_shell_layers': '5', 'brim_type': 'no_brim', 'brim_width': '0'}),
        'x1c-bed2': (BED2, {'wall_loops': '3', 'sparse_infill_density': '25%', 'sparse_infill_pattern': 'gyroid', 'top_shell_layers': '4', 'bottom_shell_layers': '4', 'brim_type': 'outer_only', 'brim_width': '5'})}
JOBS['p1s-bed2'] = JOBS['x1c-bed2']   # same layout: the P1S has the same front-left exclusion
EXCLUDE = (18, 28)

def mesh(name):
    m = trimesh.load_mesh(B/'stl'/(name+'.stl')); assert m.is_watertight, name
    m.apply_translation([-m.bounds[:, 0].mean(), -m.bounds[:, 1].mean(), -m.bounds[0, 2]])   # centred, on the bed
    return m

def main(jobs):
    for job, (layout, overrides) in JOBS.items():
        if jobs and job not in jobs: continue
        overrides = {**COMMON, **overrides}
        meshes = {n: mesh(n) for n in {n for n, _ in layout}}
        for n, (x, y) in layout:   # inside the plate, clear of the exclusion corner, with room for a brim
            m = meshes[n]; w, d = m.extents[:2]; pad = 6
            assert pad <= x - w/2 and x + w/2 <= 256 - pad and pad <= y - d/2 and y + d/2 <= 256 - pad, (job, n, x, y)
            assert x - w/2 >= EXCLUDE[0] + pad or y - d/2 >= EXCLUDE[1] + pad, ('exclusion corner', job, n, x, y)
        boxes = [(x - meshes[n].extents[0]/2, x + meshes[n].extents[0]/2, y - meshes[n].extents[1]/2, y + meshes[n].extents[1]/2) for n, (x, y) in layout]
        gap = 3 + (2*float(overrides['brim_width']) if overrides['brim_type'] != 'no_brim' else 0)
        for i, a in enumerate(boxes):
            for b in boxes[i+1:]:
                assert a[1] + gap <= b[0] or b[1] + gap <= a[0] or a[3] + gap <= b[2] or b[3] + gap <= a[2], ('parts too close', job, a, b)
        MACHINE, FILAMENT, MODEL = PRINTER[job.split('-')[0]]
        folder = B/job; presets = folder/'presets'; presets.mkdir(parents=True, exist_ok=True)
        baseline = ROOT/'models/print-in-place-globe/p1s'
        machine = s.machine_with_templates(MACHINE)
        process = s.flatten('process', PROCESS); project = json.loads((baseline/'presets/project.json').read_text())
        filament = s.flatten('filament', FILAMENT)
        process.update(overrides); process['curr_bed_type'] = 'Textured PEI Plate'; process['layer_height'] = '0.2'; process['initial_layer_print_height'] = '0.2'
        for kind, data in [('machine', machine), ('process', process), ('filament', filament)]:
            (presets/(kind+'.json')).write_text(json.dumps(data, indent=2)+'\n')
            project.update({k: v for k, v in data.items() if k not in {'type', 'name', 'inherits', 'from', 'instantiation', 'setting_id'}})
        project.update(overrides); project['filament_settings_id'] = [FILAMENT]; project['printer_settings_id'] = MACHINE
        assert project['printer_model'] == MODEL, project['printer_model']
        (presets/'project.json').write_text(json.dumps(project, indent=2)+'\n')
        model = E.Element(q('model'), {'unit': 'millimeter'})
        E.SubElement(model, q('metadata'), {'name': 'Application'}).text = 'BambuStudio-02.08.02.61'
        E.SubElement(model, q('metadata'), {'name': 'BambuStudio:3mfVersion'}).text = '1'
        resources = E.SubElement(model, q('resources')); build = E.SubElement(model, q('build'))
        config = E.Element('config'); plate = E.SubElement(config, 'plate')
        E.SubElement(plate, 'metadata', {'key': 'plater_id', 'value': '1'}); E.SubElement(plate, 'metadata', {'key': 'plater_name', 'value': f'tank {job}'})
        counts = {}
        for i, (n, (x, y)) in enumerate(layout):
            counts[n] = counts.get(n, 0) + 1; label = f'{n}_{counts[n]}'
            mid, oid = str(2*i + 1), str(2*i + 2)
            obj = E.SubElement(resources, q('object'), {'id': mid, 'type': 'model', 'name': label}); me = E.SubElement(obj, q('mesh'))
            vs = E.SubElement(me, q('vertices')); ts = E.SubElement(me, q('triangles'))
            for v in meshes[n].vertices: E.SubElement(vs, q('vertex'), {'x': f'{v[0]:.6f}', 'y': f'{v[1]:.6f}', 'z': f'{v[2]:.6f}'})
            for f in meshes[n].faces: E.SubElement(ts, q('triangle'), {'v1': str(f[0]), 'v2': str(f[1]), 'v3': str(f[2])})
            parent = E.SubElement(resources, q('object'), {'id': oid, 'type': 'model', 'name': label})
            E.SubElement(E.SubElement(parent, q('components')), q('component'), {'objectid': mid})
            E.SubElement(build, q('item'), {'objectid': oid, 'transform': f'1 0 0 0 1 0 0 0 1 {x} {y} 0', 'printable': '1'})
            o = E.SubElement(config, 'object', {'id': oid}); E.SubElement(o, 'metadata', {'key': 'name', 'value': label}); E.SubElement(o, 'metadata', {'key': 'extruder', 'value': '1'})
            part = E.SubElement(o, 'part', {'id': '1', 'subtype': 'normal_part'}); E.SubElement(part, 'metadata', {'key': 'name', 'value': label})
            E.SubElement(part, 'metadata', {'key': 'matrix', 'value': '1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1'})
            inst = E.SubElement(plate, 'model_instance')
            for key, value in [('object_id', oid), ('instance_id', '0'), ('identify_id', oid)]: E.SubElement(inst, 'metadata', {'key': key, 'value': value})
        with zipfile.ZipFile(folder/'tank-project.3mf', 'w', zipfile.ZIP_DEFLATED) as out:
            out.writestr('3D/3dmodel.model', E.tostring(model, encoding='utf-8', xml_declaration=True))
            out.writestr('Metadata/model_settings.config', E.tostring(config, encoding='utf-8', xml_declaration=True))
            out.writestr('Metadata/project_settings.config', json.dumps(project))
            with zipfile.ZipFile(ROOT/'models/print-in-place-globe/globe-project.3mf') as old:
                for filename in ['[Content_Types].xml', '_rels/.rels']: out.writestr(filename, old.read(filename))
        (folder/'layout.json').write_text(json.dumps([{'part': n, 'centre_mm': [x, y]} for n, (x, y) in layout], indent=1)+'\n')
        print('Prepared', job, len(layout), 'objects')

if __name__ == '__main__': main(sys.argv[1:])
