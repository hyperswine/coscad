"""Bambu projects for the bolted glider, one plate per job. Each part's print-frame mesh
gets its mouse-ear pads (ears-<part>.stl) joined on, is turned to the angle that fits the
plate best (the long parts lie on a diagonal) and placed as a separate object. Checks:
inside the plate with a margin, clear of the X1C/P1S front-left exclusion, parts apart.

  a1-wing-right   right wing panel, A1
  p1s-wing-left   left wing panel, P1S
  x1c-fuselage    fuselage standing on its keel, X1C (start it without flow calibration:
                  the part reaches the front of the plate)
  a1-tail         stab (flat) and fin (standing on its foot), A1
Stock 0.20mm Standard processes with lighter shells for the flying surfaces."""
from pathlib import Path
import importlib.util, json, math, sys, zipfile, xml.etree.ElementTree as E
import numpy as np, trimesh, manifold3d as mf
B = Path(__file__).resolve().parent; ROOT = B.parents[1]
spec = importlib.util.spec_from_file_location('bambu_slice', ROOT/'scripts/bambu-slice.py'); s = importlib.util.module_from_spec(spec); spec.loader.exec_module(s)
ns = 'http://schemas.microsoft.com/3dmanufacturing/core/2015/02'; E.register_namespace('', ns)
def q(t): return '{'+ns+'}'+t
PRINTER = {'a1': ('Bambu Lab A1 0.4 nozzle', 'Bambu PLA Basic @BBL A1', 'Bambu Lab A1', '0.20mm Standard @BBL A1'),
           'p1s': ('Bambu Lab P1S 0.4 nozzle', 'Bambu PLA Basic @BBL P1S 0.4 nozzle', 'Bambu Lab P1S', '0.20mm Standard @BBL X1C'),
           'x1c': ('Bambu Lab X1 Carbon 0.4 nozzle', 'Bambu PLA Basic @BBL X1C', 'Bambu Lab X1 Carbon', '0.20mm Standard @BBL X1C')}
EXCLUDE = {'a1': None, 'p1s': (18, 28), 'x1c': (18, 28)}
COMMON = {'enable_support': '0', 'support_on_build_plate_only': '1', 'only_one_wall_top': '0', 'brim_type': 'no_brim', 'brim_width': '0',
          'brim_object_gap': '0.1', 'sparse_infill_pattern': 'gyroid', 'top_shell_thickness': '0', 'bottom_shell_thickness': '0'}
SURFACE = {'wall_loops': '2', 'sparse_infill_density': '8%', 'top_shell_layers': '3', 'bottom_shell_layers': '2'}
BODY = {'wall_loops': '2', 'sparse_infill_density': '8%', 'top_shell_layers': '3', 'bottom_shell_layers': '3'}
JOBS = {'a1-wing-right': (['wing-right'], SURFACE), 'p1s-wing-left': (['wing-left'], SURFACE),
        'x1c-fuselage': (['fuselage'], BODY), 'a1-tail': (['stab', 'fin'], SURFACE)}
SIDE, PAD = 256.0, 5.0

def man(m): return mf.Manifold(mf.Mesh(vert_properties=np.asarray(m.vertices, np.float32), tri_verts=np.asarray(m.faces, np.uint32)))
def with_ears(part):
    m = man(trimesh.load_mesh(B/f'{part}.stl')) + man(trimesh.load_mesh(B/f'ears-{part}.stl'))
    o = m.to_mesh(); t = trimesh.Trimesh(o.vert_properties[:, :3], o.tri_verts)
    assert t.is_watertight and len(t.split(only_watertight=False)) == 1, (part, 'ears not joined')
    t.apply_translation([-t.bounds[:, 0].mean(), -t.bounds[:, 1].mean(), -t.bounds[0, 2]])
    return t

def place_one(m, corner):
    """Best z rotation and centre for one mesh on its own plate."""
    from scipy.spatial import ConvexHull
    xy = m.vertices[:, :2]; pts = xy[ConvexHull(xy).vertices]
    best = None
    for a10 in range(0, 1800, 5):
        a = math.radians(a10/10); c, sn = math.cos(a), math.sin(a)
        p = pts @ np.array([[c, sn], [-sn, c]])
        lo, hi = p.min(0), p.max(0); size = hi - lo
        if (size > SIDE - 2*PAD).any(): continue
        ctr = SIDE/2 - (lo + hi)/2; pp = p + ctr
        if corner and ((pp[:, 0] < corner[0] + PAD) & (pp[:, 1] < corner[1] + PAD)).any():
            # slide away from the excluded corner if there is room
            shift = np.array([SIDE - PAD - hi[0] - ctr[0], SIDE - PAD - hi[1] - ctr[1]]); pp = pp + shift; ctr = ctr + shift
            if ((pp[:, 0] < corner[0] + PAD) & (pp[:, 1] < corner[1] + PAD)).any(): continue
        slack = (SIDE - 2*PAD - size).min()
        if best is None or slack > best[0]: best = (slack, a10/10, ctr)
    assert best, 'does not fit the plate'
    return best

def layout(job, parts, corner):
    meshes = {p: with_ears(p) for p in parts}
    if len(parts) == 1:
        slack, ang, ctr = place_one(meshes[parts[0]], corner); return {parts[0]: (meshes[parts[0]], ang, tuple(ctr))}
    # stab along y on the left, fin standing to its right
    st, fn = meshes['stab'], meshes['fin']
    return {'stab': (st, 0.0, (PAD + 30 + st.extents[0]/2, SIDE/2)), 'fin': (fn, 0.0, (PAD + 30 + st.extents[0] + 15 + fn.extents[0]/2, SIDE/2))}

def main(jobs):
    from provenance import require_geometry, snapshot
    require_geometry(B)
    for job, (parts, overrides) in JOBS.items():
        if jobs and job not in jobs: continue
        printer = job.split('-')[0]; corner = EXCLUDE[printer]
        overrides = {**COMMON, **overrides}
        placed = layout(job, parts, corner)
        boxes = []
        for p, (m, ang, (x, y)) in placed.items():
            mm = m.copy(); mm.apply_transform(trimesh.transformations.rotation_matrix(math.radians(ang), [0, 0, 1])); mm.apply_translation([x, y, 0])
            lo, hi = mm.bounds[0], mm.bounds[1]
            assert lo[0] >= PAD - 1e-6 and lo[1] >= PAD - 1e-6 and hi[0] <= SIDE - PAD + 1e-6 and hi[1] <= SIDE - PAD + 1e-6, (job, p, lo, hi)
            if corner:
                v = mm.vertices; assert not ((v[:, 0] < corner[0] + PAD) & (v[:, 1] < corner[1] + PAD)).any(), (job, p, 'exclusion corner')
            placed[p] = (mm, ang, (x, y)); boxes.append(mm.bounds)
        for i, a in enumerate(boxes):
            for b in boxes[i+1:]:
                assert a[1][0] + 5 <= b[0][0] or b[1][0] + 5 <= a[0][0] or a[1][1] + 5 <= b[0][1] or b[1][1] + 5 <= a[0][1], (job, 'parts too close')
        MACHINE, FILAMENT, MODEL, PROCESS = PRINTER[printer]
        folder = B/job; presets = folder/'presets'; presets.mkdir(parents=True, exist_ok=True)
        baseline = ROOT/'models/print-in-place-globe'
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
        E.SubElement(plate, 'metadata', {'key': 'plater_id', 'value': '1'}); E.SubElement(plate, 'metadata', {'key': 'plater_name', 'value': f'bolted glider {job}'})
        for i, (p, (mm, ang, _)) in enumerate(placed.items()):
            ctr = np.array([mm.bounds[:, 0].mean(), mm.bounds[:, 1].mean(), 0]); local = mm.copy(); local.apply_translation(-ctr)
            mid, oid = str(2*i + 1), str(2*i + 2)
            obj = E.SubElement(resources, q('object'), {'id': mid, 'type': 'model', 'name': p}); me = E.SubElement(obj, q('mesh'))
            vs = E.SubElement(me, q('vertices')); ts = E.SubElement(me, q('triangles'))
            for v in local.vertices: E.SubElement(vs, q('vertex'), {'x': f'{v[0]:.6f}', 'y': f'{v[1]:.6f}', 'z': f'{v[2]:.6f}'})
            for f in local.faces: E.SubElement(ts, q('triangle'), {'v1': str(f[0]), 'v2': str(f[1]), 'v3': str(f[2])})
            parent = E.SubElement(resources, q('object'), {'id': oid, 'type': 'model', 'name': p})
            E.SubElement(E.SubElement(parent, q('components')), q('component'), {'objectid': mid})
            E.SubElement(build, q('item'), {'objectid': oid, 'transform': f'1 0 0 0 1 0 0 0 1 {ctr[0]:.4f} {ctr[1]:.4f} 0', 'printable': '1'})
            o = E.SubElement(config, 'object', {'id': oid}); E.SubElement(o, 'metadata', {'key': 'name', 'value': p}); E.SubElement(o, 'metadata', {'key': 'extruder', 'value': '1'})
            part = E.SubElement(o, 'part', {'id': '1', 'subtype': 'normal_part'}); E.SubElement(part, 'metadata', {'key': 'name', 'value': p})
            E.SubElement(part, 'metadata', {'key': 'matrix', 'value': '1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1'})
            inst = E.SubElement(plate, 'model_instance')
            for key, value in [('object_id', oid), ('instance_id', '0'), ('identify_id', oid)]: E.SubElement(inst, 'metadata', {'key': key, 'value': value})
            mm.export(folder/f'{p}-on-plate.stl')
        with zipfile.ZipFile(folder/'glider-project.3mf', 'w', zipfile.ZIP_DEFLATED) as out:
            out.writestr('3D/3dmodel.model', E.tostring(model, encoding='utf-8', xml_declaration=True))
            out.writestr('Metadata/model_settings.config', E.tostring(config, encoding='utf-8', xml_declaration=True))
            out.writestr('Metadata/project_settings.config', json.dumps(project))
            with zipfile.ZipFile(ROOT/'models/print-in-place-globe/globe-project.3mf') as old:
                for filename in ['[Content_Types].xml', '_rels/.rels']: out.writestr(filename, old.read(filename))
        (folder/'layout.json').write_text(json.dumps([{'part': p, 'rotation_deg': ang, 'centre_mm': [float(c) for c in ctr],
                                                       'bounds_mm': mm.bounds.round(2).tolist(), 'mesh_volume_mm3': float(mm.volume)}
                                                      for p, (mm, ang, ctr) in placed.items()], indent=1)+'\n')
        inputs = [B/'meshes.json', B/'prepare_slice.py', folder/'glider-project.3mf', folder/'layout.json'] + list(presets.glob('*.json'))
        (folder/'prepared-inputs.json').write_text(json.dumps(snapshot(B, inputs), indent=2)+'\n')
        print('Prepared', job, {p: (round(a, 1), [round(float(c)) for c in ctr]) for p, (_, a, ctr) in placed.items()})

if __name__ == '__main__': main(sys.argv[1:])
