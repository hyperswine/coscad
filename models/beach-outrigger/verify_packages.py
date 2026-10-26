"""Check each sliced package: success, no warnings, no supports, paths inside the model,
filament weight consistent with the solid geometry, G-code unchanged when filling model id."""
from pathlib import Path
import hashlib, json, re, zipfile, math, sys, xml.etree.ElementTree as E
import numpy as np, trimesh
B = Path(__file__).resolve().parent; NAME = 'beach-outrigger'
DENSITY = {'pla': 1.24, 'petg': 1.27}; MODEL_ID = {'P1S': 'C12', 'A1': 'N2S', 'X1C': 'BL-P001'}; PLACEMENT = {'P1S': (134., 136.), 'X1C': (134., 136.), 'A1': (128., 128.)}; MODEL = {'P1S': 'Bambu Lab P1S', 'A1': 'Bambu Lab A1', 'X1C': 'Bambu Lab X1 Carbon'}
jobs = sys.argv[1:] or ['p1s-pla', 'p1s-petg', 'a1-pla', 'a1-petg']
m = trimesh.load_mesh(B/(NAME+'.stl')); reports = json.loads((B/'packages.json').read_text()) if (B/'packages.json').exists() and sys.argv[1:] else {}
for job in jobs:
    printer = job.split('-')[0].upper(); material = job.split('-')[1]; folder = B/job; path = folder/'slice'/f'{NAME}.gcode.3mf'
    code = (folder/'slice/plate_1.gcode').read_bytes(); result = json.loads((folder/'slice/result.json').read_text())
    assert result['return_code'] == 0 and result['error_string'] == 'Success.' and len(result['sliced_plates']) == 1, result
    plate = result['sliced_plates'][0]; assert not plate['warning_message'], plate['warning_message']
    machine = json.loads((folder/'presets/machine.json').read_text()); offset = np.array([float(x) for x in machine['extruder_offset'][0].split('x')]); center = np.array(PLACEMENT[printer])-offset
    settings = {}; feature = ''; active = False; absolute = True; relative_e = True; x = y = z = e = 0.; layers = set(); count = 0; support = 0
    bounds = np.array([[math.inf]*3, [-math.inf]*3]); features = {}; bridge_len = 0.
    for line in code.decode().splitlines():
        if line.startswith('; ') and ' = ' in line: k, v = line[2:].split(' = ', 1); settings[k] = v
        if line.startswith('; CHANGE_LAYER'): active = True
        if line.startswith('; Z_HEIGHT:'): layers.add(float(line.split(':')[1]))
        if line.startswith('; FEATURE:'): feature = line.split(':')[1].strip()
        text = line.split(';')[0].strip()
        if not text: continue
        cmd = text.split()[0]
        if cmd == 'G90': absolute = True
        if cmd == 'G91': absolute = False
        if cmd == 'M83': relative_e = True
        if cmd == 'M82': relative_e = False
        v = {k: float(a) for k, a in re.findall(r'([XYZIJE])(-?\d*\.?\d+)', text)}
        if cmd == 'G92':
            if 'E' in v: e = v['E']
            continue
        if cmd not in ['G0', 'G1', 'G2', 'G3']: continue
        nx = v.get('X', x) if absolute else x+v.get('X', 0); ny = v.get('Y', y) if absolute else y+v.get('Y', 0); nz = v.get('Z', z) if absolute else z+v.get('Z', 0); de = v.get('E', 0) if relative_e else v.get('E', e)-e
        if 'E' in v: e = v['E']
        if active and de > 0 and (nx != x or ny != y) and feature not in ['Custom', 'Brim', 'Skirt']:
            if cmd in ['G2', 'G3']:
                cx = x+v.get('I', 0); cy = y+v.get('J', 0); radius = math.hypot(x-cx, y-cy); a = math.atan2(y-cy, x-cx); b = math.atan2(ny-cy, nx-cx)
                delta = (b-a) % (2*math.pi) if cmd == 'G3' else -((a-b) % (2*math.pi)); samples = max(2, math.ceil(abs(delta)*radius/.1)+1)
                angles = np.linspace(a, a+delta, samples); points = np.column_stack((cx+radius*np.cos(angles)-center[0], cy+radius*np.sin(angles)-center[1], np.linspace(z, nz, samples)))
            else: points = np.array([[x-center[0], y-center[1], z], [nx-center[0], ny-center[1], nz]])
            bounds[0] = np.minimum(bounds[0], points.min(axis=0)); bounds[1] = np.maximum(bounds[1], points.max(axis=0)); count += 1; features[feature] = features.get(feature, 0)+1
            if feature.startswith('Support'): support += 1
            if feature == 'Bridge': bridge_len = max(bridge_len, math.hypot(nx-x, ny-y))
        x, y, z = nx, ny, nz
    assert settings['wall_loops'] == '6' and settings['enable_support'] == '0' and settings['sparse_infill_density'] == '100%' and settings['layer_height'] == '0.2'
    assert count > 10000 and not support and 250 < len(layers) < 320, (count, support, len(layers))
    assert np.all(bounds[0] >= m.bounds[0]-.75) and np.all(bounds[1] <= m.bounds[1]+.75), (job, bounds, m.bounds)
    expected = float(m.volume*DENSITY[material]/1000); weight = plate['filaments'][0]['total_used_g']; assert .94*expected < weight < 1.08*expected, (expected, weight)
    with zipfile.ZipFile(path) as source:
        assert source.testzip() is None; entries = {i.filename: (i, source.read(i)) for i in source.infolist()}; assert source.read('Metadata/plate_1.gcode') == code
        cfg = json.loads(source.read('Metadata/project_settings.config')); assert cfg['printer_model'] == MODEL[printer] and cfg['enable_support'] == '0' and cfg['curr_bed_type'] == 'Textured PEI Plate'
        assert cfg['filament_type'][0].upper() == material.upper(), cfg['filament_type']
        assert source.read('Metadata/plate_1.gcode.md5').decode().strip().lower() == hashlib.md5(code).hexdigest()
        info = E.fromstring(source.read('Metadata/slice_info.config')); identity = info.find(".//metadata[@key='printer_model_id']"); assert identity is not None and identity.get('value') in ['', MODEL_ID[printer]]
        identity.set('value', MODEL_ID[printer]); key = 'Metadata/slice_info.config'; entries[key] = (entries[key][0], E.tostring(info, encoding='utf-8', xml_declaration=True))
    tmp = path.with_suffix('.tmp')
    with zipfile.ZipFile(tmp, 'w', zipfile.ZIP_DEFLATED) as out:
        for metadata, data in entries.values(): out.writestr(metadata, data)
    with zipfile.ZipFile(tmp) as check: assert check.testzip() is None and check.read('Metadata/plate_1.gcode') == code
    tmp.replace(path)
    reports[job] = {'printer': printer, 'material': material.upper(), 'filament': cfg['filament_settings_id'][0], 'nozzle_temperature': cfg['nozzle_temperature'][0], 'bed_temperature': cfg.get('textured_plate_temp', [''])[0],
                    'model_id': MODEL_ID[printer], 'package': str(path.relative_to(B)), 'package_sha256': hashlib.sha256(path.read_bytes()).hexdigest(), 'gcode_sha256': hashlib.sha256(code).hexdigest(),
                    'layers': len(layers), 'model_path_segments': count, 'support_segments': support, 'longest_bridge_segment_mm': bridge_len, 'path_bounds_relative_to_model_mm': bounds.tolist(),
                    'features': features, 'estimated_seconds': plate['total_predication'], 'estimated_hours': plate['total_predication']/3600, 'estimated_grams': weight, 'solid_geometry_grams': expected, 'passed': True}
    (folder/'package-validation.json').write_text(json.dumps(reports[job], indent=2)+'\n')
    print(job, f"{reports[job]['estimated_hours']:.1f} h, {weight:.0f} g (solid {expected:.0f} g), {len(layers)} layers, bridge <= {bridge_len:.1f} mm")
(B/'packages.json').write_text(json.dumps(reports, indent=2)+'\n')
