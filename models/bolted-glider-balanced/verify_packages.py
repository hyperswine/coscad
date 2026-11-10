"""Check each sliced bolted-glider package before it can be dispatched: slicer success with no
warnings, the intended settings, no support paths, every extrusion inside the plate and
clear of the 18 x 28 mm front-left exclusion, filament weight consistent with the parts,
and the printer model id filled in without touching the G-code. Writes
<job>/package-validation.json and packages.json."""
from pathlib import Path
import hashlib, json, math, re, sys, zipfile, xml.etree.ElementTree as E
import trimesh
from provenance import require_geometry, require_slice
B = Path(__file__).resolve().parent
PRINTERS = {'x1c': ('X1C', 'Bambu Lab X1 Carbon', 'BL-P001'), 'p1s': ('P1S', 'Bambu Lab P1S', 'C12'), 'a1': ('A1', 'Bambu Lab A1', 'N2S')}; DENSITY = 1.24
CORNER = {'x1c': True, 'p1s': True, 'a1': False}
SURF = {'wall_loops': '2', 'sparse_infill_density': '8%', 'brim_type': 'no_brim'}
EXPECT = {'a1-wing-right': SURF, 'p1s-wing-left': SURF, 'x1c-fuselage': {'wall_loops': '2', 'sparse_infill_density': '8%', 'brim_type': 'no_brim'}, 'a1-tail': SURF}
reports = json.loads((B/'packages.json').read_text()) if (B/'packages.json').exists() else {}
require_geometry(B)
for job in sys.argv[1:] or list(EXPECT):
    require_slice(B,job)
    PRINTER, MODEL, MODEL_ID = PRINTERS[job.split('-')[0]]
    folder = B/job; path = folder/'slice'/'glider.gcode.3mf'
    code = (folder/'slice/plate_1.gcode').read_bytes(); result = json.loads((folder/'slice/result.json').read_text())
    assert result['return_code'] == 0 and result['error_string'] == 'Success.' and len(result['sliced_plates']) == 1, result
    plate = result['sliced_plates'][0]; assert not plate['warning_message'], plate['warning_message']
    machine = json.loads((folder/'presets/machine.json').read_text())
    ox, oy = (float(a) for a in machine['extruder_offset'][0].split('x'))   # G-code = plate - offset
    settings = {}; feature = ''; active = False; x = y = 0.0; layers = set(); support = 0; count = 0; in_corner = 0
    lo = [math.inf, math.inf]; hi = [-math.inf, -math.inf]; features = {}
    for line in code.decode().splitlines():
        if line.startswith('; ') and ' = ' in line: k, v = line[2:].split(' = ', 1); settings[k] = v
        if line.startswith('; CHANGE_LAYER'): active = True
        if line.startswith('; Z_HEIGHT:'): layers.add(float(line.split(':')[1]))
        if line.startswith('; FEATURE:'): feature = line.split(':')[1].strip()
        text = line.split(';')[0].strip()
        if not text or text.split()[0] not in ('G0', 'G1', 'G2', 'G3'): continue
        v = {k: float(a) for k, a in re.findall(r'([XYE])(-?\d*\.?\d+)', text)}
        nx, ny = v.get('X', x), v.get('Y', y)
        if active and v.get('E', 0) > 0 and (nx, ny) != (x, y) and feature not in ('Custom', 'Skirt'):
            count += 1; features[feature] = features.get(feature, 0) + 1
            if feature.startswith('Support'): support += 1
            for px, py in ((x + ox, y + oy), (nx + ox, ny + oy)):   # plate coordinates
                lo = [min(lo[0], px), min(lo[1], py)]; hi = [max(hi[0], px), max(hi[1], py)]
                if CORNER[job.split('-')[0]] and px < 18 and py < 28: in_corner += 1
        x, y = nx, ny
    for k, want in EXPECT[job].items(): assert settings[k] == want, (job, k, settings[k])
    assert settings['enable_support'] == '0' and settings['layer_height'] == '0.2'
    assert not support and not in_corner and count > 1000 and not any(f.startswith('Brim') for f in features), (support, in_corner, count, features)
    assert lo[0] >= 0 and lo[1] >= 0 and hi[0] <= 256 and hi[1] <= 256, (lo, hi)
    layout = json.loads((folder/'layout.json').read_text())
    solid = sum(p['mesh_volume_mm3'] for p in layout) * DENSITY / 1000
    weight = plate['filaments'][0]['total_used_g']
    assert 0.1*solid < weight < 1.02*solid, (solid, weight)   # light infill: well under solid
    with zipfile.ZipFile(path) as source:
        assert source.testzip() is None; entries = {i.filename: (i, source.read(i)) for i in source.infolist()}
        assert source.read('Metadata/plate_1.gcode') == code
        cfg = json.loads(source.read('Metadata/project_settings.config'))
        assert cfg['printer_model'] == MODEL and cfg['enable_support'] == '0' and cfg['curr_bed_type'] == 'Textured PEI Plate', (cfg['printer_model'], cfg['curr_bed_type'])
        assert cfg['filament_type'][0].upper() == 'PLA', cfg['filament_type']
        assert source.read('Metadata/plate_1.gcode.md5').decode().strip().lower() == hashlib.md5(code).hexdigest()
        info = E.fromstring(source.read('Metadata/slice_info.config')); identity = info.find(".//metadata[@key='printer_model_id']")
        assert identity is not None and identity.get('value') in ['', MODEL_ID]
        identity.set('value', MODEL_ID); key = 'Metadata/slice_info.config'; entries[key] = (entries[key][0], E.tostring(info, encoding='utf-8', xml_declaration=True))
    tmp = path.with_suffix('.tmp')
    with zipfile.ZipFile(tmp, 'w', zipfile.ZIP_DEFLATED) as out:
        for info_, data in entries.values(): out.writestr(info_, data)
    with zipfile.ZipFile(tmp) as check: assert check.testzip() is None and check.read('Metadata/plate_1.gcode') == code
    tmp.replace(path)
    reports[job] = {'printer': PRINTER, 'material': 'PLA', 'filament': cfg['filament_settings_id'][0], 'process': settings.get('print_settings_id', ''),
                    'nozzle_temperature': cfg['nozzle_temperature'][0], 'bed_temperature': cfg.get('textured_plate_temp', [''])[0], 'model_id': MODEL_ID,
                    'package': str(path.relative_to(B)), 'package_sha256': hashlib.sha256(path.read_bytes()).hexdigest(), 'gcode_sha256': hashlib.sha256(code).hexdigest(),
                    'objects': len(layout), 'layers': len(layers), 'model_path_segments': count, 'support_segments': support, 'segments_in_exclusion_corner': in_corner,
                    'extrusion_bounds_xy_mm': [lo, hi], 'features': features, 'estimated_hours': plate['total_predication']/3600,
                    'estimated_grams': weight, 'solid_parts_grams': solid, 'passed': True}
    (folder/'package-validation.json').write_text(json.dumps(reports[job], indent=2)+'\n')
    print(job, f"{reports[job]['estimated_hours']:.1f} h, {weight:.0f} g (solid parts {solid:.0f} g), {len(layers)} layers, plate x {lo[0]:.0f}..{hi[0]:.0f}, y {lo[1]:.0f}..{hi[1]:.0f}")
(B/'packages.json').write_text(json.dumps(reports, indent=2)+'\n')
