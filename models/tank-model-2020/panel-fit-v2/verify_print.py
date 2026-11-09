#!/usr/bin/env python3
"""Verify X1C panel packages before dispatch; three samples by default. No printer access."""
import hashlib
import json
from pathlib import Path
import re
import sys
import xml.etree.ElementTree as E
import zipfile

import numpy as np
from scipy.spatial import cKDTree
import trimesh

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
sys.path.insert(0, str(ROOT / 'scripts'))
from bambu_preflight import preset_expectations, verify_package
from prepare_print import FOLDER


def require(condition, reason):
    if not condition:
        raise ValueError(reason)


def verify(here=HERE, folder=FOLDER, expected_parts=3):
    HERE, FOLDER = Path(here), Path(folder)
    intent = json.loads((FOLDER / 'print-intent.json').read_text())
    require(len(intent['parts']) == expected_parts and expected_parts > 0, 'Unexpected panel object count')
    expected = []
    for item in intent['parts']:
        for suffix, key in (('.coscad', 'source_sha256'), ('.stl', 'stl_sha256')):
            require(hashlib.sha256((HERE / (item['name'] + suffix)).read_bytes()).hexdigest() == item[key], 'Coupon changed; rebuild slice')
        mesh = trimesh.load_mesh(HERE / (item['name'] + '.stl'))
        require(mesh.is_watertight and len(mesh.split()) == 1, item['name'])
        require(mesh.euler_number == 2 - 2 * item['holes'], 'Identification holes differ')
        mesh.apply_translation([-mesh.bounds[:, 0].mean() + item['centre_mm'][0],
                                -mesh.bounds[:, 1].mean() + item['centre_mm'][1], -mesh.bounds[0, 2]])
        expected.append(mesh)
    out = FOLDER / 'slice'
    result = json.loads((out / 'result.json').read_text())
    require(result['return_code'] == 0 and result['error_string'] == 'Success.', 'Slicer failed')
    require(len(result['sliced_plates']) == 1, 'Not a single plate')
    plate = result['sliced_plates'][0]
    require(not plate['warning_message'] and len(plate['objects']) == expected_parts, 'Warnings or wrong object count')
    package = out / 'tank.gcode.3mf'
    with zipfile.ZipFile(package) as archive:
        require(archive.testzip() is None, 'Corrupt package')
        entries = {entry.filename: archive.read(entry) for entry in archive.infolist()}
    settings = json.loads(entries['Metadata/project_settings.config'])
    require(settings['printer_settings_id'] == 'Bambu Lab X1 Carbon 0.4 nozzle', 'Wrong printer preset')
    require(settings['printer_model'] == 'Bambu Lab X1 Carbon' and settings['nozzle_diameter'] == ['0.4'], 'Wrong machine/nozzle')
    require(settings['filament_type'] == ['PLA'] and settings['curr_bed_type'] == 'Textured PEI Plate', 'Wrong material/plate')
    for key, value in [('enable_support', '0'), ('brim_type', 'no_brim'), ('print_sequence', 'by layer'),
                       ('wall_loops', '4'), ('sparse_infill_density', '100%')]:
        require(settings[key] == value, f'Wrong {key}: {settings[key]}')
    require(float(settings['layer_height']) == .1 and float(settings['initial_layer_print_height']) == .2, 'Wrong layer heights')
    require([n for n in entries if re.fullmatch(r'Metadata/plate_\d+\.gcode', n)] == ['Metadata/plate_1.gcode'], 'Ambiguous plate selection')
    code = entries['Metadata/plate_1.gcode']
    require(code == (out / 'plate_1.gcode').read_bytes(), 'Package G-code differs from inspected slice')
    require(hashlib.md5(code).hexdigest() == entries['Metadata/plate_1.gcode.md5'].decode().strip().lower(), 'G-code checksum differs')
    # Extract the actual exported mesh, including production-extension references.
    ns = {'m': 'http://schemas.microsoft.com/3dmanufacturing/core/2015/02'}

    def matrix(text):
        value = np.eye(4)
        if text:
            value[:3] = np.array(list(map(float, text.split()))).reshape(4, 3).T
        return value

    def geometry(path, ident, transform):
        document = E.fromstring(entries[path])
        obj = document.find(f"m:resources/m:object[@id='{ident}']", ns)
        require(obj is not None, 'Missing exported object')
        data = obj.find('m:mesh', ns)
        if data is not None:
            vertices = [[float(v.get(k)) for k in ('x', 'y', 'z')] for v in data.findall('m:vertices/m:vertex', ns)]
            faces = [[int(f.get(k)) for k in ('v1', 'v2', 'v3')] for f in data.findall('m:triangles/m:triangle', ns)]
            mesh = trimesh.Trimesh(vertices, faces, process=True)
            mesh.apply_transform(transform)
            return [mesh]
        meshes = []
        for child in obj.findall('m:components/m:component', ns):
            reference = next((v for k, v in child.attrib.items() if k.endswith('}path')), path).lstrip('/')
            meshes.extend(geometry(reference, child.get('objectid'), transform @ matrix(child.get('transform'))))
        return meshes

    document = E.fromstring(entries['3D/3dmodel.model'])
    actual = []
    for item in document.findall('m:build/m:item', ns):
        reference = next((v for k, v in item.attrib.items() if k.endswith('}path')), '3D/3dmodel.model').lstrip('/')
        actual.extend(geometry(reference, item.get('objectid'), matrix(item.get('transform'))))
    require(len(actual) == expected_parts, 'Extra/missing exported meshes')
    expected.sort(key=lambda mesh: tuple(mesh.bounds[0, :2]))
    actual.sort(key=lambda mesh: tuple(mesh.bounds[0, :2]))
    errors = []
    for before, after in zip(expected, actual):
        require(after.is_watertight and len(after.split()) == 1 and after.euler_number == before.euler_number, 'Export changed topology')
        error = max(cKDTree(before.vertices).query(after.vertices)[0].max(), cKDTree(after.vertices).query(before.vertices)[0].max())
        require(error < .001 and abs(before.volume - after.volume) < .02, 'Export changed coupon geometry')
        errors.append(float(error))
    # Only straight deposited model paths are supported in this plate audit.
    x = y = e = 0.
    absolute, relative_e, active = True, True, False
    feature, layers, segments = '', [], 0
    bounds = [mesh.bounds for mesh in expected]
    offset = list(map(float, json.loads((FOLDER / 'presets/machine.json').read_text())['extruder_offset'][0].split('x')))
    for line in code.decode().splitlines():
        if line == '; CHANGE_LAYER': active = True
        if line.startswith('; filament end gcode'): active = False
        if line.startswith('; FEATURE:'): feature = line.split(':', 1)[1].strip()
        if line.startswith('; Z_HEIGHT:'): layers.append(float(line.split(':', 1)[1]))
        command = line.split(';')[0].strip()
        if not command: continue
        op = command.split()[0]
        if op == 'G90': absolute = True
        if op == 'G91': absolute = False
        if op == 'M83': relative_e = True
        if op == 'M82': relative_e = False
        values = {k: float(v) for k, v in re.findall(r'([XYE])(-?\d*\.?\d+)', command)}
        if op == 'G92':
            if 'E' in values: e = values['E']
            continue
        if op not in ('G0', 'G1', 'G2', 'G3'): continue
        nx = values.get('X', x) if absolute else x + values.get('X', 0)
        ny = values.get('Y', y) if absolute else y + values.get('Y', 0)
        de = values.get('E', 0) if relative_e else values.get('E', e) - e
        if 'E' in values: e = values['E']
        if active and de > 0 and (nx, ny) != (x, y) and feature not in ('', 'Custom', 'Skirt'):
            require(op == 'G1' and not feature.startswith(('Support', 'Brim')), 'Unexpected arc/support/brim')
            segments += 1
            for px, py in ((x + offset[0], y + offset[1]), (nx + offset[0], ny + offset[1])):
                require(20 < px < 236 and 20 < py < 236, 'Path outside central safe region')
                require(any(lo[0] - .6 <= px <= hi[0] + .6 and lo[1] - .6 <= py <= hi[1] + .6 for lo, hi in bounds), 'Path outside samples')
        x, y = nx, ny
    top = max(mesh.bounds[1, 2] for mesh in expected)
    layer_count = round((top - .2) / .1) + 1
    require(abs(.2 + .1 * (layer_count - 1) - top) < 1e-5, 'Sample height is not representable at the intended layer heights')
    require(segments > 1000 and len(layers) == layer_count, 'Missing model paths or wrong layer count')
    require(np.allclose(layers, .2 + .1 * np.arange(layer_count), atol=1e-5), 'Layer rounding could merge the fit variants')
    info = E.fromstring(entries['Metadata/slice_info.config'])
    identity = info.find(".//metadata[@key='printer_model_id']")
    require(identity is not None and identity.get('value') in ('', 'BL-P001'), 'Wrong printer model ID')
    if identity.get('value') == '':
        identity.set('value', 'BL-P001')
        entries['Metadata/slice_info.config'] = E.tostring(info, encoding='utf-8', xml_declaration=True)
        temporary = package.with_suffix('.tmp')
        with zipfile.ZipFile(temporary, 'w', zipfile.ZIP_DEFLATED) as archive:
            for name, data in entries.items(): archive.writestr(name, data)
        temporary.replace(package)
    thermal = verify_package(package, preset_expectations(FOLDER / 'presets', plate='Textured PEI Plate'))
    require(thermal['expected'] == dict(plate='Textured PEI Plate', bed_initial_c=55., bed_c=55., nozzle_initial_c=220., nozzle_c=220.), 'Thermal intent differs')
    report = {'passed': True, 'package_sha256': hashlib.sha256(package.read_bytes()).hexdigest(),
              'layers': len(layers), 'objects': expected_parts, 'layer_height_mm': .1, 'path_segments': segments,
              'export_vertex_error_max_mm': max(errors), 'thermal': thermal,
              'estimated_minutes': plate['total_predication'] / 60,
              'estimated_grams': plate['filaments'][0]['total_used_g']}
    (FOLDER / 'plate-preview.png').write_bytes(entries['Metadata/plate_1.png'])
    (FOLDER / 'package-validation.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))
    return report


if __name__ == '__main__':
    verify()
