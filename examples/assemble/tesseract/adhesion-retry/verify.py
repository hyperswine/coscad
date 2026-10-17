#!/usr/bin/env python3
"""Offline preflight of the generated retry meshes and actual A1 toolpaths.

Uses ../requirements.txt. Does not connect to, upload to, or start a printer.
"""
import argparse
from collections import Counter
from datetime import datetime
import hashlib
import json
from pathlib import Path
import re
from zipfile import ZipFile
from zoneinfo import ZoneInfo
import xml.etree.ElementTree as ET

import numpy as np
import trimesh

HERE = Path(__file__).resolve().parent


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--geometry-report', type=Path, help='successful parent verify.py report')
    args = ap.parse_args()
    manifest = json.loads((HERE / 'fit-test_manifest.json').read_text())
    assert manifest.get('schema', 'coscad.next/1') == 'coscad.next/1', 'fit-test_manifest.json must come from `coscad next`'
    assert len(manifest['beds']) == 1
    placements = manifest['beds'][0]['placements']
    assert {p['instance'] for p in placements} == {'cap_1', 'corner_1', 'bar_1', 'fullbar_1'}
    # every part reserves its 8 mm brim inside the 6 mm plate margin
    assert all(p['brim_mm'] == 8 for p in placements)
    keep_out = manifest['bed']['margin'] + 8
    mesh = trimesh.load_mesh(HERE / 'fit-test_bed1.stl')
    parts = mesh.split(only_watertight=False)
    assert mesh.is_volume and len(parts) == 4
    measured = {}
    for part in parts:
        center = part.bounds.mean(axis=0)[:2]
        placement = min(placements, key=lambda p: np.linalg.norm(center - [p['x'] + p['w']/2, p['y'] + p['d']/2]))
        name = placement['instance']
        assert name not in measured
        assert part.is_volume and abs(part.bounds[0, 2]) < 1e-5
        assert np.all(part.bounds[0, :2] >= keep_out - .01) and np.all(part.bounds[1, :2] <= 256 - keep_out + .01)
        feet = np.all(abs(part.triangles[:, :, 2] - part.bounds[0, 2]) < 1e-4, axis=1)
        measured[name] = {'bed_contact_mm2': float(part.area_faces[feet].sum()),
                          'size_mm': part.extents.tolist()}
    assert measured['cap_1']['bed_contact_mm2'] > 65
    cap_meta = next(v for v in manifest['variants'] if v['part'] == 'cap')['printability']
    assert abs(cap_meta['bed_contact_mm2'] - measured['cap_1']['bed_contact_mm2']) < .01
    fullbar_meta = next(v for v in manifest['variants'] if v['part'] == 'fullbar')['printability']
    # Polygonal cylinder tessellation puts the rendered low point at about
    # 0.805 mm; 0.8 mm is the nominal circular dimension.
    assert any(abs(h-.8) < .01 for h in fullbar_meta['elevated_start_heights_mm'])

    artifact = HERE / 'fit-test_bed1.gcode.3mf'
    with ZipFile(artifact) as archive:
        model = ET.fromstring(archive.read('3D/Objects/object_1.model'))
        ns = {'m': 'http://schemas.microsoft.com/3dmanufacturing/core/2015/02'}
        vertices = [[float(v.get(k)) for k in ('x', 'y', 'z')]
                    for v in model.findall('.//m:vertex', ns)]
        faces = [[int(t.get(k)) for k in ('v1', 'v2', 'v3')]
                 for t in model.findall('.//m:triangle', ns)]
        embedded = trimesh.Trimesh(vertices=vertices, faces=faces)
        assert embedded.is_volume and len(embedded.split(only_watertight=False)) == 4
        assert abs(embedded.volume-mesh.volume) < 1e-4*mesh.volume
        assert np.allclose(embedded.extents, mesh.extents, atol=.001)
        cfg = json.loads(archive.read('Metadata/project_settings.config'))
        assert cfg['printer_settings_id'] == 'Bambu Lab A1 0.4 nozzle'
        assert cfg['printer_model'] == 'Bambu Lab A1' and cfg['nozzle_diameter'] == ['0.4']
        assert cfg['curr_bed_type'] == 'Textured PEI Plate'
        assert cfg['filament_type'] == ['PLA']
        # the brim comes from the manifest's reservation, not an override
        assert cfg['brim_type'] == 'outer_only' and cfg['brim_width'] == '8', (cfg.get('brim_type'), cfg.get('brim_width'))
        for filename in ['process-overrides.json', 'filament-overrides.json']:
            for key, value in json.loads((HERE / filename).read_text()).items():
                assert cfg[key] == value, (key, value, cfg.get(key))
        lines = archive.read('Metadata/plate_1.gcode').decode().splitlines()
        changes = [i for i, line in enumerate(lines) if line == '; CHANGE_LAYER']
        assert len(changes) >= 6
        first = lines[changes[0]:changes[1]]
        features = Counter()
        feature = ''
        for line in first:
            if line.startswith('; FEATURE: '):
                feature = line[len('; FEATURE: '):]
            if re.match(r'G[123] ', line) and ('X' in line or 'Y' in line):
                extrusion = re.search(r'(?:^| )E(-?[0-9.]+)', line)
                if extrusion and float(extrusion.group(1)) > 0:
                    features[feature] += 1
        assert features['Brim'] > 0
        assert features['Support'] + features['Support interface'] > 0
        assert not any(line.startswith('M106 S') and float(line.split('S')[1]) > 0
                       for line in lines[changes[0]:changes[3]])
    report = {'verified_at': datetime.now(ZoneInfo('Australia/Sydney')).isoformat(),
              'verdict': 'offline preflight passed; physical print pending',
              'artifact': artifact.name,
              'sha256': hashlib.sha256(artifact.read_bytes()).hexdigest(),
              'bed_stl_sha256': hashlib.sha256((HERE / 'fit-test_bed1.stl').read_bytes()).hexdigest(),
              'measured_parts': measured,
              'first_layer_extruding_moves_by_feature': dict(features),
              'physical_print_attempted': False, 'physical_fit_tested': False}
    if args.geometry_report:
        geometry = json.loads(args.geometry_report.read_text())
        assert geometry['parts'] == 38 and geometry['sampled_positions'] == 4629
        assert geometry['unintended_overlap_mm3'] < .01
        report['assembly_geometry'] = geometry
    (HERE / 'verification.json').write_text(json.dumps(report, indent=2) + '\n')
    print('PASS: four closed parts, broad cap foot, actual brim/support extrusion, A1 PLA settings and early cooling')
    print('Physical adhesion, support removal and fit remain untested.')


if __name__ == '__main__':
    main()
