#!/usr/bin/env python3
"""Read exported X1C settings and record hashes/estimates. Never dispatches."""
import hashlib
import json
from pathlib import Path
import re
import zipfile

HERE = Path(__file__).resolve().parent


def scalar(value):
    return value[0] if isinstance(value, list) and len(value) == 1 else value


def check_keepout(gcode):
    """Complement the per-part brim envelopes with actual extrusion endpoints.

    These profiles use relative E. Ignore machine purge/start/end sequences.
    This is not a nozzle swept-volume or collision simulation.
    """
    x = y = 0.
    feature = ''
    checked = 0
    for line in gcode.splitlines():
        if line.startswith('; FEATURE: '):
            feature = line[11:].strip()
        if line.startswith('; filament end gcode'):
            feature = ''
        if not re.match(r'^G[0123] ', line):
            continue
        values = {k:float(v) for k,v in re.findall(r'\b([XYE])(-?[\d.]+)',line.split(';')[0])}
        x,y = values.get('X',x), values.get('Y',y)
        if feature and values.get('E',0) > 0 and ('X' in values or 'Y' in values):
            assert not (0 < x < 18 and 0 < y < 28), (feature,x,y)
            assert -0.01 <= x <= 256.01 and -0.01 <= y <= 256.01, (feature,x,y)
            checked += 1
    assert checked > 100
    return checked


def main():
    expected = json.loads((HERE/'process.json').read_text())
    expected.update(printer_settings_id='Bambu Lab X1 Carbon 0.4 nozzle',
                    filament_settings_id='Generic PLA', brim_type='outer_only', brim_width='5')
    report = {'dispatched': False, 'verified_settings': expected, 'builds': {}}
    for stem in ('fit','manufacture'):
        manifest = json.loads((HERE/f'{stem}_manifest.json').read_text())
        summary = json.loads((HERE/f'{stem}_print.json').read_text())
        assert len(summary['beds']) == len(manifest['beds'])
        assert summary['printability']['accept_printability'] is False
        assert all(i.get('addressed_by') for i in summary['printability']['warnings'])
        entries = []
        for bed in summary['beds']:
            path = HERE/f"{stem}_bed{bed['index']}.gcode.3mf"
            with zipfile.ZipFile(path) as archive:
                settings = json.loads(archive.read('Metadata/project_settings.config'))
                for key, value in expected.items():
                    assert scalar(settings.get(key)) == value, (path.name,key,settings.get(key),value)
                gcode = archive.read('Metadata/plate_1.gcode').decode()
                assert '; FEATURE: Outer wall' in gcode and 'G1 ' in gcode
                z = float(re.search(r'; max_z_height: ([\d.]+)',gcode).group(1))
                assert z <= 250
                plate = json.loads(archive.read('Metadata/plate_1.json'))
                bbox = plate['bbox_all']
                assert min(bbox[:2]) >= 0 and max(bbox[2:]) <= 256
                assert settings['use_relative_e_distances'] == '1'
                checked = check_keepout(gcode)
                folder = HERE/'evidence'/'plates'
                folder.mkdir(exist_ok=True)
                (folder/f'{stem}-{bed["index"]}.png').write_bytes(archive.read('Metadata/plate_1.png'))
                support = '; FEATURE: Support' in gcode
            entries.append(dict(plate=bed['index'], file=path.name,
                sha256=hashlib.sha256(path.read_bytes()).hexdigest(),
                stl_sha256=hashlib.sha256((HERE/f"{stem}_bed{bed['index']}.stl").read_bytes()).hexdigest(),
                grams=float(bed['filament_g']), estimated_seconds=bed['estimated_s'],
                time=bed['print_time'], max_z_mm=z, bbox_xy=bbox, support_toolpaths=support,
                checked_extrusion_endpoints=checked))
        report['builds'][stem] = dict(plates=entries, grams=sum(b['grams'] for b in entries),
            estimated_hours=sum(b['estimated_seconds'] for b in entries)/3600)
    (HERE/'evidence'/'slices.json').write_text(json.dumps(report, indent=2)+'\n')
    for stem,build in report['builds'].items():
        print(stem, len(build['plates']), round(build['grams'],1), 'g', round(build['estimated_hours'],2), 'hours')


if __name__ == '__main__':
    main()
