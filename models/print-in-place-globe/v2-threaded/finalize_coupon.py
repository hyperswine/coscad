"""Validate the support-free fit test and fill the CLI's missing P1S model ID."""
from pathlib import Path
import hashlib
import json
import zipfile
import xml.etree.ElementTree as E

B = Path(__file__).resolve().parent
p = B/'coupon-slice/thread-coupon-p1s.gcode.3mf'
result = json.loads((B/'coupon-slice/result.json').read_text())
assert result['return_code'] == 0 and result['error_string'] == 'Success.'
assert len(result['sliced_plates'][0]['objects']) == 2
assert not result['sliced_plates'][0]['warning_message']
with zipfile.ZipFile(p) as z:
    entries = {i.filename: (i, z.read(i)) for i in z.infolist()}
    settings = json.loads(z.read('Metadata/project_settings.config'))
    assert settings['printer_model'] == 'Bambu Lab P1S' and settings['enable_support'] == '0'
    code = z.read('Metadata/plate_1.gcode')
    assert code == (B/'coupon-slice/plate_1.gcode').read_bytes()
    assert b'; FEATURE: Support' not in code
    xml = E.fromstring(z.read('Metadata/slice_info.config'))
    model = xml.find(".//metadata[@key='printer_model_id']")
    assert model is not None and model.get('value') in ('', 'C12')
    profile = json.loads(Path('/Applications/BambuStudio.app/Contents/Resources/profiles/BBL/machine/Bambu Lab P1S.json').read_text())
    assert profile['model_id'] == 'C12'
    model.set('value', profile['model_id'])
    name = 'Metadata/slice_info.config'
    entries[name] = (entries[name][0], E.tostring(xml, encoding='utf-8', xml_declaration=True))
tmp = p.with_suffix('.tmp')
with zipfile.ZipFile(tmp, 'w', zipfile.ZIP_DEFLATED) as z:
    for info, data in entries.values():
        z.writestr(info, data)
with zipfile.ZipFile(tmp) as z:
    assert z.testzip() is None and z.read('Metadata/plate_1.gcode') == code
tmp.replace(p)
report = {'package_sha256': hashlib.sha256(p.read_bytes()).hexdigest(),
          'gcode_sha256': hashlib.sha256(code).hexdigest(), 'printer_model_id': 'C12',
          'supports': False, 'estimated_seconds': result['sliced_plates'][0]['total_predication'],
          'filament_g': result['sliced_plates'][0]['filaments'][0]['total_used_g'],
          'physical_fit': 'Not yet printed'}
(B/'coupon-package-validation.json').write_text(json.dumps(report, indent=2)+'\n')
print(json.dumps(report, indent=2))
