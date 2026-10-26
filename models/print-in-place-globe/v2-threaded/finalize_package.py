"""Fill the P1S model identifier omitted by BambuStudio's standalone CLI export.
The installed vendor model profile is authoritative; G-code is never changed.
"""
from pathlib import Path
import zipfile,json,hashlib,xml.etree.ElementTree as E
B=Path(__file__).resolve().parent
result=json.loads((B/'slice/result.json').read_text())
assert result['return_code']==0 and result['error_string']=='Success.'
assert len(result['sliced_plates'])==1 and not result['sliced_plates'][0]['warning_message']
geometry=json.loads((B/'validation.json').read_text())
paths=json.loads((B/'toolpath-validation.json').read_text())
assert paths['passed'] and paths['gcode_sha256']==hashlib.sha256((B/'slice/plate_1.gcode').read_bytes()).hexdigest()
assert geometry['rotation']['maximum_intersection_mm3']<1e-5
assert geometry['thread_fit']['maximum_intersection_mm3']<1e-5
assert paths['effective_support_on_build_plate_only']=='1'
assert not paths['socket_support_intrusions'] and not paths['male_thread_support_intrusions']
assert not paths['bearing_region_support'] and paths['bearing_gap_crossing_segments']==0
assert not paths['support_roots']['components_without_prior_support']
assert paths['support_roots']['bed_components']>0 and paths['support_segments_checked']>0
p=B/'slice/globe-v2-p1s.gcode.3mf'
profile=json.loads(Path('/Applications/BambuStudio.app/Contents/Resources/profiles/BBL/machine/Bambu Lab P1S.json').read_text())
assert profile['model_id']=='C12'
with zipfile.ZipFile(p) as z:
 entries={i.filename:(i,z.read(i)) for i in z.infolist()}
 config=json.loads(z.read('Metadata/project_settings.config'))
 assert config['printer_model']=='Bambu Lab P1S'
 assert config['support_on_build_plate_only']=='1' and config['support_type']=='tree(auto)'
 assert config['wall_loops']=='6'
 settings=E.fromstring(z.read('Metadata/model_settings.config'))
 rotor_parts=[p for p in settings.findall('.//part') if p.find("metadata[@key='name']").get('value')=='rotor']
 assert len(rotor_parts)==1
 for key,value in [('wall_loops','3'),('sparse_infill_density','12%')]:
  assert rotor_parts[0].find(f"metadata[@key='{key}']").get('value')==value
 code=z.read('Metadata/plate_1.gcode')
 assert code==(B/'slice/plate_1.gcode').read_bytes()
 xml=E.fromstring(z.read('Metadata/slice_info.config'))
 model=xml.find(".//metadata[@key='printer_model_id']")
 assert model is not None and model.get('value') in ('','C12')
 model.set('value',profile['model_id'])
 key='Metadata/slice_info.config';entries[key]=(entries[key][0],E.tostring(xml,encoding='utf-8',xml_declaration=True))
 tmp=p.with_suffix('.tmp')
 with zipfile.ZipFile(tmp,'w',zipfile.ZIP_DEFLATED) as out:
  for info,data in entries.values():out.writestr(info,data)
with zipfile.ZipFile(tmp) as z:
 assert z.testzip() is None
 assert z.read('Metadata/plate_1.gcode')==code
 assert E.fromstring(z.read('Metadata/slice_info.config')).find(".//metadata[@key='printer_model_id']").get('value')=='C12'
tmp.replace(p)
report={'package_sha256':hashlib.sha256(p.read_bytes()).hexdigest(),'gcode_sha256':hashlib.sha256(code).hexdigest(),'printer_model_id':profile['model_id'],'metadata_correction':'Filled missing printer_model_id from installed P1S vendor profile; G-code bytes unchanged.'}
(B/'package-validation.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report,indent=2))
