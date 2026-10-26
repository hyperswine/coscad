"""Fill the P1S model identifier omitted by BambuStudio's standalone CLI export.
The installed vendor model profile is authoritative; G-code is never changed.
"""
from pathlib import Path
import zipfile,json,hashlib,xml.etree.ElementTree as E
B=Path(__file__).resolve().parent
p=B/'slice/globe-p1s.gcode.3mf'
profile=json.loads(Path('/Applications/BambuStudio.app/Contents/Resources/profiles/BBL/machine/Bambu Lab P1S.json').read_text())
assert profile['model_id']=='C12'
with zipfile.ZipFile(p) as z:
 entries={i.filename:(i,z.read(i)) for i in z.infolist()}
 config=json.loads(z.read('Metadata/project_settings.config'))
 assert config['printer_model']=='Bambu Lab P1S'
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
