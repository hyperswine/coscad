#!/usr/bin/env python3
"""Verify P1S coupon settings/toolpaths and fill the slicer's missing model ID."""
from pathlib import Path
import hashlib,importlib.util,json,re,zipfile,xml.etree.ElementTree as E
HERE=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('checks',HERE.parents[1]/'two-tier-2020-shelf/verify_slices.py');checks=importlib.util.module_from_spec(spec);spec.loader.exec_module(checks)
package=HERE/'thread-fit_bed1.gcode.3mf'
expected=json.loads((HERE/'process.json').read_text());expected.update(printer_settings_id='Bambu Lab P1S 0.4 nozzle',filament_settings_id='Generic PLA',brim_width='5',brim_type='outer_only')
summary=json.loads((HERE/'thread-fit_print.json').read_text())
assert len(summary['beds'])==1 and not summary['printability']['accept_printability']
assert not summary['printability']['warnings']
with zipfile.ZipFile(package) as z:
 assert z.testzip() is None
 entries={i.filename:(i,z.read(i)) for i in z.infolist()}
 settings=json.loads(z.read('Metadata/project_settings.config'));code=z.read('Metadata/plate_1.gcode');g=code.decode()
 for k,v in expected.items():assert checks.scalar(settings.get(k))==v,(k,settings.get(k),v)
 assert settings['nozzle_diameter']==['0.4'] and settings['printer_model']=='Bambu Lab P1S'
 assert settings['curr_bed_type']=='Textured PEI Plate' and settings['use_relative_e_distances']=='1'
 assert '; FEATURE: Support' not in g
 layers=int(re.search(r'; total layer number: (\d+)',g).group(1));assert layers==55
 assert float(re.search(r'; max_z_height: ([\d.]+)',g).group(1))==11
 checked=checks.check_keepout(g)
 xml=E.fromstring(z.read('Metadata/slice_info.config'));model=xml.find(".//metadata[@key='printer_model_id']")
 profile=json.loads(Path('/Applications/BambuStudio.app/Contents/Resources/profiles/BBL/machine/Bambu Lab P1S.json').read_text())
 assert profile['model_id']=='C12' and model is not None and model.get('value') in ('','C12')
 model.set('value',profile['model_id']);name='Metadata/slice_info.config'
 entries[name]=(entries[name][0],E.tostring(xml,encoding='utf-8',xml_declaration=True))
 (HERE/'preview.png').write_bytes(z.read('Metadata/plate_1.png'))
tmp=package.with_suffix('.tmp')
with zipfile.ZipFile(tmp,'w',zipfile.ZIP_DEFLATED) as out:
 for info,data in entries.values():out.writestr(info,data)
with zipfile.ZipFile(tmp) as z:
 assert z.testzip() is None and z.read('Metadata/plate_1.gcode')==code
 assert E.fromstring(z.read('Metadata/slice_info.config')).find(".//metadata[@key='printer_model_id']").get('value')=='C12'
tmp.replace(package)
bed=summary['beds'][0]
entry={'file':package.name,'sha256':hashlib.sha256(package.read_bytes()).hexdigest(),'gcode_sha256':hashlib.sha256(code).hexdigest(),'printer_model_id':'C12','model_id_source':'installed Bambu P1S vendor profile; G-code unchanged','layers':layers,'max_z_mm':11,'supports':False,'filament_g':float(bed['filament_g']),'print_time':bed['print_time'],'estimated_seconds_including_overhead':bed['estimated_s'],'checked_extrusion_endpoints':checked}
report={'dispatched':False,'verified_settings':expected,'builds':{'fit':{'plates':[entry]}}}
(HERE/'evidence').mkdir(exist_ok=True);(HERE/'evidence/slices.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(entry,indent=2))
