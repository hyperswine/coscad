"""Check final printer-specific package, emitted paths, and unchanged G-code."""
from pathlib import Path
import hashlib,json,re,zipfile,xml.etree.ElementTree as E,math
import numpy as np,trimesh
B=Path(__file__).resolve().parent
reports={}
for name,printer,model_id in [('monohull','P1S','C12'),('outrigger','A1','N2S')]:
 folder=B/name;path=folder/'slice'/f'{name}.gcode.3mf';code=(folder/'slice/plate_1.gcode').read_bytes();result=json.loads((folder/'slice/result.json').read_text())
 assert result['return_code']==0 and result['error_string']=='Success.' and len(result['sliced_plates'])==1
 plate=result['sliced_plates'][0];assert not plate['warning_message']
 machine=json.loads((folder/'presets/machine.json').read_text());offset=np.array([float(x) for x in machine['extruder_offset'][0].split('x')]);center=np.array([128.,128.])-offset
 profile=json.loads(Path(f'/Applications/BambuStudio.app/Contents/Resources/profiles/BBL/machine/Bambu Lab {printer}.json').read_text());assert profile['model_id']==model_id
 settings={};feature='';active=False;absolute=True;relative_e=True;x=y=z=e=0.;layers=set();count=0;support=0;bounds=np.array([[math.inf,math.inf,math.inf],[-math.inf,-math.inf,-math.inf]]);features={};extrusion=0.
 for line in code.decode().splitlines():
  if line.startswith('; ') and ' = ' in line:
   k,v=line[2:].split(' = ',1);settings[k]=v
  if line.startswith('; CHANGE_LAYER'):active=True
  if line.startswith('; Z_HEIGHT:'):layers.add(float(line.split(':')[1]))
  if line.startswith('; FEATURE:'):feature=line.split(':')[1].strip()
  text=line.split(';')[0].strip()
  if not text:continue
  cmd=text.split()[0]
  if cmd=='G90':absolute=True
  if cmd=='G91':absolute=False
  if cmd=='M83':relative_e=True
  if cmd=='M82':relative_e=False
  v={k:float(a) for k,a in re.findall(r'([XYZIJE])(-?\d*\.?\d+)',text)}
  if cmd=='G92':
   if 'E' in v:e=v['E']
   continue
  if cmd not in ['G0','G1','G2','G3']:continue
  nx=v.get('X',x) if absolute else x+v.get('X',0);ny=v.get('Y',y) if absolute else y+v.get('Y',0);nz=v.get('Z',z) if absolute else z+v.get('Z',0);de=v.get('E',0) if relative_e else v.get('E',e)-e
  if 'E' in v:e=v['E']
  if active and de>0 and (nx!=x or ny!=y) and feature not in ['Custom','Brim','Skirt']:
   if cmd in ['G2','G3']:
    cx=x+v.get('I',0);cy=y+v.get('J',0);radius=math.hypot(x-cx,y-cy);a=math.atan2(y-cy,x-cx);b=math.atan2(ny-cy,nx-cx)
    delta=(b-a)%(2*math.pi) if cmd=='G3' else -((a-b)%(2*math.pi));samples=max(2,math.ceil(abs(delta)*radius/.1)+1)
    angles=np.linspace(a,a+delta,samples);points=np.column_stack((cx+radius*np.cos(angles)-center[0],cy+radius*np.sin(angles)-center[1],np.linspace(z,nz,samples)))
   else:points=np.array([[x-center[0],y-center[1],z],[nx-center[0],ny-center[1],nz]])
   bounds[0]=np.minimum(bounds[0],points.min(axis=0));bounds[1]=np.maximum(bounds[1],points.max(axis=0));count+=1;features[feature]=features.get(feature,0)+1;extrusion+=de
   if feature.startswith('Support'):support+=1
  x,y,z=nx,ny,nz
 assert settings['wall_loops']=='6' and settings['enable_support']=='0' and settings['sparse_infill_density']=='100%' and settings['sparse_infill_pattern']=='zig-zag'
 assert count>1000 and not support and 100<len(layers)<200
 m=trimesh.load_mesh(B/(name+'.stl'));assert np.all(bounds[0]>=m.bounds[0]-.75) and np.all(bounds[1]<=m.bounds[1]+.75),(name,bounds,m.bounds)
 expected=float(m.volume*1.24/1000);weight=plate['filaments'][0]['total_used_g'];assert .94*expected<weight<1.08*expected
 with zipfile.ZipFile(path) as source:
  assert source.testzip() is None;entries={i.filename:(i,source.read(i)) for i in source.infolist()};assert source.read('Metadata/plate_1.gcode')==code
  cfg=json.loads(source.read('Metadata/project_settings.config'));assert cfg['printer_model']==f'Bambu Lab {printer}' and cfg['enable_support']=='0' and cfg['wall_loops']=='6' and cfg['curr_bed_type']=='Textured PEI Plate'
  assert source.read('Metadata/plate_1.gcode.md5').decode().strip().lower()==hashlib.md5(code).hexdigest()
  info=E.fromstring(source.read('Metadata/slice_info.config'));identity=info.find(".//metadata[@key='printer_model_id']");assert identity is not None and identity.get('value') in ['',model_id];identity.set('value',model_id);key='Metadata/slice_info.config';entries[key]=(entries[key][0],E.tostring(info,encoding='utf-8',xml_declaration=True))
 tmp=path.with_suffix('.tmp')
 with zipfile.ZipFile(tmp,'w',zipfile.ZIP_DEFLATED) as out:
  for metadata,data in entries.values():out.writestr(metadata,data)
 with zipfile.ZipFile(tmp) as check:
  assert check.testzip() is None and check.read('Metadata/plate_1.gcode')==code
 tmp.replace(path)
 reports[name]={'printer':printer,'model_id':model_id,'package':str(path.relative_to(B)),'package_sha256':hashlib.sha256(path.read_bytes()).hexdigest(),'gcode_sha256':hashlib.sha256(code).hexdigest(),'layers':len(layers),'model_path_segments':count,'support_segments':support,'path_bounds_relative_to_model_mm':bounds.tolist(),'machine_extruder_offset_mm':offset.tolist(),'features':features,'estimated_seconds':plate['total_predication'],'estimated_grams':weight,'solid_geometry_grams':expected,'model_id_metadata_filled_without_changing_gcode':True,'passed':True}
 (folder/'package-validation.json').write_text(json.dumps(reports[name],indent=2)+'\n')
print(json.dumps(reports,indent=2))
(B/'packages.json').write_text(json.dumps(reports,indent=2)+'\n')
