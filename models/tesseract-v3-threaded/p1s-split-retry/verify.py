#!/usr/bin/env python3
"""Verify the actual one-plate P1S package, placement, brims and support paths."""
from pathlib import Path
import argparse,hashlib,json,math,re,sys,zipfile,xml.etree.ElementTree as E
import numpy as np
import trimesh
from shapely import wkt,contains_xy
from shapely.geometry import MultiPoint,box
from shapely.ops import unary_union
PROJECT=Path(__file__).resolve().parent;SOURCE=PROJECT.parent
ap=argparse.ArgumentParser();ap.add_argument('--bed',type=int,choices=range(1,6),required=True);ap.add_argument('--slice-dir',default='slice');args=ap.parse_args();HERE=PROJECT/('bed'+str(args.bed));OUT=HERE/args.slice_dir
expected_count=len(json.loads((HERE/'packing.json').read_text())['items'])
sys.path.insert(0,str(SOURCE.parent/'print-in-place-globe/v2-threaded'))
from support_roots import SupportRoots
result=json.loads((OUT/'result.json').read_text());assert result['return_code']==0 and result['error_string']=='Success.',result.get('error_string')
assert len(result['sliced_plates'])==1
plate=result['sliced_plates'][0];assert not plate.get('warning_message'),plate.get('warning_message')
assert len(plate['objects'])==expected_count,len(plate['objects'])
package=OUT/('tesseract-bed'+str(args.bed)+'-p1s.gcode.3mf')
packing=json.loads((HERE/'packing.json').read_text());parts=json.loads((HERE/'project-parts.json').read_text());assert len(packing['items'])==expected_count
# Saved normal meshes are single solids, brims/envelopes and bed keep-out clear.
normal={};allow=[]
for item in packing['items']:
 mesh=trimesh.load_mesh(HERE/item['file']);assert mesh.is_watertight and len(mesh.split())==1
 assert mesh.bounds[0,2]>=-1e-5 and mesh.bounds[1,2]<=100
 normal[item['id']]=mesh
 hull=MultiPoint(mesh.vertices[:,:2]).convex_hull
 footprint=hull.buffer(item['brim_mm']+.3);assert not footprint.intersects(box(0,0,18,28))
 assert box(20,20,236,236).contains(footprint);allow.append(footprint)
allowed=unary_union(allow)
inner_ids=[i['id'] for i in packing['items'] if i['name']=='inner']
support_area=MultiPoint(normal[inner_ids[0]].vertices[:,:2]).convex_hull.buffer(8.5) if inner_ids else None
# Derive exclusion cylinders from their actual compiled meshes, using parallel
# end-face normals; this checks the regions placed in the sliced project.
regions=[]
for file in parts['thread_exclusions']:
 m=trimesh.load_mesh(HERE/file);groups={}
 for n,a in zip(m.face_normals,m.area_faces):
  n=n.copy()
  if n[np.argmax(abs(n))]<0:n=-n
  key=tuple(n.round(5));groups[key]=groups.get(key,0)+a
 axis=np.array(max(groups,key=groups.get));axis/=np.linalg.norm(axis);center=m.center_mass
 rel=m.vertices-center;dist=rel@axis;rad=np.linalg.norm(rel-dist[:,None]*axis,axis=1)
 assert abs(max(dist)-min(dist)-10)<.01 and abs(rad.max()-5.65)<.01
 regions.append((center,axis,5.65,5.0))
# Model identity + exported per-object overrides must survive the slicer.
with zipfile.ZipFile(package) as z:
 assert z.testzip() is None
 assert [n for n in z.namelist() if re.fullmatch(r'Metadata/plate_\d+\.gcode',n)]==['Metadata/plate_1.gcode']
 entries={i.filename:(i,z.read(i)) for i in z.infolist()};code=z.read('Metadata/plate_1.gcode');assert code==(OUT/'plate_1.gcode').read_bytes()
 settings=json.loads(z.read('Metadata/project_settings.config'))
 assert settings['curr_bed_type']=='Textured PEI Plate', 'Wrong exported build plate'
 startup=code.split(b'; CHANGE_LAYER',1)[0]
 bed_commands=re.findall(rb'^M(?:140|190)\s+S([0-9.]+)',startup,re.M)
 bed_waits=re.findall(rb'^M190\s+S([0-9.]+)',startup,re.M)
 assert bed_commands and bed_waits and float(bed_commands[-1])==55 and float(bed_waits[-1])==55, 'Final startup bed target/wait must be 55 C'
 assert settings['printer_model']=='Bambu Lab P1S' and settings['nozzle_diameter']==['0.4'] and settings['filament_settings_id']==['Generic PLA']
 for k,v in json.loads((HERE/'process.json').read_text()).items():
  actual=settings[k];actual=actual[0] if isinstance(actual,list) and len(actual)==1 else actual
  assert actual==v,(k,actual,v)
 cfg=E.fromstring(z.read('Metadata/model_settings.config'));obj={o.find("metadata[@key='name']").get('value'):o for o in cfg.findall('object')}
 assert len(obj)==expected_count
 for item in parts['parts']:
  o=obj[item['label']]
  assert float(o.find("metadata[@key='brim_width']").get('value'))==item['brim_mm']
  assert o.find("metadata[@key='enable_support']").get('value')==('1' if item['enable_support'] else '0')
 xml=E.fromstring(z.read('Metadata/slice_info.config'));model=xml.find(".//metadata[@key='printer_model_id']")
 vendor=json.loads(Path('/Applications/BambuStudio.app/Contents/Resources/profiles/BBL/machine/Bambu Lab P1S.json').read_text());assert vendor['model_id']=='C12' and model.get('value') in ('','C12')
 model.set('value','C12');name='Metadata/slice_info.config';entries[name]=(entries[name][0],E.tostring(xml,encoding='utf-8',xml_declaration=True))
 (HERE/'plate-preview.png').write_bytes(z.read('Metadata/plate_1.png'))
# Validate extracted geometry, including production-extension referenced models.
ns={'m':'http://schemas.microsoft.com/3dmanufacturing/core/2015/02'}
def matrix(value):
 if not value:return np.eye(4)
 v=np.array(list(map(float,value.split()))).reshape(4,3).T;t=np.eye(4);t[:3,:]=v;return t
with zipfile.ZipFile(package) as z:
 cache={}
 def doc(path):
  if path not in cache:cache[path]=E.fromstring(z.read(path))
  return cache[path]
 root=doc('3D/3dmodel.model')
 def geometry(path,ident,transform):
  ob=doc(path).find("m:resources/m:object[@id='"+ident+"']",ns);assert ob is not None,(path,ident)
  mesh=ob.find('m:mesh',ns)
  if mesh is not None:
   vs=np.array([[float(v.get(k)) for k in ['x','y','z']] for v in mesh.findall('m:vertices/m:vertex',ns)])
   fs=np.array([[int(f.get(k)) for k in ['v1','v2','v3']] for f in mesh.findall('m:triangles/m:triangle',ns)])
   out=trimesh.Trimesh(vs,fs,process=False);out.apply_transform(transform);return [out]
  out=[]
  for child in ob.findall('m:components/m:component',ns):
   ref=next((v for k,v in child.attrib.items() if k.endswith('}path')),path).lstrip('/')
   out+=geometry(ref,child.get('objectid'),transform@matrix(child.get('transform')))
  return out
 # Every build item contains one normal part, plus optional excluded volumes.
 # Identify the largest connected volume (blockers are smaller separate parts).
 actual=[]
 for item in root.findall('m:build/m:item',ns):
  ref=next((v for k,v in item.attrib.items() if k.endswith('}path')),'3D/3dmodel.model').lstrip('/')
  meshes=geometry(ref,item.get('objectid'),matrix(item.get('transform')))
  actual.append(max(meshes,key=lambda m:abs(m.volume)))
 assert len(actual)==expected_count
 unmatched=list(actual);max_bound_error=0
 for expected in normal.values():
  errors=[np.max(abs(m.bounds-expected.bounds)) for m in unmatched];i=int(np.argmin(errors));error=float(errors[i]);assert error<.001,error
  observed=unmatched.pop(i);assert abs(abs(observed.volume)-expected.volume)<.02
  max_bound_error=max(max_bound_error,error)
# Bed/keep-out, per-part brims, support footprint, and threaded cavity checks.
offset=np.array([float(x) for x in json.loads((HERE/'presets/machine.json').read_text())['extruder_offset'][0].split('x')])
brim_masks={i['id']:MultiPoint(normal[i['id']].vertices[:,:2]).convex_hull.buffer(8.3) for i in packing['items']}
brim_objects={i['id']:{'segments':0,'min_radius':999.,'max_radius':0.,'sectors':set()} for i in packing['items']}
roots=SupportRoots();x=y=z=0.;e=0.;absolute=True;relative=True;active=False;feature='';width=.42;segments=0;support=0;brim=0;arc=0;intrusions=[];outside=[]
for line in code.decode().splitlines():
 if line.startswith('; CHANGE_LAYER'):active=True
 if line.startswith('; Z_HEIGHT:'):roots.layer(float(line.split(':')[1]))
 if line.startswith('; FEATURE:'):feature=line.split(':')[1].strip()
 if line.startswith('; LINE_WIDTH:'):width=float(line.split(':')[1])
 if line.startswith('; filament end gcode'):active=False
 raw=line.split(';')[0].strip()
 if not raw:continue
 cmd=raw.split()[0]
 if cmd=='G90':absolute=True
 if cmd=='G91':absolute=False
 if cmd=='M83':relative=True
 if cmd=='M82':relative=False
 v={k:float(n) for k,n in re.findall(r'([XYZIJE])(-?\d*\.?\d+)',raw)}
 if cmd=='G92':
  if 'E' in v:e=v['E']
  continue
 if cmd not in ['G0','G1','G2','G3']:continue
 nx=v.get('X',x) if absolute else x+v.get('X',0);ny=v.get('Y',y) if absolute else y+v.get('Y',0);nz=v.get('Z',z) if absolute else z+v.get('Z',0)
 de=v.get('E',0) if relative else v.get('E',e)-e
 if 'E' in v:e=v['E']
 if active and de>0 and (nx!=x or ny!=y) and feature not in ['Custom','']:
  segments+=1
  special=feature.startswith('Support') or feature in ['Skirt','Brim']
  if cmd in ['G2','G3']:
   arc+=1;cx=x+v.get('I',0);cy=y+v.get('J',0);r=math.hypot(x-cx,y-cy);a=math.atan2(y-cy,x-cx);b=math.atan2(ny-cy,nx-cx)
   delta=(b-a)%(2*math.pi) if cmd=='G3' else -((a-b)%(2*math.pi));n=max(2,math.ceil(abs(delta)*max(r,1)/.25)+1);angles=np.linspace(a,a+delta,n);pts=np.column_stack((cx+r*np.cos(angles),cy+r*np.sin(angles)))
  else:
   n=max(2,math.ceil(math.hypot(nx-x,ny-y)/.25)+1) if special else 2
   pts=np.column_stack((np.linspace(x,nx,n),np.linspace(y,ny,n)))
  pts+=offset
  assert np.all(pts>=width/2-.05) and np.all(pts<=256-width/2+.05),(feature,pts.min(0),pts.max(0))
  assert not np.any((pts[:,0]>-.2)&(pts[:,0]<18.2)&(pts[:,1]>-.2)&(pts[:,1]<28.2)),(feature,'keepout')
  if feature.startswith('Support'):
   support+=1;roots.add(pts)
   if support_area is None or not np.all(contains_xy(support_area,pts[:,0],pts[:,1])):outside.append([nz,pts.min(0).tolist(),pts.max(0).tolist()])
   p3=np.column_stack((pts,np.full(len(pts),nz)))
   for center,axis,radius,half in regions:
    rel=p3-center;axial=rel@axis;rad=np.linalg.norm(rel-axial[:,None]*axis,axis=1)
    if np.any((abs(axial)<half-.1)&(rad<radius-.1)):intrusions.append([nz,float(rad.min())])
  if feature in ['Skirt','Brim']:
   brim+=1;assert np.all(contains_xy(allowed,pts[:,0],pts[:,1])),('brim exceeds reserved footprint',pts.min(0),pts.max(0))
   if nz<.3:
    for item in packing['items']:
     center=np.array(item['center_xy_mm']);rel=pts-center;rad=np.linalg.norm(rel,axis=1)
     hull=brim_masks[item['id']]
     if not np.all(contains_xy(hull,pts[:,0],pts[:,1])):continue
     record=brim_objects[item['id']];record['segments']+=1;record['min_radius']=min(record['min_radius'],float(rad.min()));record['max_radius']=max(record['max_radius'],float(rad.max()))
     if item['name']=='cap':
      ring=rel[rad>26]
      record['sectors'].update((np.floor((np.arctan2(ring[:,1],ring[:,0])+math.pi)/(2*math.pi)*36).astype(int)%36).tolist())
 x,y,z=nx,ny,nz
assert segments>10000 and brim>0,(segments,support,brim)
assert (support>0) if inner_ids else (support==0),(inner_ids,support)
for item in packing['items']:
 record=brim_objects[item['id']];assert record['segments']>0,(item['id'],'no actual brim')
 if item['name']=='cap':assert record['max_radius']>26.5 and len(record['sectors'])==36,(item['id'],record)
 record['angular_sectors_covered']=len(record['sectors']);del record['sectors']
assert not outside,('support outside reserved inner-frame area',outside[:5])
assert not intrusions,('support inside threaded cavity',intrusions[:5])
root_report=roots.report();assert (root_report['bed_components']>0 and not root_report['components_without_prior_support']) if inner_ids else (root_report['support_components_checked']==0),root_report
# Apply only the verified vendor model ID; retain every G-code byte.
tmp=package.with_suffix('.tmp')
with zipfile.ZipFile(tmp,'w',zipfile.ZIP_DEFLATED) as out:
 for info,data in entries.values():out.writestr(info,data)
with zipfile.ZipFile(tmp) as z:assert z.testzip() is None and z.read('Metadata/plate_1.gcode')==code
tmp.replace(package)
report={'package':str(package.relative_to(HERE)),'package_sha256':hashlib.sha256(package.read_bytes()).hexdigest(),'gcode_sha256':hashlib.sha256(code).hexdigest(),'printer_model_id':'C12','normal_objects':expected_count,'bed_index':args.bed,'per_object_brim_evidence':brim_objects,'single_plate':True,'max_geometry_bound_error_mm':max_bound_error,'deposition_segments':segments,'arc_segments':arc,'arc_sampling_mm':.25,'support_segments':support,'support_thread_intrusions':len(intrusions),'support_footprint_violations':len(outside),'brim_segments':brim,'support_roots':root_report,'total_estimated_seconds':plate['total_predication'],'filaments':plate['filaments'],'slicer_warnings':plate.get('warning_message'),'checked':True,'exported_bed_type':settings['curr_bed_type'],'final_startup_bed_target_c':float(bed_commands[-1]),'final_startup_bed_wait_c':float(bed_waits[-1])}
(HERE/'package-validation.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report,indent=2))
