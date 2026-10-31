#!/usr/bin/env python3
"""Verify the center-only A1 support/adhesion experiment before every start."""
from pathlib import Path
import hashlib,json,subprocess,sys,zipfile
import numpy as np
import trimesh
from shapely.geometry import LineString,MultiPoint
from shapely.ops import polygonize
HERE=Path(__file__).resolve().parent;SPLIT=HERE.parents[1]
sys.path.insert(0,str(SPLIT.parents[2]/'scripts'))
from bambu_preflight import preset_expectations,verify_package

def verify():
 subprocess.run([sys.executable,str(SPLIT/'verify.py'),'--bed','2','--printer','a1','--output-dir',str(HERE)],check=True,stdout=subprocess.DEVNULL)
 plan=json.loads((HERE/'plan.json').read_text());geometry=json.loads((HERE/'package-validation.json').read_text())
 source=Path(plan['source_mesh']);assert hashlib.sha256(source.read_bytes()).hexdigest()==plan['source_sha256']
 assert (HERE/'1-inner.stl').read_bytes()==source.read_bytes(), 'Center mesh changed'
 assert geometry['normal_objects']==1 and geometry['printer_model_id']=='N2S' and geometry['checked']
 package=HERE/'slice/tesseract-bed2-a1.gcode.3mf'
 thermal=verify_package(package,preset_expectations(HERE/'presets',plate='Textured PEI Plate'))
 assert thermal['package_sha256']==geometry['package_sha256']
 with zipfile.ZipFile(package) as z:
  settings=json.loads(z.read('Metadata/project_settings.config'))
  assert settings['printer_settings_id']=='Bambu Lab A1 0.4 nozzle'
  assert settings['brim_type']=='no_brim' and int(settings['raft_layers'])==0
  for key,value in plan['process_overrides'].items():assert settings[key]==value,(key,settings[key],value)
 mesh=trimesh.load_mesh(source);seg=trimesh.intersections.mesh_plane(mesh,[0,0,1],[0,0,.1])
 patches=[p for p in polygonize([LineString(x[:,:2]) for x in np.round(seg,5)]) if p.area>.1]
 assert len(patches)==4 and len(plan['mouse_ear_pads'])==4
 for pad in plan['mouse_ear_pads']:
  ear=trimesh.load_mesh(HERE/pad['file']);footprint=MultiPoint(ear.vertices[:,:2]).convex_hull
  matching=[p for p in patches if footprint.intersection(p).area>.3]
  assert len(matching)==1 and matching[0].centroid.distance(footprint.centroid)<.01
 report={'checked':True,'package_sha256':thermal['package_sha256'],'thermal':thermal,'unchanged_center_geometry':True,'normal_objects':1,'additional_sacrificial_mouse_ear_volumes':4,'ear_height_mm':.2,'ear_radius_mm':7.5,'native_painted_brim_used':False,'full_brim_used':False,'source_orientation_retained':True,'thread_bore_blockers_retained':True,'support_thread_intrusions':geometry['support_thread_intrusions'],'support_footprint_violations':geometry['support_footprint_violations'],'bed_connected_support_roots':geometry['support_roots'],'actual_first_layer_pad_evidence':geometry['support_base_anchor_evidence'],'estimated_seconds':geometry['total_estimated_seconds'],'filaments':geometry['filaments'],'limitation':'Model and toolpath checks do not establish lower female-thread print quality. Closer PLA interfaces and thin pads need careful removal.'}
 (HERE/'experiment-validation.json').write_text(json.dumps(report,indent=2)+'\n')
 return report
if __name__=='__main__':
 if not __debug__:raise RuntimeError('Run validation without Python optimization')
 r=verify();print(json.dumps({'checked':True,'seconds':r['estimated_seconds'],'mouse_ears':4,'threads_clear':r['support_thread_intrusions']==0}))
