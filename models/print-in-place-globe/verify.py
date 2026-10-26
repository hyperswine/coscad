"""Mesh invariants and clearance sweep for the exact printable geometry."""
import json,math
from pathlib import Path
import numpy as np,trimesh
B=Path(__file__).resolve().parent
stand=trimesh.load_mesh(B/'stand.stl');rotor=trimesh.load_mesh(B/'rotor.stl');whole=trimesh.load_mesh(B/'globe.stl')
result={}
for name,m,count in [('stand',stand,1),('rotor',rotor,1),('assembled',whole,2)]:
 parts=m.split()
 assert m.is_watertight and m.is_winding_consistent and m.volume>0,name
 assert len(parts)==count,(name,len(parts))
 result[name]={'watertight':bool(m.is_watertight),'components':len(parts),'triangles':len(m.faces),'volume_mm3':float(m.volume),'bounds_mm':m.bounds.tolist()}
# Shaft bore and polar thrust zones are checked on exact mesh in the sweep below.
angles=list(range(0,360,15));max_overlap=0
for zshift in [0,-0.6,0.6]:
 for angle in angles:
  m=rotor.copy();m.apply_transform(trimesh.transformations.rotation_matrix(math.radians(angle),[0,0,1]));m.apply_translation([0,0,zshift])
  intersection=trimesh.boolean.intersection([stand,m],engine='manifold')
  vol=0 if intersection.is_empty else abs(intersection.volume)
  max_overlap=max(max_overlap,vol)
  assert vol<1e-5,(angle,zshift,vol)
result['rotation_check']={'angles_degrees':angles,'axial_offsets_mm':[0,-0.6,0.6],'maximum_intersection_mm3':float(max_overlap)}
result['clearance']={'nominal_radial_mm':0.6,'conservative_faceted_radial_mm':3.1*math.cos(math.pi/96)-2.5,'nominal_conical_bearing_axial_gap_mm':0.8,'lowest_rotor_z_mm':float(rotor.bounds[0,2]),'ring_vs_land_minimum_radial_mm':36-32.8,'bearing_support_blocker_radius_mm':5.5}
assert whole.bounds[0,2]>=-1e-5
assert max(whole.extents[:2])<100
(B/'validation.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
