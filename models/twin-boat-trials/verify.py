"""Freshwater displacement and transverse stability; not a physical leak test."""
from pathlib import Path
import json,math,os
os.environ.setdefault('MPLCONFIGDIR','/private/tmp/boat-matplotlib')
import numpy as np,trimesh,manifold3d as md
import matplotlib;matplotlib.use('Agg')
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
B=Path(__file__).resolve().parent

def manifold(m):return md.Manifold(md.Mesh(np.asarray(m.vertices,dtype=np.float32),np.asarray(m.faces,dtype=np.uint32)))
def mesh(m):
 data=m.to_mesh();return trimesh.Trimesh(np.asarray(data.vert_properties)[:,:3],np.asarray(data.tri_verts),process=False)
def heel_state(name,mass_g,degrees):
 body=trimesh.load_mesh(B/(name+'.stl'));env=trimesh.load_mesh(B/(name+'-envelope.stl'));opening=trimesh.load_mesh(B/(name+'-opening.stl'))
 rotation=trimesh.transformations.rotation_matrix(math.radians(degrees),[1,0,0])
 for m in [body,env,opening]:m.apply_transform(rotation)
 model=manifold(env);target=mass_g*1000
 lo,hi=env.bounds[:,2]
 assert model.volume()>target,'Insufficient full displacement'
 for _ in range(25):
  water=(lo+hi)/2;under=model.trim_by_plane((0,0,-1),-water)
  if under.volume()<target:lo=water
  else:hi=water
 water=(lo+hi)/2;under=mesh(model.trim_by_plane((0,0,-1),-water))
 g=body.center_mass;b=under.center_mass
 return {'heel_deg':degrees,'waterline_mm':float(water),'displaced_g':float(under.volume/1000),'righting_arm_mm':float(g[1]-b[1]),'opening_clearance_above_water_mm':float(opening.vertices[:,2].min()-water),'buoyancy_center_mm':b.tolist(),'gravity_center_mm':g.tolist()}
report={'assumptions':['Fresh water 1.000 g/cm3; homogeneous solid PLA at 1.24 g/cm3 (sliced at 100% infill).','Air remains in sealed outrigger chambers and un-flooded open hulls.','Static transverse heel about the long axis; no waves, payload, dynamic capsize, measured leakage or fatigue modeled.','Finite opening clearance is a downflooding limit; positive small-angle righting is not full self-righting.']}
fig=plt.figure(figsize=(13,7))
for i,name in enumerate(['monohull','outrigger']):
 m=trimesh.load_mesh(B/(name+'.stl'));shells=m.split();positive=[s for s in shells if s.volume>0];negative=[s for s in shells if s.volume<0]
 assert m.is_watertight and m.is_volume and len(positive)==1
 assert len(negative)==(2 if name=='outrigger' else 0), 'Unexpected closed air-chamber topology'
 env=trimesh.load_mesh(B/(name+'-envelope.stl'));assert env.is_watertight
 mass=float(m.volume*1.24/1000)
 states=[heel_state(name,mass,d) for d in [0,5,10,15,20,25,30,35,40]]
 # Require a meaningful, unflooded restoring range on both actual designs.
 assert all(s['righting_arm_mm']>0 and s['opening_clearance_above_water_mm']>0 for s in states[1:5]),(name,states)
 stable=[s['heel_deg'] for s in states if s['heel_deg'] and s['righting_arm_mm']>0 and s['opening_clearance_above_water_mm']>0]
 report[name]={'watertight_plastic_boundary':True,'connected_body_count':1,'volume_mm3':float(m.volume),'solid_pla_mass_g':mass,'bounds_mm':m.bounds.tolist(),'gravity_center_mm':m.center_mass.tolist(),'maximum_sampled_unflooded_restoring_heel_deg':max(stable),'hydrostatics':states}
 ax=fig.add_subplot(2,2,i+1,projection='3d');poly=Poly3DCollection(m.triangles,facecolors=['#e4b73a','#72a9a0'][i],edgecolors=(0,0,0,.035),linewidth=.08,shade=True,alpha=1);ax.add_collection3d(poly)
 ax.set_xlim(-65,65);ax.set_ylim(-55,55);ax.set_zlim(0,35);ax.set_box_aspect((130,110,35));ax.view_init(elev=34,azim=-60);ax.set_axis_off();ax.set_title(('Conventional low-keel boat · P1S' if i==0 else 'Wide outrigger boat · A1')+f'\n{mass:.1f} g solid PLA estimate')
 ax=fig.add_subplot(2,2,i+3);ax.plot([s['heel_deg'] for s in states],[s['righting_arm_mm'] for s in states],'o-',label='Restoring arm (mm)');ax.plot([s['heel_deg'] for s in states],[s['opening_clearance_above_water_mm'] for s in states],'s-',label='Lowest opening above water (mm)');ax.axhline(0,color='grey',lw=.8);ax.set_xlabel('Heel angle (degrees)');ax.grid(alpha=.2);ax.legend(fontsize=8)
fig.suptitle('Two small floating boats: geometry and static freshwater checks\nPhysical water-tightness and recovery still need testing',fontsize=13)
fig.tight_layout();fig.savefig(B/'preview-and-stability.png',dpi=150)
(B/'validation.json').write_text(json.dumps(report,indent=2)+'\n')
for name in ['monohull','outrigger']:print(name,json.dumps({k:v for k,v in report[name].items() if k!='hydrostatics'}))
