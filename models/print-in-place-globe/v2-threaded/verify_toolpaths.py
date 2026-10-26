"""Inspect deposited paths, including G2/G3 arcs, around the captive bearings."""
from pathlib import Path
import re,math,json,hashlib
import numpy as np
from support_roots import SupportRoots
roots=SupportRoots()
B=Path(__file__).resolve().parent
machine=json.loads((B/'presets/machine.json').read_text())
offset=np.array([float(v) for v in machine['extruder_offset'][0].split('x')])
design=json.loads((B/'design.json').read_text())
center=np.array(design['layout_centers_mm']['gimbal'],dtype=float)-offset
x=y=z=0.;height=0.;width=.42;feature='';active=False;absolute=True;relative_e=True;e=0.
socket_support=[];base_thread_support=[]
support_min=1e9;support_count=0;support_near=[];gap_violations=[];first_rotor=None;layers={};mid=[]
for line in (B/'slice/plate_1.gcode').open():
 if line.startswith('; CHANGE_LAYER'):active=True
 if line.startswith('; Z_HEIGHT:'):
  height=float(line.split(':')[1]);roots.layer(height)
 if line.startswith('; FEATURE:'):feature=line.split(':')[1].strip()
 if line.startswith('; LINE_WIDTH:'):width=float(line.split(':')[1])
 code=line.split(';')[0].strip()
 if not code:continue
 cmd=code.split()[0]
 if cmd=='G90':absolute=True
 if cmd=='G91':absolute=False
 if cmd=='M83':relative_e=True
 if cmd=='M82':relative_e=False
 v={k:float(n) for k,n in re.findall(r'([XYZIJE])(-?\d*\.?\d+)',code)}
 if cmd=='G92':
  if 'E' in v:e=v['E']
  continue
 if cmd not in ['G0','G1','G2','G3']:continue
 nx=v.get('X',x) if absolute else x+v.get('X',0)
 ny=v.get('Y',y) if absolute else y+v.get('Y',0)
 nz=v.get('Z',z) if absolute else z+v.get('Z',0)
 de=v.get('E',0) if relative_e else v.get('E',e)-e
 if 'E' in v:e=v['E']
 if active and de>0 and (nx!=x or ny!=y) and feature!='Custom':
  if cmd in ['G2','G3']:
   cx=x+v.get('I',0);cy=y+v.get('J',0);r=math.hypot(x-cx,y-cy)
   a=math.atan2(y-cy,x-cx);b=math.atan2(ny-cy,nx-cx)
   delta=(b-a)%(2*math.pi) if cmd=='G3' else -((a-b)%(2*math.pi))
   n=max(2,math.ceil(abs(delta)*max(r,1)/.05)+1)
   angles=np.linspace(a,a+delta,n);pts=np.column_stack((cx+r*np.cos(angles),cy+r*np.sin(angles)))
  else:
   n=max(2,math.ceil(math.hypot(nx-x,ny-y)/.05)+1)
   pts=np.column_stack((np.linspace(x,nx,n),np.linspace(y,ny,n)))
  rad=np.linalg.norm(pts-center,axis=1);lo=float(rad.min())-width/2;hi=float(rad.max())+width/2
  if feature.startswith('Support'):
   roots.add(pts)
   # Above the thread, the cavity tapers to a point at Z25.4. Trees beside
   # the outside of the tapered foot must not be confused with internal support.
   socket_radius=13.1 if height<=13 else max(0,25.4-height)
   if height<=25.4 and lo<socket_radius:socket_support.append([height,lo])
   base_center=np.array(design['layout_centers_mm']['base'],dtype=float)-offset
   if 7<=height<=20 and np.linalg.norm(pts-base_center,axis=1).min()-width/2<13:base_thread_support.append([height,float(np.linalg.norm(pts-base_center,axis=1).min())])
  if feature.startswith('Support') and 30.5<=height<=99.5:
   support_count+=1;support_min=min(support_min,lo)
   if lo<5.35:support_near.append([height,feature,round(lo,3),round(hi,3)])
  if 50<=height<=80 and not feature.startswith('Support') and lo<3.0 and hi>2.6:
   gap_violations.append([height,feature,lo,hi])
  if not feature.startswith('Support') and height>=33.2 and 5.0<lo<10 and hi<15 and first_rotor is None:first_rotor=height
  if 32.7<=height<=33.5:
   layers.setdefault(round(height,3),[]).append((feature,pts.tolist()))
  if 63.9<=height<=64.1:mid.append((feature,pts.tolist()))
 x,y,z=nx,ny,nz
settings={}
for line in (B/'slice/plate_1.gcode').open():
 if line.startswith('; ') and ' = ' in line:
  k,v=line[2:].rstrip('\r\n').split(' = ',1);settings[k]=v
assert settings['support_on_build_plate_only']=='1'
assert settings['support_type']=='tree(auto)'
assert settings['wall_loops']=='6'
assert not socket_support, socket_support[:10]
assert not base_thread_support, base_thread_support[:10]
report={'support_roots':roots.report(),'effective_support_on_build_plate_only':settings['support_on_build_plate_only'],'effective_wall_loops':settings['wall_loops'],'socket_support_intrusions':socket_support,'male_thread_support_intrusions':base_thread_support,'machine_extruder_offset_mm':offset.tolist(),'toolpath_center_mm':center.tolist(),'support_segments_checked':support_count,'minimum_support_footprint_radius_mm_in_blocker_height':support_min,'bearing_gap_crossing_segments':len(gap_violations),'inspected_lower_rotor_layer_z_mm':first_rotor,'near_start_layers':{str(k):sorted(set(f for f,p in vs)) for k,vs in layers.items()},'arc_sampling_mm':.05,'near_support_layers':sorted(set(h for h,f,l,r in support_near)),'closest_support':sorted(support_near,key=lambda q:q[2])[:8], 'bearing_region_support': [q for q in support_near if q[0]<=96.6]}
assert support_count>0
assert report['support_roots']['bed_components']>0
assert not report['support_roots']['components_without_prior_support'], 'A sampled support component has no connection toward the bed'
assert not [q for q in support_near if q[0]<=96.6], 'Support intrudes into the moving bearing region'
assert not gap_violations,gap_violations[:5]
assert first_rotor and first_rotor<33.6
report['passed']=True
report['gcode_sha256']=hashlib.sha256((B/'slice/plate_1.gcode').read_bytes()).hexdigest()
(B/'toolpath-validation.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report,indent=2))
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
fig,axes=plt.subplots(1,2,figsize=(12,6))
for ax in axes:ax.set_aspect('equal');ax.set_xlabel('X (mm)');ax.set_ylabel('Y (mm)')
ax=axes[0]
for h,vs in layers.items():
 for is_support,color in [(True,'#e28e33'),(False,'#276daf')]:
  paths=[np.array(p)-center for f,p in vs if f.startswith('Support')==is_support and (is_support or h==first_rotor)]
  ax.add_collection(LineCollection(paths,colors=color,linewidths=.65,alpha=.8))
ax.set(xlim=(-13,13),ylim=(-13,13),title=f'Lower globe layer Z={first_rotor}\nBlue model, orange nearby supports')
ax=axes[1];ax.add_collection(LineCollection([np.array(p)-center for f,p in mid],colors='#276daf',linewidths=1))
ax.set(xlim=(-5,5),ylim=(-5,5),title='Mid-height axle and bore\nSeparate extrusion paths')
fig.tight_layout();fig.savefig(B/'toolpath-check.png',dpi=150)
