"""Inspect deposited paths, including G2/G3 arcs, around the captive bearings."""
from pathlib import Path
import re,math,json
import numpy as np
B=Path(__file__).resolve().parent
x=y=z=0.;height=0.;width=.42;feature='';active=False;absolute=True;relative_e=True;e=0.
support_min=1e9;support_count=0;support_near=[];gap_violations=[];first_rotor=None;layers={};mid=[]
for line in (B/'slice/plate_1.gcode').open():
 if line.startswith('; CHANGE_LAYER'):active=True
 if line.startswith('; Z_HEIGHT:'):height=float(line.split(':')[1])
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
  rad=np.linalg.norm(pts-[128,128],axis=1);lo=float(rad.min())-width/2;hi=float(rad.max())+width/2
  if feature.startswith('Support') and 20.5<=height<=89.5:
   support_count+=1;support_min=min(support_min,lo)
   if lo<5.35:support_near.append([height,feature,round(lo,3),round(hi,3)])
  if 40<=height<=70 and not feature.startswith('Support') and lo<3.0 and hi>2.6:
   gap_violations.append([height,feature,lo,hi])
  if not feature.startswith('Support') and height>=23.2 and 5.0<lo<10 and hi<15 and first_rotor is None:first_rotor=height
  if 22.7<=height<=23.5:
   layers.setdefault(round(height,3),[]).append((feature,pts.tolist()))
  if 53.9<=height<=54.1:mid.append((feature,pts.tolist()))
 x,y,z=nx,ny,nz
report={'support_segments_checked':support_count,'minimum_support_footprint_radius_mm_in_blocker_height':support_min,'bearing_gap_crossing_segments':len(gap_violations),'inspected_lower_rotor_layer_z_mm':first_rotor,'near_start_layers':{str(k):sorted(set(f for f,p in vs)) for k,vs in layers.items()},'arc_sampling_mm':.05,'near_support_layers':sorted(set(h for h,f,l,r in support_near)),'closest_support':sorted(support_near,key=lambda q:q[2])[:8]}
(B/'toolpath-validation.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report,indent=2))
assert support_count>0
assert not [q for q in support_near if q[0]<=86.6], 'Support intrudes into the moving bearing region'
assert not gap_violations,gap_violations[:5]
assert first_rotor and first_rotor<23.6
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
fig,axes=plt.subplots(1,2,figsize=(12,6))
for ax in axes:ax.set_aspect('equal');ax.set_xlabel('X (mm)');ax.set_ylabel('Y (mm)')
ax=axes[0]
for h,vs in layers.items():
 for is_support,color in [(True,'#e28e33'),(False,'#276daf')]:
  paths=[np.array(p)-[128,128] for f,p in vs if f.startswith('Support')==is_support and (is_support or h==first_rotor)]
  ax.add_collection(LineCollection(paths,colors=color,linewidths=.65,alpha=.8))
ax.set(xlim=(-13,13),ylim=(-13,13),title=f'Lower globe layer Z={first_rotor}\nBlue model, orange nearby supports')
ax=axes[1];ax.add_collection(LineCollection([np.array(p)-[128,128] for f,p in mid],colors='#276daf',linewidths=1))
ax.set(xlim=(-5,5),ylim=(-5,5),title='Mid-height axle and bore\nSeparate extrusion paths')
fig.tight_layout();fig.savefig(B/'toolpath-check.png',dpi=150)
