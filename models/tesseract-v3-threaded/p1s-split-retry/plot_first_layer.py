#!/usr/bin/env python3
"""Draw actual deposited first-layer paths for the first retry plate."""
from pathlib import Path
import re,math,json
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
from matplotlib.patches import Rectangle
HERE=Path(__file__).resolve().parent/'bed1'
offset=np.array([float(v) for v in json.loads((HERE/'presets/machine.json').read_text())['extruder_offset'][0].split('x')])
x=y=z=e=0.;absolute=True;relative=True;active=False;feature='';lines={'brim':[],'part':[]}
for line in (HERE/'slice/plate_1.gcode').open():
 if line.startswith('; CHANGE_LAYER'):active=True
 if line.startswith('; Z_HEIGHT:') and float(line.split(':')[1])>.21:break
 if line.startswith('; FEATURE:'):feature=line.split(':')[1].strip()
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
 nx=v.get('X',x) if absolute else x+v.get('X',0);ny=v.get('Y',y) if absolute else y+v.get('Y',0);nz=v.get('Z',z) if absolute else z+v.get('Z',0);de=v.get('E',0) if relative else v.get('E',e)-e
 if 'E' in v:e=v['E']
 if active and de>0 and (nx!=x or ny!=y) and feature not in ['Custom','']:
  if cmd in ['G2','G3']:
   cx=x+v.get('I',0);cy=y+v.get('J',0);r=math.hypot(x-cx,y-cy);a=math.atan2(y-cy,x-cx);b=math.atan2(ny-cy,nx-cx);delta=(b-a)%(2*math.pi) if cmd=='G3' else -((a-b)%(2*math.pi));angles=np.linspace(a,a+delta,max(2,math.ceil(abs(delta)*r/.25)+1));pts=np.column_stack((cx+r*np.cos(angles),cy+r*np.sin(angles)))
  else:pts=np.array([[x,y],[nx,ny]])
  lines['brim' if feature in ['Skirt','Brim'] else 'part'].append(pts+offset)
 x,y,z=nx,ny,nz
fig,ax=plt.subplots(figsize=(8,8),facecolor='#faf9f5');ax.set_facecolor('#f0efea')
ax.add_patch(Rectangle((0,0),256,256,fill=False,edgecolor='#727a75',linewidth=1.4));ax.add_patch(Rectangle((20,20),216,216,fill=False,edgecolor='#aaa',linestyle='--',linewidth=.7));ax.add_patch(Rectangle((0,0),18,28,color='#dab6b0'))
for name,color in [('part','#24647b'),('brim','#dc9133')]:ax.add_collection(LineCollection(lines[name],colors=color,linewidths=.65,label='Cap toolpaths' if name=='part' else 'Brim toolpaths'))
for item in json.loads((HERE/'packing.json').read_text())['items']:
 cx,cy=item['center_xy_mm'];ax.text(cx,cy,str(item['id']),ha='center',va='center',color='white',fontsize=12,fontweight='bold')
ax.set(xlim=(-3,259),ylim=(-3,259),aspect='equal',xlabel='Bed X (mm)',ylabel='Bed Y (mm)',title='P1S retry — actual first-layer toolpaths\nEight caps, 8 mm brim setting on every object')
ax.legend(loc='center',framealpha=1,fontsize=8);fig.tight_layout();fig.savefig(HERE/'first-layer-brims.png',dpi=150)
print('Saved actual first-layer brim evidence')
