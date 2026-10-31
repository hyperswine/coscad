#!/usr/bin/env python3
"""Draw actual deposited first-layer paths for the first retry plate."""
from pathlib import Path
import re,math,json
import numpy as np
from PIL import Image,ImageDraw
HERE=Path(__file__).resolve().parent
offset=np.array([float(v) for v in json.loads((HERE/'presets/machine.json').read_text())['extruder_offset'][0].split('x')])
x=y=z=e=0.;absolute=True;relative=True;active=False;feature='';lines={'support':[],'part':[]}
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
  lines['support' if feature.startswith('Support') else 'part'].append(pts+offset)
 x,y,z=nx,ny,nz
im=Image.new('RGB',(1000,1050),'#faf9f5');draw=ImageDraw.Draw(im)
draw.text((40,20),'A1 center: actual first-layer toolpaths',fill='#222222')
draw.text((40,40),'Orange: frame + four 0.2 mm mouse-ear pads. Green: reinforced tree bases.',fill='#222222')
def xy(pt):return (50+(float(pt[0])-93)*900/70,1000-(float(pt[1])-93)*900/70)
for name,color in [('support','#38855c'),('part','#dc9133')]:
 for path in lines[name]:draw.line([xy(p) for p in path],fill=color,width=2)
for pad in json.loads((HERE/'plan.json').read_text())['mouse_ear_pads']:
 x,y=xy(pad['center_xy_mm']);draw.text((x-5,y-5),pad['name'].split('-')[-1],fill='#222222')
im.save(HERE/'first-layer-supports-and-ears.png')
print('Saved actual first-layer support/pad evidence')
