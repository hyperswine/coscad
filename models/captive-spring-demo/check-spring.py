"""Check extrusion coverage of the spiral centreline at all six strip layers."""
import math,re,json
from pathlib import Path
from shapely.geometry import LineString,Point
from shapely.ops import unary_union
root=Path(__file__).resolve().parent
points=[tuple(map(float,l.split()[1:])) for l in (root/'plate.stl').read_text().splitlines() if l.strip().startswith('vertex ')]
ox,oy=[128-(min(p[k] for p in points)+max(p[k] for p in points))/2 for k in (0,1)]
x=y=z=0.; width=.42; paths={}
for line in (root/'a1-wing-right/slice/plate_1.gcode').read_text().splitlines():
 if line.startswith('; LINE_WIDTH:'):width=float(line.split(':')[1])
 if line.startswith(('G0 ','G1 ')):
  fields={k:float(v) for k,v in re.findall(r'([XYZE])(-?[\d.]+)',line.split(';')[0])}
  a,b,c=fields.get('X',x),fields.get('Y',y),fields.get('Z',z)
  if fields.get('E',0)>0 and (a,b)!=(x,y) and .19<=c<=1.21:
   paths.setdefault(round(c,2),[]).append(LineString([(x,y),(a,b)]).buffer(width/2+.03))
  x,y,z=a,b,c
report=[]
for z in [.2,.4,.6,.8,1.,1.2]:
 region=unary_union(paths.get(z,[]))
 missing=[]
 tested=0
 for i in range(1001):
  t=3*math.pi*i/1000;r=3+7*i/1000
  if math.hypot(r*math.cos(t)+11,r*math.sin(t))<1.55:continue  # Anchor bore and perimeter transition.
  tested+=1
  if not region.covers(Point(36+ox+r*math.cos(t),oy+r*math.sin(t))):missing.append(i)
 pieces=list(region.geoms) if region.geom_type=="MultiPolygon" else [region]
 connected=any(p.covers(Point(ox+39,oy)) and p.covers(Point(ox+23.2,oy)) for p in pieces)
 assert connected, z
 report.append({'layer_z_mm':z,'hub_to_anchor_connected':connected,'centreline_samples':tested,'uncovered_samples':len(missing)})
print(json.dumps(report,indent=2))
(root/'spiral-coverage.json').write_text(json.dumps(report,indent=2))
assert all(r['uncovered_samples']==0 for r in report)
