"""Check the replacement mesh and clearances with OpenSCAD boolean probes."""
from pathlib import Path
import re,collections,json,subprocess
base=Path(__file__).resolve().parent
app='/Applications/OpenSCAD.app/Contents/MacOS/openscad'
def mesh(path):
 v=[tuple(map(float,m)) for m in re.findall(r'vertex\s+([-\d.eE+]+)\s+([-\d.eE+]+)\s+([-\d.eE+]+)',path.read_text())]
 return [v[i:i+3] for i in range(0,len(v),3)]
f=mesh(base/'bow-center.stl');edges=collections.defaultdict(list)
for i,t in enumerate(f):
 for j in range(3):edges[tuple(sorted((t[j],t[(j+1)%3])))].append(i)
assert all(len(a)==2 for a in edges.values())
adj=collections.defaultdict(set)
for a,b in edges.values():adj[a].add(b);adj[b].add(a)
seen={0};todo=[0]
while todo:
 for k in adj[todo.pop()]-seen:seen.add(k);todo.append(k)
assert len(seen)==len(f)
probes={
 'm4_bolt_paths':'union(){for(x=[-42,42])for(y=[-8,8])translate([x,y,0])cylinder(r=2,h=24,center=true,$fn=50);}',
 'm4_nut_seats':'union(){for(x=[-42,42])for(y=[-8,8])translate([x,y,-7.01])cylinder(r=7/sqrt(3),h=3.2,$fn=6);}',
 'arm_pockets':'union(){for(x=[-42,42])translate([x,0,3.9])cube([27.4,28.4,8.1],center=true);}',
 'top_recess':'hull(){for(x=[-13,13])translate([x,0,6.5])cylinder(r=6.9,h=1.2,center=true,$fn=50);}'
}
results={}
for name,probe in probes.items():
 scad=base/f'check-{name}.scad';out=scad.with_suffix('.stl')
 scad.write_text('intersection(){import("bow-center.stl");'+probe+'}\n')
 proc=subprocess.run([app,'-o',str(out),str(scad)],capture_output=True,text=True)
 empty='Current top level object is empty' in proc.stderr
 assert empty and (not out.exists() or not mesh(out)), (name,proc.stderr)
 results[name]='zero interference'
report={'closed_manifold':True,'connected_components':1,'triangles':len(f),'probes':results}
(base/'validation.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(report,indent=2))
