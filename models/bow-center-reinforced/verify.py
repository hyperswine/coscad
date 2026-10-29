"""Check the replacement mesh and clearances with manifold3d boolean probes.

manifold3d is the Manifold kernel the native compiler links; no OpenSCAD is
needed. Requires numpy and manifold3d. `--mesh` checks another render (for
example `coscad stl bow-center.coscad -o /tmp/bow-center.stl`), `--output`
writes the report elsewhere (default validation.json).
"""
from pathlib import Path
import argparse,re,collections,json,math
import numpy as np,manifold3d as md
base=Path(__file__).resolve().parent
ap=argparse.ArgumentParser(description=__doc__)
ap.add_argument('--mesh',type=Path,default=base/'bow-center.stl')
ap.add_argument('--output',type=Path,default=base/'validation.json')
args=ap.parse_args()
def mesh(path):
 v=[tuple(map(float,m)) for m in re.findall(r'vertex\s+([-\d.eE+]+)\s+([-\d.eE+]+)\s+([-\d.eE+]+)',path.read_text())]
 return [v[i:i+3] for i in range(0,len(v),3)]
f=mesh(args.mesh);edges=collections.defaultdict(list)
for i,t in enumerate(f):
 for j in range(3):edges[tuple(sorted((t[j],t[(j+1)%3])))].append(i)
assert all(len(a)==2 for a in edges.values())
adj=collections.defaultdict(set)
for a,b in edges.values():adj[a].add(b);adj[b].add(a)
seen={0};todo=[0]
while todo:
 for k in adj[todo.pop()]-seen:seen.add(k);todo.append(k)
assert len(seen)==len(f)
index={};tris=[[index.setdefault(p,len(index)) for p in t] for t in f]
part=md.Manifold(md.Mesh64(vert_properties=np.array(list(index),dtype=np.float64),tri_verts=np.array(tris,dtype=np.uint32)))
assert part.status()==md.Error.NoError,part.status()
def cyl(r,h,n=50,center=False):return md.Manifold.cylinder(h,r,r,n,center)
def union(shapes):return md.Manifold.batch_boolean(shapes,md.OpType.Add)
probes={
 'm4_bolt_paths':union([cyl(2,24,center=True).translate([x,y,0]) for x in (-42,42) for y in (-8,8)]),
 'm4_nut_seats':union([cyl(7/math.sqrt(3),3.2,6).translate([x,y,-7.01]) for x in (-42,42) for y in (-8,8)]),
 'arm_pockets':union([md.Manifold.cube([27.4,28.4,8.1],True).translate([x,0,3.9]) for x in (-42,42)]),
 'top_recess':md.Manifold.batch_hull([cyl(6.9,1.2,center=True).translate([x,0,6.5]) for x in (-13,13)])
}
results={}
for name,probe in probes.items():
 common=part^probe
 assert common.is_empty(),(name,common.volume())
 results[name]='zero interference'
report={'closed_manifold':True,'connected_components':1,'triangles':len(f),'probes':results}
args.output.write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(report,indent=2))
