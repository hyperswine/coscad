"""Generate and mesh all parts, then validate geometry. Does not slice or dispatch."""
from pathlib import Path
import argparse, json, subprocess, sys
from provenance import snapshot
B=Path(__file__).resolve().parent
p=argparse.ArgumentParser(); p.add_argument('--coscad',default='coscad'); p.add_argument('--tail-incidence',type=float,default=-5.5); a=p.parse_args()
subprocess.run([sys.executable,str(B/'design.py'),'--tail-incidence',str(a.tail_incidence)],check=True)
paths=[B/'design.py',B/'design.json']
for source in sorted(B.glob('*.coscad')):
    mesh=source.with_suffix('.stl')
    subprocess.run([a.coscad,'stl',str(source),'-o',str(mesh)],check=True,capture_output=True)
    paths.extend([source,mesh])
(B/'meshes.json').write_text(json.dumps(snapshot(B,paths),indent=2)+'\n')
subprocess.run([sys.executable,str(B/'verify.py')],check=True)
