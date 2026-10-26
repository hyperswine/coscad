from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
import subprocess
B=Path(__file__).resolve().parent
def render(name):
 with (B/(name+'-render.log')).open('w') as log:
  subprocess.run(['/Applications/OpenSCAD.app/Contents/MacOS/openscad','--backend=Manifold','-o',str(B/(name+'.stl')),str(B/(name+'.scad'))],stdout=log,stderr=log,check=True)
 print('Rendered',name,flush=True)
with ThreadPoolExecutor(max_workers=3) as pool:list(pool.map(render,['monohull','monohull-envelope','monohull-opening','outrigger','outrigger-envelope','outrigger-opening']))
