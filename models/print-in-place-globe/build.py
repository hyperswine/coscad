"""Rebuild, check, and slice locally. This script never sends a print."""
from pathlib import Path
import subprocess,sys,os
import trimesh
B=Path(__file__).resolve().parent
os.environ.setdefault('MPLCONFIGDIR','/private/tmp/globe-matplotlib')
def script(name):subprocess.run([sys.executable,str(B/name)],check=True)
script('generate.py')
for part in ['stand','rotor','bearing-blocker']:
 with (B/(part+'-render.log')).open('w') as log:
  subprocess.run(['/Applications/OpenSCAD.app/Contents/MacOS/openscad','--backend=Manifold','-o',str(B/(part+'.stl')),str(B/(part+'.scad'))],check=True,stdout=log,stderr=log)
# Preserve disconnected bodies without a redundant combined CSG re-tessellation.
trimesh.util.concatenate([trimesh.load_mesh(B/(x+'.stl')) for x in ['stand','rotor']]).export(B/'globe.stl')
script('verify.py');script('prepare_slice.py')
(B/'slice').mkdir(exist_ok=True)
with (B/'slice/slicer.log').open('w') as log:
 subprocess.run(['/Applications/BambuStudio.app/Contents/MacOS/BambuStudio','--debug','2','--load-settings',str(B/'presets/machine.json')+';'+str(B/'presets/process.json'),'--load-filaments',str(B/'presets/filament.json'),'--arrange','0','--orient','0','--slice','1','--export-3mf','globe-a1.gcode.3mf','--outputdir',str(B/'slice'),str(B/'globe-project.3mf')],check=True,stdout=log,stderr=log)
script('verify_toolpaths.py');script('finalize_package.py')
