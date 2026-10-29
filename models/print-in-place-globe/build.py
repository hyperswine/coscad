"""Rebuild, check, and slice locally. This script never sends a print.

Meshes render with the native compiler (`coscad stl`); no OpenSCAD is needed.
The compiler is `--coscad PATH`, else $COSCAD, else `cabal list-bin exe:coscad`.
The native kernel tessellates circles with 50 segments; the recorded meshes were
rendered by OpenSCAD at $fn=96 (generate.py patches the .scad files), so a
rebuild is not byte- or facet-identical to them. `--render-only` stops after
the meshes and verify.py, before any slicing.
"""
from pathlib import Path
import argparse,subprocess,sys,os,tempfile
import trimesh
B=Path(__file__).resolve().parent
os.environ.setdefault('MPLCONFIGDIR','/private/tmp/globe-matplotlib')
PARTS={'stand':'stand','rotor':'rotor','bearing-blocker':'bearing_blocker'}
def script(name):subprocess.run([sys.executable,str(B/name)],check=True)
def compiler(arg):
 if arg or os.environ.get('COSCAD'):return arg or os.environ['COSCAD']
 return subprocess.check_output(['cabal','list-bin','exe:coscad'],cwd=B.parents[1],text=True).strip()
ap=argparse.ArgumentParser(description=__doc__)
ap.add_argument('--coscad');ap.add_argument('--render-only',action='store_true')
args=ap.parse_args();coscad=compiler(args.coscad)
script('generate.py')
# generate.py keeps only globe.coscad; each part is the same source with another main.
source=(B/'globe.coscad').read_text().rsplit('main =',1)[0]
with tempfile.TemporaryDirectory(prefix='globe-parts-') as tmp:
 for part,main in PARTS.items():
  src=Path(tmp)/(part+'.coscad');src.write_text(source+'main = '+main+'\n')
  with (B/(part+'-render.log')).open('w') as log:
   subprocess.run([coscad,'stl',str(src),'-o',str(Path(tmp)/(part+'.stl'))],check=True,stdout=log,stderr=log)
  (B/(part+'.stl')).write_bytes((Path(tmp)/(part+'.stl')).read_bytes())
# Preserve disconnected bodies without a redundant combined CSG re-tessellation.
trimesh.util.concatenate([trimesh.load_mesh(B/(x+'.stl')) for x in ['stand','rotor']]).export(B/'globe.stl')
script('verify.py')
if args.render_only:sys.exit(0)
script('prepare_slice.py')
(B/'slice').mkdir(exist_ok=True)
with (B/'slice/slicer.log').open('w') as log:
 subprocess.run(['/Applications/BambuStudio.app/Contents/MacOS/BambuStudio','--debug','2','--load-settings',str(B/'presets/machine.json')+';'+str(B/'presets/process.json'),'--load-filaments',str(B/'presets/filament.json'),'--arrange','0','--orient','0','--slice','1','--export-3mf','globe-a1.gcode.3mf','--outputdir',str(B/'slice'),str(B/'globe-project.3mf')],check=True,stdout=log,stderr=log)
script('verify_toolpaths.py');script('finalize_package.py')
