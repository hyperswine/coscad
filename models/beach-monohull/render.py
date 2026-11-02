"""Render the CoScad sources to STL with the native compiler (`coscad stl`).
The compiler is `--coscad PATH`, else $COSCAD, else `cabal list-bin exe:coscad`."""
from pathlib import Path
import argparse, os, subprocess
B = Path(__file__).resolve().parent
NAMES = ['beach-monohull', 'beach-monohull-envelope', 'beach-monohull-sealed']
ap = argparse.ArgumentParser(description=__doc__); ap.add_argument('--coscad'); args = ap.parse_args()
coscad = args.coscad or os.environ.get('COSCAD') or subprocess.check_output(['cabal', 'list-bin', 'exe:coscad'], cwd=B.parents[1], text=True).strip()
for name in NAMES:
    with (B/(name+'-render.log')).open('w') as log:
        subprocess.run([coscad, 'stl', str(B/(name+'.coscad')), '-o', str(B/(name+'.stl'))], stdout=log, stderr=log, check=True)
    print('Rendered', name, flush=True)
