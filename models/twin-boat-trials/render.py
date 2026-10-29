"""Render the CoScad sources to STL with the native compiler (`coscad stl`).

No OpenSCAD is needed. The compiler is `--coscad PATH`, else $COSCAD, else
`cabal list-bin exe:coscad` (as design.py resolves it). Each NAME.coscad
renders to NAME.stl with its compiler report in NAME-render.log; `--out DIR`
writes both elsewhere (default this folder).
"""
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
import argparse, os, subprocess
B = Path(__file__).resolve().parent
NAMES = ['monohull', 'monohull-envelope', 'monohull-opening',
         'outrigger', 'outrigger-envelope', 'outrigger-opening']
WORKERS = 3


def compiler(arg):
    if arg or os.environ.get('COSCAD'):
        return arg or os.environ['COSCAD']
    return subprocess.check_output(['cabal', 'list-bin', 'exe:coscad'], cwd=B.parents[1], text=True).strip()


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--coscad')
    ap.add_argument('--out', type=Path, default=B)
    args = ap.parse_args()
    coscad = compiler(args.coscad)
    args.out.mkdir(parents=True, exist_ok=True)

    def render(name):
        with (args.out/(name+'-render.log')).open('w') as log:
            subprocess.run([coscad, 'stl', str(B/(name+'.coscad')), '-o', str(args.out/(name+'.stl'))],
                           stdout=log, stderr=log, check=True)
        print('Rendered', name, flush=True)

    with ThreadPoolExecutor(max_workers=WORKERS) as pool:
        list(pool.map(render, NAMES))


if __name__ == '__main__':
    main()
