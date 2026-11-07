#!/usr/bin/env python3
"""Generate small sliding-fit samples from the actual wall and rail sources.

No printer access. One/two/three holes identify 0.20/0.25/0.30 mm per-face
clearance. Test against an existing printed rail; the short rail is optional.
"""
from pathlib import Path
import argparse
import re
import shutil
import subprocess

HERE = Path(__file__).resolve().parent
BASE = HERE.parent


def wall_source(clearance):
    source = (BASE / 'panel_wall.coscad').read_text()
    source, count = re.subn(r'^clearance = .*$', f'clearance = {clearance}', source, flags=re.M)
    if count != 1:
        raise ValueError('Expected exactly one wall clearance binding')
    return source


def prepare(output, coscad):
    output.mkdir(parents=True, exist_ok=True)
    for holes, clearance in enumerate((0.20, 0.25, 0.30), 1):
        source = wall_source(clearance).replace('main =', 'panel =')
        source += '\n// Crop the actual keyed edge; flat print face is at Z=0.\n'
        source += 'main = panel |> isect (box 9 30 16 |> move 31.5 0 0) |> move -31.5 0 (neck / 2)\n'
        for i in range(holes):
            y = (i - (holes - 1) / 2) * 4
            source += f'  |> cut (zcyl 0.7 16 |> move -3.5 {y:g} 4)\n'
        path = output / f'panel-c{round(clearance * 100):03d}-{holes}holes.coscad'
        path.write_text(source)
        if coscad:
            subprocess.run([coscad, 'stl', str(path), '-o', str(path.with_suffix('.stl'))], check=True)
    rail = (BASE / 'rail100.coscad').read_text()
    rail, count = re.subn(r'^len = .*$', 'len = 35', rail, flags=re.M)
    if count != 1:
        raise ValueError('Expected exactly one rail length binding')
    rail = rail.replace('main =', 'profile =') + '\nmain = profile |> z (len / 2)\n'
    path = output / 'rail35.coscad'
    path.write_text(rail)
    if coscad:
        subprocess.run([coscad, 'stl', str(path), '-o', str(path.with_suffix('.stl'))], check=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, default=HERE)
    parser.add_argument('--render', action='store_true')
    parser.add_argument('--coscad', default='coscad')
    args = parser.parse_args()
    executable = shutil.which(args.coscad) if args.render else None
    if args.render and executable is None:
        parser.error(f'CoScad executable not found: {args.coscad}')
    prepare(args.output, executable)
