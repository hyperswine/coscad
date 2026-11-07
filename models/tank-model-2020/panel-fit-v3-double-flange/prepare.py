#!/usr/bin/env python3
"""Generate bilateral T-slot panel samples; no printer access.

Same 0.20/0.25/0.30 mm per-face clearances, with one/two/three diamond holes.
Print upright along the sliding direction, using integral handle-side mouse ears.
"""
import argparse
import importlib.util
from pathlib import Path
import re
import shutil
import subprocess

HERE = Path(__file__).resolve().parent
BASE = HERE.parent
spec = importlib.util.spec_from_file_location('one_sided_samples', BASE / 'panel-fit-v2/prepare.py')
previous = importlib.util.module_from_spec(spec)
spec.loader.exec_module(previous)


def coupon_source(clearance, holes):
    source = previous.wall_source(clearance).replace('main =', 'panel =')
    for binding, value in [('key_length', 26), ('lead', 3)]:
        source, count = re.subn(rf'^{binding} = .*$', f'{binding} = {value}', source, flags=re.M)
        if count != 1:
            raise ValueError(f'Expected exactly one {binding} binding')
    source = ('// V3: the one-sided panel below is only an intermediate.\n'
              '// The final sample reflects its flanges, crops one edge and prints upright.\n' + source)
    source += '''
// Complete stepped T-head: matching flanges on BOTH faces of the neck.
// 2 mm plain noses, then 3 mm ramps to the full profile at either end.
bilateral = panel |> add (⇋ 0 0 1 panel)
coupon = bilateral |> isect (box 9 30 16 |> move 31.5 0 0) |> x -31.5
upright = coupon |> rotx 90 |> z 15
// Removable first-layer ears stay 0.5 mm outside the rail face (X=-1.5).
ears = (zcyl 3 0.2 |> move -5 (neck / 2) 0.1)
  |> add (zcyl 3 0.2 |> move -5 (-neck / 2) 0.1)
main = upright |> add ears
'''
    for i in range(holes):
        z = 15 + (i - (holes - 1) / 2) * 4
        source += f'  |> cut (box 1 16 1 |> roty 45 |> move -3 0 {z:g})\n'
    return source


def prepare(output, coscad=None):
    output.mkdir(parents=True, exist_ok=True)
    for holes, clearance in enumerate((.20, .25, .30), 1):
        source = output / f'double-c{round(clearance * 100):03d}-{holes}holes.coscad'
        source.write_text(coupon_source(clearance, holes))
        if coscad:
            subprocess.run([coscad, 'stl', str(source), '-o', str(source.with_suffix('.stl'))], check=True)
    # Unchanged slot profile, shortened only. Optional: existing tank rails are preferred.
    rail = (BASE / 'rail100.coscad').read_text()
    rail = re.sub(r'^len = .*$', 'len = 35', rail, flags=re.M)
    rail = rail.replace('main =', 'profile =') + '\nmain = profile |> z (len / 2)\n'
    source = output / 'rail35.coscad'
    source.write_text(rail)
    if coscad:
        subprocess.run([coscad, 'stl', str(source), '-o', str(source.with_suffix('.stl'))], check=True)


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
