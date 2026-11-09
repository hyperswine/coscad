#!/usr/bin/env python3
"""Generate a 150 mm printed-2020 racking specimen from the tested tank profiles.

No printer access. Separate assembly geometry and upright print poses.
"""
from pathlib import Path
import hashlib
import json
import re
import subprocess

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
TANK = ROOT / 'models/tank-model-2020'
COSCAD = ROOT / 'bin/coscad'
CONFIGS = {'bare': (), 'back': ('wall_back',), 'back-side': ('wall_back', 'wall_right')}


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def prepare():
    parts = HERE / 'parts'
    parts.mkdir(exist_ok=True)
    for length in (110, 150):
        source = re.sub(r'^len = .*$', f'len = {length}', (TANK / 'rail100.coscad').read_text(), flags=re.M)
        source = source.replace('2020 rail, 100 mm long', f'2020 rail, {length} mm long')
        (parts / f'rail{length}.coscad').write_text(source)
        print_source = source.replace('main =', 'profile =')
        print_source += '''
// Upright rail; four removable first-layer ears outside the slot mouths.
ears = (zcyl 4 0.2 |> move -9.5 -9.5 0.1)
  |> add (zcyl 4 0.2 |> move -9.5 9.5 0.1)
  |> add (zcyl 4 0.2 |> move 9.5 -9.5 0.1)
  |> add (zcyl 4 0.2 |> move 9.5 9.5 0.1)
main = (profile |> z (len / 2)) |> add ears
'''
        (parts / f'rail-print-{length}.coscad').write_text(print_source)
    (parts / 'bracket.coscad').write_bytes((TANK / 'bracket.coscad').read_bytes())
    panel = (TANK / 'panel-full-v3/panel-wall-double-c025-upright.coscad').read_text().split('upright =', 1)[0]
    panel = panel.replace('70 x 70', '120 x 120').replace('box 70 70', 'box 120 120')
    panel = panel.replace('key_length = 59.4', 'key_length = 109.4')
    panel = panel.replace('move 32.7 ', 'move 57.7 ').replace('move 34.25 ', 'move 59.25 ')
    (parts / 'panel.coscad').write_text(panel + '\nmain = panel\n')
    (parts / 'panel-print.coscad').write_text(panel + '''
// Same support-free insertion-axis print pose as the successful 70 mm wall.
ears = (zcyl 4 0.2 |> move -42 -4 0.1)
  |> add (zcyl 4 0.2 |> move -42 4 0.1)
  |> add (zcyl 4 0.2 |> move 42 -4 0.1)
  |> add (zcyl 4 0.2 |> move 42 4 0.1)
main = (panel |> rotx 90 |> z 60) |> add ears
''')
    original = (TANK / 'tank.assemble').read_text()
    original = original[original.index('!glyph'):]
    original = original.replace('rail100.coscad', 'parts/rail150.coscad').replace('rail60.coscad', 'parts/rail110.coscad')
    original = original.replace('bracket.coscad', 'parts/bracket.coscad').replace('panel_wall.coscad', 'parts/panel.coscad')
    # The far fastener station is length-10, unlike spatial midpoints.
    original = original.replace('at=50', 'at=100').replace('at=70', 'at=120')
    mapping = {'102': '152', '90': '140', '80': '130', '70': '120', '50': '75'}
    # Resize positions, never the 90-degree rotations.
    original = re.sub(r'\bmove (-?\d+) (-?\d+) (-?\d+)',
                      lambda m: 'move ' + ' '.join(mapping.get(v, v) for v in m.groups()), original)
    for name, included in CONFIGS.items():
        lines = ['// 150 mm rail envelope; identical 28-part frame and 32 M5x8 joints in all configurations.']
        for line in original.splitlines():
            if re.match(r'(wall_front|wall_left|floor)\s*←', line):
                continue
            if any(re.match(rf'{part}\s*←', line) for part in ('wall_back', 'wall_right') if part not in included):
                continue
            if re.match(r'\s*⊕ \((wall_front|wall_left|floor)\b', line):
                continue
            if any(re.match(rf'\s*⊕ \({part}\b', line) for part in ('wall_back', 'wall_right') if part not in included):
                continue
            if line.startswith('joint '):
                if not any(line.startswith(f'joint drop {part} ') for part in included):
                    continue
            if 'after=' in line:
                after = re.search(r'after=(\w+)', line)[1]
                if after not in included:
                    line = re.sub(r' after=\w+', '', line)
            if line.startswith('//'):
                continue  # replace tank-specific prose with this specimen's README
            lines.append(line)
        lines += ['', 'plan beam=10', '']
        (HERE / f'{name}.assemble').write_text('\n'.join(lines))
    inventory = [('rail150', 'rail-print-150', 4), ('rail110', 'rail-print-110', 8),
                 ('bracket', 'bracket', 16), ('panel', 'panel-print', 2)]
    lines = ['// Complete two-panel kit; print poses include removable mouse ears.', 'plate 256 256 6 250']
    poses, index = [], 0
    for label, source, count in inventory:
        lines.append(f'{label} ← parts/{source}.coscad ×{count} ▽bot material=pla brim=0')
        for _ in range(count):
            poses.append(f'({label} |> x {index * 300})')
            index += 1
    lines += ['asm = ' + ' ⊕ '.join(poses), '']
    (HERE / 'manufacture.assemble').write_text('\n'.join(lines))
    provenance = {'frame_outside_mm': [150, 150, 150], 'with_brackets_bounds_mm': [[-4, -4, 0], [154, 154, 150]],
                  'panel_size_mm': [120, 120], 'clearance_per_face_mm': .25,
                  'bom': {'rail150': 4, 'rail110': 8, 'bracket': 16, 'panel': 2, 'M5x8': 32, 'slot6_M5_T_nut': 32},
                  'parent_sources': {str(p.relative_to(ROOT)): digest(p) for p in
                                     (TANK / 'rail100.coscad', TANK / 'bracket.coscad',
                                      TANK / 'panel-full-v3/panel-wall-double-c025-upright.coscad', TANK / 'tank.assemble')},
                  'physical_tested': False, 'sliced': False, 'dispatched': False}
    (HERE / 'model.json').write_text(json.dumps(provenance, indent=2) + '\n')


if __name__ == '__main__':
    prepare()
    for source in sorted((HERE / 'parts').glob('*.coscad')):
        subprocess.run([str(COSCAD), 'stl', str(source), '-o', str(source.with_suffix('.stl'))], check=True)
