"""Emit a pure CoScad N by M egg box; dimensions in mm."""
import argparse
import json
from pathlib import Path
p = argparse.ArgumentParser()
p.add_argument('--columns', type=int, default=2)
p.add_argument('--rows', type=int, default=2)
a = p.parse_args()
if not (1 <= a.columns <= 20 and 1 <= a.rows <= 20):
    p.error('rows and columns must be between 1 and 20')
base = Path(__file__).resolve().parent
text = f'''// Open egg box: {a.columns} columns by {a.rows} rows.
// Regenerate grid using generate.py; geometry is entirely CoScad.
columns = {a.columns}
rows = {a.rows}
cell = 60
wall = 2.4
floor = 3
clear_depth = 75
height = floor + clear_depth
pitch = cell + wall
width = columns * cell + (columns + 1) * wall
depth = rows * cell + (rows + 1) * wall
outer_radius = 4
inner_radius = 6
outer_a = χ outer_radius (ψ outer_radius (◎ outer_radius height))
outer_b = χ (width - outer_radius) (ψ outer_radius (◎ outer_radius height))
outer_c = χ outer_radius (ψ (depth - outer_radius) (◎ outer_radius height))
outer_d = χ (width - outer_radius) (ψ (depth - outer_radius) (◎ outer_radius height))
body = outer_a ⇓ outer_b ⇓ outer_c ⇓ outer_d
hole_a = χ inner_radius (ψ inner_radius (◎ inner_radius (clear_depth + 1)))
hole_b = χ (cell - inner_radius) (ψ inner_radius (◎ inner_radius (clear_depth + 1)))
hole_c = χ inner_radius (ψ (cell - inner_radius) (◎ inner_radius (clear_depth + 1)))
hole_d = χ (cell - inner_radius) (ψ (cell - inner_radius) (◎ inner_radius (clear_depth + 1)))
hole = hole_a ⇓ hole_b ⇓ hole_c ⇓ hole_d
'''
names=[]
for y in range(a.rows):
    for x in range(a.columns):
        name=f'cell_{x}_{y}';names.append(name)
        text += f'{name} = χ (wall + {x} * pitch) (ψ (wall + {y} * pitch) (ζ floor hole))\n'
text += 'main = body ' + ' '.join('⊖ '+name for name in names) + '\n'
(base/'egg-box.coscad').write_text(text)
(base/'egg-box_manifest.json').write_text(json.dumps({'bed':{'w':256,'d':256},'variants':[{'hints':{'material':'pla'}}],'beds':[{'index':1,'stl':'egg-box.stl','placements':[{'instance':'egg-box'}]}]},indent=2)+'\n')
print(f'Generated {a.columns} x {a.rows} egg box')
