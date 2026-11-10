"""Place each printed part (print-frame STLs) in its flight position, as design.json
describes, and check how they fit. Shared by verify.py and balance.py.

  wing-right  print (x chord, y = -span, z up) -> wing frame by a quarter turn about z
              and the 2 mm root gap; then dihedral, incidence and up to the saddle.
  wing-left   the mirror image of wing-right in use.
  stab        print (x chord, y = -span) -> quarter turn, then to the tail platform.
  fin         print frame is its use orientation; moved onto the stab's centre block.
  fuselage    already in the use frame.
"""
from pathlib import Path
import json, math
import numpy as np, trimesh
B = Path(__file__).resolve().parent
d = json.loads((B/'design.json').read_text())
W, T = d['wing'], d['tail']

def R(axis, deg):
    return trimesh.transformations.rotation_matrix(math.radians(deg), {'x': [1, 0, 0], 'y': [0, 1, 0], 'z': [0, 0, 1]}[axis])
def Tr(x, y, z): return trimesh.transformations.translation_matrix([x, y, z])

WING_TO_USE = Tr(0, 0, d['saddle']['top_z']) @ R('x', -W['incidence_deg']) @ R('y', -W['dihedral_deg'])
PRINT_TO_WING = Tr(W['root_gap'], 0, 0) @ R('z', 90)
MIRROR_X = np.diag([-1.0, 1, 1, 1])
PLACE = {
    'wing-right': WING_TO_USE @ PRINT_TO_WING,
    'wing-left': MIRROR_X @ WING_TO_USE @ PRINT_TO_WING @ np.diag([1.0, -1, 1, 1]),   # left STL is the y-mirror of the right
    'stab': Tr(0, T['stab_le_y'], T['platform_top_z']) @ R('z', 90),
    'fin': Tr(0, T['stab_le_y'] + 3, T['platform_top_z'] + T['stab_block_top']),
    'fuselage': np.eye(4)}
TAIL_TO_USE = Tr(0,T['screw_y_ref'],T['platform_top_z']) @ R('x',-T['incidence_deg']) @ Tr(0,-T['screw_y_ref'],-T['platform_top_z'])
for part in ('stab','fin'): PLACE[part] = TAIL_TO_USE @ PLACE[part]
PARTS = list(PLACE)

def load(name, place=True):
    m = trimesh.load_mesh(B/f'{name}.stl')
    if place:
        m.apply_transform(PLACE[name])
        if np.linalg.det(PLACE[name][:3, :3]) < 0: m.invert()
    return m

def wing_frame_point(x, y, z, side=1):
    p = WING_TO_USE @ np.array([x, y, z, 1.0]); p[0] *= side; return p[:3]
