"""Fit, geometry and aerodynamic checks for the bolted glider. Writes validation.json and
preview.png. Masses and the balance point come later, from the slicer (balance.py).

Fit (all parts in flight position, assembly.py):
  - no part overlaps another (boolean intersection volume);
  - each wing panel and the stab are seated: lowered 0.05 mm they would overlap their mount;
  - every screw path is clear: a 4.6 mm rod down each screw axis, from the head seat to
    the nut, meets no plastic; and the nut cavity is where the rod ends.
Aerodynamics (simple, standard estimates):
  - wing area S, span b, aspect ratio, mean aerodynamic chord and its quarter-chord point;
  - tail volumes Vh = St lt / (S c) and Vv = Sv lv / (S b);
  - neutral point h_n = 0.25 + eta Vh (a_t / a_w)(1 - de/da), lift slopes from Helmbold's
    formula and downwash de/da = 2 a_w / (pi AR), eta = 0.9.
"""
from pathlib import Path
import json, math
import numpy as np, trimesh, manifold3d as mf
import os; os.environ.setdefault('MPLCONFIGDIR', '/private/tmp/glider-matplotlib')
import matplotlib; matplotlib.use('Agg'); import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
from assembly import B, d, W, T, load, PARTS, PLACE, WING_TO_USE, R, Tr

def man(m): return mf.Manifold(mf.Mesh(vert_properties=np.asarray(m.vertices, np.float32), tri_verts=np.asarray(m.faces, np.uint32)))
def vol(a, b): return (man(a) ^ man(b)).volume()

meshes = {p: load(p) for p in PARTS}
for p, m in meshes.items():
    assert m.is_watertight and m.is_volume, p
report = {'parts': {p: {'volume_mm3': float(m.volume), 'bounds': m.bounds.round(2).tolist()} for p, m in meshes.items()}}

# ---- fit -------------------------------------------------------------------------
pairs = [(a, b) for i, a in enumerate(PARTS) for b in PARTS[i + 1:]]
overlaps = {f'{a}/{b}': vol(meshes[a], meshes[b]) for a, b in pairs}
for k, v in overlaps.items(): assert v < 1.0, ('overlap', k, v)
def seated(part, mount, normal):
    m = meshes[part].copy(); m.apply_translation(-0.05*np.asarray(normal)); return vol(m, meshes[mount]) > 0.01
nr = (WING_TO_USE[:3, :3] @ np.array([0, 0, 1.0])); nl = nr*np.array([-1, 1, 1])
seats = {'wing-right on saddle': seated('wing-right', 'fuselage', nr), 'wing-left on saddle': seated('wing-left', 'fuselage', nl),
         'stab on platform': seated('stab', 'fuselage', [0, 0, 1]), 'fin on stab': seated('fin', 'stab', [0, 0, 1])}
assert all(seats.values()), seats

def rod(p0, p1, r=2.3):
    p0, p1 = np.asarray(p0, float), np.asarray(p1, float); v = p1 - p0; L = np.linalg.norm(v)
    c = trimesh.creation.cylinder(radius=r, height=L, sections=24)
    c.apply_transform(trimesh.geometry.align_vectors([0, 0, 1], v/L)); c.apply_translation((p0 + p1)/2); return c
screws = []
for side, part in [(1, 'wing-right'), (-1, 'wing-left')]:
    for u in W['screw_u']:
        x, y = W['root_gap'] + W['screw_station'], u*W['root_chord']
        top = (WING_TO_USE @ [x, y, W['counterbore_floor'], 1])[:3]; nut = (WING_TO_USE @ [x, y, -3.0, 1])[:3]
        top[0] *= side; nut[0] *= side
        r_ = rod(top - 0.02*(top - nut)/np.linalg.norm(top - nut), nut)
        screws.append({'where': f'{part} u={u}', 'length_head_to_nut_top_mm': float(np.linalg.norm(top - nut)),
                       'plastic_in_path_mm3': vol(r_, meshes[part]) + vol(r_, meshes['fuselage'])})
ys_mid = T['screw_y_ref']
for x, dy in T['screws']:
    z_top = T['platform_top_z'] + T['stab_block_top'] + T['fin']['foot'][2]
    z_nut = T['platform_top_z'] - T['platform_thickness']/2 + 2.2
    r_ = rod([x, ys_mid + dy, z_top - 0.02], [x, ys_mid + dy, z_nut])
    screws.append({'where': f'tail x={x}', 'length_head_to_nut_top_mm': z_top - z_nut,
                   'plastic_in_path_mm3': sum(vol(r_, meshes[p]) for p in ('fin', 'stab', 'fuselage'))})
for s in screws: assert s['plastic_in_path_mm3'] < 0.5, s
report['fit'] = {'overlaps_mm3': overlaps, 'seated': seats, 'screws': screws,
                 'screw_choice': {'wing': 'M5 x 12: head seat to nut top plus a 4 mm nut, rounded up',
                                  'tail': 'M5 x 16'}}

# ---- aerodynamics ------------------------------------------------------------------
LP, C0 = W['panel_length'], W['root_chord']
st, ch = W['stations'], W['chords']
def trap(xs, ys): return sum((xs[i + 1] - xs[i])*(ys[i] + ys[i + 1])/2 for i in range(len(xs) - 1))
cosd = math.cos(math.radians(W['dihedral_deg']))
S_panel = trap(st, ch)                                   # true panel area
S = 2*S_panel*cosd + 2*W['root_gap']*C0                  # projected, with the gap
b = 2*(W['root_gap'] + LP*cosd)
mac = 2*trap(st, [c*c for c in ch])/(2*S_panel)
AR = b*b/S
ac_y = 0.25*C0                                           # the quarter-chord line is straight
def helmbold(A): return 2*math.pi*A/(2 + math.sqrt(4 + A*A))
a_w = helmbold(AR)
hs = T['stab_span']/2
st_s = [0, hs - 16, hs - 10, hs - 4, hs]
def c_stab(z):
    r = abs(z); lin = T['stab_root_chord'] + (T['stab_tip_chord'] - T['stab_root_chord'])*r/hs; q = r - (hs - 16)
    return lin*(max(0.3, math.sqrt(max(0.0, 1 - (q/16)**2))) if q > 0 else 1.0)
St = 2*trap(st_s, [c_stab(z) for z in st_s]); bt = T['stab_span']; ARt = bt*bt/St; a_t = helmbold(ARt)
stab_ac = T['stab_le_y'] + 0.25*T['stab_root_chord']
lt = stab_ac - ac_y
F = T['fin']; Sv = (F['root_chord'] + F['tip_chord'])/2*F['height']
fin_ac = T['stab_le_y'] + 3 + F['blade_y0_in_foot'] + (F['sweep'] + 0.25*(F['root_chord'] + F['tip_chord']))/2
lv = fin_ac - ac_y
Vh = St*lt/(S*mac); Vv = Sv*lv/(S*b)
eta, dedа = 0.9, 2*a_w/(math.pi*AR)
h_n = 0.25 + eta*Vh*(a_t/a_w)*(1 - dedа)
np_y = (h_n - 0.25)*mac + ac_y
report['aero'] = {'wing_area_mm2': S, 'span_mm': b, 'aspect_ratio': AR, 'mac_mm': mac, 'wing_ac_y': ac_y,
                  'stab_area_mm2': St, 'stab_aspect_ratio': ARt, 'tail_arm_mm': lt, 'Vh': Vh,
                  'fin_area_mm2': Sv, 'fin_arm_mm': lv, 'Vv': Vv, 'lift_slope_wing_per_rad': a_w, 'lift_slope_stab_per_rad': a_t,
                  'downwash_gradient': dedа, 'neutral_point_fraction_mac': h_n, 'neutral_point_y': np_y}
(B/'validation.json').write_text(json.dumps(report, indent=2, default=float) + '\n')

# ---- preview -------------------------------------------------------------------------
fig = plt.figure(figsize=(16, 6.5))
cols = {'wing-right': '#3a7bd5', 'wing-left': '#3a7bd5', 'fuselage': '#e9a23b', 'stab': '#5cb85c', 'fin': '#d9534f'}
for i, (elev, azim, title) in enumerate([(22, -130, 'assembled'), (90, -90, 'from above (nose down the page)'), (0, 0, "from the right side")]):
    ax = fig.add_subplot(1, 3, i + 1, projection='3d')
    for p_, m in meshes.items():
        if i == 2 and p_ == 'wing-right': continue
        ax.add_collection3d(Poly3DCollection(m.triangles, facecolors=cols[p_], edgecolors=(0, 0, 0, .05), linewidth=.1, shade=True))
    ax.set_xlim(-270, 270); ax.set_ylim(-150, 390); ax.set_zlim(-60, 210); ax.set_box_aspect((540, 540, 270))
    ax.view_init(elev=elev, azim=azim); ax.set_axis_off(); ax.set_title(title.replace(' (nose to the right... left)', ''))
fig.suptitle(f"Bolted glider: span {b:.0f} mm, length {meshes['fuselage'].bounds[1,1] - meshes['fuselage'].bounds[0,1]:.0f} mm fuselage, "
             f"wing {S/1e4:.2f} dm2, AR {AR:.1f}, Vh {Vh:.2f}, Vv {Vv:.3f}")
fig.tight_layout(); fig.savefig(B/'preview.png', dpi=120)
print(json.dumps({'overlaps': {k: round(v, 3) for k, v in overlaps.items() if v > 1e-3}, 'seated': seats,
                  'screws': [(s['where'], round(s['length_head_to_nut_top_mm'], 1), round(s['plastic_in_path_mm3'], 2)) for s in screws],
                  'aero': {k: round(v, 3) for k, v in report['aero'].items()}}, indent=1))
