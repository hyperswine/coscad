"""Fit, geometry and aerodynamic checks for the bolted glider. Writes validation.json and
preview.png. Masses and the balance point come later, from the slicer (balance.py).

Fit (all parts in flight position, assembly.py):
  - no part overlaps another (boolean intersection volume);
  - each wing panel and the stab are seated: lowered 0.05 mm they would overlap their mount;
  - every screw path is clear: a nominal-diameter screw down each screw axis, from the head seat to
    the nut, meets no plastic; and the nut cavity is where the rod ends.
Aerodynamics (simple, standard estimates):
  - wing area S, span b, aspect ratio, mean aerodynamic chord and its quarter-chord point;
  - tail volumes Vh = St lt / (S c) and Vv = Sv lv / (S b);
  - neutral point is the lift-slope-weighted wing/tail aerodynamic centre;
    wing lift slope uses a swept Helmbold approximation, with eta=0.9.
    These are preliminary linear estimates, not measured aerodynamic coefficients.
"""
from pathlib import Path
import json, math
import numpy as np, trimesh, manifold3d as mf
import os; os.environ.setdefault('MPLCONFIGDIR', '/private/tmp/glider-matplotlib')
import matplotlib; matplotlib.use('Agg'); import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
from assembly import B, d, W, T, load, PARTS, PLACE, WING_TO_USE, TAIL_TO_USE, R, Tr

def man(m): return mf.Manifold(mf.Mesh(vert_properties=np.asarray(m.vertices, np.float32), tri_verts=np.asarray(m.faces, np.uint32)))
def vol(a, b): return (man(a) ^ man(b)).volume()

from provenance import require_geometry, snapshot
require_geometry(B)

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
         'stab on platform': seated('stab', 'fuselage', TAIL_TO_USE[:3,:3]@[0,0,1]), 'fin on stab': seated('fin', 'stab', TAIL_TO_USE[:3,:3]@[0,0,1])}
assert all(seats.values()), seats

def rod(p0, p1, r=2.5):
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
    z_nut = T['platform_top_z'] - T['platform_thickness']/2 + 1.4
    r_ = rod([x, ys_mid + dy, z_top - 0.02], [x, ys_mid + dy, z_nut], r=1.5)
    r_.apply_transform(TAIL_TO_USE)
    screws.append({'where': f'tail x={x}', 'length_head_to_nut_top_mm': z_top - z_nut,
                   'plastic_in_path_mm3': sum(vol(r_, meshes[p]) for p in ('fin', 'stab', 'fuselage'))})
for s in screws: assert s['plastic_in_path_mm3'] < 0.5, s
# Full fastener envelopes, including head clearance and protruding thread ends.
# Standard nominal steel button heads; supplier dimensions still need checking.
def cylinder_between_z(radius, z0, z1, x, y, sections=48):
    m=trimesh.creation.cylinder(radius=radius,height=z1-z0,sections=sections)
    m.apply_translation([x,y,(z0+z1)/2]); return m
hardware_checks={}
for side,part in ((1,'wing-right'),(-1,'wing-left')):
    for index,u in enumerate(W['screw_u']):
        x,y=W['root_gap']+W['screw_station'],u*W['root_chord']
        for kind,r,z0,z1,segments in [('shank',2.5,-8,4,48),('head',4.75,4,6.75,48),('nut',8/math.sqrt(3),-7,-3,6)]:
            m=cylinder_between_z(r,z0,z1,x,y,segments);m.apply_transform(WING_TO_USE)
            if side<0: m.apply_transform(np.diag([-1.,1.,1.,1.]))
            value=sum(vol(m,meshes[p]) for p in PARTS)
            hardware_checks[f'{part}-{index}-{kind}']=value
for index,(x,dy) in enumerate(T['screws']):
    y=T['screw_y_ref']+dy; z=T['platform_top_z']+T['stab_block_top']+T['fin']['foot'][2]
    zn=T['platform_top_z']-T['platform_thickness']/2
    for kind,r,z0,z1,segments in [('shank',1.5,z-10,z,48),('head',2.85,z,z+1.65,48),('nut',5.5/math.sqrt(3),zn-1.2,zn+1.2,6)]:
        m=cylinder_between_z(r,z0,z1,x,y,segments); m.apply_transform(TAIL_TO_USE)
        hardware_checks[f'tail-{index}-{kind}']=sum(vol(m,meshes[p]) for p in PARTS)
assert max(hardware_checks.values())<0.5, hardware_checks
report['hardware_interference_mm3']=hardware_checks
report['fit'] = {'overlaps_mm3': overlaps, 'seated': seats, 'screws': screws,
                 'screw_choice': {'wing': 'M5 x 12: head seat to nut top plus a 4 mm nut, rounded up',
                                  'tail': 'M3 x 10'}}

# ---- aerodynamics ------------------------------------------------------------------
LP, C0 = W['panel_length'], W['root_chord']
st, ch = W['stations'], W['chords']
def trap(xs, ys): return sum((xs[i + 1] - xs[i])*(ys[i] + ys[i + 1])/2 for i in range(len(xs) - 1))
cosd = math.cos(math.radians(W['dihedral_deg']))
sample_s = np.linspace(st[0], st[-1], 4001)
sample_c = np.interp(sample_s, st, ch)
sample_sweep = np.interp(sample_s, st, W['sweep_offsets'])
S_panel = trap(sample_s, sample_c)                                   # true panel area
S = 2*S_panel*cosd                  # projected, with the gap
b = 2*(W['root_gap'] + LP*cosd)
mac = trap(sample_s, sample_c**2)/S_panel
AR = b*b/S
ac_y = 0.25*C0 + trap(sample_s, sample_c*sample_sweep)/S_panel #                                            # the quarter-chord line is straight
def helmbold(A): return 2*math.pi*A/(2 + math.sqrt(4 + A*A))
sweep_angle = math.atan2(W['sweep_tip_mm'], (LP - st[1])*cosd)
a_w = 2*math.pi*AR/(2 + math.sqrt(4 + (AR/math.cos(sweep_angle))**2))
hs = T['stab_span']/2
st_s = [0, hs - 16, hs - 10, hs - 4, hs]
def c_stab(z):
    r = abs(z); lin = T['stab_root_chord'] + (T['stab_tip_chord'] - T['stab_root_chord'])*r/hs; q = r - (hs - 16)
    return lin*(max(0.3, math.sqrt(max(0.0, 1 - (q/16)**2))) if q > 0 else 1.0)
St = 2*trap(st_s, [c_stab(z) for z in st_s]); bt = T['stab_span']; ARt = bt*bt/St; a_t = helmbold(ARt)
stab_ac = float((TAIL_TO_USE@[0,T['stab_le_y'] + 0.25*T['stab_root_chord'],T['platform_top_z'],1])[1])
lt = stab_ac - ac_y
F = T['fin']; Sv = (F['root_chord'] + F['tip_chord'])/2*F['height']
fin_ac = T['stab_le_y'] + 3 + F['blade_y0_in_foot'] + (F['sweep'] + 0.25*(F['root_chord'] + F['tip_chord']))/2
lv = fin_ac - ac_y
Vh = St*lt/(S*mac); Vv = Sv*lv/(S*b)
eta, dedalpha = 0.9, 2*a_w/(math.pi*AR)
h_n = 0.25 + eta*Vh*(a_t/a_w)*(1 - dedalpha)
r_tail = eta*(St/S)*(a_t/a_w)*(1 - dedalpha)
np_y = (ac_y + r_tail*stab_ac)/(1 + r_tail)
h_n = 0.25 + (np_y-ac_y)/mac
report['aero'] = {'wing_area_mm2': S, 'span_mm': b, 'aspect_ratio': AR, 'mac_mm': mac, 'wing_ac_y': ac_y,
                  'stab_area_mm2': St, 'stab_aspect_ratio': ARt, 'tail_arm_mm': lt, 'Vh': Vh,
                  'fin_area_mm2': Sv, 'fin_arm_mm': lv, 'Vv': Vv, 'lift_slope_wing_per_rad': a_w, 'lift_slope_stab_per_rad': a_t,
                  'quarter_chord_sweep_deg': math.degrees(sweep_angle), 'tail_efficiency': eta, 'downwash_gradient': dedalpha, 'neutral_point_fraction_mac': h_n, 'neutral_point_y': np_y}
report['inputs'] = snapshot(B, [B/'meshes.json', B/'assembly.py', B/'verify.py'])
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
