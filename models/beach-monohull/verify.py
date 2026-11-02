"""Geometry, wall thickness and static hydrostatics for the beach monohull.
Not a physical leak test. Writes validation.json and preview-and-stability.png.

Three conditions:
  dry      the cockpit holds air until its rim reaches the water (downflooding);
  swamped  the cockpit is full of water, so only plastic and sealed tanks float it;
  holed    swamped, and the chamber whose loss hurts most is flooded too.
"""
from pathlib import Path
import json, math, os
os.environ.setdefault('MPLCONFIGDIR', '/private/tmp/boat-matplotlib')
import numpy as np, trimesh, manifold3d as md
import matplotlib; matplotlib.use('Agg')
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
B = Path(__file__).resolve().parent
NAME = 'beach-monohull'
DENSITY = {'PLA': 1.24}
WATER = {'fresh': 1.000, 'sea': 1.025}
design = json.loads((B/'design.json').read_text())
H = design['height_mm']

def manifold(m): return md.Manifold(md.Mesh(np.asarray(m.vertices, dtype=np.float32), np.asarray(m.faces, dtype=np.uint32)))
def mesh(m):
    d = m.to_mesh(); return trimesh.Trimesh(np.asarray(d.vert_properties)[:, :3], np.asarray(d.tri_verts), process=False)
def rot(axis, deg): return trimesh.transformations.rotation_matrix(math.radians(deg), axis)

body = trimesh.load_mesh(B/(NAME+'.stl'))
env = trimesh.load_mesh(B/(NAME+'-envelope.stl'))
sealed = trimesh.load_mesh(B/(NAME+'-sealed.stl'))
assert body.is_watertight and body.is_volume and env.is_watertight and sealed.is_watertight
shells = body.split(only_watertight=False)
positive = [s for s in shells if s.volume > 0]; voids = sorted([s for s in shells if s.volume < 0], key=lambda s: (round(s.center_mass[0]), s.center_mass[1]))
assert len(positive) == 1, len(positive)
def chamber_name(v):
    x, y = v.center_mass[:2]
    along = 'bow' if x > 50 else 'stern' if x < -50 else 'fore' if x > 12 else 'aft' if x < -12 else 'mid'
    side = '' if abs(y) < 1 else (' port' if y > 0 else ' starboard')
    return along + side
outside = md.Manifold.cube([600, 600, 300], True) - manifold(env)
chambers = []
for v in voids:
    air = trimesh.Trimesh(v.vertices, v.faces[:, ::-1], process=False)
    gap_out = float(manifold(air).min_gap(outside, 10.0))
    chambers.append({'name': chamber_name(v), 'air_cm3': float(air.volume/1000), 'center_mm': air.center_mass.tolist(),
                     'min_wall_to_outside_mm': gap_out, 'mesh': air})
cockpit_void = manifold(env) - manifold(sealed)
for c in chambers:
    c['min_wall_to_cockpit_mm'] = float(manifold(c['mesh']).min_gap(cockpit_void, 10.0))
for a in chambers:   # bulkhead thickness: the nearest other chamber
    a['min_wall_to_next_chamber_mm'] = min(float(manifold(a['mesh']).min_gap(manifold(b['mesh']), 10.0)) for b in chambers if b is not a)
plastic_cm3 = body.volume/1000
top = body.vertices[body.vertices[:, 2] > H - 0.01]   # gunwale rim: the cockpit floods when any of it is under

def floating(displacer, material, water, heel=0.0, pitch=0.0, payload_g=0.0, flooded=None):
    """Static equilibrium at a fixed heel and pitch. displacer is the body that keeps water
    out; flooded (a chamber mesh) is subtracted from it. Payload sits on the cockpit floor."""
    rho = DENSITY[material]; mass = plastic_cm3*rho
    g = body.center_mass.copy()
    if payload_g:
        g = (g*mass + np.array([0, 0, design['floor_mm']])*payload_g)/(mass+payload_g); mass += payload_g
    R = rot([1, 0, 0], heel) @ rot([0, 1, 0], -pitch)
    model = manifold(displacer)
    if flooded is not None: model = model - manifold(flooded)
    model = model.transform(np.asarray(R[:3, :], dtype=np.float32))
    g = trimesh.transform_points([g], R)[0]
    target = mass/WATER[water]*1000
    if model.volume() <= target: return None
    bb = model.bounding_box(); lo, hi = bb[2], bb[5]
    for _ in range(32):
        w = (lo+hi)/2
        if model.trim_by_plane((0, 0, -1), -w).volume() < target: lo = w
        else: hi = w
    w = (lo+hi)/2; under = mesh(model.trim_by_plane((0, 0, -1), -w)); b = under.center_mass
    rim = trimesh.transform_points(top, R)
    return {'mass_g': mass, 'waterline_mm': float(w), 'righting_arm_mm': float(g[1]-b[1]), 'trim_arm_mm': float(b[0]-g[0]),
            'rim_clearance_mm': float(rim[:, 2].min()-w)}

def level_trim(displacer, material, water, **kw):
    lo, hi = -15.0, 15.0
    for _ in range(22):
        p = (lo+hi)/2; s = floating(displacer, material, water, pitch=p, **kw)
        if s['trim_arm_mm'] > 0: lo = p
        else: hi = p
    return (lo+hi)/2

angles = list(range(0, 95, 5)) + [100, 120, 150, 180]
def curve(displacer, material, water, **kw):
    out = []
    for a in angles:
        s = floating(displacer, material, water, heel=a, **kw)
        out.append({'heel_deg': a, 'righting_arm_mm': None if s is None else s['righting_arm_mm'],
                    'rim_clearance_mm': None if s is None else s['rim_clearance_mm']})
    return out
def summary(c):
    up = [x for x in c if 0 < x['heel_deg'] <= 90 and x['righting_arm_mm'] is not None]
    vanish = next((x['heel_deg'] for x in up if x['righting_arm_mm'] <= 0), None)
    best = max(up, key=lambda x: x['righting_arm_mm'])
    return {'max_righting_arm_mm': best['righting_arm_mm'], 'heel_of_max_deg': best['heel_deg'],
            'first_sampled_heel_with_no_righting_deg': vanish, 'righting_arm_inverted_180_mm': c[-1]['righting_arm_mm']}

report = {'assumptions': ['Homogeneous solid PLA (sliced at 100% infill), 1.24 g/cm3; fresh water 1.000, sea water 1.025 g/cm3.',
                          'Static heel about the long axis and static pitch; no waves, surf, slamming, dynamic capsize or leakage rate.',
                          'Swamped: the cockpit is full of water, which weighs nothing in the water around it.',
                          'Payload is a point mass on the cockpit floor at midships.'],
          'plastic_cm3': plastic_cm3, 'mass_g_PLA': plastic_cm3*1.24, 'envelope_cm3': env.volume/1000, 'sealed_body_cm3': sealed.volume/1000,
          'sealed_air_cm3': sum(c['air_cm3'] for c in chambers), 'bounds_mm': body.bounds.tolist(), 'gravity_center_mm': body.center_mass.tolist(),
          'chambers': [{k: v for k, v in c.items() if k != 'mesh'} for c in chambers], 'cases': {}}
for water in ['fresh', 'sea']:
    m = 'PLA'
    dry = floating(env, m, water); dry_curve = curve(env, m, water)
    downflood = next((x['heel_deg'] for x in dry_curve if x['rim_clearance_mm'] is not None and x['rim_clearance_mm'] <= 0), None)
    sw = floating(sealed, m, water); sw_curve = curve(sealed, m, water)
    worst = None
    for c in chambers:
        s = floating(sealed, m, water, flooded=c['mesh'])
        if s is not None and (worst is None or s['rim_clearance_mm'] < worst[1]['rim_clearance_mm']): worst = (c, s)
    holed_curve = curve(sealed, m, water, flooded=worst[0]['mesh']) if worst else None
    lo, hi = 0.0, 3000.0
    for _ in range(25):
        p = (lo+hi)/2; s = floating(env, m, water, payload_g=p)
        if s is None or s['rim_clearance_mm'] < 10: hi = p
        else: lo = p
    report['cases'][f'{m}-{water}'] = {
        'dry': {'draft_mm': dry['waterline_mm'], 'freeboard_to_gunwale_mm': H - dry['waterline_mm'],
                'static_trim_deg_bow_up': level_trim(env, m, water), 'downflooding_heel_deg': downflood, **summary(dry_curve), 'righting_curve': dry_curve},
        'swamped': {'waterline_mm': sw['waterline_mm'], 'gunwale_above_water_mm': sw['rim_clearance_mm'],
                    'static_trim_deg_bow_up': level_trim(sealed, m, water), **summary(sw_curve), 'righting_curve': sw_curve},
        'swamped_and_worst_chamber_flooded': None if worst is None else {
                    'chamber': worst[0]['name'], 'gunwale_above_water_mm': worst[1]['rim_clearance_mm'], **summary(holed_curve), 'righting_curve': holed_curve},
        'payload_g_dry_until_rim_10mm_above_water': lo}
(B/'validation.json').write_text(json.dumps(report, indent=2)+'\n')

fig = plt.figure(figsize=(14, 8))
ax = fig.add_subplot(2, 3, 1, projection='3d')
ax.add_collection3d(Poly3DCollection(body.triangles, facecolors='#e8d27a', edgecolors=(0, 0, 0, 0), linewidth=0, shade=True))
ax.set_xlim(-120, 120); ax.set_ylim(-70, 70); ax.set_zlim(0, 70); ax.set_box_aspect((240, 140, 70)); ax.view_init(elev=35, azim=-60); ax.set_axis_off()
ax.set_title(f'{design["footprint_mm"][0]:.0f} x {design["footprint_mm"][1]:.0f} x {H:.0f} mm, PLA {plastic_cm3*1.24:.0f} g\n'
             f'{len(chambers)} sealed chambers, {report["sealed_air_cm3"]:.0f} cm3 air')
for i, (x, label) in enumerate([(0, 'Midships section (x = 0)'), (design['bulkheads_x'][-1] + 10, 'Forward section')]):
    ax = fig.add_subplot(2, 3, 2+i); sec = body.section(plane_origin=[x, 0, 0], plane_normal=[1, 0, 0])
    if sec is not None:
        for e in sec.entities: pts = sec.vertices[e.points]; ax.plot(pts[:, 1], pts[:, 2], color='#7a5c12', lw=1)
    c = report['cases']['PLA-sea']
    ax.axhline(c['dry']['draft_mm'], color='#1f77b4', ls='--', lw=.9, label='dry waterline (sea)')
    ax.axhline(c['swamped']['waterline_mm'], color='#d62728', ls='--', lw=.9, label='swamped waterline (sea)')
    ax.set_aspect('equal'); ax.set_xlabel('y (mm)'); ax.set_ylabel('z (mm)'); ax.legend(fontsize=7); ax.set_title(label)
ax = fig.add_subplot(2, 3, 4)
sec = body.section(plane_origin=[0, 0, 0], plane_normal=[0, 1, 0])
for e in sec.entities: pts = sec.vertices[e.points]; ax.plot(pts[:, 0], pts[:, 2], color='#7a5c12', lw=.8)
ax.set_aspect('equal'); ax.set_title('Centreline section (y = 0)'); ax.set_xlabel('x (mm)')
for i, cond in enumerate(['dry', 'swamped']):
    ax = fig.add_subplot(2, 3, 5+i); c = report['cases']['PLA-sea'][cond]['righting_curve']
    h = [x['heel_deg'] for x in c if x['righting_arm_mm'] is not None]
    ax.plot(h, [x['righting_arm_mm'] for x in c if x['righting_arm_mm'] is not None], 'o-', ms=3, label='righting arm (mm)')
    if cond == 'swamped':
        hc = report['cases']['PLA-sea']['swamped_and_worst_chamber_flooded']['righting_curve']
        ax.plot([x['heel_deg'] for x in hc if x['righting_arm_mm'] is not None], [x['righting_arm_mm'] for x in hc if x['righting_arm_mm'] is not None], 's--', ms=3, label='plus worst chamber flooded')
    ax.axhline(0, color='grey', lw=.8); ax.set_xlabel('heel (degrees)'); ax.grid(alpha=.2); ax.legend(fontsize=7)
    ax.set_title(f'{cond}, sea water')
fig.suptitle('Beach monohull: geometry and static checks (surf behaviour and leaks untested)', fontsize=13)
fig.tight_layout(); fig.savefig(B/'preview-and-stability.png', dpi=140)
print(json.dumps({k: v for k, v in report.items() if k not in ('cases', 'chambers')}, indent=1))
for c in report['chambers']: print(' ', c['name'], {k: round(v, 2) for k, v in c.items() if isinstance(v, float)})
for k, v in report['cases'].items():
    for cond in ['dry', 'swamped', 'swamped_and_worst_chamber_flooded']:
        print(k, cond, json.dumps({a: (round(b, 2) if isinstance(b, float) else b) for a, b in (v[cond] or {}).items() if a != 'righting_curve'}))
    print(k, 'payload', round(v['payload_g_dry_until_rim_10mm_above_water']))
