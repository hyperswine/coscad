"""Geometry, wall thickness and static hydrostatics for the sealed beach outrigger.
Not a physical leak test. Writes validation.json and preview-and-stability.png."""
from pathlib import Path
import json, math, os
os.environ.setdefault('MPLCONFIGDIR', '/private/tmp/boat-matplotlib')
import numpy as np, trimesh, manifold3d as md
import matplotlib; matplotlib.use('Agg')
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
B = Path(__file__).resolve().parent
DENSITY = {'PLA': 1.24, 'PETG': 1.27}
WATER = {'fresh': 1.000, 'sea': 1.025}
design = json.loads((B/'design.json').read_text())
BEAM_Z = design['beams']['z']

def manifold(m): return md.Manifold(md.Mesh(np.asarray(m.vertices, dtype=np.float32), np.asarray(m.faces, dtype=np.uint32)))
def mesh(m):
    d = m.to_mesh(); return trimesh.Trimesh(np.asarray(d.vert_properties)[:, :3], np.asarray(d.tri_verts), process=False)
def rot(axis, deg): return trimesh.transformations.rotation_matrix(math.radians(deg), axis)

body = trimesh.load_mesh(B/'beach-outrigger.stl'); env = trimesh.load_mesh(B/'beach-outrigger-envelope.stl')
assert body.is_watertight and body.is_volume and env.is_watertight
shells = body.split(); positive = [s for s in shells if s.volume > 0]; voids = [s for s in shells if s.volume < 0]
assert len(positive) == 1 and len(voids) == 3, (len(positive), len(voids))
# Minimum wall: exact minimum gap between each air chamber and the outside world.
outside = md.Manifold.cube([400, 400, 200], True) - manifold(env)
thickness = {}
for i, v in enumerate(sorted(voids, key=lambda s: s.center_mass[1])):
    air = manifold(trimesh.Trimesh(v.vertices, v.faces[:, ::-1], process=False))
    thickness[['port pontoon', 'center hull', 'starboard pontoon'][i]] = {'min_mm': float(air.min_gap(outside, 10.0)), 'air_cm3': float(-v.volume/1000)}
plastic_cm3 = body.volume/1000; envelope_cm3 = env.volume/1000

def floating(material, water, heel=0.0, pitch=0.0, payload_g=0.0, payload_z=None):
    """Static equilibrium at a fixed heel (about x) and pitch (about y). Returns the
    waterline and righting arms. payload is a point mass on the center deck ridge."""
    rho = DENSITY[material]; mass = plastic_cm3*rho
    g = body.center_mass.copy()
    if payload_g:
        pz = payload_z if payload_z is not None else design['height_mm']
        g = (g*mass + np.array([0, 0, pz])*payload_g)/(mass+payload_g); mass += payload_g
    R = rot([1, 0, 0], heel) @ rot([0, 1, 0], -pitch)   # positive pitch lifts the bow (+x)
    E = env.copy(); E.apply_transform(R); model = manifold(E)
    g = trimesh.transform_points([g], R)[0]
    target = mass/WATER[water]*1000
    if model.volume() <= target: return None
    lo, hi = E.bounds[:, 2]
    for _ in range(30):
        w = (lo+hi)/2
        if model.trim_by_plane((0, 0, -1), -w).volume() < target: lo = w
        else: hi = w
    w = (lo+hi)/2; under = mesh(model.trim_by_plane((0, 0, -1), -w)); b = under.center_mass
    beams = trimesh.transform_points([[x, 0, BEAM_Z] for x in design['beams']['x_positions']], R)
    return {'mass_g': mass, 'waterline_mm': float(w), 'righting_arm_mm': float(g[1]-b[1]), 'trim_arm_mm': float(b[0]-g[0]),
            'beam_underside_clearance_mm': float(beams[:, 2].min()-w), 'gravity_center_mm': g.tolist(), 'buoyancy_center_mm': b.tolist()}

def level_trim(material, water, **kw):
    """Pitch angle at which the longitudinal moment vanishes (bow up positive)."""
    lo, hi = -15.0, 15.0
    for _ in range(25):
        p = (lo+hi)/2; s = floating(material, water, pitch=p, **kw)
        if s['trim_arm_mm'] > 0: lo = p   # buoyancy ahead of gravity: bow rises
        else: hi = p
    return (lo+hi)/2

def payload_until(material, water, predicate, zpos):
    lo, hi = 0.0, 3000.0
    for _ in range(25):
        p = (lo+hi)/2; s = floating(material, water, payload_g=p, payload_z=zpos)
        if s is None or not predicate(s): hi = p
        else: lo = p
    return lo

report = {'assumptions': ['Homogeneous solid plastic (sliced at 100% infill): PLA 1.24, PETG 1.27 g/cm3.',
                          'All three chambers stay airtight; fresh water 1.000, sea water 1.025 g/cm3.',
                          'Static heel about the long axis and static pitch; no waves, surf, dynamic capsize, leakage or fatigue.',
                          'Payload is a point mass on the center ridge (worst case for stability) unless noted.'],
          'plastic_cm3': plastic_cm3, 'envelope_cm3': envelope_cm3, 'bounds_mm': body.bounds.tolist(),
          'gravity_center_mm': body.center_mass.tolist(), 'wall_thickness': thickness, 'cases': {}}
angles = list(range(0, 95, 5)) + [120, 150, 180]
for material in ['PLA', 'PETG']:
    for water in ['fresh', 'sea']:
        level = floating(material, water)
        curve = [dict(heel_deg=a, **{k: v for k, v in floating(material, water, heel=a).items() if k in ['righting_arm_mm', 'beam_underside_clearance_mm', 'waterline_mm']}) for a in angles]
        upright = [c for c in curve if 0 < c['heel_deg'] <= 90]
        vanish = next((c['heel_deg'] for c in upright if c['righting_arm_mm'] <= 0), None)
        pitch = level_trim(material, water)
        report['cases'][f'{material}-{water}'] = {
            'mass_g': level['mass_g'], 'draft_mm': level['waterline_mm'],
            'pontoon_freeboard_to_ridge_mm': design['pontoon_outer_stations'][-1][0]-level['waterline_mm'],
            'beam_underside_clearance_mm': level['beam_underside_clearance_mm'],
            'static_trim_deg_bow_up': pitch,
            'max_righting_arm_mm': max(c['righting_arm_mm'] for c in upright),
            'heel_of_max_righting_arm_deg': max(upright, key=lambda c: c['righting_arm_mm'])['heel_deg'],
            'first_sampled_heel_with_no_righting_deg': vanish,
            'righting_arm_inverted_180_mm': curve[-1]['righting_arm_mm'],
            'payload_g_until_beams_touch_water_on_center_ridge': payload_until(material, water, lambda s: s['beam_underside_clearance_mm'] > 0, design['height_mm']),
            'payload_g_until_beams_touch_water_at_waterline_height': payload_until(material, water, lambda s: s['beam_underside_clearance_mm'] > 0, level['waterline_mm']),
            'payload_g_until_hulls_fully_submerged': envelope_cm3*WATER[water]-level['mass_g'],
            'righting_curve': curve}
(B/'validation.json').write_text(json.dumps(report, indent=2)+'\n')

fig = plt.figure(figsize=(13, 7))
ax = fig.add_subplot(2, 2, 1, projection='3d')
ax.add_collection3d(Poly3DCollection(body.triangles, facecolors='#72a9a0', edgecolors=(0, 0, 0, 0), linewidth=0, shade=True))
ax.set_xlim(-125, 125); ax.set_ylim(-110, 110); ax.set_zlim(0, 70); ax.set_box_aspect((250, 220, 70)); ax.view_init(elev=30, azim=-55); ax.set_axis_off()
ax.set_title(f'Sealed beach outrigger, 2.1x, {design["footprint_mm"][0]:.0f} x {design["footprint_mm"][1]:.0f} x {design["height_mm"]:.0f} mm\n'
             f'PLA {plastic_cm3*1.24:.0f} g, PETG {plastic_cm3*1.27:.0f} g; air {envelope_cm3-plastic_cm3:.0f} cm3')
ax = fig.add_subplot(2, 2, 2)
sec = body.section(plane_origin=[0, 0, 0], plane_normal=[1, 0, 0])
for e in sec.entities: pts = sec.vertices[e.points]; ax.plot(pts[:, 1], pts[:, 2], color='#2b5f59', lw=1)
BX = design['beams']['x_positions'][-1]
secb = body.section(plane_origin=[BX, 0, 0], plane_normal=[1, 0, 0])
for e in secb.entities: pts = secb.vertices[e.points]; ax.plot(pts[:, 1], pts[:, 2], color='#c97b2a', lw=.8, alpha=.8)
for key, color in [('PLA-fresh', '#1f77b4'), ('PETG-sea', '#d62728')]:
    ax.axhline(report['cases'][key]['draft_mm'], color=color, lw=.9, ls='--', label=f'waterline {key}')
ax.set_aspect('equal'); ax.set_xlabel('y (mm)'); ax.set_ylabel('z (mm)'); ax.legend(fontsize=8); ax.set_title(f'Midship section (x=0) and beam section (x={BX})')
for i, key in enumerate(['PLA-fresh', 'PETG-sea']):
    ax = fig.add_subplot(2, 2, 3+i); c = report['cases'][key]['righting_curve']
    h = [x['heel_deg'] for x in c if x['heel_deg'] <= 90]
    ax.plot(h, [x['righting_arm_mm'] for x in c if x['heel_deg'] <= 90], 'o-', label='Righting arm (mm)')
    ax.plot(h, [x['beam_underside_clearance_mm'] for x in c if x['heel_deg'] <= 90], 's-', label='Lowest beam underside above water (mm)')
    ax.axhline(0, color='grey', lw=.8); ax.set_xlabel('Heel angle (degrees)'); ax.grid(alpha=.2); ax.legend(fontsize=8)
    r = report['cases'][key]; ax.set_title(f'{key}: {r["mass_g"]:.0f} g, draft {r["draft_mm"]:.1f} mm, trim {r["static_trim_deg_bow_up"]:+.1f} deg', fontsize=10)
fig.suptitle('Beach outrigger: geometry and static checks (physical leak and surf behaviour untested)', fontsize=13)
fig.tight_layout(); fig.savefig(B/'preview-and-stability.png', dpi=150)
print(json.dumps({k: v for k, v in report.items() if k != 'cases'}, indent=1))
for k, v in report['cases'].items(): print(k, json.dumps({a: (round(b, 2) if isinstance(b, float) else b) for a, b in v.items() if a != 'righting_curve'}))
