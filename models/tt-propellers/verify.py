"""Checks and a rough performance comparison for the TT propellers. Writes validation.json
and preview.png.

Thrust and torque come from blade-element momentum theory at zero boat speed (bollard
pull): each annulus balances blade lift and drag against the momentum it gives the water,
with Prandtl's tip loss. Sections are thin printed plates at low Reynolds number
(about 5,000 to 20,000), so lift is capped at CL 0.8 and drag is CD 0.06 + 0.8 alpha^2:
treat the numbers as a ranking, good to perhaps a factor of 1.5.

The motor: a TT 1:48 gearmotor, taken as a straight torque-speed line from no-load speed to
stall torque. Typical figures (they vary by maker): 6 V about 230 rpm and 0.08 N m stall;
3 V about 110 rpm and 0.04 N m. The operating point is where the propeller's torque meets
that line.
"""
from pathlib import Path
import json, math, os
os.environ.setdefault('MPLCONFIGDIR', '/private/tmp/prop-matplotlib')
import numpy as np, trimesh
import matplotlib; matplotlib.use('Agg'); import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
B = Path(__file__).resolve().parent
d = json.loads((B/'design.json').read_text())
RHO = 1000.0; PLA = 1.24e-3
MOTOR = {'6 V': (230.0, 0.08), '3 V': (110.0, 0.04)}   # no-load rpm, stall torque N m

def bemt(p, rpm, V0=0.0, n_el=40):
    """Thrust (N) and torque (N m) of design p at rpm, boat speed V0 (m/s)."""
    st = p['stations']; R = p['D']/2000; Bn = p['blades']
    rs = np.array([s['r'] for s in st])/1000; cs = np.array([s['chord'] for s in st])/1000; bs = np.radians([s['beta_deg'] for s in st])
    r_el = np.linspace(rs[0], R*0.995, n_el); dr = r_el[1] - r_el[0]
    c_el = np.interp(r_el, rs, cs); b_el = np.interp(r_el, rs, bs)
    w = rpm*2*math.pi/60; T = Q = 0.0
    for r, c, beta in zip(r_el, c_el, b_el):
        va = 0.05
        for _ in range(200):
            Va = V0 + va; phi = math.atan2(Va, w*r)
            f = Bn*(R - r)/(2*r*max(math.sin(phi), 1e-4)); F = 2/math.pi*math.acos(min(1.0, math.exp(-f)))
            a = beta - phi; cl = max(-0.8, min(0.8, 0.9*2*math.pi*a)); cd = 0.06 + 0.8*a*a
            W2 = (w*r)**2 + Va**2
            dT = 0.5*RHO*W2*Bn*c*(cl*math.cos(phi) - cd*math.sin(phi))
            va_new = math.sqrt(max(dT, 0.0)/(4*math.pi*RHO*r*max(F, 1e-3)) + (V0/2)**2) - V0/2
            if abs(va_new - va) < 1e-6: break
            va = 0.5*va + 0.5*va_new
        dQ = 0.5*RHO*W2*Bn*c*(cl*math.sin(phi) + cd*math.cos(phi))*r
        T += dT*dr; Q += dQ*dr
    return T, Q

def operating(p, n0, qs):
    """Speed where the propeller's torque equals the motor's torque line."""
    lo, hi = 1.0, n0
    for _ in range(40):
        n = (lo + hi)/2; _, q = bemt(p, n)
        if q > qs*(1 - n/n0): hi = n
        else: lo = n
    t, q = bemt(p, lo); return {'rpm': lo, 'thrust_gf': t/9.81*1000, 'torque_N_m': q, 'motor_load_fraction_of_stall': q/qs}

report = {'assumptions': __doc__.strip().split('\n\n')[1:], 'designs': {}}
meshes = {}
for name, p in d['designs'].items():
    if name == 'coupon': continue
    m = trimesh.load_mesh(B/'stl'/f'{name}.stl'); meshes[name] = m
    assert m.is_watertight and m.is_volume and len(m.split(only_watertight=False)) == 1, name
    nz = m.face_normals[:, 2]; z = m.triangles_center[:, 2]
    overhang = float(m.area_faces[(nz < -math.cos(math.radians(45))) & (z > 0.3)].sum())
    report['designs'][name] = {
        'blades': p['blades'], 'diameter_mm': p['D'], 'pitch_mm': p['pitch'], 'pitch_over_diameter': p['pitch']/p['D'],
        'hub_length_mm': p['hub']['length'], 'mass_g': m.volume*PLA, 'downward_area_beyond_45deg_mm2': overhang,
        'at_6V': operating(p, *MOTOR['6 V']), 'at_3V': operating(p, *MOTOR['3 V'])}
coupon = trimesh.load_mesh(B/'stl'/'shaft-coupon.stl'); assert coupon.is_watertight and len(coupon.split(only_watertight=False)) == 3
report['coupon'] = {'rings': 3, 'mass_g': coupon.volume*PLA, 'clearances_mm': d['designs']['coupon']['clearances']}
(B/'validation.json').write_text(json.dumps(report, indent=2) + '\n')

fig = plt.figure(figsize=(14, 4.2))
for i, (name, m) in enumerate(meshes.items()):
    ax = fig.add_subplot(1, 4, i + 1, projection='3d')
    ax.add_collection3d(Poly3DCollection(m.triangles, facecolors='#3a7bd5', edgecolors=(0, 0, 0, .08), linewidth=.2, shade=True))
    ax.set_xlim(-31, 31); ax.set_ylim(-31, 31); ax.set_zlim(-10, 30); ax.set_box_aspect((62, 62, 40)); ax.view_init(elev=35, azim=-60); ax.set_axis_off()
    r = report['designs'][name]
    ax.set_title(f"{name}: {r['mass_g']:.1f} g\n6 V: {r['at_6V']['rpm']:.0f} rpm, ~{r['at_6V']['thrust_gf']:.0f} gf", fontsize=10)
fig.suptitle('TT gearmotor propellers (static thrust estimates, untested)'); fig.tight_layout(); fig.savefig(B/'preview.png', dpi=130)
for name, r in report['designs'].items():
    print(f"{name}: {r['mass_g']:.1f} g, overhang {r['downward_area_beyond_45deg_mm2']:.0f} mm2 | 6 V {r['at_6V']['rpm']:.0f} rpm {r['at_6V']['thrust_gf']:.1f} gf load {r['at_6V']['motor_load_fraction_of_stall']:.0%} | 3 V {r['at_3V']['rpm']:.0f} rpm {r['at_3V']['thrust_gf']:.1f} gf")
