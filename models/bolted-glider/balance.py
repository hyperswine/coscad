"""Weight and balance from the sliced packages. Writes balance.json.

Each part's mass is the slicer's filament estimate for its plate. The tail plate holds
the stab and fin together, so its mass is split by surface area (on these thin parts the
skins are most of the plastic). Each part's centre of mass is its mesh centroid in flight
position (assembly.py), which is close enough for thin, evenly shelled parts. Screws and
nuts are point masses at their positions.

The balance point is set for a static margin of 10% of the mean aerodynamic chord ahead
of the neutral point from verify.py. Ballast goes in the two nose pockets, front one first; the report gives the mass
with lead shot or steel BBs, and with steel or nylon tail screws.

Flight: speed at a lift coefficient of 0.6 (a gentle glide for a thin flat-bottomed
section at these Reynolds numbers), and sink rate for an assumed lift-to-drag ratio of 8.
"""
from pathlib import Path
import json, math
import numpy as np
from assembly import B, d, W, T, load, WING_TO_USE
v = json.loads((B/'validation.json').read_text()); aero = v['aero']
pk = json.loads((B/'packages.json').read_text())
M5 = {'M5x12': 2.6, 'M5x16': 3.2, 'nut': 1.2}            # g, steel button head screws and nuts

grams = {'wing-right': pk['a1-wing-right']['estimated_grams'], 'wing-left': pk['p1s-wing-left']['estimated_grams'],
         'fuselage': pk['x1c-fuselage']['estimated_grams']}
meshes = {p: load(p) for p in ['wing-right', 'wing-left', 'fuselage', 'stab', 'fin']}
a_st, a_fn = meshes['stab'].area, meshes['fin'].area
grams['stab'] = pk['a1-tail']['estimated_grams']*a_st/(a_st + a_fn); grams['fin'] = pk['a1-tail']['estimated_grams'] - grams['stab']
items = [(p, grams[p], meshes[p].center_mass) for p in grams]
for side in (1, -1):
    for u in W['screw_u']:
        x, y = W['root_gap'] + W['screw_station'], u*W['root_chord']
        p = (WING_TO_USE @ [x, y, 0, 1])[:3]; p[0] *= side
        items.append(('wing screw + nut', M5['M5x12'] + M5['nut'], p))
for x, dy in T['screws']:
    items.append(('tail screw + nut', M5['M5x16'] + M5['nut'], np.array([x, T['screw_y_ref'] + dy, T['platform_top_z']])))
M = sum(m for _, m, _ in items); cg = sum(m*np.asarray(c) for _, m, c in items)/M

mac = aero['mac_mm']; mac_le = aero['wing_ac_y'] - 0.25*mac
SM = 0.10
target_y = aero['neutral_point_y'] - SM*mac
pockets = d['fuselage']['ballast_pockets']                       # front pocket first
caps = [math.pi*q['radius']**2*q['depth']/1000 for q in pockets]  # cm3
def ballast_for(M, cgy, density):
    """Ballast mass that puts the balance point on target, filling the front pocket first."""
    lo, hi = 0.0, 400.0
    for _ in range(60):
        b = (lo + hi)/2; left = b/density; mom = M*cgy; mass = M
        for q, cap in zip(pockets, caps):
            v_ = min(left, cap); mom += v_*density*q['y']; mass += v_*density; left -= v_
        mom += left*density*pockets[-1]['y']; mass += left*density   # overflow (does not fit)
        if mom/mass > target_y: lo = b
        else: hi = b
    return b, b/density <= sum(caps) + 1e-9
DENS = {'lead shot': 6.8, 'steel BBs': 4.7}
options = {}
for label, dm in [('steel tail screws', 0.0), ('nylon tail screws', -2*(M5['M5x16'] + M5['nut'] - 1.0))]:
    tail_y = T['screw_y_ref']; M2 = M + dm; cg2 = (M*cg[1] + dm*tail_y)/M2
    options[label] = {k: dict(zip(['ballast_g', 'fits_pockets'], ballast_for(M2, cg2, rho))) | {'flying_g': M2 + ballast_for(M2, cg2, rho)[0]}
                      for k, rho in DENS.items()}
ballast, fits_ = ballast_for(M, cg[1], 6.8)
total = M + ballast
fill = {k: v_['ballast_g']/DENS[k] for k, v_ in options['steel tail screws'].items()}
pocket = sum(caps)
S = aero['wing_area_mm2']*1e-6
loading = total/(aero['wing_area_mm2']/1e4)
V = math.sqrt(2*total*1e-3*9.81/(1.225*S*0.6)); sink = V/8
# Root bending at a hard launch (5 g), per panel, and the screw tension it causes: the
# root's inboard edge bears on the saddle, the screws hold the panel down 13 mm outboard of it.
lift_panel = 5*total*1e-3*9.81/2
y_bar = 0.42*W['panel_length']*1e-3
moment = lift_panel*(y_bar + 0.013)
tension = moment/0.013/2

rep = {'parts_g': grams, 'hardware_g': sum(m for n_, m, _ in items if 'screw' in n_), 'empty_g': M,
       'cg_empty': cg.tolist(), 'cg_empty_fraction_mac': (cg[1] - mac_le)/mac,
       'neutral_point_fraction_mac': (aero['neutral_point_y'] - mac_le)/mac, 'static_margin': SM,
       'target_cg_y': target_y, 'target_cg_fraction_mac': (target_y - mac_le)/mac,
       'ballast_g_lead_steel_screws': ballast, 'pocket_cm3': pocket, 'ballast_volume_cm3': fill, 'options': options,
       'flying_g': total, 'wing_loading_g_per_dm2': loading, 'glide_speed_m_s_at_CL_0.6': V, 'sink_m_s_at_LD_8': sink,
       'launch_5g': {'lift_per_panel_N': lift_panel, 'root_moment_N_m': moment, 'tension_per_screw_N': tension}}
(B/'balance.json').write_text(json.dumps(rep, indent=2, default=float) + '\n')
print(json.dumps({k: (round(x, 3) if isinstance(x, float) else x) for k, x in rep.items() if not isinstance(x, (list, dict))}, indent=1))
print('parts', {k: round(x, 1) for k, x in grams.items()}, 'ballast volume', {k: round(x, 2) for k, x in fill.items()})
