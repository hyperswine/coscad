"""Mass properties, stability and printability of the two-hand top. Writes validation.json
and preview.png.

Physics: a top spinning at w about its axis stays upright ("sleeps") when
  w > w_c = sqrt(4 m g l I_t) / I_s
with m the mass, l the height of the centre of gravity above the tip, I_s the spin inertia
about the axis and I_t the tilt inertia about a horizontal axis through the tip. A low l
and a large I_s lower w_c. Solid PLA (the slice is 100% infill), steel nuts as point masses.
"""
from pathlib import Path
import json, math, os
os.environ.setdefault('MPLCONFIGDIR', '/private/tmp/top-matplotlib')
import numpy as np, trimesh
import matplotlib; matplotlib.use('Agg'); import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
B = Path(__file__).resolve().parent
d = json.loads((B/'design.json').read_text())
PLA = 1.24e-3            # g/mm^3
NUT_G = 5.2              # DIN 934 M8 steel hex nut
G = 9.81

body = trimesh.load_mesh(B/'body.stl'); stem = trimesh.load_mesh(B/'stem.stl')
for name, m in [('body', body), ('stem', stem)]:
    assert m.is_watertight and m.is_volume and len(m.split(only_watertight=False)) == 1, name
stem_in_use = stem.copy(); stem_in_use.apply_translation([0, 0, d['top_z'] - d['stem']['peg_length']])

def props(meshes, points):
    """meshes: (mesh, density g/mm^3); points: list of (xyz mm, g). Returns kg, m, kg m^2."""
    m = 0.0; c = np.zeros(3); parts = []
    for mesh, rho in meshes:
        mm = mesh.volume*rho; m += mm; c += mesh.center_mass*mm; parts.append((mesh, rho, mm))
    for p, g in points: m += g; c += np.asarray(p)*g
    c /= m
    I = np.zeros((3, 3))   # about the tip (origin), g mm^2
    for mesh, rho, mm in parts:
        Ic = mesh.moment_inertia*rho          # about the mesh's own centre of mass (trimesh, unit density)
        r = mesh.center_mass; I += Ic + mm*(np.dot(r, r)*np.eye(3) - np.outer(r, r))
    for p, g in points:
        r = np.asarray(p, dtype=float); I += g*(np.dot(r, r)*np.eye(3) - np.outer(r, r))
    I_s = I[2, 2]*1e-9; I_t = 0.5*(I[0, 0] + I[1, 1])*1e-9
    M = m*1e-3; l = c[2]*1e-3
    wc = math.sqrt(4*M*G*l*I_t)/I_s
    return {'mass_g': m, 'cog_above_tip_mm': c[2], 'cog_offset_from_axis_mm': float(math.hypot(c[0], c[1])),
            'spin_inertia_kg_m2': I_s, 'tilt_inertia_about_tip_kg_m2': I_t, 'inertia_ratio_spin_to_tilt': I_s/I_t,
            'sleeping_speed_rad_s': wc, 'sleeping_speed_rpm': wc*60/(2*math.pi)}

nuts = [((d['nut_pockets']['radius']*math.cos(2*math.pi*i/12), d['nut_pockets']['radius']*math.sin(2*math.pi*i/12),
          d['rim_underside_z'] + d['nut_pockets']['depth']/2), NUT_G) for i in range(d['nut_pockets']['count'])]
plain = props([(body, PLA), (stem_in_use, PLA)], [])
ballast = props([(body, PLA), (stem_in_use, PLA)], nuts)

# Palm spin: the palms slide past the grip; a firm two-hand rub moves the grip surface at
# roughly 1 to 2 m/s. Grip radius from the 12-sided stem (inscribed to circumscribed).
r_grip = d['stem']['grip_circumradius']*1e-3*math.cos(math.pi/d['stem']['facets'])
spin = {f'{v} m/s': {'rad_s': v/r_grip, 'rpm': v/r_grip*60/(2*math.pi)} for v in (1.0, 1.5, 2.0)}

def overhang(mesh, label):
    """Area facing downward more steeply than 45 degrees from vertical, off the plate."""
    nz = mesh.face_normals[:, 2]; z = mesh.triangles_center[:, 2]
    steep = (nz < -math.cos(math.radians(45)) - 1e-6) & (z > 0.3)
    return {'part': label, 'area_mm2_beyond_45deg': float(mesh.area_faces[steep].sum()),
            'max_z_of_steep_faces_mm': float(z[steep].max()) if steep.any() else None}
flip = trimesh.transformations.rotation_matrix(math.pi, [1, 0, 0])
body_print = body.copy(); body_print.apply_transform(flip); body_print.apply_translation([0, 0, -body_print.bounds[0, 2]])
stem_print = stem.copy(); stem_print.apply_transform(flip); stem_print.apply_translation([0, 0, -stem_print.bounds[0, 2]])
print_check = [overhang(body_print, 'body, upside down'), overhang(stem_print, 'stem, grip end down')]
(B/'stl').mkdir(exist_ok=True)   # print-oriented meshes for prepare_slice.py
body_print.export(B/'stl'/'body_print.stl'); stem_print.export(B/'stl'/'stem_print.stl')
# Pocket walls: thinnest plastic between a nut pocket and the rim's inner and outer faces.
c = (d['nut_pockets']['across_flats'] + 0.4)/math.sqrt(3)
walls = {'nut_pocket_to_rim_outside_mm': d['rim_radius'] - d['nut_pockets']['radius'] - c,
         'nut_pocket_to_rim_inside_mm': d['nut_pockets']['radius'] - c - d['rim_inner_radius']}

report = {'assumptions': ['Solid PLA 1.24 g/cm3 (printed at 100% infill); M8 nuts 5.2 g each as point masses.',
                          'Sleeping speed is the classic rigid-top threshold on a sharp point; a rounded tip and floor friction shift it a little.',
                          'Palm speeds are a rough range for a firm two-hand rub, not a measurement.'],
          'body_g': body.volume*PLA, 'stem_g': stem.volume*PLA, 'without_nuts': plain, 'with_12_m8_nuts': ballast,
          'palm_spin_estimate': spin, 'tilt_before_rim_touches_deg': d['tilt_before_rim_touches_deg'],
          'printability': print_check, 'pocket_walls': walls}
(B/'validation.json').write_text(json.dumps(report, indent=2) + '\n')

fig = plt.figure(figsize=(12, 5))
ax = fig.add_subplot(1, 2, 1, projection='3d')
for mesh, col in [(body, '#e9a23b'), (stem_in_use, '#4f86c6')]:
    ax.add_collection3d(Poly3DCollection(mesh.triangles, facecolors=col, edgecolors=(0, 0, 0, 0), linewidth=0, shade=True))
ax.set_xlim(-85, 85); ax.set_ylim(-85, 85); ax.set_zlim(0, 140); ax.set_box_aspect((170, 170, 140)); ax.view_init(elev=18, azim=-60); ax.set_axis_off()
ax.set_title(f'Two-hand top: {2*d["rim_radius"]:.0f} mm wide, {plain["mass_g"]:.0f} g ({ballast["mass_g"]:.0f} g with nuts)')
ax = fig.add_subplot(1, 2, 2)
assembled = trimesh.util.concatenate([body, stem_in_use]); sec = assembled.section(plane_origin=[0, 0, 0], plane_normal=[0, 1, 0])
for e in sec.entities: pts = sec.vertices[e.points]; ax.plot(pts[:, 0], pts[:, 2], color='#7a4a0c', lw=1)
for label, p, col in [('CoG', plain, '#1f77b4'), ('CoG with nuts', ballast, '#d62728')]:
    ax.plot([0], [p['cog_above_tip_mm']], 'o', color=col, label=f'{label}: {p["cog_above_tip_mm"]:.1f} mm, sleeps above {p["sleeping_speed_rpm"]:.0f} rpm')
t = math.radians(d['tilt_before_rim_touches_deg']); ax.plot([0, 100*math.cos(t)], [0, 100*math.sin(t)], '--', color='grey', lw=.8, label=f'floor at {d["tilt_before_rim_touches_deg"]:.1f} deg tilt')
ax.set_aspect('equal'); ax.set_xlabel('radius (mm)'); ax.set_ylabel('height (mm)'); ax.legend(fontsize=8, loc='upper right'); ax.set_title('Section through the axis')
fig.tight_layout(); fig.savefig(B/'preview.png', dpi=140)
print(json.dumps({k: v for k, v in report.items() if k != 'assumptions'}, indent=1))
