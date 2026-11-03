"""Checks for the tighter stems (stems.py) against the printed body. Writes
stems-validation.json and the print-oriented meshes stl/stem-fitNN_print.stl.

For each stem, in use:
  - no overlap with the body (boolean intersection volume);
  - the peg's measured width across flats, and its clearance in the 12.5 mm socket;
  - the seat: lowered 0.05 mm further, the stem would overlap the body (it rests on the
    roof faces), while the shoulder still clears the body's top face;
  - the lean the clearance allows over the engagement, and how far off the axis that puts
    the grip's top end (the old stem is given for comparison);
  - the print mesh gets four mouse-ear pads (stem-ears.coscad) joined to the grip end;
  - mass properties and sleeping speed of body + stem (same formula as verify.py).
"""
from pathlib import Path
import json, math
import numpy as np, trimesh, manifold3d as mf
B = Path(__file__).resolve().parent
d = json.loads((B/'design.json').read_text()); st = json.loads((B/'stems.json').read_text())
PLA = 1.24e-3; G = 9.81

def man(m): return mf.Manifold(mf.Mesh(vert_properties=np.asarray(m.vertices, np.float32), tri_verts=np.asarray(m.faces, np.uint32)))
def overlap(a, b): return (man(a) ^ man(b)).volume()

def props(meshes):
    m = sum(x.volume*PLA for x in meshes); c = sum(x.center_mass*x.volume*PLA for x in meshes)/m
    I = np.zeros((3, 3))
    for x in meshes:
        mm = x.volume*PLA; r = x.center_mass
        I += x.moment_inertia*PLA + mm*(np.dot(r, r)*np.eye(3) - np.outer(r, r))
    I_s = I[2, 2]*1e-9; I_t = 0.5*(I[0, 0] + I[1, 1])*1e-9
    wc = math.sqrt(4*m*1e-3*G*c[2]*1e-3*I_t)/I_s
    return {'mass_g': m, 'cog_above_tip_mm': c[2], 'cog_offset_from_axis_mm': float(math.hypot(c[0], c[1])),
            'spin_inertia_kg_m2': I_s, 'tilt_inertia_about_tip_kg_m2': I_t, 'sleeping_speed_rpm': wc*60/(2*math.pi)}

def width_across_flats(m, z):
    """Peg width across flats at height z (min over the three flat directions of the section)."""
    sec = m.section(plane_origin=[0, 0, z], plane_normal=[0, 0, 1]); pts = sec.vertices[:, :2]
    return min(np.ptp(pts @ np.array([math.cos(a), math.sin(a)])) for a in np.radians([30, 90, 150]))

body = trimesh.load_mesh(B/'body.stl'); assert body.is_watertight
report = {'body_alone': props([body]), 'stems': {}}
old = trimesh.load_mesh(B/'stem.stl'); old.apply_translation([0, 0, d['top_z'] - d['stem']['peg_length']])
report['old_stem'] = {'clearance_per_flat': 0.25, 'engagement_mm': d['stem']['peg_length'],
                      'lean_deg': math.degrees(math.atan(0.5/d['stem']['peg_length'])),
                      'grip_top_offset_mm': (old.bounds[1, 2] - d['top_z'])*0.5/d['stem']['peg_length'],
                      'with_body': props([body, old])}
ears = trimesh.load_mesh(B/'stem-ears.stl'); assert ears.is_watertight and abs(ears.bounds[0, 2]) < 1e-6
flip = trimesh.transformations.rotation_matrix(math.pi, [1, 0, 0])
for name, s in st['stems'].items():
    m = trimesh.load_mesh(B/f'{name}.stl')
    assert m.is_watertight and m.is_volume and len(m.split(only_watertight=False)) == 1, name
    af = width_across_flats(m, d['top_z'] - 5)
    clear = (st['socket_af'] - af)/2
    assert abs(clear - s['clearance_per_flat']) < 0.02, (name, clear)
    ov = overlap(body, m); assert ov < 0.5, (name, 'overlaps the body', ov)
    lower = m.copy(); lower.apply_translation([0, 0, -0.05]); seated = overlap(body, lower) > 0.01
    assert seated, (name, 'does not rest on the roof')
    assert m.bounds[0, 2] > st['socket_roof_apex_z'] + 0.5 and s['shoulder_z'] > d['top_z'] + 0.2
    eng = s['engagement_mm']; lean = math.atan(2*clear/eng)
    # Grooves: 1-3 notches near the grip's top end, 0.8 mm deep on facet centres.
    top = m.section(plane_origin=[0, 0, s['grip_top_z'] - 2], plane_normal=[0, 0, 1]).to_planar()[0].area
    full = m.section(plane_origin=[0, 0, s['grip_top_z'] - 20], plane_normal=[0, 0, 1]).to_planar()[0].area
    printed = m.copy(); printed.apply_transform(flip); printed.apply_translation([0, 0, -printed.bounds[0, 2]])
    with_ears = man(printed) + man(ears)
    out = with_ears.to_mesh(); printed = trimesh.Trimesh(out.vert_properties[:, :3], out.tri_verts)
    assert printed.is_watertight and len(printed.split(only_watertight=False)) == 1, (name, 'ears not joined')
    printed.export(B/'stl'/f'{name}_print.stl')
    nz = printed.face_normals[:, 2]; zc = printed.triangles_center[:, 2]
    steep = float(printed.area_faces[(nz < -math.cos(math.radians(45)) - 1e-6) & (zc > 0.3)].sum())
    report['stems'][name] = {'clearance_per_flat': clear, 'peg_af_mm': af, 'engagement_mm': eng, 'overlap_mm3': ov,
                             'rests_on_roof': seated, 'shoulder_gap_mm': s['shoulder_z'] - d['top_z'],
                             'lean_deg': math.degrees(lean), 'grip_top_offset_mm': (s['grip_top_z'] - d['top_z'])*math.tan(lean),
                             'groove_area_removed_mm2': full - top, 'grooves': s['grooves'], 'stem_g': m.volume*PLA,
                             'print_area_beyond_45deg_mm2': steep, 'with_body': props([body, m])}
(B/'stems-validation.json').write_text(json.dumps(report, indent=2) + '\n')
b = report['body_alone']; print(f"body alone: {b['mass_g']:.0f} g, CoG {b['cog_above_tip_mm']:.1f} mm, sleeps above {b['sleeping_speed_rpm']:.0f} rpm")
o = report['old_stem']; print(f"old stem 0.25: lean {o['lean_deg']:.2f} deg, grip top off-axis {o['grip_top_offset_mm']:.2f} mm, with body sleeps above {o['with_body']['sleeping_speed_rpm']:.0f} rpm")
for k, r in report['stems'].items():
    print(f"{k}: peg {r['peg_af_mm']:.2f} AF, clear {r['clearance_per_flat']:.3f}, engage {r['engagement_mm']:.1f}, seated {r['rests_on_roof']}, "
          f"lean {r['lean_deg']:.2f} deg, grip top {r['grip_top_offset_mm']:.2f} mm, grooves cut {r['groove_area_removed_mm2']:.1f} mm2, "
          f"{r['stem_g']:.1f} g, overhang {r['print_area_beyond_45deg_mm2']:.2f} mm2, sleeps above {r['with_body']['sleeping_speed_rpm']:.0f} rpm")
