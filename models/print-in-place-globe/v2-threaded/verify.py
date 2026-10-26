"""Check moving clearance and complete screw insertion on rendered meshes."""
from pathlib import Path
import json
import math
import numpy as np
import trimesh

B = Path(__file__).resolve().parent
meshes = {n: trimesh.load_mesh(B/(n+'.stl')) for n in
          ['gimbal', 'rotor', 'base', 'coupon-female', 'coupon-male']}
report = {}
for n, m in meshes.items():
    assert m.is_watertight and m.is_winding_consistent and m.volume > 0, n
    assert len(m.split()) == 1, (n, len(m.split()))
    assert m.bounds[0, 2] >= -1e-4, n
    report[n] = {'components': 1, 'watertight': True, 'triangles': len(m.faces),
                 'volume_mm3': float(m.volume), 'bounds_mm': m.bounds.tolist()}


def overlap(a, b):
    m = trimesh.boolean.intersection([a, b], engine='manifold')
    return 0. if m.is_empty else float(abs(m.volume))


gimbal, rotor, base = (meshes[k] for k in ['gimbal', 'rotor', 'base'])
rotation = []
for dz in [-.6, 0, .6]:
    for angle in range(0, 360, 15):
        m = rotor.copy()
        m.apply_transform(trimesh.transformations.rotation_matrix(math.radians(angle), [0, 0, 1]))
        m.apply_translation([0, 0, dz])
        v = overlap(gimbal, m)
        assert v < 1e-5, ('rotor', angle, dz, v)
        rotation.append(v)
report['rotation'] = {'positions_checked': len(rotation), 'axial_offsets_mm': [-.6, 0, .6],
                      'maximum_intersection_mm3': max(rotation)}

insertion = []
for angle in range(0, 721, 15):
    m = gimbal.copy()
    m.apply_transform(trimesh.transformations.rotation_matrix(math.radians(angle), [0, 0, 1]))
    m.apply_translation([0, 0, 7+6*angle/360])
    v = overlap(base, m)
    assert v < 1e-5, ('screw', angle, v)
    insertion.append(v)
# Negative controls ensure the mesh sweep really detects incompatible mating.
wrong = gimbal.copy()
wrong.apply_transform(trimesh.transformations.rotation_matrix(math.pi, [0, 0, 1]))
wrong.apply_translation([0, 0, 7])
wrong_phase = overlap(base, wrong)
shifted = gimbal.copy()
shifted.apply_translation([1.5, 0, 7])
shift_overlap = overlap(base, shifted)
assert wrong_phase > 10 and shift_overlap > 10, (wrong_phase, shift_overlap)
report['thread_fit'] = {'positions_checked': len(insertion), 'travel_mm': 12,
                        'maximum_intersection_mm3': max(insertion),
                        'wrong_phase_intersection_mm3': wrong_phase,
                        'lateral_offset_1_5mm_intersection_mm3': shift_overlap,
                        'physical_fit': 'Requires a printed test; geometric clearance only'}
original = trimesh.load_mesh(B.parent/'rotor.stl')
original.apply_translation([0, 0, 10])
# Geometry generation may retriangulate. Compare symmetric difference instead
# of requiring identical vertex ordering or triangles.
diff = abs(original.volume-rotor.volume)
common = trimesh.boolean.intersection([original, rotor], engine='manifold').volume
symmetric_difference = original.volume+rotor.volume-2*common
assert diff < .1 and abs(symmetric_difference) < .1, symmetric_difference
report['successful_rotor_preserved'] = {'translation_mm': 10, 'absolute_symmetric_difference_mm3': float(abs(symmetric_difference))}
trimesh.util.concatenate([gimbal, rotor]).export(B/'captive-assembly.stl')
parts = [base.copy(), gimbal.copy(), rotor.copy()]
for m in parts[1:]:
    m.apply_translation([0, 0, 7])
trimesh.util.concatenate(parts).export(B/'assembled.stl')
(B/'validation.json').write_text(json.dumps(report, indent=2)+'\n')
print(json.dumps(report, indent=2), flush=True)

# Render an assembled and separated view without hiding the thread behind
# supports. This is a design preview, not a simulation of the sliced supports.
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection

fig = plt.figure(figsize=(12, 7), facecolor='#f5f4ef')
for index, exploded in enumerate([False, True], 1):
    ax = fig.add_subplot(1, 2, index, projection='3d', facecolor='#f5f4ef')
    for n in ['base', 'gimbal', 'rotor']:
        m = meshes[n].copy()
        if n != 'base':
            m.apply_translation([0, 0, 7+(25 if exploded else 0)])
        colors = np.tile(np.array({'base': [.40, .43, .46], 'gimbal': [.74, .57, .29],
                                   'rotor': [.15, .43, .61]}[n]), (len(m.faces), 1))
        if n == 'rotor':
            local = meshes[n].triangles_center - [0, 0, 64]
            colors[np.linalg.norm(local, axis=1) > 32.2] = [.46, .65, .43]
        light = np.array([-.35, -.65, .7]); light /= np.linalg.norm(light)
        shade = .55+.45*np.maximum(0, m.face_normals@light)
        ax.add_collection3d(Poly3DCollection(m.triangles, facecolors=colors*shade[:, None],
                                           edgecolors='none', zsort='average'))
    ax.set(xlim=(-50, 50), ylim=(-50, 50), zlim=(0, 145 if exploded else 120))
    ax.set_box_aspect((100, 100, 145 if exploded else 120))
    ax.view_init(elev=17, azim=-64)
    ax.set_axis_off()
    ax.set_title('Assembled' if not exploded else 'Separated to show screw joint', fontsize=14)
fig.suptitle('Globe revision 2 · captive globe/gimbal + screw-on base', fontsize=17)
fig.text(.5, .045, '64 mm globe  |  90 mm base  |  24 mm coarse thread  |  thicker gimbal and broad foot', ha='center', fontsize=11)
fig.subplots_adjust(left=0, right=1, top=.90, bottom=.08, wspace=0)
fig.savefig(B/'preview.png', dpi=160, facecolor=fig.get_facecolor())
