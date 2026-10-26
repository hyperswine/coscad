"""Generate the two-piece revision entirely in CoScad, including the helix.

The original globe/land definitions are retained; no external map or thread
library is needed. Dimensions and thread phase are shared by both mating parts.
"""
from pathlib import Path
import hashlib
import json
import math
import subprocess

B = Path(__file__).resolve().parent
ROOT = B.parents[2]
PITCH = 6.0
STEP = 7.5


def point(r, angle, z):
    a = math.radians(angle)
    return f'χ {r*math.cos(a):.6f} (ψ {r*math.sin(a):.6f} (ζ {z:.6f} (point)))'


def helix(lines, name, clearance=0, axial=0):
    # Trapezoid: 20 mm root, 24 mm crest, 45-degree flanks, 0.4 mm crest.
    # Sections below/above the part are intentional: clipping fixes both ends
    # without changing the shared screw phase. Tiny overlapping cubes make
    # adjacent hulls volumetrically connected rather than merely coplanar.
    names = []
    for i in range(int(-360/STEP), int(1080/STEP)):
        corners = []
        for a in (i*STEP, (i+1)*STEP):
            z = PITCH*a/360
            for r, dz in [(9.95+clearance, -2.2-axial),
                          (12+clearance, -.2-axial),
                          (12+clearance, .2+axial),
                          (9.95+clearance, 2.2+axial)]:
                corners.append(point(r, a, z+dz))
        n = f'{name}_{len(names)}'
        lines.append(n+' = '+' ⇓ '.join(corners))
        names.append(n)
    lines.append(name+' = '+' ⊕ '.join(names))


def main():
    compiler = subprocess.check_output(['cabal', 'list-bin', 'exe:coscad'], cwd=ROOT, text=True).strip()
    original = (B.parent/'globe.coscad').read_text()
    # Retain the successful spherical relief, bearing bore and thrust geometry.
    geography = original[original.index('ocean ='):original.index('// Support exclusion')]
    geography = '\n'.join(s for s in geography.splitlines() if not s.startswith('point ='))
    lines = [
        '// Revision 2: screw-on base; captive globe and strengthened gimbal.',
        '// All printable shapes, including coarse threads, are CoScad solids.',
        'radius = 32', 'center_z = 64', 'pin_r = 2.5', 'bore_r = 3.1',
        'point = χ -0.005 (ψ -0.005 (ζ -0.005 (▬ 0.01 0.01 0.01)))',
    ]
    helix(lines, 'male_rib')
    helix(lines, 'female_rib', clearance=.4, axial=.25)
    lines += [
        'male_core = ◎ 10 12',
        'male_envelope = ((◎ 12.1 9) ⊕ ((ζ 9 (◎ 12.1 0.01)) ⇓ (ζ 12 (◎ 9.1 0.01))))',
        'male = (male_core ⊕ male_rib) ∩ male_envelope',
        'base_disk = (◎ 45 3) ⇓ (ζ 6.99 (◎ 42 0.01))',
        'base = (ζ 6.5 (◎ 10 1)) ⊕ base_disk ⊕ (ζ 6.8 (◎ 18 0.2)) ⊕ (ζ 7 male)',
        '// Socket has an entry chamfer and a 45-degree conical roof, not a bridge.',
        'socket_core = ζ -0.2 (◎ 10.4 13.2)',
        'socket_rib = female_rib ∩ (ζ -0.2 (◎ 13 13.2))',
        'socket_entry = (ζ -0.1 (◎ 13.2 0.01)) ⇓ (ζ 2.8 (◎ 10.4 0.01))',
        'socket_roof = (ζ 12.99 (◎ 12.4 0.01)) ⇓ (ζ 25.4 (◎ 0.01 0.01))',
        'socket = socket_core ⊕ socket_rib ⊕ socket_entry ⊕ socket_roof',
        'foot = ((◎ 18 12) ⊕ ((ζ 11.9 (◎ 18 0.1)) ⇓ (ζ 28.9 (◎ 9 0.1)))) ⊖ socket',
        '// Ring cross section grows from 3.5 x 6 to 5.5 x 10 mm.',
        'meridian = ζ center_z (θ 90 (ζ -5 ((◎ 41.5 10) ⊖ (ζ -0.1 (◎ 36 10.2)))))',
        'shaft = ζ 26 (◎ pin_r 78)',
        'lower_bearing = ζ 28 (◎ 4.5 6.4)',
        'upper_bearing = (ζ 96.6 (◎ 2.5 0.1)) ⇓ (ζ 98.6 (◎ 4.5 3.5))',
        'gimbal = foot ⊕ meridian ⊕ shaft ⊕ lower_bearing ⊕ upper_bearing',
        geography,
        '// Block support in both mating threads and the complete moving bore.',
        'bearing_blocker = ζ 30.5 (◎ 5.5 69)',
        'socket_blocker = ◎ 13.3 25.5',
        'support_blocker = bearing_blocker ⊕ socket_blocker',
        'coupon_female = (◎ 18 12) ⊖ socket',
    ]
    source = '\n'.join(lines)+'\n'
    outputs = {
        'gimbal': 'gimbal', 'rotor': 'rotor', 'base': 'base',
        'support-blocker': 'support_blocker',
        'assembly': '(ζ 7 (gimbal ⊕ rotor)) ⊕ base',
        'layout': '(χ -50 (gimbal ⊕ rotor)) ⊕ (χ 50 base)',
        'coupon-female': 'coupon_female',
        'coupon-male': '(◎ 18 2) ⊕ (ζ 2 male)',
    }
    for name, shape in outputs.items():
        src = B/(name+'.coscad')
        src.write_text(source+'main = '+shape+'\n')
        out = B/(name+'.scad')
        subprocess.run([compiler, str(src), '-o', str(out)], check=True, stdout=subprocess.DEVNULL)
        out.write_text(out.read_text().replace('$fn = 50;', '$fn = 96;'))
        if name != 'assembly':
            src.unlink()
    design = {
        'revision': 2, 'sphere_diameter_mm': 64, 'relief_mm': .8,
        'base_diameter_mm': 90, 'assembled_height_mm': 112.5,
        'gimbal_ring_radial_thickness_mm': 5.5, 'gimbal_ring_depth_mm': 10,
        'foot_diameter_mm': 36, 'thread': {'hand': 'right', 'major_diameter_mm': 24,
            'nominal_core_diameter_mm': 20, 'pitch_mm': PITCH, 'engagement_mm': 12,
            'female_radial_enlargement_mm': .4, 'female_axial_flank_enlargement_mm': .25,
            'segments_per_turn': 360/STEP, 'male_lead_in_mm': 3},
        'globe_center_z_print_mm': 64, 'assembly_seat_z_mm': 7,
        'bearing_blocker_z_mm': [30.5, 99.5], 'bearing_blocker_radius_mm': 5.5,
        'rotor_translation_from_original_mm': 10,
        'support': 'automatic tree; build plate only; socket and bearing blocked',
        'layout_centers_mm': {'gimbal': [78, 128], 'base': [178, 128]},
        'original_source_sha256': hashlib.sha256(original.encode()).hexdigest(),
        'physical_fit': 'Unprinted revision; printed thread coupon recommended first',
    }
    (B/'design.json').write_text(json.dumps(design, indent=2)+'\n')
    print('Generated threaded base, captive assembly, support blockers and fit coupons')


if __name__ == '__main__':
    main()
