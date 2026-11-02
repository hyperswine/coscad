"""Two-hand spinning top: CoScad generation only, no printer communication.

Spun like a fire drill: the stem is rubbed between both palms, then the top is let go
onto the floor. Two parts on one A1 plate:

  body  printed upside down, its flat top face on the plate. In that orientation it is a
        flat web, a deep rim ring and a central post tapering to the tip, all growing
        upward, so nothing overhangs. Most of the mass is in the rim, low and wide: a
        high spin inertia and a low centre of gravity, which keep it upright at low speed.
  stem  a 12-sided grip with a hex peg; it plugs into the hex socket in the body's top.
        Printed standing on its flat end.

Optional ballast: twelve pockets in the underside of the rim take M8 hex nuts (glued),
which roughly double the spin inertia. All dimensions in mm, in use (tip at z = 0).
"""
from pathlib import Path
import json, math
B = Path(__file__).resolve().parent

R = 80.0            # rim outer radius
RIM_IN = 60.0       # rim inner radius
Z_RIM = 20.0        # rim underside: the rim edge touches the floor only past about 15 degrees of tilt
Z_TOP = 44.0        # flat top face (on the plate when printing)
WEB = 5.0           # web thickness under the top face
CHAMFER = 3.0       # rim edge chamfers
POST_R = 14.0       # post radius under the web
TIP_R = 2.5         # rounded tip
HEX_AF = 12.0       # stem peg, across flats
HEX_CLEAR = 0.25    # socket clearance on each flat
SOCKET = 15.0       # socket depth from the top face
NUT_AF, NUT_H, NUT_N, NUT_R = 13.0, 6.5, 12, 70.0   # M8 nut pockets: across flats, depth, count, radius
NUT_CLEAR = 0.2
GRIP_R, GRIP_L, GRIP_SIDES = 8.0, 90.0, 12           # stem grip: circumradius, length, facets

def n(v): return f'{v:.4f}'.rstrip('0').rstrip('.') if abs(v) > 1e-9 else '0'
def disc(z, r): return f'(ζ {n(z)} (◎ {n(r)} 0.02))'
def hull(*parts): return '(' + ' ⇓ '.join(parts) + ')'
def hexprism(af, h): return f'(⎏ 6 {n(af/math.sqrt(3))} {n(h)})'   # circumradius from across-flats

z_web = Z_TOP - WEB
rim = hull(disc(Z_RIM, R - CHAMFER), disc(Z_RIM + CHAMFER, R), disc(Z_TOP - 1.0 - 0.02, R), disc(Z_TOP - 0.02, R - 1.0))
rim_hole = f'(ζ {n(Z_RIM - 1)} (◎ {n(RIM_IN)} {n(z_web - Z_RIM + 1)}))'
web = f'(ζ {n(z_web)} (◎ {n(RIM_IN + 1)} {n(WEB)}))'
post = hull(f'(ζ {n(TIP_R)} (● {n(TIP_R)}))', disc(z_web, POST_R))
# The socket ends in a point (a 45 degree roof when printed upside down), not a flat bridge.
sock_c = (HEX_AF + 2*HEX_CLEAR) / math.sqrt(3)
socket = hull(f'(ζ {n(Z_TOP - SOCKET)} {hexprism(HEX_AF + 2*HEX_CLEAR, SOCKET + 1)})', disc(Z_TOP - SOCKET - sock_c, 0.3))
nut_c = (NUT_AF + 2*NUT_CLEAR) / math.sqrt(3)
pockets = ' ⊕ '.join(f'(ω {n(360*i/NUT_N)} (χ {n(NUT_R)} (ζ {n(Z_RIM - 1)} {hexprism(NUT_AF + 2*NUT_CLEAR, NUT_H + 1)})))' for i in range(NUT_N))
body = f'((({rim} ⊖ {rim_hole}) ⊕ {web} ⊕ {post}) ⊖ {socket}) ⊖ ({pockets})'

# Stem, in use: peg z 0..SOCKET-0.5 below the shoulder, grip above, rounded top end.
peg = f'{hexprism(HEX_AF, SOCKET - 0.5)}'
grip = f'(ζ {n(SOCKET - 0.5)} (⎏ {GRIP_SIDES} {n(GRIP_R)} {n(GRIP_L)}))'
stem = f'{peg} ⊕ {grip}'

header = '// Two-hand spinning top, mm. Tip at z = 0; the body prints upside down.\n'
(B/'body.coscad').write_text(header + f'main = {body}\n')
(B/'stem.coscad').write_text(header + f'main = {stem}\n')
info = {'units': 'mm', 'rim_radius': R, 'rim_inner_radius': RIM_IN, 'rim_underside_z': Z_RIM, 'top_z': Z_TOP, 'web': WEB,
        'post_radius': POST_R, 'tip_radius': TIP_R, 'hex_af': HEX_AF, 'socket_depth': SOCKET,
        'nut_pockets': {'count': NUT_N, 'radius': NUT_R, 'across_flats': NUT_AF, 'depth': NUT_H},
        'stem': {'grip_circumradius': GRIP_R, 'grip_length': GRIP_L, 'facets': GRIP_SIDES, 'peg_length': SOCKET - 0.5},
        'tilt_before_rim_touches_deg': math.degrees(math.atan2(Z_RIM, R - CHAMFER))}
(B/'design.json').write_text(json.dumps(info, indent=2) + '\n')
print(json.dumps(info, indent=1))
