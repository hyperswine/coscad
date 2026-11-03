"""Tighter stems for the two-hand top that fit the body already printed (body.coscad is
unchanged). CoScad generation only, no printer communication.

The first stem (stem.coscad) had 0.25 mm clearance on each hex flat. It dropped in too
easily and the top spun poorly with it fitted, while the body alone spun well. Over its
14.5 mm of engagement that play let the 90 mm grip lean about 2 degrees.

These stems change two things:
  fit   the hex peg is sized for 0.15, 0.10 or 0.05 mm per flat in the existing socket
        (12.5 mm across flats). They are marked with 1, 2 and 3 grooves at the grip's top end.
  seat  the socket ends in a pointed hex roof. The peg now carries on into that roof and
        its end is the roof's own shape, so the stem rests on the roof faces and centres
        itself at the bottom as well as at the mouth. Engagement grows from 14.5 to about
        20 mm. The peg's last 1 mm is cut off so it never bottoms out on the roof's point,
        and the shoulder stays 0.3 mm above the body's top face.

Dimensions in mm, in use (tip at z = 0), the same frame as design.py.
"""
from pathlib import Path
import json, math
B = Path(__file__).resolve().parent
d = json.loads((B/'design.json').read_text())

Z_TOP, SOCKET = d['top_z'], d['socket_depth']
SOCKET_AF = d['hex_af'] + 2*0.25          # the printed body's socket, across flats
SOCK_C = SOCKET_AF/math.sqrt(3)           # its circumradius, and the roof's depth below the prism
APEX = Z_TOP - SOCKET - SOCK_C            # the roof's point
FITS = [0.15, 0.10, 0.05]                 # clearance per flat
SHOULDER_GAP = 0.3
TIP_CUT = 1.0
GRIP_R, GRIP_L, GRIP_SIDES = d['stem']['grip_circumradius'], d['stem']['grip_length'], d['stem']['facets']

def n(v): return f'{v:.4f}'.rstrip('0').rstrip('.') if abs(v) > 1e-9 else '0'
def hexprism(af, h): return f'(⎏ 6 {n(af/math.sqrt(3))} {n(h)})'

# The socket's shape, exactly as design.py cuts it (prism plus the hull down to the point).
socket = f'((ζ {n(Z_TOP - SOCKET)} {hexprism(SOCKET_AF, SOCKET + 1)}) ⇓ (ζ {n(APEX)} (◎ 0.3 0.02)))'

meta = {}
for k, clear in enumerate(FITS):
    af = SOCKET_AF - 2*clear
    z0 = APEX + TIP_CUT; z1 = Z_TOP + SHOULDER_GAP
    # Peg: its own hex prism clipped by the socket's shape, so its end is the roof itself.
    peg = f'((ζ {n(z0)} {hexprism(af, z1 - z0)}) ∩ {socket})'
    grip = f'(ζ {n(z1)} (⎏ {GRIP_SIDES} {n(GRIP_R)} {n(GRIP_L)}))'
    inr = GRIP_R*math.cos(math.pi/GRIP_SIDES)
    grooves = ' ⊕ '.join(f'(ω {n(15 + 30*j)} (box 1.6 1.2 8 |> move {n(inr)} 0 {n(z1 + GRIP_L - 4 + 0.01)}))' for j in range(k + 1))
    name = f'stem-fit{int(round(clear*100)):02d}'
    (B/f'{name}.coscad').write_text(f'// Two-hand top stem, {clear:.2f} mm per hex flat, seats on the socket roof; {k + 1} groove(s). mm, tip at z = 0.\n'
                                    f'main = ({peg} ⊕ {grip}) ⊖ ({grooves})\n')
    meta[name] = {'clearance_per_flat': clear, 'peg_af': af, 'grooves': k + 1, 'peg_bottom_z': z0, 'shoulder_z': z1,
                  'engagement_mm': Z_TOP - z0, 'grip_top_z': z1 + GRIP_L}
# Mouse-ear pads for printing (owner preference over full brims). Bambu's CLI keeps native
# brim-ear settings but slices no ears (found on the tesseract A1 retry), so the pads are
# modelled: four 0.2 mm discs around the grip end, which stands on the plate. Print frame.
EAR_R, EAR_H, EAR_AT = 6.0, 0.2, GRIP_R + 2.0
ears = ' ⊕ '.join(f'(ω {a} (χ {n(EAR_AT)} (⚓ bot (◎ {n(EAR_R)} {n(EAR_H)}))))' for a in (0, 90, 180, 270))
(B/'stem-ears.coscad').write_text(f'// Four sacrificial {EAR_H} mm mouse-ear pads for a stem standing on its grip end (print frame).\nmain = {ears}\n')
info = {'socket_af': SOCKET_AF, 'socket_roof_apex_z': APEX, 'shoulder_gap': SHOULDER_GAP, 'tip_cut': TIP_CUT, 'ears': {'radius': EAR_R, 'height': EAR_H, 'centre_radius': EAR_AT, 'count': 4}, 'stems': meta}
(B/'stems.json').write_text(json.dumps(info, indent=2) + '\n')
print(json.dumps(info, indent=1))
