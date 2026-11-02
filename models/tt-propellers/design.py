"""Water propellers for a TT gearmotor (the yellow 1:48 hobby gearmotor, about 150 to 250
rpm on 3 to 6 V): CoScad generation only, no printer communication.

A TT motor turns slowly with torque to spare, so these are large for their size and
steeply pitched (pitch 1.2 to 1.6 times the diameter). Four designs to compare in a bucket
or on the boats, plus a fit coupon for the shaft:

  P2-50   2 blades, 50 mm, pitch 75     the simplest
  P3-50   3 blades, 50 mm, pitch 60     smoother, moderate pitch
  P3-60   3 blades, 60 mm, pitch 90     wide blades: the most thrust, needs the most depth
  P4-40   4 blades, 40 mm, pitch 60     compact, for shallow water under the hulls

Blades: at each radius a thin section, chord c(r) and thickness t(r), set at the helix
angle atan(pitch / (2 pi r)); neighbouring sections are hulled pairwise, which makes a
twisted blade without a sweep primitive. Right-handed: seen from behind (from the motor)
they push water away when turning clockwise; swap the motor leads to reverse.

Shaft (TT gearmotor output): 5.4 mm round with two flats 3.7 mm apart, about 9 mm long,
with a small centre hole for a retaining screw. Makers differ by a few tenths of a mm,
so the coupon has the double-D bore at three clearances. Dimensions in mm; the hub axis
is z, and each part prints standing on its hub's flat end.
"""
from pathlib import Path
import json, math
B = Path(__file__).resolve().parent

SHAFT_D, SHAFT_FLATS = 5.4, 3.7
CLEAR = 0.15            # radial clearance used on the propellers (the coupon tries 0.10, 0.15, 0.25)
HUB_R, HUB_L = 6.0, 10.0
DESIGNS = {
    'P2-50': dict(blades=2, D=50, pitch=75, chord=18, t_root=2.6, t_tip=1.2),
    'P3-50': dict(blades=3, D=50, pitch=60, chord=15, t_root=2.4, t_tip=1.1),
    'P3-60': dict(blades=3, D=60, pitch=90, chord=20, t_root=2.8, t_tip=1.2),
    'P4-40': dict(blades=4, D=40, pitch=60, chord=12, t_root=2.2, t_tip=1.0),
}
STATIONS = 9

def n(v): return f'{v:.4f}'.rstrip('0').rstrip('.') if abs(v) > 1e-9 else '0'
def chord_at(s, c):          # s = 0 at the root, 1 at the tip
    return c*(0.75 + 0.25*math.sin(math.pi*s))*(1 - 0.45*s**4)
def section(r, c, t, beta):
    """Thin section at radius r on the +x blade: chord along y (the rotation direction),
    thickness along z, then pitched by beta about the radial axis."""
    return f'(χ {n(r)} (θ {n(math.degrees(beta))} (box 0.05 {n(c)} {n(t)})))'
def blade(p):
    R = p['D']/2; r0 = HUB_R - 0.8; out = []; info = []
    for i in range(STATIONS):
        s = i/(STATIONS - 1); r = r0 + s*(R - r0)
        c = chord_at(s, p['chord']); t = p['t_root'] + s*(p['t_tip'] - p['t_root'])
        beta = math.atan2(p['pitch'], 2*math.pi*r)
        out.append(section(r, c, t, beta)); info.append({'r': r, 'chord': c, 'thickness': t, 'beta_deg': math.degrees(beta)})
    hulls = [f'({a} ⇓ {b})' for a, b in zip(out, out[1:])]
    return '(' + ' ⊕ '.join(hulls) + ')', info
def bore(clear, hub_l=None):
    hub_l = hub_l or HUB_L
    """Double-D bore through the hub along z, with clearance on the round and the flats."""
    rr = SHAFT_D/2 + clear; flats = SHAFT_FLATS + 2*clear
    return f'((zcyl {n(rr)} {n(hub_l + 4)}) ∩ (box {n(flats)} {n(2*rr + 2)} {n(hub_l + 4)}))'

def hub(hub_l=None):
    hub_l = hub_l or HUB_L
    return f'(zcyl {n(HUB_R)} {n(hub_l)} |> move 0 0 {n(hub_l/2)})'

meta = {}
for name, p in DESIGNS.items():
    b, info = blade(p)
    blades = ' ⊕ '.join(f'(ω {n(360*k/p["blades"])} {b})' for k in range(p['blades']))
    # The hub is at least as long as the blade roots are deep, so no blade reaches below
    # the hub's end (the print plate) or above its top.
    max_axial = max(st['chord']*math.sin(math.radians(st['beta_deg'])) + st['thickness'] for st in info)
    hub_l = max(HUB_L, math.ceil(max_axial + 1.5))
    # Blades centred on the hub's mid-height; the bore runs through.
    body = f'(({hub(hub_l)}) ⊕ (ζ {n(hub_l/2)} ({blades}))) ⊖ (ζ {n(hub_l/2)} {bore(CLEAR, hub_l)})'
    (B/f'{name}.coscad').write_text(f'// {name}: {p["blades"]} blades, {p["D"]} mm, pitch {p["pitch"]} mm, TT gearmotor double-D bore.\nmain = {body}\n')
    meta[name] = {**p, 'stations': info, 'blade_axial_extent_mm': max_axial, 'hub': {'radius': HUB_R, 'length': hub_l}, 'bore_clearance': CLEAR}

# Fit coupon: three hub rings with the double-D bore at three clearances, marked with
# one, two and three notches on the rim.
rings = []
for k, clear in enumerate([0.10, 0.15, 0.25]):
    x = 16*k
    notches = ' ⊕ '.join(f'(box 1.2 2 4 |> move {n(HUB_R)} {n(-2.5 + 2.5*j)} {n(HUB_L - 2)})' for j in range(k + 1))
    rings.append(f'(χ {n(x)} ((({hub()}) ⊖ (ζ {n(HUB_L/2)} {bore(clear)})) ⊖ ({notches})))')
(B/'shaft-coupon.coscad').write_text('// TT shaft fit coupon: double-D bore at 0.10, 0.15, 0.25 mm clearance (1, 2, 3 notches).\nmain = ' + ' ⊕ '.join(rings) + '\n')
meta['coupon'] = {'clearances': [0.10, 0.15, 0.25], 'notches': [1, 2, 3]}
(B/'design.json').write_text(json.dumps({'shaft': {'diameter': SHAFT_D, 'flats': SHAFT_FLATS}, 'designs': meta}, indent=2) + '\n')
for k, v in meta.items():
    if k != 'coupon': print(k, f"blade angle {v['stations'][0]['beta_deg']:.0f} deg at the root, {v['stations'][-1]['beta_deg']:.0f} deg at the tip, axial extent {v['blade_axial_extent_mm']:.1f} mm, hub {v['hub']['length']} mm")
