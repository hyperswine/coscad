"""Beach-scale monohull with sealed buoyancy chambers: CoScad generation only, no printer
communication.

The bath-size open monohull (../twin-boat-trials) sank in surf: solid PLA is denser than
water and it had no sealed air, so a wave over its 12.7 mm freeboard swamped it. This one
keeps an open cockpit but surrounds it with sealed tanks, split by bulkheads into separate
chambers, so it floats upright when swamped and with any one chamber leaking.

Every tank closes at the top by two walls that meet, never by a flat roof, so nothing
bridges open air inside a chamber. The cockpit wall leans out at COCKPIT_LEAN from
vertical; it is the overhang over each tank.
"""
from pathlib import Path
import json, math, subprocess
import manifold3d as md
B = Path(__file__).resolve().parent
ROOT = B.parents[1]
NAME = 'beach-monohull'

T = 3.0             # hull wall thickness, mm (outer skin takes the knocks)
TC = 2.4            # cockpit wall thickness, mm (inner skin of the tanks)
TF = 4.0            # floor thickness, mm (solid, low: lowers the centre of gravity)
H = 64.0            # hull height to the gunwale, mm
COCKPIT_LEAN = 38.0 # cockpit wall lean from vertical, degrees
RIM = 3.0           # gunwale rim: hull outline to cockpit edge at the top, mm
BULKHEADS_X = [-60, -20, 20, 60]   # transverse bulkheads, mm from midships
TB = 2.4            # bulkhead thickness, mm

# Outer hull stations (z, L, W): the bath monohull's form at about 2x, with a deeper bilge
# and higher sides. Exterior overhang is at most about 44 degrees from vertical.
HULL = [(0, 186, 44), (6, 195, 52), (34, 220, 106), (H, 236, 124)]

def n(v): return f'{v:.4f}'.rstrip('0').rstrip('.') if abs(v) > 1e-9 else '0'
def outline_points(l, w):
    """Plan outline: pointed bow at +x, rounded transom at -x; 3k+1 cubic bezier control points."""
    return [(-.5*l,-.30*w),(-.5*l,-.46*w),(-.45*l,-.5*w),(-.35*l,-.5*w),(-.2*l,-.5*w),(0,-.5*w),(.15*l,-.5*w),
            (.35*l,-.48*w),(.48*l,-.18*w),(.5*l,0),(.48*l,.18*w),(.35*l,.48*w),(.15*l,.5*w),(0,.5*w),(-.2*l,.5*w),
            (-.35*l,.5*w),(-.45*l,.5*w),(-.5*l,.46*w),(-.5*l,.3*w),(-.5*l,.1*w),(-.5*l,-.1*w),(-.5*l,-.30*w)]
def outline(l, w): return '✎ ' + ' '.join(n(a)+' '+n(b) for a, b in outline_points(l, w))
def bezier_sample(cps, samples=24):
    """The compiler's own evaluation of a ✎ outline (bezPoly 24)."""
    pts = []
    for i in range(0, len(cps)-3, 3):
        (x0,y0),(x1,y1),(x2,y2),(x3,y3) = cps[i:i+4]
        for j in range(samples):
            t = j/samples; u = 1-t
            pts.append((u*u*u*x0+3*u*u*t*x1+3*u*t*t*x2+t*t*t*x3, u*u*u*y0+3*u*u*t*y1+3*u*t*t*y2+t*t*t*y3))
    return pts
def offset_outline(l, w, h):
    """The plan outline moved inward by h (outward if h < 0): a Clipper miter offset of the
    sampled bezier, emitted as a ✎ path of straight cubic segments. A scaled outline is not
    an offset; at the pointed bow it would leave the wall thin."""
    cs = md.CrossSection([bezier_sample(outline_points(l, w))]).offset(-h, md.JoinType.Miter, 2.0)
    if hasattr(cs, 'simplify'): cs = cs.simplify(0.05)
    polys = cs.to_polygons()
    assert polys, ('offset collapses', l, w, h)
    poly = [tuple(map(float, p)) for p in max(polys, key=len)]
    cps = []
    for (ax, ay), (bx, by) in zip(poly, poly[1:] + poly[:1]):
        cps += [(ax, ay), (ax+(bx-ax)/3, ay+(by-ay)/3), (ax+2*(bx-ax)/3, ay+2*(by-ay)/3)]
    cps.append(poly[0])
    return '✎ ' + ' '.join(n(a)+' '+n(b) for a, b in cps)
def ring(z, shape): return f'(ζ {n(z)} (⮕ 0.02 ({shape})))'
def loft(rings): return '(' + ' ⇓ '.join(rings) + ')'   # convex hull of thin rings
def box(x, y, z, l, w, h): return f'(χ {n(x-l/2)} (ψ {n(y-w/2)} (ζ {n(z)} (▬ {n(l)} {n(w)} {n(h)}))))'

def lean(a, b):   # steepest lean from vertical of the hull surface between two stations
    (z0, l0, w0), (z1, l1, w1) = a, b
    return math.atan(max(abs(l1-l0), abs(w1-w0)) / 2 / (z1-z0))
leans = [lean(a, b) for a, b in zip(HULL, HULL[1:])]
assert max(math.degrees(x) for x in leans) <= 45, [math.degrees(x) for x in leans]

# Outer skin and its inner face (each station inset by T measured normal to the surface).
outer = loft([ring(z, outline(l, w)) for z, l, w in HULL])
def hull_at(z):
    for (z0,l0,w0),(z1,l1,w1) in zip(HULL, HULL[1:]):
        if z0 <= z <= z1: f = (z-z0)/(z1-z0); return l0+f*(l1-l0), w0+f*(w1-w0)
    raise ValueError(z)
inner_rings = [(TF, *hull_at(TF), T/math.cos(leans[0]))]
for i, (z, l, w) in enumerate(HULL[1:], 1):
    inner_rings.append((z, l, w, T/math.cos(max(leans[i-1], leans[min(i, len(leans)-1)]))))
inner = loft([ring(z, offset_outline(l, w, h)) for z, l, w, h in inner_rings])

# Cockpit: the top hull outline offset inward by RIM at the gunwale, growing inward by
# tan(lean) per mm downward, so its wall leans COCKPIT_LEAN everywhere, bow and stern too.
LT, WT = HULL[-1][1:]
k = math.tan(math.radians(COCKPIT_LEAN))
ZTOP = H + 10   # the cockpit void runs out through the deck
def cockpit_inset(z): return RIM + (H - z) * k
cockpit = loft([ring(TF, offset_outline(LT, WT, cockpit_inset(TF))), ring(ZTOP, offset_outline(LT, WT, cockpit_inset(ZTOP)))])
tc = TC / math.cos(math.radians(COCKPIT_LEAN))   # horizontal thickness of the leaning wall
# The cockpit wall reaches below the floor so no tank has a flat ceiling over the cockpit floor.
cockpit_wall = loft([ring(-1, offset_outline(LT, WT, cockpit_inset(-1) - tc)), ring(ZTOP, offset_outline(LT, WT, cockpit_inset(ZTOP) - tc))])
floor_w = WT - 2*cockpit_inset(TF); floor_l = LT - 2*cockpit_inset(TF)
assert floor_w >= 20, floor_w

# Tanks: inside the skin, outside the cockpit wall, below the deck; bulkheads split them.
bulkheads = ' ⊕ '.join([box(x, 0, -2, TB, 200, H+20) for x in BULKHEADS_X] +
                       # centreline bulkheads split the bow and stern tanks into port and starboard
                       [box((BULKHEADS_X[-1] + 125)/2, 0, -2, 125 - BULKHEADS_X[-1], TB, H+20),
                        box((BULKHEADS_X[0] - 125)/2, 0, -2, 125 + BULKHEADS_X[0], TB, H+20)])
tanks = f'(({inner} ⊖ {cockpit_wall}) ⊖ ({bulkheads}))'

header = ('// Beach-scale monohull with sealed buoyancy tanks, mm. Open cockpit; the tanks close\n'
          '// by converging walls (no flat roofs), so it prints without supports.\n')
(B/(NAME+'.coscad')).write_text(header + f'outer = {outer}\ncockpit = {cockpit}\ntanks = {tanks}\nmain = (outer ⊖ cockpit) ⊖ tanks\n')
(B/(NAME+'-envelope.coscad')).write_text(header + f'main = {outer}\n')                       # dry cockpit
(B/(NAME+'-sealed.coscad')).write_text(header + f'main = ({outer} ⊖ {cockpit})\n')          # swamped cockpit

info = {'units': 'mm', 'name': NAME, 'hull_stations': HULL, 'inner_rings': inner_rings,
        'wall_mm': T, 'cockpit_wall_mm': TC, 'floor_mm': TF, 'height_mm': H,
        'cockpit_lean_deg': COCKPIT_LEAN, 'rim_mm': RIM, 'cockpit_floor_mm': [floor_l, floor_w],
        'cockpit_top_mm': [LT - 2*RIM, WT - 2*RIM], 'bulkheads_x': BULKHEADS_X, 'bulkhead_mm': TB,
        'hull_lean_deg': [math.degrees(x) for x in leans],
        'footprint_mm': [LT, WT],
        'aim': 'Beach try-out: open-cockpit monohull that floats upright when swamped; sealed tanks in separate chambers'}
(B/'design.json').write_text(json.dumps(info, indent=2) + '\n')
print(json.dumps({k: info[k] for k in ['footprint_mm', 'height_mm', 'cockpit_floor_mm', 'cockpit_top_mm', 'hull_lean_deg']}, indent=1))
