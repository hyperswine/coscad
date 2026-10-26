"""Beach-scale sealed outrigger: CoScad generation only, no printer communication.

Scaled 2.1x from twin-boat-trials/outrigger, but built as thin shells instead of a
solid: every hull is a closed air chamber with a self-supporting roof, so the boat
cannot be swamped. Fits one 256 x 256 mm plate (P1S or A1) with a brim.
"""
from pathlib import Path
import json, math, subprocess
import manifold3d as md
B = Path(__file__).resolve().parent
ROOT = B.parents[1]
S = 2.1            # geometric scale from the bath-size outrigger
T = 2.4            # side wall and roof thickness, mm (6 x 0.4 mm loops)
TF = 3.0           # floor thickness, mm
ROOF = 50.0        # roof slope from horizontal, degrees (overhang 40 deg from vertical)
PONTOON_Y = 38 * S # pontoon centrelines

def n(v): return f'{v:.4f}'.rstrip('0').rstrip('.') if abs(v) > 1e-9 else '0'
def outline(length, width):
    l, w = length, width
    pts = [(-.5*l,-.30*w),(-.5*l,-.46*w),(-.45*l,-.5*w),(-.35*l,-.5*w),(-.2*l,-.5*w),(0,-.5*w),(.15*l,-.5*w),(.35*l,-.48*w),(.48*l,-.18*w),(.5*l,0),(.48*l,.18*w),(.35*l,.48*w),(.15*l,.5*w),(0,.5*w),(-.2*l,.5*w),(-.35*l,.5*w),(-.45*l,.5*w),(-.5*l,.46*w),(-.5*l,.3*w),(-.5*l,.1*w),(-.5*l,-.1*w),(-.5*l,-.3*w)]
    return '✎ ' + ' '.join(n(a)+' '+n(b) for a, b in pts)
def outline_points(length, width):
    l, w = length, width
    return [(-.5*l,-.30*w),(-.5*l,-.46*w),(-.45*l,-.5*w),(-.35*l,-.5*w),(-.2*l,-.5*w),(0,-.5*w),(.15*l,-.5*w),(.35*l,-.48*w),(.48*l,-.18*w),(.5*l,0),(.48*l,.18*w),(.35*l,.48*w),(.15*l,.5*w),(0,.5*w),(-.2*l,.5*w),(-.35*l,.5*w),(-.45*l,.5*w),(-.5*l,.46*w),(-.5*l,.3*w),(-.5*l,.1*w),(-.5*l,-.1*w)]
def bezier_sample(cps, samples=24):
    """The compiler's own evaluation of a ✎ outline (bezPoly 24): 3k+1 control points, auto-closed."""
    pts = []
    for i in range(0, len(cps)-3, 3):
        (x0,y0),(x1,y1),(x2,y2),(x3,y3) = cps[i:i+4]
        for j in range(samples):
            t = j/samples; u = 1-t
            pts.append((u*u*u*x0+3*u*u*t*x1+3*u*t*t*x2+t*t*t*x3, u*u*u*y0+3*u*u*t*y1+3*u*t*t*y2+t*t*t*y3))
    return pts
def inset_outline(length, width, h):
    """The outer plan curve moved inward by h everywhere (Clipper miter offset of the sampled
    bezier), emitted as a ✎ path whose cubic segments are straight (collinear control points).
    None if the inset collapses."""
    cs = md.CrossSection([bezier_sample(outline_points(length, width))]).offset(-h, md.JoinType.Miter, 2.0)
    if hasattr(cs, 'simplify'): cs = cs.simplify(0.05)
    polys = cs.to_polygons()
    if not polys: return None
    poly = [tuple(map(float, p)) for p in max(polys, key=len)]
    cps = []
    for (ax, ay), (bx, by) in zip(poly, poly[1:] + poly[:1]):
        cps += [(ax, ay), (ax+(bx-ax)/3, ay+(by-ay)/3), (ax+2*(bx-ax)/3, ay+2*(by-ay)/3)]
    cps.append(poly[0])
    return '✎ ' + ' '.join(n(a)+' '+n(b) for a, b in cps)
def solid(rings):
    return ' ⇓ '.join(f'(ζ {n(z)} (⮕ 0.02 ({outline(l,w)})))' for z, l, w in rings)
def inset_solid(rings):
    """rings: (z, L, W, h) outer plan at z, inset horizontally by h."""
    parts = [(z, inset_outline(l, w, h)) for z, l, w, h in rings]
    return ' ⇓ '.join(f'(ζ {n(z)} (⮕ 0.02 ({o})))' for z, o in parts if o)
def box(x, y, z, l, w, h):
    return f'χ {n(x-l/2)} (ψ {n(y-w/2)} (ζ {n(z)} (▬ {n(l)} {n(w)} {n(h)})))'

def hull_stations(base, top_width, top_length):
    """base: scaled (z, L, W) stations up to the widest one; a roof at ROOF degrees
    closes the hull with a flat ridge top_width wide. Returns outer and inner lists."""
    zw, lw, ww = base[-1]
    rise = (ww - top_width) / 2 * math.tan(math.radians(ROOF))
    outer = list(base) + [(zw + rise, top_length, top_width)]
    def at(z):   # outer L, W at height z on the base polyline
        for (z0,l0,w0),(z1,l1,w1) in zip(base, base[1:]):
            if z0 <= z <= z1:
                f = (z-z0)/(z1-z0); return l0+f*(l1-l0), w0+f*(w1-w0)
        return base[-1][1:]
    def lean(z0, l0, w0, z1, l1, w1):   # steepest surface lean from vertical over a segment
        return math.atan(max(abs(l1-l0), abs(w1-w0)) / 2 / (z1-z0))
    def inset(phi): return T / math.cos(min(phi, math.radians(60)))
    segs = list(zip(outer, outer[1:]))
    leans = [lean(*a, *b) for a, b in segs]
    d = inset(math.radians(90-ROOF))                 # horizontal inset of a parallel roof
    delta = (d - T) * math.tan(math.radians(ROOF))   # roof/wall junction sits this far below zw
    lf, wf = at(TF)
    inner = [(TF, lf, wf, inset(leans[0]))]
    for i, (z, l, w) in enumerate(base[1:-1], 1):
        if TF < z < zw - delta: inner.append((z, l, w, inset(max(leans[i-1], leans[i]))))
    lj, wj = at(zw - delta)   # the outer hull is still flaring here; inset from its actual size
    inner.append((zw - delta, lj, wj, inset(leans[-2])))
    inner.append((zw + rise - T/math.cos(math.radians(ROOF)), top_length, top_width, d))
    return outer, inner

center_base = [(0, 80*S, 20*S), (8*S, 94*S, 30*S), (14*S, 100*S, 32*S)]
pontoon_base = [(0, 90*S, 8*S), (8*S, 103*S, 20*S), (12*S, 110*S, 22*S)]
center_outer, center_inner = hull_stations(center_base, 20, 190)
pontoon_outer, pontoon_inner = hull_stations(pontoon_base, 8, 210)

# Crossbeams: flat-topped bars whose underside is a 46 degree gable between the hulls,
# so no layer bridges open air. They sit where both hulls are near their widest.
# Each beam has the air chambers subtracted, so it cannot intrude into a sealed volume.
BEAM_Z, BEAM_TOP, BEAM_X_LEN = 25.0, 40.0, 24.0
BEAM_X = [-60, 15]      # inside the straight, full-width run of every hull's bezier outline (checked below)
def half_width_at_x(stations, z, x):
    """Half-width of the hull's plan outline at height z and station x, from the sampled bezier."""
    l = width_at([(a, b, c) for a, c, b in stations], z); w = width_at(stations, z)   # L and W at z
    pts = bezier_sample(outline_points(l, w)); best = 0.0
    for (x0, y0_), (x1, y1_) in zip(pts, pts[1:] + pts[:1]):
        if (x0 - x) * (x1 - x) <= 0 and x0 != x1:
            best = max(best, abs(y0_ + (x - x0) / (x1 - x0) * (y1_ - y0_)))
    return best
def width_at(stations, z):
    for (z0,l0,w0),(z1,l1,w1) in zip(stations, stations[1:]):
        if z0 <= z <= z1: return w0 + (z-z0)/(z1-z0)*(w1-w0)
    raise ValueError(z)
ya = width_at(center_outer, BEAM_Z) / 2                 # center hull outer surface at the beam root
yb = PONTOON_Y - width_at(pontoon_outer, BEAM_Z) / 2     # pontoon inner surface at the beam root
y0, y1 = ya - T - 0.5, yb + T + 0.5                      # reach into both walls
span = yb - ya; mid = (ya + yb) / 2; apex = BEAM_Z + span / 2
assert BEAM_TOP - apex >= 3, 'beam too thin over the gable apex'
beam_shapes, beam_raw = [], []
for x in BEAM_X:
    for side in [-1, 1]:
        b1 = box(x, side*(y0 + ya + 0.5)/2, BEAM_Z, BEAM_X_LEN, ya + 0.5 - y0, BEAM_TOP - BEAM_Z)
        b3 = box(x, side*(yb - 0.5 + y1)/2, BEAM_Z, BEAM_X_LEN, y1 - yb + 0.5, BEAM_TOP - BEAM_Z)
        b2 = box(x, side*mid, apex, BEAM_X_LEN, 1.0, BEAM_TOP - apex)
        raw = f'(({b1} ⇓ {b2}) ⊕ ({b2} ⇓ {b3}))'   # two hulled halves: a convex hull of all three would fill the gable
        beam_raw.append(raw)
        beam_shapes.append(f'(({raw} ⊖ center_void) ⊖ (ψ {n(side*PONTOON_Y)} pontoon_void))')
for x in BEAM_X:
    for xx in [x - BEAM_X_LEN/2, x + BEAM_X_LEN/2]:
        for stations in [center_outer, pontoon_outer]:
            full = width_at(stations, BEAM_Z) / 2
            assert half_width_at_x(stations, BEAM_Z, xx) > full - 0.3, ('beam root off the straight hull side', x, xx, half_width_at_x(stations, BEAM_Z, xx), full)
gable_deg = math.degrees(math.atan((apex - BEAM_Z) / (mid - 0.5 - ya - 0.5)))
assert gable_deg >= 45, gable_deg
beams = beam_shapes

defs = (f'center_outer = {solid(center_outer)}\ncenter_void = {inset_solid(center_inner)}\n'
        f'center_hull = center_outer ⊖ center_void\n'
        f'pontoon_outer = {solid(pontoon_outer)}\npontoon_void = {inset_solid(pontoon_inner)}\n'
        f'pontoon = pontoon_outer ⊖ pontoon_void')
main = f'center_hull ⊕ (ψ {n(-PONTOON_Y)} pontoon) ⊕ (ψ {n(PONTOON_Y)} pontoon) ⊕ ' + ' ⊕ '.join(beams)
header = '// Beach-scale sealed outrigger, mm. Three closed air chambers, no supports.\n'
(B/'beach-outrigger.coscad').write_text(header + defs + '\nmain = ' + main + '\n')
env = (solid(center_outer) + f' ⊕ (ψ {n(-PONTOON_Y)} ({solid(pontoon_outer)})) ⊕ (ψ {n(PONTOON_Y)} ({solid(pontoon_outer)})) ⊕ ' + ' ⊕ '.join(beam_raw))
(B/'beach-outrigger-envelope.coscad').write_text(header + '\nmain = ' + env + '\n')

compiler = subprocess.check_output(['cabal','list-bin','exe:coscad'], cwd=ROOT, text=True).strip()
for name in ['beach-outrigger', 'beach-outrigger-envelope']:
    subprocess.run([compiler, str(B/(name+'.coscad')), '-o', str(B/(name+'.scad'))], check=True)
info = {'units':'mm','scale_from_twin_boat_outrigger':S,'wall_mm':T,'floor_mm':TF,'roof_deg_from_horizontal':ROOF,
        'center_outer_stations':center_outer,'center_inner_stations':center_inner,'pontoon_outer_stations':pontoon_outer,
        'pontoon_inner_stations':pontoon_inner,'pontoon_centerlines_y':[-PONTOON_Y,PONTOON_Y],
        'beams':{'z':BEAM_Z,'top':BEAM_TOP,'x_positions':BEAM_X,'span_between_hulls_mm':span,'gable_deg_from_horizontal':gable_deg,'bridge_span_mm':0},
        'footprint_mm':[max(center_outer[0][1], pontoon_outer[-1][1], 110*S), 2*PONTOON_Y + pontoon_outer[2][2]],
        'height_mm':max(center_outer[-1][0], pontoon_outer[-1][0]),
        'aim':'Beach try-out: three sealed hulls, cannot be swamped; same plan form as the bath-size outrigger'}
(B/'design.json').write_text(json.dumps(info, indent=2) + '\n')
print(json.dumps({k:info[k] for k in ['footprint_mm','height_mm','beams']}, indent=1))
