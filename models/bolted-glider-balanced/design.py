"""Zero-ballast bolted glider revision. Generates CoScad, never contacts printers.
Five parts: swept wing panels, fuselage, ribbed stabilizer and lightweight fin.
Four steel M5x12 wing screws/nuts and two steel M3x10 tail screws/nuts.
Wing incidence +2.5 degrees; integral tail saddle defaults to -5.5 degrees.
Use --tail-incidence to regenerate a different trim datum, then rebuild and reslice.
Frames: x right, y aft, z up; wing root leading edge y=0, fuselage keel z=0.
The original design and packages remain in ../bolted-glider.
"""
from pathlib import Path
import json, math, argparse
parser = argparse.ArgumentParser()
parser.add_argument("--tail-incidence", type=float, default=-5.5, help="degrees; negative raises the tail trailing edge")
args = parser.parse_args()
assert -9 <= args.tail_incidence <= -2, "supported trim range: -9..-2 degrees"
TAIL_INCIDENCE = args.tail_incidence
B = Path(__file__).resolve().parent

# ---- wing ------------------------------------------------------------------
SWEEP = 172.0                 # aft sweep beyond the fixed root saddle, mm at tip
C0, C1 = 120.0, 66.0            # root and (unrounded) tip chord
T0, T1 = 0.085, 0.07            # thickness / chord at root and tip
ROOT_GAP = 2.0                  # each root edge 2 mm from the centreline (the roots lean in with dihedral)
FLANGE = 28.0                   # saddle flange width from the centreline
ROOT_FLAT = FLANGE + 3.0        # constant root chord over the saddle
TIP_ROUND = 30.0                # elliptical tip over the last 30 mm
DIHEDRAL = 6.0                  # degrees, each side
INCIDENCE = 2.5                 # wing chord to fuselage datum (the stab sits at 0)
SCREW_S = 13.0                  # screw station along the panel from its root edge
SCREW_U = (0.22, 0.62)          # screw positions as fractions of the root chord
CB_FLOOR = 4.0                  # counterbore floor height above the panel's flat bottom
HOLE_D, CB_D = 5.5, 10.5        # M5 clearance, button-head counterbore
MIN_TE = 0.9                    # thinnest trailing edge that prints (two lines)

# Clark Y, percent of chord, bottom flat from 30% chord aft (classic table).
CY_U = [0, 1.25, 2.5, 5, 7.5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100]
CY_UP = [3.50, 5.45, 6.50, 7.90, 8.85, 9.60, 10.69, 11.36, 11.70, 11.40, 10.52, 9.15, 7.35, 5.22, 2.80, 1.49, 0.12]
CY_LO = [3.50, 1.93, 1.47, 0.93, 0.63, 0.42, 0.15, 0.03, 0, 0, 0, 0, 0, 0, 0, 0, 0]

# ---- fuselage --------------------------------------------------------------
POD_W, BOOM_W = 14.0, 9.0       # slab width (x) at the pod and the boom
ZS = 44.0                       # top of the wing saddle on the centreline
FT = 7.0                        # saddle flange thickness
NUT_AF, NUT_H = 8.0, 4.0        # M5 hex nut
NOSE_Y, TAIL_Y = -119.0, 197.0  # keel 316 mm; with the tail platform, 323 mm: a plate diagonal
ZT = 19.0                       # tail platform top (stab underside)
PT, PW = 6.0, 18.0              # tail platform thickness and width
NOSE_W = 24.0                   # existing broad grip/nose profile; no ballast cavities
BALLAST = []                    # no ballast cavities or added nose weights

# ---- tail ------------------------------------------------------------------
SB, CS0, CS1 = 230.0, 72.0, 50.0    # stab span, root chord, tip chord
TS = 0.07                       # stab thickness / chord
YS0 = 161.0                     # stab root leading edge (its trailing edge overhangs the tail)
FOOT_W, FOOT_H = 16.0, 2.0      # fin foot width and height
CF0, CF1, HF, SWEEP_F = 64.0, 38.0, 95.0, 28.0   # fin root/tip chord, height, LE sweep
FIN_T = 2.2
TAIL_SCREWS = [(4.5, 10.0), (-4.5, -10.0)]       # (x, y from the tail platform's centre)

def n(v): return f'{v:.4f}'.rstrip('0').rstrip('.') if abs(v) > 1e-9 else '0'

def section(c, t):
    """Flat-bottomed Clark Y outline scaled to chord c and thickness ratio t, in (X aft, Y up),
    counter-clockwise from the leading edge along the flat bottom."""
    k = t/0.117
    # Printing flat: the bottom is truly flat back to a 45 degree chamfer under the nose
    # (Clark Y's lower surface rises gently over the front 30%, a near-flat overhang).
    le = CY_LO[0]/100*c*k
    lo = [(0.0, le), (le, 0.0), (0.3*c, 0.0), (c, 0.0)]
    up = [(u/100*c, y/100*c*k) for u, y in zip(CY_U, CY_UP)]
    up = [(x, y + MIN_TE*x/c) for x, y in up]                   # a linear ramp keeps the TE printable, no collinear runs
    lo = [lo[0], lo[1], lo[3]]
    pts = lo + [up[i] for i in range(len(up) - 1, 0, -1)]      # LE once, TE bottom and top
    return pts

def chord(s, LP):
    lin = C0 if s <= ROOT_FLAT else C0 + (C1 - C0)*(s - ROOT_FLAT)/(LP - ROOT_FLAT)
    r = s - (LP - TIP_ROUND)
    return lin*(max(0.25, math.sqrt(max(0.0, 1 - (r/TIP_ROUND)**2))) if r > 0 else 1.0)

def stations(LP):
    return [0.0, ROOT_FLAT, LP - TIP_ROUND, LP - 18, LP - 9, LP - 3, LP]

def sweep(s, LP): return SWEEP*max(0.0, s - ROOT_FLAT)/(LP - ROOT_FLAT)

def planform(LP):
    """Wing panel footprint in its print frame: x = chordwise, y = -span."""
    le, te = [], []
    for s in stations(LP):
        c = chord(s, LP); x0 = 0.25*(C0 - c) + sweep(s, LP)
        le.append((x0, -s)); te.append((x0 + c, -s))
    return le + te[::-1]

def fits(poly, side=256.0, pad=7.0, corner=None):
    """Best rotation of a convex footprint inside the plate (pad from the edges, clear of
    an excluded front-left corner). Returns (angle, centre) or None."""
    best = None
    for a10 in range(0, 900):
        a = math.radians(a10/10)
        pts = [(x*math.cos(a) - y*math.sin(a), x*math.sin(a) + y*math.cos(a)) for x, y in poly]
        xs = [p[0] for p in pts]; ys = [p[1] for p in pts]
        w, h = max(xs) - min(xs), max(ys) - min(ys)
        if w <= side - 2*pad and h <= side - 2*pad:
            cx = side/2 - (max(xs) + min(xs))/2; cy = side/2 - (max(ys) + min(ys))/2
            if corner and any(px + cx < corner[0] + pad and py + cy < corner[1] + pad for px, py in pts):
                continue
            slack = min(side - 2*pad - w, side - 2*pad - h)
            if best is None or slack > best[0]: best = (slack, a10/10, (cx, cy))
    return best

# Longest panel that fits a plate diagonal (with room for ear pads), in 1 mm steps.
def footprint(L): return planform(L) + [(0.25*C0 + SWEEP, -L - 8)]     # plus a tip ear pad
LP = 200.0
while fits(footprint(LP + 1), pad=7):
    LP += 1
EAR_R = 6.0

def wing_right():
    profs = []
    for s in stations(LP):
        c = chord(s, LP); t = T0 + (T1 - T0)*s/LP; x0 = 0.25*(C0 - c) + sweep(s, LP)
        pts = ' '.join(f'{n(x0 + x)} {n(y)}' for x, y in section(c, t))
        profs.append(f'{n(s)} (poly {pts})')
    # Loft along z = span, profile in (X chord, Y thickness); θ 90 lays it flat: x chord,
    # y = -span, z = thickness (flat bottom on the plate).
    loft = f'(θ 90 (loft {" ".join(profs)}))'
    holes = []
    for u in SCREW_U:
        x = u*C0
        holes.append(f'(zcyl {n(HOLE_D/2)} 40 |> move {n(x)} {n(-SCREW_S)} 0)')
        holes.append(f'(⚓ bot (zcyl {n(CB_D/2)} 20) |> move {n(x)} {n(-SCREW_S)} {n(CB_FLOOR)})')
    return f'{loft} ⊖ ({" ⊕ ".join(holes)})'

def root_thickness(u):
    sec = section(C0, T0)
    top = [p for p in sec if p[1] > 0.01 or p[0] == 0]
    top.sort()
    for (xa, ya), (xb, yb) in zip(top, top[1:]):
        if xa <= u*C0 <= xb: return ya + (yb - ya)*(u*C0 - xa)/(xb - xa)

# ---- fuselage ----------------------------------------------------------------
HW = POD_W/2
def station(y, h, w):
    return f'(box {n(w)} 0.5 {n(h)} |> move 0 {n(y)} {n(h/2)})'
# Broad nose/grip and narrow aft boom; continuous keel for support-free printing.
SIDE = [(NOSE_Y, 20, NOSE_W), (NOSE_Y + 6, 30, NOSE_W), (-78, 36, NOSE_W), (-48, 39, 18), (-20, ZS - 3, POD_W),
        (C0 + 4, ZS - 4 - (C0 + 4)*math.tan(math.radians(INCIDENCE)), POD_W), (C0 + 30, 26, 11.5), (YS0 - 2, ZT - 5, BOOM_W), (TAIL_Y + 7, ZT - 5, BOOM_W)]   # keel runs under the whole tail platform
pod = ' ⊕ '.join(f'({station(*a)} ⇓ {station(*b)})' for a, b in zip(SIDE, SIDE[1:]))

def to_use(shape):
    """Wing frame (x along the right panel from the centreline, y aft, z up from the saddle
    surface) to use frame: dihedral, then incidence, then up to the saddle top."""
    return f'(ζ {n(ZS)} (θ {n(-INCIDENCE)} (ϕ {n(-DIHEDRAL)} {shape})))'

def saddle_side():
    y0, y1 = 3.0, C0 - 6.0; L = y1 - y0
    # Both halves start 3 mm past the centreline so the mirrored pair overlaps (not a line contact).
    flange = f'(box {n(FLANGE + 3)} {n(L)} {n(FT)} |> move {n((FLANGE - 3)/2)} {n(y0 + L/2)} {n(-FT/2)})'
    drop = FLANGE - HW
    root = f'(box {n(HW + 3)} {n(L)} {n(drop)} |> move {n((HW - 3)/2)} {n(y0 + L/2)} {n(-FT - drop/2)})'
    nuts = []
    for u in SCREW_U:
        x, y = ROOT_GAP + SCREW_S, u*C0
        nuts.append(f'(zcyl {n(2.75)} 30 |> move {n(x)} {n(y)} 0)')
        # hex channel from below, its top 3 mm under the flange surface
        nuts.append(f'((⎏ 6 {n((NUT_AF + 0.3)/math.sqrt(3))} 40) |> move {n(x)} {n(y)} {n(-3 - 40)})')
    return f'(({flange} ⇓ {root}) ⊖ ({" ⊕ ".join(nuts)}))'

saddle_r = to_use(saddle_side())
saddle = f'({saddle_r} ⊕ (⇋ 1 0 0 {saddle_r}))'

# Tail platform with 45 degree gussets, and side-entry nut slots.
YS_MID = YS0 + CS0/2
plat_y0, plat_y1 = YS0 + 3, TAIL_Y + 7; PL = plat_y1 - plat_y0
PLAT_MID = (plat_y0 + plat_y1)/2
plat = f'(box {n(PW)} {n(PL)} {n(PT)} |> move 0 {n(plat_y0 + PL/2)} {n(ZT - PT/2)})'
g_drop = (PW - BOOM_W)/2
plat_root = f'(box {n(BOOM_W)} {n(PL)} {n(g_drop)} |> move 0 {n(plat_y0 + PL/2)} {n(ZT - PT - g_drop/2)})'
tail_cuts = []
for x, dy in TAIL_SCREWS:
    y = PLAT_MID + dy; sgn = 1 if x > 0 else -1
    tail_cuts.append(f'(zcyl 1.7 {n(PT + 8)} |> move {n(x)} {n(y)} {n(ZT - PT/2 - 3)})')   # clearance below the M3 nut for the protruding screw end
    slot_x0 = abs(x) - 3.0; slot_len = PW/2 + 2 - slot_x0
    tail_cuts.append(f'(box {n(slot_len)} {n(5.9)} {n(2.8)} |> move {n(sgn*(slot_x0 + slot_len/2))} {n(y)} {n(ZT - PT/2)})')
tail = f'(({plat} ⇓ {plat_root}) ⊖ ({" ⊕ ".join(tail_cuts)}))'

def tail_to_use(shape):
    return f'(ψ {n(PLAT_MID)} (ζ {n(ZT)} (θ {n(-TAIL_INCIDENCE)} (ψ {n(-PLAT_MID)} (ζ {n(-ZT)} {shape})))))'

fuselage_solid = f'(({pod}) ⊕ {saddle} ⊕ {tail_to_use(tail)}) ⊖ ({tail_to_use("(" + " ⊕ ".join(tail_cuts) + ")")})'

# ---- stab and fin --------------------------------------------------------------
STAB_PLATE, STAB_SPAR_H, STAB_RIM_H, STAB_RIB_H, RIB_W = 0.4, 2.4, 1.4, 1.8, 0.9
def stab_chord(y):
    hs = SB/2; r = abs(y); lin = CS0 + (CS1 - CS0)*r/hs; q = r - (hs - 16)
    return lin*(max(0.3, math.sqrt(max(0.0, 1 - (q/16)**2))) if q > 0 else 1.0)
def stab_le(y): return 0.25*(CS0 - stab_chord(y))
def strip(pts, w):
    """A polyline thickened to width w, as a closed polygon (x, y) list."""
    left, right = [], []
    for i, (x, y) in enumerate(pts):
        a = pts[max(i - 1, 0)]; b = pts[min(i + 1, len(pts) - 1)]
        dx, dy = b[0] - a[0], b[1] - a[1]; L = math.hypot(dx, dy); nx, ny = -dy/L*w/2, dx/L*w/2
        left.append((x + nx, y + ny)); right.append((x - nx, y - ny))
    return left + right[::-1]
def poly(pts): return '(poly ' + ' '.join(f'{n(x)} {n(y)}' for x, y in pts) + ')'
def stab():
    """A light ribbed plate (flat plates make fine tails): a 0.4 mm skin with a spar at 30%
    chord, rims along both edges and Warren-truss diagonals on top, all clipped to the
    tapered, round-tipped planform. Print frame: x chord, y span, flat skin on the plate."""
    hs = SB/2
    ys = [-hs, -hs + 4, -hs + 10, -hs + 16, 0.0, hs - 16, hs - 10, hs - 4, hs]
    te = [(stab_le(y) + stab_chord(y), y) for y in ys]; le = [(stab_le(y), y) for y in ys]
    outline = te + le[::-1]                                         # counter-clockwise
    inner = [(x - 1.2, y*(hs - 1.2)/hs) for x, y in te] + [(x + 1.2, y*(hs - 1.2)/hs) for x, y in le[::-1]]
    plate = f'(⮕ {n(STAB_PLATE)} {poly(outline)})'
    rim = f'((⮕ {n(STAB_RIM_H)} {poly(outline)}) ⊖ (ζ -1 (⮕ 10 {poly(inner)})))'
    sy = [y for y in ys[1:-1]]
    spar = f'(⮕ {n(STAB_SPAR_H)} {poly(strip([(stab_le(y) + 0.3*stab_chord(y), y) for y in [-hs + 3] + sy + [hs - 3]], RIB_W))})'
    k = 8; yk = [-hs + 14 + i*(SB - 28)/k for i in range(k + 1)]
    diag = []
    for i in range(k):
        a, b = yk[i], yk[i + 1]
        p0 = (stab_le(a) + 1.0, a) if i % 2 == 0 else (stab_le(a) + stab_chord(a) - 1.0, a)
        p1 = (stab_le(b) + stab_chord(b) - 1.0, b) if i % 2 == 0 else (stab_le(b) + 1.0, b)
        diag.append(f'(⮕ {n(STAB_RIB_H)} {poly(strip([p0, p1], RIB_W))})')
    block = f'(box {n(34)} {n(FOOT_W)} {n(STAB_SPAR_H)} |> move {n(PLAT_MID - YS0)} 0 {n(STAB_SPAR_H/2)})'
    mid = [(x - 0.6, y*(hs - 0.6)/hs) for x, y in te] + [(x + 0.6, y*(hs - 0.6)/hs) for x, y in le[::-1]]
    clip = f'(ζ -1 (⮕ 12 {poly(mid)}))'          # ribs end inside the rim, never on the outline
    holes = ' ⊕ '.join(f'(zcyl 1.7 30 |> move {n(PLAT_MID + dy - YS0)} {n(-x)} 0)' for x, dy in TAIL_SCREWS)
    body = f'({plate} ⊕ {rim} ⊕ (({spar} ⊕ {" ⊕ ".join(diag)} ⊕ {block}) ∩ {clip}))'
    return f'{body} ⊖ ({holes})', STAB_SPAR_H

def fin():
    """Blade tapers from 1.6 to 0.8 mm at the root, with a short 34 mm foot.
    Blade and foot both touch the bed; no floating blade or supports."""
    lf = CS0 - 6
    foot = f'(box {n(FOOT_W)} 34 {n(FOOT_H)} |> move 0 {n(PLAT_MID - YS0 - 3)} {n(FOOT_H/2)})'
    y0 = (lf - CF0)/2 + 2
    def post(y, z, r): return f'(zcyl {n(r)} 0.4 |> move 0 {n(y)} {n(z)})'
    blade = (f'({post(y0 + 0.8, 0.2, 0.8)} ⇓ {post(y0 + CF0 - 0.4, 0.2, 0.4)} ⇓ '
             f'{post(y0 + SWEEP_F + 0.6, HF, 0.6)} ⇓ {post(y0 + SWEEP_F + CF1 - 0.4, HF, 0.4)})')
    holes = ' ⊕ '.join(f'(zcyl 1.7 30 |> move {n(x)} {n(PLAT_MID + dy - YS0 - 3)} 0)' for x, dy in TAIL_SCREWS)
    return f'({foot} ⊕ {blade}) ⊖ ({holes})', lf, y0

stab_src, STAB_TMAX = stab()
fin_src, FOOT_L, FIN_Y0 = fin()

# Legacy pocket helper retained with an empty pocket list; the nose has no cavities.
def side_width(y):
    for (ya, _, wa), (yb, _, wb) in zip(SIDE, SIDE[1:]):
        if ya <= y <= yb: return wa + (wb - wa)*(y - ya)/(yb - ya)
def side_height(y):
    for (ya, ha, _), (yb, hb, _) in zip(SIDE, SIDE[1:]):
        if ya <= y <= yb: return ha + (hb - ha)*(y - ya)/(yb - ya)
pockets = [{'y': y, 'radius': r, 'floor_z': 3.0, 'depth': min(side_height(y - r), side_height(y + r)) - 3.0} for y, r in BALLAST]
pocket = ' ⊕ '.join(f'(⚓ bot (zcyl {n(q["radius"])} 60) |> move 0 {n(q["y"])} 3)' for q in pockets)
fuselage = f'({fuselage_solid})' if not pockets else f'({fuselage_solid}) ⊖ ({pocket})'

hdr = '// Bolted glider, mm. Print frame: flat face on the plate at z = 0. See design.py.\n'
(B/'wing-right.coscad').write_text(hdr + f'main = {wing_right()}\n')
(B/'wing-left.coscad').write_text(hdr + 'main = ⇋ 0 1 0 (' + wing_right() + ')\n')
(B/'fuselage.coscad').write_text(hdr + f'main = {fuselage}\n')
(B/'stab.coscad').write_text(hdr + f'main = {stab_src}\n')
(B/'fin.coscad').write_text(hdr + f'main = {fin_src}\n')

info = {
    'revision': 'balanced-v2', 'units': 'mm', 'frames': 'use: x right, y aft, z up, keel z = 0, wing root LE y = 0',
    'wing': {'panel_length': LP, 'root_chord': C0, 'tip_chord_unrounded': C1, 'thickness_root': T0, 'thickness_tip': T1,
             'sweep_tip_mm': SWEEP, 'sweep_offsets': [sweep(st, LP) for st in stations(LP)], 'root_gap': ROOT_GAP, 'dihedral_deg': DIHEDRAL, 'incidence_deg': INCIDENCE, 'stations': stations(LP),
             'chords': [chord(s, LP) for s in stations(LP)], 'screw_station': SCREW_S, 'screw_u': list(SCREW_U),
             'counterbore_floor': CB_FLOOR, 'root_thickness_at_screws': [root_thickness(u) for u in SCREW_U],
             'print_fit': fits(footprint(LP), pad=7)},
    'saddle': {'top_z': ZS, 'flange_width': FLANGE, 'flange_thickness': FT, 'y': [3.0, C0 - 6.0]},
    'fuselage': {'nose_y': NOSE_Y, 'tail_y': TAIL_Y, 'pod_width': POD_W, 'boom_width': BOOM_W, 'side': SIDE,
                 'nose_width': NOSE_W, 'ballast_pockets': pockets},
    'tail': {'incidence_deg': TAIL_INCIDENCE, 'platform_top_z': ZT, 'platform_thickness': PT, 'platform_width': PW, 'stab_le_y': YS0, 'stab_span': SB,
             'stab_root_chord': CS0, 'stab_tip_chord': CS1, 'stab_thickness': TS, 'stab_block_top': STAB_TMAX,
             'fin': {'foot': [FOOT_W, 34.0, FOOT_H], 'foot_centre_y': PLAT_MID - YS0 - 3, 'root_chord': CF0, 'tip_chord': CF1, 'height': HF, 'sweep': SWEEP_F,
                     'thickness': FIN_T, 'blade_y0_in_foot': FIN_Y0}, 'screws': TAIL_SCREWS, 'platform_y': [plat_y0, plat_y1], 'screw_y_ref': PLAT_MID},
    'hardware': {'wing': '4 x M5 x 12 button head + 4 M5 nuts', 'tail': '2 x steel M3 x 10 button head + 2 M3 nuts; no washers'},
    'ear_radius': EAR_R}
(B/'design.json').write_text(json.dumps(info, indent=2) + '\n')
print(f"panel length {LP:.0f} mm, span {2*(ROOT_GAP + LP*math.cos(math.radians(DIHEDRAL))):.0f} mm, fit {info['wing']['print_fit']}")
print('root thickness at screws', [round(t, 1) for t in info['wing']['root_thickness_at_screws']])

# ---- mouse-ear pads (print frame): 0.2 mm discs at corners that tend to lift ---------
# Modelled, not slicer brims: Bambu's CLI keeps native brim-ear settings but slices no ears.
def ears(points):
    return ' ⊕ '.join(f'(⚓ bot (◎ {n(EAR_R)} 0.2) |> move {n(x)} {n(y)} 0)' for x, y in points)
tip_c = chord(LP, LP); tip_x = 0.25*(C0 - tip_c) + tip_c/2 + SWEEP
mid_s = (ROOT_FLAT + LP - TIP_ROUND)/2; mid_te = 0.25*(C0 - chord(mid_s, LP)) + chord(mid_s, LP) + sweep(mid_s, LP)
EARS = {
    'wing-right': [(C0 - 2, -2), (tip_x, -LP + 2), (mid_te - 2, -mid_s)],
    'fuselage': [(0, NOSE_Y + 4), (0, TAIL_Y + 3)] + [(sx*side_width(y)/2, y) for y in (-60, 60, 150, 200) for sx in (1, -1)],
    'stab': [(stab_le(SB/2) + stab_chord(SB/2)/2, sy*(SB/2 - 2)) for sy in (1, -1)],
    'fin': [(sx*(FOOT_W/2 - 2), y) for sx in (1, -1) for y in (PLAT_MID - YS0 - 3 - 15, PLAT_MID - YS0 - 3 + 15)]}
EARS['wing-left'] = [(x, -y) for x, y in EARS['wing-right']]
for part, pts in EARS.items():
    (B/f'ears-{part}.coscad').write_text(f'// Mouse-ear pads for {part}, print frame.\nmain = {ears(pts)}\n')
info['ears'] = EARS
(B/'design.json').write_text(json.dumps(info, indent=2) + '\n')
