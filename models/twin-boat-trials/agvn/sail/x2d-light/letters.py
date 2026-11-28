"""Stroke lettering for the AGVN sailboat (2026-11-28, replaces the 1 mm pixel font).

The first two-colour print showed why the pixel letters looked rough: BENCHY stood 0.9 mm
proud of the hull, its strokes were 1 mm (barely two lines), and pixels that met only at a
corner (N, V) split the label into 11 loose islands. These letters are one closed outline
per glyph (shapely buffer of stroke centrelines, mitred, clipped to the glyph box), 8 mm
tall with 1.4 mm strokes (more than three 0.42 mm lines), chamfered corners. They are cut
flush into the surface: each label is extruded from DEPTH below the surface to PROUD above
it, and boat.coscad keeps only the part inside the solid (green) and removes it from the
black body.

  python letters.py        # prints the glyph and label lines that go into ../boat.coscad
"""
from shapely.geometry import LineString, box
from shapely.ops import unary_union

W, H, S = 5.0, 8.0, 1.4          # glyph box and stroke width, mm
GAP = 1.4                        # space between glyphs
DEPTH, PROUD = 1.2, 0.4          # inlay depth; overshoot so the cut is clean
FLAG_DEPTH = 1.0                 # the flag is 2.4 mm thick
h = S/2; L, R, B, T, M = h, W - h, h, H - h, H/2; c = 1.6

GLYPHS = {
    'A': [[(L, 0), (L, T - c), (L + c, T), (R - c, T), (R, T - c), (R, 0)], [(L, M - 0.4), (R, M - 0.4)]],
    'B': [[(L, B), (L, T), (R - c, T), (R, T - c), (R, M + 0.8), (R - 0.8, M), (R, M - 0.8), (R, B + c), (R - c, B), (L, B)],
          [(L, M), (R - 0.8, M)]],
    'C': [[(R, T), (L + c, T), (L, T - c), (L, B + c), (L + c, B), (R, B)]],
    'E': [[(R, T), (L, T), (L, B), (R, B)], [(L, M), (R - 0.6, M)]],
    'G': [[(R, T), (L + c, T), (L, T - c), (L, B + c), (L + c, B), (R, B), (R, M), (W/2 + 0.3, M)]],
    'H': [[(L, 0), (L, H)], [(R, 0), (R, H)], [(L, M), (R, M)]],
    'N': [[(L, 0), (L, H)], [(R, 0), (R, H)], [(L, T), (R, B)]],
    'V': [[(L, H), (L, M), (W/2, B), (R, M), (R, H)]],
    'Y': [[(L, H), (W/2, M), (R, H)], [(W/2, M), (W/2, 0)]],
}

def outline(ch):
    strokes = [LineString(p).buffer(h, cap_style='square', join_style='mitre', mitre_limit=3) for p in GLYPHS[ch]]
    g = unary_union(strokes).intersection(box(0, 0, W, H))
    assert g.geom_type == 'Polygon', (ch, g.geom_type)   # one island per glyph
    return g.simplify(0.01)

def poly(ring):
    pts = list(ring.coords)[:-1]
    return 'poly ' + ' '.join(f'{x:.4f} {y:.4f}' for x, y in pts)

def glyph_line(ch):
    g = outline(ch)
    body = f'(⮕ 1 ({poly(g.exterior)}))'
    for hole in g.interiors:
        body += f' ⊖ (ζ -0.1 (⮕ 1.2 ({poly(hole)})))'
    return f'glyph{ch} = {body}'

def word(name, text):
    parts = ' ⊕ '.join(f'(χ {i*(W + GAP):g} glyph{ch})' for i, ch in enumerate(text))
    return f'{name} = {parts}', len(text)*W + (len(text) - 1)*GAP

if __name__ == '__main__':
    for ch in sorted(set('AGVN' + 'BENCHY')): print(glyph_line(ch))
    agvn, wa = word('agvn', 'AGVN'); benchy, wb = word('benchy', 'BENCHY')
    print(agvn); print(benchy)
    print(f'// agvn {wa:g} x {H:g} mm, benchy {wb:g} x {H:g} mm, strokes {S:g} mm')
