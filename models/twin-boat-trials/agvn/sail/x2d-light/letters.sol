# Stroke lettering for the AGVN sailboat (2026-11-28; replaced the 1 mm pixel font, Sol since the
# same day).
#
# The first two-colour print showed why the pixel letters looked rough: BENCHY stood 0.9 mm proud
# of the hull, its strokes were 1 mm (barely two lines) and pixels that met only at a corner (N, V)
# split the label into 11 loose islands. These letters are 8 mm tall with 1.4 mm strokes (more
# than three 0.42 mm lines) and chamfered corners. Each glyph is the union of its strokes: a
# rectangle per segment (square caps at free ends), a mitre at every bend, clipped to the glyph
# box, so every glyph is one island. boat.coscad inlays the labels flush.
#
#   sol letters.sol        # prints the glyph and word lines that go into ../boat.coscad
N = use "../../../../lib/num".

w = 5.0.
h = 8.0.
s = 1.4.
gap = 1.4.
glyphs unused =
  r = s / 2; lx = r; rx = w - r; by = r; ty = h - r; my = h / 2; c = 1.6;
  [("A", [[(lx, 0.0), (lx, ty - c), (lx + c, ty), (rx - c, ty), (rx, ty - c), (rx, 0.0)], [(lx, my - 0.4), (rx, my - 0.4)]]),
   ("B", [[(lx, by), (lx, ty), (rx - c, ty), (rx, ty - c), (rx, my + 0.8), (rx - 0.8, my), (rx, my - 0.8), (rx, by + c), (rx - c, by), (lx, by)],
          [(lx, my), (rx - 0.8, my)]]),
   ("C", [[(rx, ty), (lx + c, ty), (lx, ty - c), (lx, by + c), (lx + c, by), (rx, by)]]),
   ("E", [[(rx, ty), (lx, ty), (lx, by), (rx, by)], [(lx, my), (rx - 0.6, my)]]),
   ("G", [[(rx, ty), (lx + c, ty), (lx, ty - c), (lx, by + c), (lx + c, by), (rx, by), (rx, my), (w / 2 + 0.3, my)]]),
   ("H", [[(lx, 0.0), (lx, h)], [(rx, 0.0), (rx, h)], [(lx, my), (rx, my)]]),
   ("N", [[(lx, 0.0), (lx, h)], [(rx, 0.0), (rx, h)], [(lx, ty), (rx, by)]]),
   ("V", [[(lx, h), (lx, my), (w / 2, by), (rx, my), (rx, h)]]),
   ("Y", [[(lx, h), (w / 2, my), (rx, h)], [(w / 2, my), (w / 2, 0.0)]])].

unit a b = (ax, ay) = a; (bx, by) = b; dx = bx - ax; dy = by - ay; len = Num.sqrt (dx * dx + dy * dy); (dx / len, dy / len).
add p q k = (px, py) = p; (qx, qy) = q; (px + k * qx, py + k * qy).
normal d = (dx, dy) = d; (0.0 - dy, dx).
area pts = List.fold (fn acc i -> (x0, y0) = pts ! i; (x1, y1) = pts ! ((i + 1) % List.len pts); acc + x0 * y1 - x1 * y0) 0.0 (List.range 0 (List.len pts - 1)).
poly pts = ccw = if area pts < 0.0 then List.rev pts else pts;
  "(⮕ 1 (poly " + Str.join " " (List.map (fn p -> (x, y) = p; N.fixed 4 x + " " + N.fixed 4 y) ccw) + "))".

# Segment i of a polyline: square caps only where the polyline ends.
segment pts i =
  r = s / 2; a = pts ! i; b = pts ! (i + 1); d = unit a b; n = normal d;
  capA = if i == 0 then r else 0.0;
  capB = if i == List.len pts - 2 then r else 0.0;
  a2 = add a d (0.0 - capA); b2 = add b d capB;
  poly [add a2 n r, add b2 n r, add b2 n (0.0 - r), add a2 n (0.0 - r)].
# Both sides of bend i get the mitre quad; the inner one lies inside the strokes already.
mitres pts i =
  r = s / 2; v = pts ! i; da = unit (pts ! (i - 1)) v; db = unit v (pts ! (i + 1));
  (ax, ay) = da; (bx, by) = db;
  cross = ax * by - ay * bx; dot = ax * bx + ay * by;
  na = normal da; nb = normal db; (nax, nay) = na; (nbx, nby) = nb;
  m = ((nax + nbx) / (1.0 + dot), (nay + nby) / (1.0 + dot));
  side k = poly [v, add v na (k * r), add v m (k * r), add v nb (k * r)];
  if Numeric.abs cross < 0.000001 then [] else [side 1.0, side (0.0 - 1.0)].
strokes pts = List.map (segment pts) (List.range 0 (List.len pts - 2)) +
  List.concat (List.map (mitres pts) (List.range 1 (List.len pts - 2))).
glyphLine entry = (ch, lines) = entry;
  parts = List.concat (List.map strokes lines);
  "glyph{ch} = (" + Str.join " ⊕ " parts + ") ∩ (ζ -0.5 (▬ {N.trim w} {N.trim h} 2))".
word name txt = letters = List.map (fn i -> Str.slice txt i (i + 1)) (List.range 0 (Str.len txt - 1));
  "{name} = " + Str.join " ⊕ " (List.map (fn i -> "(χ {N.trim (i * (w + gap))} glyph{letters ! i})") (List.range 0 (List.len letters - 1))).

> u = List.map (fn e -> print (glyphLine e)) (glyphs Unit);
  print (word "agvn" "AGVN") >> print (word "benchy" "BENCHY") >>
  print "// agvn {N.trim (4 * w + 3 * gap)} x {N.trim h} mm, benchy {N.trim (6 * w + 5 * gap)} x {N.trim h} mm, strokes {N.trim s} mm".
