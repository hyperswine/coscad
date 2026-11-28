# Long tweezers (2026-11-28): reach about 25 cm past the fingers, firm enough to pinch a cloth
# around a lead screw. Printed flat along the diagonal of a 256 mm bed, so the arms flex in the bed
# plane and the bending stress runs along the extruded lines (PLA's strong direction).
#
# Each arm has two zones:
#   spring  root .. springEnd   slim (springWidth) - sets the closing force and stores the flex
#   beam    springEnd .. jaw    a Warren truss that tapers from beamRoot to beamTip wide - stiff, so
#                               finger force reaches the tips instead of bending the front of the arm
# The arms are straight and unstressed when open; the jaws are gap mm apart at rest. Finger pads
# (ribbed) sit at the start of the beam. The jaws carry interleaved teeth for cloth.
#
#   sol design.sol          writes tweezers.coscad, print.coscad and the fea-*.coscad cases
N = use "../lib/num".

prm unused = {height = 12.0, total = 310.0, rootEnd = 18.0, slotHalf = 1.25, keyhole = 2.0,
  springEnd = 100.0, springWidth = 4.7, ramp = 10.0, beamRoot = 14.0, beamTip = 7.0, jaw = 285.0,
  jawTip = 3.0, gap = 25.0, chord = 2.4, web = 2.2, panel = 16.0, minWindow = 9.5,
  padFrom = 104.0, padTo = 164.0, ribPitch = 4.0, rib = 0.8, toothPitch = 1.6, toothDepth = 0.6, toothFrom = 289.0, toothTo = 306.0,
  rootHalf = 8.0, hole = 3.0, holeX = 8.0}.

f x = N.fixed 3 x.
pt p = (x, y) = p; f x + " " + f y.
area pts = List.fold (fn acc i -> (x0, y0) = pts ! i; (x1, y1) = pts ! ((i + 1) % List.len pts); acc + x0 * y1 - x1 * y0) 0.0 (List.range 0 (List.len pts - 1)).
poly h pts = ccw = if area pts < 0.0 then List.rev pts else pts;
  "(⮕ {f h} (poly " + Str.join " " (List.map pt ccw) + "))".
cut h pts = "(ζ -1 " + poly (h + 2.0) pts + ")".
mirror pts = List.map (fn p -> (x, y) = p; (x, 0.0 - y)) pts.
lerp a b t = a + (b - a) * t.

# Arm A (y > 0). Inner edge: straight from (rootEnd, slotHalf) to (jaw, gap / 2), continued to the tip.
inner p x = p.slotHalf + (p.gap / 2.0 - p.slotHalf) * (x - p.rootEnd) / (p.jaw - p.rootEnd).
width p x = if x <= p.springEnd - p.ramp then p.springWidth
  else if x <= p.springEnd then lerp p.springWidth p.beamRoot ((x - (p.springEnd - p.ramp)) / p.ramp)
  else if x <= p.jaw then lerp p.beamRoot p.beamTip ((x - p.springEnd) / (p.jaw - p.springEnd))
  else lerp p.beamTip p.jawTip ((x - p.jaw) / (p.total - p.jaw)).
outer p x = inner p x + width p x.
xs p = [p.rootEnd - 1.0, p.springEnd - p.ramp, p.springEnd, p.jaw, p.total].
outline p = List.map (fn x -> (x, inner p x)) (xs p) + List.rev (List.map (fn x -> (x, outer p x)) (xs p)).

# Warren truss windows between the chords: diagonals zig-zag between the inner and outer chord.
node p k = x = p.springEnd + k * p.panel / 2.0; (x, if k % 2 == 0 then inner p x + p.chord else outer p x - p.chord).
lineX p a b side = (ax, ay) = a; (bx, by) = b; dx = bx - ax; dy = by - ay; len = Num.sqrt (dx * dx + dy * dy);
  nx = 0.0 - dy / len; ny = dx / len; o = side * p.web / 2.0; ((ax + nx * o, ay + ny * o), (dx, dy)).
meet l1 l2 = ((px, py), (dx, dy)) = l1; ((qx, qy), (ex, ey)) = l2;
  den = dx * ey - dy * ex; t = ((qx - px) * ey - (qy - py) * ex) / den; (px + t * dx, py + t * dy).
chordLine p k = x0 = p.springEnd; x1 = p.jaw; up = k % 2 == 1;
  if up then ((x0, outer p x0 - p.chord), (x1 - x0, outer p x1 - outer p x0)) else ((x0, inner p x0 + p.chord), (x1 - x0, inner p x1 - inner p x0)).
# Window k sits between diagonal k (node k -> k+1) and diagonal k+1 (node k+1 -> k+2), on the chord of node k.
window p k = a = node p k; b = node p (k + 1); c = node p (k + 2);
  s = if k % 2 == 0 then 1.0 else 0.0 - 1.0;
  d1 = lineX p a b (0.0 - s); d2 = lineX p b c (0.0 - s); ch = chordLine p k;
  [meet d1 ch, meet d1 d2, meet d2 ch].
windowFits p k = (x, _) = node p (k + 2); x < p.jaw - 4.0 and width p x >= p.minWindow.
windows p = List.filter (windowFits p) (List.range 0 (Num.floor ((p.jaw - p.springEnd) / (p.panel / 2.0)) - 2)) |> List.map (window p).

# Grip ribs on the outer face of the pads, and teeth cut into the inner face of the jaw.
rib p x = a = (x, outer p x - 0.2); b = (x + 1.6, outer p (x + 1.6) - 0.2);
  [a, b, (x + 1.6, outer p (x + 1.6) + p.rib), (x, outer p x + p.rib)].
ribs p = List.map (fn i -> rib p (p.padFrom + i * p.ribPitch)) (List.range 0 (Num.floor ((p.padTo - p.padFrom) / p.ribPitch))).
tooth p off x = x0 = x + off; [(x0, inner p x0 - 0.1), (x0 + p.toothPitch / 2.0, inner p (x0 + p.toothPitch / 2.0) + p.toothDepth), (x0 + p.toothPitch, inner p (x0 + p.toothPitch) - 0.1)].
teeth p off = List.map (fn i -> tooth p off (p.toothFrom + i * p.toothPitch)) (List.range 0 (Num.floor ((p.toothTo - p.toothFrom) / p.toothPitch) - 1)).

armA p = h = p.height;
  body = Str.join " ⊕ " ([poly h (outline p)] + List.map (poly h) (ribs p));
  holes = List.map (cut h) (windows p) + List.map (cut h) (teeth p 0.0);
  "(" + body + ")" + Str.join "" (List.map (fn c -> " ⊖ " + c) holes).
armB p = h = p.height;
  body = Str.join " ⊕ " ([poly h (mirror (outline p))] + List.map (fn r -> poly h (mirror r)) (ribs p));
  holes = List.map (fn w -> cut h (mirror w)) (windows p) + List.map (fn t -> cut h (mirror t)) (teeth p (p.toothPitch / 2.0));
  "(" + body + ")" + Str.join "" (List.map (fn c -> " ⊖ " + c) holes).
root p = poly p.height [(0.0, 0.0 - p.rootHalf + 3.0), (3.0, 0.0 - p.rootHalf), (p.rootEnd, 0.0 - p.rootHalf), (p.rootEnd, p.rootHalf), (3.0, p.rootHalf), (0.0, p.rootHalf - 3.0)].
relief p = "(χ {f p.rootEnd} (ζ -1 (◎ {f p.keyhole} {f (p.height + 2.0)})))".
slot p = cut p.height [(p.rootEnd, 0.0 - p.slotHalf), (p.rootEnd + 2.0, 0.0 - inner p (p.rootEnd + 2.0)), (p.rootEnd + 2.0, inner p (p.rootEnd + 2.0)), (p.rootEnd, p.slotHalf)].
hang p = "(χ {f p.holeX} (ζ -1 (◎ {f p.hole} {f (p.height + 2.0)})))".

# FEA cases: arm A alone, clamped on a stub at its root (the root block is far stiffer than the arm).
marker name x y z = "(χ {f x} (ψ {f y} (ζ {f z} (▬ 0.4 0.4 0.4)))) |> datum {name} ctr 0 0 0".
stub p = poly p.height [(p.rootEnd - 8.0, p.slotHalf), (p.rootEnd, p.slotHalf), (p.rootEnd, p.slotHalf + p.springWidth), (p.rootEnd - 8.0, p.slotHalf + p.springWidth)].
padXs p = List.map (fn i -> p.padFrom + 6.0 + i * 12.0) (List.range 0 4).
jawXs p = [p.toothFrom + 2.0, (p.toothFrom + p.toothTo) / 2.0, p.toothTo - 2.0].
feaArm p = "{armA p} ⊕ {stub p}".
feaCase p name loads markers =
  ["// FEA case {name} for one arm of the long tweezers (generated by design.sol).",
   "arm = " + feaArm p] +
  List.map (fn m -> m) markers +
  ["main = arm" + Str.join "" (List.map (fn i -> " ⊕ m{i}") (List.range 0 (List.len markers - 1))), "", "support lft"] + loads +
  ["stress material=pla safety=2 cell=1.2"].
padMarkers p = List.map (fn i -> x = padXs p ! i; "m{i} = " + marker "pad{i}" x (outer p x - 0.2) (p.height / 2.0)) (List.range 0 4).
jawMarkers p = List.map (fn i -> x = jawXs p ! i; "m{i} = " + marker "jaw{i}" x (inner p x + 0.7) (p.height / 2.0)) (List.range 0 2).
# Unit loads: 1 N total pressing the pads inward (-y), 1 N pushing the jaw face outward (+y), 1 N on the jaw along z.
closeLoads p = List.map (fn i -> "load @pad{i} 0 -0.2 0") (List.range 0 4).
tipLoads p = List.map (fn i -> "load @jaw{i} 0 {f (1.0 / 3.0)} 0") (List.range 0 2).
sideLoads p = List.map (fn i -> "load @jaw{i} 0 0 {f (1.0 / 3.0)}") (List.range 0 2).

tweezers p = "(" + Str.join " ⊕ " [root p, armA p, armB p] + ") ⊖ " + relief p + " ⊖ " + slot p + " ⊖ " + hang p.
# Printing: turned -45 degrees so it lies along the bed diagonal clear of the X1C's front-left exclusion zone, with 0.2 mm
# mouse-ear pads at the jaws and the root (owner preference over full brims).
ears p = Str.join " ⊕ " (List.map (fn q -> (x, y) = q; "(χ {f x} (ψ {f y} (◎ 7 0.2)))")
  [(2.0, 0.0), (p.total - 2.0, inner p p.total + 1.5), (p.total - 2.0, 0.0 - inner p p.total - 1.5)]).

saveFile path text = ProcessResult code _ err = unwrap (Proc.query (ProcessSpec ["sh", "-c", "cat > \"$1\"", "w", path] "" [] text 0));
  if code == 0 then print "wrote {path}" else error err.
nl = Str.fromCode 10.
> p = prm Unit;
  here = Str.trim (unwrap (IO.cwd Unit));
  u0 = if Str.endsWith "models/long-tweezers" here then Unit else error "run from models/long-tweezers";
  model = "tweezers = " + tweezers p;
  u1 = saveFile "tweezers.coscad" (Str.join nl ["// Long PLA tweezers, design frame: x along the length, jaws at +x (generated by design.sol).", model, "main = tweezers", ""]);
  cx = p.total / 2.0;
  u2 = saveFile "print.coscad" (Str.join nl ["// Long PLA tweezers laid along the bed diagonal with mouse ears (generated by design.sol).", model,
    "ears = " + ears p, "main = ω -45 (χ {f (0.0 - cx)} (tweezers ⊕ ears))", ""]);
  u3 = saveFile "fea-close.coscad" (Str.join nl (feaCase p "close: 1 N on the pads" (closeLoads p) (padMarkers p)) + nl);
  u4 = saveFile "fea-tip.coscad" (Str.join nl (feaCase p "tip: 1 N outward on the jaw" (tipLoads p) (jawMarkers p)) + nl);
  u5 = saveFile "fea-side.coscad" (Str.join nl (feaCase p "side: 1 N along z on the jaw" (sideLoads p) (jawMarkers p)) + nl);
  print "windows per arm: {List.len (windows p)}".
