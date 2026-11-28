# Operating points of the long tweezers from the three unit-load FEA cases (2026-11-28).
#
#   fea/close.json  1 N total pressing the finger pads inward
#   fea/tip.json    1 N pushing the jaw face outward (the reaction when the jaws pinch something)
#   fea/side.json   1 N on the jaw along z (scissoring, out of the bed plane)
#
# Linear elasticity, so any state is F * close + P * tip. With the jaws touching (inner faces moved by
# the rest half-gap g): F * cF - P * cP = g, so F = (g + P * cP) / cF. Stresses superpose cell by
# cell; cells within 4 mm (along the arm) of a point load or on the clamped stub are left out (point-load
# singularities and the artificial clamp), as the stress command's notes ask.
#
#   coscad stress fea-close.coscad -o fea/close.json --full     (and tip, side)
#   sol analysis.sol
# jq extracts the nodes and cells (the reports are just over Sol's 16 MB JSON limit).
N = use "../lib/num".

jq filter file = ProcessResult code stdout err = unwrap (Proc.query (ProcessSpec ["jq", "-r", filter, file] "" [] "" 0));
  if code == 0 then stdout else error "jq on {file}: {err}".
# Large extractions go through a file: process captures stop at 1 MiB.
jqFile filter file out = ProcessResult code _ err = unwrap (Proc.query (ProcessSpec ["sh", "-c", "jq -r \"$1\" \"$2\" > \"$3\"", "jq", filter, file, out] "" [] "" 0));
  if code == 0 then unwrap (Try.readPath out) else error "jq on {file}: {err}".
nums line = List.map (fn w -> unwrap (Try.parseNum w)) (Str.words line).
rows text = List.map nums (List.filter (fn l -> l != "") (Str.lines text)).
mean xs = if xs == [] then error "no nodes" else List.fold (fn a x -> a + x) 0.0 xs / List.len xs.

# Jaw inner face: nodes 290..305 mm along, within 0.7 mm of the inner edge line, mid-height.
inner x = 1.25 + (12.5 - 1.25) * (x - 18.0) / (285.0 - 18.0).
jawFilter = ".nodes[] | select(.position_mm[0] >= 290 and .position_mm[0] <= 305 and .position_mm[2] >= 3 and .position_mm[2] <= 7) | \"\\(.position_mm[0]) \\(.position_mm[1]) \\(.displacement_mm[1]) \\(.displacement_mm[2])\"".
jaw file axis = rs = List.filter (fn r -> Numeric.abs ((r ! 1) - inner (r ! 0)) <= 0.7) (rows (jq jawFilter file));
  mean (List.map (fn r -> r ! axis) rs).
cellFilter = ".cells[] | \"\\(.centre_mm[0]) \\(.centre_mm[1]) \\(.centre_mm[2]) \\(.xx_yy_zz_xy_yz_zx_mpa | map(tostring) | join(\" \"))\"".
loadFilter = ".loads[] | .where".

vm s = sxx = s ! 0; syy = s ! 1; szz = s ! 2; sxy = s ! 3; syz = s ! 4; szx = s ! 5;
  Num.sqrt (0.5 * ((sxx - syy) * (sxx - syy) + (syy - szz) * (syy - szz) + (szz - sxx) * (szz - sxx)) + 3.0 * (sxy * sxy + syz * syz + szx * szx)).
# Load stations from design.sol: pads at 110..158, jaw at 291, 297.5, 304; the stub ends at 18.
stations = [110.0, 122.0, 134.0, 146.0, 158.0, 291.0, 297.5, 304.0].
counted r = x = r ! 0; if x < 12.0 then False else List.all (fn s -> Numeric.abs (x - s) >= 4.0) stations.
combine c t f p = List.map (fn i -> f * (c ! i) + p * (t ! i)) (List.range 0 5).
better f p acc c t = (best, at) = acc; v = vm (combine (List.drop 3 c) (List.drop 3 t) f p);
  if v > best then (v, c) else acc.
peak cs ts f p = List.fold (fn acc pair -> (c, t) = pair; if counted c then better f p acc c t else acc) (0.0, []) (List.zip cs ts).
fx x = N.fixed 1 x.
fx2 x = N.fixed 2 x.

> cF = 0.0 - jaw "fea/close.json" 2;
  cP = jaw "fea/tip.json" 2;
  cZ = jaw "fea/side.json" 3;
  g = inner 297.5;
  f0 = g / cF;
  cs = rows (jqFile cellFilter "fea/close.json" "fea/cells-close.txt");
  ts = rows (jqFile cellFilter "fea/tip.json" "fea/cells-tip.txt");
  u = if List.len cs != List.len ts then error "the two meshes differ" else Unit;
  u1 = print "jaw closes {fx2 cF} mm per N on the pads; opens {fx2 cP} mm per N pinch; moves {fx2 cZ} mm per N sideways (z)";
  u2 = print "rest half-gap at the jaw {fx2 g} mm: the jaws touch with {fx f0} N on the pads";
  table = List.map (fn p -> f = (g + p * cP) / cF; (v, at) = peak cs ts f p;
      "pinch {fx p} N: pads {fx f} N, peak von Mises {fx v} MPa at x {fx (at ! 0)} y {fx (at ! 1)} z {fx (at ! 2)}, safety {fx (50.0 / v)}") [0.0, 1.0, 2.0, 3.0, 4.0, 5.0];
  v = List.map print table;
  print "lever: each extra N on the pads adds {fx2 (cF / cP)} N at the jaw".
