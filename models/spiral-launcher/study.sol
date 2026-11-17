# Spiral (clock) spring launcher for the 10 g glider: sizing study.
# Units: mm, N, g, s; energy in N*mm = mJ, so v^2 = 2 mJ / g gives m/s.
#
#   "$SOL" models/spiral-launcher/study.sol
#
# A flat PLA spiral is clamped at its outer end; its hub turns a pinion that
# drives a rack carriage. Model (pure moment, coils not touching):
#   winding theta adds curvature theta/L everywhere; sigma = E t theta / (2 L),
#   U = E I theta^2 / (2 L), I = b t^3 / 12.
#   A strip element at arc s from the hub turns by theta (L - s)/L about the
#   arbor, so the spring's inertia seen at the rack is sum m r^2 w^2 / R^2.
#   Winding dn turns packs the same span into n + dn coils (clamped outer end),
#   so the wound gap is (r_out - r_in)/(n + dn) - t.
#   Pinion: spoked ring (rim + teeth at R) whose inertia ~ m R^2; hub disk.
# Not modelled: coil rubbing, clamp stress concentrations, PLA hysteresis and
# rack friction (a flat 0.85 efficiency), spring surge.
N = use "../lib/num".

youngs = 3500.0.   # MPa, printed PLA along the lines (the strip is printed so it bends along them)
rho = 0.00124.     # g/mm^3 solid
eff = 0.85.
mGlider = 10.0.

spiralSegs rIn rOut n =
  pitch = Num.div (rOut - rIn) n;
  steps = 2000;
  pts = List.map (fn i -> p = Num.div (2 * N.pi * n * i) steps; r = rIn + Num.div (pitch * p) (2 * N.pi); (r * N.cos p, r * N.sin p, r)) (List.range 0 steps);
  List.map (fn q -> (a, b) = q; (x1, y1, r1) = a; (x2, y2, r2) = b; (N.hypot (x2 - x1) (y2 - y1), Num.div (r1 + r2) 2))
    (List.zip pts (List.drop 1 pts)).

# t strip thickness, b height, rIn/rOut centreline radii, n coils, R pinion
# pitch radius, stroke, mCar carriage (rack bar) mass
analyse t b rIn rOut n bigR stroke mCar =
  segs = spiralSegs rIn rOut n;
  len = List.fold (fn a q -> (d, r) = q; a + d) 0.0 segs;
  thMax = Num.div (2 * 25.0 * len) (youngs * t);
  th = Num.div stroke bigR;
  sigma = Num.div (youngs * t * th) (2 * len);
  u = Num.div (youngs * Num.div (b * t * t * t) 12 * th * th) (2 * len);
  (j, m, s) = List.fold (fn acc q -> (jj, mm, ss) = acc; (d, r) = q; dm = rho * t * b * d;
      w = Num.div (len - (ss + Num.div d 2)) len; (jj + dm * r * r * w * w, mm + dm, ss + d)) (0.0, 0.0, 0.0) segs;
  ringT = 4.5; gearH = 4.0;
  mRing = rho * 2 * N.pi * bigR * ringT * gearH;
  mSpokes = rho * 4 * bigR * 3 * gearH;
  jP = mRing * bigR * bigR + Num.div (mSpokes * bigR * bigR) 3;
  mHub = rho * N.pi * rIn * rIn * (b + gearH);
  jH = 0.5 * mHub * rIn * rIn;
  mEff = Num.div (j + jP + jH) (bigR * bigR);
  v = Num.sqrt (Num.div (2 * eff * u) (mGlider + mCar + mEff));
  dn = Num.div th (2 * N.pi);
  {len = len, turns = dn, sigma = sigma, u = u, mStrip = m, mEff = mEff, v = v, thMax = thMax,
   gapRest = Num.div (rOut - rIn) n - t, gapWound = Num.div (rOut - rIn) (n + dn) - t,
   force = Num.div (Num.div (youngs * Num.div (b * t * t * t) 12 * th) len) bigR}.

report = fn d -> "{N.fixed 2 d.turns} turns, {N.fixed 1 d.sigma} MPa, {N.fixed 2 (Num.div d.u 1000)} J, strip {N.fixed 0 d.mStrip} g, spring+pinion as {N.fixed 1 d.mEff} g at the rack, {N.fixed 2 d.v} m/s; gaps {N.fixed 2 d.gapRest} rest / {N.fixed 2 d.gapWound} wound mm; full-draw force {N.fixed 1 d.force} N".

main : unsafe Unit -> Unit.
main u =
  # carriage (revision 2, 2026-11-17): the rendered rack bar with its two T-key shoes plus the
  # printed mast, 10679 mm^3 of solid PLA (revision 1: 8252 mm^3 plus an M3 pin screw, 10.8 g)
  chosen = analyse 2.0 28 8 34 7 27.5 180 13.2;
  _ = print "Chosen: strip 2.0 x 28 mm, 7 coils r 8..34 mm, pinion R 27.5 (m 1.25, z 44, 4 mm face), stroke 180 mm, 13.2 g rack carriage";
  _ = print ("  " + report chosen);
  _ = print "  strip length {N.fixed 0 chosen.len} mm";
  _ = print "\nThe old captive demo, for scale: 0.04 turns over 10 mm (32 uJ).";
  _ = print "\nWinding more turns costs speed (best design per band, strip <= 75 g, gaps >= 1.0 mm wound / 1.5 mm rest, 180 mm stroke):";
  grid = List.concat (List.map (fn t -> List.concat (List.map (fn b -> List.concat (List.map (fn n -> List.concat (List.map (fn ro ->
           List.map (fn bigR -> (t, b, n, ro, bigR)) [12.0, 15.0, 18.0, 22.5, 27.5, 32.5, 40.0]) [34.0, 42.0, 50.0])) [5, 7, 9, 12])) [16.0, 24.0, 28.0])) [1.6, 2.0, 2.4]);
  rows = List.concat (List.map (fn g -> (t, b, n, ro, bigR) = g; d = analyse t b 8 ro n bigR 180 13.2;
      if and (d.sigma <= 25.01) (and (d.gapWound >= 1.0) (and (d.gapRest >= 1.5) (d.mStrip <= 75))) then [(g, d)] else []) grid);
  _ = List.map (fn band -> (lo, hi) = band;
      inBand = List.filter (fn r -> (g, d) = r; and (d.turns >= lo) (d.turns < hi)) rows;
      case inBand of
        [] -> print "  {N.fixed 1 lo}-{N.fixed 1 hi} turns: none feasible"
      | first :: rest -> best = List.fold (fn a r -> (ga, da) = a; (gr, dr) = r; if dr.v > da.v then r else a) first rest;
          (g, d) = best; (t, b, n, ro, bigR) = g;
          print "  {N.fixed 1 lo}-{N.fixed 1 hi} turns: {N.fixed 2 d.v} m/s (strip {N.repr t} x {N.repr b}, {n} coils to r {N.repr ro}, R {N.repr bigR}; spring+pinion {N.fixed 1 d.mEff} g at the rack)")
    [(0.5, 1.0), (1.0, 1.5), (1.5, 2.0), (2.0, 3.0), (3.0, 5.0)];
  Unit.
> main Unit.
