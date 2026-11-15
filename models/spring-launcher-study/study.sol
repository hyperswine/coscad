# Spring launcher study for small gliders. SI units unless a name says mm/g.
#
# Part 1: the ceiling for printed plastic springs.
# Part 2: sizing a steel (music wire) extension spring for a pull-back launcher.
#
# All material values are typical published figures, not measurements of our
# filament. Printed parts are assumed loaded along the layers (never across).
#
#   "$SOL" models/spring-launcher-study/study.sol
N = use "../lib/num".

gAcc = 9.81.

# Energy balance; a spring fixed at one end moves 1/3 of its mass.
launchSpeed u mg mc ms = Num.sqrt (Num.div (2 * u) (mg + mc + Num.div ms 3)).

# Spring mass so that U = u*m_s = 1/2 (m_g + m_c + m_s/3) v^2; -1 = impossible
# (the spring cannot even accelerate its own mass to v).
springMassNeeded u v mg mc =
  denom = u - Num.div (v * v) 6;
  if denom <= 0 then 0 - 1 else Num.div (0.5 * (mg + mc) * v * v) denom.

# ---------------------------------------------------------------- Part 1
# (name, E GPa, density g/cm3, sigma "repeatable" MPa, sigma "near limit" MPa)
plastics = [("PLA", 3.5, 1.24, 20, 35), ("PETG", 2.1, 1.27, 20, 35),
            ("PC", 2.3, 1.2, 30, 50), ("Nylon (PA12)", 1.6, 1.02, 25, 45)].
# Fraction of sigma^2/2E a geometry can use: 1 for uniform tension or an ideal
# thin-flange beam in pure bending, 1/3 for a rectangular section at constant
# stress (spiral, tapered limb), 1/9 for a straight cantilever with an end load.
geometry = [("tension / ideal sandwich", 1.0), ("tapered limb, spiral", Num.div 1 3),
            ("straight cantilever", Num.div 1 9)].
targets = [("10 g chuck glider @ 8 m/s", 0.01, 8.0), ("20 g chuck glider @ 12 m/s", 0.02, 12.0),
           ("160 g bolted glider @ 10 m/s", 0.16, 10.0)].
mCarriage = 0.008.

padRight w s = if Str.len s >= w then s else s + Str.join "" (List.map (fn i -> " ") (List.range 1 (w - Str.len s))).
# J/kg stored at stress s (MPa) for geometry fraction eta
energy eta e rho s = Num.div (Num.div (eta * (s * 1000000.0) * (s * 1000000.0)) (2 * e * 1000000000.0)) (rho * 1000.0).

plasticRow p =
  (name, e, rho, s1, s2) = p;
  cells = List.map (fn g -> (gname, eta) = g;
    "{gname}: {N.fixed 3 (Num.div (energy eta e rho s1) 1000)}..{N.fixed 3 (Num.div (energy eta e rho s2) 1000)}") geometry;
  "  {padRight 13 name} " + Str.join " | " cells.
massCell v mg s e rho =
  ms = springMassNeeded (energy (Num.div 1 3) e rho s) v mg mCarriage;
  if ms < 0 then "impossible" else "{N.fixed 0 (ms * 1000)} g".
targetRow t =
  (label, mg, v) = t;
  row = List.map (fn p -> (name, e, rho, s1, s2) = p; "{name} {massCell v mg s1 e rho}..{massCell v mg s2 e rho}") plastics;
  "  {padRight 30 label} " + Str.join "; " row.
part1 u =
  _ = print "PART 1 - printed plastic springs";
  _ = print "Energy per gram of flexing material (J/g), repeatable .. near limit";
  _ = List.map (fn p -> print (plasticRow p)) plastics;
  _ = print "\nSpring mass for each target (tapered limb, eta=1/3, 8 g carriage)";
  _ = List.map (fn t -> print (targetRow t)) targets;
  _ = print "  (top speed with no glider, tapered limb, near limit):";
  List.map (fn p -> (name, e, rho, s1, s2) = p;
    print "    {padRight 13 name} {N.fixed 1 (Num.sqrt (6 * energy (Num.div 1 3) e rho s2))} m/s") plastics.

# ---------------------------------------------------------------- Part 2
gSteel = 79300000000.0.
rhoSteel = 7850.
wiresMm = [0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0].

# ASTM A228 tensile strength, Shigley: A/d^m with A=2211 MPa mm^m, m=0.145.
sutMusicWire dMm = Num.div 2211000000.0 (N.pow dMm 0.145).

# Everything about a wire/index pair that does not depend on the coil count.
# Close-wound music wire extension spring with machine hooks (r1=D/2, r2=2d);
# static limits body torsion 0.45 Sut, hook bending 0.75 Sut, hook torsion
# 0.40 Sut; initial tension half way into Shigley's preferred range (eq. 10-55).
springBase dMm dOuterMm =
  d = Num.div dMm 1000; dd = Num.div dOuterMm 1000;
  c = Num.div dd d;
  sut = sutMusicWire dMm;
  kb = Num.div (4 * c + 2) (4 * c - 3);
  mid = Num.div 231 (N.exp (0.105 * c));
  half = 6.9 * (4 - Num.div (c - 3) 6.5);
  tauI = (mid - half + 0.5 * 2 * half) * 1000000.0;
  fi = Num.div (tauI * N.pi * d * d * d) (8 * kb * dd);
  fBody = Num.div (0.45 * sut * N.pi * d * d * d) (8 * kb * dd);
  ka = Num.div (4 * c * c - c - 1) (4 * c * (c - 1));
  fHookA = Num.div (0.75 * sut) (Num.div (ka * 16 * dd) (N.pi * d * d * d) + Num.div 4 (N.pi * d * d));
  kbHook = Num.div (4 * 4.0 - 1) (4 * 4.0 - 4);
  fHookB = Num.div (0.4 * sut * N.pi * d * d * d) (8 * kbHook * dd);
  fMax = Numeric.min fBody (Numeric.min fHookA fHookB);
  # Python resolves equal limits by its dict order: the last equal key wins
  limit = if fMax == fHookB then "hook twist" else if fMax == fHookA then "hook bend" else "body";
  {dMm = dMm, dOuterMm = dOuterMm, c = c, d = d, dd = dd, fi = fi, fMax = fMax, limit = limit}.
springWith b n =
  k = Num.div (gSteel * b.d * b.d * b.d * b.d) (8 * b.dd * b.dd * b.dd * n);
  x = Num.div (b.fMax - b.fi) k;
  u = b.fi * x + 0.5 * k * x * x;
  lFree = (n + 1) * b.d + 2 * (b.dd - b.d);
  wire = N.pi * b.dd * (n + 2);
  m = Num.div (rhoSteel * N.pi * b.d * b.d) 4 * wire;
  {sd = b.dMm, sD = b.dOuterMm, sc = b.c, sn = n, sk = k, sfi = b.fi, sfmax = b.fMax,
   slimit = b.limit, sx = x, su = u, slfree = lFree, sm = m}.
extensionSpring dMm dOuterMm n = springWith (springBase dMm dOuterMm) n.

# Stable insertion by mass (Python's sort keeps equal masses in input order).
insertByMass s xs = case xs of
    [] -> [s]
  | h :: t -> if s.sm < h.sm then s :: xs else h :: insertByMass s t.
insertByMass : unsafe {sd : Int, sD : Int, sc : Int, sn : Int, sk : Int, sfi : Int, sfmax : Int, slimit : String, sx : Int, su : Int, slfree : Int, sm : Int} -> List {sd : Int, sD : Int, sc : Int, sn : Int, sk : Int, sfi : Int, sfmax : Int, slimit : String, sx : Int, su : Int, slfree : Int, sm : Int} -> List {sd : Int, sD : Int, sc : Int, sn : Int, sk : Int, sfi : Int, sfmax : Int, slimit : String, sx : Int, su : Int, slfree : Int, sm : Int}.

# First coil count from 5 to 399 that stores uNeed before the length or stroke
# limit stops the scan: (state, found) with state 0 scanning, 1 stopped, 2 found.
firstCoils b uNeed maxStroke maxLen =
  (state, found) = List.fold (fn st n -> (s0, f0) = st;
      if s0 != 0 then st else
        s = springWith b n;
        if s.slfree > maxLen then (1, f0)
        else if s.sx > maxStroke then (1, f0)
        else if s.su >= uNeed then (2, [s]) else (0, f0)) (0, []) (List.range 5 399);
  found.
search : unsafe Int -> Int -> Int -> Int -> List {sd : Int, sD : Int, sc : Int, sn : Int, sk : Int, sfi : Int, sfmax : Int, slimit : String, sx : Int, su : Int, slfree : Int, sm : Int}.
search uNeed maxStroke maxLen maxOd =
  found = List.concat (List.map (fn d ->
      List.concat (List.map (fn c10 ->
        cc = Num.div c10 10; dOut = cc * d;
        if dOut + d > maxOd then [] else firstCoils (springBase d dOut) uNeed maxStroke maxLen)
        (List.map (fn i -> 60 + 5 * i) (List.range 0 12)))) wiresMm);
  List.fold (fn acc s -> insertByMass s acc) [] (List.rev found) |> List.rev |> List.rev.

fmt s = "wire {N.repr s.sd} mm, OD {N.fixed 1 (s.sD + s.sd)} mm (C={N.fixed 1 s.sc}), "
  + "{s.sn} coils, free length {N.fixed 0 (s.slfree * 1000)} mm, "
  + "k={N.fixed 2 (Num.div s.sk 1000)} N/mm, Fi={N.fixed 1 s.sfi} N, Fmax={N.fixed 1 s.sfmax} N "
  + "({s.slimit}), stroke {N.fixed 0 (s.sx * 1000)} mm, U={N.fixed 2 s.su} J, "
  + "spring {N.fixed 1 (s.sm * 1000)} g".

eff = 0.7.
maxStroke = 0.15.
maxLen = 0.16.
maxOd = 16.

# Iterate the stored energy to include the spring's own moving mass (10 rounds,
# stopping early when nothing fits).
sizeTarget : unsafe (String, Int, Int) -> Unit.
sizeTarget t =
  (label, mg, v) = t;
  u0 = Num.div (0.5 * (mg + mCarriage) * v * v) eff;
  (u, cands, stopped) = List.fold (fn st i -> (uu, cs, stop) = st;
      if stop then st else
        found = search uu maxStroke maxLen maxOd;
        case found of
          [] -> (uu, found, True)
        | s :: rest -> (Num.div (0.5 * (mg + mCarriage + Num.div s.sm 3) * v * v) eff, found, False)) (u0, [], False) (List.range 0 9);
  _ = print "\n  {label}: needs {N.fixed 2 u} J stored";
  case cands of
    [] -> parallelFit u
  | _ -> _ = List.map (fn s -> print ("    " + fmt s)) (List.take 3 cands); Unit.
parallelFit : unsafe Int -> Unit.
parallelFit u =
  (done, line) = List.fold (fn st npar -> (d0, l0) = st;
      if d0 then st else
        c = search (Num.div u npar) maxStroke maxLen maxOd;
        case c of
          [] -> st
        | s :: rest -> (True, "    no single spring fits; {npar} in parallel: " + fmt s)) (False, "") (List.range 2 8);
  print (if done then line else "    not reachable with up to 8 springs in these limits").

padLeft w s = if Str.len s >= w then s else Str.join "" (List.map (fn i -> " ") (List.range 1 (w - Str.len s))) + s.
part2 : unsafe Unit -> Unit.
part2 u =
  _ = print "\nPART 2 - steel extension spring, pull-back carriage";
  store = Num.div (Num.div ((0.45 * sutMusicWire 1.0) * (0.45 * sutMusicWire 1.0)) (4 * gSteel)) rhoSteel;
  _ = print ("Music wire stores up to " + "{N.fixed 0 store} J/kg " + "in a helical body at 0.45 Sut (1 mm wire, before hooks and initial tension).");
  _ = print ("Limits: stroke <= {N.fixed 0 (maxStroke * 1000)} mm, free length <= {N.fixed 0 (maxLen * 1000)} mm, "
             + "OD <= {maxOd} mm, carriage {N.fixed 0 (mCarriage * 1000)} g, "
             + "assumed 70% of spring energy reaches the glider+carriage+spring");
  _ = List.map sizeTarget targets;
  s = extensionSpring 1.0 9.0 60;
  _ = print ("\n  Example stock-like spring: " + fmt s);
  _ = List.map (fn mg -> v = launchSpeed (eff * s.su) mg mCarriage s.sm; h = Num.div (v * v) (2 * gAcc);
      print "    glider {padLeft 4 (N.fixed 0 (mg * 1000))} g: {padLeft 5 (N.fixed 1 v)} m/s  (vacuum climb {N.fixed 1 h} m)") [0.005, 0.01, 0.02, 0.05, 0.16];
  _ = List.map (fn npar -> v = launchSpeed (eff * npar * s.su) 0.16 mCarriage (npar * s.sm);
      print "    {npar} in parallel, 160 g glider: {N.fixed 1 v} m/s") [2, 4];
  Unit.

main : unsafe Unit -> Unit.
main u = _ = part1 Unit; part2 Unit.
> main Unit.
