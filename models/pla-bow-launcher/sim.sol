# Quasi-static bow model for the PLA glider launcher. Units: mm, N, g, s.
#
# Each limb is a chain of N rigid segments joined by rotational springs
# (k = E I / ds), clamped at the riser. For each nock position the limb shape
# minimises bending energy with the string taut. That gives the draw-force
# curve, the stored energy and the stress along the limb. Launch speed uses the
# standard quasi-static mode: limb node velocities are d(position)/d(draw)
# times the carriage speed, so the limb mass is counted where it actually moves.
#
# Not modelled: string stretch, PLA hysteresis, rail friction. A 0.9 factor on
# the delivered energy is reported alongside the ideal figure.
#
# The constrained minimum is found with Newton's method on its stationarity
# conditions k_i (phi_i - phi0_i) = lambda dg/dphi_i, g = 0, with the exact
# Jacobian (tip derivatives are 90-degree rotations of tip - node), in place of
# the original's scipy SLSQP. Results agree to the printed precision.
#
#   "$SOL" models/pla-bow-launcher/sim.sol L T B0 BT S [KAPPA0]
#   e.g. sim.sol 210 3 44 8 225      (also a module: search.sol uses `simulate`)
N = use "../lib/num".

youngs = 3500.0.      # MPa, printed PLA along the lines
rho = 0.00124.        # g/mm^3, solid (100% infill)
nSeg = 24.

# ---- small list helpers -------------------------------------------------------
sumL xs = List.fold (fn a x -> a + x) 0.0 xs.
maxL xs = List.fold (fn a x -> Numeric.max a x) (xs ! 0) xs.
zipWith f xs ys = List.map (fn p -> (a, b) = p; f a b) (List.zip xs ys).
idx n = List.range 0 (n - 1).
lastOf xs = xs ! (List.len xs - 1).
dot xs ys = sumL (zipWith (fn a b -> a * b) xs ys).

# ---- the bow -----------------------------------------------------------------
makeBow lenL t b0 bt r0 kappa0 =
  ds = Num.div lenL nSeg;
  bj = List.map (fn i -> b0 + Num.div ((bt - b0) * (i * ds)) lenL) (idx nSeg);
  k = List.map (fn b -> Num.div (youngs * Num.div (b * t * t * t) 12) ds) bj;
  mseg = List.map (fn i -> rho * t * ds * (b0 + Num.div ((bt - b0) * ((i + 0.5) * ds)) lenL)) (idx nSeg);
  mTip = 0.6;
  {lenL = lenL, t = t, r0 = r0, ds = ds, k = k, mseg = mseg, mTip = mTip, mString = 0.3,
   limbMass = sumL mseg + mTip,
   # printed shape: uniform forward (reflex) curvature kappa0, 1/mm
   phi0 = List.map (fn i -> 0 - kappa0 * ds) (idx nSeg)}.

# nodes: N+1 points from the clamp (0, r0) to the tip
nodes bow phi =
  (pts, x, y, a) = List.fold (fn st p -> (ps, px, py, pa) = st; a2 = pa + p;
      nx = px + N.cos a2 * bow.ds; ny = py + N.sin a2 * bow.ds; ((nx, ny) :: ps, nx, ny, a2))
    ([(0.0, bow.r0)], 0.0, bow.r0, N.halfPi) phi;
  List.rev pts.
energy bow phi = 0.5 * sumL (zipWith (fn k d -> k * d * d) bow.k (zipWith (fn p q -> p - q) phi bow.phi0)).
stress bow phi = List.map (fn d -> Num.div (youngs * bow.t * N.abs d) (2 * bow.ds)) (zipWith (fn p q -> p - q) phi bow.phi0).

# ---- the constraint: g and its derivatives ------------------------------------
# Braced: g = tip_y - S.  Drawn: g = |tip - (xn, 0)| - S.
# w_i = d tip / d phi_i = R90 (tip - p_i); d w_i / d phi_j = -(tip - p_max(i,j)).
geometry drawnAt xn pts =
  (tx, ty) = lastOf pts;
  joints = List.take nSeg pts;
  ws = List.map (fn p -> (px, py) = p; (0 - (ty - py), tx - px)) joints;
  rel = List.map (fn p -> (px, py) = p; (tx - px, ty - py)) joints;     # tip - p_m
  dx = tx - xn; dist = if drawnAt then N.hypot dx ty else 1.0;
  (ux, uy) = if drawnAt then (Num.div dx dist, Num.div ty dist) else (0.0, 1.0);
  {ty = ty, ws = ws, rel = rel, dist = dist, ux = ux, uy = uy}.
# g and its gradient G_i = u . w_i  (braced: u = (0, 1))
constraint drawnAt xn sHalf geo =
  grad = List.map (fn w -> (wx, wy) = w; geo.ux * wx + geo.uy * wy) geo.ws;
  (if drawnAt then geo.dist - sHalf else geo.ty - sHalf, grad).
# ---- small dense solve (the 3 x 3 Woodbury capacitance) -----------------------
# Each row is a list [a_c .. a_n, rhs]; the pivot row (largest |head|) is kept
# whole for back substitution, the others lose their eliminated head.
eliminate rows =
  (piv, rest) = List.fold (fn st r -> (best, others) = st;
      if N.abs (r ! 0) > N.abs (best ! 0) then (r, best :: others) else (best, r :: others))
    (rows ! 0, []) (List.drop 1 rows);
  reduced = List.map (fn r -> f = Num.div (r ! 0) (piv ! 0); List.drop 1 (zipWith (fn a b -> a - f * b) r piv)) (List.rev rest);
  (piv, reduced).
solveLinear n rows =
  (pivots, left) = List.fold (fn st c -> (ps, rs) = st; (p, red) = eliminate rs; (p :: ps, red)) ([], rows) (idx n);
  # pivots are last-first: p_{n-1} = [a, rhs] ... p_0 = [a_00 .. a_0n, rhs]
  List.fold (fn xs p -> rest = List.drop 1 p; rhs = lastOf rest;
      xi = Num.div (rhs - dot rest xs) (p ! 0); xi :: xs) [] pivots.

# ---- structured Newton step ----------------------------------------------------
# The Newton matrix is A = K - lambda H with H = (W W^T - G G^T)/dist - T,
# T_ij = t_max(i,j) (t_m = u . (tip - p_m)). With y = cumulative sums of x,
# (K + lambda T) x = b becomes a symmetric tridiagonal system in y; the rank-3
# rest is a Woodbury correction, and the string constraint borders it.
thomasSolve d e z =
  n = List.len d;
  sub = 0.0 :: e;                       # sub_i couples row i to row i-1
  sup = e + [0.0];                      # sup_i couples row i to row i+1
  (cps, dps, cPrev, dPrev) = List.fold (fn st q -> (ca, da, cp, dp) = st; (ai, bi, ci, ri) = q;
      m = bi - ai * cp;
      c2 = Num.div ci m; d2 = Num.div (ri - ai * dp) m;
      (c2 :: ca, d2 :: da, c2, d2)) ([], [], 0.0, 0.0)
    (List.map (fn q -> (abc, ri) = q; (ab, ci) = abc; (ai, bi) = ab; (ai, bi, ci, ri)) (List.zip (List.zip (List.zip sub d) sup) z));
  (ys, last) = List.fold (fn st q -> (acc, yn) = st; (ci, di) = q; yi = di - ci * yn; (yi :: acc, yi)) ([], 0.0) (List.zip cps dps);
  ys.
# x = (K + lambda T)^-1 b
solveB bow lam t b =
  k = bow.k;
  cs = zipWith (fn a nx -> a - nx) t (List.drop 1 t + [0.0]);
  kNext = List.drop 1 k + [0.0];
  d = zipWith (fn kk c -> kk + lam * c) (zipWith (fn a nx -> a + nx) k kNext) cs;
  e = List.map (fn kn -> 0 - kn) (List.take (nSeg - 1) kNext);
  z = zipWith (fn a nx -> a - nx) b (List.drop 1 b + [0.0]);
  y = thomasSolve d e z;
  zipWith (fn a pv -> a - pv) y (0.0 :: List.take (nSeg - 1) y).
newtonDelta bow drawnAt lam r g grad geo =
  t = List.map (fn rr -> (rx, ry) = rr; geo.ux * rx + geo.uy * ry) geo.rel;
  bInv = fn b -> solveB bow lam t b;
  minusR = List.map (fn x -> 0 - x) r;
  aInv = if drawnAt then
      wx = List.map (fn w -> (a, b) = w; a) geo.ws; wy = List.map (fn w -> (a, b) = w; b) geo.ws;
      vs = [wx, wy, grad];
      sd = [0 - Num.div lam geo.dist, 0 - Num.div lam geo.dist, Num.div lam geo.dist];
      zs = List.map bInv vs;
      # capacitance I + S V^T Z (3 x 3)
      cap = List.map (fn q -> (sv, vi) = q; List.map (fn q2 -> (j, zj) = q2; (if j == 0 then 0 else 0) + sv * dot vi zj) (List.zip (idx 3) zs)) (List.zip sd vs);
      capI = zipWith (fn row i -> zipWith (fn x j -> x + (if i == j then 1 else 0)) row (idx 3)) cap (idx 3);
      fn b -> yb = bInv b;
        rhs = zipWith (fn sv vi -> sv * dot vi yb) sd vs;
        coef = solveLinear 3 (zipWith (fn row rv -> row + [rv]) capI rhs);
        List.fold (fn acc q -> (cq, zq) = q; zipWith (fn a zz -> a - cq * zz) acc zq) yb (List.zip coef zs)
    else bInv;
  p = aInv minusR;
  q = aInv grad;
  dlam = Num.div (0 - g - dot grad p) (dot grad q);
  (zipWith (fn a b -> a + dlam * b) p q, dlam).

# ---- Newton on (phi, lambda) ---------------------------------------------------
# R_i = k_i (phi_i - phi0_i) - lambda G_i,  R_N = g.
residualAt bow drawnAt xn sHalf phi lam =
  geo = geometry drawnAt xn (nodes bow phi);
  (g, grad) = constraint drawnAt xn sHalf geo;
  r = zipWith (fn kd gi -> kd - lam * gi) (zipWith (fn k d -> k * d) bow.k (zipWith (fn p q -> p - q) phi bow.phi0)) grad;
  (r, g, grad, geo).
norm2 r g = sumL (List.map (fn x -> x * x) r) + g * g.
newtonStep bow drawnAt xn sHalf state =
  (phi, lam, conv) = state;
  (r, g, grad, geo) = residualAt bow drawnAt xn sHalf phi lam;
  (dphi, dlam) = newtonDelta bow drawnAt lam r g grad geo;
  base = norm2 r g;
  # backtracking on the residual norm keeps far starts (the brace) stable
  (phiN, lamN, found) = List.fold (fn st h -> (p0, l0, ok) = st;
      if ok then st else
        scale = N.pow2 (0 - h);
        pTry = zipWith (fn p d -> p + scale * d) phi dphi; lTry = lam + scale * dlam;
        (r2, g2, gr2, geo2) = residualAt bow drawnAt xn sHalf pTry lTry;
        if or (norm2 r2 g2 < base) (h == 12) then (pTry, lTry, True) else st) (phi, lam, False) (List.range 0 12);
  done = maxL (List.map N.abs dphi) < 0.00000000000001;
  (phiN, lamN, done).
solveEq bow drawnAt xn sHalf phiStart =
  (r, g, grad, geo) = residualAt bow drawnAt xn sHalf phiStart 0;
  lam0 = Num.div (dot grad (zipWith (fn k d -> k * d) bow.k (zipWith (fn p q -> p - q) phiStart bow.phi0))) (dot grad grad);
  (phi, lam, conv) = List.fold (fn st i -> (p, l, c) = st; if c then st else newtonStep bow drawnAt xn sHalf st) (phiStart, lam0, False) (List.range 1 80);
  (r2, g2, gr2, geo2) = residualAt bow drawnAt xn sHalf phi lam;
  if N.abs g2 < 0.000001 then Ok phi else Err "no equilibrium (constraint residual {N.repr g2})".
# Bracing. A reflexed limb has more than one braced equilibrium (curled
# forward, or bent back), and the original's SLSQP, a local minimiser started at
# phi0 + 0.01, lands in whichever basin that start reaches. To follow the same
# basin: continue from that start (at its own tip height) to the string length
# in 100 steps; if that path hits a fold, continue from the unloaded printed
# shape (an exact solution, lambda = 0); finally try a direct solve.
continueFrom bow sHalf start =
  yStart = (lastOf (nodes bow start)) |> (fn p -> (px, py) = p; py);
  List.fold (fn acc k -> case acc of
      Err why -> acc
    | Ok phi -> solveEq bow False 0 (yStart + Num.div ((sHalf - yStart) * k) 100) phi) (Ok start) (List.range 0 100).
braced bow sHalf =
  start = List.map (fn p -> p + 0.01) bow.phi0;
  case continueFrom bow sHalf start of
    Ok phi -> Ok phi
  | Err e1 ->
    (fx, fy) = lastOf (nodes bow bow.phi0);
    viaFree = if N.abs fx < 0.000000001 then Err e1 else continueFrom bow sHalf bow.phi0;
    case viaFree of
      Ok phi -> Ok phi
    | Err e2 -> solveEq bow False 0 sHalf start.
drawn bow sHalf xn phiStart = solveEq bow True xn sHalf phiStart.

# ---- numpy.gradient, 1-D, second order inside, first order at the ends --------
gradient f x =
  n = List.len x;
  x3 = List.zip (List.zip x (List.drop 1 x)) (List.drop 2 x);
  f3 = List.zip (List.zip f (List.drop 1 f)) (List.drop 2 f);
  inner = zipWith (fn xx ff -> (xa, x2) = xx; (x0, x1) = xa; (fa, f2) = ff; (f0, f1) = fa;
      dx1 = x1 - x0; dx2 = x2 - x1;
      a = Num.div (0 - dx2) (dx1 * (dx1 + dx2)); b = Num.div (dx2 - dx1) (dx1 * dx2); c = Num.div dx1 (dx2 * (dx1 + dx2));
      a * f0 + b * f1 + c * f2) x3 f3;
  [Num.div ((f ! 1) - (f ! 0)) ((x ! 1) - (x ! 0))] + inner
    + [Num.div ((f ! (n - 1)) - (f ! (n - 2))) ((x ! (n - 1)) - (x ! (n - 2)))].

# ---- results -------------------------------------------------------------------
# A small typed JSON value so the output matches Python's json.dumps(indent=1).
Val = Type (VI Int | VF Int | VNaN | VS String | VL (List Val)).
rounded n x = VF (unwrap (Try.parseNum (N.fixed n x))).
# Draw back in 2 mm steps until the peak stress passes sigmaMax, then bisect
# 30 times back to it. Returns Ok (fields, curve rows) or Err message.
drawTo bow sHalf sigmaMax xBrace phiB =
  # state: (xs reversed, phis reversed, phi, x, finished, err)
  (xsR, phisR, phiL, xL, fin, err) = List.fold (fn st i -> (xr, pr, phi, x, done, e) = st;
      if or done (e != "") then st else
        x2 = x - 2.0;
        case drawn bow sHalf x2 phi of
          Err why -> (xr, pr, phi, x, True, why)
        | Ok p2 ->
          if maxL (stress bow p2) > sigmaMax then
            (lo, plo, e2) = List.fold (fn bs j -> (lo0, hi0, pl0, e0) = bs;
                if e0 != "" then bs else
                  mid = 0.5 * (lo0 + hi0);
                  case drawn bow sHalf mid pl0 of
                    Err why -> (lo0, hi0, pl0, why)
                  | Ok pm -> if maxL (stress bow pm) > sigmaMax then (lo0, mid, pl0, "") else (mid, hi0, pm, ""))
              (x, x2, phi, "") (List.range 1 30) |> (fn bs -> (l, h, p, e1) = bs; (l, p, e1));
            (lo :: xr, plo :: pr, plo, lo, True, e2)
          else if x2 < xBrace - 400 then (xr, pr, phi, x, True, "draw did not reach the stress limit")
          else (x2 :: xr, p2 :: pr, p2, x2, False, "")) ([xBrace], [phiB], phiB, xBrace, False, "") (List.range 1 400);
  if err != "" then Err err else Ok (List.rev xsR, List.rev phisR).

simulate lenL t b0 bt sHalf kappa0 sigmaMax mGlider mCarriage r0 verbose =
  bow = makeBow lenL t b0 bt r0 kappa0;
  case braced bow sHalf of
    Err why -> Err why
  | Ok phiB ->
    xBrace = (lastOf (nodes bow phiB)) |> (fn p -> (px, py) = p; px);
    case drawTo bow sHalf sigmaMax xBrace phiB of
      Err why -> Err why
    | Ok drawnPath -> report bow lenL t b0 bt sHalf kappa0 mGlider mCarriage r0 verbose phiB xBrace drawnPath.

report bow lenL t b0 bt sHalf kappa0 mGlider mCarriage r0 verbose phiB xBrace drawnPath =
  (xs, phis) = drawnPath;
  n = List.len xs;
  u = List.map (fn p -> 2 * energy bow p) phis;            # both limbs, N*mm
  uD = lastOf u;
  # the caller prepends the parameters (L .. kappa0) with their own types
  head = [("limb_mass_g", rounded 2 bow.limbMass), ("brace_mm", rounded 1 (0 - xBrace)), ("draw_mm", rounded 1 (0 - lastOf xs))];
  profile = ("sigma_profile_full_draw", VL (List.map (fn s -> rounded 1 s) (stress bow (lastOf phis))));
  sigB = ("sigma_brace_MPa", rounded 1 (maxL (stress bow phiB)));
  # A draw that never starts (the brace already exceeds sigmaMax) leaves two
  # equal positions: numpy then divides by zero and the speeds are NaN.
  if (xs ! 0) == lastOf xs then
    Ok (head + [("power_stroke_mm", rounded 1 0.0), ("stored_J", rounded 3 (Num.div uD 1000)),
                ("brace_stored_J", rounded 3 (Num.div (u ! 0) 1000)), ("peak_draw_force_N", VNaN), ("max_draw_force_N", VNaN),
                ("v_glider", VNaN), ("v_glider_90pct", VNaN), ("glider_share_of_energy", VNaN), sigB, profile, ("peak_accel_g", VNaN)], [])
  else
    f = List.map (fn g -> 0 - g) (gradient u xs);           # draw force, N
    pts = List.map (nodes bow) phis;                         # n lists of N+1 points
    # quasi-static kinetic coefficient M(x) = sum m |dP/dx|^2 (both limbs)
    mnode = List.map (fn j -> (if j < nSeg then Num.div (bow.mseg ! j) 2 else 0) + (if j > 0 then Num.div (bow.mseg ! (j - 1)) 2 else 0)
                      + (if j == nSeg then bow.mTip else 0)) (List.range 0 nSeg);
    dpx = List.map (fn j -> gradient (List.map (fn ps -> (px, py) = ps ! j; px) pts) xs) (List.range 0 nSeg);
    dpy = List.map (fn j -> gradient (List.map (fn ps -> (px, py) = ps ! j; py) pts) xs) (List.range 0 nSeg);
    # summed node by node in the original's order (j = 0 .. N for each position)
    mLimb = List.map (fn s -> 2 * s) (List.fold (fn acc q -> (mj, dd) = q; (gx, gy) = dd;
        zipWith (fn a e -> (ex, ey) = e; a + mj * (ex * ex + ey * ey)) acc (List.zip gx gy))
      (List.map (fn i -> 0.0) (idx n)) (List.zip mnode (List.zip dpx dpy)));
    # string: about half its mass moves with the nock
    m = List.map (fn ml -> ml + mGlider + mCarriage + 0.5 * bow.mString) mLimb;
    # speed vs position (energy in N*mm = mJ; mass in g -> v in m/s: v^2 = 2*mJ/g)
    v = zipWith (fn ui mi -> Num.sqrt (Numeric.max (Num.div (2 * (uD - ui)) mi) 0)) u m;
    iSep = List.fold (fn best i -> if (v ! i) > (v ! best) then i else best) 0 (idx n);
    vSep = v ! iSep;
    eff = Num.div (0.5 * mGlider * vSep * vSep) uD;
    stroke = (xs ! iSep) - lastOf xs;
    accel = maxL (zipWith (fn fi mi -> Num.div fi mi) f m);
    fields = head + [("power_stroke_mm", rounded 1 stroke), ("stored_J", rounded 3 (Num.div uD 1000)),
       ("brace_stored_J", rounded 3 (Num.div (u ! 0) 1000)), ("peak_draw_force_N", rounded 1 (lastOf f)),
       ("max_draw_force_N", rounded 1 (maxL f)), ("v_glider", rounded 2 vSep), ("v_glider_90pct", rounded 2 (vSep * Num.sqrt 0.9)),
       ("glider_share_of_energy", rounded 3 eff), sigB, profile, ("peak_accel_g", rounded 0 (Num.div (accel * 1000) 9.81))];
    curve = if verbose then List.map (fn i -> VL [rounded 1 (0 - xs ! i), rounded 2 (f ! i), rounded 4 (Num.div (u ! i) 1000), rounded 2 (v ! i)]) (idx n) else [];
    Ok (fields, curve).

# ---- Python-style JSON rendering -------------------------------------------------
pad n = Str.join "" (List.map (fn i -> " ") (List.range 1 n)).
render : unsafe Int -> Val -> String.
render ind v = case v of
    VI i -> str i
  | VF x -> N.repr x
  | VNaN -> "NaN"
  | VS s -> "\"" + s + "\""
  | VL items -> if items == [] then "[]" else
      "[\n" + Str.join ",\n" (List.map (fn it -> pad (ind + 1) + render (ind + 1) it) items) + "\n" + pad ind + "]".
renderObj : unsafe Int -> List (String, Val) -> String.
renderObj ind fields = "\{\n" + Str.join ",\n" (List.map (fn kv -> (k, v) = kv; pad (ind + 1) + "\"" + k + "\": " + render (ind + 1) v) fields) + "\n" + pad ind + "\}".

main : unsafe Unit -> Unit.
main u =
  argv = args Unit;
  nums = List.map (fn a -> unwrap (Try.parseNum a)) argv;
  _ = if or (List.len nums < 5) (List.len nums > 6) then error "usage: sim.sol L T B0 BT S [KAPPA0]" else Unit;
  kappa0 = if List.len nums == 6 then nums ! 5 else 0.0;
  # the original passes every argument through float()
  params = [("L", VF (nums ! 0)), ("t", VF (nums ! 1)), ("b0", VF (nums ! 2)), ("bt", VF (nums ! 3)),
            ("S", VF (nums ! 4)), ("R0", VF 20.0), ("kappa0", VF kappa0)];
  case simulate (nums ! 0) (nums ! 1) (nums ! 2) (nums ! 3) (nums ! 4) kappa0 30.0 10.0 8.0 20.0 True of
    Err why -> error why
  | Ok res -> (fields, curve) = res; print (renderObj 0 (params + fields + [("curve", VL curve)])).
> main Unit.
