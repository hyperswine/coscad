# Hex8 linear elasticity of sliced deposited volume; lengths mm, forces N.
#
#   GLIDER_FEA_DIR=/tmp "$SOL" compare.sol DESIGN CELL_MM
#
# Reads $GLIDER_FEA_DIR/glider-DESIGN-body.gcode and -offsets, voxelises the
# deposited volume into CELL_MM bricks, and prints one JSON line. The solve is
# a matrix-free Jacobi-preconditioned conjugate gradient: every brick shares
# one 24x24 stiffness matrix scaled by its volume fraction, so K*p is a gather,
# one dense (bricks x 24) * (24 x 24) product and a scatter. It stops at a
# relative residual of 1e-10, so the result agrees with a direct sparse solve.
N = use "../../../lib/num".
J = use "sol/lib/json".

# ---- deposition -------------------------------------------------------------

env key fallback = case unwrap (Env.get key) of EnvMissing -> fallback | EnvText s -> s.

isMove line = if not (Str.startsWith "G" line) then False else
  or (or (Str.startsWith "G0 " line) (Str.startsWith "G1 " line)) (or (Str.startsWith "G2 " line) (Str.startsWith "G3 " line)).

# Sol's number parser wants digits on both sides of the point; G-code writes
# E.8 and -.5, which Python's float() accepts.
padPoint t = if Str.endsWith "." t then t + "0" else t.
normalNum t | Str.startsWith "-." t = "-0" + padPoint (N.drop 1 t).
normalNum t | Str.startsWith "." t = "0" + padPoint t.
normalNum t = padPoint t.
noValue = 0 - 1.0e300.
# Python: {k: float(v) for k, v in re.findall(r'([XYZE])(-?[\d.]+)', code)};
# G-code words are space separated, so each axis is " X" up to the next space.
axis key code =
  i = Str.indexOf key code;
  if i < 0 then noValue else
    rest = N.drop (i + 2) code;
    e = Str.indexOf " " rest;
    word = if e < 0 then rest else N.take e rest;
    case Try.parseNum word of
      Ok v -> v
    | Err err -> (case Try.parseNum (normalNum word) of Ok v -> v | Err err2 -> noValue).
fields line =
  c = Str.indexOf ";" line;
  code = if c < 0 then line else N.take c line;
  (axis " X" code, axis " Y" code, axis " Z" code, axis " E" code).
pick v old = if v == noValue then old else v.
ceil x = 0 - Num.floor (0 - x).
sqrt2 = 1.4142135623730951.
volumeScale = N.pi * 0.765625.

# one chord split into n samples; each sample appends its key (i, j, k) to
# keys and its volume to vols (a Vec column keeps one number kind)
samples : unsafe Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Vector Int, Vector Int) -> (Vector Int, Vector Int).
samples i n x y dx dy kz vol h ox oy v | i >= n = v.
samples i n x y dx dy kz vol h ox oy v =
  t = Num.div (i + 0.5) n;
  a = x + t * dx - 128 + ox;
  b = y + t * dy - 128 + oy;
  ka = Num.floor (Num.div (Num.div (a + b) sqrt2) h);
  kb = Num.floor (Num.div (Num.div (b - a) sqrt2) h);
  (keys, vols) = v;
  samples (i + 1) n x y dx dy kz vol h ox oy (Vec.push kz (Vec.push kb (Vec.push ka keys)), Vec.push vol vols).

# state: (active, x, y, z, samples)
step : unsafe Int -> Int -> Int -> (Bool, Int, Int, Int, (Vector Int, Vector Int)) -> String -> (Bool, Int, Int, Int, (Vector Int, Vector Int)).
step h ox oy st line =
  (active0, x, y, z, v) = st;
  active1 = if Str.startsWith "; OBJECT_ID:" line then True else active0;
  active = if Str.startsWith "; FEATURE: Custom" line then False else active1;
  if not (isMove line) then (active, x, y, z, v) else
    (fx, fy, fz, fe) = fields line;
    xx = pick fx x; yy = pick fy y; zz = pick fz z;
    e = pick fe 0;
    if and (and active (e > 0)) (or (xx != x) (yy != y)) then
      n = Numeric.max 1 (ceil (Num.div (N.hypot (xx - x) (yy - y)) (h * 0.33)));
      vol = Num.div (e * volumeScale) n;
      kz = Num.floor (Num.div (zz - 0.1) h);
      (active, xx, yy, zz, samples 0 n x y (xx - x) (yy - y) kz vol h ox oy v)
    else (active, xx, yy, zz, v).

deposition : unsafe String -> String -> Int -> (Vector Int, Vector Int).
deposition folder name h =
  offsets = unwrap (J.items (unwrap (J.parse (readPath "{folder}/glider-{name}-offsets"))));
  ox = unwrap (J.num (offsets ! 0));
  oy = unwrap (J.num (offsets ! 1));
  lines = Str.lines (readPath "{folder}/glider-{name}-body.gcode");
  (a, x, y, z, v) = List.fold (step h ox oy) (False, 0.0, 0.0, 0.0, (Vec.new Unit, Vec.new Unit)) lines;
  v.


# ---- mesh -------------------------------------------------------------------

big = 1.0e300.
# bounds of the sample keys, as [min a, max a, min b, max b, min c, max c]
bounds : unsafe Int -> Int -> List Int -> Vector Int -> (List Int, Vector Int).
bounds i n acc v | i >= n = (acc, v).
bounds i n acc v =
  (a, v1) = Vec.get i v; (b, v2) = Vec.get (i + 1) v1; (c, v3) = Vec.get (i + 2) v2;
  [a0, a1, b0, b1, c0, c1] = acc;
  bounds (i + 3) n [Numeric.min a a0, Numeric.max a a1, Numeric.min b b0, Numeric.max b b1, Numeric.min c c0, Numeric.max c c1] v3.

zeros n = Vec.map (fn i -> 0.0) (Vec.range 1 n).

# accumulate sample volumes into the dense cell grid, in sample order
accumulate : unsafe Int -> Int -> Int -> Int -> Int -> Int -> Int -> Vector Int -> Vector Int -> Vector Int -> (Vector Int, Vector Int, Vector Int).
accumulate i n a0 b0 c0 nb nc v ws cells | i >= n = (v, ws, cells).
accumulate i n a0 b0 c0 nb nc v ws cells =
  (a, v1) = Vec.get (3 * i) v; (b, v2) = Vec.get (3 * i + 1) v1; (c, v3) = Vec.get (3 * i + 2) v2; (w, ws2) = Vec.get i ws;
  k = ((a - a0) * nb + (b - b0)) * nc + (c - c0);
  (old, cells2) = Vec.get k cells;
  accumulate (i + 1) n a0 b0 c0 nb nc v3 ws2 (Vec.set k (old + w) cells2).

# occupied cells in ascending dense order, as (cell index, volume)
occupied : unsafe Int -> List (Int, Int) -> Vector Int -> (List (Int, Int), Vector Int).
occupied k acc cells | k < 0 = (acc, cells).
occupied k acc cells =
  (w, c2) = Vec.get k cells;
  occupied (k - 1) (if w > 0 then (k, w) :: acc else acc) c2.

# Bricks on a dense node grid: node (i, j, k) of the bounding box is
# (i * nj + j) * nk + k, with three DOFs from 3 * node. Nodes no brick touches
# stay in the vectors with a zero mask, so a corner's DOF is base + offset.
mesh : unsafe String -> String -> Int -> {a0 : Int, b0 : Int, c0 : Int, ni : Int, nj : Int, nk : Int, bricks : List {base : Int, frac : Int, ca : Int, cb : Int, cc : Int}}.
mesh folder name h =
  (v, ws) = deposition folder name h;
  (ns3, v1) = Vec.len v;
  ns = ns3 / 3;
  ([a0, a1, b0, b1, c0, c1], v2) = bounds 0 ns3 [big, 0 - big, big, 0 - big, big, 0 - big] v1;
  na = a1 - a0 + 1; nb = b1 - b0 + 1; nc = c1 - c0 + 1;
  (v3, ws2, cells) = accumulate 0 ns a0 b0 c0 nb nc v2 ws (zeros (na * nb * nc));
  u1 = Vec.free v3; u0 = Vec.free ws2;
  (occ, cells2) = occupied (na * nb * nc - 1) [] cells;
  u2 = Vec.free cells2;
  nj = nb + 1; nk = nc + 1;
  cube = h * h * h;
  bricks = List.map (fn kw -> (k, w) = kw;
    i = k / (nb * nc); j = (k / nc) % nb; l = k % nc;
    {base = (i * nj + j) * nk + l, frac = Numeric.min 1 (Num.div w cube),
     ca = a0 + i, cb = b0 + j, cc = c0 + l}) occ;
  {a0 = a0, b0 = b0, c0 = c0, ni = na + 1, nj = nj, nk = nk, bricks = bricks}.

# ---- element ----------------------------------------------------------------

youngs = 2500.0.
poisson = 0.35.
shear = Num.div youngs (2 * (1 + poisson)).
lame = Num.div (2 * shear * poisson) (1 - 2 * poisson).
dEntry r t | and (r < 3) (t < 3) = if r == t then lame + 2 * shear else lame.
dEntry r t = if r == t then shear else 0.0.
dRows = List.map (fn r -> List.map (fn t -> dEntry r t) (List.range 0 5)) (List.range 0 5).
sgn q d = 2 * ((q / d) % 2) - 1.
signs = List.map (fn q -> [sgn q 1, sgn q 2, sgn q 4]) (List.range 0 7).
pairs = [(0, 0), (1, 1), (2, 2), (0, 1), (1, 2), (0, 2)].

# shape-function gradients at x, grad i q, as in the NumPy original
gradAt h x i q =
  s = signs ! q;
  others = List.filter (fn j -> j != i) [0, 1, 2];
  prod = List.fold (fn acc j -> acc * (1 + (s ! j) * (x ! j))) 1 others;
  Num.div ((s ! i) * prod) (4 * h).
bEntry h x row col =
  (i, j) = pairs ! row;
  q = col / 3; c = col % 3;
  if c == i then gradAt h x j q else if and (i != j) (c == j) then gradAt h x i q else 0.0.
strain h x = List.map (fn row -> List.map (fn col -> bEntry h x row col) (List.range 0 23)) (List.range 0 5).
dot xs ys = List.fold (fn acc p -> (a, b) = p; acc + a * b) 0 (List.zip xs ys).
column rows c = List.map (fn row -> row ! c) rows.
transpose rows = List.map (fn c -> column rows c) (List.range 0 (List.len (rows ! 0) - 1)).

# K = sum over the 2x2x2 Gauss points of B^T D B (h/2)^3, row-major 24x24
stiffness h =
  w = N.pow (Num.div h 2) 3;
  r3 = Num.sqrt 3.0;
  per = List.map (fn s ->
    b = strain h (List.map (fn si -> Num.div si r3) s);
    bt = transpose b;
    db = List.map (fn drow -> List.map (fn col -> dot drow col) (transpose b)) dRows;
    dbt = transpose db;
    List.map (fn brow -> List.map (fn col -> dot brow col * w) dbt) bt) signs;
  List.map (fn r -> List.map (fn c -> List.fold (fn acc m -> acc + (m ! r) ! c) 0 per) (List.range 0 23)) (List.range 0 23).

# ---- matrix-free operator ---------------------------------------------------

# Vec arithmetic consumes its operands; Vec.mmul threads them back, so a
# 1-column product by [1.0] copies a vector and a 1xN by Nx1 product dots two.
copy n v =
  (c, v2, one) = Vec.mmul n 1 1 v (Vec.fromList [1.0]);
  u = Vec.free one;
  (c, v2).
inner n a b =
  (r, a2, b2) = Vec.mmul 1 n 1 a b;
  (d, r2) = Vec.get 0 r;
  u = Vec.free r2;
  (d, a2, b2).

# The brick column (24 values per brick, brick-major) and the node vectors are
# linked by incidence lists: k = 8 * brick + corner owns column entries
# 3k..3k+2. Each used node carries (its first DOF, [3k ...]); gathering reads a
# node once and writes it to every brick corner, scattering sums the corners
# of a node and writes it once.
setInc : unsafe List Int -> Int -> Int -> Int -> Vector Int -> Vector Int.
setInc incs x y z pe | incs == [] = pe.
setInc incs x y z pe = i :: rest = incs; setInc rest x y z (Vec.set (i + 2) z (Vec.set (i + 1) y (Vec.set i x pe))).
gatherNodes : unsafe List (Int, List Int) -> Vector Int -> Vector Int -> (Vector Int, Vector Int).
gatherNodes nodes p pe | nodes == [] = (p, pe).
gatherNodes nodes p pe =
  (d, incs) :: rest = nodes;
  (x, p1) = Vec.get d p; (y, p2) = Vec.get (d + 1) p1; (z, p3) = Vec.get (d + 2) p2;
  gatherNodes rest p3 (setInc incs x y z pe).
sumInc : unsafe List Int -> Int -> Int -> Int -> Vector Int -> (Int, Int, Int, Vector Int).
sumInc incs x y z qe | incs == [] = (x, y, z, qe).
sumInc incs x y z qe =
  i :: rest = incs;
  (a, q1) = Vec.get i qe; (b, q2) = Vec.get (i + 1) q1; (c, q3) = Vec.get (i + 2) q2;
  sumInc rest (x + a) (y + b) (z + c) q3.
scatterNodes : unsafe List (Int, List Int) -> Vector Int -> Vector Int -> (Vector Int, Vector Int).
scatterNodes nodes qe q | nodes == [] = (qe, q).
scatterNodes nodes qe q =
  (d, incs) :: rest = nodes;
  (x, y, z, qe2) = sumInc incs 0 0 0 qe;
  scatterNodes rest qe2 (Vec.set (d + 2) z (Vec.set (d + 1) y (Vec.set d x q))).

# incidence lists: k = 8 * brick + corner has brick-column base 3k; link each
# k into a per-node chain (head per node, next per k), then read the chains
# out as (first DOF of the node, [3k ...]) in ascending node order
linkBricks : unsafe List Int -> List Int -> Int -> Vector Int -> Vector Int -> (Vector Int, Vector Int).
linkBricks bases offs k head next | bases == [] = (head, next).
linkBricks bases offs k head next =
  d :: rest = bases;
  (head2, next2) = linkCorners d offs k head next;
  linkBricks rest offs (k + 8) head2 next2.
linkCorners : unsafe Int -> List Int -> Int -> Vector Int -> Vector Int -> (Vector Int, Vector Int).
linkCorners d offs k head next | offs == [] = (head, next).
linkCorners d offs k head next =
  o :: rest = offs;
  nd = (d + o) / 3;
  (h, head2) = Vec.get nd head;
  linkCorners d rest (k + 1) (Vec.set nd k head2) (Vec.set k h next).
chain : unsafe Int -> List Int -> Vector Int -> (List Int, Vector Int).
chain k acc next | k < 0 = (acc, next).
chain k acc next = (k2, next2) = Vec.get k next; chain k2 (3 * k :: acc) next2.
freeBoth a b x = u1 = Vec.free a; u2 = Vec.free b; x.
readChains : unsafe Int -> List (Int, List Int) -> Vector Int -> Vector Int -> List (Int, List Int).
readChains nd acc head next | nd < 0 = freeBoth head next acc.
readChains nd acc head next =
  (k, head2) = Vec.get nd head;
  if k < 0 then readChains (nd - 1) acc head2 next else
    (incs, next2) = chain k [] next;
    readChains (nd - 1) ((3 * nd, incs) :: acc) head2 next2.
incidence : unsafe List Int -> List Int -> Int -> List (Int, List Int).
incidence bases offs nodes =
  nb = List.len bases;
  (head, next) = linkBricks bases offs 0 (Vec.map (fn i -> 0 - 1) (Vec.range 1 nodes)) (Vec.map (fn i -> 0 - 1) (Vec.range 1 (8 * nb)));
  readChains (nodes - 1) [] head next.

# q = K p over all bricks; p is consumed, the stiffness and the per-entry brick
# fractions thread back
applyK : unsafe {n : Int, bricks : Int, nodes : List (Int, List Int)} -> Vector Int -> Vector Int -> Vector Int -> (Vector Int, Vector Int, Vector Int).
applyK op p k fr =
  (fz, fr2) = copy (24 * op.bricks) fr;
  (p2, pe) = gatherNodes op.nodes p (Vec.scale 0.0 fz);
  q0 = Vec.scale 0.0 p2;
  (qe, pe2, k2) = Vec.mmul op.bricks 24 24 pe k;
  u = Vec.free pe2;
  (fc, fr3) = copy (24 * op.bricks) fr2;
  (qe2, q) = scatterNodes op.nodes (Vec.zipMul qe fc) q0;
  u2 = Vec.free qe2;
  (q, k2, fr3).

# Jacobi-preconditioned conjugate gradient on the masked (free) DOFs.
# Exact arithmetic converges within one step per free DOF, so running past
# that means the system is singular or badly conditioned: refuse it.
cg : unsafe {n : Int, bricks : Int, nodes : List (Int, List Int)} -> Int -> Int -> Int -> Int -> Vector Int -> Vector Int -> Vector Int -> Int -> Vector Int -> Vector Int -> Vector Int -> Vector Int -> (Vector Int, Int).
cg op lim tol it rz x r p rr k fr inv mask | rr <= tol =
  u1 = Vec.free r; u2 = Vec.free p; u3 = Vec.free k; u4 = Vec.free fr; u5 = Vec.free inv; u6 = Vec.free mask;
  (x, it).
cg op lim tol it rz x r p rr k fr inv mask | it > lim =
  error "compare: conjugate gradient did not converge in {lim} iterations (relative residual {Num.sqrt rr})".
cg op lim tol it rz x r p rr k fr inv mask =
  n = op.n;
  (pc, p1) = copy n p;
  (q0, k2, fr2) = applyK op pc k fr;
  (mc, mask2) = copy n mask;
  q = Vec.zipMul q0 mc;
  (pq, p2, q2) = inner n p1 q;
  alpha = Num.div rz pq;
  (pc2, p3) = copy n p2;
  x2 = Vec.zipAdd x (Vec.scale alpha pc2);
  r2 = Vec.zipSub r (Vec.scale alpha q2);
  (rc, r3) = copy n r2;
  (rr2, r4, rc2) = inner n r3 rc;
  u = Vec.free rc2;
  (rc3, r5) = copy n r4;
  (ic, inv2) = copy n inv;
  z = Vec.zipMul rc3 ic;
  (rz2, r6, z2) = inner n r5 z;
  p4 = Vec.zipAdd z2 (Vec.scale (Num.div rz2 rz) p3);
  cg op lim tol (it + 1) rz2 x2 r6 p4 rr2 k2 fr2 inv2 mask2.

# ---- fixture, solve, report -------------------------------------------------

stations = [26.4, 74.4].
inPatch h x y z station = and (and (and (x >= 9) (x <= 21)) (N.abs (y - station) <= 5))
                              (N.abs (z - (44 + 0.105 * x - 0.0437 * y)) <= h * 0.75).

# the used nodes: (first DOFs of the free ones, the two load patches' nodes)
nodePass : unsafe {h : Int, a0 : Int, b0 : Int, c0 : Int, nj : Int, nk : Int} -> List (Int, List Int) -> (List Int, List Int, List Int) -> (List Int, List Int, List Int).
nodePass g nodes acc | nodes == [] = acc.
nodePass g nodes acc =
  (d, incs) :: rest = nodes;
  (free, p1, p2) = acc;
  nd = d / 3;
  i = nd / (g.nj * g.nk); j = (nd / g.nk) % g.nj; l = nd % g.nk;
  x = (g.a0 + i) * g.h; y = (g.b0 + j) * g.h; z = (g.c0 + l) * g.h;
  f2 = if and (y >= 165) (y <= 175) then free else d :: free;
  q1 = if inPatch g.h x y z 26.4 then nd :: p1 else p1;
  q2 = if inPatch g.h x y z 74.4 then nd :: p2 else p2;
  nodePass g rest (f2, q1, q2).

# mask (1 on free DOFs) and inverse diagonal (0 off them)
freeVectors : unsafe List Int -> Vector Int -> Vector Int -> Vector Int -> (Vector Int, Vector Int, Vector Int).
freeVectors free diag mask inv | free == [] = (diag, mask, inv).
freeVectors free diag mask inv =
  d :: rest = free;
  (a, d1) = Vec.get d diag; (b, d2) = Vec.get (d + 1) d1; (c, d3) = Vec.get (d + 2) d2;
  m = Vec.set (d + 2) 1.0 (Vec.set (d + 1) 1.0 (Vec.set d 1.0 mask));
  freeVectors rest d3 m (Vec.set (d + 2) (Num.div 1 c) (Vec.set (d + 1) (Num.div 1 b) (Vec.set d (Num.div 1 a) inv))).

addLoad : unsafe List Int -> Int -> Vector Int -> Vector Int.
addLoad nodes f v | nodes == [] = v.
addLoad nodes f v = nd :: rest = nodes; (old, v2) = Vec.get (3 * nd + 2) v; addLoad rest f (Vec.set (3 * nd + 2) (old + f) v2).

sumZ : unsafe List Int -> Int -> Vector Int -> (Int, Vector Int).
sumZ nodes acc v | nodes == [] = (acc, v).
sumZ nodes acc v = nd :: rest = nodes; (w, v2) = Vec.get (3 * nd + 2) v; sumZ rest (acc + w) v2.

vonMises s = [a, b, c, d, e, f] = s;
  Num.sqrt (Num.div ((a - b) * (a - b) + (b - c) * (b - c) + (c - a) * (c - a)) 2 + 3 * (d * d + e * e + f * f)).
chunks6 : unsafe List Int -> List (List Int).
chunks6 xs | xs == [] = [].
chunks6 xs = a :: b :: c :: d :: e :: f :: rest = xs; [a, b, c, d, e, f] :: chunks6 rest.
peak name xs = if xs == [] then error "compare: no bricks in the {name} zone" else List.fold (fn a b -> Numeric.max a b) (xs ! 0) xs.

solve : unsafe String -> String -> Int -> String.
solve folder name h =
  m = mesh folder name h;
  bricks = m.bricks;
  nb = List.len bricks;
  n = 3 * m.ni * m.nj * m.nk;
  offs = List.map (fn s -> 3 * ((((s ! 0) + 1) / 2) * m.nj * m.nk + (((s ! 1) + 1) / 2) * m.nk + ((s ! 2) + 1) / 2)) signs;
  nodes = incidence (List.map (fn b -> 3 * b.base) bricks) offs (n / 3);
  ke = stiffness h;
  kdiag = List.map (fn l -> (ke ! l) ! l) (List.range 0 23);
  # one value per brick column entry, in brick order (List.concat is quadratic)
  per f = Vec.fromList (List.rev (List.fold (fn acc b -> List.fold (fn a x -> x :: a) acc (f b)) [] bricks));
  (dq, diag) = scatterNodes nodes (per (fn b -> List.map (fn kl -> b.frac * kl) kdiag)) (zeros n);
  u0 = Vec.free dq;
  g = {h = h, a0 = m.a0, b0 = m.b0, c0 = m.c0, nj = m.nj, nk = m.nk};
  (free, p1, p2) = nodePass g nodes ([], [], []);
  u1 = if or (p1 == []) (p2 == []) then error "compare: a load patch has no nodes" else Unit;
  (diag2, mask, inv) = freeVectors free diag (zeros n) (zeros n);
  u2 = Vec.free diag2;
  f = addLoad p2 (Num.div 0.5 (List.len p2)) (addLoad p1 (Num.div 0.5 (List.len p1)) (zeros n));
  (mc, mask2) = copy n mask;
  b = Vec.zipMul f mc;
  (bc, b2) = copy n b;
  (bb, b3, bc2) = inner n b2 bc;
  u3 = Vec.free bc2;
  (ic, inv2) = copy n inv;
  (rc, r) = copy n b3;
  z = Vec.zipMul rc ic;
  (rz, r2, z2) = inner n r z;
  op = {n = n, bricks = nb, nodes = nodes};
  (u, iters) = cg op (3 * List.len free) (1.0e-20 * bb) 0 rz (zeros n) r2 z2 bb (Vec.fromList (List.concat ke)) (per (fn b -> List.map (fn l -> b.frac) kdiag)) inv2 mask2;
  (t1, uA) = sumZ p1 0 u;
  (t2, uB) = sumZ p2 0 uA;
  (uC, ue) = gatherNodes nodes uB (zeros (24 * nb));
  u2 = Vec.free uC;
  b0t = Vec.fromList (List.concat (transpose (strain h [0.0, 0.0, 0.0])));
  (se, ue2, b0t2) = Vec.mmul nb 24 6 ue b0t;
  u3 = Vec.free ue2; u4 = Vec.free b0t2;
  (st, se2, dv) = Vec.mmul nb 6 6 se (Vec.fromList (List.concat dRows));
  u5 = Vec.free se2; u6 = Vec.free dv;
  vm = List.map vonMises (chunks6 (Vec.toList st));
  rows = List.map (fn pr -> (bk, s) = pr; {y = (bk.cb + 0.5) * h, z = (bk.cc + 0.5) * h, vm = s}) (List.zip bricks vm);
  zone k = List.map (fn r -> r.vm) (List.filter (fn r -> and (N.abs (r.y - k) < 11) (r.z > 20)) rows);
  body = List.map (fn r -> r.vm) (List.filter (fn r -> and (r.y > 0) (r.y < 160)) rows);
  travel = [Num.div t1 (List.len p1), Num.div t2 (List.len p2)];
  pads = [peak "front saddle" (zone (stations ! 0)), peak "rear saddle" (zone (stations ! 1))];
  list xs = "[" + Str.join ", " (List.map N.repr xs) + "]";
  "\{\"mesh_mm\": {N.repr h}, \"n_elements\": {nb}, \"pad_travel_mm\": {list travel}, \"pad_peak_MPa\": {list pads}, \"body_peak_MPa\": {N.repr (peak "body" body)}\}".

usage = "usage: GLIDER_FEA_DIR=DIR compare.sol DESIGN CELL_MM".
main : unsafe List String -> Unit.
main argv = case argv of
    [name, cell] -> (case Try.parseNum (normalNum cell) of
        Ok h -> if h > 0 then print (solve (env "GLIDER_FEA_DIR" "/tmp") name (h * 1.0)) else error usage
      | Err e -> error usage)
  | _ -> error usage.
> main (args Unit).
