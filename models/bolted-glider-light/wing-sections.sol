# Shared helpers for the light-glider wing studies (wing-study.sol,
# shell/wing-study.sol, supported/build.sol): loft stations read from the
# balanced glider's right wing, interpolated sections, CoScad formatting and
# the cantilever beam comparison.
N = use "../lib/num".
L = use "std/list".
O = use "std/order".

ribs = [0,8,20,31,49,67,85,103,121,139,157,175,193,206,218,227,236].

# ---- loft stations ------------------------------------------------------------

isRunChar c = or (and (c >= 48) (c <= 57)) (c == 46).
isPointChar c = or (isRunChar c) (c == 32).
leadingRun p s = List.fold (fn acc i -> if and (acc == i) (p (Str.at s i)) then i + 1 else acc) 0 (List.range 0 (Str.len s - 1)).
trailingRun p s =
  n = Str.len s;
  List.fold (fn acc i -> if and (acc == i - 1) (p (Str.at s (n - i))) then i else acc) 0 (List.range 1 n).

# The text between the first and second "main = ", stripped (Python's
# text.split("main = ")[1].strip()).
mainBody text =
  i = Str.indexOf "main = " text;
  _ = if i < 0 then error "no main in the wing source" else Unit;
  rest = N.drop (i + 7) text;
  j = Str.indexOf "main = " rest;
  Str.trim (if j < 0 then rest else N.take j rest).

number s = case Try.parseNum s of Ok v -> v * 1.0 | Err e -> error "bad number in loft: {s}".
pairUp values = List.map (fn i -> (values ! (2 * i), values ! (2 * i + 1))) (List.range 0 (List.len values / 2 - 1)).

# Every `S (poly X Y X Y ...)` in the source, as re.findall(r'([\d.]+)
# \(poly ([\d. ]+)\)') sees it: S is the run of digits and points before
# " (poly", the points run up to the closing parenthesis.
stationOf pair =
  (before, piece) = pair;
  body = N.drop 5 piece;
  k = leadingRun isPointChar body;
  head = if Str.endsWith " " before then N.take (Str.len before - 1) before else "";
  run = trailingRun isRunChar head;
  closed = if and (k > 0) (k < Str.len body) then Str.at body k == 41 else False;
  if and (Str.startsWith "poly " piece) (and closed (run > 0))
  then [(number (N.drop (Str.len head - run) head), pairUp (List.map number (Str.words (N.take k body))))]
  else [].
stations source =
  pieces = Str.split 40 source;
  List.concat (List.map stationOf (List.zip (List.take (List.len pieces - 1) pieces) (List.drop 1 pieces))).

# ---- sections -----------------------------------------------------------------

bisectRight xs v = List.len (List.filter (fn x -> x <= v) xs).
bisectLeft xs v = List.len (List.filter (fn x -> x < v) xs).
fst p = (a, b) = p; a.
snd p = (a, b) = p; b.
lerp x u s a b = x + Num.div ((u - x) * (s - a)) (b - a).

section sts s =
  i = N.clamp 0 (List.len sts - 2) (bisectRight (List.map fst sts) s - 1);
  (a, p) = sts ! i;
  (b, q) = sts ! (i + 1);
  List.map (fn pq -> ((x, y), (u, v)) = pq; (lerp x u s a b, lerp y v s a b)) (List.zip p q).
chordOf p = fst (p ! 2) - fst (p ! 0).

# Leading-edge point plus the upper surface, sorted like Python tuples.
upper p = L.sortWith (O.thenBy (O.by snd O.int) (O.by fst O.int)) ([p ! 0] + List.drop 3 p).

nearRib tol s = List.fold (fn m r -> Numeric.min m (N.abs (s - r))) 1000000 ribs < tol.
# Openings at span s: none at ribs, in the 8-20 mm root bay or past 227 mm.
openings tol bays s = if or (nearRib tol s) (or (and (8 < s) (s < 20)) (s > 227)) then [] else bays.
ribPairs = List.zip (List.take (List.len ribs - 1) ribs) (List.drop 1 ribs).

# ---- formatting ---------------------------------------------------------------

# Python f"{s:g}" for 0.1 <= s < 1e6 (six significant digits, zeros dropped).
intDigits s = Str.len (str (Num.floor s)).
fmtG s =
  _ = if or (s < 0.1) (s >= 1000000) then error "station out of range for {s}" else Unit;
  places = if s >= 1 then 6 - intDigits s else 6;
  N.stripZeros (N.fixed places s).

# The closed-shell cutter: a profile under the upper skin, `depth` below it,
# over the bottom skin, through the points `xsOf a b` (listed root to tip and
# written in reverse).
upperHeight up q =
  i = bisectLeft (List.map fst up) q - 1;
  _ = if i < 0 then error "cutter ahead of the leading edge" else Unit;
  (x0, z) = up ! i;
  (u, w) = up ! (i + 1);
  z + Num.div ((w - z) * (q - x0)) (u - x0).
bottomSkin p q = Numeric.max 0.0 (snd (p ! 0) - (q - fst (p ! 0))) + 0.2.
shellCutter sts depth xsOf s lo hi =
  p = section sts s;
  x = fst (p ! 0);
  c = chordOf p;
  a = x + c * lo;
  b = x + c * hi;
  up = upper p;
  profile = [(a, bottomSkin p a), (b, bottomSkin p b)]
    + List.map (fn q -> (q, Numeric.max (bottomSkin p q + 0.02) (upperHeight up q - depth))) (List.rev (xsOf a b));
  "{fmtG s} (poly " + Str.join " " (List.map (fn qz -> (q, z) = qz; "{N.fixed 5 q} {N.fixed 5 z}") profile) + ")".

# Write the frame (outline minus cuts) as the right wing and its mirror.
writeFrames root source cuts =
  frame = "outline = " + source + "\nframe = outline ⊖ (" + Str.join " ⊕ " cuts + ")\n";
  _ = writePath "{root}/wing-right.coscad" (frame + "main = frame\n");
  writePath "{root}/wing-left.coscad" (frame + "main = ⇋ 0 1 0 frame\n").

# ---- beam comparison ----------------------------------------------------------

# r**3 rounded once (as libm pow does), via an exact double-double product.
cube r =
  (p, e) = N.twoProd r r;
  (h, l) = N.twoProd p r;
  h + (l + e * r).

first1 xs = case xs of x :: others -> x | [] -> error "empty list".
advance x rest = case rest of
    seg :: more -> (if and (more != []) (fst (snd seg) <= x) then more else rest)
  | [] -> rest.

# Section bending inertia and extreme fibre distance. `layers s` gives a
# function of (chord fraction u, chord, height h, bottom) returning the
# (bottom, top, density) layers of that strip, or [] to leave it out.
properties sts layers s =
  p = section sts s;
  x0 = fst (p ! 0);
  y0 = snd (p ! 0);
  c = chordOf p;
  up = upper p;
  segs = List.zip up (List.drop 1 up);
  strip = layers s;
  width1 = Num.div c 160;
  # Strips move tip-ward, so the upper-surface segment (bisect_right capped at
  # the last segment) only advances: carry the remaining segments.
  (area, first, second, left) = List.fold (fn acc j ->
      (ar0, fi0, se0, rest0) = acc;
      u = Num.div (j + 0.5) 160;
      x = x0 + c * u;
      rest = advance x (advance x (advance x rest0));
      ((a, z), (b, w)) = first1 rest;
      _ = if and (b <= x) (List.len rest > 1) then error "upper surface sampled too coarsely" else Unit;
      h = z + Num.div ((w - z) * (x - a)) (b - a);
      bot = Numeric.max 0.0 (y0 - (x - x0));
      # Layers are contiguous, so each boundary cube is reused by the next layer.
      (ar, fi, se, rp, cp) = List.fold (fn sums layer ->
          (ar1, fi1, se1, rPrev, cPrev) = sums;
          (l, r, d) = layer;
          width = width1 * d;
          cl = if l == rPrev then cPrev else cube l;
          cr = cube r;
          (ar1 + width * (r - l), fi1 + Num.div (width * (r * r - l * l)) 2, se1 + Num.div (width * (cr - cl)) 3, r, cr))
        (ar0, fi0, se0, 0 - 1, 0) (strip u c h bot);
      (ar, fi, se, rest))
    (0.0, 0.0, 0.0, segs) (List.range 0 159);
  neutral = Num.div first area;
  top = List.fold (fn m pt -> Numeric.max m (snd pt)) (snd (up ! 0)) up;
  (second - area * (neutral * neutral), Numeric.max neutral (top - neutral)).

# Euler-Bernoulli cantilever elements, each solved from its known left nodal
# motion; Hermite element with a consistent uniform load.
solve sts layers step =
  len = 236;
  clamp = 20;
  n = Num.round (Num.div (len - clamp) step);
  dz = Num.div (len - clamp) n;
  load = Num.div 1.0 (len - clamp);
  (delta, slope, stress) = List.fold (fn st j ->
      (d0, s0, p0) = st;
      s = clamp + (j + 0.5) * dz;
      (inertia, extreme) = properties sts layers s;
      moment = Num.div (load * ((len - s) * (len - s))) 2;
      free = len - s - Num.div dz 2;
      shear = load * (free + Num.div dz 2);
      endMoment = load * (Num.div (free * free) 2 - Num.div (dz * dz) 12);
      ei = 2500 * inertia;
      d1 = d0 + (s0 * dz + Num.div (Num.div (shear * cube dz) 3 + Num.div (endMoment * (dz * dz)) 2) ei);
      s1 = s0 + Num.div (Num.div (shear * (dz * dz)) 2 + endMoment * dz) ei;
      (d1, s1, Numeric.max p0 (Num.div (moment * extreme) inertia)))
    (0.0, 0.0, 0.0) (List.range 0 (n - 1));
  "\{'tip_mm': {N.repr delta}, 'peak_nominal_MPa': {N.repr stress}\}".

# Python's `print(step, solve(False, step), solve(True, step))`.
report sts baseline candidate =
  List.map (fn st -> (label, step) = st; print "{label} {solve sts baseline step} {solve sts candidate step}")
    [("1", 1), ("0.25", 0.25), ("0.125", 0.125)].
