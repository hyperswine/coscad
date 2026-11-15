# Shared numerics for model generators and studies (Sol has + - * /, Num.sqrt,
# Num.floor, Num.round, Num.div and Numeric.abs/min/max, but no libm).
#
#   N = use "../lib/num".      N.sin x   N.fixed 4 x   N.repr x
#
# Trig and exp use Cody-Waite argument reduction and Taylor polynomials on a
# small interval; results agree with the platform libm to about 1 ulp, which
# is enough for fixed-decimal output. Formatting follows Python: `fixed n x`
# is f"{x:.nf}", `trim x` is the CoScad generators' f() (4 places, trailing
# zeros dropped), `repr x` is repr(float(x)).
#
# Sol numbers: `/` floors when both operands are integers (7 / 2 = 3); use
# Num.div for Python's true division. Limits: Sol has no negative zero, so
# atan2 (-0.0) x<0 returns +pi; `fixed` is exact while |x|*10^n < 2^53.

pi = 3.141592653589793.
halfPi = 1.5707963267948966.
# pi/2 in three parts (fdlibm pio2_1, pio2_2, pio2_2t); the first two have 33
# significant bits, so k*part is exact for |k| < 2^20.
piO2Hi = 1.5707963267341256.
piO2Mid = 6.077100506303966e-11.
piO2Lo = 2.0222662487959506e-21.
ln2Hi = 0.6931471803691238.
ln2Lo = 1.9082149292705877e-10.

radians d = Num.div (d * pi) 180.
degrees r = Num.div (r * 180) pi.
abs x = if x < 0 then 0 - x else x.
sign x = if x < 0 then 0 - 1 else 1.
hypot a b = Num.sqrt (a * a + b * b).
clamp lo hi x = if x < lo then lo else if x > hi then hi else x.
square x = x * x.

# Horner over coefficients from highest to lowest power.
horner cs x = List.fold (fn acc c -> acc * x + c) 0 cs.

# sin and cos on |r| <= pi/4 (Taylor to degree 17 and 18).
sinCoeffs = [1.0 / 355687428096000.0, 0.0 - 1.0 / 1307674368000.0, 1.0 / 6227020800.0,
             0.0 - 1.0 / 39916800.0, 1.0 / 362880.0, 0.0 - 1.0 / 5040.0, 1.0 / 120.0, 0.0 - 1.0 / 6.0, 1.0].
cosCoeffs = [0.0 - 1.0 / 6402373705728000.0, 1.0 / 20922789888000.0, 0.0 - 1.0 / 87178291200.0,
             1.0 / 479001600.0, 0.0 - 1.0 / 3628800.0, 1.0 / 40320.0, 0.0 - 1.0 / 720.0,
             1.0 / 24.0, 0.0 - 0.5, 1.0].
# Horner unrolled with literal coefficients (top-level values are re-evaluated
# on every reference in Sol 0.13, so constants are inlined here).
kSin r = x = r * r; r * ((((((((2.8114572543455206e-15 * x - 7.647163731819816e-13) * x + 1.6059043836821613e-10) * x - 2.505210838544172e-8) * x + 2.7557319223985893e-6) * x - 0.0001984126984126984) * x + 0.008333333333333333) * x - 0.16666666666666666) * x + 1.0).
kCos r = x = r * r; ((((((((((0.0 - 1.5619206968586225e-16) * x + 4.779477332387385e-14) * x - 1.1470745597729725e-11) * x + 2.08767569878681e-9) * x - 2.755731922398589e-7) * x + 2.48015873015873e-5) * x - 0.001388888888888889) * x + 0.041666666666666664) * x - 0.5) * x + 1.0).

# Reduce x to r in [-pi/4, pi/4] with x = k*pi/2 + r; returns (k mod 4, r).
reduce x =
  k = Num.round (Num.div x halfPi);
  r = ((x - k * piO2Hi) - k * piO2Mid) - k * piO2Lo;
  (k % 4, r).
quadrantSin q r | q == 0 = kSin r.
quadrantSin q r | q == 1 = kCos r.
quadrantSin q r | q == 2 = 0 - kSin r.
quadrantSin q r = 0 - kCos r.
sin x = (q, r) = reduce x; quadrantSin q r.
cos x = (q, r) = reduce x; quadrantSin ((q + 1) % 4) r.
tan x = Num.div (sin x) (cos x).

# atan: atan(x) = pi/2 - atan(1/x) for |x| > 1, then two half-angle steps
# (atan x = 2 atan(x / (1 + sqrt(1 + x^2)))) bring |x| under 0.2, where the
# series converges quickly.
atanSeries x =
  x2 = x * x;
  (acc, term) = List.fold (fn st n -> (a, t) = st; (a + Num.div t n, 0 - t * x2)) (0, x) (List.map (fn i -> 2 * i + 1) (List.range 0 20));
  acc.
atanSmall x =
  h1 = Num.div x (1 + Num.sqrt (1 + x * x));
  h2 = Num.div h1 (1 + Num.sqrt (1 + h1 * h1));
  4 * atanSeries h2.
atan x | x > 1 = halfPi - atanSmall (Num.div 1 x).
atan x | x < 0 - 1 = 0 - halfPi - atanSmall (Num.div 1 x).
atan x = atanSmall x.
atan2 y x | x > 0 = atan (Num.div y x).
atan2 y x | and (x < 0) (y >= 0) = atan (Num.div y x) + pi.
atan2 y x | x < 0 = atan (Num.div y x) - pi.
atan2 y x | y > 0 = halfPi.
atan2 y x | y < 0 = 0 - halfPi.
atan2 y x = 0.
asin x = atan2 x (Num.sqrt (1 - x * x)).
acos x = atan2 (Num.sqrt (1 - x * x)) x.

# exp: x = k ln2 + r, |r| <= ln2/2, Taylor to degree 20, then scale by 2^k.
pow2 k =
  base = if k >= 0 then 2 else 0.5;
  List.fold (fn acc i -> acc * base) 1 (List.range 1 (abs k)).
expSeries r =
  (acc, term) = List.fold (fn st n -> (a, t) = st; t2 = Num.div (t * r) n; (a + t2, t2)) (1, 1) (List.range 1 20);
  acc.
exp x =
  k = Num.round (Num.div x 0.6931471805599453);
  r = (x - k * ln2Hi) - k * ln2Lo;
  expSeries r * pow2 k.
# natural log by Newton on exp (enough for occasional use)
# natural log: Newton on exp from a bit-length estimate
log x =
  _ = if x <= 0 then error "log of a non-positive number" else Unit;
  List.fold (fn y i -> y - 1 + Num.div x (exp y)) (if x > 1 then x - 1 else 0 - 1) (List.range 1 60).
pow x y = exp (y * log x).

# ---- formatting ---------------------------------------------------------------

take n s = Str.slice s 0 n.
drop n s = Str.slice s n (Str.len s).

pow10 n = List.fold (fn acc i -> acc * 10) 1 (List.range 1 n).
zeros n = Str.join "" (List.map (fn i -> "0") (List.range 1 n)).
padLeft w c s = if Str.len s >= w then s else Str.join "" (List.map (fn i -> c) (List.range 1 (w - Str.len s))) + s.
digitsOf n = str n.
# Exact product a*b = p + e (Dekker), used to settle rounding ties exactly.
splitHi a = c = 134217729.0 * a; c - (c - a).
twoProd a b =
  p = a * b;
  ah = splitHi a; al = a - ah; bh = splitHi b; bl = b - bh;
  (p, ((ah * bh - p) + ah * bl + al * bh) + al * bl).
# Python f"{x:.nf}" rounds the exact binary value, half to even on true ties.
# Valid while |x| * 10^n stays below 2^53 (about 9e11 at four places).
roundScaled n ax =
  (sv, err) = twoProd ax (pow10 n);
  k = Num.floor sv;
  frac = sv - k;
  if frac > 0.5 then k + 1 else if frac < 0.5 then k
  else if err > 0 then k + 1 else if err < 0 then k
  else if k % 2 == 0 then k else k + 1.
fixed n x =
  neg = x < 0;
  scaled = roundScaled n (abs x);
  whole = digitsOf (scaled / pow10 n);
  body = if n == 0 then whole else
    frac = padLeft n "0" (digitsOf (scaled % pow10 n));
    "{whole}.{frac}";
  if neg then "-" + body else body.
# The generators' f(): fixed 4, trailing zeros and a bare point dropped.
# drop trailing zeros after a decimal point, then a bare point
countTrailing c s = List.fold (fn acc i -> if and (acc == i - 1) (Str.at s (Str.len s - i) == c) then i else acc) 0 (List.range 1 (Str.len s)).
countLeading c s = List.fold (fn acc i -> if and (acc == i) (Str.at s i == c) then i + 1 else acc) 0 (List.range 0 (Str.len s - 1)).
stripZeros s =
  t = if Str.contains "." s then take (Str.len s - countTrailing 48 s) s else s;
  if Str.endsWith "." t then take (Str.len t - 1) t else t.
trim x =
  s = stripZeros (fixed 4 x);
  if s == "-0" then "0" else s.

# Python repr of a float, from Sol's shortest-digit rendering.
splitExp s = case Str.split 101 s of [m, e] -> (m, unwrap (Try.parseInt e)) | _ -> (s, 0).
mantissaDigits m =
  (neg, body) = if Str.startsWith "-" m then (True, drop 1 m) else (False, m);
  parts = Str.split 46 body;
  (whole, frac) = case parts of [w, fr] -> (w, fr) | [w] -> (w, "") | _ -> (body, "");
  (neg, whole, frac).
dropTrailingZeros s = take (Str.len s - countTrailing 48 s) s.
dropLeadingZeros s = drop (countLeading 48 s) s.
# digits d (no point) with decimal exponent e10 meaning value = 0.d * 10^e10
positional neg d e10 =
  n = String.len d;
  body = if e10 <= 0 then "0." + zeros (0 - e10) + d
         else if e10 >= n then d + zeros (e10 - n) + ".0"
         else take e10 d + "." + drop e10 d;
  if neg then "-" + body else body.
scientific neg d e10 =
  e = e10 - 1;
  m = if String.len d == 1 then d else take 1 d + "." + drop 1 d;
  es = if e < 0 then "-" + padLeft 2 "0" (str (0 - e)) else "+" + padLeft 2 "0" (str e);
  (if neg then "-" else "") + m + "e" + es.
repr x =
  (m, e) = splitExp (str x);
  (neg, whole, frac) = mantissaDigits m;
  raw = whole + frac;
  lead = String.len raw - String.len (dropLeadingZeros raw);
  d0 = dropTrailingZeros (dropLeadingZeros raw);
  d = if d0 == "" then "0" else d0;
  e10 = if d0 == "" then 1 else String.len whole + e - lead;
  # Python switches to exponent form when the exponent is < -4 or >= 16
  if d0 == "" then (if neg then "-0.0" else "0.0")
  else if or (e10 - 1 < 0 - 4) (e10 - 1 >= 16) then scientific neg d e10
  else positional neg d e10.

