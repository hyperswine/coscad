# Build closed-shell wings with narrow staggered cells.
#   "$SOL" models/bolted-glider-light/supported/build.sol   (from the CoScad root or this directory)
# Writes wing-right.coscad and wing-left.coscad here.
N = use "../../lib/num".
R = use "../../lib/root".
W = use "../wing-sections".

# Chord fractions. Paired cavities leave two continuous spars, plus edge rails.
bays = [(0.012,0.29),(0.302,0.66),(0.672,0.976)].
openings s = W.openings 0.45 bays s.

# Keep the same profile vertex correspondence through every loft.
ends a b = [a, b].
cutter sts s lo hi = W.shellCutter sts 0.42 ends s lo hi.

ceil x = 0 - Num.floor (0 - x).
ribIndex station = List.len (List.filter (fn r -> r < station) W.ribs).
# Cell boundary j of `count` at a rib station; internal webs alternate
# +-0.65 mm between successive ribs.
edge lo hi count j station chord =
  u = lo + Num.div ((hi - lo) * j) count;
  sign = if ribIndex station % 2 == 0 then 1 else 0 - 1;
  if and (0 < j) (j < count) then u + Num.div (sign * 0.65) chord else u.
cellBounds lo hi count j station chord =
  left = edge lo hi count j station chord + (if j != 0 then Num.div 0.25 chord else 0);
  right = edge lo hi count (j + 1) station chord - (if j + 1 < count then Num.div 0.25 chord else 0);
  (left, right).

# Partition every original bay into narrow cells. Adjacent stations stagger
# the internal webs to make a continuous zigzag through each rib.
cutsFor sts pair =
  (a, b) = pair;
  ca = W.chordOf (W.section sts a);
  cb = W.chordOf (W.section sts b);
  if openings (Num.div (a + b) 2) == [] then [] else
    List.concat (List.map (fn bay -> (lo, hi) = bay;
      count = ceil (Num.div ((hi - lo) * Numeric.max ca cb) 7);
      List.map (fn j ->
          (l0, r0) = cellBounds lo hi count j a ca;
          (l1, r1) = cellBounds lo hi count j b cb;
          "(θ 90 (loft " + cutter sts (a + 0.45) l0 r0 + " " + cutter sts (b - 0.45) l1 r1 + "))")
        (List.range 0 (count - 1))) bays).

> root = R.modelDir "models/bolted-glider-light/supported" "build.sol";
  source = W.mainBody (readPath "{root}/../../bolted-glider-balanced/wing-right.coscad");
  sts = W.stations source;
  W.writeFrames root source (List.concat (List.map (cutsFor sts) W.ribPairs)).
