# Build closed-shell wings and compare spanwise bending by beam elements.
#   "$SOL" models/bolted-glider-light/shell/wing-study.sol   (from the CoScad root or this directory)
# Writes wing-right.coscad and wing-left.coscad here and prints the comparison.
N = use "../../lib/num".
R = use "../../lib/root".
W = use "../wing-sections".

# Chord fractions. Paired cavities leave two continuous spars, plus edge rails.
bays = [(0.012,0.29),(0.302,0.66),(0.672,0.976)].
openings s = W.openings 0.45 bays s.

# Keep the same profile vertex correspondence through every loft.
quarters a b = List.map (fn i -> a + Num.div ((b - a) * i) 3) [0, 1, 2, 3].
cutter sts s lo hi = W.shellCutter sts 0.26 quarters s lo hi.

cutsFor sts pair =
  (a, b) = pair;
  if openings (Num.div (a + b) 2) == [] then [] else
    List.map (fn bay -> (lo, hi) = bay;
      "(θ 90 (loft " + cutter sts (a + 0.45) lo hi + " " + cutter sts (b - 0.45) lo hi + "))") bays.

# Nominal slicer shells and homogenized core (8%, or 5% with the thinner
# candidate skins). Thin rails bounded by two perimeters on both sides become
# solid locally; candidate strips inside an opening are hollow under a
# 0.26 mm skin.
stripLayers candidate open edges u c h bot =
  hollow = and candidate (List.any (fn lr -> (l, r) = lr; and (l < u) (u < r)) open);
  near = List.fold (fn m e -> Numeric.min m (N.abs (u - e) * c)) 1000000 edges;
  core = if hollow then 0 else if near < (if candidate then 0.42 else 0.84) then 1 else (if candidate then 0.05 else 0.08);
  lower = Numeric.min h (bot + (if candidate then 0.2 else 0.4));
  top = Numeric.max lower (h - (if and candidate (core == 0) then 0.26 else (if candidate then 0.4 else 0.6)));
  [(bot, lower, 1), (lower, top, core), (top, h, 1)].
layers candidate s =
  open = openings s;
  edges = [0, 1] + (if candidate then List.concat (List.map (fn lr -> (l, r) = lr; [l, r]) open) else []);
  fn u c h bot -> stripLayers candidate open edges u c h bot.

> root = R.modelDir "models/bolted-glider-light/shell" "wing-study.sol";
  source = W.mainBody (readPath "{root}/../../bolted-glider-balanced/wing-right.coscad");
  sts = W.stations source;
  _ = W.writeFrames root source (List.concat (List.map (cutsFor sts) W.ribPairs));
  _ = W.report sts (layers False) (layers True);
  Unit.
