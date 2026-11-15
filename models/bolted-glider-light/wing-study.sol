# Build film-covered wing frames and compare spanwise bending by beam elements.
#   "$SOL" models/bolted-glider-light/wing-study.sol   (from the CoScad root or this directory)
# Writes wing-right.coscad and wing-left.coscad here and prints the comparison.
N = use "../lib/num".
R = use "../lib/root".
W = use "wing-sections".

# Chord fractions. Paired cavities leave two continuous spars, plus edge rails.
bays = [(0.025,0.28),(0.315,0.65),(0.675,0.97)].
openings s = W.openings 0.6 bays s.

cutter sts s lo hi =
  p = W.section sts s;
  x = W.fst (p ! 0);
  c = W.chordOf p;
  a = N.fixed 5 (x + c * lo);
  b = N.fixed 5 (x + c * hi);
  "{W.fmtG s} (poly {a} -1 {b} -1 {b} 15 {a} 15)".

cutsFor sts pair =
  (a, b) = pair;
  if openings (Num.div (a + b) 2) == [] then [] else
    List.map (fn bay -> (lo, hi) = bay;
      "(θ 90 (loft " + cutter sts (a + 0.6) lo hi + " " + cutter sts (b - 0.6) lo hi + "))") bays.

# Nominal slicer shells and homogenized 8% core. Thin rails bounded by two
# perimeters on both sides become solid locally. Candidate strips inside an
# opening are left out.
stripLayers candidate open edges u c h bot =
  if and candidate (List.any (fn lr -> (l, r) = lr; and (l < u) (u < r)) open) then [] else
    near = List.fold (fn m e -> Numeric.min m (N.abs (u - e) * c)) 1000000 edges;
    core = if near < 0.84 then 1 else 0.08;
    lower = Numeric.min h (bot + 0.4);
    top = Numeric.max lower (h - 0.6);
    [(bot, lower, 1), (lower, top, core), (top, h, 1)].
layers candidate s =
  open = openings s;
  edges = [0, 1] + (if candidate then List.concat (List.map (fn lr -> (l, r) = lr; [l, r]) open) else []);
  fn u c h bot -> stripLayers candidate open edges u c h bot.

> root = R.modelDir "models/bolted-glider-light" "wing-study.sol";
  source = W.mainBody (readPath "{root}/../bolted-glider-balanced/wing-right.coscad");
  sts = W.stations source;
  _ = W.writeFrames root source (List.concat (List.map (cutsFor sts) W.ribPairs));
  _ = W.report sts (layers False) (layers True);
  Unit.
