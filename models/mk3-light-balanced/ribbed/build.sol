# Generate a tapered carry-through, two skins and intersecting spar/rib webs.
N = use "../../lib/num".
fmt = N.trim.
post r h x y z = "(zcyl " + fmt r + " " + fmt h + " |> move " + fmt x + " " + fmt y + " " + fmt z + ")".
mid x = (x - 3)*0.1 - 4.
leading x = (x - 3)*85/110.0 - 40.
height x = 3 + 3*N.clamp 0 1 (Num.div (20 - x) 17).
edge x y = post 1.5 (height x) x y (mid x).
# The thicker section continues into the fuselage at constant root thickness.
root = Str.join " ⇓ " [post 1.5 6 0 (0 - 40) (0 - 4),post 1.5 6 0 45 (0 - 4),edge 3 (0 - 40),edge 3 45,edge 20 (leading 20),edge 20 45].
# Each trapezoidal bay leaves a full-depth web at either span station.
# Fixed fractional chord stations keep the spars continuous through the ribs.
bayPoint x fraction inset =
  y = leading x + (45 - leading x)*fraction + inset;
  post 0.12 (height x - 1.20) x y (mid x).
cell a b j =
  lo = Num.div j 10; hi = Num.div (j+1) 10;
  Str.join " ⇓ " [bayPoint a lo 0.65,bayPoint a hi (0 - 0.65),bayPoint b lo 0.65,bayPoint b hi (0 - 0.65)].
bay a b = List.map (fn j -> "(" + cell a b j + ")") (List.range 0 9).
> source = Str.lines (readPath "../glider.coscad");
  prefix = List.filter (fn s -> not (Str.startsWith "cell" s or Str.startsWith "lightR =" s or Str.startsWith "wings =" s or Str.startsWith "main =" s)) source;
  ranges = [(7,19.4),(20.6,34),(35.2,49),(50.2,64),(65.2,79),(80.2,92)];
  cutters = List.concat (List.map (fn r -> (a,b)=r; bay a b) ranges);
  defs = Str.join "\n" prefix + "\nroot = " + root + "\nlightR = (wingR ⊕ root) ⊖ (" + Str.join " ⊕ " cutters + ")\n";
  end = List.filter (fn s -> Str.startsWith "wings =" s or Str.startsWith "main =" s) source;
  writePath "glider.coscad" (defs + Str.join "\n" end + "\n").
