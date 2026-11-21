# Run the twin generator first. Reuse the proven sockets, same-handed threads
# and the corrected 106.7 mm screw from its replacement-pair model.

R = use "../../lib/root".

move part x y = "(" + part + " |> move " + str x + " " + str y + " 0)".

copies part axes = List.map (fn xy -> (x,y) = xy; move part x y) axes.

join parts = "(" + Str.join " ⊕ " parts + ")".

holes shape tool axes = shape + " ⊖ " + Str.join " ⊖ " (copies tool axes).

variant dir name width depth axes base cap printParts =
  original = readPath "{dir}/../twin/tall-screws.coscad";
  keep line = List.any (fn key -> Str.startsWith (key + " =") line) ["shell","side","endrib","socket","feet","frame","post","stop","key","male","hub","journal","screw","female","nut","bearing","keyhole"];
  deck = "(box " + str width + " " + str depth + " 12 |> move 0 0 6)";
  platform = holes (join ([deck] + copies "(zcyl 9 12 |> move 0 0 6)" axes)) "nut" axes;
  lines = List.filter keep (Str.lines original) + ["base = " + base,"platform = " + platform,"cap = " + cap];
  writePath "{dir}/{name}/plate.coscad" (Str.join "\n" (lines + ["main = " + join printParts,""])).

> dir = R.modelDir "models/sg90-lift/edge" "build.sol";
  twin = [(0 - 20,0),(20,0)];
  original = Str.lines (readPath "{dir}/../twin/plate.coscad");
  value key = case List.filter (Str.startsWith (key + " = ")) original of [line] -> Str.sub line (Str.len key + 3) (Str.len line) | _ -> error key;
  _ = variant dir "twin" 40 48 twin (value "base") (value "cap") ["base",move "platform" 70 0,move "cap" 0 65,move "screw" 55 58,move "screw" 80 58];
  quad = [(0 - 40,0),(40,0),(0,0 - 32),(0,32)];
  corners = [(0 - 49,0 - 44),(49,0 - 44),(0 - 49,44),(49,44)];
  # Turn each socket so the cable exits toward the outside of the frame.
  sockets = [move "(socket |> rotz 90)" (0 - 40) 0,move "(socket |> rotz -90)" 40 0,move "(socket |> rotz 180)" 0 (0 - 32),move "socket" 0 32];
  upright = "((box 9 9 132.2 |> move 0 0 66.1) ⊕ (box 5 5 5.8 |> move 0 0 135))";
  stops = copies "(box 8 8 46 |> move 0 0 23)" [(0 - 30,24),(30,24),(0 - 30,0 - 24),(30,0 - 24)];
  base = join (["(box 109 99 3 |> move 0 0 1.5)"] + sockets + copies upright corners + stops);
  ring = "((box 109 99 6 |> move 0 0 3) ⊖ (box 62 46 8 |> move 0 0 3))";
  cap = holes ("(" + holes ring "bearing" quad + ")") "(box 4.95 4.95 8 |> move 0 0 3)" corners;
  variant dir "quad" 80 64 quad base cap (["base",move "platform" 112 0,move "cap" 0 110] + copies "screw" [(82,95),(105,95),(128,95),(151,95)]).
