# Three-plate captive display mechanism. Dimensions in mm.
# Run from the CoScad root or this directory: sol build.sol
N = use "../lib/num".
R = use "../lib/root".

# Python str() of a number: integers print bare, other floats as repr.
ps x = if x == Num.floor x then str x else N.repr x.
disk r h x y z = "(zcyl " + ps r + " " + ps h + " |> move " + ps x + " " + ps y + " " + N.repr (z + Num.div h 2) + ")".
box w d h x y z = "(box " + ps w + " " + ps d + " " + ps h + " |> move " + ps x + " " + ps y + " " + N.repr (z + Num.div h 2) + ")".
poly pts h = "(⮕ " + ps h + " (poly " + Str.join " " (List.map (fn p -> (x, y) = p; N.fixed 4 x + " " + N.fixed 4 y) pts) + "))".
union parts = "(" + Str.join " ⊕ " parts + ")".
move s x y z = "(" + s + " |> move " + ps x + " " + ps y + " " + ps z + ")".
m1 = 0 - 1.

bigR = 10.
gmod = 1.
n = 20.
alpha = N.radians 20.
rb = bigR * N.cos alpha.
radii = [bigR - 1.25 * gmod] + List.map (fn j -> rb + Num.div ((bigR + gmod - rb) * j) 6) (List.range 0 6).
gear = List.concat (List.map (fn i ->
    List.concat (List.map (fn sd -> (side, rs) = sd;
      List.map (fn r ->
        a = N.acos (Numeric.min 1 (Num.div rb r));
        ang = Num.div ((i + 0.5) * 2 * N.pi) n + side * (Num.div N.pi (2 * n) + N.tan alpha - alpha - N.tan a + a);
        (r * N.cos ang, r * N.sin ang)) rs)
    [(m1, radii), (1, List.rev radii)]))
  (List.range 0 (n - 1))).

# Deck prints working face down. Four legs lift the servo off the bench.
legs = List.concat (List.map (fn x -> List.map (fn y -> box 8 8 32 x y 3) [0 - 21, 23]) [0 - 26, 26]).
deck = union ([box 62 54 3 0 0 0] + legs).
servoPocket = box 13.1 23.9 5 0 (0 - 5.4) m1.
servoScrews = List.map (fn y -> disk 0.95 5 0 y m1) [0 - 19.4, 8.6].
guides = List.map (fn x -> disk 1.6 5 x 18 m1) [0 - 10, 10].
base = List.fold (fn shape cut -> shape + " ⊖ " + cut) deck ([servoPocket] + servoScrews + guides).
pitch = N.pi * gmod.
tip = bigR + 1.25 * gmod.
root0 = bigR - 0.95 * gmod.
teeth = List.concat (List.map (fn i ->
    List.map (fn ys -> (y, side) = ys; (i * pitch + side * (Num.div pitch 4 - 0.11 + (y - bigR) * N.tan alpha), y))
      [(tip, m1), (root0, m1), (root0, 1), (tip, 1)])
  (List.range (0 - 7) 7)).
rack = [(0 - 24, 23), (0 - 24, tip)] + teeth + [(24, tip), (24, 23)].
# 47 mm slot minus 20 mm screw spacing and 3 mm shank = 24 mm travel.
slider = poly rack 4 + " ⊖ " + box 47 3.55 6 0 18 m1.
# Horn recess prints facing up, then faces the servo when assembled.
pinion = poly gear 5 + " ⊖ " + disk 1.25 7 0 0 m1 + " ⊖ " + box 19 5.8 3 0 0 2.6 + " ⊖ " + disk 3.6 3 0 0 2.6.
spacers = List.map (fn x -> disk 3 0.8 x 0 0 + " ⊖ " + disk 1.65 3 x 0 m1) [0 - 4, 4].
plate = union ([base, move slider 0 21 0, move pinion 47 0 0, move (union spacers) 45 17 0]).
> root = R.modelDir "models/sg90-rack" "build.sol";
  writePath "{root}/plate.coscad" ("main = " + plate + "\n").
