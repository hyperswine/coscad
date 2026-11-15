# Generate the 1.5x short-travel hand-operated demonstrator, dimensions in mm.
# Run from the CoScad root or this directory: sol build.sol
N = use "../../lib/num".
R = use "../../lib/root".

# Python str() of a number: integers print bare, other floats as repr.
ps x = if x == Num.floor x then str x else N.repr x.
profile points height = "(⮕ " + ps height + " (poly " + Str.join " " (List.map (fn p -> (x, y) = p; N.fixed 4 x + " " + N.fixed 4 y) points) + "))".
circle r h x y z = "(zcyl " + ps r + " " + ps h + " |> move " + ps x + " " + ps y + " " + N.repr (z + Num.div h 2) + ")".
box w d h x y z = "(box " + ps w + " " + ps d + " " + ps h + " |> move " + ps x + " " + ps y + " " + N.repr (z + Num.div h 2) + ")".
m1 = 0 - 1.

# Archimedean strip joined to the hub and an off-axis anchor eye.
turn = 2 * N.pi * 1.5.
spiral = List.concat (List.map (fn sd -> (side, indices) = sd;
    List.map (fn i -> t = Num.div (turn * i) 180; r = 3.0 + Num.div (7 * t) turn + side * 0.30; (r * N.cos t, r * N.sin t)) indices)
  [(1, List.range 0 180), (m1, List.rev (List.range 0 180))]).
spring = profile spiral 1.2.
eye = circle 2.4 1.2 (0 - 11) 0 0.

# Module 1, 16 teeth. Approximate involute flanks with six segments.
bigR = 8.
alpha = N.radians 20.
rb = bigR * N.cos alpha.
halfangle r =
  a = N.acos (Numeric.min 1 (Num.div rb r));
  Num.div N.pi 32 + (N.tan alpha - alpha) - (N.tan a - a).
radii = [6.75] + List.map (fn j -> rb + Num.div ((9 - rb) * j) 6) (List.range 0 6).
gear = List.concat (List.map (fn i -> t = Num.div (i * N.pi) 8;
    List.concat (List.map (fn sd -> (side, rs) = sd;
      List.map (fn r -> a = t + side * halfangle r; (r * N.cos a, r * N.sin a)) rs)
    [(m1, radii), (1, List.rev radii)]))
  (List.range 0 15)).

# A D-shaped drive aligns a flat-printed pinion on the spring hub.
drive radius flat height = "(" + circle radius height 0 0 0 + " ∩ " + box 12 12 height (flat - 6) 0 0 + ")".
hub = circle 3.5 2 0 0 0.
key = "(" + drive 3.3 2.45 2 + " |> move 0 0 2)".
rotor = "(" + spring + " ⊕ " + eye + " ⊕ " + hub + " ⊕ " + key + ") ⊖ " + circle 1.85 6 0 0 m1 + " ⊖ " + circle 1.35 3 (0 - 11) 0 m1.
pinion = "(" + profile gear 2 + " ⊖ " + drive 3.48 2.63 2 + ") ⊕ (" + circle 2.95 1.8 0 0 2 + " ⊖ " + circle 1.85 3 0 0 1.9 + ")".

# Carriage print frame is also its operating orientation. Two guide posts
# in closed slots limit the usable stroke to approximately 6 mm.
pitch = N.pi.
teeth = List.concat (List.map (fn i -> x = (i + 0.5) * pitch;
    List.map (fn yd -> (y, dx) = yd; half = Num.div pitch 4 - 0.10 + (y - 8) * N.tan alpha; (x + dx * half, y))
      [(9.25, m1), (7.05, m1), (7.05, 1), (9.25, 1)])
  (List.range (0 - 4) 4)).
rack = [(0 - 15, 16), (0 - 15, 9.25)] + teeth + [(16, 9.25), (16, 16)].
slot x = "(" + circle 1.55 4 (x - 3) 13 m1 + " ⇓ " + circle 1.55 4 (x + 3) 13 m1 + ")".
carriage = profile rack 2 + " ⊖ " + slot (0 - 8) + " ⊖ " + slot 8.
posts = List.fold (fn acc x -> acc + " ⊕ " + circle 1.25 5.6 x 13 0) "" [0 - 8, 8].
base = "(" + box 38 33 2 0 2 (0 - 2) + " ⊕ " + box 38 7 2 0 14 0 + " ⊕ " + circle 1.55 4.4 0 0 0 + " ⊕ " + circle 1.05 1.4 (0 - 11) 0 0 + posts + " ⊕ " + box 6 5 4.35 0 (0 - 12) 0 + " ⊕ " + circle 1.65 1.55 0 (0 - 12) 4.35 + ") |> move 0 0 2".
# Removable friction-fit keeper spans both guide posts, above carriage.
keeper = List.fold (fn acc x -> acc + " ⊖ " + circle 1.41 3 x 13 m1)
  ("(" + box 21 4.5 1.4 0 13 0 + " ⊕ " + box 5 25 1.4 0 0.5 0 + " ⊕ " + circle 4.4 1.4 0 0 0 + " ⊕ " + box 6 5 1.4 0 (0 - 12) 0 + ") ⊖ " + circle 3.10 3 0 0 m1 + " ⊖ " + circle 1.65 3 0 (0 - 12) m1)
  [0 - 8, 8].
# All five pieces have flat build faces.
plate = Str.join " ⊕ " (List.map (fn p -> (shape, x, y) = p; "((" + scaled shape + ") |> move " + N.repr (1.5 * x) + " " + N.repr (1.5 * y) + " 0)")
  [(base, 0, 0), (rotor, 36, 0), (carriage, 36, 8), (keeper, 0, 38), (pinion, 64, 0)]).

# Every part is written at 1.5x; the plate repeats the scaled shapes.
scaled expr = "⬈ 1.5 1.5 1 (" + expr + ")".
save root name expr = writePath "{root}/{name}.coscad" ("main = " + scaled expr + "\n").
> root = R.modelDir "models/captive-spring-demo/larger" "build.sol";
  _ = save root "rotor" rotor;
  _ = save root "pinion" pinion;
  _ = save root "carriage" carriage;
  _ = save root "base" base;
  _ = save root "keeper" keeper;
  writePath "{root}/plate.coscad" ("main = " + plate + "\n").
