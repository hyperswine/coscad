# Three-plate captive display mechanism. Dimensions in mm.
# Run from the CoScad root or this directory: sol build.sol
N = use "../../lib/num".
R = use "../../lib/root".

# Python str() of a number: integers print bare, other floats as repr.
ps x = if x == Num.floor x then str x else N.repr x.
disk r h x y z = "(zcyl " + ps r + " " + ps h + " |> move " + ps x + " " + ps y + " " + N.repr (z + Num.div h 2) + ")".
box w d h x y z = "(box " + ps w + " " + ps d + " " + ps h + " |> move " + ps x + " " + ps y + " " + N.repr (z + Num.div h 2) + ")".
poly pts h = "(⮕ " + ps h + " (poly " + Str.join " " (List.map (fn p -> (x, y) = p; N.fixed 4 x + " " + N.fixed 4 y) pts) + "))".
union parts = "(" + Str.join " ⊕ " parts + ")".
hull a b = "(" + a + " ⇓ " + b + ")".
move s x y z = "(" + s + " |> move " + ps x + " " + ps y + " " + ps z + ")".
m1 = 0 - 1.

# Thin, long spiral for low resistance; motion limited by closed carriage slots.
pts = List.concat (List.map (fn sd -> (side, indices) = sd;
    List.map (fn i -> a = Num.div (3 * N.pi * i) 500; r = 8 + Num.div (48 * i) 500 + side * 0.45; (r * N.cos a, r * N.sin a)) indices)
  [(1, List.range 0 500), (m1, List.rev (List.range 0 500))]).
drive r flat h = "(" + disk r h 0 0 0 + " ∩ " + box 24 24 h (flat - 12) 0 0 + ")".
rotor = union [poly pts 1.2, disk 8.6 2 0 0 0, move (drive 7 5.7 4) 0 0 2, disk 4 1.2 (0 - 57) 0 0] + " ⊖ " + disk 3.8 8 0 0 m1 + " ⊖ " + disk 2.15 4 (0 - 57) 0 m1.

bigR = 36.
gmod = 3.
n = 24.
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
pinion = union [poly gear 4 + " ⊖ " + drive 7.2 5.9 4, disk 5.8 3 0 0 4 + " ⊖ " + disk 3.8 5 0 0 3].
teeth = List.concat (List.map (fn i ->
    List.map (fn ys -> (y, side) = ys; (i * N.pi * gmod + side * (Num.div (N.pi * gmod) 4 - 0.15 + (y - bigR) * N.tan alpha), y))
      [(39.75, m1), (33.3, m1), (33.3, 1), (39.75, 1)])
  (List.range (0 - 7) 7)).
rack = [(0 - 70, 54), (0 - 70, 39.75)] + teeth + [(70, 39.75), (70, 54)].
car = List.fold (fn acc x -> acc + " ⊖ " + hull (disk 3.3 6 (x - 4.7) 47 m1) (disk 3.3 6 (x + 4.7) 47 m1)) (poly rack 4) [0 - 45, 45].
# Platform supports lie outside spring footprint; track sits above spring.
posts = [(0 - 45, 47), (45, 47), (0, 0 - 60)].
postParts xy =
  (x, y) = xy;
  pad = if y < 0 then " ⊕ " + box 14 8 6.5 x y 3 else "";
  # Small crush ribs enter progressively and engage only the stationary cover.
  ribs = Str.join "" (List.map (fn dx -> " ⊕ " + hull (disk 0.17 0.2 (x + dx) y 10.3) (disk 0.06 0.2 (x + dx) y 14.6)) [0 - 2.95, 2.95]);
  pad + " ⊕ " + disk 3 12 x y 3 + ribs.
base0 = union ([box 152 128 3 0 0 0, disk 3.45 8 0 0 3, disk 1.85 1.5 (0 - 57) 0 3] + List.map (fn x -> box 30 11 2 x 49 3) [0 - 54, 54]).
# Stop feet on stationary outboard pads establish axial clearance.
base = base0 + Str.join "" (List.map postParts posts) + Str.join "" (List.map (fn x -> " ⊕ " + box 5 5 6.5 x 58 3) [0 - 69, 69]).
cover0 = union [box 144 9 5 0 57 0, box 100 10 5 0 47 0, box 12 126 5 0 (0 - 3) 0, disk 11 5 0 0 0, box 10 12 5 (0 - 45) 52 0, box 10 12 5 45 52 0] + " ⊖ " + disk 6.08 7 0 0 m1.
# Close sliding fit over most of the hole, with entry chamfer.
cover = cover0 + Str.join "" (List.map (fn xy -> (x, y) = xy; " ⊖ " + disk 3.05 7 x y m1 + " ⊖ " + disk 3.40 0.8 x y (0 - 0.1)) posts).

save root name s = writePath "{root}/{name}.coscad" ("main = " + s + "\n").
> root = R.modelDir "models/captive-spring-demo/three-bed" "build.sol";
  _ = save root "bed-1-base" base;
  _ = save root "bed-2-drive" (union [rotor, move pinion 101 0 0]);
  save root "bed-3-cover-rack" (union [cover, move car 0 43 0]).
