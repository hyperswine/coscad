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

bigR = 15.
gmod = 1.5.
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
# Fixed guide posts bear against the square ends of a closed rack slot.
travel = 200.
postOffset = 10.
postRadius = 2.
slotLength = travel + 2 * (postOffset + postRadius).
base = union ([box 242 23 3 0 21 0, disk 21 3 0 0 0, disk 3 13 0 0 3] + List.map (fn x -> disk postRadius 8.4 x 24 3) [0 - postOffset, postOffset]).
pitch = N.pi * gmod.
tip = bigR + 1.25 * gmod.
root0 = bigR - 0.95 * gmod.
teeth = List.concat (List.map (fn i ->
    List.map (fn ys -> (y, side) = ys; (i * pitch + side * (Num.div pitch 4 - 0.11 + (y - bigR) * N.tan alpha), y))
      [(tip, m1), (root0, m1), (root0, 1), (tip, 1)])
  (List.range (0 - 25) 25)).
rack = [(0 - 120, 30), (0 - 120, tip)] + teeth + [(120, tip), (120, 30)].
car = poly rack 5 + " ⊖ " + box slotLength 4.55 8 0 24 m1.
# Integral scalloped wheel lies fully inside the pinion root diameter.
rim = List.map (fn i -> a = Num.div (i * 2 * N.pi) 256; rr = 12 + 0.65 * N.cos (16 * a); (rr * N.cos a, rr * N.sin a)) (List.range 0 255).
wheel = union [poly gear 5, move (poly rim 5) 0 0 5] + " ⊖ " + disk 3.27 12 0 0 m1.
# Keeper bottoms on post tips in blind sockets, setting running clearance.
# Print both retainers upside down (closed face on bed).
keeper = List.fold (fn acc x -> acc + " ⊖ " + disk 2.01 3.2 x 0 1) (box 27 8 4 0 0 0) [0 - 10, 10].
cap = disk 5.3 4 0 0 0 + " ⊖ " + disk 3.01 2.9 0 0 1.3.

save root name s = writePath "{root}/{name}.coscad" ("main = " + s + "\n").
> root = R.modelDir "models/thumbwheel-200" "build.sol";
  _ = if Num.div slotLength 2 - postRadius - postOffset != 100 then error "slot must give 200 mm travel" else Unit;
  _ = if not (25 * pitch - 100 > 3 * pitch) then error "rack too short for the travel" else Unit;
  _ = save root "base" base;
  _ = save root "rack" car;
  save root "controls" (union [wheel, move keeper 36 0 0, move cap 0 24 0]).
