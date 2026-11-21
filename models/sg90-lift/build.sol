# Two-start trapezoidal screw. Heights and radii are in mm.
N = use "../lib/num".
R = use "../lib/root".
point z clearance i =
  angle = Num.div (2 * N.pi * i) 80;
  axial = z - Num.div (20 * i) 80 + 5;
  phase = Numeric.abs (axial - 10 * Num.floor (Num.div axial 10) - 5);
  radius = 4 + clearance + N.clamp 0 1.5 (3 - phase);
  N.fixed 4 (radius * N.cos angle) + " " + N.fixed 4 (radius * N.sin angle).
section clearance step =
  z = Num.div step 2;
  N.trim z + " (poly " + Str.join " " (List.map (point z clearance) (List.range 0 79)) + ")".
thread height clearance = "loft " + Str.join " " (List.map (section clearance) (List.range 0 (height * 2))).
> dir = R.modelDir "models/sg90-lift" "build.sol";
  source = readPath "{dir}/plate.template" |> Str.replace "MALE_THREAD" (thread 23 0) |> Str.replace "NUT_PROFILE" (thread 10 0.45);
  writePath "{dir}/plate.coscad" source.
