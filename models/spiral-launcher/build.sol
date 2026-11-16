# Spiral (clock) spring launcher for the 10 g glider. Dimensions in mm.
#
# World frame: x launch direction, y across, z up; the arbor (M5 axle) is the
# z axis and the base plate top is z = 0. Pulling the rack carriage back (-x)
# turns the pinion counter-clockwise, which tightens the counter-clockwise
# spiral (study.sol sizes it: 2.0 x 28 mm strip, 7 coils r 8..34, 1.04 turns
# over the 180 mm stroke, 24.8 MPa). The rack's last tooth leaves the pinion at
# the end of the stroke, so the spring free-spins instead of stopping the
# carriage through the teeth; the carriage coasts into the front end wall and
# the glider slides off the pin. A stopper plate dropped through the guide on
# the rear rail into the carriage notch holds full draw; lift it to launch.
#
#   "$SOL" models/spiral-launcher/build.sol
N = use "../lib/num".
R = use "../lib/root".

f v = N.trim v.
half a b = Num.div (a + b) 2.
neg x = 0 - x.
box x0 x1 y0 y1 z0 z1 =
  "(box {f (x1 - x0)} {f (y1 - y0)} {f (z1 - z0)} |> move {f (half x0 x1)} {f (half y0 y1)} {f (half z0 z1)})".
zc r z0 z1 x y = "(zcyl {f r} {f (z1 - z0)} |> move {f x} {f y} {f (half z0 z1)})".
xc r x0 x1 y z = "(xcyl {f r} {f (x1 - x0)} |> move {f (half x0 x1)} {f y} {f z})".
un parts = "(" + Str.join " ⊕ " parts + ")".
dif a parts = "(" + a + Str.join "" (List.map (fn q -> " ⊖ " + q) parts) + ")".
inter a b = "(" + a + " ∩ " + b + ")".
polyAt pts z0 z1 = "((⮕ {f (z1 - z0)} (poly " + Str.join " " (List.map (fn p -> (a, b) = p; "{N.fixed 4 a} {N.fixed 4 b}") pts) + ")) |> move 0 0 {f z0})".
rotZ deg s = "(ω {f deg} {s})".

# ---- spring (study.sol) ---------------------------------------------------------
tS = 2.0. bS = 28.0. rIn = 8.0. rOut = 34.0. coils = 7.
zS0 = 1.0.                         # strip bottom: 1 mm over the plate, on the hub washer
zS1 = zS0 + bS.                    # 29
pitchS = Num.div (rOut - rIn) coils.
hubR = rIn + 1.0.
axleR = 2.7.                       # M5 axle clearance

# ---- gearing ----------------------------------------------------------------------
gmod = 1.25. teethN = 44.
bigR = Num.div (gmod * teethN) 2.  # 27.5 pitch radius
alpha = N.radians 20.
zG0 = 33.0. zG1 = 37.0.            # gear face and rack teeth (4 mm)
stroke = 180.0.

# ---- rail and carriage --------------------------------------------------------------
rootY = bigR + 1.25 * gmod.        # 29.0625 rack root line
barY1 = 40.5.                      # carriage outer edge
floorY0 = 32.0. floorY1 = 46.0. floorZ0 = 30.5. floorZ1 = 32.7.   # wall 40.8..46 leaves 0.9 mm beside the M3 holes
wallY0 = barY1 + 0.3.
lipY0 = 37.5. lipZ0 = zG1 + 0.6. lipZ1 = 39.3.   # 0.6 mm over the carriage: the lip overhang droops a little in print
railX0 = 0.0 - 215.0. railJoint = 0.0. railX1 = 200.0.
rearStop = 0.0 - 211.0.            # carriage rear end at full draw is -210
frontStop = 193.0.                 # carriage front end at the end of the stroke is 188
legs = [(0.0 - 45.0, 41.0, 10.0), (0.0, 41.0, 12.0), (45.0, 41.0, 10.0)].   # (x, y, length in x)
screwY = half wallY0 floorY1.
feetX = [0.0 - 200.0, 190.0].

# carriage local frame: x = 0 is the pinion contact at the end of the stroke,
# so at full draw the carriage sits at world x = -stroke
carX0 = 0.0 - 30.0. carX1 = 190.0.
teethLo = 0 - 1. teethHi = 47.    # rack teeth i * pi * m, local -3.9 .. 184.6
pinZ = 45.0.                        # glider pin axis (clears pinion, nut, rail)
mastX0 = 0.0 - 22.0. mastX1 = 0.0 - 14.0. mastY = 34.0.
capTop = pinZ + 3.0 + 1.2.
grooveHalf = 0.75.
notchX0 = 0.0 - 9.7. notchX1 = 0.0 - 6.3. notchY0 = 36.0.
stopperX = notchX0 - stroke + 1.7.  # world x of the stopper plate centre at full draw (-188)

# ---- spring part --------------------------------------------------------------------
spiralCentre phi = r = rIn + Num.div (pitchS * phi) (2 * N.pi); (r * N.cos phi, r * N.sin phi, r).
spiralNormal phi =
  (cx, cy, r) = spiralCentre phi;
  dr = Num.div pitchS (2 * N.pi);
  tx = dr * N.cos phi - r * N.sin phi; ty = dr * N.sin phi + r * N.cos phi;
  len = N.hypot tx ty;
  (Num.div ty len, Num.div (0 - tx) len).      # outward normal
spiralPts =
  steps = 1400;
  phis = List.map (fn i -> Num.div (2 * N.pi * coils * i) steps) (List.range 0 steps);
  side = fn s -> List.map (fn p -> (cx, cy, r) = spiralCentre p; (nx, ny) = spiralNormal p;
      (cx + s * nx * Num.div tS 2, cy + s * ny * Num.div tS 2)) phis;
  side 1 + List.rev (side (0 - 1)).
# outer end at (rOut, 0) heading +y: a clamp block behind it, bolted to two posts
clampHoles = [(38.0, 0.0 - 2.5), (43.0, 0.0 - 2.5)].
spring = dif (un [polyAt spiralPts zS0 zS1, zc hubR zS0 zS1 0 0,
                  box (rOut - 1.0) 47.0 (0.0 - 7.0) 0.6 zS0 zS1,
                  # D boss that keys the pinion
                  inter (zc 6.0 zS1 zG1 0 0) (box (0.0 - 7.0) 4.6 (0.0 - 7.0) 7.0 zS1 zG1)])
  ([zc axleR (zS0 - 1) (zG1 + 1) 0 0] + List.map (fn h -> (x, y) = h; zc 2.2 (zS0 - 1) (zS1 + 1) x y) clampHoles).

# ---- pinion: spoked ring, module 1.25, 44 teeth ---------------------------------------
rb = bigR * N.cos alpha.
radii = [bigR - 1.25 * gmod] + List.map (fn j -> rb + Num.div ((bigR + gmod - rb) * j) 6) (List.range 0 6).
gearPts = List.concat (List.map (fn i ->
    List.concat (List.map (fn sd -> (side, rs) = sd;
      List.map (fn r ->
        a = N.acos (Numeric.min 1 (Num.div rb r));
        ang = Num.div ((i + 0.5) * 2 * N.pi) teethN + side * (Num.div N.pi (2 * teethN) + N.tan alpha - alpha - N.tan a + a);
        (r * N.cos ang, r * N.sin ang)) rs)
    [(0 - 1, radii), (1, List.rev radii)]))
  (List.range 0 (teethN - 1))).
rimR = bigR - 1.25 * gmod - 2.5.
spokes = List.map (fn k -> rotZ (45 + 90 * k) (box 5.0 (rimR + 0.5) (0.0 - 1.5) 1.5 zG0 zG1)) (List.range 0 3).
pinion = dif (un ([dif (polyAt gearPts zG0 zG1) [zc rimR (zG0 - 1) (zG1 + 1) 0 0], zc 8.5 zG0 zG1 0 0] + spokes))
  [inter (zc 6.25 (zG0 - 1) (zG1 + 1) 0 0) (box (0.0 - 7.5) 4.85 (0.0 - 7.5) 7.5 (zG0 - 1) (zG1 + 1)), zc axleR (zG0 - 1) (zG1 + 1) 0 0].

# ---- carriage (rack bar), local frame ----------------------------------------------------
rackPts =
  p = N.pi * gmod;
  teeth = List.concat (List.map (fn i ->
      List.map (fn ys -> (y, side) = ys; (i * p + side * (Num.div p 4 - 0.15 + (y - bigR) * N.tan alpha), y))
        [(rootY, 0 - 1), (bigR - gmod, 0 - 1), (bigR - gmod, 1), (rootY, 1)])
    (List.range teethLo teethHi));
  [(carX0, barY1), (carX0, rootY)] + teeth + [(carX1, rootY), (carX1, barY1)].
windows = List.map (fn k -> x0 = 4.0 + 13.0 * k; box x0 (x0 + 10.5) 32.3 37.7 (zG0 - 1) (zG1 + 1)) (List.range 0 13).
carriage = dif (un [polyAt rackPts zG0 zG1,
    box mastX0 mastX1 (mastY - 2.5) (mastY + 2.5) zG1 (pinZ + 3.0),                         # mast
    box mastX0 mastX1 (mastY - 3.0) (mastY + 3.0) (pinZ + 2.99) capTop])                    # cap
  (windows
   + [box notchX0 notchX1 notchY0 (barY1 + 1) (zG0 - 1) (zG1 + 1),                           # stopper notch
      box (mastX0 - 1) (mastX1 + 1) (mastY - grooveHalf) (mastY + grooveHalf) (pinZ + 3.0) (capTop + 1),  # boom groove
      # glider pin: an M3x16 screw threaded in, 8 mm protruding forward
      xc 1.25 (mastX0 - 1) (mastX1 + 0.1) mastY pinZ]).

# ---- rail (two pieces), world frame -------------------------------------------------------
railSection x0 x1 = [box x0 x1 floorY0 floorY1 floorZ0 floorZ1,          # floor
                     box x0 x1 wallY0 floorY1 floorZ0 (lipZ0 + 0.1),     # outer wall (overlaps the lip: no near-coincident faces)
                     box x0 x1 lipY0 floorY1 lipZ0 lipZ1].               # hold-down lip
screwHoles xs = List.map (fn x -> zc 1.7 (floorZ0 - 1) (lipZ1 + 1) x screwY) xs.
guideX0 = stopperX - 6.0. guideX1 = stopperX + 6.0.
rearRail = dif (un (railSection railX0 railJoint
    + [box railX0 rearStop floorY0 (wallY0 + 0.1) (floorZ1 - 0.1) (lipZ0 + 0.1),                # rear stop wall
       box guideX0 guideX1 notchY0 floorY1 (lipZ0 + 0.1) 50.0]))                        # stopper guide
  (screwHoles [0.0 - 45.0, 0.0 - 3.0, 0.0 - 200.0]
   + [box (stopperX - 1.7) (stopperX + 1.7) (notchY0 + 0.3) (barY1 - 0.2) (floorZ0 + 0.5) 51.0]).  # stopper slot + floor pocket
frontRail = dif (un (railSection railJoint railX1 + [box frontStop railX1 floorY0 (wallY0 + 0.1) (floorZ1 - 0.1) (lipZ0 + 0.1)]))
  (screwHoles [3.0, 45.0, 190.0]).

# ---- stopper plate (world frame, engaged position) ---------------------------------------
stopper = un [box (stopperX - 1.5) (stopperX + 1.5) (notchY0 + 0.5) (barY1 - 0.4) (floorZ0 + 0.6) 54.0,
              box (stopperX - 1.5) (stopperX + 1.5) (notchY0 - 0.5) (floorY1 + 4.0) 54.0 60.0].    # finger tab

# ---- base plate with legs, axle and clamp bolts ------------------------------------------
legParts = List.map (fn l -> (x, y, lx) = l; box (x - Num.div lx 2) (x + Num.div lx 2) (y - 4) (y + 4) 0 floorZ0) legs.
legHoles = List.concat (List.map (fn l -> (x, y, lx) = l;
    if lx > 10 then [zc 1.3 (floorZ0 - 10) (floorZ0 + 1) (x - 3) screwY, zc 1.3 (floorZ0 - 10) (floorZ0 + 1) (x + 3) screwY]
    else [zc 1.3 (floorZ0 - 10) (floorZ0 + 1) x screwY]) legs).
base = dif (un ([box (0.0 - 55.0) 55.0 (0.0 - 42.0) 46.0 (0.0 - 4.0) 0,
                 dif (zc 7.5 0 zS0 0 0) [zc 3.2 (0.0 - 1) (zS0 + 1) 0 0]]                   # hub washer
                + legParts))
  ([zc axleR (0.0 - 5) (zS0 + 1) 0 0,
    "(⎏ 6 4.7 3.5 |> move 0 0 -4.1)"]                                                     # M5 head pocket (hex)
   + List.concat (List.map (fn h -> (x, y) = h; [zc 2.2 (0.0 - 5) 1 x y, "(⎏ 6 3.9 3 |> move {f x} {f y} -4.1)"]) clampHoles)
   + legHoles).
foot = dif (box (0.0 - 5) 5 (screwY - 4) (screwY + 4) (0.0 - 4.0) floorZ0) [zc 1.3 (floorZ0 - 10) (floorZ0 + 1) 0 screwY].

# ---- assemblies ----------------------------------------------------------------------------
partsHeader = "plate 256 256 6\n"
  + "base ← base.coscad ×1 ▽bot material=pla\n"
  + "spring ← spring.coscad ×1 ▽bot material=pla\n"
  + "pinion ← pinion.coscad ×1 ▽bot material=pla\n"
  + "carriage ← carriage.coscad ×1 ▽bot material=pla\n"
  + "railRear ← rail-rear.coscad ×1 ▽bot material=pla\n"
  + "railFront ← rail-front.coscad ×1 ▽bot material=pla\n"
  + "footRear ← foot.coscad ×1 ▽bot material=pla\n"
  + "footFront ← foot.coscad ×1 ▽bot material=pla\n".
gliderParts = "fuselage ← ../pla-bow-launcher/glider-fuselage.coscad ×1 ▽fwd material=pla\n"
  + "wing ← ../pla-bow-launcher/glider-wing.coscad ×1 ▽bot material=pla\n"
  + "stab ← ../pla-bow-launcher/glider-stab.coscad ×1 ▽bot material=pla\n"
  + "saddle ← ../pla-bow-launcher/glider-saddle.coscad ×1 ▽bot material=pla\n".
# carriage at world offset cx; the pinion turns -cx/R (rad) with it
fixedAt cx glide wingLe withStopper =
  pinDeg = 0 - Num.div (cx * 180) (N.pi * bigR);
  gx = cx + mastX1 + 8.0 + 0.3 + glide;   # glider slot end 0.3 mm ahead of the screw tip, plus any flight
  xw = wingLe - 18.0;
  "fixed base 0 0 0 0 0 0\nfixed spring 0 0 0 0 0 0\nfixed railRear 0 0 0 0 0 0\nfixed railFront 0 0 0 0 0 0\n"
  + "fixed footRear {f (feetX ! 0)} 0 0 0 0 0\nfixed footFront {f (feetX ! 1)} 0 0 0 0 0\n"
  + "fixed pinion 0 0 0 0 0 {f pinDeg}\nfixed carriage {f cx} 0 0 0 0 0\n"
  + (if withStopper then "fixed stopper 0 0 0 0 0 0\n" else "")
  + "fixed fuselage {f gx} {f mastY} {f pinZ} 0 0 0\nfixed saddle {f gx} {f mastY} {f pinZ} 0 0 0\n"
  + "fixed wing {f (gx + xw)} {f mastY} {f (pinZ + 12.3)} 0 -2 0\n"
  + "fixed stab {f (gx - 105)} {f mastY} {f (pinZ + 7.1)} 0 0 0\n".

writePart dir name body note = writePath "{dir}/{name}.coscad" (note + "main = {body}\n").

build dir =
  g = readPath "{dir}/../pla-bow-launcher/glider.json";
  wingLe = case List.filter (fn l -> Str.contains "\"wing_le_x\"" l) (Str.lines g) of
      l :: rest -> unwrap (Try.parseNum (Str.trim (Str.replace "," "" (Str.slice l (Str.indexOf ":" l + 1) (Str.len l)))))
    | [] -> error "glider.json has no wing_le_x";
  _ = writePart dir "spring" spring "// PLA spiral: print hub down, 100% infill; the strip is a 2.0 mm wall of perimeters.\n";
  _ = writePart dir "pinion" pinion "// Spoked pinion m1.25 z44, keyed to the spring hub's D boss.\n";
  _ = writePart dir "carriage" carriage "// Rack carriage, local frame (x = 0: pinion contact at the end of the stroke).\n";
  _ = writePart dir "rail-rear" rearRail "// Rear rail with the stopper guide and the overdraw stop. M3 screws into the legs and the rear foot.\n";
  _ = writePart dir "rail-front" frontRail "// Front rail with the end wall (put foam tape on it). M3 screws into the legs and the front foot.\n";
  _ = writePart dir "stopper" stopper "// Stopper plate, shown dropped in (full draw held). Print on its side.\n";
  _ = writePart dir "base" base "// Base plate: M5 axle bolt and two M4 clamp bolts from below, legs for the rail.\n";
  _ = writePart dir "foot" foot "// Rail end foot (two): stands on the table, M3 screw from the rail.\n";
  _ = writePath "{dir}/launcher.assemble"
        ("// Spiral launcher at full draw (carriage at x = -{f stroke}), stopper in, glider loaded.\n" + partsHeader
         + "stopper ← stopper.coscad ×1 ▽fwd material=pla\n" + gliderParts + "\n" + fixedAt (neg stroke) 0 wingLe True);
  _ = writePath "{dir}/launcher-end.assemble"
        ("// End of the stroke: last rack tooth at the pinion, stopper out, glider still on the pin.\n" + partsHeader
         + gliderParts + "\n" + fixedAt 0.0 0 wingLe False);
  _ = writePath "{dir}/launcher-coast.assemble"
        ("// Carriage at the front end wall (rack clear of the pinion), glider 100 mm on.\n" + partsHeader
         + gliderParts + "\n" + fixedAt (frontStop - carX1) 100.0 wingLe False);
  print "stopper at x {N.fixed 1 stopperX}; rack teeth local {N.fixed 1 (teethLo * N.pi * gmod)} .. {N.fixed 1 (teethHi * N.pi * gmod)}; pinion R {N.fixed 2 bigR}".

> build (R.modelDir "models/spiral-launcher" "build.sol").
