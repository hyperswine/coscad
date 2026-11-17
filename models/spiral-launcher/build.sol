# Spiral (clock) spring launcher for the 10 g glider, on printed 2020 extrusion. Dimensions in mm.
#
# World frame: x launch direction, y across, z up; the spring arbor is the z axis and the
# base plate top is z = 0 (the bench is at z = -3.8). Pulling the rack carriage back (-x)
# turns the pinion counter-clockwise, which tightens the counter-clockwise spiral
# (study.sol sizes it: 2.0 x 28 mm strip, 7 coils r 8..34, 1.04 turns over the 180 mm
# stroke, 24.8 MPa). The rack's last tooth leaves the pinion at the end of the stroke, so
# the spring free-spins; the carriage coasts into the front stop and the glider slides off
# the pin.
#
# Revision 2 (2026-11-17): the structure is printed 2020 extrusion with the tank model's
# slot-6 profile (../tank-model-2020/rail100.coscad), and the only hardware is M5x10
# screws with M5 drop-in T-nuts, plus one M5 hex nut. The track is two 221 mm beams on two
# of the tank's 100 mm rails as feet. The carriage rides the beam's top slot on two T-key
# shoes. The spring turns on a printed post, held by a cap screwed into a nut captive in
# the post; its outer clamp block drops into a pocket in the base (no screws). A stopper
# plate dropped through a guide on the beam's side, into a notch in the rack, holds full
# draw; lift it to launch.
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
yc r y0 y1 x z = "(ycyl {f r} {f (y1 - y0)} |> move {f x} {f (half y0 y1)} {f z})".
un parts = "(" + Str.join " ⊕ " parts + ")".
dif a parts = "(" + a + Str.join "" (List.map (fn q -> " ⊖ " + q) parts) + ")".
polyAt pts z0 z1 = "((⮕ {f (z1 - z0)} (poly " + Str.join " " (List.map (fn p -> (a, b) = p; "{N.fixed 4 a} {N.fixed 4 b}") pts) + ")) |> move 0 0 {f z0})".
rotZ deg s = "(ω {f deg} {s})".
hex circ z0 z1 = "(⎏ 6 {f circ} {f (z1 - z0)} |> move 0 0 {f z0})".   # the hex prism starts at z = 0
# A hole cylinder carrying a planner frame on the face it opens on (z1 - over), normal +z.
zcF name over r z0 z1 x y = "(zcyl {f r} {f (z1 - z0)} |> move {f x} {f y} {f (half z0 z1)} |> frame {name} top top bak 0 0 {f (0 - over)})".

# ---- hardware ---------------------------------------------------------------------
# Every screw is M5x10: 5 mm of printed part under the head, 5 mm into the nut.
m5 = 2.75.          # clearance hole radius
m5head = 5.2.       # counterbore radius (button head 9.5 mm)

# ---- spring (study.sol) ---------------------------------------------------------
tS = 2.0. bS = 28.0. rIn = 8.0. rOut = 34.0. coils = 7.
zS0 = 1.0.                         # strip bottom: 1 mm over the plate, on the hub washer
zS1 = zS0 + bS.                    # 29
pitchS = Num.div (rOut - rIn) coils.
hubR = rIn + 1.0.

# ---- gearing ----------------------------------------------------------------------
gmod = 1.25. teethN = 44.
bigR = Num.div (gmod * teethN) 2.  # 27.5 pitch radius
alpha = N.radians 20.
zG0 = 33.0. zG1 = 37.0.            # gear face and rack teeth (4 mm)
stroke = 180.0.

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
# Outer end at (rOut, 0) heading +y, ending in a clamp block that drops into a pocket in the base.
# The hub turns on the base's printed post: bore postR + 0.25, a collar up to the pinion (its
# shoulder) and a hex boss (16 mm across flats) that keys the pinion.
postR = 6.0.
hubBore = postR + 0.25.
bossCirc = Num.div 16.0 (Num.sqrt 3).
spring = dif (un [polyAt spiralPts zS0 zS1, zc hubR zS0 zG0 0 0,
                  box (rOut - 1.0) 47.0 (0.0 - 7.0) 0.6 zS0 zS1,
                  hex bossCirc zG0 zG1])
  [zc hubBore (zS0 - 1) (zG1 + 1) 0 0].

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
pinionHubR = 11.0.
spokes = List.map (fn k -> rotZ (45 + 90 * k) (box (pinionHubR - 0.5) (rimR + 0.5) (0.0 - 1.5) 1.5 zG0 zG1)) (List.range 0 3).
pinion = dif (un ([dif (polyAt gearPts zG0 zG1) [zc rimR (zG0 - 1) (zG1 + 1) 0 0], zc pinionHubR zG0 zG1 0 0] + spokes))
  [hex (bossCirc + Num.div 0.3 (Num.sqrt 3)) (zG0 - 1) (zG1 + 1)].

# ---- the 2020 track ---------------------------------------------------------------------
# Beams lie along x with their -y face at y 41, top at z 36.2 (the carriage's key sits in
# the top slot), on two 100 mm cross rails standing on the bench.
beamY0 = 41.0. beamY1 = 61.0. beamYc = 51.0.
beamZ1 = zG0 + 3.2. beamZ0 = beamZ1 - 20.0. beamZc = beamZ0 + 10.0.   # 36.2, 16.2, 26.2
bench = 0.0 - 3.8.
beamLen = 221.0.
beamX0 = 0.0 - 227.0. beamJoint = beamX0 + beamLen. beamX1 = beamJoint + beamLen.    # -227, -6, 215
feetX = [half beamX0 beamJoint, half beamJoint beamX1].   # each beam balances on its foot while it is built
railProfile len = "box 20 20 {f len}
  |> cut (box 6.7 6.2 ({f len} + 2) |> move 7.25 0 0)
  |> cut (box 1.8 11 ({f len} + 2) |> move 7.3 0 0)
  |> cut (box 1.3 8.4 ({f len} + 2) |> move 5.75 0 0)
  |> cut (box 6.7 6.2 ({f len} + 2) |> move -7.25 0 0)
  |> cut (box 1.8 11 ({f len} + 2) |> move -7.3 0 0)
  |> cut (box 1.3 8.4 ({f len} + 2) |> move -5.75 0 0)
  |> cut (box 6.2 6.7 ({f len} + 2) |> move 0 7.25 0)
  |> cut (box 11 1.8 ({f len} + 2) |> move 0 7.3 0)
  |> cut (box 8.4 1.3 ({f len} + 2) |> move 0 5.75 0)
  |> cut (box 6.2 6.7 ({f len} + 2) |> move 0 -7.25 0)
  |> cut (box 11 1.8 ({f len} + 2) |> move 0 -7.3 0)
  |> cut (box 8.4 1.3 ({f len} + 2) |> move 0 -5.75 0)
  |> cut (zcyl 2.1 ({f len} + 2))".

# ---- carriage (rack bar + two T-key shoes), local frame --------------------------------------
# local x = 0 is the pinion contact at the end of the stroke, so at full draw the carriage
# sits at world x = -stroke. Bottom (rack and key heads) at zG0: it prints flat on that face.
rootY = bigR + 1.25 * gmod.        # 29.0625 rack root line
barY1 = beamY0 - 0.3.              # 40.7: rack bar back edge, 0.3 mm off the beam face, which backs it against the mesh force
carX0 = 0.0 - 26.0. carX1 = 189.0.
teethLo = 0 - 1. teethHi = 47.    # rack teeth i * pi * m, local -3.9 .. 184.6
pinZ = 54.0.                       # glider pin axis
mastX0 = 0.0 - 22.0. mastX1 = 0.0 - 14.0. mastY = beamYc.
notchX0 = 0.0 - 9.7. notchX1 = 0.0 - 6.3. notchY0 = 32.0. notchY1 = 38.0.
stopperX0 = notchX0 - stroke + 0.2. stopperX1 = notchX1 - stroke - 0.2.   # plate at full draw, world
rackPts =
  p = N.pi * gmod;
  teeth = List.concat (List.map (fn i ->
      List.map (fn ys -> (y, side) = ys; (i * p + side * (Num.div p 4 - 0.15 + (y - bigR) * N.tan alpha), y))
        [(rootY, 0 - 1), (bigR - gmod, 0 - 1), (bigR - gmod, 1), (rootY, 1)])
    (List.range teethLo teethHi));
  [(carX0, barY1), (carX0, rootY)] + teeth + [(carX1, rootY), (carX1, barY1)].
windows = List.map (fn k -> x0 = 4.0 + 13.0 * k; box x0 (x0 + 10.5) 32.3 37.7 (zG0 - 1) (zG1 + 1)) (List.range 0 13).
rackBar = dif (polyAt rackPts zG0 zG1) (windows + [box notchX0 notchX1 notchY0 notchY1 (zG0 - 1) (zG1 + 1)]).
# A shoe bridges from the bar to a T-key in the beam's top slot (6.2 mm opening behind
# 1.8 mm lips, 11 mm pocket): neck 5.6 wide, head 10.2 wide and 1.2 tall, 0.2 under the lips.
shoeTop = beamZ1 + 2.2.
key x0 x1 = [box x0 x1 (beamYc - 2.8) (beamYc + 2.8) (zG0 + 1.2) (beamZ1 + 0.05),
             box x0 x1 (beamYc - 5.1) (beamYc + 5.1) zG0 (zG0 + 1.2)].
shoe x0 x1 = [box x0 x1 36.0 (beamYc + 3.5) zG1 shoeTop, box x0 x1 (barY1 - 0.4) (beamYc + 3.5) beamZ1 (zG1 + 0.05)] + key x0 x1.
bossTop = shoeTop + 1.8.
socket = box (0.0 - 20.1) (0.0 - 15.9) (mastY - 2.1) (mastY + 2.1) (beamZ1 + 0.4) (bossTop + 1).
carriage = dif (un ([rackBar] + shoe carX0 (0.0 - 11.0) + shoe 174.0 carX1
                    + [box (0.0 - 24.0) (0.0 - 12.0) (mastY - 4.0) (beamYc + 3.5) (shoeTop - 0.05) bossTop])) [socket].
# Mast: printed lying on its -y face, so the glider pin (a 6 x 2.9 mm bar, rounded tip, 8 mm
# forward of the mast) is flat on the bed. Pushed into the shoe's socket (a drop of glue).
mastTop = pinZ + 3.0. capTop = mastTop + 1.2.
mast = dif (un [box mastX0 mastX1 (mastY - 3) (mastY + 3) bossTop mastTop,
                box mastX0 mastX1 (mastY - 3) (mastY + 3) (mastTop - 0.05) capTop,
                box (0.0 - 20.0) (0.0 - 16.0) (mastY - 2) (mastY + 2) (beamZ1 + 0.6) (bossTop + 0.05),
                box (mastX1 - 0.05) (0.0 - 7.45) (mastY - 3) (mastY + 3) (pinZ - 1.45) (pinZ + 1.45),
                yc 1.45 (mastY - 3) (mastY + 3) (0.0 - 7.45) pinZ])
  [box (mastX0 - 1) (mastX1 + 1) (mastY - 0.75) (mastY + 0.75) mastTop (capTop + 1)].    # boom groove

# ---- base: plate, arbor post, clamp pocket, flange to the beam ---------------------------
postTop = zG1 + 0.3.               # 0.3 mm axial play under the cap
capZ1 = postTop + 3.0.
nutZ0 = 31.6. nutZ1 = 35.8.        # M5 nut (4 mm) slides in from -x; the spring hub covers it
flangeY0 = beamY0 - 4.0.
flangeZ1 = zG0 - 0.6.              # under the rack bar
flangeHoles = [0.0 - 50.0, 0.0 - 36.0, 56.0, 70.0].    # clear of the spring for the driver
plateX0 = 0.0 - 56.0. plateX1 = 76.0. plateY0 = 0.0 - 45.0.
pocketWalls = [box 35.5 50.0 (0.0 - 10.0) (0.0 - 7.2) 0 8.0,      # starts clear of the outer coil
               box 36.0 50.0 0.8 3.8 0 8.0,
               box 47.2 50.0 (0.0 - 10.0) 3.8 0 8.0].
base = dif (un ([box plateX0 plateX1 plateY0 beamY0 bench 0.05,
                 box plateX0 plateX1 flangeY0 beamY0 bench flangeZ1,
                 zc postR 0 postTop 0 0,
                 zc 10.0 0 zS0 0 0,                                                # hub washer
                 box (rOut - 1.0) 47.0 (0.0 - 7.0) 0.6 0 zS0]                    # pad under the clamp block
                + pocketWalls
                + List.map (fn x -> "(ycyl 6 1.05 |> move {f x} {f (flangeY0 - 0.5)} {f beamZc})") flangeHoles))
  ([zcF "post" 1 m5 29.0 (postTop + 1) 0 0,
    "(" + box (0.0 - 7.0) 4.8 (0.0 - 4.1) 4.1 nutZ0 nutZ1 + " |> frame post_nut top bot bak 1.1 0 0)"]     # the slot box is off-centre by -1.1 in x
   + List.map (fn x -> yc m5 (flangeY0 - 2) (beamY0 + 1) x beamZc) flangeHoles).
cap = dif (zc 10.5 postTop capZ1 0 0) [zcF "cap" 1 m5 (postTop - 1) (capZ1 + 1) 0 0].

# ---- track fittings -----------------------------------------------------------------------------
# Corner bracket, local: beam face at y = 0 (bracket on +y), foot top at z = 0; 18 wide, 5 thick.
bracket = dif (un [box (0.0 - 9) 9 0 5 0 (flangeZ1 - beamZ0), box (0.0 - 9) 9 0 20 0 5])
  [yc m5 (0.0 - 1) 6 0 10, zc m5 (0.0 - 1) 6 0 13].
# Splice plate on the beams' +y faces across the joint.
spliceHoles = [0.0 - 31.0, 0.0 - 16.0, 4.0, 19.0].
splice = dif (box (0.0 - 37.0) 25.0 beamY1 (beamY1 + 5) (beamZ0 + 1) (beamZ1 - 1))
  (List.map (fn x -> yc m5 (beamY1 - 1) (beamY1 + 6) x beamZc) spliceHoles).
# End stop, local: on the beam top, centred on the slot; one screw (counterbored to 5 mm) and
# key necks at both ends, clear of the T-nut. Prints upside down. Foam tape on the impact face.
stop = dif (un [box (0.0 - 10) 10 (0.0 - 9) 9 0 10,
                box (0.0 - 10) (0.0 - 6) (0.0 - 2.8) 2.8 (0.0 - 1.6) 0.05,
                box 6 10 (0.0 - 2.8) 2.8 (0.0 - 1.6) 0.05])
  [zc m5 (0.0 - 1) 11 0 0, zc m5head 5 11 0 0].
frontStop = 194.0.                 # carriage front end at the end of the stroke is 189
rearStop = carX0 - stroke - 1.0.   # 1 mm behind full draw (-207)
# Stopper guide on the beam's -y face: a C around the rack's path, with the plate slot
# above the rack and a pocket below it, so the plate is held on both sides of the notch.
gX0 = 0.0 - 197.0. gX1 = 0.0 - 162.0.
guideHoles = [0.0 - 180.5, 0.0 - 167.5].   # 13 mm apart for two T-nuts; counterbores clear of the plate pocket
slotX0 = notchX0 - stroke. slotX1 = notchX1 - stroke.
guide = dif (un [box gX0 gX1 24.0 beamY0 (beamZ0 + 1) (zG0 - 0.4),
                 box gX0 (gX0 + 4) 20.0 24.05 (beamZ0 + 1) (shoeTop + 6.4),
                 box gX0 (slotX1 + 3) 20.0 (barY1 - 0.4) (shoeTop + 0.4) (shoeTop + 6.4)])
  ([box slotX0 slotX1 notchY0 notchY1 (zG0 - 3.8) (shoeTop + 7)]
   + List.concat (List.map (fn x -> [yc m5 23 (beamY0 + 1) x beamZc, yc m5head 23 (beamY0 - 5) x beamZc]) guideHoles)).
stopper = un [box stopperX0 stopperX1 (notchY0 + 0.2) (notchY1 - 0.2) (zG0 - 3.6) (shoeTop + 9),
              box stopperX0 stopperX1 (notchY0 - 4) (notchY1 + 2) (shoeTop + 8.95) (shoeTop + 13)].   # finger tab

# ---- assemblies ----------------------------------------------------------------------------
partsHeader = "plate 256 256 6\n"
  + "base ← base.coscad ×1 ▽bot material=pla\n"
  + "spring ← spring.coscad ×1 ▽bot material=pla\n"
  + "pinion ← pinion.coscad ×1 ▽bot material=pla\n"
  + "cap ← cap.coscad ×1 ▽bot material=pla\n"
  + "carriage ← carriage.coscad ×1 ▽bot material=pla\n"
  + "mast ← mast.coscad ×1 ▽fwd material=pla\n"
  + "beamRear ← beam221.coscad ×1 ▽bot profile=2020 material=pla\n"
  + "beamFront ← beam221.coscad ×1 ▽bot profile=2020 material=pla\n"
  + "footRear ← ../tank-model-2020/rail100.coscad ×1 ▽bot profile=2020 material=pla\n"
  + "footFront ← ../tank-model-2020/rail100.coscad ×1 ▽bot profile=2020 material=pla\n"
  + "brRearL ← corner-bracket.coscad ×1 ▽lft material=pla\n"
  + "brRearR ← corner-bracket.coscad ×1 ▽lft material=pla\n"
  + "brFrontL ← corner-bracket.coscad ×1 ▽lft material=pla\n"
  + "brFrontR ← corner-bracket.coscad ×1 ▽lft material=pla\n"
  + "splice ← splice.coscad ×1 ▽bak material=pla\n"
  + "stopRear ← stop.coscad ×1 ▽top material=pla\n"
  + "stopFront ← stop.coscad ×1 ▽top material=pla\n"
  + "guide ← stopper-guide.coscad ×1 ▽lft material=pla\n".
gliderParts = "fuselage ← ../pla-bow-launcher/glider-fuselage.coscad ×1 ▽fwd material=pla\n"
  + "wing ← ../pla-bow-launcher/glider-wing.coscad ×1 ▽bot material=pla\n"
  + "stab ← ../pla-bow-launcher/glider-stab.coscad ×1 ▽bot material=pla\n"
  + "saddle ← ../pla-bow-launcher/glider-saddle.coscad ×1 ▽bot material=pla\n".
# carriage at world offset cx; the pinion turns -cx/R (rad) with it
launcherAt cx withStopper =
  pinDeg = 0 - Num.div (cx * 180) (N.pi * bigR);
  fx = feetX ! 0; gxF = feetX ! 1;
  "fixed base 0 0 0 0 0 0\nfixed spring 0 0 0 0 0 0\nfixed cap 0 0 0 0 0 0\n"
  + "fixed pinion 0 0 0 0 0 {f pinDeg}\nfixed carriage {f cx} 0 0 0 0 0\nfixed mast {f cx} 0 0 0 0 0\n"
  + "fixed beamRear {f (half beamX0 beamJoint)} {f beamYc} {f beamZc} 0 90 0\n"
  + "fixed beamFront {f (half beamJoint beamX1)} {f beamYc} {f beamZc} 0 90 0\n"
  + "fixed footRear {f fx} {f beamYc} {f (bench + 10)} 90 0 0\n"
  + "fixed footFront {f gxF} {f beamYc} {f (bench + 10)} 90 0 0\n"
  + "fixed brRearR {f fx} {f beamY1} {f beamZ0} 0 0 0\nfixed brRearL {f fx} {f beamY0} {f beamZ0} 0 0 180\n"
  + "fixed brFrontR {f gxF} {f beamY1} {f beamZ0} 0 0 0\nfixed brFrontL {f gxF} {f beamY0} {f beamZ0} 0 0 180\n"
  + "fixed splice 0 0 0 0 0 0\nfixed guide 0 0 0 0 0 0\n"
  + "fixed stopRear {f (rearStop - 10)} {f beamYc} {f beamZ1} 0 0 0\n"
  + "fixed stopFront {f (frontStop + 10)} {f beamYc} {f beamZ1} 0 0 0\n"
  + (if withStopper then "fixed stopper 0 0 0 0 0 0\n" else "").
fixedAt cx glide wingLe withStopper =
  gx = cx + mastX1 + 8.0 + 0.3 + glide;   # glider slot end 0.3 mm ahead of the pin tip, plus any flight
  xw = wingLe - 18.0;
  launcherAt cx withStopper
  + "fixed fuselage {f gx} {f mastY} {f pinZ} 0 0 0\nfixed saddle {f gx} {f mastY} {f pinZ} 0 0 0\n"
  + "fixed wing {f (gx + xw)} {f mastY} {f (pinZ + 12.3)} 0 -2 0\n"
  + "fixed stab {f (gx - 105)} {f mastY} {f (pinZ + 7.1)} 0 0 0\n".

# Build plan (coscad plan): the launcher at the end of the stroke, where the carriage goes in.
# through=5: every clamped part has 5 mm under the head (counterbored where the part is thicker).
# at= positions are mm from each rail's marked end (-x for the beams, -y for the feet).
tnut = fn part host face ats -> "fastener M5x10 {part} {host} {face}" + (if List.len ats > 1 then " ×{List.len ats}" else "")
  + " at=" + Str.join "," (List.map f ats) + " through=5 nut=dropin\n".
rearAt xs = List.map (fn x -> x - beamX0) xs.
frontAt xs = List.map (fn x -> x - beamJoint) xs.
footY0 = beamYc - 50.0.
buildPlan =
  "// Build plan for the spiral launcher (coscad plan): end-of-stroke pose, no glider. Written by build.sol.\n"
  # preload=hand: the post's nut slides in from the side before the base goes down.
  + Str.replace "base.coscad ×1 ▽bot material=pla" "base.coscad ×1 ▽bot material=pla preload=hand" partsHeader + "\n" + launcherAt 0.0 False + "\n"
  + tnut "brRearR" "beamRear" "bak" (rearAt [feetX ! 0]) + tnut "brRearR" "footRear" "top" [beamY1 + 13 - footY0]
  + tnut "brRearL" "beamRear" "fwd" (rearAt [feetX ! 0]) + tnut "brRearL" "footRear" "top" [beamY0 - 13 - footY0]
  + tnut "brFrontR" "beamFront" "bak" (frontAt [feetX ! 1]) + tnut "brFrontR" "footFront" "top" [beamY1 + 13 - footY0]
  + tnut "brFrontL" "beamFront" "fwd" (frontAt [feetX ! 1]) + tnut "brFrontL" "footFront" "top" [beamY0 - 13 - footY0]
  + tnut "splice" "beamRear" "bak" (rearAt (List.take 2 spliceHoles)) + tnut "splice" "beamFront" "bak" (frontAt (List.drop 2 spliceHoles))
  + tnut "base" "beamRear" "fwd" (rearAt (List.take 2 flangeHoles)) + tnut "base" "beamFront" "fwd" (frontAt (List.drop 2 flangeHoles))
  + tnut "guide" "beamRear" "fwd" (rearAt guideHoles)
  + tnut "stopRear" "beamRear" "top" (rearAt [rearStop - 10]) + tnut "stopFront" "beamFront" "top" (frontAt [frontStop + 10])
  + "joint drop spring base axis=top\n"
  + "joint drop pinion spring axis=top\n"
  + "fastener M5x10 cap base @post seat=@cap nut=hex pocket=@post_nut\n"
  + "joint drop carriage beamFront axis=rt\n"
  + "joint push mast carriage axis=top\n"
  + "plan rest=bot\n".

writePart dir name body note = writePath "{dir}/{name}.coscad" (note + "main = {body}\n").

build dir =
  g = readPath "{dir}/../pla-bow-launcher/glider.json";
  wingLe = case List.filter (fn l -> Str.contains "\"wing_le_x\"" l) (Str.lines g) of
      l :: rest -> unwrap (Try.parseNum (Str.trim (Str.replace "," "" (Str.slice l (Str.indexOf ":" l + 1) (Str.len l)))))
    | [] -> error "glider.json has no wing_le_x";
  _ = writePart dir "spring" spring "// PLA spiral: print hub down, 100% infill; the strip is a 2.0 mm wall of perimeters. Turns on the base's post.\n";
  _ = writePart dir "pinion" pinion "// Spoked pinion m1.25 z44, keyed to the spring hub's hex boss, resting on the hub collar.\n";
  _ = writePart dir "carriage" carriage "// Rack carriage, local frame (x = 0: pinion contact at the end of the stroke). Prints flat on its bottom.\n";
  _ = writePart dir "mast" mast "// Glider mast and pin, carriage frame. Print lying on its -y face; push into the rear shoe's socket.\n";
  _ = writePart dir "base" base "// Base plate: arbor post (M5 nut slides into it from -x), clamp pocket, flange to the beam's -y face.\n";
  _ = writePart dir "cap" cap "// Cap on the arbor post: one M5x10 into the post's nut. Keeps the hub and pinion on, 0.3 mm play.\n";
  _ = writePath "{dir}/beam221.coscad" ("// 2020 rail, 221 mm long along Z: the tank model's slot-6 profile (../tank-model-2020/rail100.coscad).\n// Prints standing on an end.\nlen = 221\nmain = " + railProfile beamLen + "\n");
  _ = writePart dir "corner-bracket" bracket "// 2020 corner bracket: beam side face (y = 0) to foot top (z = 0), 18 wide, 5 thick, one M5x10 per leg. Print on its side.\n";
  _ = writePart dir "splice" splice "// Splice plate on the beams' +y faces across the joint, four M5x10. Print flat.\n";
  _ = writePart dir "stop" stop "// End stop on the beam top, one M5x10, key necks clear of the T-nut. Print upside down. Foam tape on the impact face.\n";
  _ = writePart dir "stopper-guide" guide "// Stopper guide on the beam's -y face, two M5x10. Print on its -x face.\n";
  _ = writePart dir "stopper" stopper "// Stopper plate, shown dropped in (full draw held). Print on its side.\n";
  _ = writePath "{dir}/spiral-launcher-build.assemble" buildPlan;
  _ = writePath "{dir}/launcher.assemble"
        ("// Spiral launcher at full draw (carriage at x = -{f stroke}), stopper in, glider loaded.\n" + partsHeader
         + "stopper ← stopper.coscad ×1 ▽fwd material=pla\n" + gliderParts + "\n" + fixedAt (neg stroke) 0 wingLe True);
  _ = writePath "{dir}/launcher-end.assemble"
        ("// End of the stroke: last rack tooth at the pinion, stopper out, glider still on the pin.\n" + partsHeader
         + gliderParts + "\n" + fixedAt 0.0 0 wingLe False);
  _ = writePath "{dir}/launcher-coast.assemble"
        ("// Carriage at the front stop (rack clear of the pinion), glider 100 mm on.\n" + partsHeader
         + gliderParts + "\n" + fixedAt (frontStop - carX1) 100.0 wingLe False);
  print "beams {f beamX0}..{f beamJoint}..{f beamX1}; stops {f rearStop} / {f frontStop}; stopper x {f stopperX0}..{f stopperX1}".

> build (R.modelDir "models/spiral-launcher" "build.sol").
