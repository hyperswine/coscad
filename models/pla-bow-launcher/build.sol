# PLA bow launcher for a 10 g glider. Dimensions in mm.
#
# Assembly frame: x forward (launch direction), y across, z up. z = 0 is the
# string and limb mid-plane; x = 0 is the limb mid-thickness at the riser.
# Every part is written in this frame; launcher.assemble places them.
#
# Limb numbers come from sim.sol: L 210, t 3.0, root width 44, tip width 8,
# string half-length 225, peak stress 25 MPa at a 201.2 mm draw. Run
# glider.sol first: the assemblies read glider.json.
#
#   "$SOL" models/pla-bow-launcher/build.sol
N = use "../lib/num".
J = use "sol/lib/json".
R = use "../lib/root".

# ---- design values (sim.sol) ---------------------------------------------
lenL = 210.0. tT = 3.0. b0 = 44.0. bT = 8.0.
r0 = 20.0.             # limb root (clamp edge) at y = +-r0
sHalf = 225.0.         # string half-length between tip hole and nock
braceNock = 0.0 - 39.6.  # nock x at rest
drawNock = 0.0 - 201.2.  # nock x at full draw (25 MPa)

# ---- helpers ---------------------------------------------------------------
f v = N.trim v.
half a b = Num.div (a + b) 2.
box x0 x1 y0 y1 z0 z1 =
  "(box {f (x1 - x0)} {f (y1 - y0)} {f (z1 - z0)} |> move {f (half x0 x1)} {f (half y0 y1)} {f (half z0 z1)})".
zc r z0 z1 x y = "(zcyl {f r} {f (z1 - z0)} |> move {f x} {f y} {f (half z0 z1)})".
xc r x0 x1 y z = "(xcyl {f r} {f (x1 - x0)} |> move {f (half x0 x1)} {f y} {f z})".
un parts = "(" + Str.join " ⊕ " parts + ")".
dif a parts = "(" + a + Str.join "" (List.map (fn q -> " ⊖ " + q) parts) + ")".
poly pts h = "(⮕ {f h} (poly " + Str.join " " (List.map (fn p -> (a, b) = p; "{f a} {f b}") pts) + "))".
neg x = 0 - x.

# ---- limb ------------------------------------------------------------------
# Print frame: u along the limb (assembly y), v across it (assembly z),
# thickness w = 0..T (assembly x). Width tapers linearly from b0 at the
# clamp edge (u = r0) to bT at u = r0 + L, the shape the model assumed.
tab0 = 7.0.                       # tab inner end inside the riser
tip = r0 + lenL.                  # 230
boltU = Num.div (tab0 + r0) 2.
boltV = 12.0.
boltR = 2.2.                      # M4 clearance
stringU = tip + 2.5.              # 1.8 mm string hole
limbPts = [(tab0, neg (Num.div b0 2)), (r0, neg (Num.div b0 2)), (tip, neg (Num.div bT 2)), (tip + 6, neg (Num.div bT 2)),
           (tip + 6, Num.div bT 2), (tip, Num.div bT 2), (r0, Num.div b0 2), (tab0, Num.div b0 2)].
limbPrint = dif (poly limbPts tT) [zc boltR (0 - 1) (tT + 1) boltU boltV, zc boltR (0 - 1) (tT + 1) boltU (neg boltV),
                                   zc 0.9 (0 - 1) (tT + 1) stringU 0].
# (u, v, w) -> (x, y, z) = (w - T/2, u, v): rotate about X then Z.
limb = "((ω 90 (θ 90 {limbPrint})) |> move {f (neg (Num.div tT 2))} 0 0)".

# ---- riser -----------------------------------------------------------------
rx0 = 0.0 - 8.0. rx1 = 22.0. ry = 20.0. rz0 = 0.0 - 28.0. rz1 = 26.0.
slotHalf = 1.625.                 # 3.25 mm slot for the 3.0 mm limb
slotZ = 22.2.
railY = 6.0. railZ0 = 0.0 - 16.0. railZ1 = 0.0 - 6.0.
cl = 0.2.                         # socket clearance
sockX1 = 20.0.
railBolts = [6.0, 14.0].
sorted2 a b = if a <= b then (a, b) else (b, a).
sideCuts s =
  (y0, y1) = sorted2 (s * tab0) (s * (ry + 1));
  slot = box (neg slotHalf) slotHalf y0 y1 (neg slotZ) slotZ;
  bolts = List.map (fn zb -> xc boltR (rx0 - 1) (rx1 + 1) (s * boltU) zb) [boltV, neg boltV];
  # 3 mm round on both slot exit edges: the limb pivots on them
  r = 3.0;
  rounds = List.map (fn sx ->
      (xa, xb) = sorted2 (sx * slotHalf) (sx * (slotHalf + r));
      (ya, yb) = sorted2 (s * (ry - r)) (s * (ry + 1));
      dif (box xa xb ya yb (neg slotZ - 1) (slotZ + 1)) [zc r (neg slotZ - 2) (slotZ + 2) (sx * (slotHalf + r)) (s * (ry - r))]) [1, 0 - 1];
  [slot] + bolts + rounds.
riserCuts = List.concat (List.map sideCuts [1, 0 - 1])
  + [box (rx0 - 1) sockX1 (neg railY - cl) (railY + cl) (railZ0 - cl) (railZ1 + cl)]
  + List.map (fn xb -> zc boltR (rz0 - 1) (rz1 + 1) xb 0) railBolts.
riser = dif (un [box rx0 rx1 (neg ry) ry rz0 rz1]) riserCuts.

# ---- carriage (local frame: rear face x = 0, string slot centre x = 3) ----
carLen = 24.0. carY = 9.0.
gap = 0.3.
top0 = railZ1 + gap.
top1 = railZ1 + gap + 3.2.
mastX0 = 16.0. mastX1 = 24.0. mastZ1 = 44.0.
pinZ = 41.0. pinLen = 8.0.
latchX = 11.0. latchR = 2.2.
slotX = 3.0.
# The glider boom (1.2 mm) rests in a groove on the mast cap so it cannot roll
# about the pin. The cap stays low: the stab passes 2.9 mm over it after release.
grooveHalf = 0.75.
capTop = mastZ1 + 1.2.
carriage = dif (un [
    box 0 carLen (neg carY) carY top0 top1,                                       # top plate
    box 0 carLen (railY + gap) carY (railZ0 - gap - 1.9) top0,                    # walls
    box 0 carLen (neg carY) (neg railY - gap) (railZ0 - gap - 1.9) top0,
    box 0 carLen 3.5 carY (railZ0 - gap - 1.9) (railZ0 - gap),                    # lips
    box 0 carLen (neg carY) (0 - 3.5) (railZ0 - gap - 1.9) (railZ0 - gap),
    box 0 6 (neg carY) carY top1 3.5,                                             # nock block
    box mastX0 mastX1 (0 - 2.5) 2.5 top1 mastZ1,                                  # mast
    box mastX0 mastX1 (0 - 3.0) 3.0 (mastZ1 - 0.01) capTop])                      # cap
  [box (slotX - 1) (slotX + 1) (neg carY - 1) (carY + 1) (0 - 0.5) 4,              # string slot
   zc latchR (top0 - 1) (top1 + 1) latchX 0,                                      # latch pin hole
   box (mastX0 - 1) (mastX1 + 1) (neg grooveHalf) grooveHalf mastZ1 (capTop + 1),  # boom groove
   # The glider pin is an M3x16 screw threaded into this hole, 8 mm protruding:
   # a printed horizontal stub would start in mid-air (slicer: floating cantilever).
   xc 1.25 (mastX0 - 1) (mastX1 + 0.1) 0 pinZ].

# ---- rail --------------------------------------------------------------------
carRearDraw = drawNock - slotX.          # -204.2
stopFace = carRearDraw - 1.0.            # 1 mm overdraw allowance (25.1 MPa)
railX0 = stopFace - 8.0.
railX1 = sockX1 - 0.5.
rail = dif (un [box railX0 railX1 (neg railY) railY railZ0 railZ1,
                box railX0 stopFace (neg carY) carY railZ1 4.0])                  # rear stop
  (List.map (fn xb -> zc boltR (railZ0 - 1) (railZ1 + 1) xb 0) railBolts
   + [zc latchR (railZ0 - 1) (railZ1 + 1) (carRearDraw + latchX) 0]).             # latch hole

# ---- bumper (TPU) on the riser rear face, over the rail ----------------------
carRearBrace = braceNock - slotX.        # -42.6
bumpX0 = rx0 - 5.0.
bumper = box bumpX0 rx0 (neg carY) carY top0 4.0.

# ---- latch pin (local frame at the carriage latch hole) ----------------------
# Handle runs across (y) and stays narrow in x: the drawn string leaves the
# slot at about 0.53 mm forward per mm outward, passing 2 mm behind it.
latchPin = un [zc 2.0 (railZ0 - 0.5) (top1 + 2) 0 0, box (0 - 1.5) 1.5 (0 - 8) 8 (top1 + 2) (top1 + 8)].

# ---- glider on the carriage (glider.sol writes glider.json) ------------------
gliderLines wingLe carRear =
  gx = carRear + mastX1 + pinLen + 0.3;   # slot end 0.3 mm ahead of the pin tip
  gz = pinZ;
  xw = wingLe - 18.0;
  "fuselage ← glider-fuselage.coscad ×1 ▽fwd material=pla\n"
  + "wing ← glider-wing.coscad ×1 ▽bot material=pla\n"
  + "stab ← glider-stab.coscad ×1 ▽bot material=pla\n"
  + "saddle ← glider-saddle.coscad ×1 ▽bot material=pla\n"
  + "fixed fuselage {f gx} 0 {f gz} 0 0 0\n"
  + "fixed saddle {f gx} 0 {f gz} 0 0 0\n"
  + "fixed wing {f (gx + xw)} 0 {f (gz + 12.3)} 0 -2 0\n"
  + "fixed stab {f (gx - 105)} 0 {f (gz + 7.1)} 0 0 0\n".

parts = "riser ← riser.coscad ×1 ▽bot material=pla\n"
  + "rail ← rail.coscad ×1 ▽bot material=pla\n"
  + "limbR ← limb.coscad ×1 ▽lft material=pla\n"
  + "limbL ← limb.coscad ×1 ▽lft material=pla\n"
  + "carriage ← carriage.coscad ×1 ▽bot material=pla\n"
  + "bumper ← bumper.coscad ×1 ▽bot material=tpu\n".
fixedFrame = "fixed riser 0 0 0 0 0 0\nfixed rail 0 0 0 0 0 0\nfixed limbR 0 0 0 0 0 0\nfixed limbL 0 0 0 180 0 0\n".

writePart dir name body note = writePath "{dir}/{name}.coscad" (note + "main = {body}\n").

build dir =
  g = unwrap (J.parse (readPath "{dir}/glider.json"));
  wingLe = unwrap (J.num (unwrap (J.get "wing_le_x" g)));
  _ = writePart dir "limb" limb ("// Two identical limbs; launcher.assemble flips one about x.\n"
        + "// Print flat, 100% infill, lines along the limb.\n");
  _ = writePart dir "riser" riser "// M4x35 x4 through the limb tabs, M4x60 x2 through the rail.\n";
  _ = writePart dir "carriage" carriage "// 8 g budget. Rear face x = 0; string sits in the slot at x = 3, z = 0.\n";
  _ = writePart dir "rail" rail "// Slide the carriage on from the front before fitting the riser.\n";
  _ = writePart dir "bumper" bumper "// TPU 95A (or foam tape). Glue to the riser rear face above the rail.\n";
  _ = writePart dir "latch-pin" latchPin "// Drop through carriage and rail at full draw; lift to launch.\n";
  _ = writePath "{dir}/launcher.assemble"
        ("// PLA bow launcher with the glider, carriage at full draw (nock x = {N.repr drawNock}).\nplate 256 256 6\n"
         + parts + "pin ← latch-pin.coscad ×1 ▽top material=pla\n\n" + fixedFrame
         + "fixed carriage {f carRearDraw} 0 0 0 0 0\nfixed bumper 0 0 0 0 0 0\n"
         + "fixed pin {f (carRearDraw + latchX)} 0 0 0 0 0\n" + gliderLines wingLe carRearDraw);
  # Glider at brace, carriage stopped and latch out: the moment of release.
  _ = writePath "{dir}/launcher-brace.assemble"
        ("// Carriage at brace (nock x = {N.repr braceNock}), latch removed, glider still on the pin.\nplate 256 256 6\n"
         + parts + "\n" + fixedFrame
         + "fixed carriage {f carRearBrace} 0 0 0 0 0\nfixed bumper 0 0 0 0 0 0\n" + gliderLines wingLe carRearBrace);
  # ---- report gaps that follow from the numbers
  _ = print "carriage front at brace: {N.fixed 1 (carRearBrace + carLen)}, bumper face {N.fixed 1 bumpX0}, gap {N.fixed 1 (bumpX0 - (carRearBrace + carLen))} mm";
  _ = print "carriage rear at full draw {N.fixed 1 carRearDraw}, stop face {N.fixed 1 stopFace}";
  _ = print "rail {N.fixed 1 railX0}..{N.fixed 1 railX1} = {N.fixed 1 (railX1 - railX0)} mm long";
  # sim.sol measures S from the end of the taper; the hole sits 2.5 mm beyond it.
  print "limb print length {N.fixed 1 (tip + 6 - tab0)} mm; string between tip hole centres {N.fixed 0 (2 * (sHalf + stringU - tip))} mm".

> build (R.modelDir "models/pla-bow-launcher" "build.sol").
