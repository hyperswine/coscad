# Belt propeller drive for the Bambu X1/P1 Z belt (GT2, 2 mm pitch, 1142 mm
# closed loop, 571 teeth). Dimensions in mm. Run from the CoScad root or this
# directory: sol build.sol
#
# A TT motor on top (or the hand crank, swapped in its place) turns a 60-tooth
# pulley. The belt runs round eight idlers and a slotted tensioner to a
# 20-tooth output pulley, a 3:1 step-up, whose shaft carries the TT propellers
# with the M5 gearbox's spacers and split collar. The input, socket, shaft,
# groove and crank details are the M5 gearbox and hand-crank ones.
#
# Frame: belt plane horizontal, floor top at z = 3, x to the output.
# Every part is written in the assembled frame for `coscad check`; plate*.coscad
# lay them out to print. belt.coscad is the belt's cord as a thin band (land and
# tension member only, no teeth) so the check sees its real path.
N = use "../lib/num".
R = use "../lib/root".

ps x = if x == Num.floor x then str x else N.fixed 4 x.
pt p = (x, y) = p; ps x + " " + ps y.
at x y = " |> move " + ps x + " " + ps y + " 0".

# Belt width is not published; a Z belt measured 6 mm is assumed. Flanges
# sit 1 mm wider. Change beltW if yours differs and regenerate.
beltW = 6.
gap = beltW + 1.
pitch = 2.
pitchR z = Num.div (z * pitch) (2 * N.pi).
# Stack: 1 mm shoulders lift every pulley and idler to z = 4.
stackBot = 4.
flangeT = 1.
beltBot = stackBot + flangeT.
beltTop = beltBot + gap.
stackTop = beltTop + flangeT.
stackH = stackTop - stackBot.

# Belt path: (name, x, y, pitch radius, wrap), wrap +1 = teeth side, -1 = back
# side, in loop order (counter-clockwise). Smooth idlers are sized so the
# belt's pitch line sits on these radii: teeth-side tips 1.0 mm inside,
# back face 0.38 mm inside.
zD = 60.
zO = 20.
tensionY = 0 - 8.55.
path = [("drive", 0 - 85, 0 - 78, pitchR zD, 1), ("lowerRight", 60, 0 - 85, 10, 1), ("neckLow", 55, 0 - 15, 7, 0 - 1),
  ("output", 100, 0, pitchR zO, 1), ("neckHigh", 55, 15, 7, 0 - 1), ("upperRight", 60, 85, 10, 1),
  ("notch", 25, 0, 8, 0 - 1), ("upperMid", 0 - 15, 85, 10, 1), ("tensioner", 0 - 50, tensionY, 8, 0 - 1),
  ("upperLeft", 0 - 85, 85, 10, 1)].

# Tangent between consecutive circles, as in the belt calculator.
tangent p q =
  (_, x1, y1, r1, s1) = p; (_, x2, y2, r2, s2) = q;
  ra = s1 * r1; rb = s2 * r2;
  dx = x2 - x1; dy = y2 - y1; d = N.hypot dx dy;
  c = Num.div (ra - rb) d; h = Num.sqrt (1 - c * c);
  ux = Num.div dx d; uy = Num.div dy d;
  nx = c * ux + h * uy; ny = c * uy - h * ux;
  ((x1 + ra * nx, y1 + ra * ny), (x2 + rb * nx, y2 + rb * ny)).
nexts xs = case xs of x :: rest -> rest + [x] | [] -> [].
prevs xs = case List.rev xs of x :: rest -> [x] + List.rev rest | [] -> [].
segs = List.map (fn pq -> (p, q) = pq; tangent p q) (List.zip path (nexts path)).
twoPi = 2 * N.pi.
wrap a = a - twoPi * Num.floor (Num.div a twoPi).
dist a b = (ax, ay) = a; (bx, by) = b; N.hypot (bx - ax) (by - ay).
# Arc on each pulley from the incoming tangent point to the outgoing one.
arcs = List.map (fn t -> (pul, io) = t; (inSeg, outSeg) = io;
    (_, cx, cy, r, s) = pul; (_, ain) = inSeg; (aout, _) = outSeg;
    (ix, iy) = ain; (ox, oy) = aout;
    t0 = N.atan2 (iy - cy) (ix - cx); t1 = N.atan2 (oy - cy) (ox - cx);
    sweep = if s > 0 then wrap (t1 - t0) else wrap (t0 - t1);
    (pul, t0, sweep))
  (List.zip path (List.zip (prevs segs) segs)).
beltLength = List.fold (fn acc ab -> (a, b) = ab; acc + dist a b) 0 segs
  + List.fold (fn acc arc -> (pul, _, sw) = arc; (_, _, _, r, _) = pul; acc + r * sw) 0 arcs.

# Pitch-line points with the left normal (towards the teeth) at each.
arcPoints arc = (pul, t0, sw) = arc; (_, cx, cy, r, s) = pul;
  n = Numeric.max 2 (Num.floor (Num.div (r * sw) 1.5) + 1);
  List.map (fn k -> th = t0 + s * Num.div (sw * k) n; c = N.cos th; sn = N.sin th;
      ((cx + r * c, cy + r * sn), (0 - s * c, 0 - s * sn))) (List.range 0 n).
pitchLine = List.concat (List.map arcPoints arcs).
band off = List.map (fn pn -> (p, nrm) = pn; (x, y) = p; (nx, ny) = nrm; (x + off * nx, y + off * ny)) pitchLine.
poly pts h z = "(⮕ " + ps h + " (poly " + Str.join " " (List.map pt pts) + ") |> z " + ps z + ")".
beltZ = beltBot + 0.5.
# The band runs from 0.2 mm on the teeth side (land, 0.254) to 0.32 mm on the
# back (back face, 0.38), less the 1.5 mm chords' sag, so it marks contact
# without counting tooth engagement as interference.
belt = "belt = " + poly (band (0 - 0.32)) beltW beltZ + " ⊖ " + poly (band 0.2) (beltW + 2) (beltZ - 1).

# GT2 pulley: outside radius 0.254 mm under the pitch radius, each groove a
# 0.7 mm radius round 0.75 mm deep.
gt2 name z hubR =
  rp = pitchR z; ro = rp - 0.254; fr = ro + 1.6;
  grooves = Str.join " ⊕ " (List.map (fn k -> th = Num.div (twoPi * k) z;
      "(zcyl 0.7 " + ps (gap + 2) + " |> move " + ps ((ro + 0.65) * N.cos th) + " " + ps ((ro + 0.65) * N.sin th) + " " + ps (beltBot + Num.div gap 2) + ")") (List.range 0 (z - 1)));
  name + "Teeth = (zcyl " + ps ro + " " + ps gap + " |> z " + ps (beltBot + Num.div gap 2) + ") ⊖ (" + grooves + ")\n"
  + name + "Flanges = (zcyl " + ps fr + " " + ps flangeT + " |> z " + ps (stackBot + Num.div flangeT 2) + ") ⊕ (zcyl " + ps fr + " " + ps flangeT + " |> z " + ps (beltTop + Num.div flangeT 2) + ")\n"
  + name + "Core = zcyl " + ps hubR + " " + ps stackH + " |> z " + ps (stackBot + Num.div stackH 2).

# Shaft details shared with the M5 gearbox.
doubleD = "(zcyl 2.85 40 ∩ box 4 7.7 40)".
journalR = 4.
floorT = 3.
bearingR = 4.3.
barBot = stackTop + 1.
barT = 4.
barTop = barBot + barT.
driveTopJournal = barTop + 0.3.
outputShoulder = barTop + 6.
shaftLen = 28.8.
dx = 0 - 85.
dy = 0 - 78.
ox = 100.

# Both pulleys sit on a 1 mm floor shoulder and a 2.2 mm radius pin, so they
# print flat; the bars' bearings carry their top journals.
pinHole = "pinHole = zcyl 2.48 2.6 |> z " + ps (stackBot + 1.2).
drive = pinHole + "\n" + gt2 "drive" zD 8 + "\n" + Str.join "\n"
  [ "driveJournals = zcyl 4 " + ps (driveTopJournal - stackTop + 0.1) + " |> z " + ps (Num.div (stackTop + driveTopJournal) 2 - 0.05)
  , "driveSocket = (zcyl 2.85 " + ps (driveTopJournal - 6.9) + " ∩ box 4 7.7 40) |> z " + ps (Num.div (driveTopJournal + 7.1) 2)
  , "drivePulley = ((driveTeeth ⊕ driveFlanges ⊕ driveCore ⊕ driveJournals) ⊖ driveSocket ⊖ pinHole)" + at dx dy ].

output = gt2 "output" zO 4 + "\n" + Str.join "\n"
  [ "outputJournals = zcyl 4 " + ps (outputShoulder - stackTop + 0.1) + " |> z " + ps (Num.div (stackTop + outputShoulder) 2 - 0.05)
  , "outputShaft = (zcyl 2.7 " + ps shaftLen + " ∩ box 3.7 8 40) |> z " + ps (outputShoulder + Num.div shaftLen 2)
  , "outputGroove = (zcyl 4 2.6 ⊖ zcyl 1.68 4) |> z " + ps (outputShoulder + 23.3)
  , "outputPulley = ((outputTeeth ⊕ outputFlanges ⊕ outputCore ⊕ outputJournals ⊕ (outputShaft ⊖ outputGroove)) ⊖ pinHole)" + at ox 0 ].

# Idlers: smooth, flanged, on separate printed posts (shoulder r6, post r4,
# M5 clearance) clamped by a cap and one M5 x 20 screw into a nut under the
# floor. The tensioner's screw runs in a floor slot.
idlerBore = 4.25.
postTop = stackTop + 0.2.
idlerR r s = if s > 0 then r - 1.0 else r - 0.38.
idlerParts = Str.join "\n" (List.concat (List.map (fn pul -> (name, x, y, r, s) = pul;
    if name == "drive" or name == "output" then [] else
      rr = idlerR r s;
      [ name + "Idler = ((zcyl " + ps rr + " " + ps gap + " |> z " + ps (beltBot + Num.div gap 2) + ") ⊕ (zcyl " + ps (rr + 2) + " " + ps flangeT + " |> z " + ps (stackBot + Num.div flangeT 2) + ") ⊕ (zcyl " + ps (rr + 2) + " " + ps flangeT + " |> z " + ps (beltTop + Num.div flangeT 2) + ") ⊖ zcyl " + ps idlerBore + " 40)" + at x y
      , name + "Post = ((zcyl 6 1 |> z " + ps (floorT + 0.5) + ") ⊕ (zcyl 4 " + ps (postTop - stackBot) + " |> z " + ps (Num.div (stackBot + postTop) 2) + ") ⊖ zcyl 2.8 40)" + at x y
      , name + "Cap = ((zcyl 6 2 |> z " + ps (postTop + 1) + ") ⊖ zcyl 2.8 40)" + at x y ]) path)).
idlerNames = List.filter (fn n -> n != "drive" and n != "output") (List.map (fn pul -> (n, _, _, _, _) = pul; n) path).

# Bars over the two driven shafts, each on two floor pillars and two M5 x 25.
dp1 = (dx + 21.2132, dy + 21.2132).
dp2 = (dx - 21.2132, dy - 21.2132).
op1 = (ox, 30).
op2 = (ox, 0 - 30).
pillars = [dp1, dp2, op1, op2].
bars = Str.join "\n"
  [ "driveBar = (((zcyl 6 " + ps barT + " |> move " + pt dp1 + " " + ps (barBot + Num.div barT 2) + ") |> hull (zcyl 6 " + ps barT + " |> move " + pt dp2 + " " + ps (barBot + Num.div barT 2) + ")) ⊕ (zcyl 7 " + ps barT + " |> move " + ps dx + " " + ps dy + " " + ps (barBot + Num.div barT 2) + ")) ⊖ (zcyl " + ps bearingR + " 40" + at dx dy + ") ⊖ (zcyl 2.8 40 |> move " + pt dp1 + " 0) ⊖ (zcyl 2.8 40 |> move " + pt dp2 + " 0)"
  , "outputBar = ((zcyl 6 " + ps barT + " |> move " + pt op1 + " " + ps (barBot + Num.div barT 2) + ") |> hull (zcyl 6 " + ps barT + " |> move " + pt op2 + " " + ps (barBot + Num.div barT 2) + ")) ⊖ (zcyl " + ps bearingR + " 40" + at ox 0 + ") ⊖ (zcyl 2.8 40 |> move " + pt op1 + " 0) ⊖ (zcyl 2.8 40 |> move " + pt op2 + " 0)" ].

# Base: 3 mm floor with lightening windows, a pin and shoulder under each
# pulley, idler screw holes, the tensioner slot, bar pillars and leg holes.
legs = [(0 - 112, 92), (106, 92), (106, 0 - 100), (0 - 112, 0 - 20)].
windows = [(0 - 72, 12, 0 - 28, 72), (0 - 28, 0 - 62, 12, 0 - 14), (0, 18, 40, 70), (15, 0 - 70, 40, 0 - 15),
  (0 - 100, 0 - 50, 0 - 80, 60), (70, 42, 98, 80), (70, 0 - 80, 98, 0 - 42), (0 - 25, 0 - 100, 45, 0 - 75)].
win w = (x0, y0, x1, y1) = w; "(box " + ps (x1 - x0) + " " + ps (y1 - y0) + " 10 |> move " + ps (Num.div (x0 + x1) 2) + " " + ps (Num.div (y0 + y1) 2) + " 1.5)".
hole p = (x, y) = p; "(zcyl 2.8 40" + at x y + ")".
fixedIdlers = List.filter (fn pul -> (n, _, _, _, _) = pul; not (List.has n ["drive", "output", "tensioner"])) path.
slot x ya yb = "((zcyl 2.8 40 |> move " + ps x + " " + ps ya + " 0) |> hull (zcyl 2.8 40 |> move " + ps x + " " + ps yb + " 0))".
base = Str.join "\n"
  [ "floorPlate = box 228 208 " + ps floorT + " |> move -1 -4 " + ps (Num.div floorT 2)
  , "barPillar = zcyl 5 " + ps (barBot - floorT) + " |> z " + ps (floorT + Num.div (barBot - floorT) 2)
  , "shaftSeat = (zcyl 7 1 |> z " + ps (floorT + 0.5) + ") ⊕ (zcyl 2.2 3.3 |> z " + ps (floorT + 1.65) + ")"
  , "baseSolid = floorPlate ⊖ (" + Str.join " ⊕ " (List.map win windows) + ") ⊕ " + Str.join " ⊕ " (List.map (fn p -> (x, y) = p; "(barPillar" + at x y + ")") pillars) + " ⊕ (shaftSeat" + at dx dy + ") ⊕ (shaftSeat" + at ox 0 + ")"
  , "baseCuts = " + Str.join " ⊕ " (List.map hole (pillars + legs + List.map (fn pul -> (_, x, y, _, _) = pul; (x, y)) fixedIdlers))
      + " ⊕ " + slot (0 - 50) (tensionY - 10) (tensionY + 10)
  , "beltBase = baseSolid ⊖ baseCuts" ].

# Legs: 12 mm, enough for the nuts and screw tips under the floor; a captive
# M5 nut at the top takes one M5 x 12 from above.
legParts = "leg = (zcyl 8 12 |> z -6) ⊖ (⎏ 6 4.792 4.2 |> z -4.19) ⊖ (zcyl 2.8 30 |> z -10)\n"
  + Str.join "\n" (List.map (fn t -> (i, p) = t; (x, y) = p; "leg" + str i + " = leg" + at x y) (List.zip (List.range 1 4) legs)).

# Hand crank on the drive shaft: the hand-crank crank, with the drive pulley's
# own top journal as its bearing, so the crank is stub, collar and arm only.
throw = 32.
armBot = barTop + 6.5.
armT = 6.
armTop = armBot + armT.
crank = Str.join "\n"
  [ "crankStub = (zcyl 2.65 " + ps (driveTopJournal + 0.2 - 8.5) + " ∩ box 3.6 7 40) |> move " + ps dx + " " + ps dy + " " + ps (Num.div (8.5 + driveTopJournal + 0.2) 2)
  , "crankCollar = zcyl 6 " + ps (armBot - driveTopJournal) + " |> move " + ps dx + " " + ps dy + " " + ps (Num.div (driveTopJournal + 0.1 + armBot) 2)
  , "crankArm = (zcyl 7 " + ps armT + " |> move " + ps dx + " " + ps dy + " " + ps (armBot + Num.div armT 2) + ") |> hull (zcyl 7 " + ps armT + " |> move " + ps (dx + throw) + " " + ps dy + " " + ps (armBot + Num.div armT 2) + ")"
  , "crank = (crankStub ⊕ crankCollar ⊕ crankArm) ⊖ (zcyl 2.8 20 |> move " + ps (dx + throw) + " " + ps dy + " " + ps armTop + ") ⊖ (⎏ 6 4.792 4.2 |> move " + ps (dx + throw) + " " + ps dy + " " + ps (armBot - 0.01) + ")"
  , "crankSleeve = ((zcyl 4 16.5 |> z " + ps (armTop + 8.25) + ") ⊕ (zcyl 6.5 1.5 |> z " + ps (armTop + 17.25) + ") ⊖ (zcyl 2.75 40 |> z " + ps armTop + "))" + at (dx + throw) dy
  , "crankKnob = ((zcyl 8 16.1) ⊖ (zcyl 4.3 18.1)) |> move " + ps (dx + throw) + " " + ps dy + " " + ps (armTop + 0.2 + 8.05) ].

# Motor plate: replaces the drive bar for motor drive. The M5 gearbox tongue
# (46 x 48, clamp slots 32..46 mm out at +-18 mm) runs along the diagonal over
# pillar dp1; the TT motor lies on it, shaft down through the same bearing into
# the drive socket, and the gearbox's clamp bridge holds it from above.
tongueCut d = "((zcyl 2.8 40 |> move 32 " + ps d + " 0) |> hull (zcyl 2.8 40 |> move 46 " + ps d + " 0))".
motorPlate = "motorPlate = ((((box 46 48 " + ps barT + " |> move 34 0 " + ps (barBot + Num.div barT 2) + ") ⊖ " + tongueCut 18 + " ⊖ " + tongueCut (0 - 18) + ") |> rotz 45 |> move " + ps dx + " " + ps dy + " 0) ⊕ driveBar) ⊖ (zcyl " + ps bearingR + " 40" + at dx dy + ") ⊖ (zcyl 2.8 40 |> move " + pt dp1 + " 0) ⊖ (zcyl 2.8 40 |> move " + pt dp2 + " 0)".
# TT motor clamp bar, the M5 gearbox `bridge`.
motorBridge = "motorBridge = (box 15 48 5 |> z 2.5) ⊖ (zcyl 2.8 12 |> move 0 18 3) ⊖ (zcyl 2.8 12 |> move 0 -18 3)".

> root = R.modelDir "models/belt-prop-drive" "build.sol";
  _ = print "belt pitch length {beltLength} mm (closed belt 1142)";
  defs = Str.join "\n" [belt, drive, output, idlerParts, bars, base, legParts, crank, motorPlate, motorBridge] + "\n";
  part name expr = writePath "{root}/{name}.coscad" (defs + "main = " + expr + "\n");
  _ = part "belt" "belt";
  _ = part "base" "beltBase";
  _ = part "drive" "drivePulley";
  _ = part "output" "outputPulley";
  _ = part "driveBar" "driveBar";
  _ = part "outputBar" "outputBar";
  _ = part "crank" "crank";
  _ = part "sleeve" "crankSleeve";
  _ = part "knob" "crankKnob";
  _ = List.map (fn n -> _ = part (n + "Idler") (n + "Idler"); _ = part (n + "Post") (n + "Post"); part (n + "Cap") (n + "Cap")) idlerNames;
  _ = List.map (fn i -> part ("leg" + str i) ("leg" + str i)) (List.range 1 4);
  _ = part "motorPlate" "motorPlate";
  part "motorBridge" "motorBridge".
