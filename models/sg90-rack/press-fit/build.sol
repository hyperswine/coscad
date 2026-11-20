# Press-fit SG90 rack and pinion, revision 3. Dimensions in mm.
# Run from the CoScad root or this directory: sol build.sol
#
# Revision 3 (2026-11-21): base only. Cable slot moved to the shaft end and
# floor raised 3 mm (see the socket section). Writes base.coscad as well, so
# the base can be reprinted alone. The revision 2 pinion bore (4.95 mm)
# press-fits the shaft well.
#
# Revision 2 (2026-11-21), after the first print:
# - Pinion 22 teeth at module 1.5: pitch radius 16.5 (was 10, module 1).
# - Bore 4.95 nominal with 0.4 mm lead-in chamfers (was a plain 4.70,
#   too tight). The test strip prints 4.80 to 5.20 so the next pinion can
#   use the bore that fits.
# - Rack pitch line 0.2 mm further from the shaft for backlash.
# - Crush ribs on the socket's end walls so the servo cannot slide
#   toward or away from the rack.
# - Pinion face 7 mm against a 4 mm rack, so the press depth on the
#   shaft has about 1.5 mm of slack either way.
N = use "../../lib/num".
R = use "../../lib/root".

ps x = if x == Num.floor x then str x else N.repr x.
placeAt x y z = " |> move " + ps x + " " + ps y + " " + ps z.
# Centred primitives placed by their bottom face.
box w d h x y z = "(box " + ps w + " " + ps d + " " + ps h + placeAt x y (z + Num.div h 2) + ")".
disk r h x y z = "(zcyl " + ps r + " " + ps h + placeAt x y (z + Num.div h 2) + ")".
# frustum is already bottom-anchored.
cone r1 r2 h x y z = "(frustum " + ps r1 + " " + ps r2 + " " + ps h + placeAt x y z + ")".
poly pts h = "(⮕ " + ps h + " (poly " + Str.join " " (List.map (fn p -> (x, y) = p; N.fixed 4 x + " " + N.fixed 4 y) pts) + "))".
union parts = "(" + Str.join " ⊕ " parts + ")".
minus shape cuts = List.fold (fn s c -> s + " ⊖ " + c) shape cuts.
move s x y z = "(" + s + placeAt x y z + ")".
m1 = 0 - 1.

# Gear.
gmod = 1.5.
n = 22.
face = 7.
pitchR = Num.div (gmod * n) 2.
alpha = N.radians 20.
rb = pitchR * N.cos alpha.
tipR = pitchR + gmod.
radii = [pitchR - 1.25 * gmod] + List.map (fn j -> rb + Num.div ((tipR - rb) * j) 6) (List.range 0 6).
gear = List.concat (List.map (fn i ->
    List.concat (List.map (fn sd -> (side, rs) = sd;
      List.map (fn r ->
        a = N.acos (Numeric.min 1 (Num.div rb r));
        ang = Num.div ((i + 0.5) * 2 * N.pi) n + side * (Num.div N.pi (2 * n) + N.tan alpha - alpha - N.tan a + a);
        (r * N.cos ang, r * N.sin ang)) rs)
    [(m1, radii), (1, List.rev radii)]))
  (List.range 0 (n - 1))).

# Bore: plain through-hole with a chamfer at each end so the first layer's
# squish does not close the entry.
chamfer = 0.4.
bore d = minus (poly gear face) [ disk (Num.div d 2) (face + 2) 0 0 m1
  , cone (Num.div d 2 + chamfer + 0.01) (Num.div d 2 - 0.01) (chamfer + 0.02) 0 0 (0 - 0.01)
  , cone (Num.div d 2 - 0.01) (Num.div d 2 + chamfer + 0.01) (chamfer + 0.02) 0 0 (face - chamfer - 0.01) ].
pinionBore = 4.95.
pinion = bore pinionBore.

# Rack, in assembly coordinates: shaft at the origin, teeth facing -y.
backlash = 0.2.
pitchY = pitchR + backlash.
rackTip = pitchY - 0.95 * gmod.
rackRoot = pitchY + 1.25 * gmod.
pitch = N.pi * gmod.
slotY = pitchY + 8.
rackBack = slotY + 5.3.
halfLen = 26.
rackH = 4.
teeth = List.concat (List.map (fn i ->
    List.map (fn ys -> (y, side) = ys; (i * pitch + side * (Num.div pitch 4 - 0.11 + (y - pitchY) * N.tan alpha), y))
      [(rackRoot, m1), (rackTip, m1), (rackTip, 1), (rackRoot, 1)])
  (List.range (0 - 5) 5)).
rackPts = [(0 - halfLen, rackBack), (0 - halfLen, rackRoot)] + teeth + [(halfLen, rackRoot), (halfLen, rackBack)].
# 47 mm closed slot over a 22 mm rail: 25 mm of mechanical travel.
rack = minus (poly rackPts rackH) [box 47 3.55 6 0 slotY m1].

# Base. SG90 upright, shaft at XY origin, heights from the bed.
sockY = 0 - 5.2.
pocketL = 23.4.
pocketW = 12.8.
# Revision 3: the SG90's cable leaves from the shaft end, so the lead slot
# is in the +y wall (rack side). Revision 2 had it in the -y wall, which
# seated the servo backwards with the shaft about 10 mm from the gear.
# The floor is 3 mm higher than revision 2 to lift the pinion to the rack.
floorZ = 3.8.
sockH = floorZ + 14.2.
socket = minus (box 18 29 sockH 0 sockY 0) [ box pocketW pocketL 20 0 sockY floorZ
  , box 6 4 20 0 (sockY + Num.div pocketL 2 + 1.4) floorZ ].
ribR = 0.65.
ribIn = 0.3.
ribZ = floorZ + 0.2.
ribH = sockH - ribZ - 1.
sideRibs = List.map (fn x -> disk ribR ribH x (0 - 10) ribZ) [6.65, 0 - 6.65].
endY = Num.div pocketL 2 - ribIn + ribR.
# Two ribs per end, clear of the lead slot in the +y wall.
endRibs = List.concat (List.map (fn x ->
    [disk ribR ribH x (sockY + endY) ribZ, disk ribR ribH x (sockY - endY) ribZ]) [4.5, 0 - 4.5]).
shelfTop = 28.2.
# The shelf front stays 1 mm outside the gear tips, so a pinion pressed
# low still clears it. Rack teeth overhang the front edge.
shelfFront = tipR + 1.
shelfBack = rackBack + 0.5.
shelf = box 27 (shelfBack - shelfFront) shelfTop 0 (Num.div (shelfFront + shelfBack) 2) 0.
foot = box 28 (shelfBack - 7) 2 0 (Num.div (shelfBack + 7) 2) 0.
railH = 4.35.
rail = box 22 3.05 railH 0 slotY shelfTop.
studs = List.map (fn x -> disk 0.96 2.6 x slotY (shelfTop + railH - 0.1)) [0 - 8, 8].
base = union ([socket, foot, shelf, rail] + sideRibs + endRibs + studs).

keeper = minus (box 22 7 2.4 0 0 0) (List.map (fn x -> disk 0.93 4 x 0 m1) [0 - 8, 8]).

# Bore test strip: 4.80 to 5.20, notches on the near edge count the hole
# (one notch = 4.80, five = 5.20).
testBores = [(0, 4.8), (1, 4.9), (2, 5.0), (3, 5.1), (4, 5.2)].
strip = minus (box 56 12 face 0 0 0) (List.concat (List.map (fn t -> (i, d) = t;
      x = (i - 2) * 11;
      [ disk (Num.div d 2) (face + 2) x 1 m1
      , cone (Num.div d 2 + chamfer + 0.01) (Num.div d 2 - 0.01) (chamfer + 0.02) x 1 (0 - 0.01)
      , cone (Num.div d 2 - 0.01) (Num.div d 2 + chamfer + 0.01) (chamfer + 0.02) x 1 (face - chamfer - 0.01) ]
      + List.map (fn k -> box 0.8 1.6 (face + 2) (x - 3 + k * 1.5) (0 - 6) m1) (List.range 0 i))
    testBores)).

plate = union
  [ base
  , move rack 52 (0 - 6) 0
  , move pinion 52 (0 - 32) 0
  , move keeper 0 40 0
  , move strip 0 (0 - 38) 0 ].
# Assembly for the build plan (coscad plan): every part in place, shaft at
# the origin. The SG90 is a simple stand-in (bought part, not printed): body,
# ears, turret and spline, with its turret top under the pinion. Its exact
# heights vary between clones; the pinion height here is where it meshes.
pinionZ = shelfTop - Num.div (face - rackH) 2.
bodyL = 22.5.
bodyW = 11.8.
shaftToEnd = 6.2.
bodyY = shaftToEnd - Num.div bodyL 2.
turretZ = pinionZ - 4.2.
servo = union
  [ box bodyW bodyL (turretZ - floorZ) 0 bodyY floorZ
  , box bodyW 32.3 2.5 0 bodyY (floorZ + 15.9)
  , disk 5.9 4 0 0 turretZ
  , disk 2.4 4 0 0 (turretZ + 4) ].
keeperZ = shelfTop + railH.
asmParts =
  [ ("base", base)
  , ("servo", servo)
  , ("pinion", move pinion 0 0 pinionZ)
  , ("rack", move rack (Num.div pitch 2) 0 shelfTop)
  , ("keeper", move keeper 0 slotY keeperZ) ].
assembly = Str.join "\n"
  [ "// Build plan for the press-fit SG90 rack and pinion (coscad plan). Written by build.sol."
  , "plate 256 256 6"
  , "base ← base.coscad ×1 ▽bot material=pla preload=hand"
  , "servo ← servo.coscad ×0 ▽bot mass=9"
  , "pinion ← pinion.coscad ×1 ▽bot material=pla"
  , "rack ← rack.coscad ×1 ▽bot material=pla"
  , "keeper ← keeper.coscad ×1 ▽bot material=pla"
  , ""
  , "fixed base 0 0 0 0 0 0"
  , "fixed servo 0 0 0 0 0 0"
  , "fixed pinion 0 0 0 0 0 0"
  , "fixed rack 0 0 0 0 0 0"
  , "fixed keeper 0 0 0 0 0 0"
  , ""
  , "joint push servo base axis=top"
  , "joint push pinion servo axis=top"
  , "joint drop rack base axis=top"
  , "joint push keeper base axis=top"
  , "" ].
> root = R.modelDir "models/sg90-rack/press-fit" "build.sol";
  _ = writePath "{root}/plate.coscad" ("main = " + plate + "\n");
  _ = List.map (fn np -> (name, shape) = np; writePath "{root}/{name}.coscad" ("main = " + shape + "\n")) asmParts;
  writePath "{root}/sg90-rack.assemble" assembly.
