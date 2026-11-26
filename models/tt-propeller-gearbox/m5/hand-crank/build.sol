# Hand crank for the M5 TT propeller gearbox. Dimensions in mm.
# Run from the CoScad root or this directory: sol build.sol
#
# The gearbox stays as printed and tested (24 November). The TT motor comes
# off; a crank drives the input gear from above through a new cover:
# - cover: the original cover with a 4.3 mm radius bearing hole over the
#   input gear, the same hole the output journal already runs in;
# - crank: a 4.0 mm journal in that hole, a collar above it, and a stub with
#   the TT shaft's double-D profile that drops into the input gear's
#   existing socket (the socket is open at the gear face);
# - knob and sleeve: the knob spins on a flanged printed sleeve that one
#   M5 x 25 screw clamps to the arm through a captive M5 nut.
#
# Every part is written in the assembled frame (input axis at x = y = 0,
# gearbox floor at z = 0) for `coscad check`, and laid flat on plate.coscad.
R = use "../../../lib/root".

ps x = str x.

# Assembled heights, from gearbox.coscad: corners and rim end at 8.5, so the
# cover spans 8.5..11.5; the flipped driver gear spans 3.2..8.0 with its
# journal down through the floor.
coverZ = 8.5.
gearTop = 8.0.
coverTop = coverZ + 3.

# Crank, assembled frame.
stubBottom = 1.6.
journalBottom = gearTop + 0.1.
collarBottom = coverTop + 0.3.
# The arm clears socket-cap screw heads (5 mm) on the two input-end cover bolts,
# inserted from the top, with the handle screw's 1 mm tip below it.
armBottom = coverTop + 7.5.
armT = 6.
armTop = armBottom + armT.
throw = 32.

crank = Str.join "\n"
  [ "crankStub = (zcyl 2.65 " + ps (journalBottom - stubBottom + 0.2) + " ∩ box 3.6 7 20) |> z " + ps ((journalBottom + stubBottom) / 2 + 0.1)
  , "crankJournal = zcyl 4 " + ps (collarBottom - journalBottom + 0.2) + " |> z " + ps ((journalBottom + collarBottom) / 2 + 0.1)
  , "crankCollar = zcyl 6 " + ps (armBottom - collarBottom + 0.2) + " |> z " + ps ((collarBottom + armBottom) / 2 + 0.1)
  , "crankArm = (zcyl 7 " + ps armT + " |> z " + ps (armBottom + armT / 2) + ") |> hull (zcyl 7 " + ps armT + " |> move " + ps throw + " 0 " + ps (armBottom + armT / 2) + ")"
  , "crankScrewHole = zcyl 2.8 20 |> move " + ps throw + " 0 " + ps armTop + " |> frame handleHole top top bak 0 0 -10"
  , "crankNutPocket = ⎏ 6 4.792 4.2 |> move " + ps throw + " 0 " + ps (armBottom - 0.01) + " |> frame handleNut top bot bak"
  , "crank = (crankStub ⊕ crankJournal ⊕ crankCollar ⊕ crankArm) ⊖ crankScrewHole ⊖ crankNutPocket" ].

# Knob and sleeve, assembled frame (above the arm's handle end).
sleeveLen = 16.5.
flangeT = 1.5.
knobLen = sleeveLen - 0.4.
handle = Str.join "\n"
  [ "crankSleeve = ((zcyl 4 " + ps sleeveLen + " |> z " + ps (armTop + sleeveLen / 2) + ") ⊕ (zcyl 6.5 " + ps flangeT + " |> z " + ps (armTop + sleeveLen + flangeT / 2) + ") ⊖ (zcyl 2.75 40 |> z " + ps armTop + ")) |> x " + ps throw + " |> frame handleSeat top top bak"
  , "crankKnob = ((zcyl 8 " + ps knobLen + ") ⊖ (zcyl 4.3 " + ps (knobLen + 2) + ")) |> move " + ps throw + " 0 " + ps (armTop + 0.2 + knobLen / 2) ].

# Cover screws: x = -15 and span + 15, y = +-18 (gearbox.coscad `holes`).
screws = [("A", "-15", "18"), ("B", "-15", "-18"), ("C", "(span + 15)", "18"), ("D", "(span + 15)", "-18")].
cutter id x y extra = "(bolt |> move " + x + " " + y + " 0" + extra + ")".
# `bolt` spans z -4..16 in each part's own frame.
coverSeats = Str.join " " (List.map (fn t -> (id, x, y) = t; "⊖ " + cutter id x y (" |> frame seat" + id + " top top bak 0 0 -13")) screws).
baseFrames = Str.join " " (List.map (fn t -> (id, x, y) = t; "⊖ " + cutter id x y (" |> frame hole" + id + " top top bak 0 0 -7.5 |> frame nut" + id + " bot bot bak 0 0 4")) screws).
coverPart = "crankCover = ((cover ⊖ inputBearing) " + coverSeats + ") |> z " + ps coverZ + "\nframedBase = base " + baseFrames.

# Stand: a 10 mm open frame under the floor, so the box sits flat now that
# nothing is mounted below it. The input journal hangs 1.1 mm below the
# floor; the cover nuts sit in hex pockets with room below for M5 x 20 tips.
standT = 10.
stand = Str.join "\n"
  [ "standRim = (box length 44 " + ps standT + " ⊖ box (length - 10) 34 " + ps (standT + 2) + ") |> move (span / 2) 0 " + ps (0 - standT / 2)
  , "standPads = (zcyl 9 " + ps standT + " |> move -15 18 0) ⊕ (zcyl 9 " + ps standT + " |> move -15 -18 0) ⊕ (zcyl 9 " + ps standT + " |> move (span + 15) 18 0) ⊕ (zcyl 9 " + ps standT + " |> move (span + 15) -18 0) ⊕ zcyl 9 " + ps standT + " ⊕ box 6 44 " + ps standT
  , "standNut = ⎏ 6 4.792 4.21 |> z " + ps (standT / 2 - 4.2)
  , "standClear = zcyl 3 " + ps (standT + 2)
  , "standCuts = (zcyl 5.5 " + ps (standT + 2) + ") " + Str.join " " (List.map (fn t -> (id, x, y) = t; "⊕ (standNut |> move " + x + " " + y + " 0 |> frame hole" + id + " top top bak 0 0 -0.01 |> frame nut" + id + " top bot bak 0 0 -0.01) ⊕ (standClear |> move " + x + " " + y + " 0)") screws)
  , "crankStand = (standRim ⊕ (standPads |> z " + ps (0 - standT / 2) + ")) ⊖ (standCuts |> z " + ps (0 - standT / 2) + ")" ].
driverPart = "placedDriver = driver |> roty 180 |> z " + ps gearTop.

> root = R.modelDir "models/tt-propeller-gearbox/m5/hand-crank" "build.sol";
  gearbox = readPath "{root}/../gearbox.coscad";
  # Every definition from the tested gearbox, without its plate `main`.
  defs = Str.join "\n" (List.filter (fn l -> not (Str.startsWith "main =" l)) (Str.split 10 gearbox));
  common = defs + "\n" + crank + "\n" + handle + "\n" + coverPart + "\n" + driverPart + "\n" + stand + "\n";
  part name expr = writePath "{root}/{name}.coscad" (common + "main = " + expr + "\n");
  _ = part "base" "framedBase";
  _ = part "stand" "crankStand";
  _ = part "driver" "placedDriver";
  _ = part "cover" "crankCover";
  _ = part "crank" "crank";
  _ = part "sleeve" "crankSleeve";
  _ = part "knob" "crankKnob";
  # Print plate: cover flat, crank arm-down, sleeve flange-down, knob upright.
  _ = part "plate-stand" "crankStand |> anchor bot";
  part "plate" "(cover ⊖ inputBearing) ⊕ ((crank |> roty 180 |> anchor bot) |> move 0 52 0) ⊕ ((crankSleeve |> roty 180 |> anchor bot) |> move 60 52 0) ⊕ ((crankKnob |> anchor bot) |> move 90 52 0)".
