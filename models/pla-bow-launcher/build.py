"""PLA bow launcher for a 10 g glider. Dimensions in mm.

Assembly frame: x forward (launch direction), y across, z up. z = 0 is the
string and limb mid-plane; x = 0 is the limb mid-thickness at the riser.
Every part is written in this frame; launcher.assemble places them.

Limb numbers come from sim.py: L 210, t 3.0, root width 44, tip width 8,
string half-length 225, peak stress 25 MPa at a 201.2 mm draw.
"""
from pathlib import Path

root = Path(__file__).resolve().parent

# ---- design values (sim.py) --------------------------------------------
L, T, B0, BT = 210.0, 3.0, 44.0, 8.0
R0 = 20.0            # limb root (clamp edge) at y = +-R0
S_HALF = 225.0       # string half-length between tip hole and nock
BRACE_NOCK = -39.6   # nock x at rest
DRAW_NOCK = -201.2   # nock x at full draw (25 MPa)

# ---- helpers ------------------------------------------------------------
def f(v): return f"{v:.4f}".rstrip("0").rstrip(".")
def box(x0, x1, y0, y1, z0, z1):
    return (f"(box {f(x1-x0)} {f(y1-y0)} {f(z1-z0)} |> move "
            f"{f((x0+x1)/2)} {f((y0+y1)/2)} {f((z0+z1)/2)})")
def zc(r, z0, z1, x, y): return f"(zcyl {f(r)} {f(z1-z0)} |> move {f(x)} {f(y)} {f((z0+z1)/2)})"
def xc(r, x0, x1, y, z): return f"(xcyl {f(r)} {f(x1-x0)} |> move {f((x0+x1)/2)} {f(y)} {f(z)})"
def U(*p): return "(" + " ⊕ ".join(p) + ")"
def D(a, *p): return "(" + a + "".join(" ⊖ " + q for q in p) + ")"
def poly(pts, h): return f"(⮕ {f(h)} (poly " + " ".join(f"{f(a)} {f(b)}" for a, b in pts) + "))"
def write(name, body, extra=""):
    (root / f"{name}.coscad").write_text(extra + f"main = {body}\n")

# ---- limb ----------------------------------------------------------------
# Print frame: u along the limb (assembly y), v across it (assembly z),
# thickness w = 0..T (assembly x). Width tapers linearly from B0 at the
# clamp edge (u = R0) to BT at u = R0 + L, the shape the model assumed.
TAB0 = 7.0                      # tab inner end inside the riser
TIP = R0 + L                    # 230
pts = [(TAB0, -B0/2), (R0, -B0/2), (TIP, -BT/2), (TIP + 6, -BT/2),
       (TIP + 6, BT/2), (TIP, BT/2), (R0, B0/2), (TAB0, B0/2)]
BOLT_U, BOLT_V, BOLT_R = (TAB0 + R0) / 2, 12.0, 2.2      # M4 clearance
STRING_U = TIP + 2.5                                      # 1.8 mm string hole
limb_print = D(poly(pts, T),
               zc(BOLT_R, -1, T + 1, BOLT_U, BOLT_V), zc(BOLT_R, -1, T + 1, BOLT_U, -BOLT_V),
               zc(0.9, -1, T + 1, STRING_U, 0))
# (u, v, w) -> (x, y, z) = (w - T/2, u, v): rotate about X then Z.
limb = f"((ω 90 (θ 90 {limb_print})) |> move {f(-T/2)} 0 0)"
write("limb", limb, "// Two identical limbs; launcher.assemble flips one about x.\n"
      "// Print flat, 100% infill, lines along the limb.\n")

# ---- riser -------------------------------------------------------------
RX0, RX1, RY, RZ0, RZ1 = -8.0, 22.0, 20.0, -28.0, 26.0
SLOT_HALF = 1.625               # 3.25 mm slot for the 3.0 mm limb
SLOT_Z = 22.2
RAIL_Y, RAIL_Z0, RAIL_Z1 = 6.0, -16.0, -6.0
CL = 0.2                        # socket clearance
riser_parts = [box(RX0, RX1, -RY, RY, RZ0, RZ1)]
cuts = []
for s in (1, -1):
    y0, y1 = sorted((s * TAB0, s * (RY + 1)))
    cuts.append(box(-SLOT_HALF, SLOT_HALF, y0, y1, -SLOT_Z, SLOT_Z))
    for zb in (BOLT_V, -BOLT_V):
        cuts.append(xc(BOLT_R, RX0 - 1, RX1 + 1, s * BOLT_U, zb))
    # 3 mm round on both slot exit edges: the limb pivots on them
    r = 3.0
    for sx in (1, -1):
        xa, xb = sorted((sx * SLOT_HALF, sx * (SLOT_HALF + r)))
        ya, yb = sorted((s * (RY - r), s * (RY + 1)))
        cuts.append(D(box(xa, xb, ya, yb, -SLOT_Z - 1, SLOT_Z + 1),
                      zc(r, -SLOT_Z - 2, SLOT_Z + 2, sx * (SLOT_HALF + r), s * (RY - r))))
# rail socket, open at the rear face, and two vertical M4 bolts
SOCK_X1 = 20.0
cuts.append(box(RX0 - 1, SOCK_X1, -RAIL_Y - CL, RAIL_Y + CL, RAIL_Z0 - CL, RAIL_Z1 + CL))
RAIL_BOLTS = (6.0, 14.0)
for xb in RAIL_BOLTS:
    cuts.append(zc(BOLT_R, RZ0 - 1, RZ1 + 1, xb, 0))
riser = D(U(*riser_parts), *cuts)
write("riser", riser, "// M4x35 x4 through the limb tabs, M4x60 x2 through the rail.\n")

# ---- carriage (local frame: rear face x = 0, string slot centre x = 3) ----
CAR_LEN, CAR_Y = 24.0, 9.0
GAP = 0.3
TOP0, TOP1 = RAIL_Z1 + GAP, RAIL_Z1 + GAP + 3.2
MAST_X0, MAST_X1, MAST_Z1 = 16.0, 24.0, 44.0
PIN_Z, PIN_LEN, PIN_R = 41.0, 8.0, 1.5
LATCH_X, LATCH_R = 11.0, 2.2
SLOT_X = 3.0
# The glider boom (1.2 mm) rests in a groove on the mast cap so it cannot roll
# about the pin. The cap stays low: the stab passes 2.9 mm over it after release.
GROOVE_HALF, CAP_TOP = 0.75, MAST_Z1 + 1.2
car = U(
    box(0, CAR_LEN, -CAR_Y, CAR_Y, TOP0, TOP1),                                     # top plate
    box(0, CAR_LEN, RAIL_Y + GAP, CAR_Y, RAIL_Z0 - GAP - 1.9, TOP0),                 # walls
    box(0, CAR_LEN, -CAR_Y, -RAIL_Y - GAP, RAIL_Z0 - GAP - 1.9, TOP0),
    box(0, CAR_LEN, 3.5, CAR_Y, RAIL_Z0 - GAP - 1.9, RAIL_Z0 - GAP),                 # lips
    box(0, CAR_LEN, -CAR_Y, -3.5, RAIL_Z0 - GAP - 1.9, RAIL_Z0 - GAP),
    box(0, 6, -CAR_Y, CAR_Y, TOP1, 3.5),                                             # nock block
    box(MAST_X0, MAST_X1, -2.5, 2.5, TOP1, MAST_Z1),                                 # mast
    box(MAST_X0, MAST_X1, -3.0, 3.0, MAST_Z1 - 0.01, CAP_TOP))                       # cap
car = D(car,
        box(SLOT_X - 1, SLOT_X + 1, -CAR_Y - 1, CAR_Y + 1, -0.5, 4),                 # string slot
        zc(LATCH_R, TOP0 - 1, TOP1 + 1, LATCH_X, 0),                                 # latch pin hole
        box(MAST_X0 - 1, MAST_X1 + 1, -GROOVE_HALF, GROOVE_HALF, MAST_Z1, CAP_TOP + 1),  # boom groove
        # The glider pin is an M3x16 screw threaded into this hole, 8 mm protruding:
        # a printed horizontal stub would start in mid-air (slicer: floating cantilever).
        xc(1.25, MAST_X0 - 1, MAST_X1 + 0.1, 0, PIN_Z))
write("carriage", car, "// 8 g budget. Rear face x = 0; string sits in the slot at x = 3, z = 0.\n")

# ---- rail -----------------------------------------------------------------
CAR_REAR_DRAW = DRAW_NOCK - SLOT_X          # -204.2
STOP_FACE = CAR_REAR_DRAW - 1.0             # 1 mm overdraw allowance (25.1 MPa)
RAIL_X0, RAIL_X1 = STOP_FACE - 8.0, SOCK_X1 - 0.5
rail = D(U(box(RAIL_X0, RAIL_X1, -RAIL_Y, RAIL_Y, RAIL_Z0, RAIL_Z1),
           box(RAIL_X0, STOP_FACE, -CAR_Y, CAR_Y, RAIL_Z1, 4.0)),                    # rear stop
         *[zc(BOLT_R, RAIL_Z0 - 1, RAIL_Z1 + 1, xb, 0) for xb in RAIL_BOLTS],
         zc(LATCH_R, RAIL_Z0 - 1, RAIL_Z1 + 1, CAR_REAR_DRAW + LATCH_X, 0))         # latch hole
write("rail", rail, "// Slide the carriage on from the front before fitting the riser.\n")

# ---- bumper (TPU) on the riser rear face, over the rail -------------------
CAR_REAR_BRACE = BRACE_NOCK - SLOT_X        # -42.6
BUMP_X0 = RX0 - 5.0
bumper = box(BUMP_X0, RX0, -CAR_Y, CAR_Y, TOP0, 4.0)
write("bumper", bumper, "// TPU 95A (or foam tape). Glue to the riser rear face above the rail.\n")

# ---- latch pin (local frame at the carriage latch hole) -------------------
# Handle runs across (y) and stays narrow in x: the drawn string leaves the
# slot at about 0.53 mm forward per mm outward, passing 2 mm behind it.
pin = U(zc(2.0, RAIL_Z0 - 0.5, TOP1 + 2, 0, 0), box(-1.5, 1.5, -8, 8, TOP1 + 2, TOP1 + 8))
write("latch-pin", pin, "// Drop through carriage and rail at full draw; lift to launch.\n")

# ---- glider on the carriage (glider.py writes glider.json) ----------------------
def glider_lines(car_rear):
    import json
    g = json.loads((root / "glider.json").read_text())
    gx, gz = car_rear + MAST_X1 + PIN_LEN + 0.3, PIN_Z     # slot end 0.3 mm ahead of the pin tip
    xw = g["wing_le_x"] - 18.0
    return (f"fuselage ← glider-fuselage.coscad ×1 ▽fwd material=pla\n"
            f"wing ← glider-wing.coscad ×1 ▽bot material=pla\n"
            f"stab ← glider-stab.coscad ×1 ▽bot material=pla\n"
            f"saddle ← glider-saddle.coscad ×1 ▽bot material=pla\n"
            f"fixed fuselage {f(gx)} 0 {f(gz)} 0 0 0\n"
            f"fixed saddle {f(gx)} 0 {f(gz)} 0 0 0\n"
            f"fixed wing {f(gx + xw)} 0 {f(gz + 12.3)} 0 -2 0\n"
            f"fixed stab {f(gx - 105)} 0 {f(gz + 7.1)} 0 0 0\n")

# ---- assembly ---------------------------------------------------------------
(root / "launcher.assemble").write_text(f"""// PLA bow launcher with the glider, carriage at full draw (nock x = {DRAW_NOCK}).
plate 256 256 6
riser ← riser.coscad ×1 ▽bot material=pla
rail ← rail.coscad ×1 ▽bot material=pla
limbR ← limb.coscad ×1 ▽lft material=pla
limbL ← limb.coscad ×1 ▽lft material=pla
carriage ← carriage.coscad ×1 ▽bot material=pla
bumper ← bumper.coscad ×1 ▽bot material=tpu
pin ← latch-pin.coscad ×1 ▽top material=pla

fixed riser 0 0 0 0 0 0
fixed rail 0 0 0 0 0 0
fixed limbR 0 0 0 0 0 0
fixed limbL 0 0 0 180 0 0
fixed carriage {f(CAR_REAR_DRAW)} 0 0 0 0 0
fixed bumper 0 0 0 0 0 0
fixed pin {f(CAR_REAR_DRAW + LATCH_X)} 0 0 0 0 0
""" + glider_lines(CAR_REAR_DRAW))

# Glider at brace, carriage stopped and latch out: the moment of release.
(root / "launcher-brace.assemble").write_text(f"""// Carriage at brace (nock x = {BRACE_NOCK}), latch removed, glider still on the pin.
plate 256 256 6
riser ← riser.coscad ×1 ▽bot material=pla
rail ← rail.coscad ×1 ▽bot material=pla
limbR ← limb.coscad ×1 ▽lft material=pla
limbL ← limb.coscad ×1 ▽lft material=pla
carriage ← carriage.coscad ×1 ▽bot material=pla
bumper ← bumper.coscad ×1 ▽bot material=tpu

fixed riser 0 0 0 0 0 0
fixed rail 0 0 0 0 0 0
fixed limbR 0 0 0 0 0 0
fixed limbL 0 0 0 180 0 0
fixed carriage {f(CAR_REAR_BRACE)} 0 0 0 0 0
fixed bumper 0 0 0 0 0 0
""" + glider_lines(CAR_REAR_BRACE))

# ---- report gaps that follow from the numbers ---------------------------------
print(f"carriage front at brace: {CAR_REAR_BRACE + CAR_LEN:.1f}, bumper face {BUMP_X0:.1f}, "
      f"gap {BUMP_X0 - (CAR_REAR_BRACE + CAR_LEN):.1f} mm")
print(f"carriage rear at full draw {CAR_REAR_DRAW:.1f}, stop face {STOP_FACE:.1f}")
print(f"rail {RAIL_X0:.1f}..{RAIL_X1:.1f} = {RAIL_X1 - RAIL_X0:.1f} mm long")
# sim.py measures S from the end of the taper; the hole sits 2.5 mm beyond it.
print(f"limb print length {TIP + 6 - TAB0:.1f} mm; string between tip hole centres "
      f"{2 * (S_HALF + STRING_U - TIP):.0f} mm")
