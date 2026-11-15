"""10 g printed PLA glider for the bow launcher. Dimensions in mm, masses in g.

Glider frame: x forward, z up, y across. The origin is the closed end of the
launch-hook slot, where the carriage pin's tip pushes. Fuselage bottom is
z = 3, which rests in the groove of the carriage mast cap.

Layout: profile fuselage (1.6 mm, fin 0.8 mm) with a nose pod whose rear has
the hook slot; flat 0.4 mm wing on a saddle that sets 2 deg incidence and
6 deg dihedral a side; flat 0.4 mm stab through a slot in the tail boom.

Running it writes the parts, renders them, computes mass and CG from the
meshes (solid PLA), and places the wing so the CG is 35% of the chord behind
its leading edge. Writes glider.json and glider.assemble.
"""
import json, math, re, struct, subprocess, sys
from pathlib import Path

root = Path(__file__).resolve().parent
COSCAD = (root.parents[1] / "dist-newstyle/build/aarch64-osx/ghc-9.8.2/"
          "coscad-1.6.0.0/x/coscad/build/coscad/coscad")
RHO = 1.24e-3

def f(v): return f"{v:.4f}".rstrip("0").rstrip(".")
def box(x0, x1, y0, y1, z0, z1):
    return (f"(box {f(x1-x0)} {f(y1-y0)} {f(z1-z0)} |> move "
            f"{f((x0+x1)/2)} {f((y0+y1)/2)} {f((z0+z1)/2)})")
def poly(pts, h): return f"(⮕ {f(h)} (poly " + " ".join(f"{f(a)} {f(b)}" for a, b in pts) + "))"
def U(*p): return "(" + " ⊕ ".join(p) + ")"
def D(a, *p): return "(" + a + "".join(" ⊖ " + q for q in p) + ")"
def ycyl(r, y0, y1, x, z): return f"(ycyl {f(r)} {f(y1-y0)} |> move {f(x)} {f((y0+y1)/2)} {f(z)})"
def xz(pts, y0, y1):
    """Polygon in the x-z plane, thickness y0..y1: poly in XY (X=x, Y=z), extruded
    along Z, then rotated -90 about X ((X,Y,Z) -> (X, Z, -Y)... ) to stand up."""
    # θ 90 maps (X,Y,Z) -> (X,-Z,Y): poly Y becomes z, extrusion becomes -y.
    return f"((θ 90 {poly(pts, y1 - y0)}) |> move 0 {f(y1)} 0)"
def yz_rib(pts, x0, x1):
    """Polygon in the y-z plane, extruded along x from x0 to x1."""
    return f"((ω 90 (θ 90 {poly(pts, x1 - x0)})) |> move {f(x0)} 0 0)"

# ---- fixed geometry -------------------------------------------------------
T_POD, T_FUS, T_FIN = 1.6, 1.2, 0.4   # nose pod, boom, fin
Z_BOT, Z_TOP = 3.0, 10.0          # fuselage bottom (sits in the mast cap groove) and top edge
POD_BOT, POD_REAR = -3.65, -7.8   # nose pod; its rear edge carries the hook slot
SLOT_HALF = 1.65                  # 3.3 mm slot for the 3.0 mm pin
NOSE = 62.0
TAIL = -118.0
STAB_X0, STAB_X1 = -116.0, -94.0
STAB_Z0, STAB_Z1 = 7.0, 7.6       # 0.6 mm slot for the 0.4 mm stab
STAB_T = 0.4
WING_T, SPAR_H = 0.4, 0.6
CHORD, SPAN = 36.0, 230.0
INCIDENCE, DIHEDRAL = 2.0, 6.0
Z_SADDLE_MID = 12.3               # wing underside at mid-chord on the centreline
BALLAST_X = (44.0, 52.0)

def top_z(x):
    """Fuselage top line: flat at 10 from the nose to x = -30, then down to 9.5 at the tail."""
    return Z_TOP if x >= -30 else Z_TOP - (-30 - x) * 1.0 / (-30 - TAIL)

def fuselage():
    body = [(POD_REAR, POD_BOT), (NOSE - 12, POD_BOT), (NOSE - 4, -1.5), (NOSE, 3), (NOSE - 1.5, 7.5),
            (NOSE - 7, Z_TOP), (-30, Z_TOP), (TAIL, top_z(TAIL)), (TAIL, Z_BOT), (POD_REAR, Z_BOT)]
    fin = [(TAIL, top_z(TAIL) - 1.5), (TAIL, 34.0), (TAIL + 7, 36.0), (TAIL + 30, top_z(TAIL + 30) - 1.5)]
    pod = [(POD_REAR, POD_BOT), (NOSE - 12, POD_BOT), (NOSE - 4, -1.5), (NOSE, 3), (NOSE - 1.5, 7.5),
           (NOSE - 7, Z_TOP), (POD_REAR, Z_TOP)]
    shape = U(xz(body, -T_FUS/2, T_FUS/2), xz(pod, -T_FUS/2, -T_FUS/2 + T_POD),
              xz(fin, -T_FUS/2, -T_FUS/2 + T_FIN))
    cuts = [box(POD_REAR - 2, 0, -2, 2, -SLOT_HALF, SLOT_HALF),                       # hook slot
            # entry chamfer so the slot finds the pin
            f"((θ 90 {poly([(POD_REAR - 0.01, -SLOT_HALF - 0.8), (POD_REAR + 1.2, -SLOT_HALF), (POD_REAR + 1.2, SLOT_HALF), (POD_REAR - 0.01, SLOT_HALF + 0.8)], 4)}) |> move 0 2 0)",
            box(STAB_X0, STAB_X1, -2, 2, STAB_Z0, STAB_Z1)]                           # stab slot
    cuts += [ycyl(1.6, -2, 2, x, 1.0) for x in BALLAST_X]                              # M3 trim weights
    return D(shape, *cuts)

def wing():
    """Flat, mid-chord at x = 0, underside z = 0. Spar ridge on top at 30% chord,
    stopped short of the centre so the plate bends into the saddle's dihedral."""
    c2 = CHORD / 2; s2 = SPAN / 2
    pts = [(c2, -(s2 - 25)), (c2, s2 - 25), (c2 - 10, s2), (-c2, s2), (-c2, -s2), (c2 - 10, -s2)]
    xs = c2 - 0.30 * CHORD
    spar = [box(xs - 0.6, xs + 0.6, 10, s2 - 12, WING_T - 0.01, WING_T + SPAR_H),
            box(xs - 0.6, xs + 0.6, -(s2 - 12), -10, WING_T - 0.01, WING_T + SPAR_H)]
    return U(poly(pts, WING_T), *spar)

def stab():
    pts = [(11, -28), (11, 28), (4, 35), (-11, 35), (-11, -35), (4, -35)]
    return poly(pts, STAB_T)

def saddle(x_le):
    """Three ribs with a V top (dihedral) rising toward the front (incidence),
    joined by keel walls that straddle the fuselage top edge."""
    x_mid = x_le - CHORD / 2
    def zc(x): return Z_SADDLE_MID + (x - x_mid) * math.tan(math.radians(INCIDENCE))
    w, k = 9.0, math.tan(math.radians(DIHEDRAL))
    ribs = []
    for x0, x1 in [(x_le - 1.2, x_le), (x_mid - 0.6, x_mid + 0.6), (x_le - CHORD, x_le - CHORD + 1.2)]:
        z = zc((x0 + x1) / 2)
        ribs.append(yz_rib([(-w, 6.0), (w, 6.0), (w, z - w * k), (0, z), (-w, z - w * k)], x0, x1))
    g = T_FUS / 2 + 0.025
    keel = [box(x_le - CHORD, x_le, g, g + 0.8, 6.0, 10.6),
            box(x_le - CHORD, x_le, -g - 0.8, -g, 6.0, 10.6),
            box(x_le - CHORD, x_le, -g - 0.8, g + 0.8, 10.0, 10.6)]
    g = T_FUS / 2 + 0.025
    return D(U(*ribs, *keel), box(x_le - CHORD - 1, x_le + 1, -g, g, 5.0, 10.0))

def write(name, body, note=""):
    (root / f"{name}.coscad").write_text(note + f"main = {body}\n")

def render(name):
    out = subprocess.run([str(COSCAD), "stl", f"{name}.coscad"], cwd=root, capture_output=True, text=True)
    if out.returncode:
        sys.exit(out.stderr)
    return mass_props(root / f"{name}.stl")

def mass_props(path):
    """Volume and centroid of a closed ASCII or binary STL."""
    d = path.read_bytes()
    if d[:5] == b"solid" and b"facet" in d[:400]:
        v = [tuple(map(float, m)) for m in re.findall(rb"vertex\s+(\S+)\s+(\S+)\s+(\S+)", d)]
        tris = [v[i:i+3] for i in range(0, len(v), 3)]
    else:
        n = struct.unpack("<I", d[80:84])[0]
        tris = []
        for i in range(n):
            q = struct.unpack("<12f", d[84 + 50*i: 84 + 50*i + 48])
            tris.append((q[3:6], q[6:9], q[9:12]))
    V = 0.0; C = [0.0, 0.0, 0.0]
    for a, b, c in tris:
        vol = (a[0]*(b[1]*c[2]-b[2]*c[1]) - a[1]*(b[0]*c[2]-b[2]*c[0]) + a[2]*(b[0]*c[1]-b[1]*c[0])) / 6
        V += vol
        for k in range(3):
            C[k] += vol * (a[k] + b[k] + c[k]) / 4
    return dict(volume=V, mass=V * RHO, centroid=[c / V for c in C])

def main():
    write("glider-fuselage", fuselage(), "// Glider frame. Print on its side, fin side down (fwd).\n")
    write("glider-wing", wing(), "// Flat; bend into the saddle's 6 deg dihedral and glue.\n")
    write("glider-stab", stab(), "// Slides through the tail boom slot; glue centred.\n")
    props = {n: render(n) for n in ["glider-fuselage", "glider-wing", "glider-stab"]}
    m_ballast = float(sys.argv[1]) if len(sys.argv) > 1 else 0.0   # g of M3 screw+nuts at the nose
    m_glue = 0.3
    SM = 0.10
    S_w = CHORD * SPAN - 2 * 0.5 * 10 * 25
    S_h = 22 * 70 - 2 * 0.5 * 7 * 7
    def cg_frac(x_le):
        l_h = (x_le - 0.25 * CHORD) - (STAB_X1 - 0.25 * 22)
        Vh = S_h * l_h / (S_w * CHORD)
        return 0.25 + 0.48 * Vh - SM          # neutral point minus the margin (rough)
    x_le = 0.0
    for _ in range(6):
        write("glider-saddle", saddle(x_le), "// Glider frame. Print keel down. Glue on the fuselage top edge.\n")
        props["glider-saddle"] = render("glider-saddle")
        fixed = (props["glider-fuselage"]["mass"] * props["glider-fuselage"]["centroid"][0]
                 + props["glider-stab"]["mass"] * (STAB_X0 + STAB_X1) / 2
                 + m_ballast * sum(BALLAST_X) / 2)
        m_move = props["glider-wing"]["mass"] + props["glider-saddle"]["mass"] + m_glue
        M = props["glider-fuselage"]["mass"] + props["glider-stab"]["mass"] + m_ballast + m_move
        # solve x_le with the CG fraction evaluated at the previous x_le
        k = cg_frac(x_le)
        x_le = (fixed - m_move * CHORD / 2 + k * CHORD * M) / (M - m_move)
    cgf = cg_frac(x_le)
    x_cg = x_le - cgf * CHORD
    l_h = (x_le - 0.25 * CHORD) - (STAB_X1 - 0.25 * 22)
    Vh = S_h * l_h / (S_w * CHORD)
    W = M / 1000 * 9.81
    v_glide = math.sqrt(2 * W / (1.225 * S_w * 1e-6 * 0.6))
    res = dict(parts={k: round(v["mass"], 2) for k, v in props.items()},
               ballast_g=m_ballast, glue_g=m_glue, total_g=round(M, 2),
               wing_le_x=round(x_le, 1), cg_x=round(x_cg, 1),
               wing_area_cm2=round(S_w / 100, 1), wing_loading_g_dm2=round(M / (S_w / 1e4), 1),
               tail_volume=round(Vh, 2), cg_chord_fraction=round(cgf, 3), static_margin=SM,
               glide_speed_CL06=round(v_glide, 1))
    (root / "glider.json").write_text(json.dumps(res, indent=1) + "\n")
    print(json.dumps(res, indent=1))
    xw = x_le - CHORD / 2
    (root / "glider.assemble").write_text(f"""// 10 g glider in its own frame (origin = hook slot end). Wing shown flat at 2 deg incidence;
// in use it is bent into the saddle's 6 deg dihedral.
plate 256 256 6
fuselage ← glider-fuselage.coscad ×1 ▽fwd material=pla
wing ← glider-wing.coscad ×1 ▽bot material=pla
stab ← glider-stab.coscad ×1 ▽bot material=pla
saddle ← glider-saddle.coscad ×1 ▽bot material=pla

fixed fuselage 0 0 0 0 0 0
fixed saddle 0 0 0 0 0 0
fixed wing {f(xw)} 0 {f(Z_SADDLE_MID)} 0 {f(-INCIDENCE)} 0
fixed stab {f((STAB_X0 + STAB_X1)/2)} 0 {f(STAB_Z0 + (STAB_Z1 - STAB_Z0 - STAB_T)/2)} 0 0 0
""")
    return res

if __name__ == "__main__":
    main()
