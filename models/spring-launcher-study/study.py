"""Spring launcher study for small gliders. SI units unless a name says mm/g.

Part 1: the ceiling for printed plastic springs.
Part 2: sizing a steel (music wire) extension spring for a pull-back launcher.

All material values are typical published figures, not measurements of our
filament. Printed parts are assumed loaded along the layers (never across).
"""
import math

G_ACC = 9.81

def launch_speed(U, m_glider, m_carriage, m_spring):
    """Energy balance; a spring fixed at one end moves 1/3 of its mass."""
    return math.sqrt(2 * U / (m_glider + m_carriage + m_spring / 3))

def spring_mass_needed(u_per_kg, v, m_glider, m_carriage):
    """Spring mass so that U = u*m_s = 1/2 (m_g + m_c + m_s/3) v^2."""
    denom = u_per_kg - v * v / 6
    if denom <= 0:
        return math.inf  # the spring cannot even accelerate its own mass to v
    return 0.5 * (m_glider + m_carriage) * v * v / denom

# ---------------------------------------------------------------- Part 1
# (name, E GPa, density g/cm3, sigma "repeatable" MPa, sigma "near limit" MPa)
PLASTICS = [
    ("PLA",           3.5, 1.24, 20, 35),
    ("PETG",          2.1, 1.27, 20, 35),
    ("PC",            2.3, 1.20, 30, 50),
    ("Nylon (PA12)",  1.6, 1.02, 25, 45),
]
# Fraction of sigma^2/2E a geometry can use:
#   1    uniform tension, or an ideal thin-flange beam in pure bending
#   1/3  rectangular section at constant stress along its length
#        (pure moment, like a spiral, or a tapered bow limb)
#   1/9  straight rectangular cantilever with an end load
GEOMETRY = [("tension / ideal sandwich", 1.0), ("tapered limb, spiral", 1/3),
            ("straight cantilever", 1/9)]

TARGETS = [  # (label, glider kg, launch speed m/s)
    ("10 g chuck glider @ 8 m/s", 0.010, 8.0),
    ("20 g chuck glider @ 12 m/s", 0.020, 12.0),
    ("160 g bolted glider @ 10 m/s", 0.160, 10.0),
]
M_CARRIAGE = 0.008  # printed carriage on a rail

def part1():
    print("PART 1 - printed plastic springs")
    print("Energy per gram of flexing material (J/g), repeatable .. near limit")
    for name, E, rho, s1, s2 in PLASTICS:
        cells = []
        for gname, eta in GEOMETRY:
            u1 = eta * (s1 * 1e6) ** 2 / (2 * E * 1e9) / (rho * 1e3)  # J/kg
            u2 = eta * (s2 * 1e6) ** 2 / (2 * E * 1e9) / (rho * 1e3)
            cells.append(f"{gname}: {u1/1000:.3f}..{u2/1000:.3f}")
        print(f"  {name:13s} " + " | ".join(cells))
    print("\nSpring mass for each target (tapered limb, eta=1/3, 8 g carriage)")
    for label, mg, v in TARGETS:
        row = []
        for name, E, rho, s1, s2 in PLASTICS:
            out = []
            for s in (s1, s2):
                u = (1/3) * (s * 1e6) ** 2 / (2 * E * 1e9) / (rho * 1e3)
                ms = spring_mass_needed(u, v, mg, M_CARRIAGE)
                out.append("impossible" if ms == math.inf else f"{ms*1000:.0f} g")
            row.append(f"{name} {out[0]}..{out[1]}")
        print(f"  {label:30s} " + "; ".join(row))
    print("  (top speed with no glider, tapered limb, near limit):")
    for name, E, rho, s1, s2 in PLASTICS:
        u = (1/3) * (s2 * 1e6) ** 2 / (2 * E * 1e9) / (rho * 1e3)
        print(f"    {name:13s} {math.sqrt(6*u):.1f} m/s")

# ---------------------------------------------------------------- Part 2
G_STEEL = 79.3e9
RHO_STEEL = 7850
WIRES_MM = [0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0]

def sut_music_wire(d_mm):
    """ASTM A228 tensile strength, Shigley: A/d^m with A=2211 MPa mm^m, m=0.145."""
    return 2211e6 / d_mm ** 0.145

def extension_spring(d_mm, D_mm, n_body, Fi_frac=0.5):
    """Close-wound music wire extension spring with machine hooks (r1=D/2, r2=2d).

    Stress limits (Shigley, static): body torsion 0.45 Sut, hook bending
    0.75 Sut, hook torsion 0.40 Sut. Initial tension is set Fi_frac of the
    way into Shigley's preferred range.
    """
    d, D = d_mm / 1000, D_mm / 1000
    C = D / d
    sut = sut_music_wire(d_mm)
    Kb = (4 * C + 2) / (4 * C - 3)                 # Bergstrasser factor
    k = G_STEEL * d ** 4 / (8 * D ** 3 * n_body)
    # Preferred initial-tension stress range (Shigley eq. 10-55, MPa)
    mid = 231 / math.exp(0.105 * C)
    half = 6.9 * (4 - (C - 3) / 6.5)
    tau_i = (mid - half + Fi_frac * 2 * half) * 1e6
    Fi = tau_i * math.pi * d ** 3 / (8 * Kb * D)
    # Maximum force from each limit
    F_body = 0.45 * sut * math.pi * d ** 3 / (8 * Kb * D)
    C1 = C
    KA = (4 * C1 ** 2 - C1 - 1) / (4 * C1 * (C1 - 1))
    F_hookA = 0.75 * sut / (KA * 16 * D / (math.pi * d ** 3) + 4 / (math.pi * d ** 2))
    C2 = 4.0
    KB = (4 * C2 - 1) / (4 * C2 - 4)
    F_hookB = 0.40 * sut * math.pi * d ** 3 / (8 * KB * D)
    Fmax = min(F_body, F_hookA, F_hookB)
    limit = {F_body: "body", F_hookA: "hook bend", F_hookB: "hook twist"}[Fmax]
    x = (Fmax - Fi) / k
    U = Fi * x + 0.5 * k * x * x
    L_body = (n_body + 1) * d
    L_free = L_body + 2 * (D - d)              # two machine hooks
    wire_len = math.pi * D * (n_body + 2)      # body + roughly one turn per hook
    m = RHO_STEEL * math.pi * d * d / 4 * wire_len
    return dict(d=d_mm, D=D_mm, C=C, n=n_body, k=k, Fi=Fi, Fmax=Fmax,
                limit=limit, x=x, U=U, L_free=L_free, m=m)

def search(U_need, max_stroke, max_len, max_od):
    """Lightest single spring giving U_need within the stroke and size limits."""
    best = []
    for d in WIRES_MM:
        for C10 in range(60, 121, 5):
            C = C10 / 10
            D = C * d
            if D + d > max_od:
                continue
            for n in range(5, 400):
                s = extension_spring(d, D, n)
                if s["L_free"] > max_len:
                    break
                if s["x"] > max_stroke:
                    break
                if s["U"] >= U_need:
                    best.append(s)
                    break
    best.sort(key=lambda s: s["m"])
    return best

def fmt(s):
    return (f"wire {s['d']} mm, OD {s['D']+s['d']:.1f} mm (C={s['C']:.1f}), "
            f"{s['n']} coils, free length {s['L_free']*1000:.0f} mm, "
            f"k={s['k']/1000:.2f} N/mm, Fi={s['Fi']:.1f} N, Fmax={s['Fmax']:.1f} N "
            f"({s['limit']}), stroke {s['x']*1000:.0f} mm, U={s['U']:.2f} J, "
            f"spring {s['m']*1000:.1f} g")

def part2():
    print("\nPART 2 - steel extension spring, pull-back carriage")
    print("Music wire stores up to "
          f"{(0.45*sut_music_wire(1.0))**2/(4*G_STEEL)/RHO_STEEL:.0f} J/kg "
          "in a helical body at 0.45 Sut (1 mm wire, before hooks and initial tension).")
    max_stroke, max_len, max_od = 0.150, 0.160, 16
    print(f"Limits: stroke <= {max_stroke*1000:.0f} mm, free length <= {max_len*1000:.0f} mm, "
          f"OD <= {max_od} mm, carriage {M_CARRIAGE*1000:.0f} g, "
          "assumed 70% of spring energy reaches the glider+carriage+spring")
    eff = 0.70
    for label, mg, v in TARGETS:
        need_ke = 0.5 * (mg + M_CARRIAGE) * v * v
        # include the spring's own moving mass iteratively
        U = need_ke / eff
        for _ in range(10):
            cands = search(U, max_stroke, max_len, max_od)
            if not cands:
                break
            s = cands[0]
            U = 0.5 * (mg + M_CARRIAGE + s["m"] / 3) * v * v / eff
        print(f"\n  {label}: needs {U:.2f} J stored")
        if not cands:
            # how many springs in parallel?
            for npar in range(2, 9):
                c = search(U / npar, max_stroke, max_len, max_od)
                if c:
                    print(f"    no single spring fits; {npar} in parallel: " + fmt(c[0]))
                    break
            else:
                print("    not reachable with up to 8 springs in these limits")
            continue
        for s in cands[:3]:
            print("    " + fmt(s))

    # Speed curves for one practical spring
    s = extension_spring(1.0, 9.0, 60)
    print("\n  Example stock-like spring: " + fmt(s))
    for mg in [0.005, 0.010, 0.020, 0.050, 0.160]:
        v = launch_speed(eff * s["U"], mg, M_CARRIAGE, s["m"])
        h = v * v / (2 * G_ACC)
        print(f"    glider {mg*1000:4.0f} g: {v:5.1f} m/s  (vacuum climb {h:.1f} m)")
    for npar in [2, 4]:
        v = launch_speed(eff * npar * s["U"], 0.160, M_CARRIAGE, npar * s["m"])
        print(f"    {npar} in parallel, 160 g glider: {v:.1f} m/s")

if __name__ == "__main__":
    part1()
    part2()
