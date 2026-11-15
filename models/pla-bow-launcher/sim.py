"""Quasi-static bow model for the PLA glider launcher. Units: mm, N, g, s.

Each limb is a chain of N rigid segments joined by rotational springs
(k = E I / ds), clamped at the riser. For each nock position the limb shape
minimises bending energy with the string taut. That gives the draw-force
curve, the stored energy and the stress along the limb. Launch speed uses the
standard quasi-static mode: limb node velocities are d(position)/d(draw)
times the carriage speed, so the limb mass is counted where it actually moves.

Not modelled: string stretch, PLA hysteresis, rail friction. A 0.9 factor on
the delivered energy is reported alongside the ideal figure.
"""
import math, sys, json
import numpy as np
from scipy.optimize import minimize

E = 3500.0          # MPa, printed PLA along the lines
RHO = 1.24e-3       # g/mm^3, solid (100% infill)

class Bow:
    def __init__(s, L, t, b0, bt, R0=20.0, brace=None, N=24, m_tip=0.6,
                 m_string=0.3, kappa0=0.0):
        s.L, s.t, s.b0, s.bt, s.R0, s.N = L, t, b0, bt, R0, N
        s.ds = L / N
        sj = np.arange(N) * s.ds                     # joint i at the start of segment i
        s.bj = b0 + (bt - b0) * sj / L               # width at each joint
        s.k = E * (s.bj * t ** 3 / 12) / s.ds        # N*mm/rad
        smid = (np.arange(N) + 0.5) * s.ds
        s.mseg = RHO * t * s.ds * (b0 + (bt - b0) * smid / L)
        s.m_tip, s.m_string = m_tip, m_string
        s.limb_mass = s.mseg.sum() + m_tip
        s.brace = brace
        # printed shape: uniform forward (reflex) curvature kappa0, 1/mm
        s.phi0 = np.full(N, -kappa0 * s.ds)

    # geometry ---------------------------------------------------------
    def nodes(s, phi):
        a = math.pi / 2 + np.cumsum(phi)            # segment directions
        d = np.stack([np.cos(a), np.sin(a)], 1) * s.ds
        pts = np.vstack([[0.0, s.R0], [0.0, s.R0] + np.cumsum(d, 0)])
        return pts                                   # N+1 nodes, last = tip

    def energy(s, phi):
        return 0.5 * np.sum(s.k * (phi - s.phi0) ** 2)  # one limb

    def stress(s, phi):
        return E * s.t * np.abs(phi - s.phi0) / (2 * s.ds)  # MPa at each joint

    # equilibria -------------------------------------------------------
    def solve(s, cons, phi0):
        for start in (phi0, phi0 * 1.01 + 1e-4):
            r = minimize(s.energy, start, jac=lambda p: s.k * (p - s.phi0), method="SLSQP",
                         constraints=[cons], options=dict(maxiter=500, ftol=1e-12))
            if r.success or abs(cons["fun"](r.x)) < 1e-6:
                return r.x
        raise RuntimeError(r.message)

    def braced(s, S):
        """String half-length S, straight at rest: tip y = S."""
        c = dict(type="eq", fun=lambda p: s.nodes(p)[-1, 1] - S)
        return s.solve(c, s.phi0 + 0.01)

    def drawn(s, S, xn, phi0):
        """Nock at (xn, 0), xn negative = drawn back."""
        def g(p):
            tip = s.nodes(p)[-1]
            return math.hypot(tip[0] - xn, tip[1]) - S
        return s.solve(dict(type="eq", fun=g), phi0)

def run(L, t, b0, bt, S, kappa0=0.0, sigma_max=30.0, m_glider=10.0,
        m_carriage=8.0, R0=20.0, verbose=False):
    bow = Bow(L, t, b0, bt, R0, kappa0=kappa0)
    phi_b = bow.braced(S)
    tip_b = bow.nodes(phi_b)[-1]
    x_brace = tip_b[0]
    # draw back until peak stress reaches sigma_max
    xs, phis = [x_brace], [phi_b]
    phi = phi_b.copy()
    dx = -2.0
    x = x_brace
    while True:
        x += dx
        phi = bow.drawn(S, x, phi)
        if bow.stress(phi).max() > sigma_max:
            # bisect back to sigma_max
            lo, hi, plo = x - dx, x, phis[-1]
            for _ in range(30):
                mid = 0.5 * (lo + hi)
                pm = bow.drawn(S, mid, plo)
                if bow.stress(pm).max() > sigma_max:
                    hi = mid
                else:
                    lo, plo = mid, pm
            xs.append(lo); phis.append(plo)
            break
        xs.append(x); phis.append(phi)
        if x < x_brace - 400:
            raise RuntimeError("draw did not reach the stress limit")
    xs = np.array(xs); U = np.array([2 * bow.energy(p) for p in phis])  # both limbs, N*mm
    F = np.gradient(U, xs) * -1                                          # draw force, N
    # quasi-static kinetic coefficient M(x) = sum m |dP/dx|^2 (both limbs)
    P = np.array([bow.nodes(p) for p in phis])                           # (n, N+1, 2)
    dP = np.gradient(P, xs, axis=0)
    mnode = np.zeros(bow.N + 1)
    mnode[:-1] += bow.mseg / 2; mnode[1:] += bow.mseg / 2; mnode[-1] += bow.m_tip
    M_limb = 2 * np.sum(mnode[None, :] * np.sum(dP ** 2, 2), 1)
    # string: about half its mass moves with the nock
    M = M_limb + m_glider + m_carriage + 0.5 * bow.m_string
    U_d = U[-1]
    # speed vs position (energy in N*mm = mJ; mass in g -> v in m/s: v^2 = 2*mJ/g)
    v = np.sqrt(np.maximum(2 * (U_d - U) / M, 0))
    i_sep = int(np.argmax(v))
    v_sep = v[i_sep]
    eff = 0.5 * m_glider * v_sep ** 2 / U_d
    stroke = xs[i_sep] - xs[-1]
    res = dict(L=L, t=t, b0=b0, bt=bt, S=S, R0=R0, kappa0=kappa0,
               limb_mass_g=round(bow.limb_mass, 2),
               brace_mm=round(-x_brace, 1), draw_mm=round(-xs[-1], 1),
               power_stroke_mm=round(stroke, 1),
               stored_J=round(U_d / 1000, 3),
               brace_stored_J=round(U[0] / 1000, 3),
               peak_draw_force_N=round(F[-1], 1),
               max_draw_force_N=round(F.max(), 1),
               v_glider=round(v_sep, 2), v_glider_90pct=round(v_sep * math.sqrt(0.9), 2),
               glider_share_of_energy=round(eff, 3),
               sigma_brace_MPa=round(bow.stress(phi_b).max(), 1),
               sigma_profile_full_draw=[round(x, 1) for x in bow.stress(phis[-1])],
               peak_accel_g=round(float(np.max(F / M)) * 1000 / 9.81, 0))
    if verbose:
        res["curve"] = [(round(-a, 1), round(f, 2), round(u / 1000, 4), round(vv, 2))
                        for a, f, u, vv in zip(xs, F, U, v)]
    return res

if __name__ == "__main__":
    args = [float(a) for a in sys.argv[1:7]]
    print(json.dumps(run(*args, verbose=True), indent=1))
