"""Grid search over limb designs at a fixed peak stress. Writes search.json."""
import itertools, json, math, sys
from multiprocessing import Pool
from sim import run

SIGMA = float(sys.argv[1]) if len(sys.argv) > 1 else 30.0
GRID = dict(L=[210, 230, 250], t=[2.4, 3.0, 3.6], b0=[28, 36, 44],
            kappa0=[0.0, 0.001, 0.002], slack=[5, 10, 20])

def one(p):
    L, t, b0, k0, slack = p
    S = 20 + L - slack          # string half-length; slack sets the brace
    try:
        return run(L, t, b0, 8.0, S, kappa0=k0, sigma_max=SIGMA)
    except Exception as e:
        return dict(L=L, t=t, b0=b0, kappa0=k0, S=S, error=str(e))

if __name__ == "__main__":
    pts = list(itertools.product(*GRID.values()))
    with Pool(8) as pool:
        res = pool.map(one, pts)
    ok = [r for r in res if "error" not in r and not math.isnan(r["v_glider_90pct"])]
    for r in ok:
        r.pop("sigma_profile_full_draw", None)
    json.dump(res, open(f"search-{int(SIGMA)}.json", "w"), indent=1)
    ok.sort(key=lambda r: -r["v_glider_90pct"])
    print(f"{len(ok)}/{len(res)} solved at {SIGMA} MPa")
    cols = ["L", "t", "b0", "kappa0", "S", "limb_mass_g", "brace_mm", "draw_mm",
            "power_stroke_mm", "stored_J", "brace_stored_J", "peak_draw_force_N",
            "v_glider_90pct", "glider_share_of_energy"]
    print(" ".join(cols))
    for r in ok[:12]:
        print(" ".join(str(r[c]) for c in cols))
    hit = sorted([r for r in ok if r["v_glider_90pct"] >= 8.0], key=lambda r: r["limb_mass_g"])
    print(f"\n{len(hit)} designs reach 8 m/s (90% energy); lightest:")
    for r in hit[:8]:
        print(" ".join(str(r[c]) for c in cols))
