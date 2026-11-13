"""Exercise the sampled bridge, pressure and a sub-sample hole through the CLI."""
import argparse
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[1]
source = root / "experiments/assembly-fea"
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("exe", nargs="?", default="coscad")
parser.add_argument("--reference-output", type=Path)
args = parser.parse_args()
exe = args.exe
with tempfile.TemporaryDirectory() as temp:
    work = Path(temp)
    for name in ("arm.coscad", "shelf.assemble"):
        (work / name).write_bytes((source / name).read_bytes())
    # Extend the rear edge away from the loaded face and the bonded strips.
    shape = "(block 12 6 2) ⊕ (◎ 2 2 |> move 6 6 0)"
    shelf = work / "shelf.coscad"
    shelf.write_text(f"main = {shape}\n")
    case = json.loads((source / "shelf-analysis.json").read_text())
    case["schema"] = "coscad.assembly-sampled/1"
    load = case["loads"][0]
    del load["force_n"]
    load["pressure_mpa"] = 1 / 72
    config, report = work / "case.json", work / "report.json"
    config.write_text(json.dumps(case))
    cmd = [exe, "stress", str(work / "shelf.assemble"), "--bonded", str(config),
           "-o", str(report)]
    env = dict(os.environ, GHCRTS="-N2")
    subprocess.run(cmd, env=env, check=True)
    solved = json.loads(report.read_text())
    result = solved["result"]
    assert solved["prepare_seconds"] >= 0 and solved["solve_seconds"] > 0
    for body in result["bodies"]:
        normal_z = max(0, *(c["stress_xx_yy_zz_xy_yz_zx_mpa"][2] for c in body["cells"]))
        measured = solved["max_cell_centre_layer_normal_tension_mpa"][body["name"]]
        assert abs(measured - normal_z) < 1e-10
    assert any(c["solid_fraction"] < 1 for b in result["bodies"] for c in b["cells"])
    assert abs(result["applied"]["force_n"][2] + 1) < 1e-10
    assert max(map(abs, result["equilibrium_residual"]["moment_n_mm"])) < 1e-6
    if args.reference_output:
        module = importlib.util.spec_from_file_location(
            "bonded_reference", root / "scripts/compare-bonded-reference.py")
        reference = importlib.util.module_from_spec(module)
        module.loader.exec_module(reference)
        # Both solvers receive identical voxels and boundary conditions. The
        # reference independently builds its basis, stiffness and traction.
        problem = {k: result[k] for k in ("loads", "supports", "bonds")}
        problem["bodies"] = []
        for body in result["bodies"]:
            spec = {k: body[k] for k in ("name", "origin_mm", "cell_mm", "young_mpa", "poisson")}
            fills = {tuple(c["key"]): c["solid_fraction"] for c in body["cells"]}
            spec["cell_fractions"] = fills
            spec["counts"] = [max(key[d] for key in fills)+1 for d in range(3)]
            problem["bodies"].append(spec)
        comparison = reference.compare(problem, result, reference.reference(problem))
        args.reference_output.write_text(json.dumps(comparison, indent=2) + "\n")
        if not comparison["passed"]:
            raise RuntimeError(comparison)
    # The small hole is between the cell's occupancy sample rays.
    shelf.write_text(f"main = ({shape}) ⊖ (◎ 0.08 5 |> move 0.8 0.8 -1)\n")
    bad = subprocess.run(cmd, env=env, capture_output=True, text=True)
    assert bad.returncode and "bond crosses a hole" in bad.stderr, bad.stderr
    print("Curved assembly, pressure equilibrium and narrow-hole rejection passed.")
