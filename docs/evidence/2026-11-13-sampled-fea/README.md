# Sampled assembly solver checks

Run from the repository root with a built CoScad executable and the Python
environment in `experiments/requirements-fea.txt`.

```sh
bin=$(cabal list-bin exe:coscad)
python scripts/test-sampled-assembly.py "$bin" --reference-output curved-coupon.json
python scripts/compare-bonded-reference.py --assembly-exe "$bin" --output shelf.json
```

The coupon has a curved extension, partially filled cells, two bonded strips,
two supports and a pressure load. scikit-fem receives the occupied cells, fill
fractions and selected faces from the CLI report. It assembles its own quadratic
elements, traction, constraints and reactions and solves by sparse factorization.
This checks mechanics on the sampled discretization. The narrow-hole regression
separately checks geometric rejection. All seven comparisons passed, including
all reference nodes and the support and interface forces and moments.

Local native suite: 1,727 passed with GHCRTS=-N2 and the built CLI on PATH.
The OpenSCAD oracle was disabled. The first harness invocation picked up an
older installed CLI and stopped in the mass tier. The full rerun used the
binary path returned by cabal list-bin. The later build introducing the
separate refinement budget also passed -Werror and the seven comparisons.
