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

## Glider refinement runtime

The bonded 4 mm case took 391.15 s wall time, including 0.17 s preparation
and 389.55 s in the solve. GHC reported 929 MiB total runtime memory in use.
The mesh had 7,544 occupied cells and 122,583 reduced DOF. The archived 4 mm
solution took 3,622 PCG iterations.

At 2 mm the same geometry occupied 54,251 cells. The experimental bonded
and compliant runs were stopped after 44 min 49 s and 41 min 44 s respectively.
Each was using about 90% of one CPU at interruption. The compliant process
peaked at 2,496,987,136 bytes resident. Neither run published a solution.
The termination times are lower bounds on runtime. The native sample showed
compiled Haskell loops without source-level names. Iteration counts and
residual histories were unavailable, so convergence rate and time per
iteration could not be distinguished. The 4-to-2 mm change in pad travel,
root rotation and joint forces remains unknown.

Before repeating the long runs, add periodic iteration, true/estimated
residual and elapsed-time reporting in the experimental solver, then time the
matrix multiply and SSOR preconditioner separately on the completed 4 mm case.
