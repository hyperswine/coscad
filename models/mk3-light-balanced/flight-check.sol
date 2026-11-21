F = use "../lib/glider-flight".
R = use "../lib/root".
> s = 1.035; print (F.report (R.modelDir "models/mk3-light-balanced" "flight-check.sol") s (39.5*s) (101*s)).
