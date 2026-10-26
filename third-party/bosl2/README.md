# BOSL2 geometry conventions

Native rounded solid sampling adapts the geometry conventions in BOSL2's
`shapes3d.scad` (`_make_octa_sphere`, `_cyl_path`, `cuboid` and `torus`), with
CoScad-owned Haskell tessellation and Manifold boolean evaluation. The existing
OpenSCAD emitter already targets these same primitives. Attribution and license
are retained here for the adapted algorithms.

Source: https://github.com/BelfrySCAD/BOSL2 (BSD-2-Clause; see LICENSE).
The native/oracle tests are the compatibility gate, not a claim that arbitrary
BOSL2 modules or options are implemented. The reference checkout revision for
this adaptation is recorded below.
cee9a1725caab8586c76eb88d409d16c1e795833
