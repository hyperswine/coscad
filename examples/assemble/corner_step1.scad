include <BOSL2/std.scad>

$vpr = [55, 0, 35];
$vpt = [90, -50, 0];
$vpd = 965.368778144;

multmatrix([[1, 0, 0, 0], [0, 0, -1, 0], [0, 1, 0, 0], [0, 0, 0, 1]]) {
  color("Orange") multmatrix([[1, 0, 0, 100], [0, 1, 0, 0], [0, 0, 1, -10], [0, 0, 0, 1]]) { cuboid([200, 20, 20]); }
  color("Orange") multmatrix([[1, 0, 0, -10], [0, 1, 0, 0], [0, 0, 1, 60], [0, 0, 0, 1]]) { cuboid([20, 20, 120]); }
}
// the bench, under the rest face (scene coordinates: rest face is -Z)
%translate([-40, -140, -12]) cube([260, 180, 2]);
$fn = 24;
