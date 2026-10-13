include <BOSL2/std.scad>

$vpr = [55, 0, 35];
$vpt = [90, -50, 2.5];
$vpd = 966.87308282;

multmatrix([[1, 0, 0, 0], [0, 0, -1, 0], [0, 1, 0, 0], [0, 0, 0, 1]]) {
  color("Orange") multmatrix([[1, 0, 0, 100], [0, 1, 0, 0], [0, 0, 1, -10], [0, 0, 0, 1]]) { cuboid([200, 20, 20]); }
  color("Orange") multmatrix([[1, 0, 0, -10], [0, 1, 0, 0], [0, 0, 1, 60], [0, 0, 0, 1]]) { cuboid([20, 20, 120]); }
  color("Orange") multmatrix([[0, 1, 0, -10], [0, 0, 1, 10], [1, 0, 0, -10], [0, 0, 0, 1]]) { union() { union() { difference() { union() { union() { translate([17, 0, 0]) { translate([0, 0, 2.5]) { cuboid([34, 16, 5], chamfer = 0.5); } } translate([15.3, 0, 0]) { translate([0, 0, -0.6]) { prismoid(size1 = [2.2, 5.4], size2 = [3, 5.8], h = 1.8, anchor = CENTER); } } } translate([28.7, 0, 0]) { translate([0, 0, -0.6]) { prismoid(size1 = [2.2, 5.4], size2 = [3, 5.8], h = 1.8, anchor = CENTER); } } } translate([22, 0, 0]) { translate([0, 0, 2.5]) { zcyl(r = 2.75, l = 12); } } } rotate([0, 0, 90]) { difference() { union() { union() { translate([17, 0, 0]) { translate([0, 0, 2.5]) { cuboid([34, 16, 5], chamfer = 0.5); } } translate([15.3, 0, 0]) { translate([0, 0, -0.6]) { prismoid(size1 = [2.2, 5.4], size2 = [3, 5.8], h = 1.8, anchor = CENTER); } } } translate([28.7, 0, 0]) { translate([0, 0, -0.6]) { prismoid(size1 = [2.2, 5.4], size2 = [3, 5.8], h = 1.8, anchor = CENTER); } } } translate([22, 0, 0]) { translate([0, 0, 2.5]) { zcyl(r = 2.75, l = 12); } } } } } translate([0, 0, 2.5]) { cyl(r = 9, h = 5, chamfer = 0.5); } } }
  color("Red") translate([12, 15, -10]) rotate(a = 90, v = [-1, 0, 0]) cylinder(h = 3.5, r = 4.25);
  color("Red") translate([-10, 15, 12]) rotate(a = 90, v = [-1, 0, 0]) cylinder(h = 3.5, r = 4.25);
}
// the bench, under the rest face (scene coordinates: rest face is -Z)
color("Gainsboro") translate([-40, -140, -12.5]) cube([260, 180, 2]);
$fn = 24;
