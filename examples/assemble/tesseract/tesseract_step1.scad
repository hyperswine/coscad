include <BOSL2/std.scad>

$vpr = [55, 0, 35];
$vpt = [36.239780757, 36.239780757, 32.5];
$vpd = 414.440627086;

multmatrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]) {
  color("Orange") multmatrix([[1, 0, 0, 50], [0, 1, 0, 50], [0, 0, 1, 0], [0, 0, 0, 1]]) { union() { translate([0, 0, 1]) { cuboid([26, 26, 2]); } union() { translate([-9, -9, 18.5]) { cuboid([3, 3, 33]); } translate([18, 0, 0]) { translate([-9, -9, 18.5]) { cuboid([3, 3, 33]); } } translate([0, 18, 0]) { translate([-9, -9, 18.5]) { cuboid([3, 3, 33]); } } translate([18, 18, 0]) { translate([-9, -9, 18.5]) { cuboid([3, 3, 33]); } } } } }
  color("Orange") multmatrix([[1, 0, 0, 50], [0, 1, 0, 50], [0, 0, 1, 50], [0, 0, 0, 1]]) { difference() { difference() { difference() { difference() { difference() { difference() { difference() { difference() { difference() { difference() { difference() { difference() { cuboid([30, 30, 30]); translate([15, 15, 0]) { zcyl(r = 3.5, l = 32); } } translate([-15, 15, 0]) { zcyl(r = 3.5, l = 32); } } translate([-15, -15, 0]) { zcyl(r = 3.5, l = 32); } } translate([15, -15, 0]) { zcyl(r = 3.5, l = 32); } } translate([15, 15, 15]) { rotate([0, 0, 45]) { rotate([0, 54.7356, 0]) { zcyl(r = 2.65, l = 26); } } } } translate([-15, 15, 15]) { rotate([0, 0, 135]) { rotate([0, 54.7356, 0]) { zcyl(r = 2.65, l = 26); } } } } translate([-15, -15, 15]) { rotate([0, 0, 225]) { rotate([0, 54.7356, 0]) { zcyl(r = 2.65, l = 26); } } } } translate([15, -15, 15]) { rotate([0, 0, 315]) { rotate([0, 54.7356, 0]) { zcyl(r = 2.65, l = 26); } } } } translate([15, 15, -15]) { rotate([0, 0, 225]) { rotate([0, 54.7356, 0]) { zcyl(r = 2.65, l = 26); } } } } translate([-15, 15, -15]) { rotate([0, 0, 315]) { rotate([0, 54.7356, 0]) { zcyl(r = 2.65, l = 26); } } } } translate([-15, -15, -15]) { rotate([0, 0, 45]) { rotate([0, 54.7356, 0]) { zcyl(r = 2.65, l = 26); } } } } translate([15, -15, -15]) { rotate([0, 0, 135]) { rotate([0, 54.7356, 0]) { zcyl(r = 2.65, l = 26); } } } } }
  color("Orange") multmatrix([[0.577350269, -0.707106781, -0.40824829, 25.953738644], [0.577350269, 0.707106781, -0.40824829, 25.953738644], [0.577350269, 0, 0.816496581, 25.953738644], [0, 0, 0, 1]]) { difference() { xcyl(r = 2.5, l = 54.337168574); translate([0, 0, -5.5]) { cuboid([56.337168574, 8, 8]); } } }
}
// the bench, under the rest face (scene coordinates: rest face is -Z)
color("Gainsboro") translate([-12.520438487, -12.520438487, -2.5]) cube([97.520438487, 97.520438487, 2]);
$fn = 24;
