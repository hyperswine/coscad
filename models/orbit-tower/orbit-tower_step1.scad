include <BOSL2/std.scad>

$vpr = [55, 0, 35];
$vpt = [0, 0, 13];
$vpd = 273.720897549;

multmatrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]) {
  color("Orange") multmatrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]) { union() { union() { cylinder(h = 4, r = 29); translate([0, 0, 4]) { intersection() { cylinder(h = 9.666666667, r1 = 29, r2 = 0); cylinder(h = 2, r = 29); } } } translate([0, 0, 8.4]) { cuboid([6, 6, 5.2], chamfer = 0.3); } } }
  color("Orange") multmatrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 6], [0, 0, 0, 1]]) { difference() { union() { union() { cylinder(h = 7, r = 9); translate([0, 0, 6.8]) { cylinder(h = 8.2, r = 4.8); } } translate([0, 0, 17.4]) { cuboid([6, 6, 5.2], chamfer = 0.3); } } translate([-3.3, -3.3, -0.2]) { cube([6.6, 6.6, 5.8]); } } }
}
// the bench, under the rest face (scene coordinates: rest face is -Z)
%translate([-49, -49, -2]) cube([98, 98, 2]);
$fn = 24;
