include <BOSL2/std.scad>

$vpr = [55, 0, 35];
$vpt = [-0.5, 0, 13];
$vpd = 275.481617876;

multmatrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]) {
  color("LightGray") multmatrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]) { union() { union() { cylinder(h = 4, r = 29); translate([0, 0, 4]) { intersection() { cylinder(h = 9.666666667, r1 = 29, r2 = 0); cylinder(h = 2, r = 29); } } } translate([0, 0, 8.4]) { cuboid([6, 6, 5.2], chamfer = 0.3); } } }
  color("LightGray") multmatrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 6], [0, 0, 0, 1]]) { difference() { union() { union() { cylinder(h = 7, r = 9); translate([0, 0, 6.8]) { cylinder(h = 8.2, r = 4.8); } } translate([0, 0, 17.4]) { cuboid([6, 6, 5.2], chamfer = 0.3); } } translate([-3.3, -3.3, -0.2]) { cube([6.6, 6.6, 5.8]); } } }
  color("Orange") multmatrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 13], [0, 0, 0, 1]]) { difference() { difference() { union() { union() { union() { cylinder(h = 4.5, r = 10.5); hull() { translate([-24, 0, 0]) { cylinder(h = 4.5, r = 4); } translate([24, 0, 0]) { cylinder(h = 4.5, r = 4); } } } translate([-24, 0, 0]) { cylinder(h = 4.5, r = 6); } } translate([24, 0, 0]) { cylinder(h = 4.5, r = 5); } } translate([0, 0, -0.2]) { cylinder(h = 4.9, r = 5.2); } } translate([24, 0, 0]) { translate([0, 0, -0.2]) { cylinder(h = 4.9, r = 2.75); } } } }
}
// the bench, under the rest face (scene coordinates: rest face is -Z)
%translate([-50, -49, -2]) cube([99, 98, 2]);
$fn = 24;
