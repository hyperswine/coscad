include <BOSL2/std.scad>

$vpr = [55, 0, 35];
$vpt = [0, 0, 0];
$vpd = 230.133283987;

multmatrix([[0, 0, -1, 0], [0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1]]) {
  color("LightGray") multmatrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]) { difference() { difference() { difference() { difference() { cyl(r = 14.8, h = 16); translate([0, 0, 4]) { xcyl(r = 2.75, l = 40); } } translate([6, 0, 3.825]) { cuboid([4.4, 8.4, 9.35]); } } translate([0, 0, -4]) { ycyl(r = 2.75, l = 40); } } translate([0, 6, -3.825]) { cuboid([8.4, 4.4, 9.35]); } } }
  color("LightGray") multmatrix([[0, 1, 0, 0], [1, 0, 0, 0], [0, 0, -1, 0], [0, 0, 0, 1]]) { difference() { difference() { union() { intersection() { difference() { sphere(20); sphere(17.6); } translate([0, 0, 25]) { cuboid([50, 50, 50]); } } intersection() { translate([0, 0, 4]) { tube(h = 8, or = 20, ir = 15); } sphere(20); } } translate([20, 0, 4]) { xcyl(r = 2.75, l = 50); } } translate([22.5, 0, 4]) { xcyl(r = 5, l = 10); } } }
  color("Orange") multmatrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]) { difference() { difference() { union() { intersection() { difference() { sphere(20); sphere(17.6); } translate([0, 0, 25]) { cuboid([50, 50, 50]); } } intersection() { translate([0, 0, 4]) { tube(h = 8, or = 20, ir = 15); } sphere(20); } } translate([20, 0, 4]) { xcyl(r = 2.75, l = 50); } } translate([22.5, 0, 4]) { xcyl(r = 5, l = 10); } } }
  color("DarkRed") translate([0, 20, -4]) rotate(a = 90, v = [-1, 0, 0]) cylinder(h = 3.5, r = 4.25);
  color("Red") translate([20, 0, 4]) rotate(a = 90, v = [0, 1, 0]) cylinder(h = 3.5, r = 4.25);
}
// the bench, under the rest face (scene coordinates: rest face is -Z)
%translate([-40, -40, -22]) cube([80, 80, 2]);
$fn = 24;
