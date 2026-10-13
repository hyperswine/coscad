include <BOSL2/std.scad>

$vpr = [55, 0, 35];
$vpt = [0, 0, 0];
$vpd = 206.849992031;

multmatrix([[0, 0, -1, 0], [0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1]]) {
  color("Orange") multmatrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]) { difference() { difference() { difference() { difference() { cyl(r = 14.8, h = 16); translate([0, 0, 4]) { xcyl(r = 2.75, l = 40); } } translate([6, 0, 3.825]) { cuboid([4.4, 8.4, 9.35]); } } translate([0, 0, -4]) { ycyl(r = 2.75, l = 40); } } translate([0, 6, -3.825]) { cuboid([8.4, 4.4, 9.35]); } } }
}
// the bench, under the rest face (scene coordinates: rest face is -Z)
color("Gainsboro") translate([-28, -34.8, -17.3]) cube([56, 69.6, 2]);
$fn = 24;
