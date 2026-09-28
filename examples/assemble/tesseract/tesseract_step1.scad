include <BOSL2/std.scad>

$vpr = [55, 0, 35];
$vpt = [0.5, 0.5, 0.5];
$vpd = 54.5033321;

multmatrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]) {
}
// the bench, under the rest face (scene coordinates: rest face is -Z)
%translate([-20, -20, -2]) cube([41, 41, 2]);
$fn = 24;
