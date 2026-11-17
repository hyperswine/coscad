include <BOSL2/std.scad>

difference() {
  translate([0, 0, 38.8]) {
    zcyl(r = 10.5, l = 3);
  }
  translate([0, 0, 38.8]) {
    zcyl(r = 2.75, l = 5);
  }
}
$fn = 50;