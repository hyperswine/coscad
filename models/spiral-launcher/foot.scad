include <BOSL2/std.scad>

difference() {
  translate([0, 43.4, 13.25]) {
    cuboid([10, 8, 34.5]);
  }
  translate([0, 43.4, 26]) {
    zcyl(r = 1.3, l = 11);
  }
}
$fn = 50;