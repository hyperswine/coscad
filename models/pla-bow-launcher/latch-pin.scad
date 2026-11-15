include <BOSL2/std.scad>

union() {
  translate([0, 0, -8.5]) {
    zcyl(r = 2, l = 16);
  }
  translate([0, 0, 2.5]) {
    cuboid([3, 16, 6]);
  }
}
$fn = 50;