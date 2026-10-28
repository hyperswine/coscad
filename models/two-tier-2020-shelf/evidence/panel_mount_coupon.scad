include <BOSL2/std.scad>

difference() {
  difference() {
    cuboid([24, 24, 11]);
    zcyl(r = 2.8, l = 20);
  }
  translate([0, 0, 5.5]) {
    zcyl(r = 5.5, l = 10);
  }
}
$fn = 50;