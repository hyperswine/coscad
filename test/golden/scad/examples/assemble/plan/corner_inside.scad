include <BOSL2/std.scad>

difference() {
  difference() {
    cuboid([20, 20, 20]);
    translate([0, 0, 10]) {
      zcyl(r = 2.75, l = 30);
    }
  }
  translate([-10, 0, 0]) {
    xcyl(r = 2.75, l = 30);
  }
}
$fn = 50;