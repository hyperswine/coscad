include <BOSL2/std.scad>

difference() {
  union() {
    cuboid([2.3, 10.0, 9.0]);
    translate([1.5999999999999999, 0.0, 0.0]) {
      cuboid([0.9, 10.0, 6.0]);
    }
  }
  translate([0.85, 0.0, 0.0]) {
    xcyl(r = 2.1, l = 6.0);
  }
}
$fn = 50;