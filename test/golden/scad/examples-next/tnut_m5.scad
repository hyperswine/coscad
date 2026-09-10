include <BOSL2/std.scad>

difference() {
  union() {
    cuboid([2.3, 10, 9]);
    translate([1.6, 0, 0]) {
      cuboid([0.9, 10, 6]);
    }
  }
  translate([0.85, 0, 0]) {
    xcyl(r = 2.1, l = 6);
  }
}
$fn = 50;