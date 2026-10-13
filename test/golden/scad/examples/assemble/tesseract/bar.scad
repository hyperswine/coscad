include <BOSL2/std.scad>

union() {
  union() {
    cuboid([76, 7, 7], chamfer = 0.4);
    union() {
      translate([38.85, 0, 0]) {
        xcyl(r = 2.1, l = 1.95);
      }
      translate([40.3, 0, 0]) {
        xcyl(r = 2.7, l = 1);
      }
    }
  }
  rotate([0, 0, 180]) {
    union() {
      translate([38.85, 0, 0]) {
        xcyl(r = 2.1, l = 1.95);
      }
      translate([40.3, 0, 0]) {
        xcyl(r = 2.7, l = 1);
      }
    }
  }
}
$fn = 50;