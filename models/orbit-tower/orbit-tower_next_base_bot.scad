include <BOSL2/std.scad>

union() {
  union() {
    cylinder(h = 4, r = 29);
    translate([0, 0, 4]) {
      intersection() {
        cylinder(h = 9.666666667, r1 = 29, r2 = 0);
        cylinder(h = 2, r = 29);
      }
    }
  }
  translate([0, 0, 8.4]) {
    cuboid([6, 6, 5.2], chamfer = 0.3);
  }
}
$fn = 50;