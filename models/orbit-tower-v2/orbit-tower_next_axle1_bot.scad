include <BOSL2/std.scad>

difference() {
  union() {
    union() {
      cylinder(h = 7, r = 9);
      translate([0, 0, 6.8]) {
        cylinder(h = 8.2, r = 4.8);
      }
    }
    translate([0, 0, 17.4]) {
      cuboid([6, 6, 5.2], chamfer = 0.3);
    }
  }
  translate([-3.1, -3.1, -0.2]) {
    cube([6.2, 6.2, 5.8]);
  }
}
$fn = 50;