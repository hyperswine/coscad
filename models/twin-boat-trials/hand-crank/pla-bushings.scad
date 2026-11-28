include <BOSL2/std.scad>

union() {
  difference() {
    union() {
      cylinder(h = 0.3, r1 = 2.7, r2 = 3);
      translate([0, 0, 3.15]) {
        zcyl(r = 3, l = 5.7);
      }
    }
    translate([0, 0, 3]) {
      zcyl(r = 2.15, l = 8);
    }
  }
  translate([12, 0, 0]) {
    difference() {
      union() {
        cylinder(h = 0.3, r1 = 2.7, r2 = 3);
        translate([0, 0, 3.15]) {
          zcyl(r = 3, l = 5.7);
        }
      }
      translate([0, 0, 3]) {
        zcyl(r = 2.15, l = 8);
      }
    }
  }
}
$fn = 50;