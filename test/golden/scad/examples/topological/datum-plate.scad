include <BOSL2/std.scad>

translate([-6, 0, 2]) {
  difference() {
    union() {
      cuboid([48, 32, 4]);
      translate([30, 0, 0]) {
        cuboid([12, 12, 4]);
      }
    }
    translate([-32, -16, 0]) {
      union() {
        zcyl(r = 2, l = 6);
        translate([16, 0, 0]) {
          zcyl(r = 2, l = 6);
        }
        translate([32, 0, 0]) {
          zcyl(r = 2, l = 6);
        }
        translate([0, 16, 0]) {
          zcyl(r = 2, l = 6);
        }
        translate([16, 16, 0]) {
          zcyl(r = 2, l = 6);
        }
        translate([32, 16, 0]) {
          zcyl(r = 2, l = 6);
        }
      }
    }
  }
}
$fn = 50;