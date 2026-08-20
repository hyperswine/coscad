include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        cuboid([210.0, 210.0, 2.5], rounding = 1.0);
        translate([-85.0, 0, 0]) {
          zcyl(r = 1.7, l = 8.0);
        }
      }
      translate([75.0, 0, 0]) {
        zcyl(r = 1.7, l = 8.0);
      }
    }
    translate([0, -85.0, 0]) {
      zcyl(r = 1.7, l = 8.0);
    }
  }
  translate([0, 75.0, 0]) {
    zcyl(r = 1.7, l = 8.0);
  }
}
$fn = 50;