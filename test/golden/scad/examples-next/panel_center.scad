include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        cuboid([200.0, 200.0, 2.5], rounding = 1.0);
        translate([-80.0, 0, 0]) {
          zcyl(r = 1.7, l = 8.0);
        }
      }
      translate([80.0, 0, 0]) {
        zcyl(r = 1.7, l = 8.0);
      }
    }
    translate([0, -80.0, 0]) {
      zcyl(r = 1.7, l = 8.0);
    }
  }
  translate([0, 80.0, 0]) {
    zcyl(r = 1.7, l = 8.0);
  }
}
$fn = 50;