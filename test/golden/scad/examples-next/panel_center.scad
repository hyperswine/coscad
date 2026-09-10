include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        cuboid([200, 200, 2.5], rounding = 1);
        translate([-80, 0, 0]) {
          zcyl(r = 1.7, l = 8);
        }
      }
      translate([80, 0, 0]) {
        zcyl(r = 1.7, l = 8);
      }
    }
    translate([0, -80, 0]) {
      zcyl(r = 1.7, l = 8);
    }
  }
  translate([0, 80, 0]) {
    zcyl(r = 1.7, l = 8);
  }
}
$fn = 50;