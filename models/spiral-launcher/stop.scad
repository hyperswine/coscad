include <BOSL2/std.scad>

difference() {
  difference() {
    union() {
      union() {
        translate([0, 0, 5]) {
          cuboid([20, 18, 10]);
        }
        translate([-8, 0, -0.775]) {
          cuboid([4, 5.6, 1.65]);
        }
      }
      translate([8, 0, -0.775]) {
        cuboid([4, 5.6, 1.65]);
      }
    }
    translate([0, 0, 5]) {
      zcyl(r = 2.75, l = 12);
    }
  }
  translate([0, 0, 8]) {
    zcyl(r = 5.2, l = 6);
  }
}
$fn = 50;