include <BOSL2/std.scad>

union() {
  union() {
    union() {
      union() {
        translate([0, 21, 1.5]) {
          cuboid([242, 23, 3]);
        }
        translate([0, 0, 1.5]) {
          zcyl(r = 21, l = 3);
        }
      }
      translate([0, 0, 9.5]) {
        zcyl(r = 3, l = 13);
      }
    }
    translate([-10, 24, 7.2]) {
      zcyl(r = 2, l = 8.4);
    }
  }
  translate([10, 24, 7.2]) {
    zcyl(r = 2, l = 8.4);
  }
}
$fn = 50;