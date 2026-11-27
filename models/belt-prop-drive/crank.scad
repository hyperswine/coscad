include <BOSL2/std.scad>

difference() {
  difference() {
    union() {
      union() {
        translate([-85, -78, 13.5]) {
          intersection() {
            zcyl(r = 2.65, l = 10);
            cuboid([3.6, 7, 40]);
          }
        }
        translate([-85, -78, 21.45]) {
          zcyl(r = 6, l = 6.2);
        }
      }
      hull() {
        translate([-85, -78, 27.5]) {
          zcyl(r = 7, l = 6);
        }
        translate([-53, -78, 27.5]) {
          zcyl(r = 7, l = 6);
        }
      }
    }
    translate([-53, -78, 30.5]) {
      zcyl(r = 2.8, l = 20);
    }
  }
  translate([-53, -78, 24.49]) {
    cylinder(h = 4.2, r = 4.792, $fn = 6);
  }
}
$fn = 50;