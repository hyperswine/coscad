include <BOSL2/std.scad>

difference() {
  difference() {
    union() {
      union() {
        union() {
          translate([0, 0, 4.95]) {
            intersection() {
              zcyl(r = 2.65, l = 6.7);
              cuboid([3.6, 7, 20]);
            }
          }
          translate([0, 0, 10.05]) {
            zcyl(r = 4, l = 3.9);
          }
        }
        translate([0, 0, 15.5]) {
          zcyl(r = 6, l = 7.4);
        }
      }
      hull() {
        translate([0, 0, 22]) {
          zcyl(r = 7, l = 6);
        }
        translate([32, 0, 22]) {
          zcyl(r = 7, l = 6);
        }
      }
    }
    translate([32, 0, 25]) {
      zcyl(r = 2.8, l = 20);
    }
  }
  translate([32, 0, 18.99]) {
    cylinder(h = 4.2, r = 4.792, $fn = 6);
  }
}
$fn = 50;