include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      union() {
        union() {
          union() {
            translate([100, 39, 31.6]) {
              cuboid([200, 14, 2.2]);
            }
            translate([100, 43.4, 34.1]) {
              cuboid([200, 5.2, 7.2]);
            }
          }
          translate([100, 41.75, 38.45]) {
            cuboid([200, 8.5, 1.7]);
          }
        }
        translate([196.5, 36.45, 35.15]) {
          cuboid([7, 8.9, 5.1]);
        }
      }
      translate([3, 43.4, 34.9]) {
        zcyl(r = 1.7, l = 10.8);
      }
    }
    translate([45, 43.4, 34.9]) {
      zcyl(r = 1.7, l = 10.8);
    }
  }
  translate([190, 43.4, 34.9]) {
    zcyl(r = 1.7, l = 10.8);
  }
}
$fn = 50;