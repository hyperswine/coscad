include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      union() {
        hull() {
          translate([-63.7868, -56.7868, 16]) {
            zcyl(r = 6, l = 4);
          }
          translate([-106.2132, -99.2132, 16]) {
            zcyl(r = 6, l = 4);
          }
        }
        translate([-85, -78, 16]) {
          zcyl(r = 7, l = 4);
        }
      }
      translate([-85, -78, 0]) {
        zcyl(r = 4.3, l = 40);
      }
    }
    translate([-63.7868, -56.7868, 0]) {
      zcyl(r = 2.8, l = 40);
    }
  }
  translate([-106.2132, -99.2132, 0]) {
    zcyl(r = 2.8, l = 40);
  }
}
$fn = 50;