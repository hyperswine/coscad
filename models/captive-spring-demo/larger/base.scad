include <BOSL2/std.scad>

scale([1.5, 1.5, 1]) {
  translate([0, 0, 2]) {
    union() {
      union() {
        union() {
          union() {
            union() {
              union() {
                union() {
                  translate([0, 2, -1]) {
                    cuboid([38, 33, 2]);
                  }
                  translate([0, 14, 1]) {
                    cuboid([38, 7, 2]);
                  }
                }
                translate([0, 0, 2.2]) {
                  zcyl(r = 1.55, l = 4.4);
                }
              }
              translate([-11, 0, 0.7]) {
                zcyl(r = 1.05, l = 1.4);
              }
            }
            translate([-8, 13, 2.8]) {
              zcyl(r = 1.25, l = 5.6);
            }
          }
          translate([8, 13, 2.8]) {
            zcyl(r = 1.25, l = 5.6);
          }
        }
        translate([0, -12, 2.175]) {
          cuboid([6, 5, 4.35]);
        }
      }
      translate([0, -12, 5.125]) {
        zcyl(r = 1.65, l = 1.55);
      }
    }
  }
}
$fn = 50;