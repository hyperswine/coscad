include <BOSL2/std.scad>

union() {
  union() {
    union() {
      union() {
        union() {
          union() {
            union() {
              union() {
                union() {
                  union() {
                    union() {
                      cuboid([20, 20, 20], chamfer = 3);
                      translate([30, 0, 0]) {
                        cuboid([20, 20, 20], rounding = 5);
                      }
                    }
                    translate([60, 0, 0]) {
                      cyl(r = 10, h = 25, chamfer = 2);
                    }
                  }
                  translate([90, 0, 0]) {
                    cyl(r = 10, h = 25, rounding = 4);
                  }
                }
                translate([120, 0, 0]) {
                  tube(h = 25, or = 10, ir = 6);
                }
              }
              translate([150, 0, 0]) {
                prismoid(size1 = [24, 24], size2 = [10, 10], h = 18, anchor = CENTER);
              }
            }
            translate([185, 0, 0]) {
              torus(r_maj = 12, r_min = 4);
            }
          }
          translate([215, 0, 0]) {
            wedge([20, 20, 15], anchor = CENTER);
          }
        }
        translate([245, 0, 0]) {
          xcyl(r = 6, l = 20);
        }
      }
      translate([275, 0, 0]) {
        ycyl(r = 6, l = 20);
      }
    }
    translate([305, 0, 0]) {
      zcyl(r = 6, l = 20);
    }
  }
  translate([335, 0, 0]) {
    mirror([0, 0, 1]) {
      cylinder(h = 14, r1 = 8, r2 = 0);
    }
  }
}
$fn = 50;