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
                      cuboid([20.0, 20.0, 20.0], chamfer = 3.0);
                      translate([30.0, 0, 0]) {
                        cuboid([20.0, 20.0, 20.0], rounding = 5.0);
                      }
                    }
                    translate([60.0, 0, 0]) {
                      cyl(r = 10.0, h = 25.0, chamfer = 2.0);
                    }
                  }
                  translate([90.0, 0, 0]) {
                    cyl(r = 10.0, h = 25.0, rounding = 4.0);
                  }
                }
                translate([120.0, 0, 0]) {
                  tube(h = 25.0, or = 10.0, ir = 6.0);
                }
              }
              translate([150.0, 0, 0]) {
                prismoid(size1 = [24.0, 24.0], size2 = [10.0, 10.0], h = 18.0, anchor = CENTER);
              }
            }
            translate([185.0, 0, 0]) {
              torus(r_maj = 12.0, r_min = 4.0);
            }
          }
          translate([215.0, 0, 0]) {
            wedge([20.0, 20.0, 15.0], anchor = CENTER);
          }
        }
        translate([245.0, 0, 0]) {
          xcyl(r = 6.0, l = 20.0);
        }
      }
      translate([275.0, 0, 0]) {
        ycyl(r = 6.0, l = 20.0);
      }
    }
    translate([305.0, 0, 0]) {
      zcyl(r = 6.0, l = 20.0);
    }
  }
  translate([335.0, 0, 0]) {
    mirror([0.0, 0.0, 1.0]) {
      cylinder(h = 14.0, r1 = 8.0, r2 = 0);
    }
  }
}
$fn = 50;