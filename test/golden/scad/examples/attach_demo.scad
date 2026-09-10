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
                  cuboid([40, 40, 10], chamfer = 1);
                  translate([25, 0, 0]) {
                    rotate(a = 90, v = [0, 1, 0]) {
                      cyl(r = 3, h = 10, chamfer = 0.5);
                    }
                  }
                }
                translate([-25, 0, 0]) {
                  rotate(a = 90, v = [0, -1, 0]) {
                    cyl(r = 3, h = 10, chamfer = 0.5);
                  }
                }
              }
              translate([0, -25, 0]) {
                rotate(a = 90, v = [1, 0, 0]) {
                  cyl(r = 3, h = 10, chamfer = 0.5);
                }
              }
            }
            translate([0, 25, 0]) {
              rotate(a = 90, v = [-1, 0, 0]) {
                cyl(r = 3, h = 10, chamfer = 0.5);
              }
            }
          }
          translate([0, 0, 18]) {
            cyl(r = 5, h = 26, rounding = 2);
          }
        }
        translate([0, 0, 34]) {
          cuboid([16, 16, 6], rounding = 2);
        }
      }
      translate([0, 0, 42]) {
        sphere(5);
      }
    }
    translate([70, 0, 0]) {
      union() {
        union() {
          cuboid([20, 20, 20], chamfer = 2);
          translate([12.828427125, 0, 12.828427125]) {
            rotate(a = 45, v = [0, 0.707106781, 0]) {
              cyl(r = 3, h = 8, chamfer = 0.5);
            }
          }
        }
        translate([-12.828427125, 0, 20.606601718]) {
          rotate(a = 45, v = [0, -0.707106781, 0]) {
            cyl(r = 3, h = 8, chamfer = 0.5);
          }
        }
      }
    }
  }
  translate([-50, 0, 0]) {
    translate([0, 0, 8]) {
      sphere(8);
    }
  }
}
$fn = 50;