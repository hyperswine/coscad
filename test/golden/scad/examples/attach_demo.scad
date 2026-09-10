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
                  cuboid([40.0, 40.0, 10.0], chamfer = 1.0);
                  translate([25.0, 0.0, 3.061616997868383e-16]) {
                    translate([0.0, 0.0, 0.0]) {
                      rotate(a = 90.0, v = [0.0, 1.0, 0.0]) {
                        translate([-0.0, -0.0, -0.0]) {
                          cyl(r = 3.0, h = 10.0, chamfer = 0.5);
                        }
                      }
                    }
                  }
                }
                translate([-25.0, 0.0, 3.061616997868383e-16]) {
                  translate([0.0, 0.0, 0.0]) {
                    rotate(a = 90.0, v = [0.0, -1.0, 0.0]) {
                      translate([-0.0, -0.0, -0.0]) {
                        cyl(r = 3.0, h = 10.0, chamfer = 0.5);
                      }
                    }
                  }
                }
              }
              translate([0.0, -25.0, 3.061616997868383e-16]) {
                translate([0.0, 0.0, 0.0]) {
                  rotate(a = 90.0, v = [1.0, 0.0, -0.0]) {
                    translate([-0.0, -0.0, -0.0]) {
                      cyl(r = 3.0, h = 10.0, chamfer = 0.5);
                    }
                  }
                }
              }
            }
            translate([0.0, 25.0, 3.061616997868383e-16]) {
              translate([0.0, 0.0, 0.0]) {
                rotate(a = 90.0, v = [-1.0, 0.0, 0.0]) {
                  translate([-0.0, -0.0, -0.0]) {
                    cyl(r = 3.0, h = 10.0, chamfer = 0.5);
                  }
                }
              }
            }
          }
          translate([0.0, 0.0, 18.0]) {
            cyl(r = 5.0, h = 26.0, rounding = 2.0);
          }
        }
        translate([0.0, 0.0, 34.0]) {
          cuboid([16.0, 16.0, 6.0], rounding = 2.0);
        }
      }
      translate([0.0, 0.0, 42.0]) {
        sphere(5.0);
      }
    }
    translate([70.0, 0, 0]) {
      union() {
        union() {
          cuboid([20.0, 20.0, 20.0], chamfer = 2.0);
          translate([12.828427124746192, 0.0, 12.82842712474619]) {
            translate([0.0, 0.0, 0.0]) {
              rotate(a = 45.00000000000001, v = [0.0, 0.7071067811865475, 0.0]) {
                translate([-0.0, -0.0, -0.0]) {
                  cyl(r = 3.0, h = 8.0, chamfer = 0.5);
                }
              }
            }
          }
        }
        translate([-12.828427124746192, 0.0, 20.60660171779821]) {
          translate([0.0, 0.0, 0.0]) {
            rotate(a = 45.00000000000001, v = [0.0, -0.7071067811865475, 0.0]) {
              translate([-0.0, -0.0, -0.0]) {
                cyl(r = 3.0, h = 8.0, chamfer = 0.5);
              }
            }
          }
        }
      }
    }
  }
  translate([-50.0, 0, 0]) {
    translate([-0.0, -0.0, 8.0]) {
      sphere(8.0);
    }
  }
}
$fn = 50;