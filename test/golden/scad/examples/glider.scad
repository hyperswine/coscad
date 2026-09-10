include <BOSL2/std.scad>

scale([0.526, 0.526, 0.526]) {
  difference() {
    union() {
      union() {
        union() {
          union() {
            union() {
              union() {
                union() {
                  hull() {
                    translate([0, -128.0, 0]) {
                      sphere(2.5);
                    }
                    translate([0, -95.0, 0]) {
                      sphere(9.0);
                    }
                  }
                  hull() {
                    translate([0, -95.0, 0]) {
                      sphere(9.0);
                    }
                    translate([0, -15.0, 0]) {
                      sphere(7.0);
                    }
                  }
                }
                hull() {
                  translate([0, -15.0, 0]) {
                    sphere(7.0);
                  }
                  translate([0, 70.0, 0]) {
                    sphere(3.5);
                  }
                }
              }
              hull() {
                translate([0, 70.0, 0]) {
                  sphere(3.5);
                }
                translate([0, 126.0, 0]) {
                  sphere(2.5);
                }
              }
            }
            hull() {
              hull() {
                translate([3.0, 0, 0]) {
                  translate([0, -40.0, 0]) {
                    translate([0, 0, -4.0]) {
                      zcyl(r = 1.5, l = 3.0);
                    }
                  }
                }
                translate([3.0, 0, 0]) {
                  translate([0, 45.0, 0]) {
                    translate([0, 0, -4.0]) {
                      zcyl(r = 1.5, l = 3.0);
                    }
                  }
                }
              }
              translate([113.0, 0, 0]) {
                translate([0, 45.0, 0]) {
                  translate([0, 0, 7.0]) {
                    zcyl(r = 1.5, l = 3.0);
                  }
                }
              }
            }
          }
          mirror([1.0, 0.0, 0.0]) {
            hull() {
              hull() {
                translate([3.0, 0, 0]) {
                  translate([0, -40.0, 0]) {
                    translate([0, 0, -4.0]) {
                      zcyl(r = 1.5, l = 3.0);
                    }
                  }
                }
                translate([3.0, 0, 0]) {
                  translate([0, 45.0, 0]) {
                    translate([0, 0, -4.0]) {
                      zcyl(r = 1.5, l = 3.0);
                    }
                  }
                }
              }
              translate([113.0, 0, 0]) {
                translate([0, 45.0, 0]) {
                  translate([0, 0, 7.0]) {
                    zcyl(r = 1.5, l = 3.0);
                  }
                }
              }
            }
          }
        }
        translate([0, 114.0, 0]) {
          cuboid([96.0, 26.0, 2.5]);
        }
      }
      hull() {
        hull() {
          translate([0, 92.0, 0]) {
            translate([0, 0, 2.0]) {
              xcyl(r = 1.25, l = 2.5);
            }
          }
          translate([0, 127.0, 0]) {
            translate([0, 0, 2.0]) {
              xcyl(r = 1.25, l = 2.5);
            }
          }
        }
        translate([0, 127.0, 0]) {
          translate([0, 0, 38.0]) {
            xcyl(r = 1.25, l = 2.5);
          }
        }
      }
    }
    translate([0, -100.0, 0]) {
      translate([0, 0, -8.0]) {
        zcyl(r = 4.5, l = 10.0);
      }
    }
  }
}
$fn = 50;