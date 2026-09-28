include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          difference() {
            difference() {
              difference() {
                difference() {
                  difference() {
                    difference() {
                      difference() {
                        cuboid([30, 30, 30]);
                        translate([15, 15, 0]) {
                          zcyl(r = 3.5, l = 32);
                        }
                      }
                      translate([-15, 15, 0]) {
                        zcyl(r = 3.5, l = 32);
                      }
                    }
                    translate([-15, -15, 0]) {
                      zcyl(r = 3.5, l = 32);
                    }
                  }
                  translate([15, -15, 0]) {
                    zcyl(r = 3.5, l = 32);
                  }
                }
                translate([15, 15, 15]) {
                  rotate([0, 0, 45]) {
                    rotate([0, 54.7356, 0]) {
                      zcyl(r = 2.65, l = 26);
                    }
                  }
                }
              }
              translate([-15, 15, 15]) {
                rotate([0, 0, 135]) {
                  rotate([0, 54.7356, 0]) {
                    zcyl(r = 2.65, l = 26);
                  }
                }
              }
            }
            translate([-15, -15, 15]) {
              rotate([0, 0, 225]) {
                rotate([0, 54.7356, 0]) {
                  zcyl(r = 2.65, l = 26);
                }
              }
            }
          }
          translate([15, -15, 15]) {
            rotate([0, 0, 315]) {
              rotate([0, 54.7356, 0]) {
                zcyl(r = 2.65, l = 26);
              }
            }
          }
        }
        translate([15, 15, -15]) {
          rotate([0, 0, 225]) {
            rotate([0, 54.7356, 0]) {
              zcyl(r = 2.65, l = 26);
            }
          }
        }
      }
      translate([-15, 15, -15]) {
        rotate([0, 0, 315]) {
          rotate([0, 54.7356, 0]) {
            zcyl(r = 2.65, l = 26);
          }
        }
      }
    }
    translate([-15, -15, -15]) {
      rotate([0, 0, 45]) {
        rotate([0, 54.7356, 0]) {
          zcyl(r = 2.65, l = 26);
        }
      }
    }
  }
  translate([15, -15, -15]) {
    rotate([0, 0, 135]) {
      rotate([0, 54.7356, 0]) {
        zcyl(r = 2.65, l = 26);
      }
    }
  }
}
$fn = 50;