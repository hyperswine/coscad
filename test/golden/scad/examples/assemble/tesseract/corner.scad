include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      intersection() {
        translate([6, 6, 6]) {
          cuboid([12, 12, 12], chamfer = 0.4);
        }
        translate([6.9282032e-2, 6.9282032e-2, 6.9282032e-2]) {
          translate([7, 7, 7]) {
            rotate([0, 0, 45]) {
              rotate([0, 54.735610317, 0]) {
                translate([0, 0, 50]) {
                  cuboid([100, 100, 100]);
                }
              }
            }
          }
        }
      }
      union() {
        union() {
          hull() {
            union() {
              translate([9.7, 6, 6]) {
                xcyl(r = 2.8, l = 1.2);
              }
              translate([11.25, 6, 6]) {
                xcyl(r = 2.2, l = 2.3);
              }
            }
            translate([0, -12, -12]) {
              union() {
                translate([9.7, 6, 6]) {
                  xcyl(r = 2.8, l = 1.2);
                }
                translate([11.25, 6, 6]) {
                  xcyl(r = 2.2, l = 2.3);
                }
              }
            }
          }
          hull() {
            translate([12, 0, 0]) {
              rotate([0, 0, 90]) {
                union() {
                  translate([9.7, 6, 6]) {
                    xcyl(r = 2.8, l = 1.2);
                  }
                  translate([11.25, 6, 6]) {
                    xcyl(r = 2.2, l = 2.3);
                  }
                }
              }
            }
            translate([-12, 0, -12]) {
              translate([12, 0, 0]) {
                rotate([0, 0, 90]) {
                  union() {
                    translate([9.7, 6, 6]) {
                      xcyl(r = 2.8, l = 1.2);
                    }
                    translate([11.25, 6, 6]) {
                      xcyl(r = 2.2, l = 2.3);
                    }
                  }
                }
              }
            }
          }
        }
        hull() {
          translate([12, 0, 0]) {
            rotate([0, -90, 0]) {
              union() {
                translate([9.7, 6, 6]) {
                  xcyl(r = 2.8, l = 1.2);
                }
                translate([11.25, 6, 6]) {
                  xcyl(r = 2.2, l = 2.3);
                }
              }
            }
          }
          translate([-12, -12, 0]) {
            translate([12, 0, 0]) {
              rotate([0, -90, 0]) {
                union() {
                  translate([9.7, 6, 6]) {
                    xcyl(r = 2.8, l = 1.2);
                  }
                  translate([11.25, 6, 6]) {
                    xcyl(r = 2.2, l = 2.3);
                  }
                }
              }
            }
          }
        }
      }
    }
    translate([7, 7, 7]) {
      rotate([0, 0, 45]) {
        rotate([0, 54.735610317, 0]) {
          translate([0, 0, 1.3]) {
            zcyl(r = 1.65, l = 3.6);
          }
        }
      }
    }
  }
  translate([11.6, 11.6, 11.6]) {
    rotate([0, 0, 45]) {
      rotate([0, 54.735610317, 0]) {
        zcyl(r = 2.65, l = 5.6);
      }
    }
  }
}
$fn = 50;