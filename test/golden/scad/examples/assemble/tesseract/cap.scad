include <BOSL2/std.scad>

difference() {
  union() {
    intersection() {
      difference() {
        translate([6, 6, 6]) {
          cuboid([12, 12, 12], chamfer = 0.4);
        }
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
      translate([-4, -4, -4]) {
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
    translate([7, 7, 7]) {
      rotate([0, 0, 45]) {
        rotate([0, 54.735610317, 0]) {
          difference() {
            translate([0, 0, 1.25]) {
              zcyl(r = 1.68, l = 2.9);
            }
            translate([0, 0, 2]) {
              cuboid([0.7, 4, 3]);
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
        translate([12, 12, 12]) {
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
        translate([12, 12, 12]) {
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
      translate([12, 12, 12]) {
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
$fn = 50;