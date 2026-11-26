include <BOSL2/std.scad>

difference() {
  union() {
    translate([66.4, 0, -5]) {
      difference() {
        cuboid([173.8, 44, 10]);
        cuboid([163.8, 34, 12]);
      }
    }
    translate([0, 0, -5]) {
      union() {
        union() {
          union() {
            union() {
              union() {
                translate([-15, 18, 0]) {
                  zcyl(r = 9, l = 10);
                }
                translate([-15, -18, 0]) {
                  zcyl(r = 9, l = 10);
                }
              }
              translate([147.8, 18, 0]) {
                zcyl(r = 9, l = 10);
              }
            }
            translate([147.8, -18, 0]) {
              zcyl(r = 9, l = 10);
            }
          }
          zcyl(r = 9, l = 10);
        }
        cuboid([6, 44, 10]);
      }
    }
  }
  translate([0, 0, -5]) {
    union() {
      union() {
        union() {
          union() {
            union() {
              union() {
                union() {
                  union() {
                    zcyl(r = 5.5, l = 12);
                    translate([-15, 18, 0]) {
                      translate([0, 0, 0.8]) {
                        cylinder(h = 4.21, r = 4.792, $fn = 6);
                      }
                    }
                  }
                  translate([-15, 18, 0]) {
                    zcyl(r = 3, l = 12);
                  }
                }
                translate([-15, -18, 0]) {
                  translate([0, 0, 0.8]) {
                    cylinder(h = 4.21, r = 4.792, $fn = 6);
                  }
                }
              }
              translate([-15, -18, 0]) {
                zcyl(r = 3, l = 12);
              }
            }
            translate([147.8, 18, 0]) {
              translate([0, 0, 0.8]) {
                cylinder(h = 4.21, r = 4.792, $fn = 6);
              }
            }
          }
          translate([147.8, 18, 0]) {
            zcyl(r = 3, l = 12);
          }
        }
        translate([147.8, -18, 0]) {
          translate([0, 0, 0.8]) {
            cylinder(h = 4.21, r = 4.792, $fn = 6);
          }
        }
      }
      translate([147.8, -18, 0]) {
        zcyl(r = 3, l = 12);
      }
    }
  }
}
$fn = 50;