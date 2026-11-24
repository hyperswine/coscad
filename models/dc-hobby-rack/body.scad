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
                  difference() {
                    difference() {
                      difference() {
                        translate([0, 0, 21.4]) {
                          cuboid([23.7, 18.8, 22.4]);
                        }
                        translate([0, 0, 20.3]) {
                          cuboid([20, 15.1, 22.2]);
                        }
                      }
                      translate([0, 0, 31.4]) {
                        zcyl(r = 6, l = 5);
                      }
                    }
                    translate([0, -8, 20.3]) {
                      cuboid([5, 6, 22.2]);
                    }
                  }
                  translate([10.25, 0, 20.8]) {
                    zcyl(r = 0.55, l = 16);
                  }
                }
                translate([-20.5, 0, 0]) {
                  translate([10.25, 0, 20.8]) {
                    zcyl(r = 0.55, l = 16);
                  }
                }
              }
              translate([10.925, 0, 5.2]) {
                cuboid([1.85, 18.8, 10.4]);
              }
            }
            translate([-21.85, 0, 0]) {
              translate([10.925, 0, 5.2]) {
                cuboid([1.85, 18.8, 10.4]);
              }
            }
          }
          difference() {
            translate([0, 18, 1]) {
              cuboid([27, 24, 2]);
            }
            translate([0, -2, 5.1]) {
              cuboid([20, 20, 11.2]);
            }
          }
        }
        translate([0, 24.75, 16.6]) {
          cuboid([27, 11.5, 33.2]);
        }
      }
      translate([0, 24.7, 35.375]) {
        cuboid([22, 3.05, 4.35]);
      }
    }
    translate([8, 24.7, 38.75]) {
      zcyl(r = 0.96, l = 2.6);
    }
  }
  translate([-16, 0, 0]) {
    translate([8, 24.7, 38.75]) {
      zcyl(r = 0.96, l = 2.6);
    }
  }
}
$fn = 50;