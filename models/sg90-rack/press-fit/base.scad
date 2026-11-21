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
                      difference() {
                        difference() {
                          translate([0, -5.2, 9]) {
                            cuboid([18, 29, 18]);
                          }
                          translate([0, -5.2, 13.8]) {
                            cuboid([12.8, 23.4, 20]);
                          }
                        }
                        translate([0, 7.9, 13.8]) {
                          cuboid([6, 4, 20]);
                        }
                      }
                      translate([0, 18.75, 1]) {
                        cuboid([28, 23.5, 2]);
                      }
                    }
                    translate([0, 24.75, 19.1]) {
                      cuboid([27, 11.5, 38.2]);
                    }
                  }
                  translate([0, 24.7, 40.375]) {
                    cuboid([22, 3.05, 4.35]);
                  }
                }
                translate([6.65, -10, 10.5]) {
                  zcyl(r = 0.65, l = 13);
                }
              }
              translate([-6.65, -10, 10.5]) {
                zcyl(r = 0.65, l = 13);
              }
            }
            translate([4.5, 6.85, 10.5]) {
              zcyl(r = 0.65, l = 13);
            }
          }
          translate([4.5, -17.25, 10.5]) {
            zcyl(r = 0.65, l = 13);
          }
        }
        translate([-4.5, 6.85, 10.5]) {
          zcyl(r = 0.65, l = 13);
        }
      }
      translate([-4.5, -17.25, 10.5]) {
        zcyl(r = 0.65, l = 13);
      }
    }
    translate([-8, 24.7, 43.75]) {
      zcyl(r = 0.96, l = 2.6);
    }
  }
  translate([8, 24.7, 43.75]) {
    zcyl(r = 0.96, l = 2.6);
  }
}
$fn = 50;