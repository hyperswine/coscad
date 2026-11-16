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
                    union() {
                      union() {
                        union() {
                          union() {
                            translate([0, 2, -2]) {
                              cuboid([110, 88, 4]);
                            }
                            difference() {
                              translate([0, 0, 0.5]) {
                                zcyl(r = 7.5, l = 1);
                              }
                              translate([0, 0, 0.5]) {
                                zcyl(r = 3.2, l = 3);
                              }
                            }
                          }
                          translate([-45, 41, 15.25]) {
                            cuboid([10, 8, 30.5]);
                          }
                        }
                        translate([0, 41, 15.25]) {
                          cuboid([12, 8, 30.5]);
                        }
                      }
                      translate([45, 41, 15.25]) {
                        cuboid([10, 8, 30.5]);
                      }
                    }
                    translate([0, 0, -1.5]) {
                      zcyl(r = 2.7, l = 7);
                    }
                  }
                  translate([0, 0, -4.1]) {
                    cylinder(h = 3.5, r = 4.7, $fn = 6);
                  }
                }
                translate([38, -2.5, -2]) {
                  zcyl(r = 2.2, l = 6);
                }
              }
              translate([38, -2.5, -4.1]) {
                cylinder(h = 3, r = 3.9, $fn = 6);
              }
            }
            translate([43, -2.5, -2]) {
              zcyl(r = 2.2, l = 6);
            }
          }
          translate([43, -2.5, -4.1]) {
            cylinder(h = 3, r = 3.9, $fn = 6);
          }
        }
        translate([-45, 43.4, 26]) {
          zcyl(r = 1.3, l = 11);
        }
      }
      translate([-3, 43.4, 26]) {
        zcyl(r = 1.3, l = 11);
      }
    }
    translate([3, 43.4, 26]) {
      zcyl(r = 1.3, l = 11);
    }
  }
  translate([45, 43.4, 26]) {
    zcyl(r = 1.3, l = 11);
  }
}
$fn = 50;