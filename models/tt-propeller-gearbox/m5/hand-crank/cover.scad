include <BOSL2/std.scad>

translate([0, 0, 8.5]) {
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
                        translate([66.4, 0, 1.5]) {
                          cuboid([173.8, 44, 3]);
                        }
                        translate([-15, 0, 0]) {
                          union() {
                            translate([0, 18, 0]) {
                              translate([0, 0, 6]) {
                                zcyl(r = 2.8, l = 20);
                              }
                            }
                            translate([0, -18, 0]) {
                              translate([0, 0, 6]) {
                                zcyl(r = 2.8, l = 20);
                              }
                            }
                          }
                        }
                      }
                      translate([147.8, 0, 0]) {
                        union() {
                          translate([0, 18, 0]) {
                            translate([0, 0, 6]) {
                              zcyl(r = 2.8, l = 20);
                            }
                          }
                          translate([0, -18, 0]) {
                            translate([0, 0, 6]) {
                              zcyl(r = 2.8, l = 20);
                            }
                          }
                        }
                      }
                    }
                    translate([33.2, 0, 0]) {
                      translate([0, 0, 1.5]) {
                        zcyl(r = 3.25, l = 6);
                      }
                    }
                  }
                  translate([66.4, 0, 0]) {
                    translate([0, 0, 1.5]) {
                      zcyl(r = 3.25, l = 6);
                    }
                  }
                }
                translate([99.6, 0, 0]) {
                  translate([0, 0, 1.5]) {
                    zcyl(r = 3.25, l = 6);
                  }
                }
              }
              translate([132.8, 0, 0]) {
                translate([0, 0, 2]) {
                  zcyl(r = 4.3, l = 8);
                }
              }
            }
            translate([0, 0, 2]) {
              zcyl(r = 4.3, l = 8);
            }
          }
          translate([-15, 18, 0]) {
            translate([0, 0, 6]) {
              zcyl(r = 2.8, l = 20);
            }
          }
        }
        translate([-15, -18, 0]) {
          translate([0, 0, 6]) {
            zcyl(r = 2.8, l = 20);
          }
        }
      }
      translate([147.8, 18, 0]) {
        translate([0, 0, 6]) {
          zcyl(r = 2.8, l = 20);
        }
      }
    }
    translate([147.8, -18, 0]) {
      translate([0, 0, 6]) {
        zcyl(r = 2.8, l = 20);
      }
    }
  }
}
$fn = 50;