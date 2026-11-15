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
                      union() {
                        union() {
                          union() {
                            union() {
                              union() {
                                translate([0, 0, 1.5]) {
                                  cuboid([152, 128, 3]);
                                }
                                translate([0, 0, 7]) {
                                  zcyl(r = 3.45, l = 8);
                                }
                              }
                              translate([-57, 0, 3.75]) {
                                zcyl(r = 1.85, l = 1.5);
                              }
                            }
                            translate([-54, 49, 4]) {
                              cuboid([30, 11, 2]);
                            }
                          }
                          translate([54, 49, 4]) {
                            cuboid([30, 11, 2]);
                          }
                        }
                        translate([-45, 47, 9]) {
                          zcyl(r = 3, l = 12);
                        }
                      }
                      hull() {
                        translate([-47.95, 47, 10.4]) {
                          zcyl(r = 0.17, l = 0.2);
                        }
                        translate([-47.95, 47, 14.7]) {
                          zcyl(r = 6.0e-2, l = 0.2);
                        }
                      }
                    }
                    hull() {
                      translate([-42.05, 47, 10.4]) {
                        zcyl(r = 0.17, l = 0.2);
                      }
                      translate([-42.05, 47, 14.7]) {
                        zcyl(r = 6.0e-2, l = 0.2);
                      }
                    }
                  }
                  translate([45, 47, 9]) {
                    zcyl(r = 3, l = 12);
                  }
                }
                hull() {
                  translate([42.05, 47, 10.4]) {
                    zcyl(r = 0.17, l = 0.2);
                  }
                  translate([42.05, 47, 14.7]) {
                    zcyl(r = 6.0e-2, l = 0.2);
                  }
                }
              }
              hull() {
                translate([47.95, 47, 10.4]) {
                  zcyl(r = 0.17, l = 0.2);
                }
                translate([47.95, 47, 14.7]) {
                  zcyl(r = 6.0e-2, l = 0.2);
                }
              }
            }
            translate([0, -60, 6.25]) {
              cuboid([14, 8, 6.5]);
            }
          }
          translate([0, -60, 9]) {
            zcyl(r = 3, l = 12);
          }
        }
        hull() {
          translate([-2.95, -60, 10.4]) {
            zcyl(r = 0.17, l = 0.2);
          }
          translate([-2.95, -60, 14.7]) {
            zcyl(r = 6.0e-2, l = 0.2);
          }
        }
      }
      hull() {
        translate([2.95, -60, 10.4]) {
          zcyl(r = 0.17, l = 0.2);
        }
        translate([2.95, -60, 14.7]) {
          zcyl(r = 6.0e-2, l = 0.2);
        }
      }
    }
    translate([-69, 58, 6.25]) {
      cuboid([5, 5, 6.5]);
    }
  }
  translate([69, 58, 6.25]) {
    cuboid([5, 5, 6.5]);
  }
}
$fn = 50;