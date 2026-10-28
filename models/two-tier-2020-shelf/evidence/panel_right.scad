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
                        intersection() {
                          union() {
                            translate([0, 0, 9.5]) {
                              cuboid([200, 200, 3]);
                            }
                            union() {
                              union() {
                                union() {
                                  union() {
                                    union() {
                                      union() {
                                        union() {
                                          union() {
                                            union() {
                                              translate([-90, 0, 4.05]) {
                                                cuboid([20, 200, 8.1]);
                                              }
                                              translate([90, 0, 4.05]) {
                                                cuboid([20, 200, 8.1]);
                                              }
                                            }
                                            translate([0, -90, 4.05]) {
                                              cuboid([200, 20, 8.1]);
                                            }
                                          }
                                          translate([0, 90, 4.05]) {
                                            cuboid([200, 20, 8.1]);
                                          }
                                        }
                                        translate([-50, 0, 4.05]) {
                                          cuboid([4, 200, 8.1]);
                                        }
                                      }
                                      translate([0, 0, 4.05]) {
                                        cuboid([4, 200, 8.1]);
                                      }
                                    }
                                    translate([50, 0, 4.05]) {
                                      cuboid([4, 200, 8.1]);
                                    }
                                  }
                                  translate([0, -50, 4.05]) {
                                    cuboid([200, 4, 8.1]);
                                  }
                                }
                                translate([0, 0, 4.05]) {
                                  cuboid([200, 4, 8.1]);
                                }
                              }
                              translate([0, 50, 4.05]) {
                                cuboid([200, 4, 8.1]);
                              }
                            }
                          }
                          translate([0, 0, 5.5]) {
                            cuboid([199.6, 199.6, 12]);
                          }
                        }
                        translate([-100, -90, 5]) {
                          cuboid([20.6, 20.6, 30]);
                        }
                      }
                      translate([-100, 90, 5]) {
                        cuboid([20.6, 20.6, 30]);
                      }
                    }
                    translate([90, -90, 5]) {
                      cuboid([20.6, 20.6, 30]);
                    }
                  }
                  translate([90, 90, 5]) {
                    cuboid([20.6, 20.6, 30]);
                  }
                }
                translate([-50, -90, 5]) {
                  zcyl(r = 2.8, l = 30);
                }
              }
              translate([-50, -90, 11]) {
                zcyl(r = 5.5, l = 10);
              }
            }
            translate([-50, 90, 5]) {
              zcyl(r = 2.8, l = 30);
            }
          }
          translate([-50, 90, 11]) {
            zcyl(r = 5.5, l = 10);
          }
        }
        translate([50, -90, 5]) {
          zcyl(r = 2.8, l = 30);
        }
      }
      translate([50, -90, 11]) {
        zcyl(r = 5.5, l = 10);
      }
    }
    translate([50, 90, 5]) {
      zcyl(r = 2.8, l = 30);
    }
  }
  translate([50, 90, 11]) {
    zcyl(r = 5.5, l = 10);
  }
}
$fn = 50;