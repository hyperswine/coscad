include <BOSL2/std.scad>

union() {
  union() {
    union() {
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
      translate([0, 52, 0]) {
        translate([16, 0, 25]) {
          rotate([0, 180, 0]) {
            difference() {
              difference() {
                union() {
                  union() {
                    union() {
                      translate([0, 0, 4.95]) {
                        intersection() {
                          zcyl(r = 2.65, l = 6.7);
                          cuboid([3.6, 7, 20]);
                        }
                      }
                      translate([0, 0, 10.05]) {
                        zcyl(r = 4, l = 3.9);
                      }
                    }
                    translate([0, 0, 15.5]) {
                      zcyl(r = 6, l = 7.4);
                    }
                  }
                  hull() {
                    translate([0, 0, 22]) {
                      zcyl(r = 7, l = 6);
                    }
                    translate([32, 0, 22]) {
                      zcyl(r = 7, l = 6);
                    }
                  }
                }
                translate([32, 0, 25]) {
                  zcyl(r = 2.8, l = 20);
                }
              }
              translate([32, 0, 18.99]) {
                cylinder(h = 4.2, r = 4.792, $fn = 6);
              }
            }
          }
        }
      }
    }
    translate([60, 52, 0]) {
      translate([32, 0, 43]) {
        rotate([0, 180, 0]) {
          translate([32, 0, 0]) {
            difference() {
              union() {
                translate([0, 0, 33.25]) {
                  zcyl(r = 4, l = 16.5);
                }
                translate([0, 0, 42.25]) {
                  zcyl(r = 6.5, l = 1.5);
                }
              }
              translate([0, 0, 25]) {
                zcyl(r = 2.75, l = 40);
              }
            }
          }
        }
      }
    }
  }
  translate([90, 52, 0]) {
    translate([-32, 0, -25.2]) {
      translate([32, 0, 33.25]) {
        difference() {
          zcyl(r = 8, l = 16.1);
          zcyl(r = 4.3, l = 18.1);
        }
      }
    }
  }
}
$fn = 50;