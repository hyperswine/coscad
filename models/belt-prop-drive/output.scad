include <BOSL2/std.scad>

translate([100, 0, 0]) {
  difference() {
    union() {
      union() {
        union() {
          union() {
            difference() {
              translate([0, 0, 8.5]) {
                zcyl(r = 6.1122, l = 7);
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
                                                    translate([6.7622, 0, 8.5]) {
                                                      zcyl(r = 0.7, l = 9);
                                                    }
                                                    translate([6.4312, 2.0896, 8.5]) {
                                                      zcyl(r = 0.7, l = 9);
                                                    }
                                                  }
                                                  translate([5.4707, 3.9747, 8.5]) {
                                                    zcyl(r = 0.7, l = 9);
                                                  }
                                                }
                                                translate([3.9747, 5.4707, 8.5]) {
                                                  zcyl(r = 0.7, l = 9);
                                                }
                                              }
                                              translate([2.0896, 6.4312, 8.5]) {
                                                zcyl(r = 0.7, l = 9);
                                              }
                                            }
                                            translate([0, 6.7622, 8.5]) {
                                              zcyl(r = 0.7, l = 9);
                                            }
                                          }
                                          translate([-2.0896, 6.4312, 8.5]) {
                                            zcyl(r = 0.7, l = 9);
                                          }
                                        }
                                        translate([-3.9747, 5.4707, 8.5]) {
                                          zcyl(r = 0.7, l = 9);
                                        }
                                      }
                                      translate([-5.4707, 3.9747, 8.5]) {
                                        zcyl(r = 0.7, l = 9);
                                      }
                                    }
                                    translate([-6.4312, 2.0896, 8.5]) {
                                      zcyl(r = 0.7, l = 9);
                                    }
                                  }
                                  translate([-6.7622, 0, 8.5]) {
                                    zcyl(r = 0.7, l = 9);
                                  }
                                }
                                translate([-6.4312, -2.0896, 8.5]) {
                                  zcyl(r = 0.7, l = 9);
                                }
                              }
                              translate([-5.4707, -3.9747, 8.5]) {
                                zcyl(r = 0.7, l = 9);
                              }
                            }
                            translate([-3.9747, -5.4707, 8.5]) {
                              zcyl(r = 0.7, l = 9);
                            }
                          }
                          translate([-2.0896, -6.4312, 8.5]) {
                            zcyl(r = 0.7, l = 9);
                          }
                        }
                        translate([0, -6.7622, 8.5]) {
                          zcyl(r = 0.7, l = 9);
                        }
                      }
                      translate([2.0896, -6.4312, 8.5]) {
                        zcyl(r = 0.7, l = 9);
                      }
                    }
                    translate([3.9747, -5.4707, 8.5]) {
                      zcyl(r = 0.7, l = 9);
                    }
                  }
                  translate([5.4707, -3.9747, 8.5]) {
                    zcyl(r = 0.7, l = 9);
                  }
                }
                translate([6.4312, -2.0896, 8.5]) {
                  zcyl(r = 0.7, l = 9);
                }
              }
            }
            union() {
              translate([0, 0, 4.5]) {
                zcyl(r = 7.7122, l = 1);
              }
              translate([0, 0, 12.5]) {
                zcyl(r = 7.7122, l = 1);
              }
            }
          }
          translate([0, 0, 8.5]) {
            zcyl(r = 4, l = 9);
          }
        }
        translate([0, 0, 18.45]) {
          zcyl(r = 4, l = 11.1);
        }
      }
      difference() {
        translate([0, 0, 38.4]) {
          intersection() {
            zcyl(r = 2.7, l = 28.8);
            cuboid([3.7, 8, 40]);
          }
        }
        translate([0, 0, 47.3]) {
          difference() {
            zcyl(r = 4, l = 2.6);
            zcyl(r = 1.68, l = 4);
          }
        }
      }
    }
    translate([0, 0, 5.2]) {
      zcyl(r = 2.48, l = 2.6);
    }
  }
}
$fn = 50;