include <BOSL2/std.scad>

difference() {
  union() {
    union() {
      union() {
        union() {
          union() {
            union() {
              difference() {
                translate([-1, -4, 1.5]) {
                  cuboid([228, 208, 3]);
                }
                union() {
                  union() {
                    union() {
                      union() {
                        union() {
                          union() {
                            union() {
                              translate([-50, 42, 1.5]) {
                                cuboid([44, 60, 10]);
                              }
                              translate([-8, -38, 1.5]) {
                                cuboid([40, 48, 10]);
                              }
                            }
                            translate([20, 44, 1.5]) {
                              cuboid([40, 52, 10]);
                            }
                          }
                          translate([27.5, -42.5, 1.5]) {
                            cuboid([25, 55, 10]);
                          }
                        }
                        translate([-90, 5, 1.5]) {
                          cuboid([20, 110, 10]);
                        }
                      }
                      translate([84, 61, 1.5]) {
                        cuboid([28, 38, 10]);
                      }
                    }
                    translate([84, -61, 1.5]) {
                      cuboid([28, 38, 10]);
                    }
                  }
                  translate([10, -87.5, 1.5]) {
                    cuboid([70, 25, 10]);
                  }
                }
              }
              translate([-63.7868, -56.7868, 0]) {
                translate([0, 0, 8.5]) {
                  zcyl(r = 5, l = 11);
                }
              }
            }
            translate([-106.2132, -99.2132, 0]) {
              translate([0, 0, 8.5]) {
                zcyl(r = 5, l = 11);
              }
            }
          }
          translate([100, 30, 0]) {
            translate([0, 0, 8.5]) {
              zcyl(r = 5, l = 11);
            }
          }
        }
        translate([100, -30, 0]) {
          translate([0, 0, 8.5]) {
            zcyl(r = 5, l = 11);
          }
        }
      }
      translate([-85, -78, 0]) {
        union() {
          translate([0, 0, 3.5]) {
            zcyl(r = 7, l = 1);
          }
          translate([0, 0, 4.65]) {
            zcyl(r = 2.2, l = 3.3);
          }
        }
      }
    }
    translate([100, 0, 0]) {
      union() {
        translate([0, 0, 3.5]) {
          zcyl(r = 7, l = 1);
        }
        translate([0, 0, 4.65]) {
          zcyl(r = 2.2, l = 3.3);
        }
      }
    }
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
                                translate([-63.7868, -56.7868, 0]) {
                                  zcyl(r = 2.8, l = 40);
                                }
                                translate([-106.2132, -99.2132, 0]) {
                                  zcyl(r = 2.8, l = 40);
                                }
                              }
                              translate([100, 30, 0]) {
                                zcyl(r = 2.8, l = 40);
                              }
                            }
                            translate([100, -30, 0]) {
                              zcyl(r = 2.8, l = 40);
                            }
                          }
                          translate([-112, 92, 0]) {
                            zcyl(r = 2.8, l = 40);
                          }
                        }
                        translate([106, 92, 0]) {
                          zcyl(r = 2.8, l = 40);
                        }
                      }
                      translate([106, -100, 0]) {
                        zcyl(r = 2.8, l = 40);
                      }
                    }
                    translate([-112, -20, 0]) {
                      zcyl(r = 2.8, l = 40);
                    }
                  }
                  translate([60, -85, 0]) {
                    zcyl(r = 2.8, l = 40);
                  }
                }
                translate([55, -15, 0]) {
                  zcyl(r = 2.8, l = 40);
                }
              }
              translate([55, 15, 0]) {
                zcyl(r = 2.8, l = 40);
              }
            }
            translate([60, 85, 0]) {
              zcyl(r = 2.8, l = 40);
            }
          }
          translate([25, 0, 0]) {
            zcyl(r = 2.8, l = 40);
          }
        }
        translate([-15, 85, 0]) {
          zcyl(r = 2.8, l = 40);
        }
      }
      translate([-85, 85, 0]) {
        zcyl(r = 2.8, l = 40);
      }
    }
    hull() {
      translate([-50, -18.55, 0]) {
        zcyl(r = 2.8, l = 40);
      }
      translate([-50, 1.45, 0]) {
        zcyl(r = 2.8, l = 40);
      }
    }
  }
}
$fn = 50;