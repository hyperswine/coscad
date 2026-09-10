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
                  union() {
                    union() {
                      union() {
                        difference() {
                          difference() {
                            union() {
                              hull() {
                                translate([0, 122, 0]) {
                                  sphere(9.5);
                                }
                                translate([0, 155, 0]) {
                                  sphere(9.5);
                                }
                              }
                              hull() {
                                translate([0, 155, 0]) {
                                  sphere(9.5);
                                }
                                translate([0, 246, 0]) {
                                  sphere(5);
                                }
                              }
                            }
                            translate([0, 131, 0]) {
                              ycyl(r = 6, l = 46);
                            }
                          }
                          hull() {
                            translate([0, 165, 0]) {
                              sphere(5.5);
                            }
                            translate([0, 238, 0]) {
                              sphere(2.5);
                            }
                          }
                        }
                        translate([0, 0, 8]) {
                          rotate([0, -6, 0]) {
                            translate([13, 0, 0]) {
                              translate([0, 187, 0]) {
                                translate([0, 0, -3]) {
                                  cuboid([30, 92, 6]);
                                }
                              }
                            }
                          }
                        }
                      }
                      mirror([1, 0, 0]) {
                        translate([0, 0, 8]) {
                          rotate([0, -6, 0]) {
                            translate([13, 0, 0]) {
                              translate([0, 187, 0]) {
                                translate([0, 0, -3]) {
                                  cuboid([30, 92, 6]);
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                    translate([0, 239, 0]) {
                      translate([0, 0, 3]) {
                        cuboid([16, 22, 6]);
                      }
                    }
                  }
                  translate([0, 0, 8]) {
                    rotate([0, -6, 0]) {
                      translate([13, 0, 0]) {
                        translate([0, 187, 0]) {
                          translate([0, 0, 3]) {
                            cuboid([30, 92, 6]);
                          }
                        }
                      }
                    }
                  }
                }
                mirror([1, 0, 0]) {
                  translate([0, 0, 8]) {
                    rotate([0, -6, 0]) {
                      translate([13, 0, 0]) {
                        translate([0, 187, 0]) {
                          translate([0, 0, 3]) {
                            cuboid([30, 92, 6]);
                          }
                        }
                      }
                    }
                  }
                }
              }
              translate([0, 0, 8]) {
                rotate([0, -6, 0]) {
                  translate([16.5, 0, 0]) {
                    translate([0, 160, 0]) {
                      translate([0, 0, -3.5]) {
                        zcyl(r = 2.05, l = 9);
                      }
                    }
                  }
                }
              }
            }
            translate([0, 0, 8]) {
              rotate([0, -6, 0]) {
                translate([16.5, 0, 0]) {
                  translate([0, 208, 0]) {
                    translate([0, 0, -3.5]) {
                      zcyl(r = 2.05, l = 9);
                    }
                  }
                }
              }
            }
          }
          mirror([1, 0, 0]) {
            translate([0, 0, 8]) {
              rotate([0, -6, 0]) {
                translate([16.5, 0, 0]) {
                  translate([0, 160, 0]) {
                    translate([0, 0, -3.5]) {
                      zcyl(r = 2.05, l = 9);
                    }
                  }
                }
              }
            }
          }
        }
        mirror([1, 0, 0]) {
          translate([0, 0, 8]) {
            rotate([0, -6, 0]) {
              translate([16.5, 0, 0]) {
                translate([0, 208, 0]) {
                  translate([0, 0, -3.5]) {
                    zcyl(r = 2.05, l = 9);
                  }
                }
              }
            }
          }
        }
      }
      translate([0, 232, 0]) {
        translate([0, 0, 2]) {
          zcyl(r = 2.05, l = 9);
        }
      }
    }
    translate([0, 246, 0]) {
      translate([0, 0, 2]) {
        zcyl(r = 2.05, l = 9);
      }
    }
  }
  translate([0, 127, 0]) {
    translate([0, 0, 6]) {
      zcyl(r = 1.7, l = 12);
    }
  }
}
$fn = 50;