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
                                translate([0, 122.0, 0]) {
                                  sphere(9.5);
                                }
                                translate([0, 155.0, 0]) {
                                  sphere(9.5);
                                }
                              }
                              hull() {
                                translate([0, 155.0, 0]) {
                                  sphere(9.5);
                                }
                                translate([0, 246.0, 0]) {
                                  sphere(5.0);
                                }
                              }
                            }
                            translate([0, 131.0, 0]) {
                              ycyl(r = 6.0, l = 46.0);
                            }
                          }
                          hull() {
                            translate([0, 165.0, 0]) {
                              sphere(5.5);
                            }
                            translate([0, 238.0, 0]) {
                              sphere(2.5);
                            }
                          }
                        }
                        translate([0, 0, 8.0]) {
                          rotate([0, -6.0, 0]) {
                            translate([13.0, 0, 0]) {
                              translate([0, 187.0, 0]) {
                                translate([0, 0, -3.0]) {
                                  cuboid([30.0, 92.0, 6.0]);
                                }
                              }
                            }
                          }
                        }
                      }
                      mirror([1.0, 0.0, 0.0]) {
                        translate([0, 0, 8.0]) {
                          rotate([0, -6.0, 0]) {
                            translate([13.0, 0, 0]) {
                              translate([0, 187.0, 0]) {
                                translate([0, 0, -3.0]) {
                                  cuboid([30.0, 92.0, 6.0]);
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                    translate([0, 239.0, 0]) {
                      translate([0, 0, 3.0]) {
                        cuboid([16.0, 22.0, 6.0]);
                      }
                    }
                  }
                  translate([0, 0, 8.0]) {
                    rotate([0, -6.0, 0]) {
                      translate([13.0, 0, 0]) {
                        translate([0, 187.0, 0]) {
                          translate([0, 0, 3.0]) {
                            cuboid([30.0, 92.0, 6.0]);
                          }
                        }
                      }
                    }
                  }
                }
                mirror([1.0, 0.0, 0.0]) {
                  translate([0, 0, 8.0]) {
                    rotate([0, -6.0, 0]) {
                      translate([13.0, 0, 0]) {
                        translate([0, 187.0, 0]) {
                          translate([0, 0, 3.0]) {
                            cuboid([30.0, 92.0, 6.0]);
                          }
                        }
                      }
                    }
                  }
                }
              }
              translate([0, 0, 8.0]) {
                rotate([0, -6.0, 0]) {
                  translate([16.5, 0, 0]) {
                    translate([0, 160.0, 0]) {
                      translate([0, 0, -3.5]) {
                        zcyl(r = 2.05, l = 9.0);
                      }
                    }
                  }
                }
              }
            }
            translate([0, 0, 8.0]) {
              rotate([0, -6.0, 0]) {
                translate([16.5, 0, 0]) {
                  translate([0, 208.0, 0]) {
                    translate([0, 0, -3.5]) {
                      zcyl(r = 2.05, l = 9.0);
                    }
                  }
                }
              }
            }
          }
          mirror([1.0, 0.0, 0.0]) {
            translate([0, 0, 8.0]) {
              rotate([0, -6.0, 0]) {
                translate([16.5, 0, 0]) {
                  translate([0, 160.0, 0]) {
                    translate([0, 0, -3.5]) {
                      zcyl(r = 2.05, l = 9.0);
                    }
                  }
                }
              }
            }
          }
        }
        mirror([1.0, 0.0, 0.0]) {
          translate([0, 0, 8.0]) {
            rotate([0, -6.0, 0]) {
              translate([16.5, 0, 0]) {
                translate([0, 208.0, 0]) {
                  translate([0, 0, -3.5]) {
                    zcyl(r = 2.05, l = 9.0);
                  }
                }
              }
            }
          }
        }
      }
      translate([0, 232.0, 0]) {
        translate([0, 0, 2.0]) {
          zcyl(r = 2.05, l = 9.0);
        }
      }
    }
    translate([0, 246.0, 0]) {
      translate([0, 0, 2.0]) {
        zcyl(r = 2.05, l = 9.0);
      }
    }
  }
  translate([0, 127.0, 0]) {
    translate([0, 0, 6.0]) {
      zcyl(r = 1.7, l = 12.0);
    }
  }
}
$fn = 50;