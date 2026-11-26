include <BOSL2/std.scad>

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
                    union() {
                      translate([66.4, 0, 1.5]) {
                        cuboid([173.8, 44, 3]);
                      }
                      difference() {
                        difference() {
                          translate([-34, 0, 1.5]) {
                            cuboid([46, 48, 3]);
                          }
                          translate([0, 18, 0]) {
                            hull() {
                              translate([-46, 0, 0]) {
                                zcyl(r = 2.8, l = 8);
                              }
                              translate([-32, 0, 0]) {
                                zcyl(r = 2.8, l = 8);
                              }
                            }
                          }
                        }
                        translate([0, -18, 0]) {
                          hull() {
                            translate([-46, 0, 0]) {
                              zcyl(r = 2.8, l = 8);
                            }
                            translate([-32, 0, 0]) {
                              zcyl(r = 2.8, l = 8);
                            }
                          }
                        }
                      }
                    }
                    union() {
                      union() {
                        union() {
                          translate([66.4, 20.75, 5.65]) {
                            cuboid([173.8, 2.5, 5.7]);
                          }
                          translate([0, -41.5, 0]) {
                            translate([66.4, 20.75, 5.65]) {
                              cuboid([173.8, 2.5, 5.7]);
                            }
                          }
                        }
                        translate([-19.6, 0, 5.65]) {
                          cuboid([1.8, 44, 5.7]);
                        }
                      }
                      translate([172, 0, 0]) {
                        translate([-19.6, 0, 5.65]) {
                          cuboid([1.8, 44, 5.7]);
                        }
                      }
                    }
                  }
                  union() {
                    union() {
                      translate([33.2, 0, 0]) {
                        translate([0, 0, 6]) {
                          zcyl(r = 3, l = 8);
                        }
                      }
                      translate([66.4, 0, 0]) {
                        translate([0, 0, 6]) {
                          zcyl(r = 3, l = 8);
                        }
                      }
                    }
                    translate([99.6, 0, 0]) {
                      translate([0, 0, 6]) {
                        zcyl(r = 3, l = 8);
                      }
                    }
                  }
                }
                translate([132.8, 0, 4]) {
                  zcyl(r = 2.2, l = 2.8);
                }
              }
              union() {
                union() {
                  union() {
                    translate([-15, 18, 0]) {
                      translate([0, 0, 4.25]) {
                        zcyl(r = 5.2, l = 8.5);
                      }
                    }
                    translate([-15, -18, 0]) {
                      translate([0, 0, 4.25]) {
                        zcyl(r = 5.2, l = 8.5);
                      }
                    }
                  }
                  translate([147.8, 18, 0]) {
                    translate([0, 0, 4.25]) {
                      zcyl(r = 5.2, l = 8.5);
                    }
                  }
                }
                translate([147.8, -18, 0]) {
                  translate([0, 0, 4.25]) {
                    zcyl(r = 5.2, l = 8.5);
                  }
                }
              }
            }
            translate([0, 0, 2]) {
              zcyl(r = 4.3, l = 8);
            }
          }
          union() {
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
$fn = 50;