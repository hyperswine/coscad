include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      union() {
        union() {
          union() {
            union() {
              intersection() {
                rotate([-90, 0, 0]) {
                  skin([ccw_polygon([[-4.4, 0], [4.4, 0], [8, -3.6], [8, -26.4], [4.4, -30], [-4.4, -30], [-8, -26.4], [-8, -3.6]]), ccw_polygon([[-4.4, 0], [4.4, 0], [8, -3.6], [8, -26.4], [4.4, -30], [-4.4, -30], [-8, -26.4], [-8, -3.6]]), ccw_polygon([[-4.4, 0], [4.4, 0], [8, -3.6], [8, -26.4], [4.4, -30], [-4.4, -30], [-8, -26.4], [-8, -3.6]]), ccw_polygon([[-2.75, 0], [2.75, 0], [5, -2.25], [5, -10.75], [2.75, -13], [-2.75, -13], [-5, -10.75], [-5, -2.25]]), ccw_polygon([[-2.75, 0], [2.75, 0], [5, -2.25], [5, -10.75], [2.75, -13], [-2.75, -13], [-5, -10.75], [-5, -2.25]])], z = [-44, -32, 82, 158, 201], slices = 0, method = "reindex");
                }
                translate([0, 52, 25]) {
                  cuboid([80, 230, 80]);
                }
              }
              union() {
                intersection() {
                  difference() {
                    union() {
                      union() {
                        union() {
                          union() {
                            union() {
                              union() {
                                union() {
                                  union() {
                                    union() {
                                      hull() {
                                        translate([0, -119, 10]) {
                                          cuboid([24, 0.5, 20]);
                                        }
                                        translate([0, -113, 15]) {
                                          cuboid([24, 0.5, 30]);
                                        }
                                      }
                                      hull() {
                                        translate([0, -113, 15]) {
                                          cuboid([24, 0.5, 30]);
                                        }
                                        translate([0, -78, 18]) {
                                          cuboid([24, 0.5, 36]);
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([0, -78, 18]) {
                                        cuboid([24, 0.5, 36]);
                                      }
                                      translate([0, -48, 19.5]) {
                                        cuboid([18, 0.5, 39]);
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([0, -48, 19.5]) {
                                      cuboid([18, 0.5, 39]);
                                    }
                                    translate([0, -20, 20.5]) {
                                      cuboid([14, 0.5, 41]);
                                    }
                                  }
                                }
                                hull() {
                                  translate([0, -20, 20.5]) {
                                    cuboid([14, 0.5, 41]);
                                  }
                                  translate([0, 124, 17.293]) {
                                    cuboid([14, 0.5, 34.586]);
                                  }
                                }
                              }
                              hull() {
                                translate([0, 124, 17.293]) {
                                  cuboid([14, 0.5, 34.586]);
                                }
                                translate([0, 150, 13]) {
                                  cuboid([11.5, 0.5, 26]);
                                }
                              }
                            }
                            hull() {
                              translate([0, 150, 13]) {
                                cuboid([11.5, 0.5, 26]);
                              }
                              translate([0, 159, 7]) {
                                cuboid([9, 0.5, 14]);
                              }
                            }
                          }
                          hull() {
                            translate([0, 159, 7]) {
                              cuboid([9, 0.5, 14]);
                            }
                            translate([0, 204, 7]) {
                              cuboid([9, 0.5, 14]);
                            }
                          }
                        }
                        union() {
                          translate([0, 0, 44]) {
                            rotate([-2.5, 0, 0]) {
                              rotate([0, -6, 0]) {
                                difference() {
                                  hull() {
                                    translate([12.5, 58.5, -3.5]) {
                                      cuboid([31, 111, 7]);
                                    }
                                    translate([2, 58.5, -17.5]) {
                                      cuboid([10, 111, 21]);
                                    }
                                  }
                                  union() {
                                    union() {
                                      union() {
                                        translate([15, 26.4, 0]) {
                                          zcyl(r = 2.75, l = 30);
                                        }
                                        translate([15, 26.4, -43]) {
                                          cylinder(h = 40, r = 4.792, $fn = 6);
                                        }
                                      }
                                      translate([15, 74.4, 0]) {
                                        zcyl(r = 2.75, l = 30);
                                      }
                                    }
                                    translate([15, 74.4, -43]) {
                                      cylinder(h = 40, r = 4.792, $fn = 6);
                                    }
                                  }
                                }
                              }
                            }
                          }
                          mirror([1, 0, 0]) {
                            translate([0, 0, 44]) {
                              rotate([-2.5, 0, 0]) {
                                rotate([0, -6, 0]) {
                                  difference() {
                                    hull() {
                                      translate([12.5, 58.5, -3.5]) {
                                        cuboid([31, 111, 7]);
                                      }
                                      translate([2, 58.5, -17.5]) {
                                        cuboid([10, 111, 21]);
                                      }
                                    }
                                    union() {
                                      union() {
                                        union() {
                                          translate([15, 26.4, 0]) {
                                            zcyl(r = 2.75, l = 30);
                                          }
                                          translate([15, 26.4, -43]) {
                                            cylinder(h = 40, r = 4.792, $fn = 6);
                                          }
                                        }
                                        translate([15, 74.4, 0]) {
                                          zcyl(r = 2.75, l = 30);
                                        }
                                      }
                                      translate([15, 74.4, -43]) {
                                        cylinder(h = 40, r = 4.792, $fn = 6);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                      translate([0, 184, 0]) {
                        translate([0, 0, 19]) {
                          rotate([5.5, 0, 0]) {
                            translate([0, -184, 0]) {
                              translate([0, 0, -19]) {
                                difference() {
                                  hull() {
                                    translate([0, 184, 16]) {
                                      cuboid([18, 40, 6]);
                                    }
                                    translate([0, 184, 10.75]) {
                                      cuboid([9, 40, 4.5]);
                                    }
                                  }
                                  union() {
                                    union() {
                                      union() {
                                        translate([4.5, 194, 13]) {
                                          zcyl(r = 1.7, l = 14);
                                        }
                                        translate([6.25, 194, 16]) {
                                          cuboid([9.5, 5.9, 2.8]);
                                        }
                                      }
                                      translate([-4.5, 174, 13]) {
                                        zcyl(r = 1.7, l = 14);
                                      }
                                    }
                                    translate([-6.25, 174, 16]) {
                                      cuboid([9.5, 5.9, 2.8]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                    translate([0, 184, 0]) {
                      translate([0, 0, 19]) {
                        rotate([5.5, 0, 0]) {
                          translate([0, -184, 0]) {
                            translate([0, 0, -19]) {
                              union() {
                                union() {
                                  union() {
                                    translate([4.5, 194, 13]) {
                                      zcyl(r = 1.7, l = 14);
                                    }
                                    translate([6.25, 194, 16]) {
                                      cuboid([9.5, 5.9, 2.8]);
                                    }
                                  }
                                  translate([-4.5, 174, 13]) {
                                    zcyl(r = 1.7, l = 14);
                                  }
                                }
                                translate([-6.25, 174, 16]) {
                                  cuboid([9.5, 5.9, 2.8]);
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                  translate([0, -52.3, 25]) {
                    cuboid([60, 25, 60]);
                  }
                }
                translate([0, -53, 18]) {
                  cuboid([23, 18, 28]);
                }
              }
            }
            intersection() {
              difference() {
                union() {
                  union() {
                    union() {
                      union() {
                        union() {
                          union() {
                            union() {
                              union() {
                                union() {
                                  hull() {
                                    translate([0, -119, 10]) {
                                      cuboid([24, 0.5, 20]);
                                    }
                                    translate([0, -113, 15]) {
                                      cuboid([24, 0.5, 30]);
                                    }
                                  }
                                  hull() {
                                    translate([0, -113, 15]) {
                                      cuboid([24, 0.5, 30]);
                                    }
                                    translate([0, -78, 18]) {
                                      cuboid([24, 0.5, 36]);
                                    }
                                  }
                                }
                                hull() {
                                  translate([0, -78, 18]) {
                                    cuboid([24, 0.5, 36]);
                                  }
                                  translate([0, -48, 19.5]) {
                                    cuboid([18, 0.5, 39]);
                                  }
                                }
                              }
                              hull() {
                                translate([0, -48, 19.5]) {
                                  cuboid([18, 0.5, 39]);
                                }
                                translate([0, -20, 20.5]) {
                                  cuboid([14, 0.5, 41]);
                                }
                              }
                            }
                            hull() {
                              translate([0, -20, 20.5]) {
                                cuboid([14, 0.5, 41]);
                              }
                              translate([0, 124, 17.293]) {
                                cuboid([14, 0.5, 34.586]);
                              }
                            }
                          }
                          hull() {
                            translate([0, 124, 17.293]) {
                              cuboid([14, 0.5, 34.586]);
                            }
                            translate([0, 150, 13]) {
                              cuboid([11.5, 0.5, 26]);
                            }
                          }
                        }
                        hull() {
                          translate([0, 150, 13]) {
                            cuboid([11.5, 0.5, 26]);
                          }
                          translate([0, 159, 7]) {
                            cuboid([9, 0.5, 14]);
                          }
                        }
                      }
                      hull() {
                        translate([0, 159, 7]) {
                          cuboid([9, 0.5, 14]);
                        }
                        translate([0, 204, 7]) {
                          cuboid([9, 0.5, 14]);
                        }
                      }
                    }
                    union() {
                      translate([0, 0, 44]) {
                        rotate([-2.5, 0, 0]) {
                          rotate([0, -6, 0]) {
                            difference() {
                              hull() {
                                translate([12.5, 58.5, -3.5]) {
                                  cuboid([31, 111, 7]);
                                }
                                translate([2, 58.5, -17.5]) {
                                  cuboid([10, 111, 21]);
                                }
                              }
                              union() {
                                union() {
                                  union() {
                                    translate([15, 26.4, 0]) {
                                      zcyl(r = 2.75, l = 30);
                                    }
                                    translate([15, 26.4, -43]) {
                                      cylinder(h = 40, r = 4.792, $fn = 6);
                                    }
                                  }
                                  translate([15, 74.4, 0]) {
                                    zcyl(r = 2.75, l = 30);
                                  }
                                }
                                translate([15, 74.4, -43]) {
                                  cylinder(h = 40, r = 4.792, $fn = 6);
                                }
                              }
                            }
                          }
                        }
                      }
                      mirror([1, 0, 0]) {
                        translate([0, 0, 44]) {
                          rotate([-2.5, 0, 0]) {
                            rotate([0, -6, 0]) {
                              difference() {
                                hull() {
                                  translate([12.5, 58.5, -3.5]) {
                                    cuboid([31, 111, 7]);
                                  }
                                  translate([2, 58.5, -17.5]) {
                                    cuboid([10, 111, 21]);
                                  }
                                }
                                union() {
                                  union() {
                                    union() {
                                      translate([15, 26.4, 0]) {
                                        zcyl(r = 2.75, l = 30);
                                      }
                                      translate([15, 26.4, -43]) {
                                        cylinder(h = 40, r = 4.792, $fn = 6);
                                      }
                                    }
                                    translate([15, 74.4, 0]) {
                                      zcyl(r = 2.75, l = 30);
                                    }
                                  }
                                  translate([15, 74.4, -43]) {
                                    cylinder(h = 40, r = 4.792, $fn = 6);
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                  translate([0, 184, 0]) {
                    translate([0, 0, 19]) {
                      rotate([5.5, 0, 0]) {
                        translate([0, -184, 0]) {
                          translate([0, 0, -19]) {
                            difference() {
                              hull() {
                                translate([0, 184, 16]) {
                                  cuboid([18, 40, 6]);
                                }
                                translate([0, 184, 10.75]) {
                                  cuboid([9, 40, 4.5]);
                                }
                              }
                              union() {
                                union() {
                                  union() {
                                    translate([4.5, 194, 13]) {
                                      zcyl(r = 1.7, l = 14);
                                    }
                                    translate([6.25, 194, 16]) {
                                      cuboid([9.5, 5.9, 2.8]);
                                    }
                                  }
                                  translate([-4.5, 174, 13]) {
                                    zcyl(r = 1.7, l = 14);
                                  }
                                }
                                translate([-6.25, 174, 16]) {
                                  cuboid([9.5, 5.9, 2.8]);
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
                translate([0, 184, 0]) {
                  translate([0, 0, 19]) {
                    rotate([5.5, 0, 0]) {
                      translate([0, -184, 0]) {
                        translate([0, 0, -19]) {
                          union() {
                            union() {
                              union() {
                                translate([4.5, 194, 13]) {
                                  zcyl(r = 1.7, l = 14);
                                }
                                translate([6.25, 194, 16]) {
                                  cuboid([9.5, 5.9, 2.8]);
                                }
                              }
                              translate([-4.5, 174, 13]) {
                                zcyl(r = 1.7, l = 14);
                              }
                            }
                            translate([-6.25, 174, 16]) {
                              cuboid([9.5, 5.9, 2.8]);
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
              translate([0, 26.4, 33]) {
                cuboid([70, 19, 34]);
              }
            }
          }
          intersection() {
            difference() {
              union() {
                union() {
                  union() {
                    union() {
                      union() {
                        union() {
                          union() {
                            union() {
                              union() {
                                hull() {
                                  translate([0, -119, 10]) {
                                    cuboid([24, 0.5, 20]);
                                  }
                                  translate([0, -113, 15]) {
                                    cuboid([24, 0.5, 30]);
                                  }
                                }
                                hull() {
                                  translate([0, -113, 15]) {
                                    cuboid([24, 0.5, 30]);
                                  }
                                  translate([0, -78, 18]) {
                                    cuboid([24, 0.5, 36]);
                                  }
                                }
                              }
                              hull() {
                                translate([0, -78, 18]) {
                                  cuboid([24, 0.5, 36]);
                                }
                                translate([0, -48, 19.5]) {
                                  cuboid([18, 0.5, 39]);
                                }
                              }
                            }
                            hull() {
                              translate([0, -48, 19.5]) {
                                cuboid([18, 0.5, 39]);
                              }
                              translate([0, -20, 20.5]) {
                                cuboid([14, 0.5, 41]);
                              }
                            }
                          }
                          hull() {
                            translate([0, -20, 20.5]) {
                              cuboid([14, 0.5, 41]);
                            }
                            translate([0, 124, 17.293]) {
                              cuboid([14, 0.5, 34.586]);
                            }
                          }
                        }
                        hull() {
                          translate([0, 124, 17.293]) {
                            cuboid([14, 0.5, 34.586]);
                          }
                          translate([0, 150, 13]) {
                            cuboid([11.5, 0.5, 26]);
                          }
                        }
                      }
                      hull() {
                        translate([0, 150, 13]) {
                          cuboid([11.5, 0.5, 26]);
                        }
                        translate([0, 159, 7]) {
                          cuboid([9, 0.5, 14]);
                        }
                      }
                    }
                    hull() {
                      translate([0, 159, 7]) {
                        cuboid([9, 0.5, 14]);
                      }
                      translate([0, 204, 7]) {
                        cuboid([9, 0.5, 14]);
                      }
                    }
                  }
                  union() {
                    translate([0, 0, 44]) {
                      rotate([-2.5, 0, 0]) {
                        rotate([0, -6, 0]) {
                          difference() {
                            hull() {
                              translate([12.5, 58.5, -3.5]) {
                                cuboid([31, 111, 7]);
                              }
                              translate([2, 58.5, -17.5]) {
                                cuboid([10, 111, 21]);
                              }
                            }
                            union() {
                              union() {
                                union() {
                                  translate([15, 26.4, 0]) {
                                    zcyl(r = 2.75, l = 30);
                                  }
                                  translate([15, 26.4, -43]) {
                                    cylinder(h = 40, r = 4.792, $fn = 6);
                                  }
                                }
                                translate([15, 74.4, 0]) {
                                  zcyl(r = 2.75, l = 30);
                                }
                              }
                              translate([15, 74.4, -43]) {
                                cylinder(h = 40, r = 4.792, $fn = 6);
                              }
                            }
                          }
                        }
                      }
                    }
                    mirror([1, 0, 0]) {
                      translate([0, 0, 44]) {
                        rotate([-2.5, 0, 0]) {
                          rotate([0, -6, 0]) {
                            difference() {
                              hull() {
                                translate([12.5, 58.5, -3.5]) {
                                  cuboid([31, 111, 7]);
                                }
                                translate([2, 58.5, -17.5]) {
                                  cuboid([10, 111, 21]);
                                }
                              }
                              union() {
                                union() {
                                  union() {
                                    translate([15, 26.4, 0]) {
                                      zcyl(r = 2.75, l = 30);
                                    }
                                    translate([15, 26.4, -43]) {
                                      cylinder(h = 40, r = 4.792, $fn = 6);
                                    }
                                  }
                                  translate([15, 74.4, 0]) {
                                    zcyl(r = 2.75, l = 30);
                                  }
                                }
                                translate([15, 74.4, -43]) {
                                  cylinder(h = 40, r = 4.792, $fn = 6);
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
                translate([0, 184, 0]) {
                  translate([0, 0, 19]) {
                    rotate([5.5, 0, 0]) {
                      translate([0, -184, 0]) {
                        translate([0, 0, -19]) {
                          difference() {
                            hull() {
                              translate([0, 184, 16]) {
                                cuboid([18, 40, 6]);
                              }
                              translate([0, 184, 10.75]) {
                                cuboid([9, 40, 4.5]);
                              }
                            }
                            union() {
                              union() {
                                union() {
                                  translate([4.5, 194, 13]) {
                                    zcyl(r = 1.7, l = 14);
                                  }
                                  translate([6.25, 194, 16]) {
                                    cuboid([9.5, 5.9, 2.8]);
                                  }
                                }
                                translate([-4.5, 174, 13]) {
                                  zcyl(r = 1.7, l = 14);
                                }
                              }
                              translate([-6.25, 174, 16]) {
                                cuboid([9.5, 5.9, 2.8]);
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
              translate([0, 184, 0]) {
                translate([0, 0, 19]) {
                  rotate([5.5, 0, 0]) {
                    translate([0, -184, 0]) {
                      translate([0, 0, -19]) {
                        union() {
                          union() {
                            union() {
                              translate([4.5, 194, 13]) {
                                zcyl(r = 1.7, l = 14);
                              }
                              translate([6.25, 194, 16]) {
                                cuboid([9.5, 5.9, 2.8]);
                              }
                            }
                            translate([-4.5, 174, 13]) {
                              zcyl(r = 1.7, l = 14);
                            }
                          }
                          translate([-6.25, 174, 16]) {
                            cuboid([9.5, 5.9, 2.8]);
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
            translate([0, 74.4, 33]) {
              cuboid([70, 19, 34]);
            }
          }
        }
        intersection() {
          difference() {
            union() {
              union() {
                union() {
                  union() {
                    union() {
                      union() {
                        union() {
                          union() {
                            union() {
                              hull() {
                                translate([0, -119, 10]) {
                                  cuboid([24, 0.5, 20]);
                                }
                                translate([0, -113, 15]) {
                                  cuboid([24, 0.5, 30]);
                                }
                              }
                              hull() {
                                translate([0, -113, 15]) {
                                  cuboid([24, 0.5, 30]);
                                }
                                translate([0, -78, 18]) {
                                  cuboid([24, 0.5, 36]);
                                }
                              }
                            }
                            hull() {
                              translate([0, -78, 18]) {
                                cuboid([24, 0.5, 36]);
                              }
                              translate([0, -48, 19.5]) {
                                cuboid([18, 0.5, 39]);
                              }
                            }
                          }
                          hull() {
                            translate([0, -48, 19.5]) {
                              cuboid([18, 0.5, 39]);
                            }
                            translate([0, -20, 20.5]) {
                              cuboid([14, 0.5, 41]);
                            }
                          }
                        }
                        hull() {
                          translate([0, -20, 20.5]) {
                            cuboid([14, 0.5, 41]);
                          }
                          translate([0, 124, 17.293]) {
                            cuboid([14, 0.5, 34.586]);
                          }
                        }
                      }
                      hull() {
                        translate([0, 124, 17.293]) {
                          cuboid([14, 0.5, 34.586]);
                        }
                        translate([0, 150, 13]) {
                          cuboid([11.5, 0.5, 26]);
                        }
                      }
                    }
                    hull() {
                      translate([0, 150, 13]) {
                        cuboid([11.5, 0.5, 26]);
                      }
                      translate([0, 159, 7]) {
                        cuboid([9, 0.5, 14]);
                      }
                    }
                  }
                  hull() {
                    translate([0, 159, 7]) {
                      cuboid([9, 0.5, 14]);
                    }
                    translate([0, 204, 7]) {
                      cuboid([9, 0.5, 14]);
                    }
                  }
                }
                union() {
                  translate([0, 0, 44]) {
                    rotate([-2.5, 0, 0]) {
                      rotate([0, -6, 0]) {
                        difference() {
                          hull() {
                            translate([12.5, 58.5, -3.5]) {
                              cuboid([31, 111, 7]);
                            }
                            translate([2, 58.5, -17.5]) {
                              cuboid([10, 111, 21]);
                            }
                          }
                          union() {
                            union() {
                              union() {
                                translate([15, 26.4, 0]) {
                                  zcyl(r = 2.75, l = 30);
                                }
                                translate([15, 26.4, -43]) {
                                  cylinder(h = 40, r = 4.792, $fn = 6);
                                }
                              }
                              translate([15, 74.4, 0]) {
                                zcyl(r = 2.75, l = 30);
                              }
                            }
                            translate([15, 74.4, -43]) {
                              cylinder(h = 40, r = 4.792, $fn = 6);
                            }
                          }
                        }
                      }
                    }
                  }
                  mirror([1, 0, 0]) {
                    translate([0, 0, 44]) {
                      rotate([-2.5, 0, 0]) {
                        rotate([0, -6, 0]) {
                          difference() {
                            hull() {
                              translate([12.5, 58.5, -3.5]) {
                                cuboid([31, 111, 7]);
                              }
                              translate([2, 58.5, -17.5]) {
                                cuboid([10, 111, 21]);
                              }
                            }
                            union() {
                              union() {
                                union() {
                                  translate([15, 26.4, 0]) {
                                    zcyl(r = 2.75, l = 30);
                                  }
                                  translate([15, 26.4, -43]) {
                                    cylinder(h = 40, r = 4.792, $fn = 6);
                                  }
                                }
                                translate([15, 74.4, 0]) {
                                  zcyl(r = 2.75, l = 30);
                                }
                              }
                              translate([15, 74.4, -43]) {
                                cylinder(h = 40, r = 4.792, $fn = 6);
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
              translate([0, 184, 0]) {
                translate([0, 0, 19]) {
                  rotate([5.5, 0, 0]) {
                    translate([0, -184, 0]) {
                      translate([0, 0, -19]) {
                        difference() {
                          hull() {
                            translate([0, 184, 16]) {
                              cuboid([18, 40, 6]);
                            }
                            translate([0, 184, 10.75]) {
                              cuboid([9, 40, 4.5]);
                            }
                          }
                          union() {
                            union() {
                              union() {
                                translate([4.5, 194, 13]) {
                                  zcyl(r = 1.7, l = 14);
                                }
                                translate([6.25, 194, 16]) {
                                  cuboid([9.5, 5.9, 2.8]);
                                }
                              }
                              translate([-4.5, 174, 13]) {
                                zcyl(r = 1.7, l = 14);
                              }
                            }
                            translate([-6.25, 174, 16]) {
                              cuboid([9.5, 5.9, 2.8]);
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
            translate([0, 184, 0]) {
              translate([0, 0, 19]) {
                rotate([5.5, 0, 0]) {
                  translate([0, -184, 0]) {
                    translate([0, 0, -19]) {
                      union() {
                        union() {
                          union() {
                            translate([4.5, 194, 13]) {
                              zcyl(r = 1.7, l = 14);
                            }
                            translate([6.25, 194, 16]) {
                              cuboid([9.5, 5.9, 2.8]);
                            }
                          }
                          translate([-4.5, 174, 13]) {
                            zcyl(r = 1.7, l = 14);
                          }
                        }
                        translate([-6.25, 174, 16]) {
                          cuboid([9.5, 5.9, 2.8]);
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          translate([0, 184.5, 17.5]) {
            cuboid([30, 41, 35]);
          }
        }
      }
      difference() {
        rotate([-90, 0, 0]) {
          skin([ccw_polygon([[-3.933, -0.85], [3.933, -0.85], [7.15, -4.067], [7.15, -25.932], [3.933, -29.15], [-3.933, -29.15], [-7.15, -25.932], [-7.15, -4.067]]), ccw_polygon([[-3.933, -0.85], [3.933, -0.85], [7.15, -4.067], [7.15, -25.932], [3.933, -29.15], [-3.933, -29.15], [-7.15, -25.932], [-7.15, -4.067]]), ccw_polygon([[-3.933, -0.85], [3.933, -0.85], [7.15, -4.067], [7.15, -25.932], [3.933, -29.15], [-3.933, -29.15], [-7.15, -25.932], [-7.15, -4.067]]), ccw_polygon([[-2.283, -0.85], [2.283, -0.85], [4.15, -2.718], [4.15, -10.283], [2.283, -12.15], [-2.283, -12.15], [-4.15, -10.283], [-4.15, -2.718]]), ccw_polygon([[-2.283, -0.85], [2.283, -0.85], [4.15, -2.718], [4.15, -10.283], [2.283, -12.15], [-2.283, -12.15], [-4.15, -10.283], [-4.15, -2.718]])], z = [-40, -32, 82, 158, 200], slices = 0, method = "reindex");
        }
        intersection() {
          rotate([-90, 0, 0]) {
            skin([ccw_polygon([[-4.4, 0], [4.4, 0], [8, -3.6], [8, -26.4], [4.4, -30], [-4.4, -30], [-8, -26.4], [-8, -3.6]]), ccw_polygon([[-4.4, 0], [4.4, 0], [8, -3.6], [8, -26.4], [4.4, -30], [-4.4, -30], [-8, -26.4], [-8, -3.6]]), ccw_polygon([[-4.4, 0], [4.4, 0], [8, -3.6], [8, -26.4], [4.4, -30], [-4.4, -30], [-8, -26.4], [-8, -3.6]]), ccw_polygon([[-2.75, 0], [2.75, 0], [5, -2.25], [5, -10.75], [2.75, -13], [-2.75, -13], [-5, -10.75], [-5, -2.25]]), ccw_polygon([[-2.75, 0], [2.75, 0], [5, -2.25], [5, -10.75], [2.75, -13], [-2.75, -13], [-5, -10.75], [-5, -2.25]])], z = [-44, -32, 82, 158, 201], slices = 0, method = "reindex");
          }
          union() {
            union() {
              translate([0, 26.4, 20]) {
                cuboid([36, 1.6, 60]);
              }
              translate([0, 74.4, 20]) {
                cuboid([36, 1.6, 60]);
              }
            }
            translate([0, 154, 20]) {
              cuboid([36, 1.6, 60]);
            }
          }
        }
      }
    }
    translate([0, -53, 18]) {
      cuboid([12.4, 24.4, 24.4]);
    }
  }
  union() {
    translate([0, -53, 12]) {
      xcyl(r = 1.7, l = 60);
    }
    translate([0, -53, 24]) {
      xcyl(r = 1.7, l = 60);
    }
  }
}
$fn = 50;