include <BOSL2/std.scad>

difference() {
  difference() {
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
        translate([0, -165, 30]) {
          cuboid([100, 200, 100]);
        }
      }
      translate([0, -53, 18]) {
        cuboid([12, 24, 24]);
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
  union() {
    union() {
      hull() {
        translate([0, -112.3, 15.06]) {
          cuboid([22.2, 2.0e-2, 28.12]);
        }
        translate([0, -95.7, 16.4829]) {
          cuboid([22.2, 2.0e-2, 30.9657]);
        }
      }
      hull() {
        translate([0, -94.3, 16.6029]) {
          cuboid([22.2, 2.0e-2, 31.2057]);
        }
        translate([0, -78.7, 17.94]) {
          cuboid([22.2, 2.0e-2, 33.88]);
        }
      }
    }
    hull() {
      translate([0, -77.3, 18.035]) {
        cuboid([22.06, 2.0e-2, 34.07]);
      }
      translate([0, -67.7, 18.515]) {
        cuboid([20.14, 2.0e-2, 35.03]);
      }
    }
  }
}
$fn = 50;