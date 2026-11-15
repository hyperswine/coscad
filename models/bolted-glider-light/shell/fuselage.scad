include <BOSL2/std.scad>

difference() {
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
          translate([0, 135.2, 50]) {
            cuboid([150, 400, 150]);
          }
        }
        translate([0, -53, 18]) {
          cuboid([23, 18, 28]);
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
                      translate([0, -38.3, 19.8464]) {
                        cuboid([14.8143, 2.0e-2, 37.6929]);
                      }
                      translate([0, -20.7, 20.475]) {
                        cuboid([12.3, 2.0e-2, 38.95]);
                      }
                    }
                    hull() {
                      translate([0, -19.3, 20.4844]) {
                        cuboid([12.2, 2.0e-2, 38.9688]);
                      }
                      translate([0, 0.3, 20.0479]) {
                        cuboid([12.2, 2.0e-2, 38.0958]);
                      }
                    }
                  }
                  hull() {
                    translate([0, 1.7, 20.0167]) {
                      cuboid([12.2, 2.0e-2, 38.0335]);
                    }
                    translate([0, 22.3, 19.558]) {
                      cuboid([12.2, 2.0e-2, 37.1159]);
                    }
                  }
                }
                hull() {
                  translate([0, 30.7, 19.3709]) {
                    cuboid([12.2, 2.0e-2, 36.7418]);
                  }
                  translate([0, 51.3, 18.9121]) {
                    cuboid([12.2, 2.0e-2, 35.8242]);
                  }
                }
              }
              hull() {
                translate([0, 52.7, 18.8809]) {
                  cuboid([12.2, 2.0e-2, 35.7618]);
                }
                translate([0, 70.3, 18.489]) {
                  cuboid([12.2, 2.0e-2, 34.9779]);
                }
              }
            }
            hull() {
              translate([0, 79.7, 18.2796]) {
                cuboid([12.2, 2.0e-2, 34.5592]);
              }
              translate([0, 99.3, 17.8431]) {
                cuboid([12.2, 2.0e-2, 33.6862]);
              }
            }
          }
          hull() {
            translate([0, 100.7, 17.8119]) {
              cuboid([12.2, 2.0e-2, 33.6239]);
            }
            translate([0, 123.3, 17.3086]) {
              cuboid([12.2, 2.0e-2, 32.6172]);
            }
          }
        }
        hull() {
          translate([0, 124.7, 17.1774]) {
            cuboid([12.1327, 2.0e-2, 32.3549]);
          }
          translate([0, 149.3, 13.1156]) {
            cuboid([9.7673, 2.0e-2, 24.2312]);
          }
        }
      }
      hull() {
        translate([0, 150.7, 12.5333]) {
          cuboid([9.5056, 2.0e-2, 23.0667]);
        }
        translate([0, 158.3, 7.4667]) {
          cuboid([7.3944, 2.0e-2, 12.9333]);
        }
      }
    }
    hull() {
      translate([0, 159.7, 7]) {
        cuboid([7.2, 2.0e-2, 12]);
      }
      translate([0, 168.3, 7]) {
        cuboid([7.2, 2.0e-2, 12]);
      }
    }
  }
}
$fn = 50;