include <BOSL2/std.scad>

translate([0, 0, 5]) {
  union() {
    union() {
      union() {
        difference() {
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
                translate([0, 159, 9]) {
                  cuboid([9, 0.5, 18]);
                }
              }
            }
            hull() {
              translate([0, 159, 9]) {
                cuboid([9, 0.5, 18]);
              }
              translate([0, 204, 9]) {
                cuboid([9, 0.5, 18]);
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
                        hull() {
                          translate([0, -74.2, 18.19]) {
                            cuboid([21.24, 0.1, 34.38]);
                          }
                          translate([0, -48.8, 19.46]) {
                            cuboid([16.16, 0.1, 36.92]);
                          }
                        }
                        hull() {
                          translate([0, -47.2, 19.5286]) {
                            cuboid([15.8857, 0.1, 37.0571]);
                          }
                          translate([0, -20.8, 20.4714]) {
                            cuboid([12.1143, 0.1, 38.9429]);
                          }
                        }
                      }
                      hull() {
                        translate([0, -19.2, 20.4822]) {
                          cuboid([12, 0.1, 38.9644]);
                        }
                        translate([0, 14.2, 19.7383]) {
                          cuboid([12, 0.1, 37.4767]);
                        }
                      }
                    }
                    hull() {
                      translate([0, 15.8, 19.7027]) {
                        cuboid([12, 0.1, 37.4054]);
                      }
                      translate([0, 49.2, 18.9589]) {
                        cuboid([12, 0.1, 35.9177]);
                      }
                    }
                  }
                  hull() {
                    translate([0, 50.8, 18.9232]) {
                      cuboid([12, 0.1, 35.8465]);
                    }
                    translate([0, 84.2, 18.1794]) {
                      cuboid([12, 0.1, 34.3588]);
                    }
                  }
                }
                hull() {
                  translate([0, 85.8, 18.1438]) {
                    cuboid([12, 0.1, 34.2875]);
                  }
                  translate([0, 123.2, 17.3108]) {
                    cuboid([12, 0.1, 32.6217]);
                  }
                }
              }
              hull() {
                translate([0, 124.8, 17.1609]) {
                  cuboid([11.9231, 0.1, 32.3219]);
                }
                translate([0, 149.2, 13.1321]) {
                  cuboid([9.5769, 0.1, 24.2642]);
                }
              }
            }
            hull() {
              translate([0, 150.8, 12.6444]) {
                cuboid([9.2778, 0.1, 23.2889]);
              }
              translate([0, 158.2, 9.3556]) {
                cuboid([7.2222, 0.1, 16.7111]);
              }
            }
          }
        }
        translate([0, 24, 0]) {
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
      }
      difference() {
        hull() {
          translate([0, 184, 15]) {
            cuboid([24, 40, 8]);
          }
          translate([0, 184, 7.25]) {
            cuboid([9, 40, 7.5]);
          }
        }
        union() {
          union() {
            union() {
              translate([6.5, 197, 12]) {
                zcyl(r = 2.75, l = 16);
              }
              translate([7.85, 197, 15]) {
                cuboid([12.3, 8.4, 4.4]);
              }
            }
            translate([-6.5, 171, 12]) {
              zcyl(r = 2.75, l = 16);
            }
          }
          translate([-7.85, 171, 15]) {
            cuboid([12.3, 8.4, 4.4]);
          }
        }
      }
    }
    hull() {
      translate([0, 58, 0.6]) {
        cuboid([9, 26, 2]);
      }
      translate([0, 58, -4]) {
        cuboid([5, 18, 2]);
      }
    }
  }
}
$fn = 50;