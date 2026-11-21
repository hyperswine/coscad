include <BOSL2/std.scad>

scale([0.526, 0.526, 0.526]) {
  union() {
    union() {
      union() {
        union() {
          difference() {
            union() {
              union() {
                union() {
                  hull() {
                    translate([0, -128, 0]) {
                      sphere(2.5);
                    }
                    translate([0, -95, 0]) {
                      sphere(9);
                    }
                  }
                  hull() {
                    translate([0, -95, 0]) {
                      sphere(9);
                    }
                    translate([0, -15, 0]) {
                      sphere(7);
                    }
                  }
                }
                hull() {
                  translate([0, -15, 0]) {
                    sphere(7);
                  }
                  translate([0, 70, 0]) {
                    sphere(3.5);
                  }
                }
              }
              hull() {
                translate([0, 70, 0]) {
                  sphere(3.5);
                }
                translate([0, 126, 0]) {
                  sphere(2.5);
                }
              }
            }
            union() {
              union() {
                hull() {
                  translate([0, -76, 0]) {
                    sphere(5.6);
                  }
                  translate([0, -39, 0]) {
                    sphere(4.9);
                  }
                }
                hull() {
                  translate([0, -23, 0]) {
                    sphere(4.6);
                  }
                  translate([0, 36, 0]) {
                    sphere(2.6);
                  }
                }
              }
              hull() {
                translate([0, 46, 0]) {
                  sphere(2.2);
                }
                translate([0, 94, 0]) {
                  sphere(1.2);
                }
              }
            }
          }
          translate([0, 4, 0]) {
            union() {
              difference() {
                hull() {
                  hull() {
                    translate([3, 0, 0]) {
                      translate([0, -40, 0]) {
                        translate([0, 0, -4]) {
                          zcyl(r = 1.5, l = 3);
                        }
                      }
                    }
                    translate([3, 0, 0]) {
                      translate([0, 45, 0]) {
                        translate([0, 0, -4]) {
                          zcyl(r = 1.5, l = 3);
                        }
                      }
                    }
                  }
                  translate([113, 0, 0]) {
                    translate([0, 45, 0]) {
                      translate([0, 0, 7]) {
                        zcyl(r = 1.5, l = 3);
                      }
                    }
                  }
                }
                union() {
                  union() {
                    hull() {
                      hull() {
                        translate([12, -27, -3.1]) {
                          zcyl(r = 0.45, l = 1.25);
                        }
                        translate([12, 44, -3.1]) {
                          zcyl(r = 0.45, l = 1.25);
                        }
                      }
                      translate([46, 44, 0.3]) {
                        zcyl(r = 0.45, l = 1.25);
                      }
                    }
                    hull() {
                      hull() {
                        translate([49, 4, 0.6]) {
                          zcyl(r = 0.45, l = 1.25);
                        }
                        translate([49, 44, 0.6]) {
                          zcyl(r = 0.45, l = 1.25);
                        }
                      }
                      translate([76, 44, 3.3]) {
                        zcyl(r = 0.45, l = 1.25);
                      }
                    }
                  }
                  hull() {
                    hull() {
                      translate([79, 26, 3.6]) {
                        zcyl(r = 0.45, l = 1.25);
                      }
                      translate([79, 44, 3.6]) {
                        zcyl(r = 0.45, l = 1.25);
                      }
                    }
                    translate([102, 44, 5.9]) {
                      zcyl(r = 0.45, l = 1.25);
                    }
                  }
                }
              }
              mirror([1, 0, 0]) {
                difference() {
                  hull() {
                    hull() {
                      translate([3, 0, 0]) {
                        translate([0, -40, 0]) {
                          translate([0, 0, -4]) {
                            zcyl(r = 1.5, l = 3);
                          }
                        }
                      }
                      translate([3, 0, 0]) {
                        translate([0, 45, 0]) {
                          translate([0, 0, -4]) {
                            zcyl(r = 1.5, l = 3);
                          }
                        }
                      }
                    }
                    translate([113, 0, 0]) {
                      translate([0, 45, 0]) {
                        translate([0, 0, 7]) {
                          zcyl(r = 1.5, l = 3);
                        }
                      }
                    }
                  }
                  union() {
                    union() {
                      hull() {
                        hull() {
                          translate([12, -27, -3.1]) {
                            zcyl(r = 0.45, l = 1.25);
                          }
                          translate([12, 44, -3.1]) {
                            zcyl(r = 0.45, l = 1.25);
                          }
                        }
                        translate([46, 44, 0.3]) {
                          zcyl(r = 0.45, l = 1.25);
                        }
                      }
                      hull() {
                        hull() {
                          translate([49, 4, 0.6]) {
                            zcyl(r = 0.45, l = 1.25);
                          }
                          translate([49, 44, 0.6]) {
                            zcyl(r = 0.45, l = 1.25);
                          }
                        }
                        translate([76, 44, 3.3]) {
                          zcyl(r = 0.45, l = 1.25);
                        }
                      }
                    }
                    hull() {
                      hull() {
                        translate([79, 26, 3.6]) {
                          zcyl(r = 0.45, l = 1.25);
                        }
                        translate([79, 44, 3.6]) {
                          zcyl(r = 0.45, l = 1.25);
                        }
                      }
                      translate([102, 44, 5.9]) {
                        zcyl(r = 0.45, l = 1.25);
                      }
                    }
                  }
                }
              }
            }
          }
        }
        translate([0, 114, 0]) {
          rotate([4, 0, 0]) {
            translate([0, -114, 0]) {
              translate([0, 114, 0]) {
                cuboid([96, 26, 2.5]);
              }
            }
          }
        }
      }
      hull() {
        hull() {
          translate([0, 92, 0]) {
            translate([0, 0, 2]) {
              xcyl(r = 1.25, l = 2.5);
            }
          }
          translate([0, 127, 0]) {
            translate([0, 0, 2]) {
              xcyl(r = 1.25, l = 2.5);
            }
          }
        }
        translate([0, 127, 0]) {
          translate([0, 0, 38]) {
            xcyl(r = 1.25, l = 2.5);
          }
        }
      }
    }
    hull() {
      translate([0, 11, -5]) {
        cuboid([8, 21, 2]);
      }
      translate([0, 11, -12]) {
        cuboid([5, 15, 2]);
      }
    }
  }
}
$fn = 50;