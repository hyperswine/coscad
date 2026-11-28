include <BOSL2/std.scad>

translate([-66.4, 0, 40]) {
  rotate([90, 0, 0]) {
    union() {
      union() {
        union() {
          union() {
            union() {
              union() {
                union() {
                  union() {
                    translate([66.4, -22.5, 4.25]) {
                      cuboid([173.8, 1.4, 8.5]);
                    }
                    difference() {
                      difference() {
                        union() {
                          union() {
                            union() {
                              union() {
                                translate([66.4, 0, 1.5]) {
                                  cuboid([173.8, 44, 3]);
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
                  }
                  union() {
                    translate([21, 12.5, -10.425]) {
                      cuboid([12, 1.8, 21.45]);
                    }
                    hull() {
                      translate([21, 11.95, -21.55]) {
                        cuboid([12, 2.9, 0.8]);
                      }
                      translate([21, 13, -23.75]) {
                        cuboid([12, 0.6, 0.3]);
                      }
                    }
                  }
                }
                translate([42, 0, 0]) {
                  rotate([0, 0, 180]) {
                    union() {
                      translate([21, 12.5, -10.425]) {
                        cuboid([12, 1.8, 21.45]);
                      }
                      hull() {
                        translate([21, 11.95, -21.55]) {
                          cuboid([12, 2.9, 0.8]);
                        }
                        translate([21, 13, -23.75]) {
                          cuboid([12, 0.6, 0.3]);
                        }
                      }
                    }
                  }
                }
              }
              translate([8, 12.8, -4.3]) {
                cuboid([31, 2.4, 9]);
              }
            }
            translate([0, -25.6, 0]) {
              translate([8, 12.8, -4.3]) {
                cuboid([31, 2.4, 9]);
              }
            }
          }
          translate([8, 8.6, -0.85]) {
            cuboid([30, 2.8, 2.3]);
          }
        }
        translate([0, -17.2, 0]) {
          translate([8, 8.6, -0.85]) {
            cuboid([30, 2.8, 2.3]);
          }
        }
      }
      union() {
        union() {
          translate([-2, 0, 0]) {
            union() {
              hull() {
                translate([0, -22.5, 4.25]) {
                  cuboid([14, 2.4, 8.5]);
                }
                translate([0, -30, 4]) {
                  cuboid([12, 2, 10]);
                }
              }
              hull() {
                translate([0, -30, 4]) {
                  cuboid([12, 2, 10]);
                }
                translate([0, -38.5, 0]) {
                  cuboid([16, 3, 54]);
                }
              }
            }
          }
          translate([64, 0, 0]) {
            union() {
              hull() {
                translate([0, -22.5, 4.25]) {
                  cuboid([14, 2.4, 8.5]);
                }
                translate([0, -30, 4]) {
                  cuboid([12, 2, 10]);
                }
              }
              hull() {
                translate([0, -30, 4]) {
                  cuboid([12, 2, 10]);
                }
                translate([0, -38.5, 0]) {
                  cuboid([16, 3, 54]);
                }
              }
            }
          }
        }
        translate([120, 0, 0]) {
          union() {
            hull() {
              translate([0, -22.5, 4.25]) {
                cuboid([14, 2.4, 8.5]);
              }
              translate([0, -30, 4]) {
                cuboid([12, 2, 10]);
              }
            }
            hull() {
              translate([0, -30, 4]) {
                cuboid([12, 2, 10]);
              }
              translate([0, -38.5, 0]) {
                cuboid([16, 3, 54]);
              }
            }
          }
        }
      }
    }
  }
}
$fn = 50;