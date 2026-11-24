include <BOSL2/std.scad>

union() {
  union() {
    union() {
      union() {
        difference() {
          union() {
            union() {
              union() {
                union() {
                  translate([0, 0, 81]) {
                    zcyl(r = 4, l = 162);
                  }
                  translate([0, 0, 44]) {
                    zcyl(r = 5, l = 12);
                  }
                }
                translate([0, 0, 82]) {
                  zcyl(r = 5, l = 12);
                }
              }
              hull() {
                translate([0, 0, 146.01]) {
                  zcyl(r = 4, l = 2.0e-2);
                }
                translate([0, 0, 148.51]) {
                  zcyl(r = 6.5, l = 2.0e-2);
                }
              }
            }
            translate([0, 0, 155.25]) {
              zcyl(r = 6.5, l = 13.5);
            }
          }
          union() {
            translate([0, 0, 157.5]) {
              zcyl(r = 2.475, l = 9.2);
            }
            hull() {
              translate([0, 0, 162.05]) {
                zcyl(r = 2.875, l = 2.0e-2);
              }
              translate([0, 0, 161.55]) {
                zcyl(r = 2.475, l = 2.0e-2);
              }
            }
          }
        }
        translate([0, 0, 34]) {
          mirror([0, 0, 1]) {
            difference() {
              union() {
                union() {
                  union() {
                    union() {
                      union() {
                        union() {
                          union() {
                            union() {
                              hull() {
                                translate([0, 0, 12]) {
                                  zcyl(r = 7, l = 24);
                                }
                                translate([0, 0, 9.656]) {
                                  zcyl(r = 9.875, l = 19.312);
                                }
                              }
                              hull() {
                                translate([0, 0, 9.656]) {
                                  zcyl(r = 9.875, l = 19.312);
                                }
                                translate([0, 0, 7.625]) {
                                  zcyl(r = 12.75, l = 15.25);
                                }
                              }
                            }
                            hull() {
                              translate([0, 0, 7.625]) {
                                zcyl(r = 12.75, l = 15.25);
                              }
                              translate([0, 0, 5.906]) {
                                zcyl(r = 15.625, l = 11.812);
                              }
                            }
                          }
                          hull() {
                            translate([0, 0, 5.906]) {
                              zcyl(r = 15.625, l = 11.812);
                            }
                            translate([0, 0, 4.5]) {
                              zcyl(r = 18.5, l = 9);
                            }
                          }
                        }
                        hull() {
                          translate([0, 0, 4.5]) {
                            zcyl(r = 18.5, l = 9);
                          }
                          translate([0, 0, 3.406]) {
                            zcyl(r = 21.375, l = 6.812);
                          }
                        }
                      }
                      hull() {
                        translate([0, 0, 3.406]) {
                          zcyl(r = 21.375, l = 6.812);
                        }
                        translate([0, 0, 2.625]) {
                          zcyl(r = 24.25, l = 5.25);
                        }
                      }
                    }
                    hull() {
                      translate([0, 0, 2.625]) {
                        zcyl(r = 24.25, l = 5.25);
                      }
                      translate([0, 0, 2.156]) {
                        zcyl(r = 27.125, l = 4.312);
                      }
                    }
                  }
                  hull() {
                    translate([0, 0, 2.156]) {
                      zcyl(r = 27.125, l = 4.312);
                    }
                    translate([0, 0, 2]) {
                      zcyl(r = 30, l = 4);
                    }
                  }
                }
                union() {
                  union() {
                    union() {
                      union() {
                        union() {
                          union() {
                            rotate([0, 0, 0]) {
                              union() {
                                union() {
                                  union() {
                                    union() {
                                      union() {
                                        union() {
                                          union() {
                                            hull() {
                                              rotate([0, 0, 0.47]) {
                                                translate([8, 0, 0]) {
                                                  translate([0, 0, 22.968]) {
                                                    cuboid([2.2, 1.6, 3.339]);
                                                  }
                                                }
                                              }
                                              rotate([0, 0, 3]) {
                                                translate([10.75, 0, 0]) {
                                                  translate([0, 0, 21.582]) {
                                                    cuboid([2.2, 1.6, 9.144]);
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              rotate([0, 0, 3]) {
                                                translate([10.75, 0, 0]) {
                                                  translate([0, 0, 21.582]) {
                                                    cuboid([2.2, 1.6, 9.144]);
                                                  }
                                                }
                                              }
                                              rotate([0, 0, 6.48]) {
                                                translate([13.5, 0, 0]) {
                                                  translate([0, 0, 18.804]) {
                                                    cuboid([2.2, 1.6, 11.022]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            rotate([0, 0, 6.48]) {
                                              translate([13.5, 0, 0]) {
                                                translate([0, 0, 18.804]) {
                                                  cuboid([2.2, 1.6, 11.022]);
                                                }
                                              }
                                            }
                                            rotate([0, 0, 10.62]) {
                                              translate([16.25, 0, 0]) {
                                                translate([0, 0, 15.24]) {
                                                  cuboid([2.2, 1.6, 10.185]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          rotate([0, 0, 10.62]) {
                                            translate([16.25, 0, 0]) {
                                              translate([0, 0, 15.24]) {
                                                cuboid([2.2, 1.6, 10.185]);
                                              }
                                            }
                                          }
                                          rotate([0, 0, 15.28]) {
                                            translate([19, 0, 0]) {
                                              translate([0, 0, 12.249]) {
                                                cuboid([2.2, 1.6, 9.348]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        rotate([0, 0, 15.28]) {
                                          translate([19, 0, 0]) {
                                            translate([0, 0, 12.249]) {
                                              cuboid([2.2, 1.6, 9.348]);
                                            }
                                          }
                                        }
                                        rotate([0, 0, 20.4]) {
                                          translate([21.75, 0, 0]) {
                                            translate([0, 0, 9.829]) {
                                              cuboid([2.2, 1.6, 8.511]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      rotate([0, 0, 20.4]) {
                                        translate([21.75, 0, 0]) {
                                          translate([0, 0, 9.829]) {
                                            cuboid([2.2, 1.6, 8.511]);
                                          }
                                        }
                                      }
                                      rotate([0, 0, 25.92]) {
                                        translate([24.5, 0, 0]) {
                                          translate([0, 0, 7.981]) {
                                            cuboid([2.2, 1.6, 7.674]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    rotate([0, 0, 25.92]) {
                                      translate([24.5, 0, 0]) {
                                        translate([0, 0, 7.981]) {
                                          cuboid([2.2, 1.6, 7.674]);
                                        }
                                      }
                                    }
                                    rotate([0, 0, 31.8]) {
                                      translate([27.25, 0, 0]) {
                                        translate([0, 0, 6.704]) {
                                          cuboid([2.2, 1.6, 6.837]);
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  rotate([0, 0, 31.8]) {
                                    translate([27.25, 0, 0]) {
                                      translate([0, 0, 6.704]) {
                                        cuboid([2.2, 1.6, 6.837]);
                                      }
                                    }
                                  }
                                  rotate([0, 0, 38]) {
                                    translate([30, 0, 0]) {
                                      translate([0, 0, 6]) {
                                        cuboid([2.2, 1.6, 6]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            rotate([0, 0, 51.429]) {
                              union() {
                                union() {
                                  union() {
                                    union() {
                                      union() {
                                        union() {
                                          union() {
                                            hull() {
                                              rotate([0, 0, 0.47]) {
                                                translate([8, 0, 0]) {
                                                  translate([0, 0, 22.968]) {
                                                    cuboid([2.2, 1.6, 3.339]);
                                                  }
                                                }
                                              }
                                              rotate([0, 0, 3]) {
                                                translate([10.75, 0, 0]) {
                                                  translate([0, 0, 21.582]) {
                                                    cuboid([2.2, 1.6, 9.144]);
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              rotate([0, 0, 3]) {
                                                translate([10.75, 0, 0]) {
                                                  translate([0, 0, 21.582]) {
                                                    cuboid([2.2, 1.6, 9.144]);
                                                  }
                                                }
                                              }
                                              rotate([0, 0, 6.48]) {
                                                translate([13.5, 0, 0]) {
                                                  translate([0, 0, 18.804]) {
                                                    cuboid([2.2, 1.6, 11.022]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            rotate([0, 0, 6.48]) {
                                              translate([13.5, 0, 0]) {
                                                translate([0, 0, 18.804]) {
                                                  cuboid([2.2, 1.6, 11.022]);
                                                }
                                              }
                                            }
                                            rotate([0, 0, 10.62]) {
                                              translate([16.25, 0, 0]) {
                                                translate([0, 0, 15.24]) {
                                                  cuboid([2.2, 1.6, 10.185]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          rotate([0, 0, 10.62]) {
                                            translate([16.25, 0, 0]) {
                                              translate([0, 0, 15.24]) {
                                                cuboid([2.2, 1.6, 10.185]);
                                              }
                                            }
                                          }
                                          rotate([0, 0, 15.28]) {
                                            translate([19, 0, 0]) {
                                              translate([0, 0, 12.249]) {
                                                cuboid([2.2, 1.6, 9.348]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        rotate([0, 0, 15.28]) {
                                          translate([19, 0, 0]) {
                                            translate([0, 0, 12.249]) {
                                              cuboid([2.2, 1.6, 9.348]);
                                            }
                                          }
                                        }
                                        rotate([0, 0, 20.4]) {
                                          translate([21.75, 0, 0]) {
                                            translate([0, 0, 9.829]) {
                                              cuboid([2.2, 1.6, 8.511]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      rotate([0, 0, 20.4]) {
                                        translate([21.75, 0, 0]) {
                                          translate([0, 0, 9.829]) {
                                            cuboid([2.2, 1.6, 8.511]);
                                          }
                                        }
                                      }
                                      rotate([0, 0, 25.92]) {
                                        translate([24.5, 0, 0]) {
                                          translate([0, 0, 7.981]) {
                                            cuboid([2.2, 1.6, 7.674]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    rotate([0, 0, 25.92]) {
                                      translate([24.5, 0, 0]) {
                                        translate([0, 0, 7.981]) {
                                          cuboid([2.2, 1.6, 7.674]);
                                        }
                                      }
                                    }
                                    rotate([0, 0, 31.8]) {
                                      translate([27.25, 0, 0]) {
                                        translate([0, 0, 6.704]) {
                                          cuboid([2.2, 1.6, 6.837]);
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  rotate([0, 0, 31.8]) {
                                    translate([27.25, 0, 0]) {
                                      translate([0, 0, 6.704]) {
                                        cuboid([2.2, 1.6, 6.837]);
                                      }
                                    }
                                  }
                                  rotate([0, 0, 38]) {
                                    translate([30, 0, 0]) {
                                      translate([0, 0, 6]) {
                                        cuboid([2.2, 1.6, 6]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                          rotate([0, 0, 102.857]) {
                            union() {
                              union() {
                                union() {
                                  union() {
                                    union() {
                                      union() {
                                        union() {
                                          hull() {
                                            rotate([0, 0, 0.47]) {
                                              translate([8, 0, 0]) {
                                                translate([0, 0, 22.968]) {
                                                  cuboid([2.2, 1.6, 3.339]);
                                                }
                                              }
                                            }
                                            rotate([0, 0, 3]) {
                                              translate([10.75, 0, 0]) {
                                                translate([0, 0, 21.582]) {
                                                  cuboid([2.2, 1.6, 9.144]);
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            rotate([0, 0, 3]) {
                                              translate([10.75, 0, 0]) {
                                                translate([0, 0, 21.582]) {
                                                  cuboid([2.2, 1.6, 9.144]);
                                                }
                                              }
                                            }
                                            rotate([0, 0, 6.48]) {
                                              translate([13.5, 0, 0]) {
                                                translate([0, 0, 18.804]) {
                                                  cuboid([2.2, 1.6, 11.022]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          rotate([0, 0, 6.48]) {
                                            translate([13.5, 0, 0]) {
                                              translate([0, 0, 18.804]) {
                                                cuboid([2.2, 1.6, 11.022]);
                                              }
                                            }
                                          }
                                          rotate([0, 0, 10.62]) {
                                            translate([16.25, 0, 0]) {
                                              translate([0, 0, 15.24]) {
                                                cuboid([2.2, 1.6, 10.185]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        rotate([0, 0, 10.62]) {
                                          translate([16.25, 0, 0]) {
                                            translate([0, 0, 15.24]) {
                                              cuboid([2.2, 1.6, 10.185]);
                                            }
                                          }
                                        }
                                        rotate([0, 0, 15.28]) {
                                          translate([19, 0, 0]) {
                                            translate([0, 0, 12.249]) {
                                              cuboid([2.2, 1.6, 9.348]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      rotate([0, 0, 15.28]) {
                                        translate([19, 0, 0]) {
                                          translate([0, 0, 12.249]) {
                                            cuboid([2.2, 1.6, 9.348]);
                                          }
                                        }
                                      }
                                      rotate([0, 0, 20.4]) {
                                        translate([21.75, 0, 0]) {
                                          translate([0, 0, 9.829]) {
                                            cuboid([2.2, 1.6, 8.511]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    rotate([0, 0, 20.4]) {
                                      translate([21.75, 0, 0]) {
                                        translate([0, 0, 9.829]) {
                                          cuboid([2.2, 1.6, 8.511]);
                                        }
                                      }
                                    }
                                    rotate([0, 0, 25.92]) {
                                      translate([24.5, 0, 0]) {
                                        translate([0, 0, 7.981]) {
                                          cuboid([2.2, 1.6, 7.674]);
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  rotate([0, 0, 25.92]) {
                                    translate([24.5, 0, 0]) {
                                      translate([0, 0, 7.981]) {
                                        cuboid([2.2, 1.6, 7.674]);
                                      }
                                    }
                                  }
                                  rotate([0, 0, 31.8]) {
                                    translate([27.25, 0, 0]) {
                                      translate([0, 0, 6.704]) {
                                        cuboid([2.2, 1.6, 6.837]);
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                rotate([0, 0, 31.8]) {
                                  translate([27.25, 0, 0]) {
                                    translate([0, 0, 6.704]) {
                                      cuboid([2.2, 1.6, 6.837]);
                                    }
                                  }
                                }
                                rotate([0, 0, 38]) {
                                  translate([30, 0, 0]) {
                                    translate([0, 0, 6]) {
                                      cuboid([2.2, 1.6, 6]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                        rotate([0, 0, 154.286]) {
                          union() {
                            union() {
                              union() {
                                union() {
                                  union() {
                                    union() {
                                      union() {
                                        hull() {
                                          rotate([0, 0, 0.47]) {
                                            translate([8, 0, 0]) {
                                              translate([0, 0, 22.968]) {
                                                cuboid([2.2, 1.6, 3.339]);
                                              }
                                            }
                                          }
                                          rotate([0, 0, 3]) {
                                            translate([10.75, 0, 0]) {
                                              translate([0, 0, 21.582]) {
                                                cuboid([2.2, 1.6, 9.144]);
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          rotate([0, 0, 3]) {
                                            translate([10.75, 0, 0]) {
                                              translate([0, 0, 21.582]) {
                                                cuboid([2.2, 1.6, 9.144]);
                                              }
                                            }
                                          }
                                          rotate([0, 0, 6.48]) {
                                            translate([13.5, 0, 0]) {
                                              translate([0, 0, 18.804]) {
                                                cuboid([2.2, 1.6, 11.022]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        rotate([0, 0, 6.48]) {
                                          translate([13.5, 0, 0]) {
                                            translate([0, 0, 18.804]) {
                                              cuboid([2.2, 1.6, 11.022]);
                                            }
                                          }
                                        }
                                        rotate([0, 0, 10.62]) {
                                          translate([16.25, 0, 0]) {
                                            translate([0, 0, 15.24]) {
                                              cuboid([2.2, 1.6, 10.185]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      rotate([0, 0, 10.62]) {
                                        translate([16.25, 0, 0]) {
                                          translate([0, 0, 15.24]) {
                                            cuboid([2.2, 1.6, 10.185]);
                                          }
                                        }
                                      }
                                      rotate([0, 0, 15.28]) {
                                        translate([19, 0, 0]) {
                                          translate([0, 0, 12.249]) {
                                            cuboid([2.2, 1.6, 9.348]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    rotate([0, 0, 15.28]) {
                                      translate([19, 0, 0]) {
                                        translate([0, 0, 12.249]) {
                                          cuboid([2.2, 1.6, 9.348]);
                                        }
                                      }
                                    }
                                    rotate([0, 0, 20.4]) {
                                      translate([21.75, 0, 0]) {
                                        translate([0, 0, 9.829]) {
                                          cuboid([2.2, 1.6, 8.511]);
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  rotate([0, 0, 20.4]) {
                                    translate([21.75, 0, 0]) {
                                      translate([0, 0, 9.829]) {
                                        cuboid([2.2, 1.6, 8.511]);
                                      }
                                    }
                                  }
                                  rotate([0, 0, 25.92]) {
                                    translate([24.5, 0, 0]) {
                                      translate([0, 0, 7.981]) {
                                        cuboid([2.2, 1.6, 7.674]);
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                rotate([0, 0, 25.92]) {
                                  translate([24.5, 0, 0]) {
                                    translate([0, 0, 7.981]) {
                                      cuboid([2.2, 1.6, 7.674]);
                                    }
                                  }
                                }
                                rotate([0, 0, 31.8]) {
                                  translate([27.25, 0, 0]) {
                                    translate([0, 0, 6.704]) {
                                      cuboid([2.2, 1.6, 6.837]);
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              rotate([0, 0, 31.8]) {
                                translate([27.25, 0, 0]) {
                                  translate([0, 0, 6.704]) {
                                    cuboid([2.2, 1.6, 6.837]);
                                  }
                                }
                              }
                              rotate([0, 0, 38]) {
                                translate([30, 0, 0]) {
                                  translate([0, 0, 6]) {
                                    cuboid([2.2, 1.6, 6]);
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                      rotate([0, 0, 205.714]) {
                        union() {
                          union() {
                            union() {
                              union() {
                                union() {
                                  union() {
                                    union() {
                                      hull() {
                                        rotate([0, 0, 0.47]) {
                                          translate([8, 0, 0]) {
                                            translate([0, 0, 22.968]) {
                                              cuboid([2.2, 1.6, 3.339]);
                                            }
                                          }
                                        }
                                        rotate([0, 0, 3]) {
                                          translate([10.75, 0, 0]) {
                                            translate([0, 0, 21.582]) {
                                              cuboid([2.2, 1.6, 9.144]);
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        rotate([0, 0, 3]) {
                                          translate([10.75, 0, 0]) {
                                            translate([0, 0, 21.582]) {
                                              cuboid([2.2, 1.6, 9.144]);
                                            }
                                          }
                                        }
                                        rotate([0, 0, 6.48]) {
                                          translate([13.5, 0, 0]) {
                                            translate([0, 0, 18.804]) {
                                              cuboid([2.2, 1.6, 11.022]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      rotate([0, 0, 6.48]) {
                                        translate([13.5, 0, 0]) {
                                          translate([0, 0, 18.804]) {
                                            cuboid([2.2, 1.6, 11.022]);
                                          }
                                        }
                                      }
                                      rotate([0, 0, 10.62]) {
                                        translate([16.25, 0, 0]) {
                                          translate([0, 0, 15.24]) {
                                            cuboid([2.2, 1.6, 10.185]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    rotate([0, 0, 10.62]) {
                                      translate([16.25, 0, 0]) {
                                        translate([0, 0, 15.24]) {
                                          cuboid([2.2, 1.6, 10.185]);
                                        }
                                      }
                                    }
                                    rotate([0, 0, 15.28]) {
                                      translate([19, 0, 0]) {
                                        translate([0, 0, 12.249]) {
                                          cuboid([2.2, 1.6, 9.348]);
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  rotate([0, 0, 15.28]) {
                                    translate([19, 0, 0]) {
                                      translate([0, 0, 12.249]) {
                                        cuboid([2.2, 1.6, 9.348]);
                                      }
                                    }
                                  }
                                  rotate([0, 0, 20.4]) {
                                    translate([21.75, 0, 0]) {
                                      translate([0, 0, 9.829]) {
                                        cuboid([2.2, 1.6, 8.511]);
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                rotate([0, 0, 20.4]) {
                                  translate([21.75, 0, 0]) {
                                    translate([0, 0, 9.829]) {
                                      cuboid([2.2, 1.6, 8.511]);
                                    }
                                  }
                                }
                                rotate([0, 0, 25.92]) {
                                  translate([24.5, 0, 0]) {
                                    translate([0, 0, 7.981]) {
                                      cuboid([2.2, 1.6, 7.674]);
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              rotate([0, 0, 25.92]) {
                                translate([24.5, 0, 0]) {
                                  translate([0, 0, 7.981]) {
                                    cuboid([2.2, 1.6, 7.674]);
                                  }
                                }
                              }
                              rotate([0, 0, 31.8]) {
                                translate([27.25, 0, 0]) {
                                  translate([0, 0, 6.704]) {
                                    cuboid([2.2, 1.6, 6.837]);
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            rotate([0, 0, 31.8]) {
                              translate([27.25, 0, 0]) {
                                translate([0, 0, 6.704]) {
                                  cuboid([2.2, 1.6, 6.837]);
                                }
                              }
                            }
                            rotate([0, 0, 38]) {
                              translate([30, 0, 0]) {
                                translate([0, 0, 6]) {
                                  cuboid([2.2, 1.6, 6]);
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                    rotate([0, 0, 257.143]) {
                      union() {
                        union() {
                          union() {
                            union() {
                              union() {
                                union() {
                                  union() {
                                    hull() {
                                      rotate([0, 0, 0.47]) {
                                        translate([8, 0, 0]) {
                                          translate([0, 0, 22.968]) {
                                            cuboid([2.2, 1.6, 3.339]);
                                          }
                                        }
                                      }
                                      rotate([0, 0, 3]) {
                                        translate([10.75, 0, 0]) {
                                          translate([0, 0, 21.582]) {
                                            cuboid([2.2, 1.6, 9.144]);
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      rotate([0, 0, 3]) {
                                        translate([10.75, 0, 0]) {
                                          translate([0, 0, 21.582]) {
                                            cuboid([2.2, 1.6, 9.144]);
                                          }
                                        }
                                      }
                                      rotate([0, 0, 6.48]) {
                                        translate([13.5, 0, 0]) {
                                          translate([0, 0, 18.804]) {
                                            cuboid([2.2, 1.6, 11.022]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    rotate([0, 0, 6.48]) {
                                      translate([13.5, 0, 0]) {
                                        translate([0, 0, 18.804]) {
                                          cuboid([2.2, 1.6, 11.022]);
                                        }
                                      }
                                    }
                                    rotate([0, 0, 10.62]) {
                                      translate([16.25, 0, 0]) {
                                        translate([0, 0, 15.24]) {
                                          cuboid([2.2, 1.6, 10.185]);
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  rotate([0, 0, 10.62]) {
                                    translate([16.25, 0, 0]) {
                                      translate([0, 0, 15.24]) {
                                        cuboid([2.2, 1.6, 10.185]);
                                      }
                                    }
                                  }
                                  rotate([0, 0, 15.28]) {
                                    translate([19, 0, 0]) {
                                      translate([0, 0, 12.249]) {
                                        cuboid([2.2, 1.6, 9.348]);
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                rotate([0, 0, 15.28]) {
                                  translate([19, 0, 0]) {
                                    translate([0, 0, 12.249]) {
                                      cuboid([2.2, 1.6, 9.348]);
                                    }
                                  }
                                }
                                rotate([0, 0, 20.4]) {
                                  translate([21.75, 0, 0]) {
                                    translate([0, 0, 9.829]) {
                                      cuboid([2.2, 1.6, 8.511]);
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              rotate([0, 0, 20.4]) {
                                translate([21.75, 0, 0]) {
                                  translate([0, 0, 9.829]) {
                                    cuboid([2.2, 1.6, 8.511]);
                                  }
                                }
                              }
                              rotate([0, 0, 25.92]) {
                                translate([24.5, 0, 0]) {
                                  translate([0, 0, 7.981]) {
                                    cuboid([2.2, 1.6, 7.674]);
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            rotate([0, 0, 25.92]) {
                              translate([24.5, 0, 0]) {
                                translate([0, 0, 7.981]) {
                                  cuboid([2.2, 1.6, 7.674]);
                                }
                              }
                            }
                            rotate([0, 0, 31.8]) {
                              translate([27.25, 0, 0]) {
                                translate([0, 0, 6.704]) {
                                  cuboid([2.2, 1.6, 6.837]);
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          rotate([0, 0, 31.8]) {
                            translate([27.25, 0, 0]) {
                              translate([0, 0, 6.704]) {
                                cuboid([2.2, 1.6, 6.837]);
                              }
                            }
                          }
                          rotate([0, 0, 38]) {
                            translate([30, 0, 0]) {
                              translate([0, 0, 6]) {
                                cuboid([2.2, 1.6, 6]);
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                  rotate([0, 0, 308.571]) {
                    union() {
                      union() {
                        union() {
                          union() {
                            union() {
                              union() {
                                union() {
                                  hull() {
                                    rotate([0, 0, 0.47]) {
                                      translate([8, 0, 0]) {
                                        translate([0, 0, 22.968]) {
                                          cuboid([2.2, 1.6, 3.339]);
                                        }
                                      }
                                    }
                                    rotate([0, 0, 3]) {
                                      translate([10.75, 0, 0]) {
                                        translate([0, 0, 21.582]) {
                                          cuboid([2.2, 1.6, 9.144]);
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    rotate([0, 0, 3]) {
                                      translate([10.75, 0, 0]) {
                                        translate([0, 0, 21.582]) {
                                          cuboid([2.2, 1.6, 9.144]);
                                        }
                                      }
                                    }
                                    rotate([0, 0, 6.48]) {
                                      translate([13.5, 0, 0]) {
                                        translate([0, 0, 18.804]) {
                                          cuboid([2.2, 1.6, 11.022]);
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  rotate([0, 0, 6.48]) {
                                    translate([13.5, 0, 0]) {
                                      translate([0, 0, 18.804]) {
                                        cuboid([2.2, 1.6, 11.022]);
                                      }
                                    }
                                  }
                                  rotate([0, 0, 10.62]) {
                                    translate([16.25, 0, 0]) {
                                      translate([0, 0, 15.24]) {
                                        cuboid([2.2, 1.6, 10.185]);
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                rotate([0, 0, 10.62]) {
                                  translate([16.25, 0, 0]) {
                                    translate([0, 0, 15.24]) {
                                      cuboid([2.2, 1.6, 10.185]);
                                    }
                                  }
                                }
                                rotate([0, 0, 15.28]) {
                                  translate([19, 0, 0]) {
                                    translate([0, 0, 12.249]) {
                                      cuboid([2.2, 1.6, 9.348]);
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              rotate([0, 0, 15.28]) {
                                translate([19, 0, 0]) {
                                  translate([0, 0, 12.249]) {
                                    cuboid([2.2, 1.6, 9.348]);
                                  }
                                }
                              }
                              rotate([0, 0, 20.4]) {
                                translate([21.75, 0, 0]) {
                                  translate([0, 0, 9.829]) {
                                    cuboid([2.2, 1.6, 8.511]);
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            rotate([0, 0, 20.4]) {
                              translate([21.75, 0, 0]) {
                                translate([0, 0, 9.829]) {
                                  cuboid([2.2, 1.6, 8.511]);
                                }
                              }
                            }
                            rotate([0, 0, 25.92]) {
                              translate([24.5, 0, 0]) {
                                translate([0, 0, 7.981]) {
                                  cuboid([2.2, 1.6, 7.674]);
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          rotate([0, 0, 25.92]) {
                            translate([24.5, 0, 0]) {
                              translate([0, 0, 7.981]) {
                                cuboid([2.2, 1.6, 7.674]);
                              }
                            }
                          }
                          rotate([0, 0, 31.8]) {
                            translate([27.25, 0, 0]) {
                              translate([0, 0, 6.704]) {
                                cuboid([2.2, 1.6, 6.837]);
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        rotate([0, 0, 31.8]) {
                          translate([27.25, 0, 0]) {
                            translate([0, 0, 6.704]) {
                              cuboid([2.2, 1.6, 6.837]);
                            }
                          }
                        }
                        rotate([0, 0, 38]) {
                          translate([30, 0, 0]) {
                            translate([0, 0, 6]) {
                              cuboid([2.2, 1.6, 6]);
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
              translate([0, 0, 12]) {
                zcyl(r = 4.05, l = 40);
              }
            }
          }
        }
      }
      translate([0, 0, 96]) {
        difference() {
          union() {
            translate([0, 0, 3.5]) {
              zcyl(r = 14, l = 7);
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
                                    rotate([0, 0, 0]) {
                                      union() {
                                        union() {
                                          union() {
                                            union() {
                                              hull() {
                                                translate([13, 0, 0]) {
                                                  translate([0, 0, 6.108]) {
                                                    rotate([72, 0, 0]) {
                                                      cuboid([5.0e-2, 12, 2.6]);
                                                    }
                                                  }
                                                }
                                                translate([16.8, 0, 0]) {
                                                  translate([0, 0, 5.796]) {
                                                    rotate([67.2, 0, 0]) {
                                                      cuboid([5.0e-2, 11.6, 2.32]);
                                                    }
                                                  }
                                                }
                                              }
                                              hull() {
                                                translate([16.8, 0, 0]) {
                                                  translate([0, 0, 5.796]) {
                                                    rotate([67.2, 0, 0]) {
                                                      cuboid([5.0e-2, 11.6, 2.32]);
                                                    }
                                                  }
                                                }
                                                translate([20.6, 0, 0]) {
                                                  translate([0, 0, 5.435]) {
                                                    rotate([62.4, 0, 0]) {
                                                      cuboid([5.0e-2, 11.2, 2.04]);
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([20.6, 0, 0]) {
                                                translate([0, 0, 5.435]) {
                                                  rotate([62.4, 0, 0]) {
                                                    cuboid([5.0e-2, 11.2, 2.04]);
                                                  }
                                                }
                                              }
                                              translate([24.4, 0, 0]) {
                                                translate([0, 0, 5.031]) {
                                                  rotate([57.6, 0, 0]) {
                                                    cuboid([5.0e-2, 10.8, 1.76]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([24.4, 0, 0]) {
                                              translate([0, 0, 5.031]) {
                                                rotate([57.6, 0, 0]) {
                                                  cuboid([5.0e-2, 10.8, 1.76]);
                                                }
                                              }
                                            }
                                            translate([28.2, 0, 0]) {
                                              translate([0, 0, 4.589]) {
                                                rotate([52.8, 0, 0]) {
                                                  cuboid([5.0e-2, 10.4, 1.48]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([28.2, 0, 0]) {
                                            translate([0, 0, 4.589]) {
                                              rotate([52.8, 0, 0]) {
                                                cuboid([5.0e-2, 10.4, 1.48]);
                                              }
                                            }
                                          }
                                          translate([32, 0, 0]) {
                                            translate([0, 0, 4.117]) {
                                              rotate([48, 0, 0]) {
                                                cuboid([5.0e-2, 10, 1.2]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                    rotate([0, 0, 27.692]) {
                                      union() {
                                        union() {
                                          union() {
                                            union() {
                                              hull() {
                                                translate([13, 0, 0]) {
                                                  translate([0, 0, 6.108]) {
                                                    rotate([72, 0, 0]) {
                                                      cuboid([5.0e-2, 12, 2.6]);
                                                    }
                                                  }
                                                }
                                                translate([16.8, 0, 0]) {
                                                  translate([0, 0, 5.796]) {
                                                    rotate([67.2, 0, 0]) {
                                                      cuboid([5.0e-2, 11.6, 2.32]);
                                                    }
                                                  }
                                                }
                                              }
                                              hull() {
                                                translate([16.8, 0, 0]) {
                                                  translate([0, 0, 5.796]) {
                                                    rotate([67.2, 0, 0]) {
                                                      cuboid([5.0e-2, 11.6, 2.32]);
                                                    }
                                                  }
                                                }
                                                translate([20.6, 0, 0]) {
                                                  translate([0, 0, 5.435]) {
                                                    rotate([62.4, 0, 0]) {
                                                      cuboid([5.0e-2, 11.2, 2.04]);
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([20.6, 0, 0]) {
                                                translate([0, 0, 5.435]) {
                                                  rotate([62.4, 0, 0]) {
                                                    cuboid([5.0e-2, 11.2, 2.04]);
                                                  }
                                                }
                                              }
                                              translate([24.4, 0, 0]) {
                                                translate([0, 0, 5.031]) {
                                                  rotate([57.6, 0, 0]) {
                                                    cuboid([5.0e-2, 10.8, 1.76]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([24.4, 0, 0]) {
                                              translate([0, 0, 5.031]) {
                                                rotate([57.6, 0, 0]) {
                                                  cuboid([5.0e-2, 10.8, 1.76]);
                                                }
                                              }
                                            }
                                            translate([28.2, 0, 0]) {
                                              translate([0, 0, 4.589]) {
                                                rotate([52.8, 0, 0]) {
                                                  cuboid([5.0e-2, 10.4, 1.48]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([28.2, 0, 0]) {
                                            translate([0, 0, 4.589]) {
                                              rotate([52.8, 0, 0]) {
                                                cuboid([5.0e-2, 10.4, 1.48]);
                                              }
                                            }
                                          }
                                          translate([32, 0, 0]) {
                                            translate([0, 0, 4.117]) {
                                              rotate([48, 0, 0]) {
                                                cuboid([5.0e-2, 10, 1.2]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  rotate([0, 0, 55.385]) {
                                    union() {
                                      union() {
                                        union() {
                                          union() {
                                            hull() {
                                              translate([13, 0, 0]) {
                                                translate([0, 0, 6.108]) {
                                                  rotate([72, 0, 0]) {
                                                    cuboid([5.0e-2, 12, 2.6]);
                                                  }
                                                }
                                              }
                                              translate([16.8, 0, 0]) {
                                                translate([0, 0, 5.796]) {
                                                  rotate([67.2, 0, 0]) {
                                                    cuboid([5.0e-2, 11.6, 2.32]);
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([16.8, 0, 0]) {
                                                translate([0, 0, 5.796]) {
                                                  rotate([67.2, 0, 0]) {
                                                    cuboid([5.0e-2, 11.6, 2.32]);
                                                  }
                                                }
                                              }
                                              translate([20.6, 0, 0]) {
                                                translate([0, 0, 5.435]) {
                                                  rotate([62.4, 0, 0]) {
                                                    cuboid([5.0e-2, 11.2, 2.04]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([20.6, 0, 0]) {
                                              translate([0, 0, 5.435]) {
                                                rotate([62.4, 0, 0]) {
                                                  cuboid([5.0e-2, 11.2, 2.04]);
                                                }
                                              }
                                            }
                                            translate([24.4, 0, 0]) {
                                              translate([0, 0, 5.031]) {
                                                rotate([57.6, 0, 0]) {
                                                  cuboid([5.0e-2, 10.8, 1.76]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([24.4, 0, 0]) {
                                            translate([0, 0, 5.031]) {
                                              rotate([57.6, 0, 0]) {
                                                cuboid([5.0e-2, 10.8, 1.76]);
                                              }
                                            }
                                          }
                                          translate([28.2, 0, 0]) {
                                            translate([0, 0, 4.589]) {
                                              rotate([52.8, 0, 0]) {
                                                cuboid([5.0e-2, 10.4, 1.48]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([28.2, 0, 0]) {
                                          translate([0, 0, 4.589]) {
                                            rotate([52.8, 0, 0]) {
                                              cuboid([5.0e-2, 10.4, 1.48]);
                                            }
                                          }
                                        }
                                        translate([32, 0, 0]) {
                                          translate([0, 0, 4.117]) {
                                            rotate([48, 0, 0]) {
                                              cuboid([5.0e-2, 10, 1.2]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                rotate([0, 0, 83.077]) {
                                  union() {
                                    union() {
                                      union() {
                                        union() {
                                          hull() {
                                            translate([13, 0, 0]) {
                                              translate([0, 0, 6.108]) {
                                                rotate([72, 0, 0]) {
                                                  cuboid([5.0e-2, 12, 2.6]);
                                                }
                                              }
                                            }
                                            translate([16.8, 0, 0]) {
                                              translate([0, 0, 5.796]) {
                                                rotate([67.2, 0, 0]) {
                                                  cuboid([5.0e-2, 11.6, 2.32]);
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([16.8, 0, 0]) {
                                              translate([0, 0, 5.796]) {
                                                rotate([67.2, 0, 0]) {
                                                  cuboid([5.0e-2, 11.6, 2.32]);
                                                }
                                              }
                                            }
                                            translate([20.6, 0, 0]) {
                                              translate([0, 0, 5.435]) {
                                                rotate([62.4, 0, 0]) {
                                                  cuboid([5.0e-2, 11.2, 2.04]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([20.6, 0, 0]) {
                                            translate([0, 0, 5.435]) {
                                              rotate([62.4, 0, 0]) {
                                                cuboid([5.0e-2, 11.2, 2.04]);
                                              }
                                            }
                                          }
                                          translate([24.4, 0, 0]) {
                                            translate([0, 0, 5.031]) {
                                              rotate([57.6, 0, 0]) {
                                                cuboid([5.0e-2, 10.8, 1.76]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([24.4, 0, 0]) {
                                          translate([0, 0, 5.031]) {
                                            rotate([57.6, 0, 0]) {
                                              cuboid([5.0e-2, 10.8, 1.76]);
                                            }
                                          }
                                        }
                                        translate([28.2, 0, 0]) {
                                          translate([0, 0, 4.589]) {
                                            rotate([52.8, 0, 0]) {
                                              cuboid([5.0e-2, 10.4, 1.48]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([28.2, 0, 0]) {
                                        translate([0, 0, 4.589]) {
                                          rotate([52.8, 0, 0]) {
                                            cuboid([5.0e-2, 10.4, 1.48]);
                                          }
                                        }
                                      }
                                      translate([32, 0, 0]) {
                                        translate([0, 0, 4.117]) {
                                          rotate([48, 0, 0]) {
                                            cuboid([5.0e-2, 10, 1.2]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              rotate([0, 0, 110.769]) {
                                union() {
                                  union() {
                                    union() {
                                      union() {
                                        hull() {
                                          translate([13, 0, 0]) {
                                            translate([0, 0, 6.108]) {
                                              rotate([72, 0, 0]) {
                                                cuboid([5.0e-2, 12, 2.6]);
                                              }
                                            }
                                          }
                                          translate([16.8, 0, 0]) {
                                            translate([0, 0, 5.796]) {
                                              rotate([67.2, 0, 0]) {
                                                cuboid([5.0e-2, 11.6, 2.32]);
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([16.8, 0, 0]) {
                                            translate([0, 0, 5.796]) {
                                              rotate([67.2, 0, 0]) {
                                                cuboid([5.0e-2, 11.6, 2.32]);
                                              }
                                            }
                                          }
                                          translate([20.6, 0, 0]) {
                                            translate([0, 0, 5.435]) {
                                              rotate([62.4, 0, 0]) {
                                                cuboid([5.0e-2, 11.2, 2.04]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([20.6, 0, 0]) {
                                          translate([0, 0, 5.435]) {
                                            rotate([62.4, 0, 0]) {
                                              cuboid([5.0e-2, 11.2, 2.04]);
                                            }
                                          }
                                        }
                                        translate([24.4, 0, 0]) {
                                          translate([0, 0, 5.031]) {
                                            rotate([57.6, 0, 0]) {
                                              cuboid([5.0e-2, 10.8, 1.76]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([24.4, 0, 0]) {
                                        translate([0, 0, 5.031]) {
                                          rotate([57.6, 0, 0]) {
                                            cuboid([5.0e-2, 10.8, 1.76]);
                                          }
                                        }
                                      }
                                      translate([28.2, 0, 0]) {
                                        translate([0, 0, 4.589]) {
                                          rotate([52.8, 0, 0]) {
                                            cuboid([5.0e-2, 10.4, 1.48]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([28.2, 0, 0]) {
                                      translate([0, 0, 4.589]) {
                                        rotate([52.8, 0, 0]) {
                                          cuboid([5.0e-2, 10.4, 1.48]);
                                        }
                                      }
                                    }
                                    translate([32, 0, 0]) {
                                      translate([0, 0, 4.117]) {
                                        rotate([48, 0, 0]) {
                                          cuboid([5.0e-2, 10, 1.2]);
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            rotate([0, 0, 138.462]) {
                              union() {
                                union() {
                                  union() {
                                    union() {
                                      hull() {
                                        translate([13, 0, 0]) {
                                          translate([0, 0, 6.108]) {
                                            rotate([72, 0, 0]) {
                                              cuboid([5.0e-2, 12, 2.6]);
                                            }
                                          }
                                        }
                                        translate([16.8, 0, 0]) {
                                          translate([0, 0, 5.796]) {
                                            rotate([67.2, 0, 0]) {
                                              cuboid([5.0e-2, 11.6, 2.32]);
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([16.8, 0, 0]) {
                                          translate([0, 0, 5.796]) {
                                            rotate([67.2, 0, 0]) {
                                              cuboid([5.0e-2, 11.6, 2.32]);
                                            }
                                          }
                                        }
                                        translate([20.6, 0, 0]) {
                                          translate([0, 0, 5.435]) {
                                            rotate([62.4, 0, 0]) {
                                              cuboid([5.0e-2, 11.2, 2.04]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([20.6, 0, 0]) {
                                        translate([0, 0, 5.435]) {
                                          rotate([62.4, 0, 0]) {
                                            cuboid([5.0e-2, 11.2, 2.04]);
                                          }
                                        }
                                      }
                                      translate([24.4, 0, 0]) {
                                        translate([0, 0, 5.031]) {
                                          rotate([57.6, 0, 0]) {
                                            cuboid([5.0e-2, 10.8, 1.76]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([24.4, 0, 0]) {
                                      translate([0, 0, 5.031]) {
                                        rotate([57.6, 0, 0]) {
                                          cuboid([5.0e-2, 10.8, 1.76]);
                                        }
                                      }
                                    }
                                    translate([28.2, 0, 0]) {
                                      translate([0, 0, 4.589]) {
                                        rotate([52.8, 0, 0]) {
                                          cuboid([5.0e-2, 10.4, 1.48]);
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([28.2, 0, 0]) {
                                    translate([0, 0, 4.589]) {
                                      rotate([52.8, 0, 0]) {
                                        cuboid([5.0e-2, 10.4, 1.48]);
                                      }
                                    }
                                  }
                                  translate([32, 0, 0]) {
                                    translate([0, 0, 4.117]) {
                                      rotate([48, 0, 0]) {
                                        cuboid([5.0e-2, 10, 1.2]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                          rotate([0, 0, 166.154]) {
                            union() {
                              union() {
                                union() {
                                  union() {
                                    hull() {
                                      translate([13, 0, 0]) {
                                        translate([0, 0, 6.108]) {
                                          rotate([72, 0, 0]) {
                                            cuboid([5.0e-2, 12, 2.6]);
                                          }
                                        }
                                      }
                                      translate([16.8, 0, 0]) {
                                        translate([0, 0, 5.796]) {
                                          rotate([67.2, 0, 0]) {
                                            cuboid([5.0e-2, 11.6, 2.32]);
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([16.8, 0, 0]) {
                                        translate([0, 0, 5.796]) {
                                          rotate([67.2, 0, 0]) {
                                            cuboid([5.0e-2, 11.6, 2.32]);
                                          }
                                        }
                                      }
                                      translate([20.6, 0, 0]) {
                                        translate([0, 0, 5.435]) {
                                          rotate([62.4, 0, 0]) {
                                            cuboid([5.0e-2, 11.2, 2.04]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([20.6, 0, 0]) {
                                      translate([0, 0, 5.435]) {
                                        rotate([62.4, 0, 0]) {
                                          cuboid([5.0e-2, 11.2, 2.04]);
                                        }
                                      }
                                    }
                                    translate([24.4, 0, 0]) {
                                      translate([0, 0, 5.031]) {
                                        rotate([57.6, 0, 0]) {
                                          cuboid([5.0e-2, 10.8, 1.76]);
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([24.4, 0, 0]) {
                                    translate([0, 0, 5.031]) {
                                      rotate([57.6, 0, 0]) {
                                        cuboid([5.0e-2, 10.8, 1.76]);
                                      }
                                    }
                                  }
                                  translate([28.2, 0, 0]) {
                                    translate([0, 0, 4.589]) {
                                      rotate([52.8, 0, 0]) {
                                        cuboid([5.0e-2, 10.4, 1.48]);
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([28.2, 0, 0]) {
                                  translate([0, 0, 4.589]) {
                                    rotate([52.8, 0, 0]) {
                                      cuboid([5.0e-2, 10.4, 1.48]);
                                    }
                                  }
                                }
                                translate([32, 0, 0]) {
                                  translate([0, 0, 4.117]) {
                                    rotate([48, 0, 0]) {
                                      cuboid([5.0e-2, 10, 1.2]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                        rotate([0, 0, 193.846]) {
                          union() {
                            union() {
                              union() {
                                union() {
                                  hull() {
                                    translate([13, 0, 0]) {
                                      translate([0, 0, 6.108]) {
                                        rotate([72, 0, 0]) {
                                          cuboid([5.0e-2, 12, 2.6]);
                                        }
                                      }
                                    }
                                    translate([16.8, 0, 0]) {
                                      translate([0, 0, 5.796]) {
                                        rotate([67.2, 0, 0]) {
                                          cuboid([5.0e-2, 11.6, 2.32]);
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([16.8, 0, 0]) {
                                      translate([0, 0, 5.796]) {
                                        rotate([67.2, 0, 0]) {
                                          cuboid([5.0e-2, 11.6, 2.32]);
                                        }
                                      }
                                    }
                                    translate([20.6, 0, 0]) {
                                      translate([0, 0, 5.435]) {
                                        rotate([62.4, 0, 0]) {
                                          cuboid([5.0e-2, 11.2, 2.04]);
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([20.6, 0, 0]) {
                                    translate([0, 0, 5.435]) {
                                      rotate([62.4, 0, 0]) {
                                        cuboid([5.0e-2, 11.2, 2.04]);
                                      }
                                    }
                                  }
                                  translate([24.4, 0, 0]) {
                                    translate([0, 0, 5.031]) {
                                      rotate([57.6, 0, 0]) {
                                        cuboid([5.0e-2, 10.8, 1.76]);
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([24.4, 0, 0]) {
                                  translate([0, 0, 5.031]) {
                                    rotate([57.6, 0, 0]) {
                                      cuboid([5.0e-2, 10.8, 1.76]);
                                    }
                                  }
                                }
                                translate([28.2, 0, 0]) {
                                  translate([0, 0, 4.589]) {
                                    rotate([52.8, 0, 0]) {
                                      cuboid([5.0e-2, 10.4, 1.48]);
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([28.2, 0, 0]) {
                                translate([0, 0, 4.589]) {
                                  rotate([52.8, 0, 0]) {
                                    cuboid([5.0e-2, 10.4, 1.48]);
                                  }
                                }
                              }
                              translate([32, 0, 0]) {
                                translate([0, 0, 4.117]) {
                                  rotate([48, 0, 0]) {
                                    cuboid([5.0e-2, 10, 1.2]);
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                      rotate([0, 0, 221.538]) {
                        union() {
                          union() {
                            union() {
                              union() {
                                hull() {
                                  translate([13, 0, 0]) {
                                    translate([0, 0, 6.108]) {
                                      rotate([72, 0, 0]) {
                                        cuboid([5.0e-2, 12, 2.6]);
                                      }
                                    }
                                  }
                                  translate([16.8, 0, 0]) {
                                    translate([0, 0, 5.796]) {
                                      rotate([67.2, 0, 0]) {
                                        cuboid([5.0e-2, 11.6, 2.32]);
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([16.8, 0, 0]) {
                                    translate([0, 0, 5.796]) {
                                      rotate([67.2, 0, 0]) {
                                        cuboid([5.0e-2, 11.6, 2.32]);
                                      }
                                    }
                                  }
                                  translate([20.6, 0, 0]) {
                                    translate([0, 0, 5.435]) {
                                      rotate([62.4, 0, 0]) {
                                        cuboid([5.0e-2, 11.2, 2.04]);
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([20.6, 0, 0]) {
                                  translate([0, 0, 5.435]) {
                                    rotate([62.4, 0, 0]) {
                                      cuboid([5.0e-2, 11.2, 2.04]);
                                    }
                                  }
                                }
                                translate([24.4, 0, 0]) {
                                  translate([0, 0, 5.031]) {
                                    rotate([57.6, 0, 0]) {
                                      cuboid([5.0e-2, 10.8, 1.76]);
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([24.4, 0, 0]) {
                                translate([0, 0, 5.031]) {
                                  rotate([57.6, 0, 0]) {
                                    cuboid([5.0e-2, 10.8, 1.76]);
                                  }
                                }
                              }
                              translate([28.2, 0, 0]) {
                                translate([0, 0, 4.589]) {
                                  rotate([52.8, 0, 0]) {
                                    cuboid([5.0e-2, 10.4, 1.48]);
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([28.2, 0, 0]) {
                              translate([0, 0, 4.589]) {
                                rotate([52.8, 0, 0]) {
                                  cuboid([5.0e-2, 10.4, 1.48]);
                                }
                              }
                            }
                            translate([32, 0, 0]) {
                              translate([0, 0, 4.117]) {
                                rotate([48, 0, 0]) {
                                  cuboid([5.0e-2, 10, 1.2]);
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                    rotate([0, 0, 249.231]) {
                      union() {
                        union() {
                          union() {
                            union() {
                              hull() {
                                translate([13, 0, 0]) {
                                  translate([0, 0, 6.108]) {
                                    rotate([72, 0, 0]) {
                                      cuboid([5.0e-2, 12, 2.6]);
                                    }
                                  }
                                }
                                translate([16.8, 0, 0]) {
                                  translate([0, 0, 5.796]) {
                                    rotate([67.2, 0, 0]) {
                                      cuboid([5.0e-2, 11.6, 2.32]);
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([16.8, 0, 0]) {
                                  translate([0, 0, 5.796]) {
                                    rotate([67.2, 0, 0]) {
                                      cuboid([5.0e-2, 11.6, 2.32]);
                                    }
                                  }
                                }
                                translate([20.6, 0, 0]) {
                                  translate([0, 0, 5.435]) {
                                    rotate([62.4, 0, 0]) {
                                      cuboid([5.0e-2, 11.2, 2.04]);
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([20.6, 0, 0]) {
                                translate([0, 0, 5.435]) {
                                  rotate([62.4, 0, 0]) {
                                    cuboid([5.0e-2, 11.2, 2.04]);
                                  }
                                }
                              }
                              translate([24.4, 0, 0]) {
                                translate([0, 0, 5.031]) {
                                  rotate([57.6, 0, 0]) {
                                    cuboid([5.0e-2, 10.8, 1.76]);
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([24.4, 0, 0]) {
                              translate([0, 0, 5.031]) {
                                rotate([57.6, 0, 0]) {
                                  cuboid([5.0e-2, 10.8, 1.76]);
                                }
                              }
                            }
                            translate([28.2, 0, 0]) {
                              translate([0, 0, 4.589]) {
                                rotate([52.8, 0, 0]) {
                                  cuboid([5.0e-2, 10.4, 1.48]);
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([28.2, 0, 0]) {
                            translate([0, 0, 4.589]) {
                              rotate([52.8, 0, 0]) {
                                cuboid([5.0e-2, 10.4, 1.48]);
                              }
                            }
                          }
                          translate([32, 0, 0]) {
                            translate([0, 0, 4.117]) {
                              rotate([48, 0, 0]) {
                                cuboid([5.0e-2, 10, 1.2]);
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                  rotate([0, 0, 276.923]) {
                    union() {
                      union() {
                        union() {
                          union() {
                            hull() {
                              translate([13, 0, 0]) {
                                translate([0, 0, 6.108]) {
                                  rotate([72, 0, 0]) {
                                    cuboid([5.0e-2, 12, 2.6]);
                                  }
                                }
                              }
                              translate([16.8, 0, 0]) {
                                translate([0, 0, 5.796]) {
                                  rotate([67.2, 0, 0]) {
                                    cuboid([5.0e-2, 11.6, 2.32]);
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([16.8, 0, 0]) {
                                translate([0, 0, 5.796]) {
                                  rotate([67.2, 0, 0]) {
                                    cuboid([5.0e-2, 11.6, 2.32]);
                                  }
                                }
                              }
                              translate([20.6, 0, 0]) {
                                translate([0, 0, 5.435]) {
                                  rotate([62.4, 0, 0]) {
                                    cuboid([5.0e-2, 11.2, 2.04]);
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([20.6, 0, 0]) {
                              translate([0, 0, 5.435]) {
                                rotate([62.4, 0, 0]) {
                                  cuboid([5.0e-2, 11.2, 2.04]);
                                }
                              }
                            }
                            translate([24.4, 0, 0]) {
                              translate([0, 0, 5.031]) {
                                rotate([57.6, 0, 0]) {
                                  cuboid([5.0e-2, 10.8, 1.76]);
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([24.4, 0, 0]) {
                            translate([0, 0, 5.031]) {
                              rotate([57.6, 0, 0]) {
                                cuboid([5.0e-2, 10.8, 1.76]);
                              }
                            }
                          }
                          translate([28.2, 0, 0]) {
                            translate([0, 0, 4.589]) {
                              rotate([52.8, 0, 0]) {
                                cuboid([5.0e-2, 10.4, 1.48]);
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([28.2, 0, 0]) {
                          translate([0, 0, 4.589]) {
                            rotate([52.8, 0, 0]) {
                              cuboid([5.0e-2, 10.4, 1.48]);
                            }
                          }
                        }
                        translate([32, 0, 0]) {
                          translate([0, 0, 4.117]) {
                            rotate([48, 0, 0]) {
                              cuboid([5.0e-2, 10, 1.2]);
                            }
                          }
                        }
                      }
                    }
                  }
                }
                rotate([0, 0, 304.615]) {
                  union() {
                    union() {
                      union() {
                        union() {
                          hull() {
                            translate([13, 0, 0]) {
                              translate([0, 0, 6.108]) {
                                rotate([72, 0, 0]) {
                                  cuboid([5.0e-2, 12, 2.6]);
                                }
                              }
                            }
                            translate([16.8, 0, 0]) {
                              translate([0, 0, 5.796]) {
                                rotate([67.2, 0, 0]) {
                                  cuboid([5.0e-2, 11.6, 2.32]);
                                }
                              }
                            }
                          }
                          hull() {
                            translate([16.8, 0, 0]) {
                              translate([0, 0, 5.796]) {
                                rotate([67.2, 0, 0]) {
                                  cuboid([5.0e-2, 11.6, 2.32]);
                                }
                              }
                            }
                            translate([20.6, 0, 0]) {
                              translate([0, 0, 5.435]) {
                                rotate([62.4, 0, 0]) {
                                  cuboid([5.0e-2, 11.2, 2.04]);
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([20.6, 0, 0]) {
                            translate([0, 0, 5.435]) {
                              rotate([62.4, 0, 0]) {
                                cuboid([5.0e-2, 11.2, 2.04]);
                              }
                            }
                          }
                          translate([24.4, 0, 0]) {
                            translate([0, 0, 5.031]) {
                              rotate([57.6, 0, 0]) {
                                cuboid([5.0e-2, 10.8, 1.76]);
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([24.4, 0, 0]) {
                          translate([0, 0, 5.031]) {
                            rotate([57.6, 0, 0]) {
                              cuboid([5.0e-2, 10.8, 1.76]);
                            }
                          }
                        }
                        translate([28.2, 0, 0]) {
                          translate([0, 0, 4.589]) {
                            rotate([52.8, 0, 0]) {
                              cuboid([5.0e-2, 10.4, 1.48]);
                            }
                          }
                        }
                      }
                    }
                    hull() {
                      translate([28.2, 0, 0]) {
                        translate([0, 0, 4.589]) {
                          rotate([52.8, 0, 0]) {
                            cuboid([5.0e-2, 10.4, 1.48]);
                          }
                        }
                      }
                      translate([32, 0, 0]) {
                        translate([0, 0, 4.117]) {
                          rotate([48, 0, 0]) {
                            cuboid([5.0e-2, 10, 1.2]);
                          }
                        }
                      }
                    }
                  }
                }
              }
              rotate([0, 0, 332.308]) {
                union() {
                  union() {
                    union() {
                      union() {
                        hull() {
                          translate([13, 0, 0]) {
                            translate([0, 0, 6.108]) {
                              rotate([72, 0, 0]) {
                                cuboid([5.0e-2, 12, 2.6]);
                              }
                            }
                          }
                          translate([16.8, 0, 0]) {
                            translate([0, 0, 5.796]) {
                              rotate([67.2, 0, 0]) {
                                cuboid([5.0e-2, 11.6, 2.32]);
                              }
                            }
                          }
                        }
                        hull() {
                          translate([16.8, 0, 0]) {
                            translate([0, 0, 5.796]) {
                              rotate([67.2, 0, 0]) {
                                cuboid([5.0e-2, 11.6, 2.32]);
                              }
                            }
                          }
                          translate([20.6, 0, 0]) {
                            translate([0, 0, 5.435]) {
                              rotate([62.4, 0, 0]) {
                                cuboid([5.0e-2, 11.2, 2.04]);
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([20.6, 0, 0]) {
                          translate([0, 0, 5.435]) {
                            rotate([62.4, 0, 0]) {
                              cuboid([5.0e-2, 11.2, 2.04]);
                            }
                          }
                        }
                        translate([24.4, 0, 0]) {
                          translate([0, 0, 5.031]) {
                            rotate([57.6, 0, 0]) {
                              cuboid([5.0e-2, 10.8, 1.76]);
                            }
                          }
                        }
                      }
                    }
                    hull() {
                      translate([24.4, 0, 0]) {
                        translate([0, 0, 5.031]) {
                          rotate([57.6, 0, 0]) {
                            cuboid([5.0e-2, 10.8, 1.76]);
                          }
                        }
                      }
                      translate([28.2, 0, 0]) {
                        translate([0, 0, 4.589]) {
                          rotate([52.8, 0, 0]) {
                            cuboid([5.0e-2, 10.4, 1.48]);
                          }
                        }
                      }
                    }
                  }
                  hull() {
                    translate([28.2, 0, 0]) {
                      translate([0, 0, 4.589]) {
                        rotate([52.8, 0, 0]) {
                          cuboid([5.0e-2, 10.4, 1.48]);
                        }
                      }
                    }
                    translate([32, 0, 0]) {
                      translate([0, 0, 4.117]) {
                        rotate([48, 0, 0]) {
                          cuboid([5.0e-2, 10, 1.2]);
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          translate([0, 0, 3.5]) {
            zcyl(r = 4.05, l = 20);
          }
        }
      }
    }
    translate([0, 0, 112]) {
      difference() {
        union() {
          translate([0, 0, 3.5]) {
            zcyl(r = 14, l = 7);
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
                                  rotate([0, 0, 0]) {
                                    union() {
                                      union() {
                                        union() {
                                          union() {
                                            hull() {
                                              translate([13, 0, 0]) {
                                                translate([0, 0, 6.108]) {
                                                  rotate([72, 0, 0]) {
                                                    cuboid([5.0e-2, 12, 2.6]);
                                                  }
                                                }
                                              }
                                              translate([16.8, 0, 0]) {
                                                translate([0, 0, 5.796]) {
                                                  rotate([67.2, 0, 0]) {
                                                    cuboid([5.0e-2, 11.6, 2.32]);
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([16.8, 0, 0]) {
                                                translate([0, 0, 5.796]) {
                                                  rotate([67.2, 0, 0]) {
                                                    cuboid([5.0e-2, 11.6, 2.32]);
                                                  }
                                                }
                                              }
                                              translate([20.6, 0, 0]) {
                                                translate([0, 0, 5.435]) {
                                                  rotate([62.4, 0, 0]) {
                                                    cuboid([5.0e-2, 11.2, 2.04]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([20.6, 0, 0]) {
                                              translate([0, 0, 5.435]) {
                                                rotate([62.4, 0, 0]) {
                                                  cuboid([5.0e-2, 11.2, 2.04]);
                                                }
                                              }
                                            }
                                            translate([24.4, 0, 0]) {
                                              translate([0, 0, 5.031]) {
                                                rotate([57.6, 0, 0]) {
                                                  cuboid([5.0e-2, 10.8, 1.76]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([24.4, 0, 0]) {
                                            translate([0, 0, 5.031]) {
                                              rotate([57.6, 0, 0]) {
                                                cuboid([5.0e-2, 10.8, 1.76]);
                                              }
                                            }
                                          }
                                          translate([28.2, 0, 0]) {
                                            translate([0, 0, 4.589]) {
                                              rotate([52.8, 0, 0]) {
                                                cuboid([5.0e-2, 10.4, 1.48]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([28.2, 0, 0]) {
                                          translate([0, 0, 4.589]) {
                                            rotate([52.8, 0, 0]) {
                                              cuboid([5.0e-2, 10.4, 1.48]);
                                            }
                                          }
                                        }
                                        translate([32, 0, 0]) {
                                          translate([0, 0, 4.117]) {
                                            rotate([48, 0, 0]) {
                                              cuboid([5.0e-2, 10, 1.2]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  rotate([0, 0, 27.692]) {
                                    union() {
                                      union() {
                                        union() {
                                          union() {
                                            hull() {
                                              translate([13, 0, 0]) {
                                                translate([0, 0, 6.108]) {
                                                  rotate([72, 0, 0]) {
                                                    cuboid([5.0e-2, 12, 2.6]);
                                                  }
                                                }
                                              }
                                              translate([16.8, 0, 0]) {
                                                translate([0, 0, 5.796]) {
                                                  rotate([67.2, 0, 0]) {
                                                    cuboid([5.0e-2, 11.6, 2.32]);
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([16.8, 0, 0]) {
                                                translate([0, 0, 5.796]) {
                                                  rotate([67.2, 0, 0]) {
                                                    cuboid([5.0e-2, 11.6, 2.32]);
                                                  }
                                                }
                                              }
                                              translate([20.6, 0, 0]) {
                                                translate([0, 0, 5.435]) {
                                                  rotate([62.4, 0, 0]) {
                                                    cuboid([5.0e-2, 11.2, 2.04]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([20.6, 0, 0]) {
                                              translate([0, 0, 5.435]) {
                                                rotate([62.4, 0, 0]) {
                                                  cuboid([5.0e-2, 11.2, 2.04]);
                                                }
                                              }
                                            }
                                            translate([24.4, 0, 0]) {
                                              translate([0, 0, 5.031]) {
                                                rotate([57.6, 0, 0]) {
                                                  cuboid([5.0e-2, 10.8, 1.76]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([24.4, 0, 0]) {
                                            translate([0, 0, 5.031]) {
                                              rotate([57.6, 0, 0]) {
                                                cuboid([5.0e-2, 10.8, 1.76]);
                                              }
                                            }
                                          }
                                          translate([28.2, 0, 0]) {
                                            translate([0, 0, 4.589]) {
                                              rotate([52.8, 0, 0]) {
                                                cuboid([5.0e-2, 10.4, 1.48]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([28.2, 0, 0]) {
                                          translate([0, 0, 4.589]) {
                                            rotate([52.8, 0, 0]) {
                                              cuboid([5.0e-2, 10.4, 1.48]);
                                            }
                                          }
                                        }
                                        translate([32, 0, 0]) {
                                          translate([0, 0, 4.117]) {
                                            rotate([48, 0, 0]) {
                                              cuboid([5.0e-2, 10, 1.2]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                rotate([0, 0, 55.385]) {
                                  union() {
                                    union() {
                                      union() {
                                        union() {
                                          hull() {
                                            translate([13, 0, 0]) {
                                              translate([0, 0, 6.108]) {
                                                rotate([72, 0, 0]) {
                                                  cuboid([5.0e-2, 12, 2.6]);
                                                }
                                              }
                                            }
                                            translate([16.8, 0, 0]) {
                                              translate([0, 0, 5.796]) {
                                                rotate([67.2, 0, 0]) {
                                                  cuboid([5.0e-2, 11.6, 2.32]);
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([16.8, 0, 0]) {
                                              translate([0, 0, 5.796]) {
                                                rotate([67.2, 0, 0]) {
                                                  cuboid([5.0e-2, 11.6, 2.32]);
                                                }
                                              }
                                            }
                                            translate([20.6, 0, 0]) {
                                              translate([0, 0, 5.435]) {
                                                rotate([62.4, 0, 0]) {
                                                  cuboid([5.0e-2, 11.2, 2.04]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([20.6, 0, 0]) {
                                            translate([0, 0, 5.435]) {
                                              rotate([62.4, 0, 0]) {
                                                cuboid([5.0e-2, 11.2, 2.04]);
                                              }
                                            }
                                          }
                                          translate([24.4, 0, 0]) {
                                            translate([0, 0, 5.031]) {
                                              rotate([57.6, 0, 0]) {
                                                cuboid([5.0e-2, 10.8, 1.76]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([24.4, 0, 0]) {
                                          translate([0, 0, 5.031]) {
                                            rotate([57.6, 0, 0]) {
                                              cuboid([5.0e-2, 10.8, 1.76]);
                                            }
                                          }
                                        }
                                        translate([28.2, 0, 0]) {
                                          translate([0, 0, 4.589]) {
                                            rotate([52.8, 0, 0]) {
                                              cuboid([5.0e-2, 10.4, 1.48]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([28.2, 0, 0]) {
                                        translate([0, 0, 4.589]) {
                                          rotate([52.8, 0, 0]) {
                                            cuboid([5.0e-2, 10.4, 1.48]);
                                          }
                                        }
                                      }
                                      translate([32, 0, 0]) {
                                        translate([0, 0, 4.117]) {
                                          rotate([48, 0, 0]) {
                                            cuboid([5.0e-2, 10, 1.2]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              rotate([0, 0, 83.077]) {
                                union() {
                                  union() {
                                    union() {
                                      union() {
                                        hull() {
                                          translate([13, 0, 0]) {
                                            translate([0, 0, 6.108]) {
                                              rotate([72, 0, 0]) {
                                                cuboid([5.0e-2, 12, 2.6]);
                                              }
                                            }
                                          }
                                          translate([16.8, 0, 0]) {
                                            translate([0, 0, 5.796]) {
                                              rotate([67.2, 0, 0]) {
                                                cuboid([5.0e-2, 11.6, 2.32]);
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([16.8, 0, 0]) {
                                            translate([0, 0, 5.796]) {
                                              rotate([67.2, 0, 0]) {
                                                cuboid([5.0e-2, 11.6, 2.32]);
                                              }
                                            }
                                          }
                                          translate([20.6, 0, 0]) {
                                            translate([0, 0, 5.435]) {
                                              rotate([62.4, 0, 0]) {
                                                cuboid([5.0e-2, 11.2, 2.04]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([20.6, 0, 0]) {
                                          translate([0, 0, 5.435]) {
                                            rotate([62.4, 0, 0]) {
                                              cuboid([5.0e-2, 11.2, 2.04]);
                                            }
                                          }
                                        }
                                        translate([24.4, 0, 0]) {
                                          translate([0, 0, 5.031]) {
                                            rotate([57.6, 0, 0]) {
                                              cuboid([5.0e-2, 10.8, 1.76]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([24.4, 0, 0]) {
                                        translate([0, 0, 5.031]) {
                                          rotate([57.6, 0, 0]) {
                                            cuboid([5.0e-2, 10.8, 1.76]);
                                          }
                                        }
                                      }
                                      translate([28.2, 0, 0]) {
                                        translate([0, 0, 4.589]) {
                                          rotate([52.8, 0, 0]) {
                                            cuboid([5.0e-2, 10.4, 1.48]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([28.2, 0, 0]) {
                                      translate([0, 0, 4.589]) {
                                        rotate([52.8, 0, 0]) {
                                          cuboid([5.0e-2, 10.4, 1.48]);
                                        }
                                      }
                                    }
                                    translate([32, 0, 0]) {
                                      translate([0, 0, 4.117]) {
                                        rotate([48, 0, 0]) {
                                          cuboid([5.0e-2, 10, 1.2]);
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            rotate([0, 0, 110.769]) {
                              union() {
                                union() {
                                  union() {
                                    union() {
                                      hull() {
                                        translate([13, 0, 0]) {
                                          translate([0, 0, 6.108]) {
                                            rotate([72, 0, 0]) {
                                              cuboid([5.0e-2, 12, 2.6]);
                                            }
                                          }
                                        }
                                        translate([16.8, 0, 0]) {
                                          translate([0, 0, 5.796]) {
                                            rotate([67.2, 0, 0]) {
                                              cuboid([5.0e-2, 11.6, 2.32]);
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([16.8, 0, 0]) {
                                          translate([0, 0, 5.796]) {
                                            rotate([67.2, 0, 0]) {
                                              cuboid([5.0e-2, 11.6, 2.32]);
                                            }
                                          }
                                        }
                                        translate([20.6, 0, 0]) {
                                          translate([0, 0, 5.435]) {
                                            rotate([62.4, 0, 0]) {
                                              cuboid([5.0e-2, 11.2, 2.04]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([20.6, 0, 0]) {
                                        translate([0, 0, 5.435]) {
                                          rotate([62.4, 0, 0]) {
                                            cuboid([5.0e-2, 11.2, 2.04]);
                                          }
                                        }
                                      }
                                      translate([24.4, 0, 0]) {
                                        translate([0, 0, 5.031]) {
                                          rotate([57.6, 0, 0]) {
                                            cuboid([5.0e-2, 10.8, 1.76]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([24.4, 0, 0]) {
                                      translate([0, 0, 5.031]) {
                                        rotate([57.6, 0, 0]) {
                                          cuboid([5.0e-2, 10.8, 1.76]);
                                        }
                                      }
                                    }
                                    translate([28.2, 0, 0]) {
                                      translate([0, 0, 4.589]) {
                                        rotate([52.8, 0, 0]) {
                                          cuboid([5.0e-2, 10.4, 1.48]);
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([28.2, 0, 0]) {
                                    translate([0, 0, 4.589]) {
                                      rotate([52.8, 0, 0]) {
                                        cuboid([5.0e-2, 10.4, 1.48]);
                                      }
                                    }
                                  }
                                  translate([32, 0, 0]) {
                                    translate([0, 0, 4.117]) {
                                      rotate([48, 0, 0]) {
                                        cuboid([5.0e-2, 10, 1.2]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                          rotate([0, 0, 138.462]) {
                            union() {
                              union() {
                                union() {
                                  union() {
                                    hull() {
                                      translate([13, 0, 0]) {
                                        translate([0, 0, 6.108]) {
                                          rotate([72, 0, 0]) {
                                            cuboid([5.0e-2, 12, 2.6]);
                                          }
                                        }
                                      }
                                      translate([16.8, 0, 0]) {
                                        translate([0, 0, 5.796]) {
                                          rotate([67.2, 0, 0]) {
                                            cuboid([5.0e-2, 11.6, 2.32]);
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([16.8, 0, 0]) {
                                        translate([0, 0, 5.796]) {
                                          rotate([67.2, 0, 0]) {
                                            cuboid([5.0e-2, 11.6, 2.32]);
                                          }
                                        }
                                      }
                                      translate([20.6, 0, 0]) {
                                        translate([0, 0, 5.435]) {
                                          rotate([62.4, 0, 0]) {
                                            cuboid([5.0e-2, 11.2, 2.04]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([20.6, 0, 0]) {
                                      translate([0, 0, 5.435]) {
                                        rotate([62.4, 0, 0]) {
                                          cuboid([5.0e-2, 11.2, 2.04]);
                                        }
                                      }
                                    }
                                    translate([24.4, 0, 0]) {
                                      translate([0, 0, 5.031]) {
                                        rotate([57.6, 0, 0]) {
                                          cuboid([5.0e-2, 10.8, 1.76]);
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([24.4, 0, 0]) {
                                    translate([0, 0, 5.031]) {
                                      rotate([57.6, 0, 0]) {
                                        cuboid([5.0e-2, 10.8, 1.76]);
                                      }
                                    }
                                  }
                                  translate([28.2, 0, 0]) {
                                    translate([0, 0, 4.589]) {
                                      rotate([52.8, 0, 0]) {
                                        cuboid([5.0e-2, 10.4, 1.48]);
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([28.2, 0, 0]) {
                                  translate([0, 0, 4.589]) {
                                    rotate([52.8, 0, 0]) {
                                      cuboid([5.0e-2, 10.4, 1.48]);
                                    }
                                  }
                                }
                                translate([32, 0, 0]) {
                                  translate([0, 0, 4.117]) {
                                    rotate([48, 0, 0]) {
                                      cuboid([5.0e-2, 10, 1.2]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                        rotate([0, 0, 166.154]) {
                          union() {
                            union() {
                              union() {
                                union() {
                                  hull() {
                                    translate([13, 0, 0]) {
                                      translate([0, 0, 6.108]) {
                                        rotate([72, 0, 0]) {
                                          cuboid([5.0e-2, 12, 2.6]);
                                        }
                                      }
                                    }
                                    translate([16.8, 0, 0]) {
                                      translate([0, 0, 5.796]) {
                                        rotate([67.2, 0, 0]) {
                                          cuboid([5.0e-2, 11.6, 2.32]);
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([16.8, 0, 0]) {
                                      translate([0, 0, 5.796]) {
                                        rotate([67.2, 0, 0]) {
                                          cuboid([5.0e-2, 11.6, 2.32]);
                                        }
                                      }
                                    }
                                    translate([20.6, 0, 0]) {
                                      translate([0, 0, 5.435]) {
                                        rotate([62.4, 0, 0]) {
                                          cuboid([5.0e-2, 11.2, 2.04]);
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([20.6, 0, 0]) {
                                    translate([0, 0, 5.435]) {
                                      rotate([62.4, 0, 0]) {
                                        cuboid([5.0e-2, 11.2, 2.04]);
                                      }
                                    }
                                  }
                                  translate([24.4, 0, 0]) {
                                    translate([0, 0, 5.031]) {
                                      rotate([57.6, 0, 0]) {
                                        cuboid([5.0e-2, 10.8, 1.76]);
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([24.4, 0, 0]) {
                                  translate([0, 0, 5.031]) {
                                    rotate([57.6, 0, 0]) {
                                      cuboid([5.0e-2, 10.8, 1.76]);
                                    }
                                  }
                                }
                                translate([28.2, 0, 0]) {
                                  translate([0, 0, 4.589]) {
                                    rotate([52.8, 0, 0]) {
                                      cuboid([5.0e-2, 10.4, 1.48]);
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([28.2, 0, 0]) {
                                translate([0, 0, 4.589]) {
                                  rotate([52.8, 0, 0]) {
                                    cuboid([5.0e-2, 10.4, 1.48]);
                                  }
                                }
                              }
                              translate([32, 0, 0]) {
                                translate([0, 0, 4.117]) {
                                  rotate([48, 0, 0]) {
                                    cuboid([5.0e-2, 10, 1.2]);
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                      rotate([0, 0, 193.846]) {
                        union() {
                          union() {
                            union() {
                              union() {
                                hull() {
                                  translate([13, 0, 0]) {
                                    translate([0, 0, 6.108]) {
                                      rotate([72, 0, 0]) {
                                        cuboid([5.0e-2, 12, 2.6]);
                                      }
                                    }
                                  }
                                  translate([16.8, 0, 0]) {
                                    translate([0, 0, 5.796]) {
                                      rotate([67.2, 0, 0]) {
                                        cuboid([5.0e-2, 11.6, 2.32]);
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([16.8, 0, 0]) {
                                    translate([0, 0, 5.796]) {
                                      rotate([67.2, 0, 0]) {
                                        cuboid([5.0e-2, 11.6, 2.32]);
                                      }
                                    }
                                  }
                                  translate([20.6, 0, 0]) {
                                    translate([0, 0, 5.435]) {
                                      rotate([62.4, 0, 0]) {
                                        cuboid([5.0e-2, 11.2, 2.04]);
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([20.6, 0, 0]) {
                                  translate([0, 0, 5.435]) {
                                    rotate([62.4, 0, 0]) {
                                      cuboid([5.0e-2, 11.2, 2.04]);
                                    }
                                  }
                                }
                                translate([24.4, 0, 0]) {
                                  translate([0, 0, 5.031]) {
                                    rotate([57.6, 0, 0]) {
                                      cuboid([5.0e-2, 10.8, 1.76]);
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([24.4, 0, 0]) {
                                translate([0, 0, 5.031]) {
                                  rotate([57.6, 0, 0]) {
                                    cuboid([5.0e-2, 10.8, 1.76]);
                                  }
                                }
                              }
                              translate([28.2, 0, 0]) {
                                translate([0, 0, 4.589]) {
                                  rotate([52.8, 0, 0]) {
                                    cuboid([5.0e-2, 10.4, 1.48]);
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([28.2, 0, 0]) {
                              translate([0, 0, 4.589]) {
                                rotate([52.8, 0, 0]) {
                                  cuboid([5.0e-2, 10.4, 1.48]);
                                }
                              }
                            }
                            translate([32, 0, 0]) {
                              translate([0, 0, 4.117]) {
                                rotate([48, 0, 0]) {
                                  cuboid([5.0e-2, 10, 1.2]);
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                    rotate([0, 0, 221.538]) {
                      union() {
                        union() {
                          union() {
                            union() {
                              hull() {
                                translate([13, 0, 0]) {
                                  translate([0, 0, 6.108]) {
                                    rotate([72, 0, 0]) {
                                      cuboid([5.0e-2, 12, 2.6]);
                                    }
                                  }
                                }
                                translate([16.8, 0, 0]) {
                                  translate([0, 0, 5.796]) {
                                    rotate([67.2, 0, 0]) {
                                      cuboid([5.0e-2, 11.6, 2.32]);
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([16.8, 0, 0]) {
                                  translate([0, 0, 5.796]) {
                                    rotate([67.2, 0, 0]) {
                                      cuboid([5.0e-2, 11.6, 2.32]);
                                    }
                                  }
                                }
                                translate([20.6, 0, 0]) {
                                  translate([0, 0, 5.435]) {
                                    rotate([62.4, 0, 0]) {
                                      cuboid([5.0e-2, 11.2, 2.04]);
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([20.6, 0, 0]) {
                                translate([0, 0, 5.435]) {
                                  rotate([62.4, 0, 0]) {
                                    cuboid([5.0e-2, 11.2, 2.04]);
                                  }
                                }
                              }
                              translate([24.4, 0, 0]) {
                                translate([0, 0, 5.031]) {
                                  rotate([57.6, 0, 0]) {
                                    cuboid([5.0e-2, 10.8, 1.76]);
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([24.4, 0, 0]) {
                              translate([0, 0, 5.031]) {
                                rotate([57.6, 0, 0]) {
                                  cuboid([5.0e-2, 10.8, 1.76]);
                                }
                              }
                            }
                            translate([28.2, 0, 0]) {
                              translate([0, 0, 4.589]) {
                                rotate([52.8, 0, 0]) {
                                  cuboid([5.0e-2, 10.4, 1.48]);
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([28.2, 0, 0]) {
                            translate([0, 0, 4.589]) {
                              rotate([52.8, 0, 0]) {
                                cuboid([5.0e-2, 10.4, 1.48]);
                              }
                            }
                          }
                          translate([32, 0, 0]) {
                            translate([0, 0, 4.117]) {
                              rotate([48, 0, 0]) {
                                cuboid([5.0e-2, 10, 1.2]);
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                  rotate([0, 0, 249.231]) {
                    union() {
                      union() {
                        union() {
                          union() {
                            hull() {
                              translate([13, 0, 0]) {
                                translate([0, 0, 6.108]) {
                                  rotate([72, 0, 0]) {
                                    cuboid([5.0e-2, 12, 2.6]);
                                  }
                                }
                              }
                              translate([16.8, 0, 0]) {
                                translate([0, 0, 5.796]) {
                                  rotate([67.2, 0, 0]) {
                                    cuboid([5.0e-2, 11.6, 2.32]);
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([16.8, 0, 0]) {
                                translate([0, 0, 5.796]) {
                                  rotate([67.2, 0, 0]) {
                                    cuboid([5.0e-2, 11.6, 2.32]);
                                  }
                                }
                              }
                              translate([20.6, 0, 0]) {
                                translate([0, 0, 5.435]) {
                                  rotate([62.4, 0, 0]) {
                                    cuboid([5.0e-2, 11.2, 2.04]);
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([20.6, 0, 0]) {
                              translate([0, 0, 5.435]) {
                                rotate([62.4, 0, 0]) {
                                  cuboid([5.0e-2, 11.2, 2.04]);
                                }
                              }
                            }
                            translate([24.4, 0, 0]) {
                              translate([0, 0, 5.031]) {
                                rotate([57.6, 0, 0]) {
                                  cuboid([5.0e-2, 10.8, 1.76]);
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([24.4, 0, 0]) {
                            translate([0, 0, 5.031]) {
                              rotate([57.6, 0, 0]) {
                                cuboid([5.0e-2, 10.8, 1.76]);
                              }
                            }
                          }
                          translate([28.2, 0, 0]) {
                            translate([0, 0, 4.589]) {
                              rotate([52.8, 0, 0]) {
                                cuboid([5.0e-2, 10.4, 1.48]);
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([28.2, 0, 0]) {
                          translate([0, 0, 4.589]) {
                            rotate([52.8, 0, 0]) {
                              cuboid([5.0e-2, 10.4, 1.48]);
                            }
                          }
                        }
                        translate([32, 0, 0]) {
                          translate([0, 0, 4.117]) {
                            rotate([48, 0, 0]) {
                              cuboid([5.0e-2, 10, 1.2]);
                            }
                          }
                        }
                      }
                    }
                  }
                }
                rotate([0, 0, 276.923]) {
                  union() {
                    union() {
                      union() {
                        union() {
                          hull() {
                            translate([13, 0, 0]) {
                              translate([0, 0, 6.108]) {
                                rotate([72, 0, 0]) {
                                  cuboid([5.0e-2, 12, 2.6]);
                                }
                              }
                            }
                            translate([16.8, 0, 0]) {
                              translate([0, 0, 5.796]) {
                                rotate([67.2, 0, 0]) {
                                  cuboid([5.0e-2, 11.6, 2.32]);
                                }
                              }
                            }
                          }
                          hull() {
                            translate([16.8, 0, 0]) {
                              translate([0, 0, 5.796]) {
                                rotate([67.2, 0, 0]) {
                                  cuboid([5.0e-2, 11.6, 2.32]);
                                }
                              }
                            }
                            translate([20.6, 0, 0]) {
                              translate([0, 0, 5.435]) {
                                rotate([62.4, 0, 0]) {
                                  cuboid([5.0e-2, 11.2, 2.04]);
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([20.6, 0, 0]) {
                            translate([0, 0, 5.435]) {
                              rotate([62.4, 0, 0]) {
                                cuboid([5.0e-2, 11.2, 2.04]);
                              }
                            }
                          }
                          translate([24.4, 0, 0]) {
                            translate([0, 0, 5.031]) {
                              rotate([57.6, 0, 0]) {
                                cuboid([5.0e-2, 10.8, 1.76]);
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([24.4, 0, 0]) {
                          translate([0, 0, 5.031]) {
                            rotate([57.6, 0, 0]) {
                              cuboid([5.0e-2, 10.8, 1.76]);
                            }
                          }
                        }
                        translate([28.2, 0, 0]) {
                          translate([0, 0, 4.589]) {
                            rotate([52.8, 0, 0]) {
                              cuboid([5.0e-2, 10.4, 1.48]);
                            }
                          }
                        }
                      }
                    }
                    hull() {
                      translate([28.2, 0, 0]) {
                        translate([0, 0, 4.589]) {
                          rotate([52.8, 0, 0]) {
                            cuboid([5.0e-2, 10.4, 1.48]);
                          }
                        }
                      }
                      translate([32, 0, 0]) {
                        translate([0, 0, 4.117]) {
                          rotate([48, 0, 0]) {
                            cuboid([5.0e-2, 10, 1.2]);
                          }
                        }
                      }
                    }
                  }
                }
              }
              rotate([0, 0, 304.615]) {
                union() {
                  union() {
                    union() {
                      union() {
                        hull() {
                          translate([13, 0, 0]) {
                            translate([0, 0, 6.108]) {
                              rotate([72, 0, 0]) {
                                cuboid([5.0e-2, 12, 2.6]);
                              }
                            }
                          }
                          translate([16.8, 0, 0]) {
                            translate([0, 0, 5.796]) {
                              rotate([67.2, 0, 0]) {
                                cuboid([5.0e-2, 11.6, 2.32]);
                              }
                            }
                          }
                        }
                        hull() {
                          translate([16.8, 0, 0]) {
                            translate([0, 0, 5.796]) {
                              rotate([67.2, 0, 0]) {
                                cuboid([5.0e-2, 11.6, 2.32]);
                              }
                            }
                          }
                          translate([20.6, 0, 0]) {
                            translate([0, 0, 5.435]) {
                              rotate([62.4, 0, 0]) {
                                cuboid([5.0e-2, 11.2, 2.04]);
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([20.6, 0, 0]) {
                          translate([0, 0, 5.435]) {
                            rotate([62.4, 0, 0]) {
                              cuboid([5.0e-2, 11.2, 2.04]);
                            }
                          }
                        }
                        translate([24.4, 0, 0]) {
                          translate([0, 0, 5.031]) {
                            rotate([57.6, 0, 0]) {
                              cuboid([5.0e-2, 10.8, 1.76]);
                            }
                          }
                        }
                      }
                    }
                    hull() {
                      translate([24.4, 0, 0]) {
                        translate([0, 0, 5.031]) {
                          rotate([57.6, 0, 0]) {
                            cuboid([5.0e-2, 10.8, 1.76]);
                          }
                        }
                      }
                      translate([28.2, 0, 0]) {
                        translate([0, 0, 4.589]) {
                          rotate([52.8, 0, 0]) {
                            cuboid([5.0e-2, 10.4, 1.48]);
                          }
                        }
                      }
                    }
                  }
                  hull() {
                    translate([28.2, 0, 0]) {
                      translate([0, 0, 4.589]) {
                        rotate([52.8, 0, 0]) {
                          cuboid([5.0e-2, 10.4, 1.48]);
                        }
                      }
                    }
                    translate([32, 0, 0]) {
                      translate([0, 0, 4.117]) {
                        rotate([48, 0, 0]) {
                          cuboid([5.0e-2, 10, 1.2]);
                        }
                      }
                    }
                  }
                }
              }
            }
            rotate([0, 0, 332.308]) {
              union() {
                union() {
                  union() {
                    union() {
                      hull() {
                        translate([13, 0, 0]) {
                          translate([0, 0, 6.108]) {
                            rotate([72, 0, 0]) {
                              cuboid([5.0e-2, 12, 2.6]);
                            }
                          }
                        }
                        translate([16.8, 0, 0]) {
                          translate([0, 0, 5.796]) {
                            rotate([67.2, 0, 0]) {
                              cuboid([5.0e-2, 11.6, 2.32]);
                            }
                          }
                        }
                      }
                      hull() {
                        translate([16.8, 0, 0]) {
                          translate([0, 0, 5.796]) {
                            rotate([67.2, 0, 0]) {
                              cuboid([5.0e-2, 11.6, 2.32]);
                            }
                          }
                        }
                        translate([20.6, 0, 0]) {
                          translate([0, 0, 5.435]) {
                            rotate([62.4, 0, 0]) {
                              cuboid([5.0e-2, 11.2, 2.04]);
                            }
                          }
                        }
                      }
                    }
                    hull() {
                      translate([20.6, 0, 0]) {
                        translate([0, 0, 5.435]) {
                          rotate([62.4, 0, 0]) {
                            cuboid([5.0e-2, 11.2, 2.04]);
                          }
                        }
                      }
                      translate([24.4, 0, 0]) {
                        translate([0, 0, 5.031]) {
                          rotate([57.6, 0, 0]) {
                            cuboid([5.0e-2, 10.8, 1.76]);
                          }
                        }
                      }
                    }
                  }
                  hull() {
                    translate([24.4, 0, 0]) {
                      translate([0, 0, 5.031]) {
                        rotate([57.6, 0, 0]) {
                          cuboid([5.0e-2, 10.8, 1.76]);
                        }
                      }
                    }
                    translate([28.2, 0, 0]) {
                      translate([0, 0, 4.589]) {
                        rotate([52.8, 0, 0]) {
                          cuboid([5.0e-2, 10.4, 1.48]);
                        }
                      }
                    }
                  }
                }
                hull() {
                  translate([28.2, 0, 0]) {
                    translate([0, 0, 4.589]) {
                      rotate([52.8, 0, 0]) {
                        cuboid([5.0e-2, 10.4, 1.48]);
                      }
                    }
                  }
                  translate([32, 0, 0]) {
                    translate([0, 0, 4.117]) {
                      rotate([48, 0, 0]) {
                        cuboid([5.0e-2, 10, 1.2]);
                      }
                    }
                  }
                }
              }
            }
          }
        }
        translate([0, 0, 3.5]) {
          zcyl(r = 4.05, l = 20);
        }
      }
    }
  }
  translate([0, 0, 128]) {
    difference() {
      union() {
        translate([0, 0, 3.5]) {
          zcyl(r = 14, l = 7);
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
                                rotate([0, 0, 0]) {
                                  union() {
                                    union() {
                                      union() {
                                        union() {
                                          hull() {
                                            translate([13, 0, 0]) {
                                              translate([0, 0, 6.108]) {
                                                rotate([72, 0, 0]) {
                                                  cuboid([5.0e-2, 12, 2.6]);
                                                }
                                              }
                                            }
                                            translate([16.8, 0, 0]) {
                                              translate([0, 0, 5.796]) {
                                                rotate([67.2, 0, 0]) {
                                                  cuboid([5.0e-2, 11.6, 2.32]);
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([16.8, 0, 0]) {
                                              translate([0, 0, 5.796]) {
                                                rotate([67.2, 0, 0]) {
                                                  cuboid([5.0e-2, 11.6, 2.32]);
                                                }
                                              }
                                            }
                                            translate([20.6, 0, 0]) {
                                              translate([0, 0, 5.435]) {
                                                rotate([62.4, 0, 0]) {
                                                  cuboid([5.0e-2, 11.2, 2.04]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([20.6, 0, 0]) {
                                            translate([0, 0, 5.435]) {
                                              rotate([62.4, 0, 0]) {
                                                cuboid([5.0e-2, 11.2, 2.04]);
                                              }
                                            }
                                          }
                                          translate([24.4, 0, 0]) {
                                            translate([0, 0, 5.031]) {
                                              rotate([57.6, 0, 0]) {
                                                cuboid([5.0e-2, 10.8, 1.76]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([24.4, 0, 0]) {
                                          translate([0, 0, 5.031]) {
                                            rotate([57.6, 0, 0]) {
                                              cuboid([5.0e-2, 10.8, 1.76]);
                                            }
                                          }
                                        }
                                        translate([28.2, 0, 0]) {
                                          translate([0, 0, 4.589]) {
                                            rotate([52.8, 0, 0]) {
                                              cuboid([5.0e-2, 10.4, 1.48]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([28.2, 0, 0]) {
                                        translate([0, 0, 4.589]) {
                                          rotate([52.8, 0, 0]) {
                                            cuboid([5.0e-2, 10.4, 1.48]);
                                          }
                                        }
                                      }
                                      translate([32, 0, 0]) {
                                        translate([0, 0, 4.117]) {
                                          rotate([48, 0, 0]) {
                                            cuboid([5.0e-2, 10, 1.2]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                rotate([0, 0, 27.692]) {
                                  union() {
                                    union() {
                                      union() {
                                        union() {
                                          hull() {
                                            translate([13, 0, 0]) {
                                              translate([0, 0, 6.108]) {
                                                rotate([72, 0, 0]) {
                                                  cuboid([5.0e-2, 12, 2.6]);
                                                }
                                              }
                                            }
                                            translate([16.8, 0, 0]) {
                                              translate([0, 0, 5.796]) {
                                                rotate([67.2, 0, 0]) {
                                                  cuboid([5.0e-2, 11.6, 2.32]);
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([16.8, 0, 0]) {
                                              translate([0, 0, 5.796]) {
                                                rotate([67.2, 0, 0]) {
                                                  cuboid([5.0e-2, 11.6, 2.32]);
                                                }
                                              }
                                            }
                                            translate([20.6, 0, 0]) {
                                              translate([0, 0, 5.435]) {
                                                rotate([62.4, 0, 0]) {
                                                  cuboid([5.0e-2, 11.2, 2.04]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([20.6, 0, 0]) {
                                            translate([0, 0, 5.435]) {
                                              rotate([62.4, 0, 0]) {
                                                cuboid([5.0e-2, 11.2, 2.04]);
                                              }
                                            }
                                          }
                                          translate([24.4, 0, 0]) {
                                            translate([0, 0, 5.031]) {
                                              rotate([57.6, 0, 0]) {
                                                cuboid([5.0e-2, 10.8, 1.76]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([24.4, 0, 0]) {
                                          translate([0, 0, 5.031]) {
                                            rotate([57.6, 0, 0]) {
                                              cuboid([5.0e-2, 10.8, 1.76]);
                                            }
                                          }
                                        }
                                        translate([28.2, 0, 0]) {
                                          translate([0, 0, 4.589]) {
                                            rotate([52.8, 0, 0]) {
                                              cuboid([5.0e-2, 10.4, 1.48]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([28.2, 0, 0]) {
                                        translate([0, 0, 4.589]) {
                                          rotate([52.8, 0, 0]) {
                                            cuboid([5.0e-2, 10.4, 1.48]);
                                          }
                                        }
                                      }
                                      translate([32, 0, 0]) {
                                        translate([0, 0, 4.117]) {
                                          rotate([48, 0, 0]) {
                                            cuboid([5.0e-2, 10, 1.2]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              rotate([0, 0, 55.385]) {
                                union() {
                                  union() {
                                    union() {
                                      union() {
                                        hull() {
                                          translate([13, 0, 0]) {
                                            translate([0, 0, 6.108]) {
                                              rotate([72, 0, 0]) {
                                                cuboid([5.0e-2, 12, 2.6]);
                                              }
                                            }
                                          }
                                          translate([16.8, 0, 0]) {
                                            translate([0, 0, 5.796]) {
                                              rotate([67.2, 0, 0]) {
                                                cuboid([5.0e-2, 11.6, 2.32]);
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([16.8, 0, 0]) {
                                            translate([0, 0, 5.796]) {
                                              rotate([67.2, 0, 0]) {
                                                cuboid([5.0e-2, 11.6, 2.32]);
                                              }
                                            }
                                          }
                                          translate([20.6, 0, 0]) {
                                            translate([0, 0, 5.435]) {
                                              rotate([62.4, 0, 0]) {
                                                cuboid([5.0e-2, 11.2, 2.04]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([20.6, 0, 0]) {
                                          translate([0, 0, 5.435]) {
                                            rotate([62.4, 0, 0]) {
                                              cuboid([5.0e-2, 11.2, 2.04]);
                                            }
                                          }
                                        }
                                        translate([24.4, 0, 0]) {
                                          translate([0, 0, 5.031]) {
                                            rotate([57.6, 0, 0]) {
                                              cuboid([5.0e-2, 10.8, 1.76]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([24.4, 0, 0]) {
                                        translate([0, 0, 5.031]) {
                                          rotate([57.6, 0, 0]) {
                                            cuboid([5.0e-2, 10.8, 1.76]);
                                          }
                                        }
                                      }
                                      translate([28.2, 0, 0]) {
                                        translate([0, 0, 4.589]) {
                                          rotate([52.8, 0, 0]) {
                                            cuboid([5.0e-2, 10.4, 1.48]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([28.2, 0, 0]) {
                                      translate([0, 0, 4.589]) {
                                        rotate([52.8, 0, 0]) {
                                          cuboid([5.0e-2, 10.4, 1.48]);
                                        }
                                      }
                                    }
                                    translate([32, 0, 0]) {
                                      translate([0, 0, 4.117]) {
                                        rotate([48, 0, 0]) {
                                          cuboid([5.0e-2, 10, 1.2]);
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            rotate([0, 0, 83.077]) {
                              union() {
                                union() {
                                  union() {
                                    union() {
                                      hull() {
                                        translate([13, 0, 0]) {
                                          translate([0, 0, 6.108]) {
                                            rotate([72, 0, 0]) {
                                              cuboid([5.0e-2, 12, 2.6]);
                                            }
                                          }
                                        }
                                        translate([16.8, 0, 0]) {
                                          translate([0, 0, 5.796]) {
                                            rotate([67.2, 0, 0]) {
                                              cuboid([5.0e-2, 11.6, 2.32]);
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([16.8, 0, 0]) {
                                          translate([0, 0, 5.796]) {
                                            rotate([67.2, 0, 0]) {
                                              cuboid([5.0e-2, 11.6, 2.32]);
                                            }
                                          }
                                        }
                                        translate([20.6, 0, 0]) {
                                          translate([0, 0, 5.435]) {
                                            rotate([62.4, 0, 0]) {
                                              cuboid([5.0e-2, 11.2, 2.04]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([20.6, 0, 0]) {
                                        translate([0, 0, 5.435]) {
                                          rotate([62.4, 0, 0]) {
                                            cuboid([5.0e-2, 11.2, 2.04]);
                                          }
                                        }
                                      }
                                      translate([24.4, 0, 0]) {
                                        translate([0, 0, 5.031]) {
                                          rotate([57.6, 0, 0]) {
                                            cuboid([5.0e-2, 10.8, 1.76]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([24.4, 0, 0]) {
                                      translate([0, 0, 5.031]) {
                                        rotate([57.6, 0, 0]) {
                                          cuboid([5.0e-2, 10.8, 1.76]);
                                        }
                                      }
                                    }
                                    translate([28.2, 0, 0]) {
                                      translate([0, 0, 4.589]) {
                                        rotate([52.8, 0, 0]) {
                                          cuboid([5.0e-2, 10.4, 1.48]);
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([28.2, 0, 0]) {
                                    translate([0, 0, 4.589]) {
                                      rotate([52.8, 0, 0]) {
                                        cuboid([5.0e-2, 10.4, 1.48]);
                                      }
                                    }
                                  }
                                  translate([32, 0, 0]) {
                                    translate([0, 0, 4.117]) {
                                      rotate([48, 0, 0]) {
                                        cuboid([5.0e-2, 10, 1.2]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                          rotate([0, 0, 110.769]) {
                            union() {
                              union() {
                                union() {
                                  union() {
                                    hull() {
                                      translate([13, 0, 0]) {
                                        translate([0, 0, 6.108]) {
                                          rotate([72, 0, 0]) {
                                            cuboid([5.0e-2, 12, 2.6]);
                                          }
                                        }
                                      }
                                      translate([16.8, 0, 0]) {
                                        translate([0, 0, 5.796]) {
                                          rotate([67.2, 0, 0]) {
                                            cuboid([5.0e-2, 11.6, 2.32]);
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([16.8, 0, 0]) {
                                        translate([0, 0, 5.796]) {
                                          rotate([67.2, 0, 0]) {
                                            cuboid([5.0e-2, 11.6, 2.32]);
                                          }
                                        }
                                      }
                                      translate([20.6, 0, 0]) {
                                        translate([0, 0, 5.435]) {
                                          rotate([62.4, 0, 0]) {
                                            cuboid([5.0e-2, 11.2, 2.04]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([20.6, 0, 0]) {
                                      translate([0, 0, 5.435]) {
                                        rotate([62.4, 0, 0]) {
                                          cuboid([5.0e-2, 11.2, 2.04]);
                                        }
                                      }
                                    }
                                    translate([24.4, 0, 0]) {
                                      translate([0, 0, 5.031]) {
                                        rotate([57.6, 0, 0]) {
                                          cuboid([5.0e-2, 10.8, 1.76]);
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([24.4, 0, 0]) {
                                    translate([0, 0, 5.031]) {
                                      rotate([57.6, 0, 0]) {
                                        cuboid([5.0e-2, 10.8, 1.76]);
                                      }
                                    }
                                  }
                                  translate([28.2, 0, 0]) {
                                    translate([0, 0, 4.589]) {
                                      rotate([52.8, 0, 0]) {
                                        cuboid([5.0e-2, 10.4, 1.48]);
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([28.2, 0, 0]) {
                                  translate([0, 0, 4.589]) {
                                    rotate([52.8, 0, 0]) {
                                      cuboid([5.0e-2, 10.4, 1.48]);
                                    }
                                  }
                                }
                                translate([32, 0, 0]) {
                                  translate([0, 0, 4.117]) {
                                    rotate([48, 0, 0]) {
                                      cuboid([5.0e-2, 10, 1.2]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                        rotate([0, 0, 138.462]) {
                          union() {
                            union() {
                              union() {
                                union() {
                                  hull() {
                                    translate([13, 0, 0]) {
                                      translate([0, 0, 6.108]) {
                                        rotate([72, 0, 0]) {
                                          cuboid([5.0e-2, 12, 2.6]);
                                        }
                                      }
                                    }
                                    translate([16.8, 0, 0]) {
                                      translate([0, 0, 5.796]) {
                                        rotate([67.2, 0, 0]) {
                                          cuboid([5.0e-2, 11.6, 2.32]);
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([16.8, 0, 0]) {
                                      translate([0, 0, 5.796]) {
                                        rotate([67.2, 0, 0]) {
                                          cuboid([5.0e-2, 11.6, 2.32]);
                                        }
                                      }
                                    }
                                    translate([20.6, 0, 0]) {
                                      translate([0, 0, 5.435]) {
                                        rotate([62.4, 0, 0]) {
                                          cuboid([5.0e-2, 11.2, 2.04]);
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([20.6, 0, 0]) {
                                    translate([0, 0, 5.435]) {
                                      rotate([62.4, 0, 0]) {
                                        cuboid([5.0e-2, 11.2, 2.04]);
                                      }
                                    }
                                  }
                                  translate([24.4, 0, 0]) {
                                    translate([0, 0, 5.031]) {
                                      rotate([57.6, 0, 0]) {
                                        cuboid([5.0e-2, 10.8, 1.76]);
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([24.4, 0, 0]) {
                                  translate([0, 0, 5.031]) {
                                    rotate([57.6, 0, 0]) {
                                      cuboid([5.0e-2, 10.8, 1.76]);
                                    }
                                  }
                                }
                                translate([28.2, 0, 0]) {
                                  translate([0, 0, 4.589]) {
                                    rotate([52.8, 0, 0]) {
                                      cuboid([5.0e-2, 10.4, 1.48]);
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([28.2, 0, 0]) {
                                translate([0, 0, 4.589]) {
                                  rotate([52.8, 0, 0]) {
                                    cuboid([5.0e-2, 10.4, 1.48]);
                                  }
                                }
                              }
                              translate([32, 0, 0]) {
                                translate([0, 0, 4.117]) {
                                  rotate([48, 0, 0]) {
                                    cuboid([5.0e-2, 10, 1.2]);
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                      rotate([0, 0, 166.154]) {
                        union() {
                          union() {
                            union() {
                              union() {
                                hull() {
                                  translate([13, 0, 0]) {
                                    translate([0, 0, 6.108]) {
                                      rotate([72, 0, 0]) {
                                        cuboid([5.0e-2, 12, 2.6]);
                                      }
                                    }
                                  }
                                  translate([16.8, 0, 0]) {
                                    translate([0, 0, 5.796]) {
                                      rotate([67.2, 0, 0]) {
                                        cuboid([5.0e-2, 11.6, 2.32]);
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([16.8, 0, 0]) {
                                    translate([0, 0, 5.796]) {
                                      rotate([67.2, 0, 0]) {
                                        cuboid([5.0e-2, 11.6, 2.32]);
                                      }
                                    }
                                  }
                                  translate([20.6, 0, 0]) {
                                    translate([0, 0, 5.435]) {
                                      rotate([62.4, 0, 0]) {
                                        cuboid([5.0e-2, 11.2, 2.04]);
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([20.6, 0, 0]) {
                                  translate([0, 0, 5.435]) {
                                    rotate([62.4, 0, 0]) {
                                      cuboid([5.0e-2, 11.2, 2.04]);
                                    }
                                  }
                                }
                                translate([24.4, 0, 0]) {
                                  translate([0, 0, 5.031]) {
                                    rotate([57.6, 0, 0]) {
                                      cuboid([5.0e-2, 10.8, 1.76]);
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([24.4, 0, 0]) {
                                translate([0, 0, 5.031]) {
                                  rotate([57.6, 0, 0]) {
                                    cuboid([5.0e-2, 10.8, 1.76]);
                                  }
                                }
                              }
                              translate([28.2, 0, 0]) {
                                translate([0, 0, 4.589]) {
                                  rotate([52.8, 0, 0]) {
                                    cuboid([5.0e-2, 10.4, 1.48]);
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([28.2, 0, 0]) {
                              translate([0, 0, 4.589]) {
                                rotate([52.8, 0, 0]) {
                                  cuboid([5.0e-2, 10.4, 1.48]);
                                }
                              }
                            }
                            translate([32, 0, 0]) {
                              translate([0, 0, 4.117]) {
                                rotate([48, 0, 0]) {
                                  cuboid([5.0e-2, 10, 1.2]);
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                    rotate([0, 0, 193.846]) {
                      union() {
                        union() {
                          union() {
                            union() {
                              hull() {
                                translate([13, 0, 0]) {
                                  translate([0, 0, 6.108]) {
                                    rotate([72, 0, 0]) {
                                      cuboid([5.0e-2, 12, 2.6]);
                                    }
                                  }
                                }
                                translate([16.8, 0, 0]) {
                                  translate([0, 0, 5.796]) {
                                    rotate([67.2, 0, 0]) {
                                      cuboid([5.0e-2, 11.6, 2.32]);
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([16.8, 0, 0]) {
                                  translate([0, 0, 5.796]) {
                                    rotate([67.2, 0, 0]) {
                                      cuboid([5.0e-2, 11.6, 2.32]);
                                    }
                                  }
                                }
                                translate([20.6, 0, 0]) {
                                  translate([0, 0, 5.435]) {
                                    rotate([62.4, 0, 0]) {
                                      cuboid([5.0e-2, 11.2, 2.04]);
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([20.6, 0, 0]) {
                                translate([0, 0, 5.435]) {
                                  rotate([62.4, 0, 0]) {
                                    cuboid([5.0e-2, 11.2, 2.04]);
                                  }
                                }
                              }
                              translate([24.4, 0, 0]) {
                                translate([0, 0, 5.031]) {
                                  rotate([57.6, 0, 0]) {
                                    cuboid([5.0e-2, 10.8, 1.76]);
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([24.4, 0, 0]) {
                              translate([0, 0, 5.031]) {
                                rotate([57.6, 0, 0]) {
                                  cuboid([5.0e-2, 10.8, 1.76]);
                                }
                              }
                            }
                            translate([28.2, 0, 0]) {
                              translate([0, 0, 4.589]) {
                                rotate([52.8, 0, 0]) {
                                  cuboid([5.0e-2, 10.4, 1.48]);
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([28.2, 0, 0]) {
                            translate([0, 0, 4.589]) {
                              rotate([52.8, 0, 0]) {
                                cuboid([5.0e-2, 10.4, 1.48]);
                              }
                            }
                          }
                          translate([32, 0, 0]) {
                            translate([0, 0, 4.117]) {
                              rotate([48, 0, 0]) {
                                cuboid([5.0e-2, 10, 1.2]);
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                  rotate([0, 0, 221.538]) {
                    union() {
                      union() {
                        union() {
                          union() {
                            hull() {
                              translate([13, 0, 0]) {
                                translate([0, 0, 6.108]) {
                                  rotate([72, 0, 0]) {
                                    cuboid([5.0e-2, 12, 2.6]);
                                  }
                                }
                              }
                              translate([16.8, 0, 0]) {
                                translate([0, 0, 5.796]) {
                                  rotate([67.2, 0, 0]) {
                                    cuboid([5.0e-2, 11.6, 2.32]);
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([16.8, 0, 0]) {
                                translate([0, 0, 5.796]) {
                                  rotate([67.2, 0, 0]) {
                                    cuboid([5.0e-2, 11.6, 2.32]);
                                  }
                                }
                              }
                              translate([20.6, 0, 0]) {
                                translate([0, 0, 5.435]) {
                                  rotate([62.4, 0, 0]) {
                                    cuboid([5.0e-2, 11.2, 2.04]);
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([20.6, 0, 0]) {
                              translate([0, 0, 5.435]) {
                                rotate([62.4, 0, 0]) {
                                  cuboid([5.0e-2, 11.2, 2.04]);
                                }
                              }
                            }
                            translate([24.4, 0, 0]) {
                              translate([0, 0, 5.031]) {
                                rotate([57.6, 0, 0]) {
                                  cuboid([5.0e-2, 10.8, 1.76]);
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([24.4, 0, 0]) {
                            translate([0, 0, 5.031]) {
                              rotate([57.6, 0, 0]) {
                                cuboid([5.0e-2, 10.8, 1.76]);
                              }
                            }
                          }
                          translate([28.2, 0, 0]) {
                            translate([0, 0, 4.589]) {
                              rotate([52.8, 0, 0]) {
                                cuboid([5.0e-2, 10.4, 1.48]);
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([28.2, 0, 0]) {
                          translate([0, 0, 4.589]) {
                            rotate([52.8, 0, 0]) {
                              cuboid([5.0e-2, 10.4, 1.48]);
                            }
                          }
                        }
                        translate([32, 0, 0]) {
                          translate([0, 0, 4.117]) {
                            rotate([48, 0, 0]) {
                              cuboid([5.0e-2, 10, 1.2]);
                            }
                          }
                        }
                      }
                    }
                  }
                }
                rotate([0, 0, 249.231]) {
                  union() {
                    union() {
                      union() {
                        union() {
                          hull() {
                            translate([13, 0, 0]) {
                              translate([0, 0, 6.108]) {
                                rotate([72, 0, 0]) {
                                  cuboid([5.0e-2, 12, 2.6]);
                                }
                              }
                            }
                            translate([16.8, 0, 0]) {
                              translate([0, 0, 5.796]) {
                                rotate([67.2, 0, 0]) {
                                  cuboid([5.0e-2, 11.6, 2.32]);
                                }
                              }
                            }
                          }
                          hull() {
                            translate([16.8, 0, 0]) {
                              translate([0, 0, 5.796]) {
                                rotate([67.2, 0, 0]) {
                                  cuboid([5.0e-2, 11.6, 2.32]);
                                }
                              }
                            }
                            translate([20.6, 0, 0]) {
                              translate([0, 0, 5.435]) {
                                rotate([62.4, 0, 0]) {
                                  cuboid([5.0e-2, 11.2, 2.04]);
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([20.6, 0, 0]) {
                            translate([0, 0, 5.435]) {
                              rotate([62.4, 0, 0]) {
                                cuboid([5.0e-2, 11.2, 2.04]);
                              }
                            }
                          }
                          translate([24.4, 0, 0]) {
                            translate([0, 0, 5.031]) {
                              rotate([57.6, 0, 0]) {
                                cuboid([5.0e-2, 10.8, 1.76]);
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([24.4, 0, 0]) {
                          translate([0, 0, 5.031]) {
                            rotate([57.6, 0, 0]) {
                              cuboid([5.0e-2, 10.8, 1.76]);
                            }
                          }
                        }
                        translate([28.2, 0, 0]) {
                          translate([0, 0, 4.589]) {
                            rotate([52.8, 0, 0]) {
                              cuboid([5.0e-2, 10.4, 1.48]);
                            }
                          }
                        }
                      }
                    }
                    hull() {
                      translate([28.2, 0, 0]) {
                        translate([0, 0, 4.589]) {
                          rotate([52.8, 0, 0]) {
                            cuboid([5.0e-2, 10.4, 1.48]);
                          }
                        }
                      }
                      translate([32, 0, 0]) {
                        translate([0, 0, 4.117]) {
                          rotate([48, 0, 0]) {
                            cuboid([5.0e-2, 10, 1.2]);
                          }
                        }
                      }
                    }
                  }
                }
              }
              rotate([0, 0, 276.923]) {
                union() {
                  union() {
                    union() {
                      union() {
                        hull() {
                          translate([13, 0, 0]) {
                            translate([0, 0, 6.108]) {
                              rotate([72, 0, 0]) {
                                cuboid([5.0e-2, 12, 2.6]);
                              }
                            }
                          }
                          translate([16.8, 0, 0]) {
                            translate([0, 0, 5.796]) {
                              rotate([67.2, 0, 0]) {
                                cuboid([5.0e-2, 11.6, 2.32]);
                              }
                            }
                          }
                        }
                        hull() {
                          translate([16.8, 0, 0]) {
                            translate([0, 0, 5.796]) {
                              rotate([67.2, 0, 0]) {
                                cuboid([5.0e-2, 11.6, 2.32]);
                              }
                            }
                          }
                          translate([20.6, 0, 0]) {
                            translate([0, 0, 5.435]) {
                              rotate([62.4, 0, 0]) {
                                cuboid([5.0e-2, 11.2, 2.04]);
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([20.6, 0, 0]) {
                          translate([0, 0, 5.435]) {
                            rotate([62.4, 0, 0]) {
                              cuboid([5.0e-2, 11.2, 2.04]);
                            }
                          }
                        }
                        translate([24.4, 0, 0]) {
                          translate([0, 0, 5.031]) {
                            rotate([57.6, 0, 0]) {
                              cuboid([5.0e-2, 10.8, 1.76]);
                            }
                          }
                        }
                      }
                    }
                    hull() {
                      translate([24.4, 0, 0]) {
                        translate([0, 0, 5.031]) {
                          rotate([57.6, 0, 0]) {
                            cuboid([5.0e-2, 10.8, 1.76]);
                          }
                        }
                      }
                      translate([28.2, 0, 0]) {
                        translate([0, 0, 4.589]) {
                          rotate([52.8, 0, 0]) {
                            cuboid([5.0e-2, 10.4, 1.48]);
                          }
                        }
                      }
                    }
                  }
                  hull() {
                    translate([28.2, 0, 0]) {
                      translate([0, 0, 4.589]) {
                        rotate([52.8, 0, 0]) {
                          cuboid([5.0e-2, 10.4, 1.48]);
                        }
                      }
                    }
                    translate([32, 0, 0]) {
                      translate([0, 0, 4.117]) {
                        rotate([48, 0, 0]) {
                          cuboid([5.0e-2, 10, 1.2]);
                        }
                      }
                    }
                  }
                }
              }
            }
            rotate([0, 0, 304.615]) {
              union() {
                union() {
                  union() {
                    union() {
                      hull() {
                        translate([13, 0, 0]) {
                          translate([0, 0, 6.108]) {
                            rotate([72, 0, 0]) {
                              cuboid([5.0e-2, 12, 2.6]);
                            }
                          }
                        }
                        translate([16.8, 0, 0]) {
                          translate([0, 0, 5.796]) {
                            rotate([67.2, 0, 0]) {
                              cuboid([5.0e-2, 11.6, 2.32]);
                            }
                          }
                        }
                      }
                      hull() {
                        translate([16.8, 0, 0]) {
                          translate([0, 0, 5.796]) {
                            rotate([67.2, 0, 0]) {
                              cuboid([5.0e-2, 11.6, 2.32]);
                            }
                          }
                        }
                        translate([20.6, 0, 0]) {
                          translate([0, 0, 5.435]) {
                            rotate([62.4, 0, 0]) {
                              cuboid([5.0e-2, 11.2, 2.04]);
                            }
                          }
                        }
                      }
                    }
                    hull() {
                      translate([20.6, 0, 0]) {
                        translate([0, 0, 5.435]) {
                          rotate([62.4, 0, 0]) {
                            cuboid([5.0e-2, 11.2, 2.04]);
                          }
                        }
                      }
                      translate([24.4, 0, 0]) {
                        translate([0, 0, 5.031]) {
                          rotate([57.6, 0, 0]) {
                            cuboid([5.0e-2, 10.8, 1.76]);
                          }
                        }
                      }
                    }
                  }
                  hull() {
                    translate([24.4, 0, 0]) {
                      translate([0, 0, 5.031]) {
                        rotate([57.6, 0, 0]) {
                          cuboid([5.0e-2, 10.8, 1.76]);
                        }
                      }
                    }
                    translate([28.2, 0, 0]) {
                      translate([0, 0, 4.589]) {
                        rotate([52.8, 0, 0]) {
                          cuboid([5.0e-2, 10.4, 1.48]);
                        }
                      }
                    }
                  }
                }
                hull() {
                  translate([28.2, 0, 0]) {
                    translate([0, 0, 4.589]) {
                      rotate([52.8, 0, 0]) {
                        cuboid([5.0e-2, 10.4, 1.48]);
                      }
                    }
                  }
                  translate([32, 0, 0]) {
                    translate([0, 0, 4.117]) {
                      rotate([48, 0, 0]) {
                        cuboid([5.0e-2, 10, 1.2]);
                      }
                    }
                  }
                }
              }
            }
          }
          rotate([0, 0, 332.308]) {
            union() {
              union() {
                union() {
                  union() {
                    hull() {
                      translate([13, 0, 0]) {
                        translate([0, 0, 6.108]) {
                          rotate([72, 0, 0]) {
                            cuboid([5.0e-2, 12, 2.6]);
                          }
                        }
                      }
                      translate([16.8, 0, 0]) {
                        translate([0, 0, 5.796]) {
                          rotate([67.2, 0, 0]) {
                            cuboid([5.0e-2, 11.6, 2.32]);
                          }
                        }
                      }
                    }
                    hull() {
                      translate([16.8, 0, 0]) {
                        translate([0, 0, 5.796]) {
                          rotate([67.2, 0, 0]) {
                            cuboid([5.0e-2, 11.6, 2.32]);
                          }
                        }
                      }
                      translate([20.6, 0, 0]) {
                        translate([0, 0, 5.435]) {
                          rotate([62.4, 0, 0]) {
                            cuboid([5.0e-2, 11.2, 2.04]);
                          }
                        }
                      }
                    }
                  }
                  hull() {
                    translate([20.6, 0, 0]) {
                      translate([0, 0, 5.435]) {
                        rotate([62.4, 0, 0]) {
                          cuboid([5.0e-2, 11.2, 2.04]);
                        }
                      }
                    }
                    translate([24.4, 0, 0]) {
                      translate([0, 0, 5.031]) {
                        rotate([57.6, 0, 0]) {
                          cuboid([5.0e-2, 10.8, 1.76]);
                        }
                      }
                    }
                  }
                }
                hull() {
                  translate([24.4, 0, 0]) {
                    translate([0, 0, 5.031]) {
                      rotate([57.6, 0, 0]) {
                        cuboid([5.0e-2, 10.8, 1.76]);
                      }
                    }
                  }
                  translate([28.2, 0, 0]) {
                    translate([0, 0, 4.589]) {
                      rotate([52.8, 0, 0]) {
                        cuboid([5.0e-2, 10.4, 1.48]);
                      }
                    }
                  }
                }
              }
              hull() {
                translate([28.2, 0, 0]) {
                  translate([0, 0, 4.589]) {
                    rotate([52.8, 0, 0]) {
                      cuboid([5.0e-2, 10.4, 1.48]);
                    }
                  }
                }
                translate([32, 0, 0]) {
                  translate([0, 0, 4.117]) {
                    rotate([48, 0, 0]) {
                      cuboid([5.0e-2, 10, 1.2]);
                    }
                  }
                }
              }
            }
          }
        }
      }
      translate([0, 0, 3.5]) {
        zcyl(r = 4.05, l = 20);
      }
    }
  }
}
$fn = 50;