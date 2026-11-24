include <BOSL2/std.scad>

union() {
  union() {
    union() {
      union() {
        union() {
          union() {
            union() {
              translate([0, 0, 8.25]) {
                zcyl(r = 3.05, l = 16.5);
              }
              hull() {
                translate([0, 0, 16.01]) {
                  zcyl(r = 3.05, l = 2.0e-2);
                }
                translate([0, 0, 16.99]) {
                  zcyl(r = 4, l = 2.0e-2);
                }
              }
            }
            translate([0, 0, 50.5]) {
              zcyl(r = 4, l = 68);
            }
          }
          translate([0, 0, 89.25]) {
            zcyl(r = 3.05, l = 10.5);
          }
        }
        union() {
          hull() {
            translate([0, 0, 38.01]) {
              zcyl(r = 4, l = 2.0e-2);
            }
            translate([0, 0, 39.99]) {
              zcyl(r = 6, l = 2.0e-2);
            }
          }
          translate([0, 0, 42.99]) {
            zcyl(r = 6, l = 6.02);
          }
        }
      }
      intersection() {
        translate([0, 0, -2]) {
          union() {
            union() {
              union() {
                union() {
                  union() {
                    union() {
                      translate([0, 0, 0]) {
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
                                                                      hull() {
                                                                        translate([0, 0, 0]) {
                                                                          rotate([0, 0, 0]) {
                                                                            hull() {
                                                                              translate([3.02, 0, 0]) {
                                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                              }
                                                                              translate([4, 0, 0]) {
                                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                        translate([0, 0, 0.125]) {
                                                                          rotate([0, 0, 15]) {
                                                                            hull() {
                                                                              translate([3.02, 0, 0]) {
                                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                              }
                                                                              translate([4, 0, 0]) {
                                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                      hull() {
                                                                        translate([0, 0, 0.125]) {
                                                                          rotate([0, 0, 15]) {
                                                                            hull() {
                                                                              translate([3.02, 0, 0]) {
                                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                              }
                                                                              translate([4, 0, 0]) {
                                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                        translate([0, 0, 0.25]) {
                                                                          rotate([0, 0, 30]) {
                                                                            hull() {
                                                                              translate([3.02, 0, 0]) {
                                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                              }
                                                                              translate([4, 0, 0]) {
                                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                    hull() {
                                                                      translate([0, 0, 0.25]) {
                                                                        rotate([0, 0, 30]) {
                                                                          hull() {
                                                                            translate([3.02, 0, 0]) {
                                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                            }
                                                                            translate([4, 0, 0]) {
                                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                      translate([0, 0, 0.375]) {
                                                                        rotate([0, 0, 45]) {
                                                                          hull() {
                                                                            translate([3.02, 0, 0]) {
                                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                            }
                                                                            translate([4, 0, 0]) {
                                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                  hull() {
                                                                    translate([0, 0, 0.375]) {
                                                                      rotate([0, 0, 45]) {
                                                                        hull() {
                                                                          translate([3.02, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                          }
                                                                          translate([4, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                    translate([0, 0, 0.5]) {
                                                                      rotate([0, 0, 60]) {
                                                                        hull() {
                                                                          translate([3.02, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                          }
                                                                          translate([4, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                hull() {
                                                                  translate([0, 0, 0.5]) {
                                                                    rotate([0, 0, 60]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                  translate([0, 0, 0.625]) {
                                                                    rotate([0, 0, 75]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              hull() {
                                                                translate([0, 0, 0.625]) {
                                                                  rotate([0, 0, 75]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                translate([0, 0, 0.75]) {
                                                                  rotate([0, 0, 90]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            hull() {
                                                              translate([0, 0, 0.75]) {
                                                                rotate([0, 0, 90]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              translate([0, 0, 0.875]) {
                                                                rotate([0, 0, 105]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                          hull() {
                                                            translate([0, 0, 0.875]) {
                                                              rotate([0, 0, 105]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            translate([0, 0, 1]) {
                                                              rotate([0, 0, 120]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                        hull() {
                                                          translate([0, 0, 1]) {
                                                            rotate([0, 0, 120]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                          translate([0, 0, 1.125]) {
                                                            rotate([0, 0, 135]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                      hull() {
                                                        translate([0, 0, 1.125]) {
                                                          rotate([0, 0, 135]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                        translate([0, 0, 1.25]) {
                                                          rotate([0, 0, 150]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                    hull() {
                                                      translate([0, 0, 1.25]) {
                                                        rotate([0, 0, 150]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                      translate([0, 0, 1.375]) {
                                                        rotate([0, 0, 165]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                  hull() {
                                                    translate([0, 0, 1.375]) {
                                                      rotate([0, 0, 165]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                    translate([0, 0, 1.5]) {
                                                      rotate([0, 0, 180]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                                hull() {
                                                  translate([0, 0, 1.5]) {
                                                    rotate([0, 0, 180]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                  translate([0, 0, 1.625]) {
                                                    rotate([0, 0, 195]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                              hull() {
                                                translate([0, 0, 1.625]) {
                                                  rotate([0, 0, 195]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                                translate([0, 0, 1.75]) {
                                                  rotate([0, 0, 210]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([0, 0, 1.75]) {
                                                rotate([0, 0, 210]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                              translate([0, 0, 1.875]) {
                                                rotate([0, 0, 225]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([0, 0, 1.875]) {
                                              rotate([0, 0, 225]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                            translate([0, 0, 2]) {
                                              rotate([0, 0, 240]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([0, 0, 2]) {
                                            rotate([0, 0, 240]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                          translate([0, 0, 2.125]) {
                                            rotate([0, 0, 255]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([0, 0, 2.125]) {
                                          rotate([0, 0, 255]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                        translate([0, 0, 2.25]) {
                                          rotate([0, 0, 270]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([0, 0, 2.25]) {
                                        rotate([0, 0, 270]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                      translate([0, 0, 2.375]) {
                                        rotate([0, 0, 285]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([0, 0, 2.375]) {
                                      rotate([0, 0, 285]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                    translate([0, 0, 2.5]) {
                                      rotate([0, 0, 300]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([0, 0, 2.5]) {
                                    rotate([0, 0, 300]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                  translate([0, 0, 2.625]) {
                                    rotate([0, 0, 315]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([0, 0, 2.625]) {
                                  rotate([0, 0, 315]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                                translate([0, 0, 2.75]) {
                                  rotate([0, 0, 330]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([0, 0, 2.75]) {
                                rotate([0, 0, 330]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                              translate([0, 0, 2.875]) {
                                rotate([0, 0, 345]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([0, 0, 2.875]) {
                              rotate([0, 0, 345]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                            translate([0, 0, 3]) {
                              rotate([0, 0, 360]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                      translate([0, 0, 3]) {
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
                                                                      hull() {
                                                                        translate([0, 0, 0]) {
                                                                          rotate([0, 0, 0]) {
                                                                            hull() {
                                                                              translate([3.02, 0, 0]) {
                                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                              }
                                                                              translate([4, 0, 0]) {
                                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                        translate([0, 0, 0.125]) {
                                                                          rotate([0, 0, 15]) {
                                                                            hull() {
                                                                              translate([3.02, 0, 0]) {
                                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                              }
                                                                              translate([4, 0, 0]) {
                                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                      hull() {
                                                                        translate([0, 0, 0.125]) {
                                                                          rotate([0, 0, 15]) {
                                                                            hull() {
                                                                              translate([3.02, 0, 0]) {
                                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                              }
                                                                              translate([4, 0, 0]) {
                                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                        translate([0, 0, 0.25]) {
                                                                          rotate([0, 0, 30]) {
                                                                            hull() {
                                                                              translate([3.02, 0, 0]) {
                                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                              }
                                                                              translate([4, 0, 0]) {
                                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                    hull() {
                                                                      translate([0, 0, 0.25]) {
                                                                        rotate([0, 0, 30]) {
                                                                          hull() {
                                                                            translate([3.02, 0, 0]) {
                                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                            }
                                                                            translate([4, 0, 0]) {
                                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                      translate([0, 0, 0.375]) {
                                                                        rotate([0, 0, 45]) {
                                                                          hull() {
                                                                            translate([3.02, 0, 0]) {
                                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                            }
                                                                            translate([4, 0, 0]) {
                                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                  hull() {
                                                                    translate([0, 0, 0.375]) {
                                                                      rotate([0, 0, 45]) {
                                                                        hull() {
                                                                          translate([3.02, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                          }
                                                                          translate([4, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                    translate([0, 0, 0.5]) {
                                                                      rotate([0, 0, 60]) {
                                                                        hull() {
                                                                          translate([3.02, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                          }
                                                                          translate([4, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                hull() {
                                                                  translate([0, 0, 0.5]) {
                                                                    rotate([0, 0, 60]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                  translate([0, 0, 0.625]) {
                                                                    rotate([0, 0, 75]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              hull() {
                                                                translate([0, 0, 0.625]) {
                                                                  rotate([0, 0, 75]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                translate([0, 0, 0.75]) {
                                                                  rotate([0, 0, 90]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            hull() {
                                                              translate([0, 0, 0.75]) {
                                                                rotate([0, 0, 90]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              translate([0, 0, 0.875]) {
                                                                rotate([0, 0, 105]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                          hull() {
                                                            translate([0, 0, 0.875]) {
                                                              rotate([0, 0, 105]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            translate([0, 0, 1]) {
                                                              rotate([0, 0, 120]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                        hull() {
                                                          translate([0, 0, 1]) {
                                                            rotate([0, 0, 120]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                          translate([0, 0, 1.125]) {
                                                            rotate([0, 0, 135]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                      hull() {
                                                        translate([0, 0, 1.125]) {
                                                          rotate([0, 0, 135]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                        translate([0, 0, 1.25]) {
                                                          rotate([0, 0, 150]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                    hull() {
                                                      translate([0, 0, 1.25]) {
                                                        rotate([0, 0, 150]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                      translate([0, 0, 1.375]) {
                                                        rotate([0, 0, 165]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                  hull() {
                                                    translate([0, 0, 1.375]) {
                                                      rotate([0, 0, 165]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                    translate([0, 0, 1.5]) {
                                                      rotate([0, 0, 180]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                                hull() {
                                                  translate([0, 0, 1.5]) {
                                                    rotate([0, 0, 180]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                  translate([0, 0, 1.625]) {
                                                    rotate([0, 0, 195]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                              hull() {
                                                translate([0, 0, 1.625]) {
                                                  rotate([0, 0, 195]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                                translate([0, 0, 1.75]) {
                                                  rotate([0, 0, 210]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([0, 0, 1.75]) {
                                                rotate([0, 0, 210]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                              translate([0, 0, 1.875]) {
                                                rotate([0, 0, 225]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([0, 0, 1.875]) {
                                              rotate([0, 0, 225]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                            translate([0, 0, 2]) {
                                              rotate([0, 0, 240]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([0, 0, 2]) {
                                            rotate([0, 0, 240]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                          translate([0, 0, 2.125]) {
                                            rotate([0, 0, 255]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([0, 0, 2.125]) {
                                          rotate([0, 0, 255]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                        translate([0, 0, 2.25]) {
                                          rotate([0, 0, 270]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([0, 0, 2.25]) {
                                        rotate([0, 0, 270]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                      translate([0, 0, 2.375]) {
                                        rotate([0, 0, 285]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([0, 0, 2.375]) {
                                      rotate([0, 0, 285]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                    translate([0, 0, 2.5]) {
                                      rotate([0, 0, 300]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([0, 0, 2.5]) {
                                    rotate([0, 0, 300]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                  translate([0, 0, 2.625]) {
                                    rotate([0, 0, 315]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([0, 0, 2.625]) {
                                  rotate([0, 0, 315]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                                translate([0, 0, 2.75]) {
                                  rotate([0, 0, 330]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([0, 0, 2.75]) {
                                rotate([0, 0, 330]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                              translate([0, 0, 2.875]) {
                                rotate([0, 0, 345]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([0, 0, 2.875]) {
                              rotate([0, 0, 345]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                            translate([0, 0, 3]) {
                              rotate([0, 0, 360]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                    translate([0, 0, 6]) {
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
                                                                    hull() {
                                                                      translate([0, 0, 0]) {
                                                                        rotate([0, 0, 0]) {
                                                                          hull() {
                                                                            translate([3.02, 0, 0]) {
                                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                            }
                                                                            translate([4, 0, 0]) {
                                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                      translate([0, 0, 0.125]) {
                                                                        rotate([0, 0, 15]) {
                                                                          hull() {
                                                                            translate([3.02, 0, 0]) {
                                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                            }
                                                                            translate([4, 0, 0]) {
                                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                    hull() {
                                                                      translate([0, 0, 0.125]) {
                                                                        rotate([0, 0, 15]) {
                                                                          hull() {
                                                                            translate([3.02, 0, 0]) {
                                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                            }
                                                                            translate([4, 0, 0]) {
                                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                      translate([0, 0, 0.25]) {
                                                                        rotate([0, 0, 30]) {
                                                                          hull() {
                                                                            translate([3.02, 0, 0]) {
                                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                            }
                                                                            translate([4, 0, 0]) {
                                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                  hull() {
                                                                    translate([0, 0, 0.25]) {
                                                                      rotate([0, 0, 30]) {
                                                                        hull() {
                                                                          translate([3.02, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                          }
                                                                          translate([4, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                    translate([0, 0, 0.375]) {
                                                                      rotate([0, 0, 45]) {
                                                                        hull() {
                                                                          translate([3.02, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                          }
                                                                          translate([4, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                hull() {
                                                                  translate([0, 0, 0.375]) {
                                                                    rotate([0, 0, 45]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                  translate([0, 0, 0.5]) {
                                                                    rotate([0, 0, 60]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              hull() {
                                                                translate([0, 0, 0.5]) {
                                                                  rotate([0, 0, 60]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                translate([0, 0, 0.625]) {
                                                                  rotate([0, 0, 75]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            hull() {
                                                              translate([0, 0, 0.625]) {
                                                                rotate([0, 0, 75]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              translate([0, 0, 0.75]) {
                                                                rotate([0, 0, 90]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                          hull() {
                                                            translate([0, 0, 0.75]) {
                                                              rotate([0, 0, 90]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            translate([0, 0, 0.875]) {
                                                              rotate([0, 0, 105]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                        hull() {
                                                          translate([0, 0, 0.875]) {
                                                            rotate([0, 0, 105]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                          translate([0, 0, 1]) {
                                                            rotate([0, 0, 120]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                      hull() {
                                                        translate([0, 0, 1]) {
                                                          rotate([0, 0, 120]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                        translate([0, 0, 1.125]) {
                                                          rotate([0, 0, 135]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                    hull() {
                                                      translate([0, 0, 1.125]) {
                                                        rotate([0, 0, 135]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                      translate([0, 0, 1.25]) {
                                                        rotate([0, 0, 150]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                  hull() {
                                                    translate([0, 0, 1.25]) {
                                                      rotate([0, 0, 150]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                    translate([0, 0, 1.375]) {
                                                      rotate([0, 0, 165]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                                hull() {
                                                  translate([0, 0, 1.375]) {
                                                    rotate([0, 0, 165]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                  translate([0, 0, 1.5]) {
                                                    rotate([0, 0, 180]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                              hull() {
                                                translate([0, 0, 1.5]) {
                                                  rotate([0, 0, 180]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                                translate([0, 0, 1.625]) {
                                                  rotate([0, 0, 195]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([0, 0, 1.625]) {
                                                rotate([0, 0, 195]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                              translate([0, 0, 1.75]) {
                                                rotate([0, 0, 210]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([0, 0, 1.75]) {
                                              rotate([0, 0, 210]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                            translate([0, 0, 1.875]) {
                                              rotate([0, 0, 225]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([0, 0, 1.875]) {
                                            rotate([0, 0, 225]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                          translate([0, 0, 2]) {
                                            rotate([0, 0, 240]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([0, 0, 2]) {
                                          rotate([0, 0, 240]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                        translate([0, 0, 2.125]) {
                                          rotate([0, 0, 255]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([0, 0, 2.125]) {
                                        rotate([0, 0, 255]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                      translate([0, 0, 2.25]) {
                                        rotate([0, 0, 270]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([0, 0, 2.25]) {
                                      rotate([0, 0, 270]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                    translate([0, 0, 2.375]) {
                                      rotate([0, 0, 285]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([0, 0, 2.375]) {
                                    rotate([0, 0, 285]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                  translate([0, 0, 2.5]) {
                                    rotate([0, 0, 300]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([0, 0, 2.5]) {
                                  rotate([0, 0, 300]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                                translate([0, 0, 2.625]) {
                                  rotate([0, 0, 315]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([0, 0, 2.625]) {
                                rotate([0, 0, 315]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                              translate([0, 0, 2.75]) {
                                rotate([0, 0, 330]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([0, 0, 2.75]) {
                              rotate([0, 0, 330]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                            translate([0, 0, 2.875]) {
                              rotate([0, 0, 345]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([0, 0, 2.875]) {
                            rotate([0, 0, 345]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                          translate([0, 0, 3]) {
                            rotate([0, 0, 360]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                  translate([0, 0, 9]) {
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
                                                                  hull() {
                                                                    translate([0, 0, 0]) {
                                                                      rotate([0, 0, 0]) {
                                                                        hull() {
                                                                          translate([3.02, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                          }
                                                                          translate([4, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                    translate([0, 0, 0.125]) {
                                                                      rotate([0, 0, 15]) {
                                                                        hull() {
                                                                          translate([3.02, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                          }
                                                                          translate([4, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                  hull() {
                                                                    translate([0, 0, 0.125]) {
                                                                      rotate([0, 0, 15]) {
                                                                        hull() {
                                                                          translate([3.02, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                          }
                                                                          translate([4, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                    translate([0, 0, 0.25]) {
                                                                      rotate([0, 0, 30]) {
                                                                        hull() {
                                                                          translate([3.02, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                          }
                                                                          translate([4, 0, 0]) {
                                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                hull() {
                                                                  translate([0, 0, 0.25]) {
                                                                    rotate([0, 0, 30]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                  translate([0, 0, 0.375]) {
                                                                    rotate([0, 0, 45]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              hull() {
                                                                translate([0, 0, 0.375]) {
                                                                  rotate([0, 0, 45]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                translate([0, 0, 0.5]) {
                                                                  rotate([0, 0, 60]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            hull() {
                                                              translate([0, 0, 0.5]) {
                                                                rotate([0, 0, 60]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              translate([0, 0, 0.625]) {
                                                                rotate([0, 0, 75]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                          hull() {
                                                            translate([0, 0, 0.625]) {
                                                              rotate([0, 0, 75]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            translate([0, 0, 0.75]) {
                                                              rotate([0, 0, 90]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                        hull() {
                                                          translate([0, 0, 0.75]) {
                                                            rotate([0, 0, 90]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                          translate([0, 0, 0.875]) {
                                                            rotate([0, 0, 105]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                      hull() {
                                                        translate([0, 0, 0.875]) {
                                                          rotate([0, 0, 105]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                        translate([0, 0, 1]) {
                                                          rotate([0, 0, 120]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                    hull() {
                                                      translate([0, 0, 1]) {
                                                        rotate([0, 0, 120]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                      translate([0, 0, 1.125]) {
                                                        rotate([0, 0, 135]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                  hull() {
                                                    translate([0, 0, 1.125]) {
                                                      rotate([0, 0, 135]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                    translate([0, 0, 1.25]) {
                                                      rotate([0, 0, 150]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                                hull() {
                                                  translate([0, 0, 1.25]) {
                                                    rotate([0, 0, 150]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                  translate([0, 0, 1.375]) {
                                                    rotate([0, 0, 165]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                              hull() {
                                                translate([0, 0, 1.375]) {
                                                  rotate([0, 0, 165]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                                translate([0, 0, 1.5]) {
                                                  rotate([0, 0, 180]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([0, 0, 1.5]) {
                                                rotate([0, 0, 180]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                              translate([0, 0, 1.625]) {
                                                rotate([0, 0, 195]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([0, 0, 1.625]) {
                                              rotate([0, 0, 195]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                            translate([0, 0, 1.75]) {
                                              rotate([0, 0, 210]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([0, 0, 1.75]) {
                                            rotate([0, 0, 210]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                          translate([0, 0, 1.875]) {
                                            rotate([0, 0, 225]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([0, 0, 1.875]) {
                                          rotate([0, 0, 225]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                        translate([0, 0, 2]) {
                                          rotate([0, 0, 240]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([0, 0, 2]) {
                                        rotate([0, 0, 240]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                      translate([0, 0, 2.125]) {
                                        rotate([0, 0, 255]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([0, 0, 2.125]) {
                                      rotate([0, 0, 255]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                    translate([0, 0, 2.25]) {
                                      rotate([0, 0, 270]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([0, 0, 2.25]) {
                                    rotate([0, 0, 270]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                  translate([0, 0, 2.375]) {
                                    rotate([0, 0, 285]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([0, 0, 2.375]) {
                                  rotate([0, 0, 285]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                                translate([0, 0, 2.5]) {
                                  rotate([0, 0, 300]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([0, 0, 2.5]) {
                                rotate([0, 0, 300]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                              translate([0, 0, 2.625]) {
                                rotate([0, 0, 315]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([0, 0, 2.625]) {
                              rotate([0, 0, 315]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                            translate([0, 0, 2.75]) {
                              rotate([0, 0, 330]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([0, 0, 2.75]) {
                            rotate([0, 0, 330]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                          translate([0, 0, 2.875]) {
                            rotate([0, 0, 345]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([0, 0, 2.875]) {
                          rotate([0, 0, 345]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                        translate([0, 0, 3]) {
                          rotate([0, 0, 360]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
                translate([0, 0, 12]) {
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
                                                                hull() {
                                                                  translate([0, 0, 0]) {
                                                                    rotate([0, 0, 0]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                  translate([0, 0, 0.125]) {
                                                                    rotate([0, 0, 15]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                hull() {
                                                                  translate([0, 0, 0.125]) {
                                                                    rotate([0, 0, 15]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                  translate([0, 0, 0.25]) {
                                                                    rotate([0, 0, 30]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              hull() {
                                                                translate([0, 0, 0.25]) {
                                                                  rotate([0, 0, 30]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                translate([0, 0, 0.375]) {
                                                                  rotate([0, 0, 45]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            hull() {
                                                              translate([0, 0, 0.375]) {
                                                                rotate([0, 0, 45]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              translate([0, 0, 0.5]) {
                                                                rotate([0, 0, 60]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                          hull() {
                                                            translate([0, 0, 0.5]) {
                                                              rotate([0, 0, 60]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            translate([0, 0, 0.625]) {
                                                              rotate([0, 0, 75]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                        hull() {
                                                          translate([0, 0, 0.625]) {
                                                            rotate([0, 0, 75]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                          translate([0, 0, 0.75]) {
                                                            rotate([0, 0, 90]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                      hull() {
                                                        translate([0, 0, 0.75]) {
                                                          rotate([0, 0, 90]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                        translate([0, 0, 0.875]) {
                                                          rotate([0, 0, 105]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                    hull() {
                                                      translate([0, 0, 0.875]) {
                                                        rotate([0, 0, 105]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                      translate([0, 0, 1]) {
                                                        rotate([0, 0, 120]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                  hull() {
                                                    translate([0, 0, 1]) {
                                                      rotate([0, 0, 120]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                    translate([0, 0, 1.125]) {
                                                      rotate([0, 0, 135]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                                hull() {
                                                  translate([0, 0, 1.125]) {
                                                    rotate([0, 0, 135]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                  translate([0, 0, 1.25]) {
                                                    rotate([0, 0, 150]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                              hull() {
                                                translate([0, 0, 1.25]) {
                                                  rotate([0, 0, 150]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                                translate([0, 0, 1.375]) {
                                                  rotate([0, 0, 165]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([0, 0, 1.375]) {
                                                rotate([0, 0, 165]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                              translate([0, 0, 1.5]) {
                                                rotate([0, 0, 180]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([0, 0, 1.5]) {
                                              rotate([0, 0, 180]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                            translate([0, 0, 1.625]) {
                                              rotate([0, 0, 195]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([0, 0, 1.625]) {
                                            rotate([0, 0, 195]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                          translate([0, 0, 1.75]) {
                                            rotate([0, 0, 210]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([0, 0, 1.75]) {
                                          rotate([0, 0, 210]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                        translate([0, 0, 1.875]) {
                                          rotate([0, 0, 225]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([0, 0, 1.875]) {
                                        rotate([0, 0, 225]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                      translate([0, 0, 2]) {
                                        rotate([0, 0, 240]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([0, 0, 2]) {
                                      rotate([0, 0, 240]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                    translate([0, 0, 2.125]) {
                                      rotate([0, 0, 255]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([0, 0, 2.125]) {
                                    rotate([0, 0, 255]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                  translate([0, 0, 2.25]) {
                                    rotate([0, 0, 270]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([0, 0, 2.25]) {
                                  rotate([0, 0, 270]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                                translate([0, 0, 2.375]) {
                                  rotate([0, 0, 285]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([0, 0, 2.375]) {
                                rotate([0, 0, 285]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                              translate([0, 0, 2.5]) {
                                rotate([0, 0, 300]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([0, 0, 2.5]) {
                              rotate([0, 0, 300]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                            translate([0, 0, 2.625]) {
                              rotate([0, 0, 315]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([0, 0, 2.625]) {
                            rotate([0, 0, 315]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                          translate([0, 0, 2.75]) {
                            rotate([0, 0, 330]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([0, 0, 2.75]) {
                          rotate([0, 0, 330]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                        translate([0, 0, 2.875]) {
                          rotate([0, 0, 345]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                      }
                    }
                    hull() {
                      translate([0, 0, 2.875]) {
                        rotate([0, 0, 345]) {
                          hull() {
                            translate([3.02, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                            }
                            translate([4, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                            }
                          }
                        }
                      }
                      translate([0, 0, 3]) {
                        rotate([0, 0, 360]) {
                          hull() {
                            translate([3.02, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                            }
                            translate([4, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
              translate([0, 0, 15]) {
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
                                                              hull() {
                                                                translate([0, 0, 0]) {
                                                                  rotate([0, 0, 0]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                translate([0, 0, 0.125]) {
                                                                  rotate([0, 0, 15]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              hull() {
                                                                translate([0, 0, 0.125]) {
                                                                  rotate([0, 0, 15]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                translate([0, 0, 0.25]) {
                                                                  rotate([0, 0, 30]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            hull() {
                                                              translate([0, 0, 0.25]) {
                                                                rotate([0, 0, 30]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              translate([0, 0, 0.375]) {
                                                                rotate([0, 0, 45]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                          hull() {
                                                            translate([0, 0, 0.375]) {
                                                              rotate([0, 0, 45]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            translate([0, 0, 0.5]) {
                                                              rotate([0, 0, 60]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                        hull() {
                                                          translate([0, 0, 0.5]) {
                                                            rotate([0, 0, 60]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                          translate([0, 0, 0.625]) {
                                                            rotate([0, 0, 75]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                      hull() {
                                                        translate([0, 0, 0.625]) {
                                                          rotate([0, 0, 75]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                        translate([0, 0, 0.75]) {
                                                          rotate([0, 0, 90]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                    hull() {
                                                      translate([0, 0, 0.75]) {
                                                        rotate([0, 0, 90]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                      translate([0, 0, 0.875]) {
                                                        rotate([0, 0, 105]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                  hull() {
                                                    translate([0, 0, 0.875]) {
                                                      rotate([0, 0, 105]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                    translate([0, 0, 1]) {
                                                      rotate([0, 0, 120]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                                hull() {
                                                  translate([0, 0, 1]) {
                                                    rotate([0, 0, 120]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                  translate([0, 0, 1.125]) {
                                                    rotate([0, 0, 135]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                              hull() {
                                                translate([0, 0, 1.125]) {
                                                  rotate([0, 0, 135]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                                translate([0, 0, 1.25]) {
                                                  rotate([0, 0, 150]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([0, 0, 1.25]) {
                                                rotate([0, 0, 150]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                              translate([0, 0, 1.375]) {
                                                rotate([0, 0, 165]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([0, 0, 1.375]) {
                                              rotate([0, 0, 165]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                            translate([0, 0, 1.5]) {
                                              rotate([0, 0, 180]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([0, 0, 1.5]) {
                                            rotate([0, 0, 180]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                          translate([0, 0, 1.625]) {
                                            rotate([0, 0, 195]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([0, 0, 1.625]) {
                                          rotate([0, 0, 195]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                        translate([0, 0, 1.75]) {
                                          rotate([0, 0, 210]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([0, 0, 1.75]) {
                                        rotate([0, 0, 210]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                      translate([0, 0, 1.875]) {
                                        rotate([0, 0, 225]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([0, 0, 1.875]) {
                                      rotate([0, 0, 225]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                    translate([0, 0, 2]) {
                                      rotate([0, 0, 240]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([0, 0, 2]) {
                                    rotate([0, 0, 240]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                  translate([0, 0, 2.125]) {
                                    rotate([0, 0, 255]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([0, 0, 2.125]) {
                                  rotate([0, 0, 255]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                                translate([0, 0, 2.25]) {
                                  rotate([0, 0, 270]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([0, 0, 2.25]) {
                                rotate([0, 0, 270]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                              translate([0, 0, 2.375]) {
                                rotate([0, 0, 285]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([0, 0, 2.375]) {
                              rotate([0, 0, 285]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                            translate([0, 0, 2.5]) {
                              rotate([0, 0, 300]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([0, 0, 2.5]) {
                            rotate([0, 0, 300]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                          translate([0, 0, 2.625]) {
                            rotate([0, 0, 315]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([0, 0, 2.625]) {
                          rotate([0, 0, 315]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                        translate([0, 0, 2.75]) {
                          rotate([0, 0, 330]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                      }
                    }
                    hull() {
                      translate([0, 0, 2.75]) {
                        rotate([0, 0, 330]) {
                          hull() {
                            translate([3.02, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                            }
                            translate([4, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                            }
                          }
                        }
                      }
                      translate([0, 0, 2.875]) {
                        rotate([0, 0, 345]) {
                          hull() {
                            translate([3.02, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                            }
                            translate([4, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                            }
                          }
                        }
                      }
                    }
                  }
                  hull() {
                    translate([0, 0, 2.875]) {
                      rotate([0, 0, 345]) {
                        hull() {
                          translate([3.02, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                          }
                          translate([4, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                          }
                        }
                      }
                    }
                    translate([0, 0, 3]) {
                      rotate([0, 0, 360]) {
                        hull() {
                          translate([3.02, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                          }
                          translate([4, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
            translate([0, 0, 18]) {
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
                                                            hull() {
                                                              translate([0, 0, 0]) {
                                                                rotate([0, 0, 0]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              translate([0, 0, 0.125]) {
                                                                rotate([0, 0, 15]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            hull() {
                                                              translate([0, 0, 0.125]) {
                                                                rotate([0, 0, 15]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              translate([0, 0, 0.25]) {
                                                                rotate([0, 0, 30]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                          hull() {
                                                            translate([0, 0, 0.25]) {
                                                              rotate([0, 0, 30]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            translate([0, 0, 0.375]) {
                                                              rotate([0, 0, 45]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                        hull() {
                                                          translate([0, 0, 0.375]) {
                                                            rotate([0, 0, 45]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                          translate([0, 0, 0.5]) {
                                                            rotate([0, 0, 60]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                      hull() {
                                                        translate([0, 0, 0.5]) {
                                                          rotate([0, 0, 60]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                        translate([0, 0, 0.625]) {
                                                          rotate([0, 0, 75]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                    hull() {
                                                      translate([0, 0, 0.625]) {
                                                        rotate([0, 0, 75]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                      translate([0, 0, 0.75]) {
                                                        rotate([0, 0, 90]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                  hull() {
                                                    translate([0, 0, 0.75]) {
                                                      rotate([0, 0, 90]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                    translate([0, 0, 0.875]) {
                                                      rotate([0, 0, 105]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                                hull() {
                                                  translate([0, 0, 0.875]) {
                                                    rotate([0, 0, 105]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                  translate([0, 0, 1]) {
                                                    rotate([0, 0, 120]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                              hull() {
                                                translate([0, 0, 1]) {
                                                  rotate([0, 0, 120]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                                translate([0, 0, 1.125]) {
                                                  rotate([0, 0, 135]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([0, 0, 1.125]) {
                                                rotate([0, 0, 135]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                              translate([0, 0, 1.25]) {
                                                rotate([0, 0, 150]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([0, 0, 1.25]) {
                                              rotate([0, 0, 150]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                            translate([0, 0, 1.375]) {
                                              rotate([0, 0, 165]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([0, 0, 1.375]) {
                                            rotate([0, 0, 165]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                          translate([0, 0, 1.5]) {
                                            rotate([0, 0, 180]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([0, 0, 1.5]) {
                                          rotate([0, 0, 180]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                        translate([0, 0, 1.625]) {
                                          rotate([0, 0, 195]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([0, 0, 1.625]) {
                                        rotate([0, 0, 195]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                      translate([0, 0, 1.75]) {
                                        rotate([0, 0, 210]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([0, 0, 1.75]) {
                                      rotate([0, 0, 210]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                    translate([0, 0, 1.875]) {
                                      rotate([0, 0, 225]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([0, 0, 1.875]) {
                                    rotate([0, 0, 225]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                  translate([0, 0, 2]) {
                                    rotate([0, 0, 240]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([0, 0, 2]) {
                                  rotate([0, 0, 240]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                                translate([0, 0, 2.125]) {
                                  rotate([0, 0, 255]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([0, 0, 2.125]) {
                                rotate([0, 0, 255]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                              translate([0, 0, 2.25]) {
                                rotate([0, 0, 270]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([0, 0, 2.25]) {
                              rotate([0, 0, 270]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                            translate([0, 0, 2.375]) {
                              rotate([0, 0, 285]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([0, 0, 2.375]) {
                            rotate([0, 0, 285]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                          translate([0, 0, 2.5]) {
                            rotate([0, 0, 300]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([0, 0, 2.5]) {
                          rotate([0, 0, 300]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                        translate([0, 0, 2.625]) {
                          rotate([0, 0, 315]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                      }
                    }
                    hull() {
                      translate([0, 0, 2.625]) {
                        rotate([0, 0, 315]) {
                          hull() {
                            translate([3.02, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                            }
                            translate([4, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                            }
                          }
                        }
                      }
                      translate([0, 0, 2.75]) {
                        rotate([0, 0, 330]) {
                          hull() {
                            translate([3.02, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                            }
                            translate([4, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                            }
                          }
                        }
                      }
                    }
                  }
                  hull() {
                    translate([0, 0, 2.75]) {
                      rotate([0, 0, 330]) {
                        hull() {
                          translate([3.02, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                          }
                          translate([4, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                          }
                        }
                      }
                    }
                    translate([0, 0, 2.875]) {
                      rotate([0, 0, 345]) {
                        hull() {
                          translate([3.02, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                          }
                          translate([4, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                          }
                        }
                      }
                    }
                  }
                }
                hull() {
                  translate([0, 0, 2.875]) {
                    rotate([0, 0, 345]) {
                      hull() {
                        translate([3.02, 0, 0]) {
                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                        }
                        translate([4, 0, 0]) {
                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                        }
                      }
                    }
                  }
                  translate([0, 0, 3]) {
                    rotate([0, 0, 360]) {
                      hull() {
                        translate([3.02, 0, 0]) {
                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                        }
                        translate([4, 0, 0]) {
                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
        translate([0, 0, 8]) {
          zcyl(r = 4.1, l = 16);
        }
      }
    }
    intersection() {
      translate([0, 0, 82]) {
        union() {
          union() {
            union() {
              union() {
                translate([0, 0, 0]) {
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
                                                                hull() {
                                                                  translate([0, 0, 0]) {
                                                                    rotate([0, 0, 0]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                  translate([0, 0, 0.125]) {
                                                                    rotate([0, 0, 15]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                hull() {
                                                                  translate([0, 0, 0.125]) {
                                                                    rotate([0, 0, 15]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                  translate([0, 0, 0.25]) {
                                                                    rotate([0, 0, 30]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              hull() {
                                                                translate([0, 0, 0.25]) {
                                                                  rotate([0, 0, 30]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                translate([0, 0, 0.375]) {
                                                                  rotate([0, 0, 45]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            hull() {
                                                              translate([0, 0, 0.375]) {
                                                                rotate([0, 0, 45]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              translate([0, 0, 0.5]) {
                                                                rotate([0, 0, 60]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                          hull() {
                                                            translate([0, 0, 0.5]) {
                                                              rotate([0, 0, 60]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            translate([0, 0, 0.625]) {
                                                              rotate([0, 0, 75]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                        hull() {
                                                          translate([0, 0, 0.625]) {
                                                            rotate([0, 0, 75]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                          translate([0, 0, 0.75]) {
                                                            rotate([0, 0, 90]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                      hull() {
                                                        translate([0, 0, 0.75]) {
                                                          rotate([0, 0, 90]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                        translate([0, 0, 0.875]) {
                                                          rotate([0, 0, 105]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                    hull() {
                                                      translate([0, 0, 0.875]) {
                                                        rotate([0, 0, 105]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                      translate([0, 0, 1]) {
                                                        rotate([0, 0, 120]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                  hull() {
                                                    translate([0, 0, 1]) {
                                                      rotate([0, 0, 120]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                    translate([0, 0, 1.125]) {
                                                      rotate([0, 0, 135]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                                hull() {
                                                  translate([0, 0, 1.125]) {
                                                    rotate([0, 0, 135]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                  translate([0, 0, 1.25]) {
                                                    rotate([0, 0, 150]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                              hull() {
                                                translate([0, 0, 1.25]) {
                                                  rotate([0, 0, 150]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                                translate([0, 0, 1.375]) {
                                                  rotate([0, 0, 165]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([0, 0, 1.375]) {
                                                rotate([0, 0, 165]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                              translate([0, 0, 1.5]) {
                                                rotate([0, 0, 180]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([0, 0, 1.5]) {
                                              rotate([0, 0, 180]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                            translate([0, 0, 1.625]) {
                                              rotate([0, 0, 195]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([0, 0, 1.625]) {
                                            rotate([0, 0, 195]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                          translate([0, 0, 1.75]) {
                                            rotate([0, 0, 210]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([0, 0, 1.75]) {
                                          rotate([0, 0, 210]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                        translate([0, 0, 1.875]) {
                                          rotate([0, 0, 225]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([0, 0, 1.875]) {
                                        rotate([0, 0, 225]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                      translate([0, 0, 2]) {
                                        rotate([0, 0, 240]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([0, 0, 2]) {
                                      rotate([0, 0, 240]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                    translate([0, 0, 2.125]) {
                                      rotate([0, 0, 255]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([0, 0, 2.125]) {
                                    rotate([0, 0, 255]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                  translate([0, 0, 2.25]) {
                                    rotate([0, 0, 270]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([0, 0, 2.25]) {
                                  rotate([0, 0, 270]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                                translate([0, 0, 2.375]) {
                                  rotate([0, 0, 285]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([0, 0, 2.375]) {
                                rotate([0, 0, 285]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                              translate([0, 0, 2.5]) {
                                rotate([0, 0, 300]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([0, 0, 2.5]) {
                              rotate([0, 0, 300]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                            translate([0, 0, 2.625]) {
                              rotate([0, 0, 315]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([0, 0, 2.625]) {
                            rotate([0, 0, 315]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                          translate([0, 0, 2.75]) {
                            rotate([0, 0, 330]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([0, 0, 2.75]) {
                          rotate([0, 0, 330]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                        translate([0, 0, 2.875]) {
                          rotate([0, 0, 345]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                      }
                    }
                    hull() {
                      translate([0, 0, 2.875]) {
                        rotate([0, 0, 345]) {
                          hull() {
                            translate([3.02, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                            }
                            translate([4, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                            }
                          }
                        }
                      }
                      translate([0, 0, 3]) {
                        rotate([0, 0, 360]) {
                          hull() {
                            translate([3.02, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                            }
                            translate([4, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                            }
                          }
                        }
                      }
                    }
                  }
                }
                translate([0, 0, 3]) {
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
                                                                hull() {
                                                                  translate([0, 0, 0]) {
                                                                    rotate([0, 0, 0]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                  translate([0, 0, 0.125]) {
                                                                    rotate([0, 0, 15]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                hull() {
                                                                  translate([0, 0, 0.125]) {
                                                                    rotate([0, 0, 15]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                  translate([0, 0, 0.25]) {
                                                                    rotate([0, 0, 30]) {
                                                                      hull() {
                                                                        translate([3.02, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                        }
                                                                        translate([4, 0, 0]) {
                                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              hull() {
                                                                translate([0, 0, 0.25]) {
                                                                  rotate([0, 0, 30]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                translate([0, 0, 0.375]) {
                                                                  rotate([0, 0, 45]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            hull() {
                                                              translate([0, 0, 0.375]) {
                                                                rotate([0, 0, 45]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              translate([0, 0, 0.5]) {
                                                                rotate([0, 0, 60]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                          hull() {
                                                            translate([0, 0, 0.5]) {
                                                              rotate([0, 0, 60]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            translate([0, 0, 0.625]) {
                                                              rotate([0, 0, 75]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                        hull() {
                                                          translate([0, 0, 0.625]) {
                                                            rotate([0, 0, 75]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                          translate([0, 0, 0.75]) {
                                                            rotate([0, 0, 90]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                      hull() {
                                                        translate([0, 0, 0.75]) {
                                                          rotate([0, 0, 90]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                        translate([0, 0, 0.875]) {
                                                          rotate([0, 0, 105]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                    hull() {
                                                      translate([0, 0, 0.875]) {
                                                        rotate([0, 0, 105]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                      translate([0, 0, 1]) {
                                                        rotate([0, 0, 120]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                  hull() {
                                                    translate([0, 0, 1]) {
                                                      rotate([0, 0, 120]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                    translate([0, 0, 1.125]) {
                                                      rotate([0, 0, 135]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                                hull() {
                                                  translate([0, 0, 1.125]) {
                                                    rotate([0, 0, 135]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                  translate([0, 0, 1.25]) {
                                                    rotate([0, 0, 150]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                              hull() {
                                                translate([0, 0, 1.25]) {
                                                  rotate([0, 0, 150]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                                translate([0, 0, 1.375]) {
                                                  rotate([0, 0, 165]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([0, 0, 1.375]) {
                                                rotate([0, 0, 165]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                              translate([0, 0, 1.5]) {
                                                rotate([0, 0, 180]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([0, 0, 1.5]) {
                                              rotate([0, 0, 180]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                            translate([0, 0, 1.625]) {
                                              rotate([0, 0, 195]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([0, 0, 1.625]) {
                                            rotate([0, 0, 195]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                          translate([0, 0, 1.75]) {
                                            rotate([0, 0, 210]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([0, 0, 1.75]) {
                                          rotate([0, 0, 210]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                        translate([0, 0, 1.875]) {
                                          rotate([0, 0, 225]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([0, 0, 1.875]) {
                                        rotate([0, 0, 225]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                      translate([0, 0, 2]) {
                                        rotate([0, 0, 240]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([0, 0, 2]) {
                                      rotate([0, 0, 240]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                    translate([0, 0, 2.125]) {
                                      rotate([0, 0, 255]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([0, 0, 2.125]) {
                                    rotate([0, 0, 255]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                  translate([0, 0, 2.25]) {
                                    rotate([0, 0, 270]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([0, 0, 2.25]) {
                                  rotate([0, 0, 270]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                                translate([0, 0, 2.375]) {
                                  rotate([0, 0, 285]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([0, 0, 2.375]) {
                                rotate([0, 0, 285]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                              translate([0, 0, 2.5]) {
                                rotate([0, 0, 300]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([0, 0, 2.5]) {
                              rotate([0, 0, 300]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                            translate([0, 0, 2.625]) {
                              rotate([0, 0, 315]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([0, 0, 2.625]) {
                            rotate([0, 0, 315]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                          translate([0, 0, 2.75]) {
                            rotate([0, 0, 330]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([0, 0, 2.75]) {
                          rotate([0, 0, 330]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                        translate([0, 0, 2.875]) {
                          rotate([0, 0, 345]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                      }
                    }
                    hull() {
                      translate([0, 0, 2.875]) {
                        rotate([0, 0, 345]) {
                          hull() {
                            translate([3.02, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                            }
                            translate([4, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                            }
                          }
                        }
                      }
                      translate([0, 0, 3]) {
                        rotate([0, 0, 360]) {
                          hull() {
                            translate([3.02, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                            }
                            translate([4, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
              translate([0, 0, 6]) {
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
                                                              hull() {
                                                                translate([0, 0, 0]) {
                                                                  rotate([0, 0, 0]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                translate([0, 0, 0.125]) {
                                                                  rotate([0, 0, 15]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              hull() {
                                                                translate([0, 0, 0.125]) {
                                                                  rotate([0, 0, 15]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                translate([0, 0, 0.25]) {
                                                                  rotate([0, 0, 30]) {
                                                                    hull() {
                                                                      translate([3.02, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                      }
                                                                      translate([4, 0, 0]) {
                                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            hull() {
                                                              translate([0, 0, 0.25]) {
                                                                rotate([0, 0, 30]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              translate([0, 0, 0.375]) {
                                                                rotate([0, 0, 45]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                          hull() {
                                                            translate([0, 0, 0.375]) {
                                                              rotate([0, 0, 45]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            translate([0, 0, 0.5]) {
                                                              rotate([0, 0, 60]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                        hull() {
                                                          translate([0, 0, 0.5]) {
                                                            rotate([0, 0, 60]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                          translate([0, 0, 0.625]) {
                                                            rotate([0, 0, 75]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                      hull() {
                                                        translate([0, 0, 0.625]) {
                                                          rotate([0, 0, 75]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                        translate([0, 0, 0.75]) {
                                                          rotate([0, 0, 90]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                    hull() {
                                                      translate([0, 0, 0.75]) {
                                                        rotate([0, 0, 90]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                      translate([0, 0, 0.875]) {
                                                        rotate([0, 0, 105]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                  hull() {
                                                    translate([0, 0, 0.875]) {
                                                      rotate([0, 0, 105]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                    translate([0, 0, 1]) {
                                                      rotate([0, 0, 120]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                                hull() {
                                                  translate([0, 0, 1]) {
                                                    rotate([0, 0, 120]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                  translate([0, 0, 1.125]) {
                                                    rotate([0, 0, 135]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                              hull() {
                                                translate([0, 0, 1.125]) {
                                                  rotate([0, 0, 135]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                                translate([0, 0, 1.25]) {
                                                  rotate([0, 0, 150]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([0, 0, 1.25]) {
                                                rotate([0, 0, 150]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                              translate([0, 0, 1.375]) {
                                                rotate([0, 0, 165]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([0, 0, 1.375]) {
                                              rotate([0, 0, 165]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                            translate([0, 0, 1.5]) {
                                              rotate([0, 0, 180]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([0, 0, 1.5]) {
                                            rotate([0, 0, 180]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                          translate([0, 0, 1.625]) {
                                            rotate([0, 0, 195]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([0, 0, 1.625]) {
                                          rotate([0, 0, 195]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                        translate([0, 0, 1.75]) {
                                          rotate([0, 0, 210]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([0, 0, 1.75]) {
                                        rotate([0, 0, 210]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                      translate([0, 0, 1.875]) {
                                        rotate([0, 0, 225]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([0, 0, 1.875]) {
                                      rotate([0, 0, 225]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                    translate([0, 0, 2]) {
                                      rotate([0, 0, 240]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([0, 0, 2]) {
                                    rotate([0, 0, 240]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                  translate([0, 0, 2.125]) {
                                    rotate([0, 0, 255]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([0, 0, 2.125]) {
                                  rotate([0, 0, 255]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                                translate([0, 0, 2.25]) {
                                  rotate([0, 0, 270]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([0, 0, 2.25]) {
                                rotate([0, 0, 270]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                              translate([0, 0, 2.375]) {
                                rotate([0, 0, 285]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([0, 0, 2.375]) {
                              rotate([0, 0, 285]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                            translate([0, 0, 2.5]) {
                              rotate([0, 0, 300]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([0, 0, 2.5]) {
                            rotate([0, 0, 300]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                          translate([0, 0, 2.625]) {
                            rotate([0, 0, 315]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([0, 0, 2.625]) {
                          rotate([0, 0, 315]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                        translate([0, 0, 2.75]) {
                          rotate([0, 0, 330]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                      }
                    }
                    hull() {
                      translate([0, 0, 2.75]) {
                        rotate([0, 0, 330]) {
                          hull() {
                            translate([3.02, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                            }
                            translate([4, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                            }
                          }
                        }
                      }
                      translate([0, 0, 2.875]) {
                        rotate([0, 0, 345]) {
                          hull() {
                            translate([3.02, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                            }
                            translate([4, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                            }
                          }
                        }
                      }
                    }
                  }
                  hull() {
                    translate([0, 0, 2.875]) {
                      rotate([0, 0, 345]) {
                        hull() {
                          translate([3.02, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                          }
                          translate([4, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                          }
                        }
                      }
                    }
                    translate([0, 0, 3]) {
                      rotate([0, 0, 360]) {
                        hull() {
                          translate([3.02, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                          }
                          translate([4, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
            translate([0, 0, 9]) {
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
                                                            hull() {
                                                              translate([0, 0, 0]) {
                                                                rotate([0, 0, 0]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              translate([0, 0, 0.125]) {
                                                                rotate([0, 0, 15]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            hull() {
                                                              translate([0, 0, 0.125]) {
                                                                rotate([0, 0, 15]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                              translate([0, 0, 0.25]) {
                                                                rotate([0, 0, 30]) {
                                                                  hull() {
                                                                    translate([3.02, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                    }
                                                                    translate([4, 0, 0]) {
                                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                          hull() {
                                                            translate([0, 0, 0.25]) {
                                                              rotate([0, 0, 30]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            translate([0, 0, 0.375]) {
                                                              rotate([0, 0, 45]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                        hull() {
                                                          translate([0, 0, 0.375]) {
                                                            rotate([0, 0, 45]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                          translate([0, 0, 0.5]) {
                                                            rotate([0, 0, 60]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                      hull() {
                                                        translate([0, 0, 0.5]) {
                                                          rotate([0, 0, 60]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                        translate([0, 0, 0.625]) {
                                                          rotate([0, 0, 75]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                    hull() {
                                                      translate([0, 0, 0.625]) {
                                                        rotate([0, 0, 75]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                      translate([0, 0, 0.75]) {
                                                        rotate([0, 0, 90]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                  hull() {
                                                    translate([0, 0, 0.75]) {
                                                      rotate([0, 0, 90]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                    translate([0, 0, 0.875]) {
                                                      rotate([0, 0, 105]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                                hull() {
                                                  translate([0, 0, 0.875]) {
                                                    rotate([0, 0, 105]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                  translate([0, 0, 1]) {
                                                    rotate([0, 0, 120]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                              hull() {
                                                translate([0, 0, 1]) {
                                                  rotate([0, 0, 120]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                                translate([0, 0, 1.125]) {
                                                  rotate([0, 0, 135]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([0, 0, 1.125]) {
                                                rotate([0, 0, 135]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                              translate([0, 0, 1.25]) {
                                                rotate([0, 0, 150]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([0, 0, 1.25]) {
                                              rotate([0, 0, 150]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                            translate([0, 0, 1.375]) {
                                              rotate([0, 0, 165]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([0, 0, 1.375]) {
                                            rotate([0, 0, 165]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                          translate([0, 0, 1.5]) {
                                            rotate([0, 0, 180]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([0, 0, 1.5]) {
                                          rotate([0, 0, 180]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                        translate([0, 0, 1.625]) {
                                          rotate([0, 0, 195]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([0, 0, 1.625]) {
                                        rotate([0, 0, 195]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                      translate([0, 0, 1.75]) {
                                        rotate([0, 0, 210]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([0, 0, 1.75]) {
                                      rotate([0, 0, 210]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                    translate([0, 0, 1.875]) {
                                      rotate([0, 0, 225]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([0, 0, 1.875]) {
                                    rotate([0, 0, 225]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                  translate([0, 0, 2]) {
                                    rotate([0, 0, 240]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([0, 0, 2]) {
                                  rotate([0, 0, 240]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                                translate([0, 0, 2.125]) {
                                  rotate([0, 0, 255]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([0, 0, 2.125]) {
                                rotate([0, 0, 255]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                              translate([0, 0, 2.25]) {
                                rotate([0, 0, 270]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([0, 0, 2.25]) {
                              rotate([0, 0, 270]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                            translate([0, 0, 2.375]) {
                              rotate([0, 0, 285]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([0, 0, 2.375]) {
                            rotate([0, 0, 285]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                          translate([0, 0, 2.5]) {
                            rotate([0, 0, 300]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([0, 0, 2.5]) {
                          rotate([0, 0, 300]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                        translate([0, 0, 2.625]) {
                          rotate([0, 0, 315]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                      }
                    }
                    hull() {
                      translate([0, 0, 2.625]) {
                        rotate([0, 0, 315]) {
                          hull() {
                            translate([3.02, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                            }
                            translate([4, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                            }
                          }
                        }
                      }
                      translate([0, 0, 2.75]) {
                        rotate([0, 0, 330]) {
                          hull() {
                            translate([3.02, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                            }
                            translate([4, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                            }
                          }
                        }
                      }
                    }
                  }
                  hull() {
                    translate([0, 0, 2.75]) {
                      rotate([0, 0, 330]) {
                        hull() {
                          translate([3.02, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                          }
                          translate([4, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                          }
                        }
                      }
                    }
                    translate([0, 0, 2.875]) {
                      rotate([0, 0, 345]) {
                        hull() {
                          translate([3.02, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                          }
                          translate([4, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                          }
                        }
                      }
                    }
                  }
                }
                hull() {
                  translate([0, 0, 2.875]) {
                    rotate([0, 0, 345]) {
                      hull() {
                        translate([3.02, 0, 0]) {
                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                        }
                        translate([4, 0, 0]) {
                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                        }
                      }
                    }
                  }
                  translate([0, 0, 3]) {
                    rotate([0, 0, 360]) {
                      hull() {
                        translate([3.02, 0, 0]) {
                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                        }
                        translate([4, 0, 0]) {
                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          translate([0, 0, 12]) {
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
                                                          hull() {
                                                            translate([0, 0, 0]) {
                                                              rotate([0, 0, 0]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            translate([0, 0, 0.125]) {
                                                              rotate([0, 0, 15]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                          hull() {
                                                            translate([0, 0, 0.125]) {
                                                              rotate([0, 0, 15]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                            translate([0, 0, 0.25]) {
                                                              rotate([0, 0, 30]) {
                                                                hull() {
                                                                  translate([3.02, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                  }
                                                                  translate([4, 0, 0]) {
                                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                        hull() {
                                                          translate([0, 0, 0.25]) {
                                                            rotate([0, 0, 30]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                          translate([0, 0, 0.375]) {
                                                            rotate([0, 0, 45]) {
                                                              hull() {
                                                                translate([3.02, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                                }
                                                                translate([4, 0, 0]) {
                                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                      hull() {
                                                        translate([0, 0, 0.375]) {
                                                          rotate([0, 0, 45]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                        translate([0, 0, 0.5]) {
                                                          rotate([0, 0, 60]) {
                                                            hull() {
                                                              translate([3.02, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                              }
                                                              translate([4, 0, 0]) {
                                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                    hull() {
                                                      translate([0, 0, 0.5]) {
                                                        rotate([0, 0, 60]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                      translate([0, 0, 0.625]) {
                                                        rotate([0, 0, 75]) {
                                                          hull() {
                                                            translate([3.02, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                            }
                                                            translate([4, 0, 0]) {
                                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                  hull() {
                                                    translate([0, 0, 0.625]) {
                                                      rotate([0, 0, 75]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                    translate([0, 0, 0.75]) {
                                                      rotate([0, 0, 90]) {
                                                        hull() {
                                                          translate([3.02, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                          }
                                                          translate([4, 0, 0]) {
                                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                                hull() {
                                                  translate([0, 0, 0.75]) {
                                                    rotate([0, 0, 90]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                  translate([0, 0, 0.875]) {
                                                    rotate([0, 0, 105]) {
                                                      hull() {
                                                        translate([3.02, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                        }
                                                        translate([4, 0, 0]) {
                                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                              hull() {
                                                translate([0, 0, 0.875]) {
                                                  rotate([0, 0, 105]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                                translate([0, 0, 1]) {
                                                  rotate([0, 0, 120]) {
                                                    hull() {
                                                      translate([3.02, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                      }
                                                      translate([4, 0, 0]) {
                                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                            hull() {
                                              translate([0, 0, 1]) {
                                                rotate([0, 0, 120]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                              translate([0, 0, 1.125]) {
                                                rotate([0, 0, 135]) {
                                                  hull() {
                                                    translate([3.02, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                    }
                                                    translate([4, 0, 0]) {
                                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                          hull() {
                                            translate([0, 0, 1.125]) {
                                              rotate([0, 0, 135]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                            translate([0, 0, 1.25]) {
                                              rotate([0, 0, 150]) {
                                                hull() {
                                                  translate([3.02, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                  }
                                                  translate([4, 0, 0]) {
                                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                        hull() {
                                          translate([0, 0, 1.25]) {
                                            rotate([0, 0, 150]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                          translate([0, 0, 1.375]) {
                                            rotate([0, 0, 165]) {
                                              hull() {
                                                translate([3.02, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                                }
                                                translate([4, 0, 0]) {
                                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                      hull() {
                                        translate([0, 0, 1.375]) {
                                          rotate([0, 0, 165]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                        translate([0, 0, 1.5]) {
                                          rotate([0, 0, 180]) {
                                            hull() {
                                              translate([3.02, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                                              }
                                              translate([4, 0, 0]) {
                                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                    hull() {
                                      translate([0, 0, 1.5]) {
                                        rotate([0, 0, 180]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                      translate([0, 0, 1.625]) {
                                        rotate([0, 0, 195]) {
                                          hull() {
                                            translate([3.02, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                                            }
                                            translate([4, 0, 0]) {
                                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  hull() {
                                    translate([0, 0, 1.625]) {
                                      rotate([0, 0, 195]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                    translate([0, 0, 1.75]) {
                                      rotate([0, 0, 210]) {
                                        hull() {
                                          translate([3.02, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                                          }
                                          translate([4, 0, 0]) {
                                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                hull() {
                                  translate([0, 0, 1.75]) {
                                    rotate([0, 0, 210]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                  translate([0, 0, 1.875]) {
                                    rotate([0, 0, 225]) {
                                      hull() {
                                        translate([3.02, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                                        }
                                        translate([4, 0, 0]) {
                                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              hull() {
                                translate([0, 0, 1.875]) {
                                  rotate([0, 0, 225]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                                translate([0, 0, 2]) {
                                  rotate([0, 0, 240]) {
                                    hull() {
                                      translate([3.02, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                                      }
                                      translate([4, 0, 0]) {
                                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            hull() {
                              translate([0, 0, 2]) {
                                rotate([0, 0, 240]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                              translate([0, 0, 2.125]) {
                                rotate([0, 0, 255]) {
                                  hull() {
                                    translate([3.02, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 2.2]);
                                    }
                                    translate([4, 0, 0]) {
                                      cuboid([4.0e-2, 4.0e-2, 0.2]);
                                    }
                                  }
                                }
                              }
                            }
                          }
                          hull() {
                            translate([0, 0, 2.125]) {
                              rotate([0, 0, 255]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                            translate([0, 0, 2.25]) {
                              rotate([0, 0, 270]) {
                                hull() {
                                  translate([3.02, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 2.2]);
                                  }
                                  translate([4, 0, 0]) {
                                    cuboid([4.0e-2, 4.0e-2, 0.2]);
                                  }
                                }
                              }
                            }
                          }
                        }
                        hull() {
                          translate([0, 0, 2.25]) {
                            rotate([0, 0, 270]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                          translate([0, 0, 2.375]) {
                            rotate([0, 0, 285]) {
                              hull() {
                                translate([3.02, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 2.2]);
                                }
                                translate([4, 0, 0]) {
                                  cuboid([4.0e-2, 4.0e-2, 0.2]);
                                }
                              }
                            }
                          }
                        }
                      }
                      hull() {
                        translate([0, 0, 2.375]) {
                          rotate([0, 0, 285]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                        translate([0, 0, 2.5]) {
                          rotate([0, 0, 300]) {
                            hull() {
                              translate([3.02, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 2.2]);
                              }
                              translate([4, 0, 0]) {
                                cuboid([4.0e-2, 4.0e-2, 0.2]);
                              }
                            }
                          }
                        }
                      }
                    }
                    hull() {
                      translate([0, 0, 2.5]) {
                        rotate([0, 0, 300]) {
                          hull() {
                            translate([3.02, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                            }
                            translate([4, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                            }
                          }
                        }
                      }
                      translate([0, 0, 2.625]) {
                        rotate([0, 0, 315]) {
                          hull() {
                            translate([3.02, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 2.2]);
                            }
                            translate([4, 0, 0]) {
                              cuboid([4.0e-2, 4.0e-2, 0.2]);
                            }
                          }
                        }
                      }
                    }
                  }
                  hull() {
                    translate([0, 0, 2.625]) {
                      rotate([0, 0, 315]) {
                        hull() {
                          translate([3.02, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                          }
                          translate([4, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                          }
                        }
                      }
                    }
                    translate([0, 0, 2.75]) {
                      rotate([0, 0, 330]) {
                        hull() {
                          translate([3.02, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 2.2]);
                          }
                          translate([4, 0, 0]) {
                            cuboid([4.0e-2, 4.0e-2, 0.2]);
                          }
                        }
                      }
                    }
                  }
                }
                hull() {
                  translate([0, 0, 2.75]) {
                    rotate([0, 0, 330]) {
                      hull() {
                        translate([3.02, 0, 0]) {
                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                        }
                        translate([4, 0, 0]) {
                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                        }
                      }
                    }
                  }
                  translate([0, 0, 2.875]) {
                    rotate([0, 0, 345]) {
                      hull() {
                        translate([3.02, 0, 0]) {
                          cuboid([4.0e-2, 4.0e-2, 2.2]);
                        }
                        translate([4, 0, 0]) {
                          cuboid([4.0e-2, 4.0e-2, 0.2]);
                        }
                      }
                    }
                  }
                }
              }
              hull() {
                translate([0, 0, 2.875]) {
                  rotate([0, 0, 345]) {
                    hull() {
                      translate([3.02, 0, 0]) {
                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                      }
                      translate([4, 0, 0]) {
                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                      }
                    }
                  }
                }
                translate([0, 0, 3]) {
                  rotate([0, 0, 360]) {
                    hull() {
                      translate([3.02, 0, 0]) {
                        cuboid([4.0e-2, 4.0e-2, 2.2]);
                      }
                      translate([4, 0, 0]) {
                        cuboid([4.0e-2, 4.0e-2, 0.2]);
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      translate([0, 0, 89.25]) {
        zcyl(r = 4.1, l = 10.5);
      }
    }
  }
  union() {
    union() {
      union() {
        rotate([0, 0, 0]) {
          translate([8, 0, 0]) {
            translate([0, 0, 0.1]) {
              zcyl(r = 4, l = 0.2);
            }
          }
        }
        rotate([0, 0, 90]) {
          translate([8, 0, 0]) {
            translate([0, 0, 0.1]) {
              zcyl(r = 4, l = 0.2);
            }
          }
        }
      }
      rotate([0, 0, 180]) {
        translate([8, 0, 0]) {
          translate([0, 0, 0.1]) {
            zcyl(r = 4, l = 0.2);
          }
        }
      }
    }
    rotate([0, 0, 270]) {
      translate([8, 0, 0]) {
        translate([0, 0, 0.1]) {
          zcyl(r = 4, l = 0.2);
        }
      }
    }
  }
}
$fn = 50;