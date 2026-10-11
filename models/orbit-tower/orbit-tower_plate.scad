include <BOSL2/std.scad>

union() {
  translate([0, 0, -0.8]) {
    cube([256, 256, 0.6]);
  }
  translate([7, 7, 0]) {
    translate([29, 29, 0]) {
      union() {
        union() {
          cylinder(h = 4, r = 29);
          translate([0, 0, 4]) {
            intersection() {
              cylinder(h = 9.666666667, r1 = 29, r2 = 0);
              cylinder(h = 2, r = 29);
            }
          }
        }
        translate([0, 0, 8.4]) {
          cuboid([6, 6, 5.2], chamfer = 0.3);
        }
      }
    }
  }
  translate([72, 7, 0]) {
    translate([30, 10.5, 0]) {
      difference() {
        difference() {
          union() {
            union() {
              union() {
                cylinder(h = 4.5, r = 10.5);
                hull() {
                  translate([-24, 0, 0]) {
                    cylinder(h = 4.5, r = 4);
                  }
                  translate([24, 0, 0]) {
                    cylinder(h = 4.5, r = 4);
                  }
                }
              }
              translate([-24, 0, 0]) {
                cylinder(h = 4.5, r = 6);
              }
            }
            translate([24, 0, 0]) {
              cylinder(h = 4.5, r = 5);
            }
          }
          translate([0, 0, -0.2]) {
            cylinder(h = 4.9, r = 5.2);
          }
        }
        translate([24, 0, 0]) {
          translate([0, 0, -0.2]) {
            cylinder(h = 4.9, r = 2.75);
          }
        }
      }
    }
  }
  translate([138, 7, 0]) {
    translate([30, 10.5, 0]) {
      difference() {
        difference() {
          union() {
            union() {
              union() {
                cylinder(h = 4.5, r = 10.5);
                hull() {
                  translate([-24, 0, 0]) {
                    cylinder(h = 4.5, r = 4);
                  }
                  translate([24, 0, 0]) {
                    cylinder(h = 4.5, r = 4);
                  }
                }
              }
              translate([-24, 0, 0]) {
                cylinder(h = 4.5, r = 6);
              }
            }
            translate([24, 0, 0]) {
              cylinder(h = 4.5, r = 5);
            }
          }
          translate([0, 0, -0.2]) {
            cylinder(h = 4.9, r = 5.2);
          }
        }
        translate([24, 0, 0]) {
          translate([0, 0, -0.2]) {
            cylinder(h = 4.9, r = 2.75);
          }
        }
      }
    }
  }
  translate([7, 72, 0]) {
    translate([30, 10.5, 0]) {
      difference() {
        difference() {
          union() {
            union() {
              union() {
                cylinder(h = 4.5, r = 10.5);
                hull() {
                  translate([-24, 0, 0]) {
                    cylinder(h = 4.5, r = 4);
                  }
                  translate([24, 0, 0]) {
                    cylinder(h = 4.5, r = 4);
                  }
                }
              }
              translate([-24, 0, 0]) {
                cylinder(h = 4.5, r = 6);
              }
            }
            translate([24, 0, 0]) {
              cylinder(h = 4.5, r = 5);
            }
          }
          translate([0, 0, -0.2]) {
            cylinder(h = 4.9, r = 5.2);
          }
        }
        translate([24, 0, 0]) {
          translate([0, 0, -0.2]) {
            cylinder(h = 4.9, r = 2.75);
          }
        }
      }
    }
  }
  translate([73, 72, 0]) {
    translate([30, 10.5, 0]) {
      difference() {
        difference() {
          union() {
            union() {
              union() {
                cylinder(h = 4.5, r = 10.5);
                hull() {
                  translate([-24, 0, 0]) {
                    cylinder(h = 4.5, r = 4);
                  }
                  translate([24, 0, 0]) {
                    cylinder(h = 4.5, r = 4);
                  }
                }
              }
              translate([-24, 0, 0]) {
                cylinder(h = 4.5, r = 6);
              }
            }
            translate([24, 0, 0]) {
              cylinder(h = 4.5, r = 5);
            }
          }
          translate([0, 0, -0.2]) {
            cylinder(h = 4.9, r = 5.2);
          }
        }
        translate([24, 0, 0]) {
          translate([0, 0, -0.2]) {
            cylinder(h = 4.9, r = 2.75);
          }
        }
      }
    }
  }
  translate([139, 72, 0]) {
    translate([30, 10.5, 0]) {
      difference() {
        difference() {
          union() {
            union() {
              union() {
                cylinder(h = 4.5, r = 10.5);
                hull() {
                  translate([-24, 0, 0]) {
                    cylinder(h = 4.5, r = 4);
                  }
                  translate([24, 0, 0]) {
                    cylinder(h = 4.5, r = 4);
                  }
                }
              }
              translate([-24, 0, 0]) {
                cylinder(h = 4.5, r = 6);
              }
            }
            translate([24, 0, 0]) {
              cylinder(h = 4.5, r = 5);
            }
          }
          translate([0, 0, -0.2]) {
            cylinder(h = 4.9, r = 5.2);
          }
        }
        translate([24, 0, 0]) {
          translate([0, 0, -0.2]) {
            cylinder(h = 4.9, r = 2.75);
          }
        }
      }
    }
  }
  translate([7, 100, 0]) {
    translate([30, 10.5, 0]) {
      difference() {
        difference() {
          union() {
            union() {
              union() {
                cylinder(h = 4.5, r = 10.5);
                hull() {
                  translate([-24, 0, 0]) {
                    cylinder(h = 4.5, r = 4);
                  }
                  translate([24, 0, 0]) {
                    cylinder(h = 4.5, r = 4);
                  }
                }
              }
              translate([-24, 0, 0]) {
                cylinder(h = 4.5, r = 6);
              }
            }
            translate([24, 0, 0]) {
              cylinder(h = 4.5, r = 5);
            }
          }
          translate([0, 0, -0.2]) {
            cylinder(h = 4.9, r = 5.2);
          }
        }
        translate([24, 0, 0]) {
          translate([0, 0, -0.2]) {
            cylinder(h = 4.9, r = 2.75);
          }
        }
      }
    }
  }
  translate([73, 100, 0]) {
    translate([9, 9, 0]) {
      difference() {
        union() {
          union() {
            cylinder(h = 7, r = 9);
            translate([0, 0, 6.8]) {
              cylinder(h = 8.2, r = 4.8);
            }
          }
          translate([0, 0, 17.4]) {
            cuboid([6, 6, 5.2], chamfer = 0.3);
          }
        }
        translate([-3.3, -3.3, -0.2]) {
          cube([6.6, 6.6, 5.8]);
        }
      }
    }
  }
  translate([98, 100, 0]) {
    translate([9, 9, 0]) {
      difference() {
        union() {
          union() {
            cylinder(h = 7, r = 9);
            translate([0, 0, 6.8]) {
              cylinder(h = 8.2, r = 4.8);
            }
          }
          translate([0, 0, 17.4]) {
            cuboid([6, 6, 5.2], chamfer = 0.3);
          }
        }
        translate([-3.3, -3.3, -0.2]) {
          cube([6.6, 6.6, 5.8]);
        }
      }
    }
  }
  translate([123, 100, 0]) {
    translate([9, 9, 0]) {
      difference() {
        union() {
          union() {
            cylinder(h = 7, r = 9);
            translate([0, 0, 6.8]) {
              cylinder(h = 8.2, r = 4.8);
            }
          }
          translate([0, 0, 17.4]) {
            cuboid([6, 6, 5.2], chamfer = 0.3);
          }
        }
        translate([-3.3, -3.3, -0.2]) {
          cube([6.6, 6.6, 5.8]);
        }
      }
    }
  }
  translate([148, 100, 0]) {
    translate([9, 9, 0]) {
      difference() {
        union() {
          union() {
            cylinder(h = 7, r = 9);
            translate([0, 0, 6.8]) {
              cylinder(h = 8.2, r = 4.8);
            }
          }
          translate([0, 0, 17.4]) {
            cuboid([6, 6, 5.2], chamfer = 0.3);
          }
        }
        translate([-3.3, -3.3, -0.2]) {
          cube([6.6, 6.6, 5.8]);
        }
      }
    }
  }
  translate([173, 100, 0]) {
    translate([9, 9, 0]) {
      difference() {
        union() {
          union() {
            cylinder(h = 7, r = 9);
            translate([0, 0, 6.8]) {
              cylinder(h = 8.2, r = 4.8);
            }
          }
          translate([0, 0, 17.4]) {
            cuboid([6, 6, 5.2], chamfer = 0.3);
          }
        }
        translate([-3.3, -3.3, -0.2]) {
          cube([6.6, 6.6, 5.8]);
        }
      }
    }
  }
  translate([198, 100, 0]) {
    translate([9, 9, 0]) {
      difference() {
        union() {
          union() {
            cylinder(h = 7, r = 9);
            translate([0, 0, 6.8]) {
              cylinder(h = 8.2, r = 4.8);
            }
          }
          translate([0, 0, 17.4]) {
            cuboid([6, 6, 5.2], chamfer = 0.3);
          }
        }
        translate([-3.3, -3.3, -0.2]) {
          cube([6.6, 6.6, 5.8]);
        }
      }
    }
  }
  translate([223, 100, 0]) {
    translate([9, 9, 0]) {
      difference() {
        union() {
          cylinder(h = 7, r = 9);
          translate([0, 0, 7]) {
            intersection() {
              cylinder(h = 6, r1 = 9, r2 = 0);
              cylinder(h = 4, r = 9);
            }
          }
        }
        translate([-3.3, -3.3, -0.2]) {
          cube([6.6, 6.6, 5.8]);
        }
      }
    }
  }
  translate([7, 128, 0]) {
    translate([6.5, 6.5, 0]) {
      union() {
        union() {
          union() {
            cylinder(h = 4, r = 6.5);
            translate([0, 0, 4]) {
              intersection() {
                cylinder(h = 7.8, r1 = 6.5, r2 = 0);
                cylinder(h = 3, r = 6.5);
              }
            }
          }
          translate([0, 0, 6.8]) {
            cylinder(h = 3.6, r = 2.4);
          }
        }
        translate([0, 0, 10.4]) {
          intersection() {
            cylinder(h = 2.4, r1 = 2.4, r2 = 0);
            cylinder(h = 0.4, r = 2.4);
          }
        }
      }
    }
  }
  translate([27, 128, 0]) {
    translate([6.5, 6.5, 0]) {
      union() {
        union() {
          union() {
            cylinder(h = 4, r = 6.5);
            translate([0, 0, 4]) {
              intersection() {
                cylinder(h = 7.8, r1 = 6.5, r2 = 0);
                cylinder(h = 3, r = 6.5);
              }
            }
          }
          translate([0, 0, 6.8]) {
            cylinder(h = 3.6, r = 2.4);
          }
        }
        translate([0, 0, 10.4]) {
          intersection() {
            cylinder(h = 2.4, r1 = 2.4, r2 = 0);
            cylinder(h = 0.4, r = 2.4);
          }
        }
      }
    }
  }
  translate([47, 128, 0]) {
    translate([6.5, 6.5, 0]) {
      union() {
        union() {
          union() {
            cylinder(h = 4, r = 6.5);
            translate([0, 0, 4]) {
              intersection() {
                cylinder(h = 7.8, r1 = 6.5, r2 = 0);
                cylinder(h = 3, r = 6.5);
              }
            }
          }
          translate([0, 0, 6.8]) {
            cylinder(h = 3.6, r = 2.4);
          }
        }
        translate([0, 0, 10.4]) {
          intersection() {
            cylinder(h = 2.4, r1 = 2.4, r2 = 0);
            cylinder(h = 0.4, r = 2.4);
          }
        }
      }
    }
  }
  translate([67, 128, 0]) {
    translate([6.5, 6.5, 0]) {
      union() {
        union() {
          union() {
            cylinder(h = 4, r = 6.5);
            translate([0, 0, 4]) {
              intersection() {
                cylinder(h = 7.8, r1 = 6.5, r2 = 0);
                cylinder(h = 3, r = 6.5);
              }
            }
          }
          translate([0, 0, 6.8]) {
            cylinder(h = 3.6, r = 2.4);
          }
        }
        translate([0, 0, 10.4]) {
          intersection() {
            cylinder(h = 2.4, r1 = 2.4, r2 = 0);
            cylinder(h = 0.4, r = 2.4);
          }
        }
      }
    }
  }
  translate([87, 128, 0]) {
    translate([6.5, 6.5, 0]) {
      union() {
        union() {
          union() {
            cylinder(h = 4, r = 6.5);
            translate([0, 0, 4]) {
              intersection() {
                cylinder(h = 7.8, r1 = 6.5, r2 = 0);
                cylinder(h = 3, r = 6.5);
              }
            }
          }
          translate([0, 0, 6.8]) {
            cylinder(h = 3.6, r = 2.4);
          }
        }
        translate([0, 0, 10.4]) {
          intersection() {
            cylinder(h = 2.4, r1 = 2.4, r2 = 0);
            cylinder(h = 0.4, r = 2.4);
          }
        }
      }
    }
  }
  translate([107, 128, 0]) {
    translate([6.5, 6.5, 0]) {
      union() {
        union() {
          union() {
            cylinder(h = 4, r = 6.5);
            translate([0, 0, 4]) {
              intersection() {
                cylinder(h = 7.8, r1 = 6.5, r2 = 0);
                cylinder(h = 3, r = 6.5);
              }
            }
          }
          translate([0, 0, 6.8]) {
            cylinder(h = 3.6, r = 2.4);
          }
        }
        translate([0, 0, 10.4]) {
          intersection() {
            cylinder(h = 2.4, r1 = 2.4, r2 = 0);
            cylinder(h = 0.4, r = 2.4);
          }
        }
      }
    }
  }
}
$fn = 50;