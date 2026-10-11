difference() {
  difference() {
    difference() {
      difference() {
        hull() {
          hull() {
            hull() {
              translate([4, 0, 0]) {
                translate([0, 4, 0]) {
                  cylinder(h = 78, r = 4);
                }
              }
              translate([123.2, 0, 0]) {
                translate([0, 4, 0]) {
                  cylinder(h = 78, r = 4);
                }
              }
            }
            translate([4, 0, 0]) {
              translate([0, 123.2, 0]) {
                cylinder(h = 78, r = 4);
              }
            }
          }
          translate([123.2, 0, 0]) {
            translate([0, 123.2, 0]) {
              cylinder(h = 78, r = 4);
            }
          }
        }
        translate([2.4, 0, 0]) {
          translate([0, 2.4, 0]) {
            translate([0, 0, 3]) {
              hull() {
                hull() {
                  hull() {
                    translate([6, 0, 0]) {
                      translate([0, 6, 0]) {
                        cylinder(h = 76, r = 6);
                      }
                    }
                    translate([54, 0, 0]) {
                      translate([0, 6, 0]) {
                        cylinder(h = 76, r = 6);
                      }
                    }
                  }
                  translate([6, 0, 0]) {
                    translate([0, 54, 0]) {
                      cylinder(h = 76, r = 6);
                    }
                  }
                }
                translate([54, 0, 0]) {
                  translate([0, 54, 0]) {
                    cylinder(h = 76, r = 6);
                  }
                }
              }
            }
          }
        }
      }
      translate([64.8, 0, 0]) {
        translate([0, 2.4, 0]) {
          translate([0, 0, 3]) {
            hull() {
              hull() {
                hull() {
                  translate([6, 0, 0]) {
                    translate([0, 6, 0]) {
                      cylinder(h = 76, r = 6);
                    }
                  }
                  translate([54, 0, 0]) {
                    translate([0, 6, 0]) {
                      cylinder(h = 76, r = 6);
                    }
                  }
                }
                translate([6, 0, 0]) {
                  translate([0, 54, 0]) {
                    cylinder(h = 76, r = 6);
                  }
                }
              }
              translate([54, 0, 0]) {
                translate([0, 54, 0]) {
                  cylinder(h = 76, r = 6);
                }
              }
            }
          }
        }
      }
    }
    translate([2.4, 0, 0]) {
      translate([0, 64.8, 0]) {
        translate([0, 0, 3]) {
          hull() {
            hull() {
              hull() {
                translate([6, 0, 0]) {
                  translate([0, 6, 0]) {
                    cylinder(h = 76, r = 6);
                  }
                }
                translate([54, 0, 0]) {
                  translate([0, 6, 0]) {
                    cylinder(h = 76, r = 6);
                  }
                }
              }
              translate([6, 0, 0]) {
                translate([0, 54, 0]) {
                  cylinder(h = 76, r = 6);
                }
              }
            }
            translate([54, 0, 0]) {
              translate([0, 54, 0]) {
                cylinder(h = 76, r = 6);
              }
            }
          }
        }
      }
    }
  }
  translate([64.8, 0, 0]) {
    translate([0, 64.8, 0]) {
      translate([0, 0, 3]) {
        hull() {
          hull() {
            hull() {
              translate([6, 0, 0]) {
                translate([0, 6, 0]) {
                  cylinder(h = 76, r = 6);
                }
              }
              translate([54, 0, 0]) {
                translate([0, 6, 0]) {
                  cylinder(h = 76, r = 6);
                }
              }
            }
            translate([6, 0, 0]) {
              translate([0, 54, 0]) {
                cylinder(h = 76, r = 6);
              }
            }
          }
          translate([54, 0, 0]) {
            translate([0, 54, 0]) {
              cylinder(h = 76, r = 6);
            }
          }
        }
      }
    }
  }
}
$fn = 50;