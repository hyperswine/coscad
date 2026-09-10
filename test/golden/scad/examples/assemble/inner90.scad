include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          union() {
            union() {
              union() {
                union() {
                  union() {
                    union() {
                      translate([16.0, 0, 0]) {
                        translate([0, 0, 2.5]) {
                          cuboid([32.0, 20.0, 5.0], chamfer = 0.5);
                        }
                      }
                      translate([2.5, 0, 0]) {
                        translate([0, 0, 16.0]) {
                          cuboid([5.0, 20.0, 32.0], chamfer = 0.5);
                        }
                      }
                    }
                    translate([17.5, 0, 0]) {
                      translate([0, 0, 17.5]) {
                        rotate([0, 0, -90.0]) {
                          wedge([20.0, 25.0, 25.0], anchor = CENTER);
                        }
                      }
                    }
                  }
                  translate([13.3, 0, 0]) {
                    translate([0, 0, -0.6]) {
                      prismoid(size1 = [2.2, 5.4], size2 = [3.0, 5.8], h = 1.8, anchor = CENTER);
                    }
                  }
                }
                translate([26.7, 0, 0]) {
                  translate([0, 0, -0.6]) {
                    prismoid(size1 = [2.2, 5.4], size2 = [3.0, 5.8], h = 1.8, anchor = CENTER);
                  }
                }
              }
              translate([-0.6, 0, 0]) {
                translate([0, 0, 13.3]) {
                  rotate([0, 90.0, 0]) {
                    prismoid(size1 = [2.2, 5.4], size2 = [3.0, 5.8], h = 1.8, anchor = CENTER);
                  }
                }
              }
            }
            translate([-0.6, 0, 0]) {
              translate([0, 0, 26.7]) {
                rotate([0, 90.0, 0]) {
                  prismoid(size1 = [2.2, 5.4], size2 = [3.0, 5.8], h = 1.8, anchor = CENTER);
                }
              }
            }
          }
          translate([20.0, 0, 0]) {
            translate([0, 0, 2.5]) {
              zcyl(r = 2.75, l = 12.0);
            }
          }
        }
        translate([2.5, 0, 0]) {
          translate([0, 0, 20.0]) {
            xcyl(r = 2.75, l = 12.0);
          }
        }
      }
      translate([20.0, 0, 0]) {
        translate([0, 0, 15.9]) {
          zcyl(r = 5.0, l = 22.0);
        }
      }
    }
    translate([15.9, 0, 0]) {
      translate([0, 0, 20.0]) {
        xcyl(r = 5.0, l = 22.0);
      }
    }
  }
  ycyl(r = 3.0, l = 24.0);
}
$fn = 50;