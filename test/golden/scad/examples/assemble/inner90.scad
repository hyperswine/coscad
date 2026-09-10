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
                      translate([16, 0, 0]) {
                        translate([0, 0, 2.5]) {
                          cuboid([32, 20, 5], chamfer = 0.5);
                        }
                      }
                      translate([2.5, 0, 0]) {
                        translate([0, 0, 16]) {
                          cuboid([5, 20, 32], chamfer = 0.5);
                        }
                      }
                    }
                    translate([17.5, 0, 0]) {
                      translate([0, 0, 17.5]) {
                        rotate([0, 0, -90]) {
                          wedge([20, 25, 25], anchor = CENTER);
                        }
                      }
                    }
                  }
                  translate([13.3, 0, 0]) {
                    translate([0, 0, -0.6]) {
                      prismoid(size1 = [2.2, 5.4], size2 = [3, 5.8], h = 1.8, anchor = CENTER);
                    }
                  }
                }
                translate([26.7, 0, 0]) {
                  translate([0, 0, -0.6]) {
                    prismoid(size1 = [2.2, 5.4], size2 = [3, 5.8], h = 1.8, anchor = CENTER);
                  }
                }
              }
              translate([-0.6, 0, 0]) {
                translate([0, 0, 13.3]) {
                  rotate([0, 90, 0]) {
                    prismoid(size1 = [2.2, 5.4], size2 = [3, 5.8], h = 1.8, anchor = CENTER);
                  }
                }
              }
            }
            translate([-0.6, 0, 0]) {
              translate([0, 0, 26.7]) {
                rotate([0, 90, 0]) {
                  prismoid(size1 = [2.2, 5.4], size2 = [3, 5.8], h = 1.8, anchor = CENTER);
                }
              }
            }
          }
          translate([20, 0, 0]) {
            translate([0, 0, 2.5]) {
              zcyl(r = 2.75, l = 12);
            }
          }
        }
        translate([2.5, 0, 0]) {
          translate([0, 0, 20]) {
            xcyl(r = 2.75, l = 12);
          }
        }
      }
      translate([20, 0, 0]) {
        translate([0, 0, 15.9]) {
          zcyl(r = 5, l = 22);
        }
      }
    }
    translate([15.9, 0, 0]) {
      translate([0, 0, 20]) {
        xcyl(r = 5, l = 22);
      }
    }
  }
  ycyl(r = 3, l = 24);
}
$fn = 50;