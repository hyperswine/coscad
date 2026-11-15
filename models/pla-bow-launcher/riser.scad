include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          difference() {
            difference() {
              difference() {
                difference() {
                  difference() {
                    difference() {
                      difference() {
                        difference() {
                          translate([7, 0, -1]) {
                            cuboid([30, 40, 54]);
                          }
                          translate([0, 14, 0]) {
                            cuboid([3.25, 14, 44.4]);
                          }
                        }
                        translate([7, 13.5, 12]) {
                          xcyl(r = 2.2, l = 32);
                        }
                      }
                      translate([7, 13.5, -12]) {
                        xcyl(r = 2.2, l = 32);
                      }
                    }
                    difference() {
                      translate([3.125, 19, 0]) {
                        cuboid([3, 4, 46.4]);
                      }
                      translate([4.625, 17, 0]) {
                        zcyl(r = 3, l = 48.4);
                      }
                    }
                  }
                  difference() {
                    translate([-3.125, 19, 0]) {
                      cuboid([3, 4, 46.4]);
                    }
                    translate([-4.625, 17, 0]) {
                      zcyl(r = 3, l = 48.4);
                    }
                  }
                }
                translate([0, -14, 0]) {
                  cuboid([3.25, 14, 44.4]);
                }
              }
              translate([7, -13.5, 12]) {
                xcyl(r = 2.2, l = 32);
              }
            }
            translate([7, -13.5, -12]) {
              xcyl(r = 2.2, l = 32);
            }
          }
          difference() {
            translate([3.125, -19, 0]) {
              cuboid([3, 4, 46.4]);
            }
            translate([4.625, -17, 0]) {
              zcyl(r = 3, l = 48.4);
            }
          }
        }
        difference() {
          translate([-3.125, -19, 0]) {
            cuboid([3, 4, 46.4]);
          }
          translate([-4.625, -17, 0]) {
            zcyl(r = 3, l = 48.4);
          }
        }
      }
      translate([5.5, 0, -11]) {
        cuboid([29, 12.4, 10.4]);
      }
    }
    translate([6, 0, -1]) {
      zcyl(r = 2.2, l = 56);
    }
  }
  translate([14, 0, -1]) {
    zcyl(r = 2.2, l = 56);
  }
}
$fn = 50;