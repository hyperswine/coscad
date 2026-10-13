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
                          translate([9.5, 9.5, 9.5]) {
                            cuboid([19, 19, 19]);
                          }
                          translate([16.1, 9.5, 9.5]) {
                            cuboid([6, 6.4, 6.4]);
                          }
                        }
                        translate([9.5, 16.1, 9.5]) {
                          cuboid([6.4, 6, 6.4]);
                        }
                      }
                      translate([9.5, 9.5, 16.1]) {
                        cuboid([6.4, 6.4, 6]);
                      }
                    }
                    translate([16.1, 9.5, 9.5]) {
                      zcyl(r = 1.7, l = 40);
                    }
                  }
                  translate([16.1, 9.5, 0]) {
                    zcyl(r = 3, l = 6);
                  }
                }
                translate([16.1, 9.5, 15]) {
                  rotate([0, 0, 30]) {
                    cylinder(h = 8, r = 3.3, $fn = 6);
                  }
                }
              }
              translate([9.5, 16.1, 9.5]) {
                xcyl(r = 1.7, l = 40);
              }
            }
            translate([0, 16.1, 9.5]) {
              xcyl(r = 3, l = 6);
            }
          }
          translate([15, 16.1, 9.5]) {
            rotate([0, 90, 0]) {
              cylinder(h = 8, r = 3.3, $fn = 6);
            }
          }
        }
        translate([9.5, 9.5, 16.1]) {
          ycyl(r = 1.7, l = 40);
        }
      }
      translate([9.5, 0, 16.1]) {
        ycyl(r = 3, l = 6);
      }
    }
    translate([9.5, 23, 16.1]) {
      rotate([90, 0, 0]) {
        cylinder(h = 8, r = 3.3, $fn = 6);
      }
    }
  }
  translate([18, 18, 18]) {
    rotate([0, 0, 45]) {
      rotate([0, 54.7356, 0]) {
        zcyl(r = 2.65, l = 10);
      }
    }
  }
}
$fn = 50;