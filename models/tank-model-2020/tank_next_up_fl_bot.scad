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
                          cuboid([20, 20, 100]);
                          translate([7.25, 0, 0]) {
                            cuboid([6.7, 6.2, 102]);
                          }
                        }
                        translate([7.3, 0, 0]) {
                          cuboid([1.8, 11, 102]);
                        }
                      }
                      translate([5.75, 0, 0]) {
                        cuboid([1.3, 8.4, 102]);
                      }
                    }
                    translate([-7.25, 0, 0]) {
                      cuboid([6.7, 6.2, 102]);
                    }
                  }
                  translate([-7.3, 0, 0]) {
                    cuboid([1.8, 11, 102]);
                  }
                }
                translate([-5.75, 0, 0]) {
                  cuboid([1.3, 8.4, 102]);
                }
              }
              translate([0, 7.25, 0]) {
                cuboid([6.2, 6.7, 102]);
              }
            }
            translate([0, 7.3, 0]) {
              cuboid([11, 1.8, 102]);
            }
          }
          translate([0, 5.75, 0]) {
            cuboid([8.4, 1.3, 102]);
          }
        }
        translate([0, -7.25, 0]) {
          cuboid([6.2, 6.7, 102]);
        }
      }
      translate([0, -7.3, 0]) {
        cuboid([11, 1.8, 102]);
      }
    }
    translate([0, -5.75, 0]) {
      cuboid([8.4, 1.3, 102]);
    }
  }
  zcyl(r = 2.1, l = 102);
}
$fn = 50;