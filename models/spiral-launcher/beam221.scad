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
                          cuboid([20, 20, 221]);
                          translate([7.25, 0, 0]) {
                            cuboid([6.7, 6.2, 223]);
                          }
                        }
                        translate([7.3, 0, 0]) {
                          cuboid([1.8, 11, 223]);
                        }
                      }
                      translate([5.75, 0, 0]) {
                        cuboid([1.3, 8.4, 223]);
                      }
                    }
                    translate([-7.25, 0, 0]) {
                      cuboid([6.7, 6.2, 223]);
                    }
                  }
                  translate([-7.3, 0, 0]) {
                    cuboid([1.8, 11, 223]);
                  }
                }
                translate([-5.75, 0, 0]) {
                  cuboid([1.3, 8.4, 223]);
                }
              }
              translate([0, 7.25, 0]) {
                cuboid([6.2, 6.7, 223]);
              }
            }
            translate([0, 7.3, 0]) {
              cuboid([11, 1.8, 223]);
            }
          }
          translate([0, 5.75, 0]) {
            cuboid([8.4, 1.3, 223]);
          }
        }
        translate([0, -7.25, 0]) {
          cuboid([6.2, 6.7, 223]);
        }
      }
      translate([0, -7.3, 0]) {
        cuboid([11, 1.8, 223]);
      }
    }
    translate([0, -5.75, 0]) {
      cuboid([8.4, 1.3, 223]);
    }
  }
  zcyl(r = 2.1, l = 223);
}
$fn = 50;