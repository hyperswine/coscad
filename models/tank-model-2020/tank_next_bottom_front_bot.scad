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
                          cuboid([20, 20, 60]);
                          translate([7.25, 0, 0]) {
                            cuboid([6.7, 6.2, 62]);
                          }
                        }
                        translate([7.3, 0, 0]) {
                          cuboid([1.8, 11, 62]);
                        }
                      }
                      translate([5.75, 0, 0]) {
                        cuboid([1.3, 8.4, 62]);
                      }
                    }
                    translate([-7.25, 0, 0]) {
                      cuboid([6.7, 6.2, 62]);
                    }
                  }
                  translate([-7.3, 0, 0]) {
                    cuboid([1.8, 11, 62]);
                  }
                }
                translate([-5.75, 0, 0]) {
                  cuboid([1.3, 8.4, 62]);
                }
              }
              translate([0, 7.25, 0]) {
                cuboid([6.2, 6.7, 62]);
              }
            }
            translate([0, 7.3, 0]) {
              cuboid([11, 1.8, 62]);
            }
          }
          translate([0, 5.75, 0]) {
            cuboid([8.4, 1.3, 62]);
          }
        }
        translate([0, -7.25, 0]) {
          cuboid([6.2, 6.7, 62]);
        }
      }
      translate([0, -7.3, 0]) {
        cuboid([11, 1.8, 62]);
      }
    }
    translate([0, -5.75, 0]) {
      cuboid([8.4, 1.3, 62]);
    }
  }
  zcyl(r = 2.1, l = 62);
}
$fn = 50;