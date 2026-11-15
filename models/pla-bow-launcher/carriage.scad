include <BOSL2/std.scad>

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
                    union() {
                      translate([12, 0, -4.1]) {
                        cuboid([24, 18, 3.2]);
                      }
                      translate([12, 7.65, -11.95]) {
                        cuboid([24, 2.7, 12.5]);
                      }
                    }
                    translate([12, -7.65, -11.95]) {
                      cuboid([24, 2.7, 12.5]);
                    }
                  }
                  translate([12, 6.25, -17.25]) {
                    cuboid([24, 5.5, 1.9]);
                  }
                }
                translate([12, -6.25, -17.25]) {
                  cuboid([24, 5.5, 1.9]);
                }
              }
              translate([3, 0, 0.5]) {
                cuboid([6, 18, 6]);
              }
            }
            translate([20, 0, 20.75]) {
              cuboid([8, 5, 46.5]);
            }
          }
          translate([20, 0, 44.595]) {
            cuboid([8, 6, 1.21]);
          }
        }
        translate([3, 0, 1.75]) {
          cuboid([2, 20, 4.5]);
        }
      }
      translate([11, 0, -4.1]) {
        zcyl(r = 2.2, l = 5.2);
      }
    }
    translate([20, 0, 45.1]) {
      cuboid([10, 1.5, 2.2]);
    }
  }
  translate([19.55, 0, 41]) {
    xcyl(r = 1.25, l = 9.1);
  }
}
$fn = 50;