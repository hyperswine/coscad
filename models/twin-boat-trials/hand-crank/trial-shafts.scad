include <BOSL2/std.scad>

union() {
  union() {
    union() {
      difference() {
        translate([0, 0, 1.65]) {
          xcyl(r = 1.95, l = 88);
        }
        translate([0, 0, -2]) {
          cuboid([92, 10, 4]);
        }
      }
      union() {
        difference() {
          translate([48.5, 0, 0.5]) {
            cuboid([7, 6, 1]);
          }
          translate([47, 3, 0.5]) {
            cuboid([0.9, 2.4, 3]);
          }
        }
        translate([44.55, 0, 0.65]) {
          cuboid([1.5, 1, 0.6]);
        }
      }
    }
    translate([0, 12, 0]) {
      union() {
        difference() {
          translate([0, 0, 1.7]) {
            xcyl(r = 2, l = 88);
          }
          translate([0, 0, -2]) {
            cuboid([92, 10, 4]);
          }
        }
        difference() {
          union() {
            difference() {
              translate([48.5, 0, 0.5]) {
                cuboid([7, 6, 1]);
              }
              translate([47, 3, 0.5]) {
                cuboid([0.9, 2.4, 3]);
              }
            }
            translate([44.55, 0, 0.65]) {
              cuboid([1.5, 1, 0.6]);
            }
          }
          translate([1.7, 0, 0]) {
            translate([47, 3, 0.5]) {
              cuboid([0.9, 2.4, 3]);
            }
          }
        }
      }
    }
  }
  translate([0, 24, 0]) {
    union() {
      difference() {
        translate([0, 0, 1.75]) {
          xcyl(r = 2.05, l = 88);
        }
        translate([0, 0, -2]) {
          cuboid([92, 10, 4]);
        }
      }
      difference() {
        difference() {
          union() {
            difference() {
              translate([48.5, 0, 0.5]) {
                cuboid([7, 6, 1]);
              }
              translate([47, 3, 0.5]) {
                cuboid([0.9, 2.4, 3]);
              }
            }
            translate([44.55, 0, 0.65]) {
              cuboid([1.5, 1, 0.6]);
            }
          }
          translate([1.7, 0, 0]) {
            translate([47, 3, 0.5]) {
              cuboid([0.9, 2.4, 3]);
            }
          }
        }
        translate([3.4, 0, 0]) {
          translate([47, 3, 0.5]) {
            cuboid([0.9, 2.4, 3]);
          }
        }
      }
    }
  }
}
$fn = 50;