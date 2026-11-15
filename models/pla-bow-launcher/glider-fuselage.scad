include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          union() {
            union() {
              translate([0, 0.6, 0]) {
                rotate([90, 0, 0]) {
                  linear_extrude(height = 1.2) {
                    polygon(points = [[-7.8, -3.65], [50, -3.65], [58, -1.5], [62, 3], [60.5, 7.5], [55, 10], [-30, 10], [-118, 9], [-118, 3], [-7.8, 3]]);
                  }
                }
              }
              translate([0, 1, 0]) {
                rotate([90, 0, 0]) {
                  linear_extrude(height = 1.6) {
                    polygon(points = [[-7.8, -3.65], [50, -3.65], [58, -1.5], [62, 3], [60.5, 7.5], [55, 10], [-7.8, 10]]);
                  }
                }
              }
            }
            translate([0, -0.2, 0]) {
              rotate([90, 0, 0]) {
                linear_extrude(height = 0.4) {
                  polygon(points = [[-118, 7.5], [-118, 34], [-111, 36], [-88, 7.8409]]);
                }
              }
            }
          }
          translate([-4.9, 0, 0]) {
            cuboid([9.8, 4, 3.3]);
          }
        }
        translate([0, 2, 0]) {
          rotate([90, 0, 0]) {
            linear_extrude(height = 4) {
              polygon(points = [[-7.81, -2.45], [-6.6, -1.65], [-6.6, 1.65], [-7.81, 2.45]]);
            }
          }
        }
      }
      translate([-105, 0, 7.3]) {
        cuboid([22, 4, 0.6]);
      }
    }
    translate([44, 0, 1]) {
      ycyl(r = 1.6, l = 4);
    }
  }
  translate([52, 0, 1]) {
    ycyl(r = 1.6, l = 4);
  }
}
$fn = 50;