include <BOSL2/std.scad>

difference() {
  union() {
    union() {
      union() {
        union() {
          union() {
            translate([-9.9027, 0, 0]) {
              rotate([0, 0, 90]) {
                rotate([90, 0, 0]) {
                  linear_extrude(height = 1.2) {
                    polygon(points = [[-9, 6], [9, 6], [9, 11.9617], [0, 12.9076], [-9, 11.9617]]);
                  }
                }
              }
            }
            translate([-27.3027, 0, 0]) {
              rotate([0, 0, 90]) {
                rotate([90, 0, 0]) {
                  linear_extrude(height = 1.2) {
                    polygon(points = [[-9, 6], [9, 6], [9, 11.3541], [0, 12.3], [-9, 11.3541]]);
                  }
                }
              }
            }
          }
          translate([-44.7027, 0, 0]) {
            rotate([0, 0, 90]) {
              rotate([90, 0, 0]) {
                linear_extrude(height = 1.2) {
                  polygon(points = [[-9, 6], [9, 6], [9, 10.7464], [0, 11.6924], [-9, 10.7464]]);
                }
              }
            }
          }
        }
        translate([-26.7027, 1.025, 8.3]) {
          cuboid([36, 0.8, 4.6]);
        }
      }
      translate([-26.7027, -1.025, 8.3]) {
        cuboid([36, 0.8, 4.6]);
      }
    }
    translate([-26.7027, 0, 10.3]) {
      cuboid([36, 2.85, 0.6]);
    }
  }
  translate([-26.7027, 0, 7.5]) {
    cuboid([38, 1.25, 5]);
  }
}
$fn = 50;