include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        union() {
          union() {
            union() {
              union() {
                translate([-107.5, 39, 31.6]) {
                  cuboid([215, 14, 2.2]);
                }
                translate([-107.5, 43.4, 34.1]) {
                  cuboid([215, 5.2, 7.2]);
                }
              }
              translate([-107.5, 41.75, 38.45]) {
                cuboid([215, 8.5, 1.7]);
              }
            }
            translate([-213, 36.45, 35.15]) {
              cuboid([4, 8.9, 5.1]);
            }
          }
          translate([-188, 41.75, 43.85]) {
            cuboid([12, 8.5, 12.3]);
          }
        }
        translate([-45, 43.4, 34.9]) {
          zcyl(r = 1.7, l = 10.8);
        }
      }
      translate([-3, 43.4, 34.9]) {
        zcyl(r = 1.7, l = 10.8);
      }
    }
    translate([-200, 43.4, 34.9]) {
      zcyl(r = 1.7, l = 10.8);
    }
  }
  translate([-188, 38.3, 41]) {
    cuboid([3.4, 4, 20]);
  }
}
$fn = 50;