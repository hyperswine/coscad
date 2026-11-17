include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          union() {
            union() {
              translate([-179.5, 32.5, 24.9]) {
                cuboid([35, 17, 15.4]);
              }
              translate([-195, 22.025, 31]) {
                cuboid([4, 4.05, 27.6]);
              }
            }
            translate([-190.15, 30.15, 41.8]) {
              cuboid([13.7, 20.3, 6]);
            }
          }
          translate([-188, 35, 37.3]) {
            cuboid([3.4, 6, 16.2]);
          }
        }
        translate([-180.5, 32.5, 26.2]) {
          ycyl(r = 2.75, l = 19);
        }
      }
      translate([-180.5, 29.5, 26.2]) {
        ycyl(r = 5.2, l = 13);
      }
    }
    translate([-167.5, 32.5, 26.2]) {
      ycyl(r = 2.75, l = 19);
    }
  }
  translate([-167.5, 29.5, 26.2]) {
    ycyl(r = 5.2, l = 13);
  }
}
$fn = 50;