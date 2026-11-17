include <BOSL2/std.scad>

difference() {
  union() {
    union() {
      union() {
        union() {
          translate([-18, 51, 48.6]) {
            cuboid([8, 6, 16.8]);
          }
          translate([-18, 51, 57.575]) {
            cuboid([8, 6, 1.25]);
          }
        }
        translate([-18, 51, 38.525]) {
          cuboid([4, 4, 3.45]);
        }
      }
      translate([-10.75, 51, 54]) {
        cuboid([6.6, 6, 2.9]);
      }
    }
    translate([-7.45, 51, 54]) {
      ycyl(r = 1.45, l = 6);
    }
  }
  translate([-18, 51, 58.1]) {
    cuboid([10, 1.5, 2.2]);
  }
}
$fn = 50;