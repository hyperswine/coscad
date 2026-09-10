include <BOSL2/std.scad>

difference() {
  difference() {
    union() {
      union() {
        union() {
          union() {
            cuboid([40, 40, 8]);
            translate([0, 0, 14]) {
              cyl(r = 6, h = 20);
            }
          }
          translate([0, 0, 26.5]) {
            cuboid([18, 18, 5], rounding = 2);
          }
        }
        translate([24, 0, 12.5]) {
          rotate(a = 90, v = [0, 1, 0]) {
            cyl(r = 3, h = 8, chamfer = 0.5);
          }
        }
      }
      translate([-24, 0, 12.5]) {
        rotate(a = 90, v = [0, -1, 0]) {
          cyl(r = 3, h = 8, chamfer = 0.5);
        }
      }
    }
    translate([0, 0, 27]) {
      zcyl(r = 2.15, l = 30);
    }
  }
  translate([22, 0, -4]) {
    ycyl(r = 2, l = 60);
  }
}
$fn = 50;