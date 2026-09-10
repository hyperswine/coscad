include <BOSL2/std.scad>

difference() {
  difference() {
    union() {
      union() {
        union() {
          union() {
            cuboid([40.0, 40.0, 8.0]);
            translate([0.0, 0.0, 14.0]) {
              cyl(r = 6.0, h = 20.0);
            }
          }
          translate([0.0, 0.0, 26.5]) {
            cuboid([18.0, 18.0, 5.0], rounding = 2.0);
          }
        }
        translate([24.0, 0.0, 12.5]) {
          translate([0.0, 0.0, 0.0]) {
            rotate(a = 90.0, v = [0.0, 1.0, 0.0]) {
              translate([-0.0, -0.0, -0.0]) {
                cyl(r = 3.0, h = 8.0, chamfer = 0.5);
              }
            }
          }
        }
      }
      translate([-24.0, 0.0, 12.5]) {
        translate([0.0, 0.0, 0.0]) {
          rotate(a = 90.0, v = [0.0, -1.0, 0.0]) {
            translate([-0.0, -0.0, -0.0]) {
              cyl(r = 3.0, h = 8.0, chamfer = 0.5);
            }
          }
        }
      }
    }
    translate([0.0, 0.0, 27.0]) {
      zcyl(r = 2.15, l = 30.0);
    }
  }
  translate([22.0, 0.0, -4.0]) {
    ycyl(r = 2.0, l = 60.0);
  }
}
$fn = 50;