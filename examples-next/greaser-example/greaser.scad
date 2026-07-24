include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      union() {
        union() {
          tube(h = 14.0, or = 8.0, ir = 4.25);
          translate([-99.0, 0, 0]) {
            xcyl(r = 7.5, l = 188.0);
          }
        }
        translate([-193.0, 0, 0]) {
          sphere(7.5);
        }
      }
      translate([5.0, 0, 0]) {
        cuboid([10.0, 7.4, 16.0]);
      }
    }
    translate([7.2, 0, 0]) {
      cuboid([6.0, 11.4, 16.0]);
    }
  }
  torus(r_maj = 4.6, r_min = 1.0);
}
$fn = 50;