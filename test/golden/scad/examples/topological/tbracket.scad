include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      union() {
        union() {
          union() {
            cuboid([5, 15, 20]);
            translate([5.5, 0, 7.5]) {
              cuboid([16, 15, 5]);
            }
          }
          translate([-3, 0, 0]) {
            cuboid([1, 15, 5.8]);
          }
        }
        translate([6, 0, -1.9]) {
          mirror([0, 0, 1]) {
            rotate([0, 0, -90]) {
              wedge([15, 15, 14.2], anchor = CENTER);
            }
          }
        }
      }
      xcyl(r = 2.7, l = 12);
    }
    translate([13, 0, 0]) {
      xcyl(r = 5, l = 21);
    }
  }
  translate([7.5, 0, 6]) {
    zcyl(r = 1.4, l = 10);
  }
}
$fn = 50;