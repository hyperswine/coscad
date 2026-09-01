include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      union() {
        union() {
          union() {
            cuboid([5.0, 15.0, 20.0]);
            translate([5.5, 0.0, 7.5]) {
              cuboid([16.0, 15.0, 5.0]);
            }
          }
          translate([-3.0, 0.0, 0.0]) {
            cuboid([1.0, 15.0, 5.8]);
          }
        }
        translate([6.0, 0.0, -1.9]) {
          mirror([0.0, 0.0, 1.0]) {
            rotate([0, 0, -90.0]) {
              wedge([15.0, 15.0, 14.2], anchor = CENTER);
            }
          }
        }
      }
      translate([0.0, 0.0, 0.0]) {
        xcyl(r = 2.7, l = 12.0);
      }
    }
    translate([13.0, 0.0, 0.0]) {
      xcyl(r = 4.5, l = 21.0);
    }
  }
  translate([7.5, 0.0, 6.0]) {
    zcyl(r = 1.4, l = 10.0);
  }
}
$fn = 50;