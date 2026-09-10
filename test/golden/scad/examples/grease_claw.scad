include <BOSL2/std.scad>

union() {
  union() {
    difference() {
      difference() {
        translate([7.4, 0, 0]) {
          cuboid([14.8, 22.0, 14.8], rounding = 1.5);
        }
        translate([7.4, 0, 0]) {
          ycyl(r = 4.4, l = 24.0);
        }
      }
      translate([7.4, 0, 0]) {
        translate([0, 0, 25.3]) {
          cuboid([16.8, 24.0, 50.0]);
        }
      }
    }
    hull() {
      translate([14.8, 0, 0]) {
        cuboid([0.1, 22.0, 14.8]);
      }
      translate([26.8, 0, 0]) {
        xcyl(r = 7.0, l = 0.1);
      }
    }
  }
  translate([126.8, 0, 0]) {
    xcyl(r = 7.0, l = 200.0);
  }
}
$fn = 50;