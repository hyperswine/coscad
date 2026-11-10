include <BOSL2/std.scad>

difference() {
  union() {
    translate([0, 20, 1]) {
      cuboid([16, 34, 2]);
    }
    hull() {
      hull() {
        hull() {
          translate([0, 3.8, 0.2]) {
            zcyl(r = 0.8, l = 0.4);
          }
          translate([0, 66.6, 0.2]) {
            zcyl(r = 0.4, l = 0.4);
          }
        }
        translate([0, 31.6, 95]) {
          zcyl(r = 0.6, l = 0.4);
        }
      }
      translate([0, 68.6, 95]) {
        zcyl(r = 0.4, l = 0.4);
      }
    }
  }
  union() {
    translate([4.5, 30, 0]) {
      zcyl(r = 1.7, l = 30);
    }
    translate([-4.5, 10, 0]) {
      zcyl(r = 1.7, l = 30);
    }
  }
}
$fn = 50;