include <BOSL2/std.scad>

difference() {
  union() {
    difference() {
      hull() {
        translate([0, -65.25, 17]) {
          cuboid([20, 0.5, 34]);
        }
        translate([0, -79, 17]) {
          cuboid([8, 0.5, 10]);
        }
      }
      hull() {
        translate([0, -66.3, 17]) {
          cuboid([18, 0.4, 29]);
        }
        translate([0, -78, 17]) {
          cuboid([6, 0.4, 8]);
        }
      }
    }
    translate([0, -53, 18]) {
      cuboid([12, 24, 24]);
    }
  }
  union() {
    translate([0, -53, 12]) {
      xcyl(r = 1.7, l = 60);
    }
    translate([0, -53, 24]) {
      xcyl(r = 1.7, l = 60);
    }
  }
}
$fn = 50;