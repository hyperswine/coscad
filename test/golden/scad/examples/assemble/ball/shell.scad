include <BOSL2/std.scad>

difference() {
  difference() {
    union() {
      intersection() {
        difference() {
          sphere(20);
          sphere(17.6);
        }
        translate([0, 0, 25]) {
          cuboid([50, 50, 50]);
        }
      }
      intersection() {
        translate([0, 0, 4]) {
          tube(h = 8, or = 20, ir = 15);
        }
        sphere(20);
      }
    }
    translate([20, 0, 4]) {
      xcyl(r = 2.75, l = 50);
    }
  }
  translate([22.5, 0, 4]) {
    xcyl(r = 5, l = 10);
  }
}
$fn = 50;