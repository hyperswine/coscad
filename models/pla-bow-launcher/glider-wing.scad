include <BOSL2/std.scad>

union() {
  union() {
    linear_extrude(height = 0.4) {
      polygon(points = [[18, -90], [18, 90], [8, 115], [-18, 115], [-18, -115], [8, -115]]);
    }
    translate([7.2, 56.5, 0.695]) {
      cuboid([1.2, 93, 0.61]);
    }
  }
  translate([7.2, -56.5, 0.695]) {
    cuboid([1.2, 93, 0.61]);
  }
}
$fn = 50;