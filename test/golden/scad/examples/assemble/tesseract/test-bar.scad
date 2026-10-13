include <BOSL2/std.scad>

union() {
  union() {
    cuboid([16, 7, 7], chamfer = 0.4);
    translate([-8.85, 0, 0]) {
      xcyl(r = 2.1, l = 1.95);
    }
  }
  translate([-10.3, 0, 0]) {
    xcyl(r = 2.7, l = 1);
  }
}
$fn = 50;