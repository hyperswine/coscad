include <BOSL2/std.scad>

union() {
  translate([-188, 38.3, 42.55]) {
    cuboid([3, 3.6, 22.9]);
  }
  translate([-188, 42.75, 57]) {
    cuboid([3, 14.5, 6]);
  }
}
$fn = 50;