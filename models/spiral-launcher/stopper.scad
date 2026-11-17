include <BOSL2/std.scad>

union() {
  translate([-188, 35, 38.4]) {
    cuboid([3, 5.6, 18]);
  }
  translate([-188, 34, 49.375]) {
    cuboid([3, 12, 4.05]);
  }
}
$fn = 50;