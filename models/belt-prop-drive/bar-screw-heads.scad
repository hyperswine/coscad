include <BOSL2/std.scad>

union() {
  translate([-63.7868, -56.7868, 20.5]) {
    zcyl(r = 4.25, l = 5);
  }
  translate([-106.2132, -99.2132, 20.5]) {
    zcyl(r = 4.25, l = 5);
  }
}
$fn = 50;