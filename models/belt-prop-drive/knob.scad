include <BOSL2/std.scad>

translate([-53, -78, 38.75]) {
  difference() {
    zcyl(r = 8, l = 16.1);
    zcyl(r = 4.3, l = 18.1);
  }
}
$fn = 50;