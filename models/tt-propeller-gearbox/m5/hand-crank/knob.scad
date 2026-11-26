include <BOSL2/std.scad>

translate([32, 0, 33.25]) {
  difference() {
    zcyl(r = 8, l = 16.1);
    zcyl(r = 4.3, l = 18.1);
  }
}
$fn = 50;