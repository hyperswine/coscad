include <BOSL2/std.scad>

union() {
  difference() {
    skin([circle(r = 8, $fn = 100), circle(r = 8, $fn = 100), circle(r = 20, $fn = 100)], z = [0, 12, 52], slices = 0, method = "reindex");
    skin([circle(r = 6, $fn = 100), circle(r = 6, $fn = 100), circle(r = 18, $fn = 100)], z = [-1, 12, 53], slices = 0, method = "reindex");
  }
  translate([22, 0, 0]) {
    translate([0, 0, 46]) {
      skin([circle(r = 3, $fn = 100), zrot(30, p = circle(r = 4, $fn = 3))], z = [0, 6], slices = 0, method = "distance");
    }
  }
}
$fn = 50;