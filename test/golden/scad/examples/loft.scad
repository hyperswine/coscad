include <BOSL2/std.scad>

union() {
  difference() {
    skin([circle(r = 8.0, $fn = 100), circle(r = 8.0, $fn = 100), circle(r = 20.0, $fn = 100)], z = [0.0, 12.0, 52.0], slices = 0, method = "reindex");
    skin([circle(r = 6.0, $fn = 100), circle(r = 6.0, $fn = 100), circle(r = 18.0, $fn = 100)], z = [-1.0, 12.0, 53.0], slices = 0, method = "reindex");
  }
  translate([22.0, 0, 0]) {
    translate([0, 0, 46.0]) {
      skin([circle(r = 3.0, $fn = 100), zrot(30.0, p = circle(r = 4.0, $fn = 3))], z = [0.0, 6.0], slices = 0, method = "distance");
    }
  }
}
$fn = 50;