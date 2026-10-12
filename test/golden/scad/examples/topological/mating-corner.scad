include <BOSL2/std.scad>

union() {
  union() {
    cuboid([20, 20, 4]);
    multmatrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 5], [0, 0, 0, 1]]) {
      cuboid([2, 4, 6]);
    }
  }
  multmatrix([[0, 0, 1, 13], [1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1]]) {
    cuboid([2, 4, 6]);
  }
}
$fn = 50;