difference() {
  difference() {
    union() {
      union() {
        union() {
          cylinder(h = 4.5, r = 10.5);
          hull() {
            translate([-24, 0, 0]) {
              cylinder(h = 4.5, r = 4);
            }
            translate([24, 0, 0]) {
              cylinder(h = 4.5, r = 4);
            }
          }
        }
        translate([-24, 0, 0]) {
          cylinder(h = 4.5, r = 6);
        }
      }
      translate([24, 0, 0]) {
        cylinder(h = 4.5, r = 5);
      }
    }
    translate([0, 0, -0.2]) {
      cylinder(h = 4.9, r = 5.2);
    }
  }
  translate([24, 0, 0]) {
    translate([0, 0, -0.2]) {
      cylinder(h = 4.9, r = 2.75);
    }
  }
}
$fn = 50;