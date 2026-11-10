union() {
  union() {
    union() {
      union() {
        union() {
          union() {
            union() {
              union() {
                union() {
                  translate([0, -115, 0]) {
                    cylinder(h = 0.2, r = 6);
                  }
                  translate([0, 200, 0]) {
                    cylinder(h = 0.2, r = 6);
                  }
                }
                translate([10.2, -60, 0]) {
                  cylinder(h = 0.2, r = 6);
                }
              }
              translate([-10.2, -60, 0]) {
                cylinder(h = 0.2, r = 6);
              }
            }
            translate([7, 60, 0]) {
              cylinder(h = 0.2, r = 6);
            }
          }
          translate([-7, 60, 0]) {
            cylinder(h = 0.2, r = 6);
          }
        }
        translate([5.75, 150, 0]) {
          cylinder(h = 0.2, r = 6);
        }
      }
      translate([-5.75, 150, 0]) {
        cylinder(h = 0.2, r = 6);
      }
    }
    translate([4.5, 200, 0]) {
      cylinder(h = 0.2, r = 6);
    }
  }
  translate([-4.5, 200, 0]) {
    cylinder(h = 0.2, r = 6);
  }
}
$fn = 50;