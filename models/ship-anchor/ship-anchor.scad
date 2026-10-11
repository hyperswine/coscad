union() {
  union() {
    union() {
      union() {
        union() {
          translate([-5, 0, 0]) {
            translate([0, 4, 0]) {
              cube([10, 76, 6]);
            }
          }
          hull() {
            translate([-23, 0, 0]) {
              translate([0, 69, 0]) {
                cylinder(h = 6, r = 3);
              }
            }
            translate([23, 0, 0]) {
              translate([0, 69, 0]) {
                cylinder(h = 6, r = 3);
              }
            }
          }
        }
        difference() {
          translate([0, 88, 0]) {
            cylinder(h = 6, r = 12);
          }
          translate([0, 88, 0]) {
            translate([0, 0, -1]) {
              cylinder(h = 8, r = 7);
            }
          }
        }
      }
      difference() {
        difference() {
          translate([0, 34, 0]) {
            cylinder(h = 6, r = 34);
          }
          translate([0, 34, 0]) {
            translate([0, 0, -1]) {
              cylinder(h = 8, r = 25);
            }
          }
        }
        translate([-40, 0, 0]) {
          translate([0, 34, 0]) {
            translate([0, 0, -1]) {
              cube([80, 80, 8]);
            }
          }
        }
      }
    }
    hull() {
      hull() {
        translate([-36, 0, 0]) {
          translate([0, 40, 0]) {
            cylinder(h = 6, r = 1);
          }
        }
        translate([-18, 0, 0]) {
          translate([0, 33, 0]) {
            cylinder(h = 6, r = 1);
          }
        }
      }
      translate([-32, 0, 0]) {
        translate([0, 23, 0]) {
          cylinder(h = 6, r = 1);
        }
      }
    }
  }
  mirror([1, 0, 0]) {
    hull() {
      hull() {
        translate([-36, 0, 0]) {
          translate([0, 40, 0]) {
            cylinder(h = 6, r = 1);
          }
        }
        translate([-18, 0, 0]) {
          translate([0, 33, 0]) {
            cylinder(h = 6, r = 1);
          }
        }
      }
      translate([-32, 0, 0]) {
        translate([0, 23, 0]) {
          cylinder(h = 6, r = 1);
        }
      }
    }
  }
}
$fn = 50;