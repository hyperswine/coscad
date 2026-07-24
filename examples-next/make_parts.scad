// make_parts.scad
// Generic multi-part output helper for OpenSCAD.
//
// Usage:
//   include <make_parts.scad>
//
//   make_parts(["a", "b"], spacing=60, asm=[[0,0,0], [0,0,10]]) {
//     part_A();   // child 0  <-> names[0]
//     part_B();   // child 1  <-> names[1]
//   }
//
// Controlled by two top-level variables (overridable with -D on the CLI):
//   output_part : "all" shows everything, "__list__" echoes part names,
//                 or a single part name renders just that part at the origin.
//   view        : when output_part=="all", "plate" lays parts out in a row
//                 (print-plate style), "assembly" places each part at its
//                 asm[] position/rotation to preview the assembled device.
//
// CLI examples:
//   openscad -o a.stl -D 'output_part="a"' my.scad
//   openscad -o /dev/stdout --export-format echo -D 'output_part="__list__"' my.scad

output_part = "all"; // "all" | "__list__" | part name
view = "plate"; // "plate" | "assembly"   (only matters for "all")

// names   : list of strings, one per child, in child order
// spacing : X spacing between parts in plate view
// asm     : optional assembly placement per part. Each entry is either:
//             [x, y, z]                      (translate only)
//             [[x, y, z], [rx, ry, rz]]      (translate + rotate)
module make_parts(names, spacing = 60, asm = undef) {
  assert(
    len(names) == $children,
    str("make_parts: ", len(names), " names but ", $children, " children")
  );

  if (output_part == "__list__") {
    for (n = names) echo(str("PART: ", n));
  } else if (output_part == "all") {
    for (i = [0:$children - 1]) {
      if (view == "assembly" && !is_undef(asm)) {
        p = asm[i];
        has_rot = is_list(p[0]);
        pos = has_rot ? p[0] : p;
        rot = has_rot ? p[1] : [0, 0, 0];
        translate(pos) rotate(rot) children(i);
      } else {
        translate([i * spacing, 0, 0]) children(i);
      }
    }
  } else {
    idx = [for (i = [0:len(names) - 1]) if (names[i] == output_part) i];
    assert(
      len(idx) == 1,
      str(
        "make_parts: unknown part \"", output_part,
        "\". Known parts: ", names
      )
    );
    // Render the selected part at the origin, untransformed,
    // so exported STLs are always in the part's own frame.
    children(idx[0]);
  }
}
