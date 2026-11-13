# Generate the box grid in row order. Geometry is expressed in millimetres.
C = use "sol/lib/cli".
J = use "sol/lib/json".
header columns rows = [
  "// Open egg box: {columns} columns by {rows} rows.",
  "// Regenerate grid using generate.sol; geometry is entirely CoScad.",
  "columns = {columns}","rows = {rows}",
  "cell = 60","wall = 2.4","floor = 3","clear_depth = 75","height = floor + clear_depth",
  "pitch = cell + wall","width = columns * cell + (columns + 1) * wall",
  "depth = rows * cell + (rows + 1) * wall","outer_radius = 4","inner_radius = 6",
  "outer_a = χ outer_radius (ψ outer_radius (◎ outer_radius height))",
  "outer_b = χ (width - outer_radius) (ψ outer_radius (◎ outer_radius height))",
  "outer_c = χ outer_radius (ψ (depth - outer_radius) (◎ outer_radius height))",
  "outer_d = χ (width - outer_radius) (ψ (depth - outer_radius) (◎ outer_radius height))",
  "body = outer_a ⇓ outer_b ⇓ outer_c ⇓ outer_d",
  "hole_a = χ inner_radius (ψ inner_radius (◎ inner_radius (clear_depth + 1)))",
  "hole_b = χ (cell - inner_radius) (ψ inner_radius (◎ inner_radius (clear_depth + 1)))",
  "hole_c = χ inner_radius (ψ (cell - inner_radius) (◎ inner_radius (clear_depth + 1)))",
  "hole_d = χ (cell - inner_radius) (ψ (cell - inner_radius) (◎ inner_radius (clear_depth + 1)))",
  "hole = hole_a ⇓ hole_b ⇓ hole_c ⇓ hole_d"].
cell y x =
  name = "cell_{x}_{y}";
  (name,"{name} = χ (wall + {x} * pitch) (ψ (wall + {y} * pitch) (ζ floor hole))").
manifest =
  hints = J.ofRecord [("material","pla")];
  placement = J.ofRecord [("instance","egg-box")];
  bed = J.JObj [("index",J.JNum 1),("stl",J.JStr "egg-box.stl"),("placements",J.JArr [placement])];
  J.JObj [("bed",J.JObj [("w",J.JNum 256),("d",J.JNum 256)]),
          ("variants",J.JArr [J.JObj [("hints",hints)]]),("beds",J.JArr [bed])].
dimension option opts =
  value = unwrap (Try.parseInt (C.valueOr "2" option opts));
  if and (value >= 1) (value <= 20) then value else error "rows and columns must be between 1 and 20".
generate dir columns rows =
  cells = List.concat (List.map (fn y -> List.map (cell y) (List.range 0 (columns - 1))) (List.range 0 (rows - 1)));
  definitions = List.map (fn item -> (name,line) = item; line) cells;
  cuts = List.map (fn item -> (name,line) = item; "⊖ {name}") cells;
  _ = writePath "{dir}/egg-box.coscad" (Str.join "\n" (header columns rows + definitions + ["main = body " + Str.join " " cuts,""]));
  _ = writePath "{dir}/egg-box_manifest.json" (J.pretty manifest + "\n");
  print "Generated {columns} x {rows} egg box".
> opts = unwrap (C.parse [C.Option "columns" "" C.One,C.Option "rows" "" C.One] (args Unit));
  dir = if exists "models/egg-box/generate.sol" then "models/egg-box" else ".";
  if or (C.operands opts != []) (not (exists "{dir}/generate.sol")) then error "run from the CoScad or egg-box directory: generate.sol [--columns N] [--rows N]"
  else generate dir (dimension "columns" opts) (dimension "rows" opts).
