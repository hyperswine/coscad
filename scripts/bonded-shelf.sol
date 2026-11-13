# Generate the two explicit-grid shelf fixtures.
J = use "sol/lib/json".
numbers xs = J.JArr (List.map J.JNum xs).
body name origin counts h modulus = J.JObj [
  ("name", J.JStr name), ("origin_mm", numbers origin), ("cell_mm", J.JNum h),
  ("counts", numbers counts), ("young_mpa", J.JNum modulus), ("poisson", J.JNum 0.3)].
patch name faces = J.JObj [("body", J.JStr name), ("faces", J.JArr faces)].
grid width height face = List.concat (List.map (fn i ->
  List.map (fn j -> numbers (face i j)) (List.range 0 (height - 1))) (List.range 0 (width - 1))).

problem n modulus =
  h = Numeric.div 2 n;
  arm name = body name [(if name == "left" then 0 else 10), -2, 0] [n,4*n,n] h (if name == "left" then 2000 else modulus);
  bond name start = J.JObj [
    ("name", J.JStr (name + "_bond")),
    ("a", patch name (grid n (3*n) (fn i j -> [i,j+n,n - 1,2,1]))),
    ("b", patch "shelf" (grid n (3*n) (fn i j -> [i+start,j,0,2,-1])))];
  clamp name = patch name (grid n n (fn i k -> [i,0,k,1,-1]));
  load = patch "shelf" (grid (6*n) (3*n) (fn i j -> [i,j,n - 1,2,1]));
  J.JObj [
    ("schema", J.JStr "coscad.bonded-problem/1"),
    ("bodies", J.JArr [body "shelf" [0,0,2] [6*n,3*n,n] h 2000, arm "left", arm "right"]),
    ("bonds", J.JArr [bond "left" 0, bond "right" (5*n)]),
    ("supports", J.JArr [clamp "left", clamp "right"]),
    ("loads", J.JArr [J.JObj [("patch", load), ("force_n", numbers [0,0,-1])]]),
    ("tolerance", J.JNum 0.0000000001), ("iterations", J.JNum 20000)].
generate dir n =
  _ = mkdirp dir;
  writePath "{dir}/shelf-symmetric.json" (J.pretty (problem n 2000) + "\n") >>
  writePath "{dir}/shelf-soft-right.json" (J.pretty (problem n 500) + "\n").
> case args Unit of
    ["--output-dir",dir] -> generate dir 1
  | ["--output-dir",dir,"--refinement",value] ->
      n = unwrap (Try.parseInt value);
      if and (n >= 1) (n <= 3) then generate dir n else error "refinement must be 1, 2 or 3"
  | _ -> error "usage: bonded-shelf.sol --output-dir DIR [--refinement 1|2|3]".
