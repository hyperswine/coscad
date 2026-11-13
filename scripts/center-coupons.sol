# Center either a single vertical shelf rail or a pair of Tesseract thread coupons.
C = use "sol/lib/cli".
D = use "sol/lib/digest".
J = use "sol/lib/json".
P = use "sol/lib/proc".
field name obj = unwrap (J.get name obj).
items name obj = unwrap (J.items (field name obj)).
number name obj = unwrap (J.num (field name obj)).
check value message = if value then Unit else error message.
center root python mode =
  rail = mode == "rail";
  _ = check (rail or mode == "thread") "expected rail or thread";
  stem = if rail then "fit" else "thread-fit";
  count = if rail then 1 else 2;
  file = "{root}/{stem}_manifest.json";
  mesh = "{root}/{stem}_bed1.stl";
  receipt = unwrap (D.sha256File file);
  meshHash = unwrap (D.sha256File mesh);
  data = unwrap (J.parse (unwrap (P.output (Proc.query (P.spec ["cat",file])))));
  variants = items "variants" data;
  beds = items "beds" data;
  _ = check (List.len beds == 1) "expected one bed";
  positions = items "placements" (beds ! 0);
  _ = check (List.len positions == count) "unexpected placement count";
  _ = check (List.all (fn v -> items "warnings" (field "printability" v) == []) variants) "printability warning";
  _ = if rail then check (List.len variants == 1 and field "down" (variants ! 0) == J.JStr "lft"
       and field "footprint" (variants ! 0) == J.nums [20,20,50]) "expected one upright 20 x 20 x 50 rail" else Unit;
  scripts = if rail then "{root}/../../../scripts" else "{root}/../../scripts";
  host = [python,"{scripts}/prepare_project_host.py"];
  tmp = tempDir "coscad-center-coupons";
  staged = "{tmp}/centered.stl";
  facts = unwrap (J.parse (unwrap (P.output (Proc.query (P.spec (host + ["center-mesh",root,mesh,staged,str count]))))));
  delta = unwrap (J.items facts) |> List.map (fn n -> unwrap (J.num n));
  _ = if rail then Unit else
    _ = List.map (fn p ->
      x = number "x" p + (delta ! 0);
      y = number "y" p + (delta ! 1);
      check (x - 5 >= 18 and y - 5 >= 28 and x + number "w" p + 5 < 250 and y + number "d" p + 5 < 250)
            "centered thread coupon or brim leaves the usable bed") positions;
    Unit;
  publish = P.spec (host + ["center-publish",root,mode,file,mesh,staged,receipt,meshHash]) |> P.withStdin (J.render facts);
  _ = case Proc.streamNow publish of Ok 0 -> Unit | other -> error "could not publish centered coupons: {other}";
  print (if rail then "Centered upright 20 x 20 x 50 mm rail; re-slice before using its toolpath"
                 else "Centered two thread coupons; re-slice before using the toolpath").
> usage = "usage: center-coupons.sol [--python PATH] rail|thread MODEL-DIR";
  argv = args Unit;
  if List.has "--help" argv or List.has "-h" argv then print usage else
    opts = unwrap (C.parse [C.Option "python" "" C.One] argv);
    case C.operands opts of [mode,dir] ->
      root = Str.trim (unwrap (P.output (Proc.query (P.spec ["realpath",dir]))));
      center root (C.valueOr "python3" "python" opts) mode
    | _ -> error usage.
