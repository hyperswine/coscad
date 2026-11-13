# Read the computational saddle gauges from a full assembly stress report.
J = use "sol/lib/json".
P = use "sol/lib/proc".
near = [8,40,0].
far = [24,40,0].
corners = [[184,172,0],[184,176,0],[188,172,0],[188,176,0]].
number v = unwrap (J.num v).
field name obj = unwrap (J.get name obj).
items name obj = unwrap (J.items (field name obj)).
samePosition point node =
  coords = items "position_mm" node;
  # Integer gauge locations have even final digits at seven decimal places.
  # Compare the distance directly to avoid rounding a scaled coordinate twice.
  and (List.len coords == List.len point) (List.all (fn pair ->
    (value,target) = pair; Numeric.abs (number value - target) <= 0.00000005) (List.zip coords point)).
displacement body point =
  pointName = unwrap (J.text (field "name" body));
  matches = List.filter (samePosition point) (items "nodes" body);
  case List.rev matches of
    node :: others -> List.map number (items "displacement_mm" node)
  | [] -> error "missing gauge {point} on {pointName}".
bodyNamed name result =
  matches = List.filter (fn body -> field "name" body == J.JStr name) (items "bodies" result);
  case matches of body :: others -> body | [] -> error "missing body: {name}".
angle body =
  a = displacement body near;
  b = displacement body far;
  dz = (b ! 2) - (a ! 2);
  dx = 16 + (b ! 0) - (a ! 0);
  # jq's atan2 uses the host libm. Preserve the quadrant for a rotated baseline.
  query = P.spec ["jq","-n","--argjson","dy","{dz}","--argjson","dx","{dx}","atan2($dy;$dx)"];
  unwrap (P.output (Proc.query query) |>? J.parse |>? J.num).
gauges report =
  # jq projects just the requested nodes from the full report. Matching the
  # nearest integer here is deliberately broad; Sol applies the seven-place test.
  targets = J.render (J.JArr (List.map J.nums ([near,far] + corners)));
  selection = Str.join " " [
    "def required($keys): . as $obj | if all($keys[]; . as $k | $obj | has($k)) then . else error(\"missing gauge input\") end;",
    ".result | required([\"degrees_of_freedom\",\"iterations\",\"free_residual_relative\",\"equilibrium_residual\",\"interfaces\",\"bodies\"]) |",
    "\{degrees_of_freedom, iterations, free_residual_relative, equilibrium_residual,",
    "interfaces: [.interfaces[] | required([\"name\",\"on_a\"]) | \{name,on_a\}],",
    "bodies: [.bodies[] | select(.name == \"wing\" or .name == \"body\") |",
    "\{name, nodes: [.nodes[] | (.position_mm | map(round)) as $p |",
    "select(any($targets[]; . == $p))]\}]\}"];
  path = if Str.startsWith "/" report then report else "./{report}";
  compact = unwrap (P.output (Proc.query (P.spec ["jq","-ce","--argjson","targets",targets,selection,path])));
  result = unwrap (J.parse compact);
  wing = bodyNamed "wing" result;
  rotation = angle wing - angle (bodyNamed "body" result);
  padZ = List.map (fn point -> (displacement wing point) ! 2) corners;
  wrenches = List.fold (fn obj i -> J.set (unwrap (J.text (field "name" i))) (field "on_a" i) obj)
                       (J.JObj []) (items "interfaces" result);
  J.JObj [("load_pad_corner_mean_z_mm",J.JNum (Numeric.div (List.fold (fn a b -> a + b) 0 padZ) 4)),
          ("root_baseline_angle_rad",J.JNum rotation),("baseline_mm",J.JArr [J.nums near,J.nums far]),
          ("degrees_of_freedom",field "degrees_of_freedom" result),("iterations",field "iterations" result),
          ("relative_residual",field "free_residual_relative" result),("equilibrium",field "equilibrium_residual" result),
          ("interface_wrenches",wrenches)].
> case args Unit of [path] -> print (J.pretty (gauges path)) | _ -> error "usage: wing-gauges.sol REPORT.json".
