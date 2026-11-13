# Verify model-specific slicer and toolpath requirements before stamping the 3MF.
B = use "sol/lib/base".
C = use "sol/lib/cli".
J = use "sol/lib/json".
P = use "sol/lib/proc".
gliders = ["bolted-glider","bolted-glider-balanced"].
boats = ["beach-monohull","beach-outrigger","twin-boat-trials"].
isLight model = List.has model (gliders + ["orbit-tower-v2"]).
defaults model = case model of
    "tt-propellers" -> ["x1c-props"]
  | "two-hand-top" -> ["a1-top","a1-stems"]
  | "tank-model-2020" -> ["x1c-bed1","x1c-bed2","p1s-bed2"]
  | "orbit-tower-v2" -> ["x1c-tower"]
  | "beach-monohull" -> ["p1s-pla","a1-pla","x1c-pla"]
  | "beach-outrigger" -> ["p1s-pla","p1s-petg","a1-pla","a1-petg"]
  | "twin-boat-trials" -> ["monohull","outrigger"]
  | _ -> if List.has model gliders then
      ["a1-wing-right","p1s-wing-left","x1c-fuselage","a1-tail"] +
      (if model == "bolted-glider-balanced" then ["p1s-wing-right","p1s-tail"] else [])
    else error "unsupported prepared model: {model}".
profile model job =
  if model == "tt-propellers" then ("props","3","100%","1","0.12")
  else if model == "two-hand-top" then ("top","4",if job == "a1-stems" then "15%" else "100%","0","0.2")
  else if model == "tank-model-2020" then
    if job == "x1c-bed1" then ("tank","4","100%","0","0.2") else ("tank","3","25%","0","0.2")
  else if model == "orbit-tower-v2" then ("glider","3","15%","0","0.2")
  else ("glider","2","8%","0","0.2").
printer job = case (Str.split 45 job) ! 0 of
    "a1" -> ("A1","Bambu Lab A1","N2S") | "p1s" -> ("P1S","Bambu Lab P1S","C12")
  | "x1c" -> ("X1C","Bambu Lab X1 Carbon","BL-P001") | other -> error "unknown printer: {other}".
field name obj = unwrap (J.get name obj).
number name obj = unwrap (J.num (field name obj)).
check condition reason = if condition then Unit else error reason.
setting obj key value = check (field key obj == J.JStr value) "expected {key} = {value}".
runHost argv = case Proc.streamNow (P.spec argv) of
    Ok 0 -> Unit | Ok code -> error "package check exited {code}" | Err why -> error why.
verifyParts host root model tmp job =
  (stem,walls,infill,support,height) = profile model job;
  (device,fullName,modelId) = printer job;
  noBrim = job == "a1-stems" or isLight model;
  request = host + ["measure",root,job,stem,tmp,device,modelId];
  facts = unwrap (J.parse (unwrap (P.output (Proc.query (P.spec request)))));
  settings = field "settings" facts;
  _ = setting settings "wall_loops" walls;
  _ = setting settings "sparse_infill_density" infill;
  _ = setting settings "enable_support" support;
  _ = setting settings "layer_height" height;
  _ = if noBrim then setting settings "brim_type" "no_brim" else Unit;
  config = field "config" facts;
  _ = setting config "printer_model" fullName;
  _ = setting config "enable_support" support;
  _ = setting config "curr_bed_type" "Textured PEI Plate";
  _ = setting facts "material" "PLA";
  nSupport = number "support" facts;
  _ = check (if support == "1" then nSupport > 0 else nSupport == 0) "unexpected number of support segments";
  _ = check (number "segments" facts > 1000) "too few extrusion segments";
  _ = check (field "on_plate" facts == J.JBool True) "toolpath leaves the build plate";
  _ = check (number "in_corner" facts == 0) "toolpath enters excluded corner";
  _ = check (not (isLight model) or field "has_brim" facts == J.JBool False) "unexpected brim extrusion";
  mass = number "solid" facts;
  weight = number "weight" facts;
  upper = if support == "1" then 2.0 else if isLight model then 1.02 else 1.08;
  lowerOK = if isLight model then weight > 0.1 * mass else if infill == "100%" then weight > 0.94 * mass else True;
  _ = check (lowerOK and weight < upper * mass) "filament weight outside the model's expected range";
  runHost (host + ["publish",root,job,stem,tmp,device,modelId]).
verifyBoat host root model tmp job =
  twin = model == "twin-boat-trials";
  mono = model == "beach-monohull";
  stem = if twin then job else model;
  printerKey = if twin then (if job == "monohull" then "p1s" else "a1") else job;
  (device,fullName,modelId) = printer printerKey;
  material = if twin then "PLA" else case Str.split 45 job of
      prefix :: "pla" :: rest -> "PLA" | prefix :: "petg" :: rest -> "PETG" | _ -> error "unknown material: {job}";
  args = [root,job,stem,tmp,device,modelId];
  facts = unwrap (J.parse (unwrap (P.output (Proc.query (P.spec (host + ["measure-boat"] + args + [material]))))));
  settings = field "settings" facts;
  walls = if mono then "8" else "6";
  _ = setting settings "wall_loops" walls;
  _ = setting settings "enable_support" "0";
  _ = setting settings "sparse_infill_density" "100%";
  _ = if twin then setting settings "sparse_infill_pattern" "zig-zag" else setting settings "layer_height" "0.2";
  config = field "config" facts;
  _ = setting config "printer_model" fullName;
  _ = setting config "enable_support" "0";
  _ = setting config "curr_bed_type" "Textured PEI Plate";
  _ = if twin then setting config "wall_loops" walls else setting facts "material" material;
  _ = if mono then setting facts "temperature" "225" else Unit;
  (minLayers,maxLayers) = if twin then (100,200) else if mono then (300,340) else (250,320);
  layers = number "layers" facts;
  _ = check (layers > minLayers and layers < maxLayers) "unexpected layer count";
  _ = check (number "segments" facts > (if twin then 1000 else 10000) and number "support" facts == 0) "unexpected toolpaths";
  _ = check (field "inside_mesh_bounds" facts == J.JBool True) "toolpath outside the hull bounds";
  solid = number "solid" facts;
  weight = number "weight" facts;
  _ = check (weight > 0.94 * solid and weight < 1.08 * solid) "filament weight outside hull range";
  runHost (host + ["publish"] + args).
verify host root model tmp job =
  _ = check (not (List.has job ["",".",".."]) and not (Str.contains "/" job)) "invalid job directory: {job}";
  selectableBoat = List.has model ["beach-monohull","beach-outrigger"];
  _ = check (selectableBoat or List.has job (defaults model)) "unknown {model} job: {job}";
  if List.has model boats then verifyBoat host root model tmp job else verifyParts host root model tmp job.
> usage = "usage: verify-prepared.sol [--python PATH] MODEL-DIR [JOB ...]";
  argv = args Unit;
  if List.has "--help" argv or List.has "-h" argv then print usage else
    opts = unwrap (C.parse [C.Option "python" "" C.One] argv);
    case C.operands opts of
      dir :: selected ->
        root = Str.trim (unwrap (P.output (Proc.query (P.spec ["realpath",dir]))));
        model = B.baseName root;
        jobs = if selected == [] then defaults model else selected;
        host = [C.valueOr "python3" "python" opts,"{root}/../../scripts/prepared_package_host.py"];
        tmp = tempDir "coscad-verify-packages";
        mode = if List.has model boats and selected == [] then "fresh" else "merge";
        _ = runHost (host + ["check-index",root,mode]);
        _ = List.map (verify host root model tmp) jobs;
        runHost (host + ["index",root,mode] + jobs)
    | [] -> error usage.
