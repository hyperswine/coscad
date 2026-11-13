# Prepare fixed-layout print projects using their model's authoritative job table.
B = use "sol/lib/base".
C = use "sol/lib/cli".
D = use "sol/lib/digest".
J = use "sol/lib/json".
P = use "sol/lib/proc".
G = use "../models/bolted-glider-balanced/provenance".
field key obj = unwrap (J.get key obj).
strings list = unwrap (J.items list) |> List.map (fn x -> unwrap (J.text x)).
nums list = unwrap (J.items list) |> List.map (fn x -> unwrap (J.num x)).
query argv = unwrap (P.output (Proc.query (P.spec argv))).
export spec = case Proc.streamNow spec of
    Ok 0 -> Unit | Ok code -> error "project export exited {code}" | Err why -> error why.
bounds sizes exclude entry =
  row = unwrap (J.items entry);
  name = unwrap (J.text (row ! 0));
  xy = nums (row ! 1);
  extents = nums (field name sizes);
  dx = Numeric.div (extents ! 0) 2;
  dy = Numeric.div (extents ! 1) 2;
  left = (xy ! 0) - dx;
  right = (xy ! 0) + dx;
  bottom = (xy ! 1) - dy;
  top = (xy ! 1) + dy;
  if left < 6 or bottom < 6 or right > 250 or top > 250 then error "{name} leaves build plate margin"
  else if left < (exclude ! 0) + 6 and bottom < (exclude ! 1) + 6 then error "{name} enters exclusion corner"
  else (left,right,bottom,top).
apart gap a b =
  (left,right,bottom,top) = a;
  (x0,x1,y0,y1) = b;
  right + gap <= x0 or x1 + gap <= left or top + gap <= y0 or y1 + gap <= bottom.
prepare host root job =
  desc = unwrap (J.parse (query (host + ["describe",root,job])));
  exclude = nums (field "exclude" desc);
  boxes = List.map (bounds (field "sizes" desc) exclude) (unwrap (J.items (field "layout" desc)));
  switches = field "overrides" desc;
  gap = if field "brim_type" switches == J.JStr "no_brim" then 3 else
    3 + 2 * unwrap (J.num (unwrap (J.parse (unwrap (J.text (field "brim_width" switches))))));
  _ = List.fold (fn previous box ->
    if List.all (apart gap box) previous then box :: previous else error "parts too close on {job}") [] boxes;
  export (P.spec (host + ["export",root,job])).
boatJobs model = case model of
    "beach-monohull" -> ["p1s-pla","a1-pla","x1c-pla"]
  | "beach-outrigger" -> ["p1s-pla","p1s-petg","a1-pla","a1-petg","x1c-pla","x1c-petg"]
  | "twin-boat-trials" -> ["monohull","outrigger"]
  | _ -> [].
prepareBoat host root model job =
  twin = model == "twin-boat-trials";
  mono = model == "beach-monohull";
  printer = if job == "monohull" or Str.startsWith "p1s-" job then "P1S"
            else if Str.startsWith "x1c-" job then "X1C" else "A1";
  spool = if Str.endsWith "-petg" job then "PETG HF" else "PLA Basic";
  nozzle = if printer == "P1S" then " 0.4 nozzle" else "";
  stem = if twin then job else model;
  label = if twin then "{job} buoyancy trial" else "{model} {job}";
  center = if twin or printer == "A1" then [128,128] else [if mono then 128 else 134,136];
  wall = if mono then "8" else "6";
  brim = if twin then "3" else if mono then "5" else "4";
  options = [("wall_loops",wall),("sparse_infill_density","100%"),("sparse_infill_pattern","zig-zag"),
             ("top_shell_layers","6"),("bottom_shell_layers","6"),("enable_support","0"),
             ("support_on_build_plate_only","1"),("brim_type","outer_only"),("brim_width",brim),
             ("brim_object_gap","0.1"),("only_one_wall_top","0")];
  overrides = J.JObj (List.map (fn pair -> (k,v) = pair; (k,J.JStr v)) options + [("outer_wall_speed",J.strs ["35"])]);
  recipe = J.JObj [("stem",J.JStr stem),("printer",J.JStr printer),("center",J.nums center),("plate_name",J.JStr label),
      ("filament",J.JStr "Bambu {spool} @BBL {printer}{nozzle}"),("height",J.JStr (if twin then "0.16" else "0.2")),
      ("temperature",J.JStr (if mono then "225" else "")),("overrides",overrides)];
  export (P.withStdin (J.render recipe) (P.spec (host + ["boat",root,job]))).
gliderJobs = ["a1-wing-right","p1s-wing-left","x1c-fuselage","a1-tail"].
p1sExtras = ["p1s-wing-right","p1s-tail"].
digest root name = unwrap (D.sha256File "{root}/{name}").
geometry root =
  rows = G.entries "{root}/meshes.json";
  _ = G.requireKeys (["design.py","design.json"] + G.geometryFiles (G.sourceFiles root)) rows;
  _ = G.require root rows;
  rows.
copyFile src dest = case Fs.copyPublishNow src dest 65536 True of
    Ok count -> Unit | Err why -> error (str why).
prepareGlider host root model job =
  balanced = model == "bolted-glider-balanced";
  extra = List.has job p1sExtras;
  folder = "{root}/{job}";
  guard = fn unused -> if extra then
    ProcessResult code msg why = unwrap (Proc.query (P.spec ["test","!","-e","{folder}/print-run.json"]));
    if code == 0 then Unit else error "Existing dispatch prevents rebuilding this job"
    else Unit;
  _ = guard Unit;
  helper = if job == "p1s-tail" then "prepare_tail_p1s.py" else "prepare_right_p1s.py";
  producers = ["../../scripts/prepare-projects.sol","../../scripts/prepare_project_host.py","provenance.sol"];
  source = ["meshes.json","prepare_slice.py"] + producers + (if extra then [helper] else []);
  fingerprint = fn name -> (name,J.JStr (digest root name));
  initial = if balanced then List.map fingerprint source else [];
  checked = if balanced then geometry root else [];
  parts = if Str.endsWith "-tail" job then ["stab","fin"]
          else if Str.endsWith "-fuselage" job then ["fuselage"]
          else [if Str.endsWith "-right" job then "wing-right" else "wing-left"];
  stage = tempDir "coscad-glider-project";
  _ = export (P.spec (host + ["glider",root,job,stage] + parts));
  _ = if balanced and (geometry root != checked or List.map fingerprint source != initial) then error "glider inputs changed during preparation" else Unit;
  _ = guard Unit;
  _ = export (P.spec ["mkdir","-p","{folder}/presets"]);
  presets = List.map (fn n -> "presets/{n}.json") ["machine","process","filament","project"];
  outputs = ["glider-project.3mf","layout.json"] + presets + List.map (fn p -> "{p}-on-plate.stl") parts;
  _ = List.map (fn n -> copyFile "{stage}/{n}" "{folder}/{n}") outputs;
  if balanced then
    _ = guard Unit;
    _ = if geometry root == checked and List.map fingerprint source == initial then Unit else error "glider inputs changed before receipt";
    # Keep historical inputs and also hash both implementations that generated the project.
    names = ["meshes.json","prepare_slice.py","{job}/glider-project.3mf","{job}/layout.json"] +
      List.map (fn n -> "{job}/presets/{n}") (List.filter (Str.endsWith ".json") (ls "{folder}/presets")) +
      (if extra then [helper] else []) + producers;
    generated = List.map fingerprint names;
    receipt = "{stage}/prepared-inputs.json";
    put = PipelineSpec [P.spec ["cat"]] (PipeBytes (unwrap (Utf8.encode (J.pretty (J.JObj generated) + "\n"))))
          (PipeFileNew receipt) PipeOutputInherit 0 PipeFail;
    _ = case Proc.pipeline put of Ok (PipelineResult 0 _ _ _) -> Unit | other -> error "cannot stage preparation receipt: {other}";
    copyFile receipt "{folder}/prepared-inputs.json"
  else Unit.
> usage = "usage: prepare-projects.sol [--python PATH] MODEL-DIR [JOB ...]";
  argv = args Unit;
  if List.has "--help" argv or List.has "-h" argv then print usage else
    opts = unwrap (C.parse [C.Option "python" "" C.One] argv);
    case C.operands opts of
      directory :: chosen ->
        root = Str.trim (query ["realpath",directory]);
        model = B.baseName root;
        host = [C.valueOr "python3" "python" opts,"{root}/../../scripts/prepare_project_host.py"];
        boats = boatJobs model;
        glider = List.has model ["bolted-glider","bolted-glider-balanced"];
        optional = if model == "bolted-glider-balanced" then List.filter (fn job -> List.has job chosen) p1sExtras else [];
        jobs = if glider then gliderJobs + optional else if boats != [] then boats else if List.has model ["two-hand-top","tt-propellers","tank-model-2020"] then
          strings (unwrap (J.parse (query (host + ["jobs",root])))) else error "unknown model: {model}";
        selected = List.filter (fn job -> chosen == [] or List.has job chosen) jobs;
        _ = List.map (if glider then prepareGlider host root model
                      else if boats == [] then prepare host root else prepareBoat host root model) selected;
        Unit
    | [] -> error usage.
